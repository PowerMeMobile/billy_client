-module(tc).

-compile(export_all).

-include_lib("billy_common/include/logging.hrl").
-include_lib("billy_common/include/service.hrl").

t() ->
	{ok, SessionId} = billy_client:start_session("127.0.0.1", 16062, <<"client1">>, <<"secureme!">>),

	{accepted, TransId1} = billy_client:reserve(SessionId, 1, <<"sms_on">>, 10),
	commited = billy_client:commit(TransId1),

	{accepted, TransId2} = billy_client:reserve(SessionId, 1, <<"sms_on">>, 10),
	rolledback = billy_client:rollback(TransId2),

	ok = billy_client:stop_session(SessionId).

test() ->
	{ok, SessionId} = billy_client:start_session("127.0.0.1", 16062, <<"client1">>, <<"secureme!">>),

	{accepted, TransId1} = billy_client:reserve(SessionId, 1, <<"sms_on">>, 9),
	commited = billy_client:commit(TransId1),

	{accepted, TransId2} = billy_client:reserve(SessionId, 1, <<"sms_off">>, 1),
	commited = billy_client:commit(TransId2),

	{accepted, TransId3} = billy_client:reserve(SessionId, 1, <<"sms_international">>, 3),
	commited = billy_client:commit(TransId3),

	{accepted, TransId4} = billy_client:reserve(SessionId, 1, <<"sms_international">>, 1),
	commited = billy_client:commit(TransId4),

	{accepted, TransId5} = billy_client:reserve(SessionId, 1, <<"sms_on">>, 2),
	commited = billy_client:commit(TransId5),

	{accepted, TransId6} = billy_client:reserve(SessionId, 1, <<"sms_off">>, 2),
	commited = billy_client:commit(TransId6),

	ok = billy_client:stop_session(SessionId).

start_session() ->
	billy_client:start_session("127.0.0.1", 16062, <<"client1">>, <<"secureme!">>).

stop_session(SessionId) ->
	billy_client:stop_session(SessionId).

start_transaction(SessionId) ->
	{ok, TransId} = billy_client:start_transaction(SessionId),
	{ok, TransId}.

test_commit(SessionId, CID) ->
	case billy_client:reserve(SessionId, CID, <<"sms_on">>, 10) of
		{accepted, TransId} ->
			?log_debug("Reserving accepted... ~p", [TransId]),
			commited = billy_client:commit(TransId),
			?log_debug("Commit completed... ", []);
		{rejected, Reason} ->
			?log_debug("Rejected! Reason: ~p", [Reason]);
		Any ->
			?log_debug("[transaction] Error. Reserve request returned: ~p", [Any])
	end.

test_rollback(SessionId, CID) ->
	case billy_client:reserve(SessionId, CID, <<"sms_on">>, 10) of
		{accepted, TransId} ->
			rolledback = billy_client:rollback(TransId),
			?log_debug("Rollingback complited: ~p", [now()]);
		{rejected, Reason} ->
			?log_debug("Rejected! Reason: ~p", [Reason]);
		Any ->
			?log_debug("[transaction] Error. Reserve request returned: ~p", [Any])
	end.

start(StartSeqNum, EndSeqNum, TranQuantity, ThreadsQuantity) ->
	?log_debug("[tc] Generating tasks...", []),
	{ok, TaskList} = generate_cids(StartSeqNum, EndSeqNum, TranQuantity, ThreadsQuantity),
	?log_debug("[tc] TaskList generated...~p", [TaskList]),
	{ok, SessionId} = billy_client:start_session("127.0.0.1", 16062, <<"client1">>, <<"secureme!">>),
	StartTime = now(),
	CounterSrv = spawn_link(?MODULE, start_counter, [StartTime, TranQuantity]),
	lists:foreach(
		fun(Task) ->
			spawn_link(?MODULE, start_thread, [Task, SessionId, CounterSrv])
		end,
		TaskList).

generate_cids(StartSeqNum, EndSeqNum, TranQuantity, ThreadsQuantity) ->
	{ok, TaskMap} = gen_task_map(TranQuantity, ThreadsQuantity),
	?log_debug("TaskMap generated", []),
	Set = sets:new(),
	RandomFun = fun() -> random:uniform(EndSeqNum - StartSeqNum) + StartSeqNum end,
	{ok, TaskList} = gen_tasks([], TaskMap, RandomFun, Set),
	{ok, TaskList}.

gen_task_map(TranQuantity, ThreadsQuantity) ->
	Div = TranQuantity div ThreadsQuantity, % trans for each thread
	Rem = TranQuantity rem ThreadsQuantity, % rem of trans
	First = lists:map(fun(_) -> Div + 1 end, lists:seq(1, Rem)),
	Second = lists:map(fun(_) -> Div end, lists:seq(1, ThreadsQuantity - Rem)),
	TaskMap = lists:merge(First, Second),
	{ok, TaskMap}.

gen_tasks(TaskListAcc, [], _RandomFun, _Set) ->
	{ok, TaskListAcc};
gen_tasks(TaskListAcc, [CIDCnt | TaskMapTail], RandomFun, Set) ->
	{ok, Task, NewSet} = gen_task([], CIDCnt, RandomFun, Set),
	?log_debug("~p tasks remains.", [length(TaskMapTail)]),
	gen_tasks([Task | TaskListAcc], TaskMapTail, RandomFun, NewSet).

gen_task(Task, 0, _RandomFun, NewSet) ->
	{ok, Task, NewSet};
gen_task(Task, CIDCnt, RandomFun, Set) ->
	NewCID = RandomFun(),
	Bool = sets:is_element(NewCID, Set),
	case Bool of
		true ->
			gen_task(Task, CIDCnt, RandomFun, Set);
		false ->
			NewSet = sets:add_element(NewCID, Set),
			gen_task([NewCID | Task], CIDCnt - 1, RandomFun, NewSet)
	end.

start_thread(Task, SessionId, CounterSrv) ->
	lists:foreach(
		fun(CID)->
			start_transaction(SessionId, CID, CounterSrv),
			?log_debug(".", [])
		end,
		Task).

start_transaction(SessionId, CID, CounterSrv) ->
	case billy_client:reserve(SessionId, CID, <<"sms_on">>, 10) of
		{accepted, TransId} ->
			?log_debug("Reserving accepted... ~p", [TransId]),
			% commited = billy_client:commit(TransId),
			% ?log_debug("Commit complited... ", []);
			rolledback = billy_client:rollback(TransId),
			?log_debug("Rollingback completed: ~p", [now()]),
			CounterSrv ! {report, rolledback};
		{rejected, Reason} ->
			?log_debug("Rejected! Reason: ~p", [Reason]),
			CounterSrv ! {report, rejected};
		Any ->
			?log_debug("[transaction] Error. Reserve request returned: ~p", [Any]),
			CounterSrv ! {report, {error, Any}}
	end.

start_counter(StartTime, TranQuantity) ->
	counter(StartTime, TranQuantity, 0, {0, 0, 0, 0}).

counter(StartTime, TranQuantity, ReportCnt, Report) when  TranQuantity == ReportCnt ->
	report(StartTime, TranQuantity, ReportCnt, Report);
counter(StartTime, TranQuantity, ReportCnt, {Rolledback, Rejected, TransactionErrors, CounterErrors})->
 	receive
 		{report, rolledback} ->
 			counter(StartTime, TranQuantity, ReportCnt + 1, {Rolledback + 1, Rejected, TransactionErrors, CounterErrors});
 		{report, rejected} ->
 			counter(StartTime, TranQuantity, ReportCnt + 1, {Rolledback, Rejected + 1, TransactionErrors, CounterErrors});
		{report, {error, _Any}} ->
 			counter(StartTime, TranQuantity, ReportCnt + 1, {Rolledback, Rejected, TransactionErrors +1, CounterErrors});
 		Any ->
 			?log_debug("[counter]: Error. Receive bad match: ~p", [Any]),
 			counter(StartTime, TranQuantity, ReportCnt + 1, {Rolledback, Rejected, TransactionErrors, CounterErrors + 1})
 	after
 		10000 ->
 			?log_debug("TIMEOUT...", []),
 			report(StartTime, TranQuantity, ReportCnt, {Rolledback, Rejected, TransactionErrors, CounterErrors})
 	end.

report(StartTime, TranQuantity, ReportCnt, Report) ->
	NStartTime = calendar:now_to_local_time(StartTime),
	?log_debug("StartTime: ~p", [NStartTime]),
	Now = now(),
	NEndTime = calendar:now_to_local_time(Now),
	?log_debug("NEndTime: ~p", [NEndTime]),

  	?log_debug("StartTime: ~p", [StartTime]),
 	?log_debug("EndTime: ~p", [Now]),

 	?log_debug("TranQuantity: ~p", [TranQuantity]),
 	?log_debug("ReportCnt: ~p", [ReportCnt]),
 	{Rolledback, Rejected, TransactionErrors, CounterErrors} = Report,
 	?log_debug("Rolledback: ~p ", [Rolledback]),
 	?log_debug("Rejected: ~p ", [Rejected]),
 	?log_debug("TransactionErrors: ~p", [TransactionErrors]),
 	?log_debug("CounterErrors: ~p", [CounterErrors]),

 	{S1, S2, S3} = StartTime,
 	NewStartTime = S1 * 1000000 + S2 + S3/1000000,
 	{E1, E2, E3} = Now,
 	NewEndTime = E1 * 1000000 + E2 + E3/1000000,
 	RPS = TranQuantity/(NewEndTime - NewStartTime),
 	?log_debug("Requests per second: ~p", [RPS]).
