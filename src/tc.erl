-module(tc).

-compile(export_all).

-include("billy_client.hrl").
-include_lib("billy_common/include/logging.hrl").
-include_lib("billy_common/include/service.hrl").

start_session() ->
	billy_client:start_session(
		"127.0.0.1", 16062, <<"test">>, <<"test">>
	).

stop_session(SessionId) ->
	billy_client:stop_session(SessionId).

c() ->
	ClientType = <<"test">>,
	CustomerId = <<74,63,139,182,19,146,17,226,129,226,0,38,158,66,247,165>>,
	UserId = <<"User">>,

	{ok, SessionId} = start_session(),

	case billy_client:reserve(SessionId, ClientType, CustomerId, UserId, ?SERVICE_TYPE_SMS_ON, 10) of
		{accepted, TransId} ->
			?log_debug("Reserving accepted... ~p", [TransId]),
			commited = billy_client:commit(TransId),
			?log_debug("Commit completed... ", []);
		{rejected, Reason} ->
			?log_debug("Rejected! Reason: ~p", [Reason]);
		Any ->
			?log_debug("[transaction] Error. Reserve request returned: ~p", [Any])
	end,

	ok = stop_session(SessionId).

r() ->
	ClientType = <<"test">>,
	CustomerId = <<74,63,139,182,19,146,17,226,129,226,0,38,158,66,247,165>>,
	UserId = <<"User">>,

	{ok, SessionId} = start_session(),

	case billy_client:reserve(SessionId, ClientType, CustomerId, UserId, ?SERVICE_TYPE_SMS_ON, 10) of
		{accepted, TransId} ->
			?log_debug("Reserving accepted... ~p", [TransId]),
			rolledback = billy_client:rollback(TransId),
			?log_debug("Rollback completed... ", []);
		{rejected, Reason} ->
			?log_debug("Rejected! Reason: ~p", [Reason]);
		Any ->
			?log_debug("[transaction] Error. Reserve request returned: ~p", [Any])
	end,

	ok = stop_session(SessionId).

test() ->
	Start = erlang:now(),

	{ok, SessionId} = start_session(),

	ClientType = <<"test">>,
	CustomerId = <<74,63,139,182,19,146,17,226,129,226,0,38,158,66,247,165>>,
	UserId = <<"User">>,

	MaxAmount = 1,
	RejectsToFinish = 5,
	Args = [SessionId, ClientType, CustomerId, UserId, self(), MaxAmount, RejectsToFinish],

	Clients = [
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args),
		spawn_link(?MODULE, start_client, Args)
	],

	{ok, Dict} = join_all(length(Clients), dict:new()),

	ok = stop_session(SessionId),

	Stop = erlang:now(),
	Diff = timer:now_diff(Stop, Start) / 1000000,
	timer:sleep(1000),

	print_stats(Dict, Diff).

start_client(_SessionId, _ClientType, _CustomerId, _UserId, CollectorPid, _MaxAmount, 0) ->
	CollectorPid ! {done};
start_client(SessionId, ClientType, CustomerId, UserId, CollectorPid, MaxAmount, RejectsToFinish) ->
	Amount = random:uniform(MaxAmount),
	case billy_client:reserve(SessionId, ClientType, CustomerId, UserId, ?SERVICE_TYPE_SMS_ON, Amount) of
		{accepted, TransId} ->
			%timer:sleep(200),
			commited = billy_client:commit(TransId),
			CollectorPid ! {consumed, {?SERVICE_TYPE_SMS_ON, Amount}},
			start_client(SessionId, ClientType, CustomerId, UserId, CollectorPid, MaxAmount, RejectsToFinish);
		{rejected, _Reason} ->
			io:format("Reserve rejected with: ~p~n", [_Reason]),
			start_client(SessionId, ClientType, CustomerId, UserId, CollectorPid, MaxAmount, RejectsToFinish-1)
	end.

join_all(0, Dict) ->
	{ok, Dict};
join_all(HowMany, Dict) ->
	receive
		{consumed, {Service, Amount}} ->
			NewDict = dict:update_counter(Service, Amount, Dict),
			join_all(HowMany, NewDict);
		{done} ->
			join_all(HowMany-1, Dict)
	end.

print_stats(Dict, Time) ->
	List = dict:to_list(Dict),
	io:format("Services consumed:~n", []),
	lists:foreach(
		fun({Service, Amount}) ->
			io:format("~p: ~p~n", [Service, Amount])
		end,
		List),
	io:format("Time used: ~p secs~n", [Time]).

test_commit(SessionId, CID) ->
	case billy_client:reserve(SessionId, CID, ?SERVICE_TYPE_SMS_ON, 10) of
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
	case billy_client:reserve(SessionId, CID, ?SERVICE_TYPE_SMS_ON, 10) of
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
	{ok, SessionId} = start_session(),
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
	case billy_client:reserve(SessionId, CID, ?SERVICE_TYPE_SMS_ON, 10) of
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
