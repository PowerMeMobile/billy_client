-module(tc).

-compile(export_all).

-include("logging.hrl").

start(StartSeqNum, EndSeqNum, TranQuantity, ThreadsQuantity) ->
	io:format("[tc] Generating tasks...~n", []), 
	{ok, TaskList} = generate_cids(StartSeqNum, EndSeqNum, TranQuantity, ThreadsQuantity),
	io:format("[tc] TaskList generated...~n", []),
	{ok, SessionPid} = billy_client_session:start(),
	io:format("SessionPid: ~p~n", [SessionPid]),
	StartTime = now(),
	CounterSrv = spawn_link(?MODULE, start_counter, [StartTime, TranQuantity]),
	lists:foreach(fun(Task)->
		spawn_link(?MODULE, start_thread, [Task, SessionPid, CounterSrv])
	end, TaskList).

start_thread(Task, SessionPid, CounterSrv) ->
	lists:foreach(fun(CID)->
		start_transaction(CID, SessionPid, CounterSrv),
		io:format(".", [])
	end, Task).

start_transaction(CID, SessionPid, CounterSrv) ->
	{ok, TranPid} = billy_client_session:start_transaction(SessionPid),
	{ok, Container} = new_container(10),
	case billy_client_transaction:reserve( TranPid, CID, Container ) of 
		{ok, accepted} ->
			% io:format("Reserving accepted... ~p~n", [TranPid]),
			% {ok, {commited, ok}} = billy_client_transaction:commit(TranPid),
			% io:format("Commit complited... ~n", []);
			{ok, {rolledback, ok}} = billy_client_transaction:rollback(TranPid),
			% io:format("Rollingback complited: ~p~n", [now()]);
			CounterSrv ! {report, rolledback};
		{ok, {rejected, _Reason}} ->
			% io:format("Rejected! Reason: ~p~n", [Reason]);
			CounterSrv ! {report, rejected};
		Any ->
			io:format("[transaction] Error. Reserve request returned: ~p~n", [Any]),
			CounterSrv ! {report, {error, Any}}
	end.

new_container(Cnt) ->
	{ok, EmptyCnt} = billy_service:cont_create(),
	{ok, Container} = billy_service:cont_set_id( EmptyCnt, <<"sms_on">>, {svc_details, Cnt} ),
	{ok, Container}.

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
 			io:format("[counter]: Error. Receive bad match: ~p~n", [Any]),
 			counter(StartTime, TranQuantity, ReportCnt + 1, {Rolledback, Rejected, TransactionErrors, CounterErrors + 1})
 	after
 		10000 ->
 			io:format("TIMEOUT...~n", []),
 			report(StartTime, TranQuantity, ReportCnt, {Rolledback, Rejected, TransactionErrors, CounterErrors})
 	end.

report(StartTime, TranQuantity, ReportCnt, Report) ->
	NStartTime = calendar:now_to_local_time(StartTime),
	io:format("~nStartTime: ~p~n", [NStartTime]),
	Now = now(),
	NEndTime = calendar:now_to_local_time(Now),
	io:format("NEndTime: ~p~n", [NEndTime]),

  	io:format("StartTime: ~p~n", [StartTime]),
 	io:format("EndTime: ~p~n", [Now]),

 	io:format("TranQuantity: ~p~n", [TranQuantity]),
 	io:format("ReportCnt: ~p~n", [ReportCnt]),
 	{Rolledback, Rejected, TransactionErrors, CounterErrors} = Report,
 	io:format("Rolledback: ~p ~n", [Rolledback]),
 	io:format("Rejected: ~p ~n", [Rejected]),
 	io:format("TransactionErrors: ~p~n", [TransactionErrors]),
 	io:format("CounterErrors: ~p~n", [CounterErrors]),

 	{S1,S2,S3} = StartTime,
 	NewStartTime = S1 * 1000000 + S2 + S3/1000000,
 	{E1,E2,E3} = Now,
 	NewEndTime = E1 * 1000000 + E2 + E3/1000000,
 	RPS = TranQuantity/(NewEndTime - NewStartTime),
 	io:format("Requests per second: ~p~n", [RPS]).

generate_cids(StartSeqNum, EndSeqNum, TranQuantity, ThreadsQuantity)->
	{ok, TaskMap} = gen_task_map(TranQuantity, ThreadsQuantity),
	io:format("TaskMap generated~n", []),
	Set = sets:new(),
	Random = fun()->random:uniform(EndSeqNum-StartSeqNum)+StartSeqNum end,
	{ok, TaskList} = gen_tasks([], TaskMap, Random, Set),
	{ok, TaskList}.

gen_task_map(TranQuantity, ThreadsQuantity)->
	Div = TranQuantity div ThreadsQuantity, % trans for each thread
	Rem = TranQuantity rem ThreadsQuantity, % rem of trans
	First = lists:map(fun(_)->
		Div+1
	end, lists:seq(1, Rem)),
	Second = lists:map(fun(_)->
		Div
	end, lists:seq(1, ThreadsQuantity - Rem)),
	TaskMap = lists:merge(First, Second), 
	{ok, TaskMap}.

gen_tasks(TaskList, [], _Random, _Set)->
	{ok, TaskList};
gen_tasks(TaskList, [CIDCnt | TaskMapTail], Random, Set)->
	{ok, Task, NewSet} = gen_task([], CIDCnt, Random, Set),
	io:format("~p tasks remains.~n", [length(TaskMapTail)]),
	gen_tasks([Task | TaskList], TaskMapTail, Random, NewSet).

gen_task(Task, 0, _Random, NewSet)->
	{ok, Task, NewSet};
gen_task(Task, CIDCnt, Random, Set)->
	NewCID = Random(),
	Bool = sets:is_element(NewCID, Set),
	case Bool of
		true ->
			gen_task(Task, CIDCnt, Random, Set);
		false ->
			NewSet = sets:add_element(NewCID, Set),
			gen_task([NewCID | Task], CIDCnt-1, Random, NewSet)
	end.