-module(tc).

-compile(export_all).

-include("billy_client.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("billy_common/include/service.hrl").

start_session() ->
    billy_client:start_session(
        "127.0.0.1", 16062, <<"test">>, <<"test">>
    ).

stop_session(SessionId) ->
    billy_client:stop_session(SessionId).

c() ->
    ClientType = <<"test">>,
    CustomerId = <<"50cec0fa-ea33-11e2-8cb1-00269e42f7a5">>,
    UserId = <<"User">>,

    {ok, SessionId} = start_session(),

    case billy_client:reserve(SessionId, ClientType, CustomerId, UserId, ?SERVICE_TYPE_SMS_ON, 300) of
        {accepted, TransId} ->
            ?log_debug("Reserve accepted... ~p", [TransId]),
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
    CustomerId = <<"50cec0fa-ea33-11e2-8cb1-00269e42f7a5">>,
    UserId = <<"User">>,

    {ok, SessionId} = start_session(),

    case billy_client:reserve(
        SessionId, ClientType, CustomerId, UserId, ?SERVICE_TYPE_SMS_ON, 300
    ) of
        {accepted, TransId} ->
            ?log_debug("Reserve accepted... ~p", [TransId]),
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
    CustomerId = <<"50cec0fa-ea33-11e2-8cb1-00269e42f7a5">>,
    UserId = <<"User">>,

    MaxAmount = 1,
    RejectsToFinish = 5,
    Args = [
        SessionId, ClientType, CustomerId, UserId,
        self(), MaxAmount, RejectsToFinish
    ],

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
