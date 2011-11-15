-module(billy_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("billy_client_app:start 1~n", []),
    Ret = billy_client_sup:start_link(),
    io:format("billy_client_app:start 2 ~p~n", [Ret]),
    Ret.

stop(_State) ->
    ok.
