-module(billy_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	log4erl:conf("etc/l4e.config"),
    billy_client_sup:start_link().

stop(_State) ->
    ok.
