-module(billy_client_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include_lib("billy_common/include/logging.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	?log_debug("start 1", []),
    Ret = billy_client_sup:start_link(),
    ?log_debug("billy_client_app:start 2 ~p", [Ret]),
    Ret.

stop(_State) ->
    ok.
