-module(billy_client_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include_lib("alley_common/include/logging.hrl").

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    billy_client_sup:start_link().

stop(_State) ->
    ok.
