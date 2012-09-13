-module(billy_client_session_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_session/3
]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 50000, Type, [I]}).

-include_lib("billy_common/include/logging.hrl").

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(Socket, ClientId, ClientPw) ->
	supervisor:start_child(?MODULE, [Socket, ClientId, ClientPw]).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
    {ok, {{simple_one_for_one, 5, 10}, [
    	?CHILD(billy_client_session, worker)
    ]}}.
