-module(billy_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("billy_client_sup:init~n", []),
    {ok, {{one_for_one, 5, 10}, [
    	?CHILD(billy_client_session_sup, supervisor),
    	?CHILD(billy_client_transaction_sup, supervisor),
    	?CHILD(billy_client_transaction_dispatcher_sup, supervisor)
    ]}}.
