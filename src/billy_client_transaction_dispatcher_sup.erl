-module(billy_client_transaction_dispatcher_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_dispatcher/0
]).

%% supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_dispatcher() ->
	supervisor:start_child(?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
    	{billy_client_transaction_dispatcher,
			{billy_client_transaction_dispatcher, start_link, []},
			temporary, 20000, worker, [billy_client_transaction_dispatcher]}
    ]}}.

