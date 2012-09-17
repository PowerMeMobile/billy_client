-module(billy_client_transaction_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_transaction/1
]).

%% supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_transaction({SessionPid, TransactionId}) ->
	supervisor:start_child(?MODULE, [{SessionPid, TransactionId}]).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
    	{billy_client_transaction,
			{billy_client_transaction, start_link, []},
			temporary, 20000, worker, [billy_client_transaction]}
    ]}}.
