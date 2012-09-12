-module(billy_client).

-export([
	start_session/4,
	stop_session/1,

	start_transaction/1,
	reserve/3,
	commit/1,
	rollback/1
]).

-include_lib("billy_common/include/service.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_session(
	Host::string(),
	Port::integer(),
	ClientId::binary(),
	ClientPw::binary()
) ->
	{ok, SessionId::any()} | {error, Reason::any()}.
start_session(Host, Port, ClientId, ClientPw) ->
	billy_client_session:start_session(Host, Port, ClientId, ClientPw).

-spec stop_session(SessionId::any()) -> ok | {error, Reason::any()}.
stop_session(SessionId) ->
	billy_client_session:stop_session(SessionId).

-spec start_transaction(SessionId::any()) -> {ok, TransactionId::any()} | {error, Reason::any()}.
start_transaction(SessionId) ->
	billy_client_session:start_transaction(SessionId).

-spec reserve(
	TransactionId::any(),
	CustomerId::any(),
	SvcContainer::#svc_container{}
) ->
	{ok, accepted} | {ok, {rejected, Reason::any()}} | {error, Reason::any()}.
reserve(TransactionId, CustomerId, SvcContainer) ->
	billy_client_transaction:reserve(TransactionId, CustomerId, SvcContainer).

-spec commit(TransactionId::any()) ->
	{ok, {commited, ok}} | {error, Reason::any()}.
commit(TransactionId) ->
	billy_client_transaction:commit(TransactionId).

-spec rollback(TransactionId::any()) ->
	{ok, {rolledback, ok}} | {error, Reason::any()}.
rollback(TransactionId) ->
	billy_client_transaction:rollback(TransactionId).
