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
	{ok, SessionPid::pid()} | {error, Reason::any()}.
start_session(Host, Port, ClientId, ClientPw) ->
	billy_client_session:start_session(Host, Port, ClientId, ClientPw).

-spec stop_session(SessionPid::any()) -> ok | {error, Reason::any()}.
stop_session(SessionPid) ->
	billy_client_session:stop_session(SessionPid).

-spec start_transaction(SessionPid::any()) -> {ok, TransactionPid::any()} | {error, Reason::any()}.
start_transaction(SessionPid) ->
	billy_client_session:start_transaction(SessionPid).

-spec reserve(
	TransactionPid::pid(),
	CustomerId::any(),
	SvcContainer::#svc_container{}
) ->
	{ok, accepted} | {ok, {rejected, Reason::any()}} | {error, Reason::any()}.
reserve(TransactionPid, CustomerId, SvcContainer) ->
	billy_client_transaction:reserve(TransactionPid, CustomerId, SvcContainer).

-spec commit(TransactionPid::pid()) ->
	{ok, {commited, ok}} | {error, Reason::any()}.
commit(TransactionPid) ->
	billy_client_transaction:commit(TransactionPid).

-spec rollback(TransactionPid::pid()) ->
	{ok, {rolledback, ok}} | {error, Reason::any()}.
rollback(TransactionPid) ->
	billy_client_transaction:rollback(TransactionPid).
