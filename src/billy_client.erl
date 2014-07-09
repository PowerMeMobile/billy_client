-module(billy_client).

-export([
    start_session/4,
    stop_session/1,
    reserve/5,
    commit/1,
    rollback/1
]).

-include_lib("billy_common/include/service.hrl").

-type billy_session_id() :: binary().
-type billy_transaction_id() :: {SessionId::billy_session_id(), TransactionId::integer()}.

-type service_type() :: binary().
-type service_quantity() :: pos_integer().
-type service_request() :: [{service_type(), service_quantity()}].

%% ===================================================================
%% API
%% ===================================================================

-spec start_session(
    Host::string(),
    Port::integer(),
    ClientId::binary(),
    ClientPw::binary()
) ->
    {ok, SessionId::billy_session_id()} | {error, Reason::any()}.
start_session(Host, Port, ClientId, ClientPw) ->
    billy_client_session:start_session(Host, Port, ClientId, ClientPw).

-spec stop_session(SessionId::billy_session_id()) -> ok | {error, Reason::any()}.
stop_session(SessionId) ->
    billy_client_session:stop_session(SessionId).

-spec reserve(
    SessionId::billy_session_id(),
    ClientType::binary(),
    CustomerId::binary(),
    UserId::binary(),
    ServiceRequest::service_request()
) ->
    {accepted, TransactionId::billy_transaction_id()}
  | {rejected, Reason::any()}
  | {error, Reason::any()}.
reserve(SessionId, ClientType, CustomerId, UserId, ServiceRequest)
        when length(ServiceRequest) > 0 ->
    case billy_client_session:start_transaction(SessionId) of
        {ok, TransactionId} ->
            SvcCont = build_svc_cont(ServiceRequest),
            case billy_client_transaction:reserve(TransactionId, ClientType,
                    CustomerId, UserId, SvcCont) of
                {ok, accepted} ->
                    {accepted, TransactionId};
                {ok, {rejected, Reason}} ->
                    %% TODO: should not the transaction be stopped here?
                    %% have it stopped already?
                    {rejected, list_to_atom(binary_to_list(Reason))};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec commit(TransactionId::billy_transaction_id()) ->
    commited | {error, Reason::any()}.
commit(TransactionId) ->
    case billy_client_transaction:commit(TransactionId) of
        % TODO: any other results but ok?
        {ok, {commited, ok}} ->
            commited;
        {error, Reason} ->
            {error, Reason}
    end.

-spec rollback(TransactionId::billy_transaction_id()) ->
    rolledback | {error, Reason::any()}.
rollback(TransactionId) ->
    case billy_client_transaction:rollback(TransactionId) of
        % TODO: any other results but ok?
        {ok, {rolledback, ok}} ->
            rolledback;
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

build_svc_cont(SvcReq) ->
    {ok, EmptySvcCont} = billy_service:cont_create(),
    build_svc_cont(SvcReq, EmptySvcCont).

build_svc_cont([], SvcCont) ->
    SvcCont;
build_svc_cont([{SvcType, SvcQnt} | SrvReq], SvcCont) ->
    {ok, SvcCont2} = billy_service:cont_set_id(SvcCont,
        SvcType, #svc_details{quantity = SvcQnt}),
    build_svc_cont(SrvReq, SvcCont2).
