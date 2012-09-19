-module(billy_client).

-export([
	start_session/4,
	stop_session/1,

	reserve/4,
	commit/1,
	rollback/1
]).

-include_lib("billy_common/include/service.hrl").

-type billy_session_id() :: binary().
-type billy_transaction_id() :: {SessionId::billy_session_id(), TransactionId::integer()}.

-define(SERVICE_TYPE_SMS_ON, <<"sms_on">>).
-define(SERVICE_TYPE_SMS_OFF, <<"sms_off">>).
-define(SERVICE_TYPE_SMS_INTERNATIONAL, <<"sms_international">>).

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
	CustomerId::any(),
	ServiceType::binary(),
	ServiceCount::pos_integer()
) ->
	{accepted, TransactionId::billy_transaction_id()}
  | {rejected, Reason::any()}
  | {error, Reason::any()}.
reserve(SessionId, CustomerId, ServiceType, ServiceCount) when ServiceCount > 0 ->
	case billy_client_session:start_transaction(SessionId) of
		{ok, TransactionId} ->
			{ok, EmptySvcContainer} = billy_service:cont_create(),
			{ok, FullSvcContainer} = billy_service:cont_set_id(
				EmptySvcContainer,
				ServiceType,
				#svc_details{quantity = ServiceCount}
			),
			case billy_client_transaction:reserve(TransactionId, CustomerId, FullSvcContainer) of
				{ok, accepted} ->
					{accepted, TransactionId};
				{ok, {rejected, Reason}} ->
					{rejected, Reason};
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
		{ok, {commited, ok}} ->
			commited;
		{error, Reason} ->
			{error, Reason}
	end.

-spec rollback(TransactionId::billy_transaction_id()) ->
	rolledback | {error, Reason::any()}.
rollback(TransactionId) ->
	case billy_client_transaction:rollback(TransactionId) of
		{ok, {rolledback, ok}} ->
			rolledback;
		{error, Reason} ->
			{error, Reason}
	end.
