-module(billy_client).

-export([
	start_session/5,
	stop_session/1,

	reserve/5,
	commit/1,
	rollback/1
]).

-include_lib("billy_common/include/service.hrl").

-type billy_session_id() :: binary().
-type billy_transaction_id() :: {SessionId::billy_session_id(), TransactionId::integer()}.

% TODO: move these defines to billy_client.hrl
-define(CLIENT_TYPE_ONE_API, <<"one_api">>).
-define(CLIENT_TYPE_FUNNEL, <<"funnel">>).

-define(SERVICE_TYPE_SMS_ON, <<"sms_on">>).
-define(SERVICE_TYPE_SMS_OFF, <<"sms_off">>).
-define(SERVICE_TYPE_SMS_INTERNATIONAL, <<"sms_international">>).

%% ===================================================================
%% API
%% ===================================================================

-spec start_session(
	Host::string(),
	Port::integer(),
	ClientType::binary(),
	ClientId::binary(),
	ClientPw::binary()
) ->
	{ok, SessionId::billy_session_id()} | {error, Reason::any()}.
start_session(Host, Port, ClientType, ClientId, ClientPw) ->
	billy_client_session:start_session(Host, Port, ClientType, ClientId, ClientPw).

-spec stop_session(SessionId::billy_session_id()) -> ok | {error, Reason::any()}.
stop_session(SessionId) ->
	billy_client_session:stop_session(SessionId).

-spec reserve(
	SessionId::billy_session_id(),
	CustomerId::binary(),
	UserId::binary(),
	ServiceType::binary(),
	ServiceQuantity::pos_integer()
) ->
	{accepted, TransactionId::billy_transaction_id()}
  | {rejected, Reason::any()}
  | {error, Reason::any()}.
reserve(SessionId, CustomerId, UserId, ServiceType, ServiceQuantity) when ServiceQuantity > 0 ->
	case billy_client_session:start_transaction(SessionId) of
		{ok, TransactionId} ->
			{ok, EmptySvcContainer} = billy_service:cont_create(),
			{ok, FullSvcContainer} = billy_service:cont_set_id(
				EmptySvcContainer,
				ServiceType,
				#svc_details{quantity = ServiceQuantity}
			),
			case billy_client_transaction:reserve(TransactionId, CustomerId, UserId, FullSvcContainer) of
				{ok, accepted} ->
					{accepted, TransactionId};
				{ok, {rejected, Reason}} ->
					% TODO: should not the transaction be stopped here? have it stopped already?
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
