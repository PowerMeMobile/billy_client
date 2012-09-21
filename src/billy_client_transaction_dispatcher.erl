-module(billy_client_transaction_dispatcher).

-behaviour(gen_server).

%% API
-export([
	start_link/0,

	reserve/3,
	commit/1,
	rollback/1,

	dispatch_response/2
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include_lib("billy_common/include/logging.hrl").
-include_lib("billy_common/include/billy_transaction_piqi.hrl").
-include_lib("billy_common/include/service.hrl").
-include_lib("billy_common/include/gen_server_spec.hrl").

-type billy_transaction_id() :: {SessionId::binary(), TransactionId::integer()}.

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

% -spec start_link(pid()) -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

reserve({SessionId, TransactionId}, CustomerId, Container) ->
	{ok, PiqiContainer} = re_pack(Container),
	ReserveRequest = #billy_transaction_reserve_request{
	    transaction_id = TransactionId,
	    customer_id = CustomerId,
	    svc_container = PiqiContainer
	},
	ReserveRequestDeepList = billy_transaction_piqi:gen_transaction({reserve_request, ReserveRequest}),
	ResponseBin = list_to_binary(ReserveRequestDeepList),
	billy_client_session:send(SessionId, ResponseBin).

commit({SessionId, TransactionId})->
	Request = {commit_request, #billy_transaction_commit_request{transaction_id = TransactionId}},
	RequestDeepList = billy_transaction_piqi:gen_transaction(Request),
	ResponseBin = list_to_binary(RequestDeepList),
	billy_client_session:send(SessionId, ResponseBin).

rollback({SessionId, TransactionId})->
	Request = {rollback_request, #billy_transaction_rollback_request{transaction_id = TransactionId}},
	RequestDeepList = billy_transaction_piqi:gen_transaction(Request),
	ResponseBin = list_to_binary(RequestDeepList),
	billy_client_session:send(SessionId, ResponseBin).

-spec dispatch_response(binary(), binary()) -> ok.
dispatch_response(SessionId, {_, BinData}) ->
	{ok, Pid} = billy_client_transaction_dispatcher_sup:start_dispatcher(),
	gen_server:cast(Pid, {dispatch_response, SessionId, BinData}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({dispatch_response, SessionId, BinData}, State = #state{}) ->
	{_TransactionType, TransactionData} = billy_transaction_piqi:parse_transaction(BinData),
	dispatch_response_internal(SessionId, TransactionData),
	{stop, normal, State};

handle_cast(Request, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #state{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec dispatch_response_internal(binary(),
	billy_transaction_reserve_response()
  | billy_transaction_commit_response()
  | billy_transaction_rollback_response()) -> term().

dispatch_response_internal(SessionId, #billy_transaction_reserve_response{
	transaction_id = TransactionId,
	result = Result
}) ->
	billy_client_transaction:reserved({SessionId, TransactionId}, Result);

dispatch_response_internal(SessionId, #billy_transaction_commit_response{
	transaction_id = TransactionId,
	result = Result
}) ->
	billy_client_transaction:commited({SessionId, TransactionId}, Result);

dispatch_response_internal(SessionId, #billy_transaction_rollback_response{
	transaction_id = TransactionId,
	result = Result
}) ->
	billy_client_transaction:rolledback({SessionId, TransactionId}, Result).

re_pack(#svc_container{
	details = ListOfPackages
}) ->
	DetailsList = lists:map(fun pack/1, ListOfPackages),
	{ok, #billy_transaction_svc_container{details = DetailsList}}.

pack({SvcTypeID, #svc_details{
	quantity = Quantity
}})->
	#billy_transaction_svc_package{
		svc_type_id = SvcTypeID,
		svc_details = #billy_transaction_svc_details{
			quantity = Quantity
	}}.
