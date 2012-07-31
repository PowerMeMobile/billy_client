-module(billy_client_transaction_dispatcher).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	dispatch/2,
	reserve/4,
	commit/2,
	rollback/2
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

-include("logging.hrl").
-include_lib("billy_common/include/billy_transaction_piqi.hrl").
-include_lib("billy_common/include/service.hrl").
-include_lib("billy_common/include/gen_server_spec.hrl").

-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================

% -spec start_link(pid()) -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, {}, []).

reserve(TranID, SessPid, CID, Container) ->
	{ok, PiqiContainer} = re_pack(Container),
	ReserveRequest = #billy_transaction_reserve_request{
	    transaction_id = TranID,
	    customer_id = CID,
	    svc_container = PiqiContainer
},
	ReserveRequestDeepList = billy_transaction_piqi:gen_transaction({reserve_request, ReserveRequest}),
	ResponseBin = list_to_binary(ReserveRequestDeepList),
	billy_client_session:send(SessPid, ResponseBin).

commit(SessPid, TranID)->
	Request = {commit_request, #billy_transaction_commit_request{transaction_id = TranID}},
	RequestDeepList = billy_transaction_piqi:gen_transaction(Request),
	ResponseBin = list_to_binary(RequestDeepList),
	billy_client_session:send(SessPid, ResponseBin).

rollback(SessPid, TranID)->
	Request = {rollback_request, #billy_transaction_rollback_request{transaction_id = TranID}},
	RequestDeepList = billy_transaction_piqi:gen_transaction(Request),
	ResponseBin = list_to_binary(RequestDeepList),
	billy_client_session:send(SessPid, ResponseBin).

% -spec dispatch(binary(), binary(), pid()) -> ok.
dispatch(SessPid, {_, BinResponse}) ->
	{ok, Pid} = supervisor:start_child(billy_client_transaction_dispatcher_sup, []),
	gen_server:cast(Pid, {dispatch, SessPid, BinResponse}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init({}) ->
	?log_debug("billy_transaction_dispatcher:init", []),
	{ok, #state{}}.

handle_call(Request, _From, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast({dispatch, SessPid, Bin}, State = #state{}) ->
	{_TranType, TranData} = billy_transaction_piqi:parse_transaction(Bin),
	dispatch_internal(SessPid, TranData),
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

% -spec dispatch_internal(binary(), billy_transaction_reserve_request()
% 				| billy_transaction_commit_request()
% 				| billy_transaction_rollback_request()) -> term().

dispatch_internal(SessionPID, #billy_transaction_reserve_response{
	transaction_id = TranID,
	result = Result
}) ->
	billy_client_transaction:reserved({SessionPID, TranID, Result});

dispatch_internal(SessionPID, #billy_transaction_commit_response{
	transaction_id = TranID,
	result = Result
}) ->
	billy_client_transaction:commited({SessionPID, TranID, Result});

dispatch_internal(SessionPID, #billy_transaction_rollback_response{
	transaction_id = TranID,
	result = Result
}) ->
	billy_client_transaction:rolledback({SessionPID, TranID, Result}).

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
