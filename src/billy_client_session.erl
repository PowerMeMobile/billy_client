-module(billy_client_session).

-behaviour(gen_billy_session_c).

%% API
-export([
	start/0,
	start_link/2,
	process/1,
	send/2,
	start_transaction/1
]).

%% gen_billy_session_c callbacks
-export([
	init/2,
	handle_call/4,
	handle_cast/3,

	handle_hello/3,
	handle_bind_accept/3,
	handle_bind_reject/3,
	handle_require_unbind/3,
	handle_unbind_response/3,
	handle_bye/3,
	handle_data_pdu/3
]).

-include_lib("billy_common/include/logging.hrl").
-include_lib("billy_common/include/billy_session_piqi.hrl").

-record(state, {
	last_tran_id
}).

% start(Sock) ->
% 	{ok, Sess} = supervisor:start_child(billy_client_session_sup, [Sock, {}]),
% 	gen_billy_session_c:pass_socket_control(Sess, Sock),
% 	{ok, Sess}.

%% ===================================================================
%% API
%% ===================================================================

start_link(Sock, Args) ->
	gen_billy_session_c:start_link(Sock, ?MODULE, Args).

start() ->
	et:trace_me(85, client, server, connect, []),
	{ok, Sock} = gen_tcp:connect("127.0.0.1", 16062, [binary, {active, false}]),
	?log_debug("sock: ~p", [Sock]),
	{ok, Sess} = supervisor:start_child(billy_client_session_sup, [Sock, {}]),
	gen_billy_session_c:pass_socket_control(Sess, Sock),
	{ok, FSM} = gen_server:call(Sess, peer_request),
	{ok, unbound} = gen_fsm:sync_send_all_state_event(FSM, wait_till_st_unbound, infinity),
	?log_debug("state unbound got...", []),
	gen_billy_session_c:reply_bind(Sess, [
		{client_id, <<"client1">>},
		{client_pw, <<"secureme!">>}
	]),
	{ok, bound} = gen_fsm:sync_send_all_state_event(FSM, wait_till_st_bound, infinity),
	?log_debug("state bound got...", []),
	{ok, Sess}.

process(Session) ->
	gen_server:cast(Session, start_processing).

start_transaction(SessionPid) ->
	gen_server:call(SessionPid, start_transaction).

send(Session, ResponseBin) ->
	gen_billy_session_c:reply_data_pdu(Session, ResponseBin).

%% ===================================================================
%% gen_billy_session_c callbacks
%% ===================================================================

init(_Args, _FSM) ->
	{ok, #state{last_tran_id = 0}}.

handle_call(start_transaction, _From, _FSM, State = #state{last_tran_id = TranID}) ->
	if
		TranID < 16#7FFFFFFFFFFFFFFF ->
			NewTransID = TranID + 1,
			{ok, Pid} = billy_client_transaction:start({self(), NewTransID});
		true ->
			NewTransID = 1,
			{ok, Pid} = billy_client_transaction:start({self(), NewTransID}),
			{reply, {ok, Pid}, State}
	end,
	{reply, {ok, Pid}, State#state{last_tran_id = NewTransID}};

handle_call(peer_request, _From, FSM, State = #state{}) ->
	{reply, {ok, FSM}, State};

handle_call(Request, _From, _FSM, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(start_processing, _FSM, State = #state{}) ->
	{noreply, State};

handle_cast(Request, _FSM, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_hello(#billy_session_hello{}, _FSM, State) ->
	{noreply, State}.

handle_bind_accept(#billy_session_bind_response{}, _FSM, State) ->
	{noreply, State}.

handle_bind_reject(#billy_session_bind_response{}, _FSM, State) ->
	{noreply, State}.

handle_require_unbind(#billy_session_require_unbind{}, _FSM, State) ->
	{noreply, State}.

handle_unbind_response(#billy_session_unbind_response{}, _FSM, State) ->
	{noreply, State}.

handle_bye(#billy_session_bye{}, _FSM, State) ->
	{noreply, State}.

handle_data_pdu(Data = #billy_session_data_pdu{}, _FSM, State) ->
	billy_client_transaction_dispatcher:dispatch(self(), Data),
	{noreply, State}.
