-module(billy_client_session).

-behaviour(gen_billy_session_c).

%% API
-export([
	start_link/2,

	start_session/4,
	stop_session/1,

	start_transaction/1,
	send/2
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
	last_trans_id
}).

%% ===================================================================
%% API
%% ===================================================================

start_link(Sock, Args) ->
	gen_billy_session_c:start_link(Sock, ?MODULE, Args).

start_session(Host, Port, ClientId, ClientPw) ->
	et:trace_me(85, client, server, connect, []),

	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
	{ok, Session} = billy_client_session_sup:start_session(Socket),
	{ok, FSM} = gen_server:call(Session, get_fsm),
	?log_debug("passing control over ~p to ~p", [Socket, FSM]),
	ok = gen_tcp:controlling_process(Socket, FSM),
	inet:setopts(Socket, [{active, once}]),

	{ok, unbound} = gen_fsm:sync_send_all_state_event(FSM, wait_until_st_unbound, infinity),
	%?log_debug("state unbound got...", []),
	gen_billy_session_c:reply_bind(Session, [
		{client_id, ClientId},
		{client_pw, ClientPw}
	]),
	{ok, bound} = gen_fsm:sync_send_all_state_event(FSM, wait_until_st_bound, infinity),
	%?log_debug("state bound got...", []),

	{ok, Session}.

stop_session(Session) ->
	et:trace_me(85, client, server, disconnect, []),

	gen_billy_session_c:reply_unbind(Session, [{reason, normal}]),
	{ok, FSM} = gen_server:call(Session, get_fsm),
	{ok, unbound} = gen_fsm:sync_send_all_state_event(FSM, wait_until_st_unbound, infinity),
	gen_billy_session_c:reply_bye(Session, []).

start_transaction(Session) ->
	gen_server:call(Session, start_transaction).

send(Session, ResponseBin) ->
	gen_billy_session_c:reply_data_pdu(Session, ResponseBin).

%% ===================================================================
%% gen_billy_session_c callbacks
%% ===================================================================

init(_Args, _FSM) ->
	{ok, #state{last_trans_id = 0}}.

handle_call(start_transaction, _From, _FSM, State = #state{last_trans_id = TransId}) ->
	NewTransId =
		case TransId < 16#7FFFFFFFFFFFFFFF of
			false ->
				TransId + 1;
			true ->
				1
		end,
	{ok, Pid} = billy_client_transaction:start({self(), NewTransId}),
	{reply, {ok, Pid}, State#state{last_trans_id = NewTransId}};

handle_call(get_fsm, _From, FSM, State = #state{}) ->
	{reply, {ok, FSM}, State};

handle_call(Request, _From, _FSM, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

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
