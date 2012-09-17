-module(billy_client_session).

-behaviour(gen_billy_session_c).

%% API
-export([
	start_link/3,

	start_session/4,
	stop_session/1,

	start_transaction/1,
	send/2
]).

%% gen_billy_session_c callbacks
-export([
	init/1,
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
	client_id,
	client_pw,
	last_trans_id,
	bind_result_callback,
	unbind_result_callback
}).

%% ===================================================================
%% API
%% ===================================================================

start_link(Socket, ClientId, ClientPw) ->
	gen_billy_session_c:start_link(Socket, ?MODULE, [ClientId, ClientPw]).

start_session(Host, Port, ClientId, ClientPw) ->
	et:trace_me(85, client, server, connect, []),
	case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
		{ok, Socket} ->
			%% start new session and pass socket to it.
			{ok, SessionPid} = billy_client_session_sup:start_session(Socket, ClientId, ClientPw),
			{ok, FSM} = gen_server:call(SessionPid, get_fsm),
			ok = gen_tcp:controlling_process(Socket, FSM),
			inet:setopts(Socket, [{active, once}]),
			%% wait for bind result.
			case gen_server:call(SessionPid, wait_for_bind_result, 10000) of
				{ok, accepted} ->
					%% all is ok.
					{ok, SessionPid};
				{ok, rejected} ->
					et:trace_me(85, client, server, disconnect, []),
					%% stop the session.
					gen_server:cast(SessionPid, disconnect),
					{error, invalid_credentials};
				Error ->
					Error
			end;
		Error ->
			Error
	end.

stop_session(SessionPid) ->
	et:trace_me(85, client, server, disconnect, []),
	{ok, FSM} = gen_server:call(SessionPid, get_fsm),
	gen_billy_session_c:reply_unbind(FSM, [{reason, normal}]),
	%% wait for unbind result.
	{ok, unbound} = gen_server:call(SessionPid, wait_for_unbind_result, 10000),
	gen_server:cast(SessionPid, disconnect).

start_transaction(SessionPid) ->
	gen_server:call(SessionPid, start_transaction).

send(SessionPid, ResponseBin) ->
	{ok, FSM} = gen_server:call(SessionPid, get_fsm),
	gen_billy_session_c:reply_data_pdu(FSM, ResponseBin).

%% ===================================================================
%% gen_billy_session_c callbacks
%% ===================================================================

init([ClientId, ClientPw]) ->
	{ok, #state{
		client_id = ClientId,
		client_pw = ClientPw,
		last_trans_id = 0
	}}.

handle_call(start_transaction, _From, _FSM, State = #state{last_trans_id = TransId}) ->
	NewTransId =
		case TransId < 16#7FFFFFFFFFFFFFFF of
			false ->
				TransId + 1;
			true ->
				1
		end,
	{ok, Pid} = billy_client_transaction:start_transaction({self(), NewTransId}),
	{reply, {ok, Pid}, State#state{last_trans_id = NewTransId}};

handle_call(get_fsm, _From, FSM, State = #state{}) ->
	{reply, {ok, FSM}, State};

handle_call(wait_for_bind_result, From, _FSM, State = #state{}) ->
	{noreply, State#state{bind_result_callback = From}};

handle_call(wait_for_unbind_result, From, _FSM, State = #state{}) ->
	{noreply, State#state{unbind_result_callback = From}};

handle_call(Request, _From, _FSM, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(disconnect, FSM, State = #state{}) ->
	gen_billy_session_c:reply_bye(FSM, []),
	?log_debug("sending bye...", []),
	{stop, normal, State};

handle_cast(Request, _FSM, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_hello(#billy_session_hello{}, FSM, State = #state{
	client_id = ClientId,
	client_pw = ClientPw
}) ->
	?log_debug("got hello...", []),
	gen_billy_session_c:reply_bind(FSM, [
		{client_id, ClientId},
		{client_pw, ClientPw}
	]),
	?log_debug("sending bind request...", []),
	{noreply, State}.

handle_bind_accept(#billy_session_bind_response{}, _FSM, State = #state{
	bind_result_callback = ReplyTo
}) ->
	?log_debug("bind response accepted", []),
	gen_server:reply(ReplyTo, {ok, accepted}),
	{noreply, State}.

handle_bind_reject(#billy_session_bind_response{}, FSM, State = #state{
	bind_result_callback = ReplyTo
}) ->
	?log_debug("bind response rejected", []),
	gen_server:reply(ReplyTo, {ok, rejected}),
	{noreply, State#state{bind_result_callback = undefined}}.

handle_require_unbind(#billy_session_require_unbind{}, _FSM, State) ->
	{noreply, State}.

handle_unbind_response(#billy_session_unbind_response{}, _FSM, State = #state{
	unbind_result_callback = ReplyTo
}) ->
	?log_debug("unbind response", []),
	gen_server:reply(ReplyTo, {ok, unbound}),
	{noreply, State#state{unbind_result_callback = undefined}}.

handle_bye(#billy_session_bye{}, _FSM, State) ->
	{noreply, State}.

handle_data_pdu(ResponseData = #billy_session_data_pdu{}, _FSM, State) ->
	billy_client_transaction_dispatcher:dispatch_response(self(), ResponseData),
	{noreply, State}.
