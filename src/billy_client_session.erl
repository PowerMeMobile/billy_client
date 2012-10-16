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
	terminate/3,

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
	session_id,
	last_trans_id,
	bind_result_callback,
	unbind_result_callback
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(
	Socket::gen_tcp:socket(),
	ClientId::binary(),
	ClientPw::binary()
) ->
	{ok, pid()} | {error, Reason::any()}.
start_link(Socket, ClientId, ClientPw) ->
	gen_billy_session_c:start_link(Socket, ?MODULE, [ClientId, ClientPw]).

-spec start_session(
	Host::string(),
	Port::integer(),
	ClientId::binary(),
	ClientPw::binary()
) ->
	{ok, SessionId::binary()} | {error, Reason::any()}.
start_session(Host, Port, ClientId, ClientPw) ->
	et:trace_me(85, client, server, connect, []),
	case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
		{ok, Socket} ->
			%% start new session and pass socket to it.
			{ok, SessionPid} = billy_client_session_sup:start_session(Socket, ClientId, ClientPw),
			%% calling process will monitor the session process.
			erlang:link(SessionPid),
			{ok, FSM} = gen_server:call(SessionPid, get_fsm),
			%% pass socket control to FSM.
			ok = gen_tcp:controlling_process(Socket, FSM),
			inet:setopts(Socket, [{active, once}]),
			%% wait for bind result.
			case gen_server:call(SessionPid, wait_for_bind_result, 5000) of
				{ok, {accepted, SessionId}} ->
					{ok, SessionId};
				{ok, {rejected, <<"invalid_credentials">>}} ->
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

-spec stop_session(SessionId::binary()) -> ok | {error, Reason::any()}.
stop_session(SessionId) ->
	et:trace_me(85, client, server, disconnect, []),
	{ok, SessionPid} = get_session_pid(SessionId),
	{ok, FSM} = gen_server:call(SessionPid, get_fsm),
	gen_billy_session_c:reply_unbind(FSM, [{reason, normal}]),
	%% wait for unbind result.
	{ok, unbound} = gen_server:call(SessionPid, wait_for_unbind_result, 5000),
	gen_server:cast(SessionPid, disconnect).

-spec start_transaction(SessionId::binary()) -> {ok, TransactionId::any()} | {error, Reason::any()}.
start_transaction(SessionId) ->
	{ok, SessionPid} = get_session_pid(SessionId),
	gen_server:call(SessionPid, start_transaction).

-spec send(SessionId::binary(), ResponseBin::binary()) -> ok.
send(SessionId, ResponseBin) ->
	{ok, SessionPid} = get_session_pid(SessionId),
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

handle_call(start_transaction, _From, _FSM, State = #state{
	session_id = SessionId,
	last_trans_id = TransId
}) ->
	NewTransId =
		case TransId < 16#7FFFFFFFFFFFFFFF of
			true ->
				TransId + 1;
			false ->
				1
		end,
	{ok, _TransactionPid} = billy_client_transaction:start_transaction({SessionId, NewTransId}),
	{reply, {ok, {SessionId, NewTransId}}, State#state{last_trans_id = NewTransId}};

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

handle_hello(#billy_session_hello{
	server_version = ServerVersion,
	session_id = SessionId,
	bind_request_timeout = Timeout
}, FSM, State = #state{
	client_id = ClientId,
	client_pw = ClientPw
}) ->
	?log_debug("got hello...", []),
	?log_debug("server version: ~p", [ServerVersion]),
	?log_debug("session id: ~p", [SessionId]),
	?log_debug("timeout: ~p", [Timeout]),
	gproc:add_local_name({?MODULE, SessionId}),
	gen_billy_session_c:reply_bind(FSM, [
		{client_id, ClientId},
		{client_pw, ClientPw}
	]),
	?log_debug("sending bind request...", []),
	{noreply, State#state{session_id = SessionId}}.

handle_bind_accept(#billy_session_bind_response{}, _FSM, State = #state{
	session_id = SessionId,
	bind_result_callback = ReplyTo
}) ->
	?log_debug("bind response accepted", []),
	gen_server:reply(ReplyTo, {ok, {accepted, SessionId}}),
	{noreply, State}.

handle_bind_reject(#billy_session_bind_response{result = {reject, Reason}}, FSM, State = #state{
	bind_result_callback = ReplyTo
}) ->
	?log_debug("bind response rejected with: ~p", [Reason]),
	gen_server:reply(ReplyTo, {ok, {rejected, Reason}}),
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

handle_data_pdu(ResponseData = #billy_session_data_pdu{}, _FSM, State = #state{
	session_id = SessionId
}) ->
	billy_client_transaction_dispatcher:dispatch_response(SessionId, ResponseData),
	{noreply, State}.

terminate(Reason, _FSM, State = #state{}) ->
	?log_debug("~p", [Reason]),
	ok.

%% ===================================================================
%% Internal
%% ===================================================================

-spec get_session_pid(binary()) -> {ok, pid()} | {error, no_session}.
get_session_pid(SessionId) ->
	case gproc:lookup_local_name({?MODULE, SessionId}) of
		undefined ->
			{error, no_session};
		SessionPid ->
			{ok, SessionPid}
	end.
