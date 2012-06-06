-module(billy_client_session).

-behaviour(gen_fsm).
-export([
	start_link/4,
	unbind_async/1,
	disconnect_async/1
]).
-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4,

	st_initial/2,
	st_initial/3,

	st_awaiting_hello/2,
	st_awaiting_hello/3,

	st_binding/2,
	st_binding/3,

	st_ready/2,
	st_ready/3,

	st_unbinding/2,
	st_unbinding/3
	]).

-include("logging.hrl").
-include_lib("billy_common/include/billy_session_piqi.hrl").

-record(state, {
	sock,
	client_id,
	client_pw
}).

-define(DEFAULT_UNBIND_TIMEOUT, 30000).


start_link(Addr, Port, ClientID, ClientPw) ->
	gen_fsm:start_link(?MODULE, {Addr, Port, ClientID, ClientPw}, []).

init({Addr, Port, ClientID, ClientPw}) ->
	{ok, Sock} = gen_tcp:connect(Addr, Port, [binary]),
	?set_pname_fmt("billy_client_session[~p]", [Sock]),
	inet:setopts(Sock, [{active, once}]),

	{ok, st_awaiting_hello, #state{
		sock = Sock,
		client_id = ClientID,
		client_pw = ClientPw
	}}.

handle_event(Event, _StateName, StateData) ->
	% {next_state, StateName, StateData}.
	?log_debug("Got event: ~p", [Event]),
	{stop, {bad_arg, Event}, StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
	% {reply, {error, bad_arg}, StateName, StateData}.
	?log_debug("Got sync_event: ~p", [Event]),
	{stop, {bad_arg, Event}, bad_arg, StateData}.

handle_info({tcp, Sock, TcpData}, StateName, StateData = #state{
	sock = Sock
}) ->
	try
		PDU = billy_protocol_piqi:parse_pdu(TcpData),
		?log_debug("[~p] got PDU: ~p", [StateName, PDU]),
		gen_fsm:send_event(self(), PDU),

		{next_state, StateName, StateData}
	catch
		_EType:_Error ->
			?log_debug("[~p] failed to parse incoming PDU", [StateName]),
			Bye = #billy_session_bye{
				reason = <<"billy.invalid_pdu">>
			},
			ByePDU = billy_protocol_piqi:gen_pdu({bye, Bye}),
			ok = gen_tcp:send(Sock, ByePDU),

			{stop, normal, StateData}
	end;

handle_info(Info, StateName, StateData) ->
	%{next_state, StateName, StateData}.
	?log_debug("[~p] Got info: ~p", [StateName, Info]),
	{stop, {bad_arg, Info}, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

st_awaiting_hello(
	{
		hello,
		#billy_session_hello{server_version = ServerVersion, session_id = SessionID}
	},
	StateData = #state{
		sock = Sock,
		client_id = ClientID,
		client_pw = ClientPw
	}) ->
	?log_debug("[st_awaiting_hello]: got Hello. Server version: ~p, SessionID: ~s", [ServerVersion, uuid:to_string(SessionID)]),
	?set_pname("billy_client_session[~s]", SessionID),

	BindReq = #billy_session_bind_request{
		client_id = ClientID,
		client_pw = ClientPw
	},
	BindPDU = billy_protocol_piqi:gen_pdu({bind_request, BindReq}),
	ok = gen_tcp:send(Sock, BindPDU),
	inet:setopts(Sock, [{active, once}]),

	{next_state, st_binding, StateData};

st_awaiting_hello(Event, StateData) ->
	%{next_state, st_free, StateData}.
	{stop, {bad_arg, Event}, StateData}.

st_awaiting_hello(Event, _From, StateData) ->
	%{reply, {error, bad_arg}, StateName, StateData}.
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.


st_binding({bind_response, #billy_session_bind_response{
	result = accept
}}, StateData = #state{
	sock = Sock
}) ->
	?log_debug("got bind_response -> accept", []),
	inet:setopts(Sock, [{active, once}]),

	{next_state, st_ready, StateData};

st_binding({bind_response, #billy_session_bind_response{
	result = {reject, RejectReason}
}}, StateData = #state{
	sock = Sock
}) ->
	?log_debug("got bind_response -> reject, ~p", [RejectReason]),
	inet:setopts(Sock, [{active, once}]),

	{stop, {bind_rejected, RejectReason}, StateData};

st_binding(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_binding(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.


st_ready({control, unbind, Reason, Timeout}, StateData = #state{
	sock = Sock
}) ->
	UnbindReq = #billy_session_unbind_request{
		reason = Reason%,
		%timeout = Timeout
	},
	UnbindReqPDU = billy_protocol_piqi:gen_pdu({unbind_request, UnbindReq}),
	ok = gen_tcp:send(Sock, UnbindReqPDU),
	inet:setopts(Sock, [{active, once}]),

	{next_state, st_unbinding, StateData, Timeout};

st_ready({bye, #billy_session_bye{reason = ByeReason}}, StateData) ->
	{stop, {session_halt, ByeReason}, StateData};

st_ready(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_ready(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.

st_unbinding({unbind_response, #billy_session_unbind_response{}}, StateData) ->
	{next_state, st_initial, StateData};

st_unbinding(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_unbinding(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.

st_initial(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_initial(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.


%%% API

unbind_async(Session) ->
	gen_fsm:send_event(Session, {control, unbind, <<"normal">>, ?DEFAULT_UNBIND_TIMEOUT}).

unbind_sync(Session) ->
	{error, not_impl}.
	% gen_fsm:sync_send_event(Session, {control, unbind, <<"normal">>, ?DEFAULT_UNBIND_TIMEOUT}, infinity).



disconnect_async(Session) ->
	gen_fsm:send_event(Session, {control, disconnect}).

