-module(billy_client_session).

-behaviour(gen_billy_session_c).

-export([
	start/1,
	start_link/2,
	process/1
]).
-export([
	init/2,
	handle_call/4,
	handle_cast/3,

	handle_hello/3,
	handle_bind_accept/3,
	handle_bind_reject/3,
	handle_require_unbind/3,
	handle_unbind_response/3,
	handle_bye/3
]).

-include_lib("billy_common/include/billy_session_piqi.hrl").

-include("logging.hrl").

start(Sock) ->
	{ok, Sess} = supervisor:start_child(billy_client_session_sup, [ Sock, {} ]),
	gen_billy_session_c:pass_socket_control(Sess, Sock),
	{ok, Sess}.

start_link(Sock, Args) ->
	gen_billy_session_c:start_link(Sock, ?MODULE, Args).

-record(state, {}).

init(_Args, _Peer) ->
	{ok, #state{}}.


handle_call(Request, _From, _Peer, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(start_processing, _Peer, State = #state{}) ->
	{noreply, State};

handle_cast(Request, _Peer, State = #state{}) ->
	{stop, {bad_arg, Request}, State}.




handle_hello(#billy_session_hello{}, _Peer, State) ->
	{noreply, State}.

handle_bind_accept(#billy_session_bind_response{}, _Peer, State) ->
	{noreply, State}.

handle_bind_reject(#billy_session_bind_response{}, _Peer, State) ->
	{noreply, State}.

handle_require_unbind(#billy_session_require_unbind{}, _Peer, State) ->
	{noreply, State}.

handle_unbind_response(#billy_session_unbind_response{}, _Peer, State) ->
	{noreply, State}.

handle_bye(#billy_session_bye{}, _Peer, State) ->
	{noreply, State}.



%%% API %%%

process(Session) ->
	gen_server:cast(Session, start_processing).



