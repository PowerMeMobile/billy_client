-module(billy_client_transaction).

-behaviour(gen_fsm).

%% API
-export([
	start_link/1,

	% client calls
	start_transaction/1,
	reserve/3,
	commit/1,
	rollback/1,

	% server responses
	reserved/2,
	commited/2,
	rolledback/2
]).

%% gen_fsm callbacks
-export([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4,

	st_initial/2,
	st_initial/3,

	st_reserving/2,
	st_reserving/3,

	st_reserved/2,
	st_reserved/3,

	st_commiting/2,
	st_commiting/3,

	st_rollingback/2,
	st_rollingback/3
]).

-include_lib("billy_common/include/logging.hrl").
-include_lib("billy_common/include/billy_transaction_piqi.hrl").
-include_lib("billy_common/include/service.hrl").
-include_lib("billy_common/include/transaction.hrl").
-include_lib("billy_common/include/gen_fsm_spec.hrl").

-spec st_initial(gen_fsm_event(), gen_fsm_state_data()) -> gen_fsm_handle_event_result().
-spec st_initial(gen_fsm_event(), gen_fsm_from(), gen_fsm_state_data()) -> gen_fsm_handle_sync_event_result().
-spec st_reserved(gen_fsm_event(), gen_fsm_state_data()) -> gen_fsm_handle_event_result().
-spec st_reserved(gen_fsm_event(), gen_fsm_from(), gen_fsm_state_data()) -> gen_fsm_handle_sync_event_result().

-record(state, {
	session_pid,
	transaction_id,
	caller
}).

-type billy_transaction_id() :: {SessionPid::pid(), TransactionId::integer()}.

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(billy_transaction_id()) -> {ok, pid()}.
start_link({SessionPid, TransactionId}) ->
	gen_fsm:start_link(?MODULE, [{SessionPid, TransactionId}], []).

%%
%% Client calls
%%

-spec start_transaction(billy_transaction_id()) -> {ok, TransactionPid::pid()}.
start_transaction({SessionPid, TransactionId}) ->
	billy_client_transaction_sup:start_transaction({SessionPid, TransactionId}).

-spec reserve(pid(), integer(), term()) -> term().
reserve(TransactionPid, CustomerId, Container) ->
	gen_fsm:sync_send_event(TransactionPid, {reserve, CustomerId, Container}).

-spec commit(pid()) -> term().
commit(TransactionPid) ->
	gen_fsm:sync_send_event(TransactionPid, commit).

-spec rollback(pid()) -> term().
rollback(TransactionPid) ->
	gen_fsm:sync_send_event(TransactionPid, rollback).

%%
%% Server responses
%%

-spec reserved(billy_transaction_id(), term()) -> ok.
reserved({SessionPid, TransactionId}, Result) ->
	{ok, TransactionPid} = get_transaction_pid({SessionPid, TransactionId}),
	gen_fsm:send_event(TransactionPid, {reserved, Result}).

-spec commited(billy_transaction_id(), term()) -> ok.
commited({SessionPid, TransactionId}, Result) ->
	{ok, TransactionPid} = get_transaction_pid({SessionPid, TransactionId}),
	gen_fsm:send_event(TransactionPid, {commited, Result}).

-spec rolledback(billy_transaction_id(), term()) -> ok.
rolledback({SessionPid, TransactionId}, Result) ->
	{ok, TransactionPid} = get_transaction_pid({SessionPid, TransactionId}),
	gen_fsm:send_event(TransactionPid, {rolledback, Result}).

%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================

init([{SessionPid, TransactionId}]) ->
	?set_pname("billy_client_transaction"),
	gproc:add_local_name({?MODULE, {SessionPid, TransactionId}}),
	{ok, st_initial, #state{session_pid = SessionPid, transaction_id = TransactionId}, ?TRANSACTION_TIMEOUT}.

handle_event(Event, _StateName, State) ->
	{stop, {bad_arg, Event}, State}.

handle_sync_event(Event, _From, _StateName, State) ->
	{stop, {bad_arg, Event}, bad_arg, State}.

handle_info(Info, _StateName, State) ->
	{stop, {bad_arg, Info}, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

%% ===================================================================
%% INITIAL STATE
%% ===================================================================

st_initial(timeout, State = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, State};

st_initial(Event, State) ->
	{stop, {bad_arg, Event}, State}.

st_initial({reserve, CustomerId, Container}, From, State = #state{
	session_pid = SessionPid,
	transaction_id = TransactionId
}) ->
	billy_client_transaction_dispatcher:reserve(SessionPid, TransactionId, CustomerId, Container),
	{next_state, st_reserving, State#state{caller = From}, ?TRANSACTION_TIMEOUT};

st_initial(Event, _From, State) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, State}.

%% ===================================================================
%% RESERVING STATE
%% ===================================================================

st_reserving(timeout, State = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, State};

st_reserving({reserved, Result}, State = #state{caller = Caller}) ->
	gen_fsm:reply(Caller, {ok, Result}),
	{next_state, st_reserved, State, ?TRANSACTION_TIMEOUT};

st_reserving(Event, State) ->
	{stop, {bad_arg, Event}, State}.

st_reserving(Event, _From, State) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, State}.

%% ===================================================================
%% RESERVED STATE
%% ===================================================================

st_reserved(timeout, State = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, State};

st_reserved(Event, State) ->
	{stop, {bad_arg, Event}, State}.

st_reserved(commit, From, State = #state{
	session_pid = SessionPid,
	transaction_id = TransactionId
}) ->
	billy_client_transaction_dispatcher:commit(SessionPid, TransactionId),
	{next_state, st_commiting, State#state{caller = From}, ?TRANSACTION_TIMEOUT};

st_reserved(rollback, From, State = #state{
	session_pid = SessionPid,
	transaction_id = TransactionId
}) ->
	billy_client_transaction_dispatcher:rollback(SessionPid, TransactionId),
	{next_state, st_rollingback, State#state{caller = From}, ?TRANSACTION_TIMEOUT};

st_reserved(Event, _From, State) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, State}.

%% ===================================================================
%% COMMITING STATE
%% ===================================================================

st_commiting(timeout, State = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, State};

st_commiting({commited, Result}, State = #state{caller = Caller}) ->
	gen_fsm:reply(Caller, {ok, {commited, Result}}),
	{stop, normal, State};

st_commiting(Event, State) ->
	{stop, {bad_arg, Event}, State}.

st_commiting(Event, _From, State) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, State}.

%% ===================================================================
%% ROLLINGBACK STATE
%% ===================================================================

st_rollingback(timeout, State = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, State};

st_rollingback({rolledback, Result}, State = #state{caller = Caller}) ->
	gen_fsm:reply(Caller, {ok, {rolledback, Result}}),
	{stop, normal, State};

st_rollingback(Event, State) ->
	{stop, {bad_arg, Event}, State}.

st_rollingback(Event, _From, State) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec get_transaction_pid(billy_transaction_id()) -> {ok, pid()} | {error, no_transaction}.
get_transaction_pid({SessionPid, TransactionId}) ->
	case gproc:lookup_pids({n, l, {?MODULE, {SessionPid, TransactionId}}}) of
		[TransactionPid] ->
			{ok, TransactionPid};
		[] ->
			{error, no_transaction}
	end.
