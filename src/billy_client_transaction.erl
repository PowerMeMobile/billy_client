-module(billy_client_transaction).

-behaviour(gen_fsm).

%% API
-export([
	start_link/1,
	start/1,
	get/1,
	reserve/3,
	reserved/1,
	commit/1,
	commited/1,
	rollback/1,
	rolledback/1
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
	tranid,
	sesspid,
	caller
}).

-type billy_transaction_transaction_id() :: {binary(), integer()}.

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(billy_transaction_transaction_id()) -> {ok, pid()}.
start_link({SessionID, TranID}) ->
	gen_fsm:start_link(?MODULE, {SessionID, TranID}, []).

-spec start(binary()) -> {ok, binary()}.
start({SessionPID, TranID}) ->
	{ok, TranPid} = supervisor:start_child(billy_client_transaction_sup, [{SessionPID, TranID}]),
	{ok, TranPid}.

-spec get(billy_transaction_transaction_id()) -> {ok, pid()} | {error, no_transaction}.
get({SessionPID, TranID}) ->
	case gproc:lookup_pids({n, l, {?MODULE, {SessionPID, TranID}} }) of
		[TranSrv] ->
			{ok, TranSrv};
		[] ->
			{error, no_transaction}
	end.

-spec reserve(billy_transaction_transaction_id(), integer(), term()) -> term().
reserve(TranSrv, CID, Container) ->
	gen_fsm:sync_send_event(TranSrv, {reserve, CID, Container}).

-spec commit(billy_transaction_transaction_id()) -> term().
commit(TranSrv) ->
	gen_fsm:sync_send_event(TranSrv, commit).

-spec rollback(billy_transaction_transaction_id()) -> term().
rollback(TranSrv) ->
	gen_fsm:sync_send_event(TranSrv, rollback).

reserved({SessionPID, TranID, Result}) ->
	{ok, TranSrv} = ?MODULE:get({SessionPID, TranID}),
	gen_fsm:send_event(TranSrv, {reserved, Result}).

commited({SessionPID, TranID, Result}) ->
	{ok, TranSrv} = ?MODULE:get({SessionPID, TranID}),
	gen_fsm:send_event(TranSrv, {commited, Result}).

rolledback({SessionPID, TranID, Result}) ->
	% io:format("rolledback... ~p~n", [TranID]),
	{ok, TranSrv} = ?MODULE:get({SessionPID, TranID}),
	gen_fsm:send_event(TranSrv, {rolledback, Result}).

%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================

init({SessionPID, TranID}) ->
	?set_pname("billy_client_transaction"),
	gproc:add_local_name({?MODULE, {SessionPID, TranID}}),
	{ok, st_initial, #state{tranid = TranID, sesspid = SessionPID}, ?TRANSACTION_TIMEOUT}.

handle_event(Event, _StateName, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
	{stop, {bad_arg, Event}, bad_arg, StateData}.

handle_info(Info, _StateName, StateData) ->
	{stop, {bad_arg, Info}, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

%% ===================================================================
%% INITIAL STATE
%% ===================================================================

st_initial(timeout, StateData = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, StateData};

st_initial(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_initial({reserve, CID, Container}, From, StateData = #state{ tranid = TranID, sesspid = SessPid }) ->
	billy_client_transaction_dispatcher:reserve(TranID, SessPid, CID, Container),
	{next_state, st_reserving, StateData#state{caller = From}, ?TRANSACTION_TIMEOUT};

st_initial(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.

%% ===================================================================
%% RESERVING STATE
%% ===================================================================

st_reserving(timeout, StateData = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, StateData};

st_reserving({reserved, Result}, StateData = #state{caller = Caller }) ->
	gen_fsm:reply(Caller, {ok, Result}),
	{next_state, st_reserved, StateData, ?TRANSACTION_TIMEOUT};

st_reserving(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_reserving(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.

%% ===================================================================
%% RESERVED STATE
%% ===================================================================

st_reserved(timeout, StateData = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, StateData};

st_reserved(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_reserved(commit, From, StateData = #state{ sesspid = SessPid, tranid = TranID}) ->
	billy_client_transaction_dispatcher:commit(SessPid, TranID),
	{next_state, st_commiting, StateData#state{caller = From}, ?TRANSACTION_TIMEOUT};

st_reserved(rollback, From, StateData = #state{sesspid = SessPid, tranid = TranID}) ->
	billy_client_transaction_dispatcher:rollback(SessPid, TranID),
	{next_state, st_rollingback, StateData#state{caller = From}, ?TRANSACTION_TIMEOUT};

st_reserved(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.

%% ===================================================================
%% COMMITING STATE
%% ===================================================================

st_commiting(timeout, StateData = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, StateData};

st_commiting({commited, Result}, StateData = #state{caller = Caller}) ->
	gen_fsm:reply(Caller, {ok, {commited, Result}}),
	{stop, normal, StateData};

st_commiting(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_commiting(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.

%% ===================================================================
%% ROLLINGBACK STATE
%% ===================================================================

st_rollingback(timeout, StateData = #state{}) ->
	?log_debug("timeout occured...", []),
	{stop, normal, StateData};

st_rollingback({rolledback, Result}, StateData = #state{caller = Caller}) ->
	gen_fsm:reply(Caller, {ok, {rolledback, Result}}),
	{stop, normal, StateData};

st_rollingback(Event, StateData) ->
	{stop, {bad_arg, Event}, StateData}.

st_rollingback(Event, _From, StateData) ->
	{stop, {bad_arg, Event}, {error, bad_arg}, StateData}.
