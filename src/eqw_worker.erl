%% Description: eqw_worker
%%

-module(eqw_worker).

-behaviour(gen_server).

%% Management API
-export([start_link/4, new/4]).

%% API
-export([handle_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management API -------------------------------------------------------------

start_link(PoolRef, Bridge, Worker, Opts) ->
    gen_server:start_link(?MODULE, [PoolRef, Bridge, Worker, Opts], []).

new(PoolRef, Bridge, Worker, Opts) ->
    eqw_worker_sup:add_child([PoolRef, Bridge, Worker, Opts]).

%% API ------------------------------------------------------------------------

handle_message(Pid, Msg) ->
    gen_server:cast(Pid, {handle_message, Msg}).

%% gen_server callbacks -------------------------------------------------------

init([PoolRef, {Bridge, BridgeState}, {Worker, WorkerArgs}, Opts]) ->
    case catch Worker:init(WorkerArgs) of
        {ok, WorkerState} ->
            {ok, #{pool_ref => PoolRef,
                   bridge => Bridge,
                   bridge_state => BridgeState,
                   worker => Worker,
                   worker_state => WorkerState,
                   opts => Opts}};
        {error, Reason} ->
            {stop, {gen_eqw, Worker, Reason}};
        {'EXIT', Reason} ->
            exit({gen_eqw, Worker, Reason})
    end.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({handle_message, Msg}, State) ->
    #{bridge := Bridge,
      bridge_state := BridgeState,
      worker := Worker,
      worker_state := WorkerState,
      pool_ref:=PoolRef,
      opts := #{timer_interval := Interval}} = State,
    {ok, TPid} = eqw_timer:start_link(Interval, {Bridge, BridgeState}, Msg),
    DecodedMsg = decode(Bridge, BridgeState, Msg),
    case catch Worker:handle_msg(DecodedMsg, PoolRef, WorkerState) of
        ok ->
            inc(bridge_msg_handled, 1),
            case catch Bridge:ack(Msg, BridgeState) of
                {'EXIT', Reason} ->
                    inc(bridge_msg_ack_crash, 1),
                    exit({gen_eqw_bridge, Bridge, Reason});
                _ ->
                    inc(bridge_msg_ack, 1),
                    eqw_timer:stop(TPid),
                    {stop, normal, State}
            end;
        error ->
            inc(bridge_msg_handled, 1),
            eqw_timer:stop(TPid),
            {stop, normal, State};
        {'EXIT', Reason} ->
            inc(bridge_msg_handled_crash, 1),
            exit({gen_eqw_bridge, Bridge, Reason})
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

decode(Bridge, BridgeState, Msg) ->
    case catch Bridge:decode(Msg, BridgeState) of
        {'EXIT', Reason} ->
            inc(bridge_msg_decode_crash, 1),
            exit({gen_eqw_bridge, Bridge, Reason});
        DecodedMsg ->
            DecodedMsg
    end.

inc(Counter, Steps) ->
    eqw_info:inc(Counter, Steps).
