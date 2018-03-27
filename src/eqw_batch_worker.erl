%% Description: eqw_batch_worker
%%

-module(eqw_batch_worker).

-behaviour(gen_server).

%% Management API
-export([start_link/4, new/4]).

%% API
-export([handle_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Management API -------------------------------------------------------------

start_link(PoolRef, Bridge, Worker, Opts) ->
    gen_server:start_link(?MODULE, [PoolRef, Bridge, Worker, Opts], []).

new(PoolRef, Bridge, Worker, Opts) ->
    eqw_worker_sup:add_child([PoolRef, Bridge, Worker, Opts]).

%% API ------------------------------------------------------------------------

handle_message(Pid, Msg) ->
    gen_server:call(Pid, {handle_message, Msg}).

%% gen_server callbacks -------------------------------------------------------

init([PoolRef, {Bridge, BridgeState}, {Worker, WorkerArgs}, Opts]) ->
    case catch Worker:init(WorkerArgs) of
        {ok, WorkerState} ->
            #{batch_timeout := BatchTimeout} = Opts,
            {ok, TRef} = timer:send_after(BatchTimeout, self(), batch_timeout),
            {ok, #{pool_ref => PoolRef,
                   batch_timer => TRef,
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

handle_call({handle_message, Msg}, _, State) ->
    #{bridge := Bridge,
      bridge_state := BridgeState,
      batch := Batch,
      opts := #{timer_interval := Interval,
                max_batch_size := MaxBatchSize}} = State,
    {ok, TPid} = eqw_timer:start_link(Interval, {Bridge, BridgeState}, Msg),
    DecodedMsg = decode(Bridge, BridgeState, Msg),
    NewBatch =[{DecodedMsg, TPid}|Batch],
    case length(NewBatch) < MaxBatchSize of
        true ->
            {reply, {ok, length(NewBatch)}, State#{batch=>NewBatch}};
        false ->
            handle_batch(Batch, State)
    end;
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(batch_timeout, #{batch:=Batch} = State) ->
    handle_batch(Batch, State);
handle_info(_, State) ->
    {noreply, State}.

%% Internal -------------------------------------------------------------------

handle_batch(Batch, State) ->
    #{bridge := Bridge,
      bridge_state := BridgeState,
      worker := Worker,
      worker_state := WorkerState,
      pool_ref := PoolRef} = State,
    case catch Worker:handle_msg(Batch, PoolRef, WorkerState) of
        ok ->
            inc(bridge_msg_handled, length(Batch)),
            ok = ack_batch(Batch, Bridge, BridgeState),
            {stop, normal, State};
        error ->
            inc(bridge_msg_handled, length(Batch)),
            [eqw_timer:stop(TPid) || {_, TPid} <- Batch],
            {stop, normal, State};
        {'EXIT', Reason} ->
            inc(bridge_msg_handled_crash, 1),
            exit({gen_eqw_bridge, Bridge, Reason})
    end.

ack_batch(Batch, Bridge, BridgeState) ->
    case catch [begin Bridge:ack(Msg, BridgeState), TPid end || {Msg, TPid} <- Batch] of
        {'EXIT', Reason} ->
            inc(bridge_msg_ack_crash, length(Batch)),
            exit({gen_eqw_bridge, Bridge, Reason});
        TPids ->
            inc(bridge_msg_ack, length(Batch)),
            [eqw_timer:stop(TPid) || TPid <- TPids],
            ok
    end.

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
