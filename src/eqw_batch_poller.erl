%% Description: eqw_poller
%%

-module(eqw_batch_poller).

-behaviour(gen_server).

%% Management API
-export([start_link/4, new/5, stop/1]).

%% API
-export([pause/1, resume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Management API -------------------------------------------------------------

start_link(PoolRef, Bridge, Worker, Opts) ->
    gen_server:start_link(?MODULE, [PoolRef, Bridge, Worker, Opts], []).

new(ParentSup, PoolRef, Bridge, Worker, Opts) ->
    eqw_poller_sup:add_child(ParentSup, [PoolRef, Bridge, Worker, Opts]).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% API ------------------------------------------------------------------------

pause(Pid) ->
    gen_server:cast(Pid, pause).

resume(Pid) ->
    gen_server:cast(Pid, resume).

%% gen_server callbacks -------------------------------------------------------

init([PoolRef, {Bridge, BridgeState}, Worker, Opts]) ->
    #{poll_interval := PollInterval} = Opts,
    timer:send_after(PollInterval, poll),
    {ok, #{pool_ref => PoolRef,
           bridge => Bridge,
           bridge_state => BridgeState,
           worker => Worker,
           pool => #{},
           buffer => [],
           opts => Opts,
           state => running}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(pause, State) ->
    {noreply, State#{state := paused}};
handle_cast(resume, State) ->
    {noreply, State#{state := running}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(poll, #{state := paused} = State) ->
    #{opts := #{poll_interval := PollInterval}} = State,
    timer:send_after(PollInterval, poll),
    {noreply, State};
handle_info(poll, #{state := running} = State) ->
    #{pool_ref := PoolRef,
      bridge := Bridge,
      bridge_state := BridgeState,
      worker := Worker,
      pool := Pool,
      buffer := Buffer,
      opts := #{batch_timeout := _BatchTimeout,
                fetch_size := FetchSize,
                max_workers := _MaxWorkers,
                max_buffer_size := _MaxBufferSize,
                max_batch_size := _BatchSize,
                poll_interval := PollInterval} = Opts} = State,
    BufferLen = length(Buffer),
    WorkerArgs = {PoolRef, {Bridge, BridgeState}, Worker},
    case BufferLen > FetchSize of
        true ->
            {NewBuffer, NewPool} = dispatch_messages(Buffer, Pool, Opts,
                                                     WorkerArgs),
            %% TODO: Check here if we couldn't dispatch anything and adjust
            %%       sleep cycle and inc metrics for 'full_pool'
            timer:send_after(PollInterval, poll),
            {noreply, State#{pool:=NewPool, buffer:=NewBuffer}};
        false ->
            NewFetchSize = min(FetchSize, BufferLen-BufferLen),
            inc(bridge_receive),
            case catch Bridge:recv(NewFetchSize, BridgeState) of
                {error, _Reason} ->
                    inc(bridge_receive_error),
                    timer:send_after(PollInterval, poll),
                    {noreply, State};
                {'EXIT', _Reason} ->
                    inc(bridge_receive_crash),
                    timer:send_after(PollInterval, poll),
                    {noreply, State};
                {ok, Msgs} ->
                    inc(bridge_receive_msgs, length(Msgs)),
                    {ok, NewBuffer} = split_messages(Msgs, Worker),
                    {NewBuffer, NewPool} = dispatch_messages(NewBuffer, Pool,
                                                             Opts, WorkerArgs),
                    erlang:send(self(), poll),
                    {noreply, State#{pool:=NewPool, buffer:=NewBuffer}}
            end
    end;
handle_info({'DOWN', _, _, Pid, normal}, #{pool := Pool} = State) ->
    inc(worker_handled_msg),
    {noreply, State#{pool := maps:remove(Pid, Pool)}};
handle_info({'DOWN', _, _, Pid, _}, #{pool := Pool} = State) ->
    inc(worker_crashed),
    {noreply, State#{pool := maps:remove(Pid, Pool)}};
handle_info(_, State) ->
    {noreply, State}.

%% Internal -------------------------------------------------------------------

dispatch_messages(Buffer, Pool, WorkerArgs, Opts) ->
    dispatch_messages(Buffer, Pool, WorkerArgs, Opts, []).

dispatch_messages([{Key, Msg}|Buffer], Pool, WorkerArgs, Opts, Res) ->
    #{max_pool_size:=MaxPoolSize, max_batch_size:=MaxBatchSize} = Opts,
    case maps:find(Key, Pool) of
        {ok, #{size:=Size, pid:=Pid}} when Size < MaxBatchSize ->
            {ok, NewSize} = eqw_batch_worker:handle_message(Pid, Msg),
            NewPool = Pool#{pid:=Pid, size:=NewSize},
            dispatch_messages(Buffer, NewPool, Opts, WorkerArgs, Res);
        {ok, _} ->
            dispatch_messages(Buffer, Pool, Opts, WorkerArgs, [{Key, Msg}|Res]);
        error ->
            case size(Pool) < MaxPoolSize of
                true ->
                    {PoolRef, Bridge, Worker} = WorkerArgs,
                    {ok, Pid} = eqw_worker:new(PoolRef, Bridge, Worker, Opts),
                    _Ref = monitor(process, Pid),
                    {ok, NewSize} = eqw_batch_worker:handle_message(Pid, Msg),
                    dispatch_messages(Buffer, Pool#{pid=>Pid, size=>NewSize},
                                      Opts, WorkerArgs, Res);
                false ->
                    dispatch_messages(Buffer, Pool, Opts,
                                      WorkerArgs, [{Key, Msg}|Res])
            end
    end;
dispatch_messages([], Pool, _, _, Res) ->
    {lists:reverse(Res), Pool}.

split_messages(Msgs, Worker) ->
    split_messages(Msgs, Worker, []).

split_messages([Msg|Msgs], {Worker, WorkerState}, Res) ->
    case Worker:batch_split_key(Msg, WorkerState) of
        {ok, Key, ParsedMsg} ->
            split_messages(Msgs, {Worker, WorkerState}, [{Key, ParsedMsg}|Res]);
        {error, Error} ->
            %%TODO: How to propagate split-errors?
            exit({gen_eqw_batch_poller, split_message, Worker, Error});
        {'EXIT', Reason} ->
            exit({gen_eqw_batch_poller, split_message, Worker, Reason})
    end;
split_messages([], _, Res) ->
    lists:reverse(Res).

inc(Counter) ->
    eqw_info:inc(Counter).

inc(Counter, Steps) ->
    eqw_info:inc(Counter, Steps).
