%% Description: eqw_poller
%%

-module(eqw_poller).

-behaviour(gen_server).

%% Management API
-export([start_link/4, new/5, stop/1]).

%% API
-export([pause/1, resume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link(PoolRef, Bridge, Worker, Opts) ->
    gen_server:start_link(?MODULE, [PoolRef, Bridge, Worker, Opts], []).

new(ParentSup, PoolRef, Bridge, Worker, Opts) ->
    file:write_file("/tmp/log", "new-poller\n", [append]),
    eqw_poller_sup:add_child(ParentSup, [PoolRef, Bridge, Worker, Opts]).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% Api ------------------------------------------------------------------------

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
           pool => [],
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
    file:write_file("/tmp/log", "poller-handle-info-paused\n", [append]),
    #{opts := #{poll_interval := PollInterval}} = State,
    timer:send_after(PollInterval, poll),
    {noreply, State};
handle_info(poll, #{state := running} = State) ->
    file:write_file("/tmp/log", "poller-handle-info\n", [append]),
    #{pool_ref := PoolRef,
      bridge := Bridge,
      bridge_state := BridgeState,
      worker := Worker,
      pool := Pool,
      opts := #{max_workers := MaxWorkers,
                poll_interval := PollInterval} = Opts} = State,
    case length(Pool) < MaxWorkers of
        false ->
            file:write_file("/tmp/log", "poller-handle-info-false\n", [append]),
            timer:send_after(PollInterval, poll),
            {noreply, State};
        true ->
            file:write_file("/tmp/log", "poller-handle-info-true\n", [append]),
            NumMessages = min(MaxWorkers - length(Pool), 10),
	    file:write_file("/tmp/log", io_lib:fwrite("~p.\n", [NumMessages]) , [append]),
            inc(bridge_receive),
            case catch Bridge:recv(NumMessages, BridgeState) of
                {error, _Reason} ->
	            file:write_file("/tmp/log", "poller-handle-info-true-error\n", [append]),
                    inc(bridge_receive_error),
                    timer:send_after(PollInterval, poll),
                    {noreply, State};
                {'EXIT', _Reason} ->
	            file:write_file("/tmp/log", "poller-handle-info-true-crash\n", [append]),
                    inc(bridge_receive_crash),
                    timer:send_after(PollInterval, poll),
                    {noreply, State};
                {ok, Msgs} ->
	            file:write_file("/tmp/log", "poller-handle-info-messages\n", [append]),
		    file:write_file("/tmp/log", io_lib:fwrite("~p.\n", [Msgs]), [append]),
                    inc(bridge_receive_msgs, length(Msgs)),
                    Pids = setup_workers(PoolRef, {Bridge, BridgeState},
                                         Worker, Opts, length(Msgs)),
                    PidMsgs = lists:zip(Pids, Msgs),
                    [eqw_worker:handle_message(P, M) || {P, M} <- PidMsgs],
                    erlang:send(self(), poll),
                    {noreply, State#{pool := Pids ++ Pool}}
            end
    end;
handle_info({'DOWN', _, _, Pid, normal}, #{pool := Pool} = State) ->
    file:write_file("/tmp/log", "poller-handle-info-down-1\n", [append]),
    inc(worker_handled_msg),
    {noreply, State#{pool := Pool -- [Pid]}};
handle_info({'DOWN', _, _, Pid, _}, #{pool := Pool} = State) ->
    file:write_file("/tmp/log", "poller-handle-info-down-2\n", [append]),
    inc(worker_crashed),
    {noreply, State#{pool := Pool -- [Pid]}};
handle_info(_, State) ->
    file:write_file("/tmp/log", "poller-handle-info-other\n", [append]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

setup_workers(PoolRef, Bridge, Worker, Opts, Num) ->
    setup_workers(PoolRef, Bridge, Worker, Opts, Num, 0, []).

setup_workers(_, _, _, _, Num, Num, Res) ->
    Res;
setup_workers(PoolRef, Bridge, Worker, Opts, Num, SoFar, Res) ->
    {ok, Pid} = eqw_worker:new(PoolRef, Bridge, Worker, Opts),
    monitor(process, Pid),
    setup_workers(PoolRef, Bridge, Worker, Opts, Num, SoFar + 1, [Pid|Res]).

inc(Counter) ->
    eqw_info:inc(Counter).

inc(Counter, Steps) ->
    eqw_info:inc(Counter, Steps).
