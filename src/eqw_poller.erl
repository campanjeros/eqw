%% Description: eqw_poller
%%

-module(eqw_poller).

-behaviour(gen_server).

%% Management API
-export([start_link/3, new/4]).

%% API
-export([pause/1, resume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link(Bridge, Worker, Opts) ->
    gen_server:start_link(?MODULE, [Bridge, Worker, Opts], []).

new(ParentSup, Bridge, Worker, Opts) ->
    eqw_poller_sup:add_child(ParentSup, [Bridge, Worker, Opts]).

%% Api ------------------------------------------------------------------------

pause(Pid) ->
    gen_server:call(Pid, pause).

resume(Pid) ->
    gen_server:call(Pid, resume).

%% gen_server callbacks -------------------------------------------------------

init([{Bridge, BridgeArgs}, Worker, Opts]) ->
    case catch Bridge:setup(BridgeArgs) of
        {error, Reason} ->
            % Send info to eqw:info
            {stop, {Bridge, setup, Reason}};
        {'EXIT', Reason} ->
            % Send info to eqw:info
            exit({Bridge, setup, Reason});
        {ok, BridgeState} ->
            #{poll_interval := PollInterval} = Opts,
            Timer = timer:send_after(PollInterval, poll),
            {ok, #{bridge => Bridge,
                   bridge_state => BridgeState,
                   worker => Worker,
                   timer => Timer,
                   opts => Opts}}
    end.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(poll, State) ->
    #{bridge := Bridge,
      bridge_state := BridgeState,
      worker := Worker,
      pool := Pool,
      opts := #{max_workers := MaxWorkers,
                poll_interval := PollInterval} = Opts} = State,
    case length(Pool) < MaxWorkers of
        false ->
            Timer = timer:send_after(PollInterval, poll),
            {noreply, State#{timer => Timer}};
        true ->
            NumMessages = max(MaxWorkers - length(Pool), 10),
            case catch Bridge:recv(NumMessages, BridgeState) of
                {error, _Reason} ->
                    % Send info to eqw:info
                    Timer = timer:send_after(PollInterval, poll),
                    {noreply, State#{timer => Timer}};
                {'EXIT', _Reason} ->
                    % Send info to eqw:info
                    Timer = timer:send_after(PollInterval, poll),
                    {noreply, State#{timer => Timer}};
                {ok, Msgs} ->
                    Pids = setup_workers({Bridge, BridgeState},
                                         Worker, Opts, length(Msgs)),
                    PidMsgs = lists:zip(Pids, Msgs),
                    [ eqw_worker:handle_message(P, M) || {P, M} <- PidMsgs ],
                    Timer = timer:send_after(PollInterval, poll),
                    {noreply, State#{timer => Timer}}
            end
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

setup_workers(Bridge, Worker, Opts, Num) ->
    [ setup_worker(Bridge, Worker, Opts) || _ <- lists:seq(1, Num) ].

setup_worker(Bridge, Worker, Opts) ->
    {ok, Pid} = eqw_worker:new(Bridge, Worker, Opts),
    Pid.
