%% For now this is a implicit pool manager, might transform it into a explicit
%% pool type with add_pool(PoolSize, ...)
%%

-module(eqw_queue_handler).

-behaviour(gen_fsm).

%% Management API
-export([start_link/4]).

%% APO
-export([get_pool_info/1, set_pool_size/2]).

%% gen_fsm default callbacks
-export([init/1, handle_sync_event/4, handle_event/3,
         handle_info/3, code_change/4, terminate/3]).

%% gen_fsm state callbacks
-export([polling/2, pool_full/2]).

%%-record(config, {timer, timeout, pool_size}).
%%-type opt() :: timeout | pool_size.

-record(state, {bridge, bridge_state,
                mod, mod_state,
                pool, pool_size,
                timer_interval}).

%% Callbacks ------------------------------------------------------------------

%% Should also tell about how many workers should handle request at max
-callback init(Args::any()) -> {ok, State::any(), PoolSize::non_neg_integer()} |
                               %%{ok, State::any()} |
                               {stop, Reason::any()}.

-callback handle_msg(Msg::any(), State::any()) -> ok.

%% Management API -------------------------------------------------------------
%% TODO: Take options, like pool_size, poll_interval, etc
start_link(Bridge, BridgeArgs, Module, Args) ->
    gen_fsm:start_link(?MODULE, {Bridge, BridgeArgs, Module, Args}, []).

%% Api ------------------------------------------------------------------------

get_pool_info(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_pool_info).

set_pool_size(Pid, Size) ->
    gen_fsm:send_all_state_event(Pid, {set_pool_info, Size}).

%% gen_fsm standard callbacks -------------------------------------------------

init({Bridge, BridgeArgs, Module, Args}) ->
    process_flag(trap_exit, true),
    case catch Bridge:setup(BridgeArgs) of
        {ok, BridgeState, TimerInterval} ->
            case catch Module:init(Args) of
                {ok, ModState, PoolSize} ->
                    gen_fsm:send_event(self(), poll),
                    {ok, polling, #state{bridge=Bridge,
                                         bridge_state=BridgeState,
                                         mod=Module, mod_state=ModState,
                                         pool_size=PoolSize, pool=[],
                                         timer_interval=TimerInterval}};
                {stop, Reason} ->
                    {stop, Reason};
                {'EXIT', Reason} ->
                    exit({eqw_queue_handler, init, Reason})
            end;
        {error, Reason} ->
            {stop, {bridge_setup, Reason}};
        {'EXIT', Reason} ->
            exit({Bridge, setup, Reason})
    end.

handle_sync_event(get_pool_info, _, StateName, State) ->
    #state{pool=Pool, pool_size=Size} = State,
    {reply, [{pool_size, Size}, {in_use, length(Pool)}], StateName, State};
handle_sync_event(_, _, StateName, State) ->
    {next_state, StateName, State}.

handle_event({set_pool_info, Size}, StateName, State) ->
    {next_state, StateName, State#state{pool_size=Size}};
handle_event(_, StateName, State) ->
    {next_state, StateName, State}.

handle_info({'EXIT', Pid, normal}, _, #state{pool=Pool} = State) ->
    gen_fsm:send_event(self(), poll),
    {next_state, polling, State#state{pool=Pool -- [Pid]}};
handle_info(Info, StateName, State) ->
    io:format("unexpected info: ~p~n", [Info]),
    {next_state, StateName, State}.

code_change(_, StateName, State, _) -> {ok, StateName, State}.

terminate(_, _, _) -> ok.

%% gen_fsm state callbacks ----------------------------------------------------

%% QUICK-HACK: Using spawn to prototype this behaviour
polling(poll, #state{pool_size=PoolSize, pool=Pool} = State) ->
    #state{bridge=Bridge, bridge_state=BridgeState,
           mod=Mod, mod_state=ModState,
           timer_interval=TimerInterval} = State,
    case length(Pool) < PoolSize of
        false ->
            {next_state, pool_full, State};
        true ->
            %% TODO: Ensure this logic handles batches of msgs
            case catch Bridge:recv(10, BridgeState) of
                {ok, []} ->
                    timer:sleep(100),
                    gen_fsm:send_event(self(), poll),
                    {next_state, polling, State};
                {ok, Msgs} ->
                    Fun = fun(Msg) ->
                              TimerPid = spawn_timer(self(), Bridge,
                                                     BridgeState,
                                                     Msg, TimerInterval),
                              Mod:handle_msg(Msg, ModState),
                              Bridge:ack(Msg, BridgeState),
                              TimerPid ! {self(), stop}
                          end,
                    Pids = [ spawn_link(fun() -> Fun(M) end) || M <- Msgs ],
                    NewPool = Pids ++ Pool,
                    %% TODO: verify if we should keep polling or wait
                    gen_fsm:send_event(self(), poll),
                    {next_state, polling, State#state{pool=NewPool}};
                {'EXIT', _Reason} ->
                    timer:sleep(100),
                    gen_fsm:send_event(self(), poll),
                    {next_state, polling, State}
            end
    end.

pool_full(_, State) ->
    {next_state, pool_full, State}.

%% Internal -------------------------------------------------------------------

spawn_timer(Parent, Bridge, BridgeState, Msg, TimerInterval) ->
    spawn_link(fun() ->
                   time_func(Parent, Bridge, BridgeState, Msg, TimerInterval)
               end).

time_func(Parent, Bridge, BridgeState, Msg, TimerInterval) ->
    receive
        {Parent, stop} ->
            ok
    after TimerInterval ->
        Bridge:timeout(Msg, BridgeState),
        time_func(Parent, Bridge, BridgeState, Msg, TimerInterval)
    end.
