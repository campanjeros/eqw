%% For now this is a implicit pool manager, might transform it into a explicit
%% pool type with add_pool(PoolSize, ...)
%%

-module(eqw_queue_handler).

-behaviour(gen_fsm).

%% Management API
-export([start_link/3]).

%% APO
-export([get_pool_info/1]).

%% gen_fsm default callbacks
-export([init/1, handle_sync_event/4, handle_event/3,
         handle_info/3, code_change/4, terminate/3]).

%% gen_fsm state callbacks
-export([polling/2]).

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
start_link(Bridge, Module, Args) ->
    gen_fsm:start_link(?MODULE, {Bridge, Module, Args}, []).

%% Api ------------------------------------------------------------------------

get_pool_info(Pid) ->
    gen_fsm:sync_send_event(Pid, get_pool_info).

%% gen_fsm standard callbacks -------------------------------------------------

init({Bridge, Module, Args}) ->
    process_flag(trap_exit, true),
    case catch Bridge:setup() of
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

handle_event(_, StateName, State) -> {next_state, StateName, State}.

handle_info({'EXIT', Pid, normal}, StateName, #state{pool=Pool} = State) ->
    {next_state, StateName, State#state{pool=lists:keydelete(Pid, 1, Pool)}};
handle_info(_, StateName, State) ->
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
            case catch Bridge:recv(1, BridgeState) of
                {ok, [Msg]} ->
                    Fun = fun() ->
                              TimerPid = spawn_timer(self(), Bridge,
                                                     BridgeState,
                                                     Msg, TimerInterval),
                              Mod:handle_msg(Msg, ModState),
                              Bridge:ack(Msg, BridgeState),
                              TimerPid ! {self(), stop}
                          end,
                    Pid = spawn_link(Fun),
                    NewPool = [Pid|Pool],
                    %% TODO: verify if we should keep polling or wait
                    gen_fsm:send_event(self(), poll),
                    {next_state, polling, State#state{pool=NewPool}};
                {ok, []} ->
                    timer:sleep(1000),
                    gen_fsm:send_event(self(), poll),
                    {next_state, polling, State};
                {'EXIT', _Reason} ->
                    timer:sleep(1000),
                    gen_fsm:send_event(self(), poll),
                    {next_state, polling, State}
            end
    end.

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
