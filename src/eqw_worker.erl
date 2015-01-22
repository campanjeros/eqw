%% Description: eqw_worker
%%

-module(eqw_worker).

-behaviour(gen_server).

%% Management API
-export([start_link/3, new/3]).

%% API
-export([handle_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {bridge, bridge_state, worker, worker_state, opts}).
%% Management Api -------------------------------------------------------------

start_link(Bridge, Worker, Opts) ->
    gen_server:start_link(?MODULE, [Bridge, Worker, Opts], []).

new(Bridge, Worker, Opts) ->
    eqw_worker_sup:add_child([Bridge, Worker, Opts]).

%% Api ------------------------------------------------------------------------

handle_message(Pid, Msg) ->
    gen_server:cast(Pid, {handle_message, Msg}).

%% gen_server callbacks -------------------------------------------------------

init([{Bridge, BridgeState}, {Worker, WorkerArgs}, Opts]) ->
    case catch Worker:init(WorkerArgs) of
        {ok, WorkerState} ->
            {ok, #state{bridge=Bridge,
                        bridge_state=BridgeState,
                        worker=Worker,
                        worker_state=WorkerState,
                        opts=Opts}};
        {error, Reason} ->
            {stop, {gen_eqw, Worker, Reason}};
        {'EXIT', Reason} ->
            exit({gen_eqw, Worker, Reason})
    end.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({handle_message, Msg}, State) ->
    #state{bridge=Bridge,
           bridge_state=BridgeState,
           worker=Worker,
           worker_state= WorkerState,
           opts=Opts} = State,
    Interval = proplists:get_value(timer_interval, Opts),
    eqw_timer:start_link(Interval, {Bridge, BridgeState}, Msg),
    case Worker:handle_msg(Msg, WorkerState) of
        ok ->
            inc(message_handled, 1),
            case catch Bridge:ack(Msg, BridgeState) of
                {'EXIT', Reason} ->
                    inc(bridge_ack_crash, 1),
                    exit({gen_eqw_bridge, Bridge, Reason});
                _ ->
                    inc(bridge_ack, 1),
                    {stop, normal, State}
            end;
        _ ->
            {stop, normal, State}
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

inc(Counter, Steps) ->
    eqw_info:inc(Counter, Steps).
