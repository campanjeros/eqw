%% Description: eqw_timer
%%

-module(eqw_timer).

-behaviour(gen_server).

%% Management API
-export([start_link/3, stop/1]).

%% API
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link(Interval, Bridge, Msg) ->
    gen_server:start_link(?MODULE, [Interval, Bridge, Msg], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% Api ------------------------------------------------------------------------

%% gen_server callbacks -------------------------------------------------------

init([Interval, {Bridge, BridgeState}, Msg]) ->
    timer:send_after(Interval, tick),
    {ok, #{interval => Interval,
           bridge => Bridge,
           bridge_state => BridgeState,
           msg => Msg}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    #{interval := Interval,
      bridge := Bridge,
      bridge_state := BridgeState,
      msg := Msg} = State,
    eqw_info:inc(timer_tick),
    case catch Bridge:timeout(Msg, BridgeState) of
        {'EXIT', _} ->
            {stop, normal, State};
        _ ->
            timer:send_after(Interval, tick),
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------
