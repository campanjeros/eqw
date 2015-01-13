%% Description: eqw_pool
%%

-module(eqw_pool).

-behaviour(gen_server).

%% Management API
-export([start_link/5, new/5, stop/1]).

%% API
-export([pause/1, resume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link(Bridge, BridgeArgs, Worker, WorkerArgs, Opts) ->
    Args = [Bridge, BridgeArgs, Worker, WorkerArgs, Opts],
    gen_server:start_link(?MODULE, Args, []).

new(Bridge, BridgeArgs, Worker, WorkerArgs, Opts) ->
    eqw_pool_sup:add_child([Bridge, BridgeArgs, Worker, WorkerArgs, Opts]).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% Api ------------------------------------------------------------------------

pause(Pid) ->
    gen_server:call(Pid, pause).

resume(Pid) ->
    gen_server:call(Pid, resume).

%% gen_server callbacks -------------------------------------------------------

init([Bridge, BridgeArgs, Worker, WorkerArgs, Opts]) ->
    {ok, #{bridge => Bridge, bridge_args => BridgeArgs,
           worker => Worker, worker_args => WorkerArgs, opts => Opts}}.

handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------
