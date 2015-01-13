%% Description: eqw_pool_mgr
%%
%% TODO: Handle default options better
%% TODO: Tweak separate pool options

-module(eqw_pool_mgr).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([add_pool/4, del_pool/1,
         pause_pool/1, resume_pool/1,
         list_pools/0, pool_info/1]).

-export([send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Api ------------------------------------------------------------------------

add_pool(Bridge, BridgeArgs, Worker, WorkerArgs) ->
    Args = {Bridge, BridgeArgs, Worker, WorkerArgs},
    gen_server:call(?MODULE, {add_pool, Args}).

del_pool(PoolRef) ->
    gen_server:call(?MODULE, {del_pool, PoolRef}).

pause_pool(PoolRef) ->
    gen_server:call(?MODULE, {pause_pool, PoolRef}).

resume_pool(PoolRef) ->
    gen_server:call(?MODULE, {resume_pool, PoolRef}).

list_pools() ->
    gen_server:call(?MODULE, list_pools).

pool_info(PoolRef) ->
    gen_server:call(?MODULE, {pool_info, PoolRef}).

send(_PoolRef, _Msgs) ->
    ok.

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{pools => #{},
           default_options => #{num_pollers => 10,
                                num_worker => 10}}}.

handle_call({add_pool, Args}, _, #{pools := Pools} = State) ->
    {Bridge, BridgeArgs, Worker, WorkerArgs, Opts} = Args,
    {ok, PoolPid} = eqw_pool:new(Bridge, BridgeArgs, Worker, WorkerArgs, Opts),
    erlang:monitor(process, PoolPid),
    Ref = make_ref(),
    Pool = #{pid => PoolPid,
             %% TODO: Expand args to separate keys?
             args => Args,
             state => polling},
    {reply, Ref, State#{pools :=  maps:put(Ref, Pool, Pools)}};
handle_call({del_pool, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, #{pid := Pid}} ->
            ok = eqw_pool:stop(Pid),
            {reply, ok, State#{pools := maps:remove(Ref, Pools)}}
    end;
handle_call({pause_pool, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, #{pid := Pid} = Pool} ->
            ok = eqw_pool:pause(Pid),
            NewPools = maps:update(Ref, Pool#{state := paused}, Pools),
            {reply, ok, State#{pools := NewPools}}
    end;
handle_call({resume_pool, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, #{pid := Pid} = Pool} ->
            ok = eqw_pool:resume(Pid),
            NewPools = maps:update(Ref, Pool#{state := polling}, Pools),
            {reply, ok, State#{pools := NewPools}}
    end;
handle_call(list_pools, _, #{pools := Pools} = State) ->
    {reply, maps:keys(Pools), State};
handle_call({pool_info, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, Pool} ->
            {reply, Pool, State}
    end;
handle_call(Msg, _, State) ->
    io:format("wtf-msg: ~p~nstate: ~p~n", [Msg, State]),
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
