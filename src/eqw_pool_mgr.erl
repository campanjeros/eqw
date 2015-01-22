%% Description: eqw_pool_mgr
%%
%% TODO: Handle default options better
%% TODO: Tweak separate pool options

-module(eqw_pool_mgr).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([add_pool/5, del_pool/1,
         pause_pool/1, resume_pool/1,
         list_pools/0, pool_info/1]).

-export([send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(pool, {pid, bridge, worker, opts, state}).
-record(state, {pools=[], default_options=[]}).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Api ------------------------------------------------------------------------

add_pool(Bridge, BridgeArgs, Worker, WorkerArgs, Opts) ->
    Args = {{Bridge, BridgeArgs}, {Worker, WorkerArgs}, Opts},
    gen_server:call(?MODULE, {add_pool, Args}, timer:seconds(60)).

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
    {ok, #state{default_options=[{num_pollers, 20},
                                 {max_workers, 20},
                                 {timer_interval, timer:seconds(15)},
                                 {poll_interval, 50}]}}.

handle_call({add_pool, Args}, _, State) ->
    #state{pools=Pools, default_options=DefaultOpts} = State,
    Ref = make_ref(),
    Pool = add_pool(Args, DefaultOpts),
    {reply, Ref, State#state{pools=[{Ref, Pool}|Pools]}};
handle_call({del_pool, Ref}, _, #state{pools=Pools} = State) ->
    case lists:keyfind(Ref, 1, Pools) of
        false ->
            {reply, {error, not_found}, State};
        {Ref, #pool{pid=Pid}} ->
            ok = eqw_pool_sup:del_child(Pid),
            {reply, ok, State#state{pools=lists:keydelete(Ref, 1, Pools)}}
    end;
handle_call({pause_pool, Ref}, _, #state{pools=Pools} = State) ->
    case lists:keyfind(Ref, 1, Pools) of
        false ->
            {reply, {error, not_found}, State};
        {Ref, #pool{pid=Pid} = Pool} ->
            ok = pause_pool_pollers(Pid),
            NewPool = Pool#pool{state=paused},
            NewPools = lists:keyreplace(Ref, 1, Pools, {Ref, NewPool}),
            {reply, ok, State#state{pools=NewPools}}
    end;
handle_call({resume_pool, Ref}, _, #state{pools=Pools} = State) ->
    case lists:keyfind(Ref, 1, Pools) of
        false ->
            {reply, {error, not_found}, State};
        {Ref, #pool{pid=Pid} = Pool} ->
            ok = resume_pool_pollers(Pid),
            NewPool = Pool#pool{state=polling},
            NewPools = lists:keyreplace(Ref, 1, Pools, {Ref, NewPool}),
            {reply, ok, State#state{pools=NewPools}}
    end;
handle_call(list_pools, _, #state{pools=Pools} = State) ->
    Keys = [ K || {K,_} <- Pools ],
    {reply, Keys, State};
handle_call({pool_info, Ref}, _, #state{pools=Pools} = State) ->
    case lists:keyfind(Ref, 1, Pools) of
        false ->
            {reply, {error, not_found}, State};
        {Ref, Pool} ->
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

add_pool({Bridge, Worker, Opts}, DefaultOpts) ->
    NewOpts = merge_opts(Opts, DefaultOpts),
    NumPollers = proplists:get_value(num_pollers, NewOpts),
    {ok, Pool} = new_pool(),
    [ new_poller(Pool, Bridge, Worker, NewOpts) ||
      _ <- lists:seq(1, NumPollers) ],
    #pool{pid=Pool,
          bridge=Bridge,
          worker=Worker,
          opts=NewOpts,
          state=polling}.

merge_opts(Proper, Fallback) ->
    F = fun(K, Acc) ->
            case proplists:get_value(K, Proper) of
                undefined ->
                    case proplists:get_value(K, Fallback) of
                        undefined ->
                            Acc;
                        FallbackValue ->
                            [{K, FallbackValue}|Acc]
                    end;
                ProperValue ->
                    [{K, ProperValue}|Acc]
            end
        end,
    lists:foldl(F, [], proplists:get_keys(Fallback)).

new_pool() ->
    eqw_pool_sup:add_child([]).

new_poller(Pool, Bridge, Worker, Opts) ->
    eqw_poller:new(Pool, Bridge, Worker, Opts).

pause_pool_pollers(Pool) ->
    PollerPids = get_pool_pollers(Pool),
    [ eqw_poller:pause(P) || P <- PollerPids ],
    ok.

resume_pool_pollers(Pool) ->
    PollerPids = get_pool_pollers(Pool),
    [ eqw_poller:resume(P) || P <- PollerPids ],
    ok.

get_pool_pollers(Pool) ->
    [ P || {_, P, _, _} <- supervisor:which_children(Pool) ].
