%%  Description: eqw_sup
%%

-module(eqw_sup).

-behaviour(supervisor).

%% Management Api
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Management Api -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% supervisor callbacks -------------------------------------------------------

init(no_arg) ->
    InfoSrv = child(eqw_info, eqw_info, worker, []),
    WorkerSup = child(eqw_worker_sup, eqw_worker_sup, supervisor, []),
    PollerSup = child(eqw_poller_sup, eqw_poller_sup, supervisor, []),
    PoolSup = child(eqw_pool_sup, eqw_pool_sup, supervisor, []),
    PoolMgr = child(eqw_pool_mgr, eqw_pool_mgr, worker, []),
    Strategy = {one_for_one, 1, 5},
    {ok, {Strategy, [InfoSrv, WorkerSup, PollerSup, PoolSup, PoolMgr]}}.

%% Internal -------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, permanent, 3000, Type, [Mod]}.
