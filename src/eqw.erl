%% eqw application facade
%%

-module(eqw).

%% Api
-export([start/0, stop/0,
         add_pool/5, del_pool/1,
         pause_pool/1, resume_pool/1,
         list_pools/0, pool_info/1,
         status/0]).

%% Utility
-export([send/2]).

%% Api ------------------------------------------------------------------------

start() ->
    application:ensure_all_started(eqw).

stop() ->
    application:stop(eqw).

add_pool(Bridge, BridgeArgs, Worker, WorkerArgs, Opts) ->
    eqw_pool_mgr:add_pool(Bridge, BridgeArgs, Worker, WorkerArgs, Opts).

del_pool(PoolRef) ->
    eqw_pool_mgr:del_pool(PoolRef).

pause_pool(PoolRef) ->
    eqw_pool_mgr:pause_pool(PoolRef).

resume_pool(PoolRef) ->
    eqw_pool_mgr:resume_pool(PoolRef).

list_pools() ->
    eqw_pool_mgr:list_pools().

pool_info(PoolRef) ->
    eqw_pool_mgr:pool_info(PoolRef).

status() ->
    eqw_info:status().

%% Utility --------------------------------------------------------------------

send(PoolRef, Msgs) ->
    eqw_pool_mgr:send(PoolRef, Msgs).
