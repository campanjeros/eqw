%%  Description: eqw_pool_sup
%%

-module(eqw_pool_sup).

-behaviour(supervisor).

%% Management API
-export([start_link/0, add_child/2, del_child/1]).

%% supervisor callbacks
-export([init/1]).

%% Management API -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

add_child(Ref, Type) ->
    PollerSup = case Type of
                    single ->
                        child(Ref,
                              eqw_poller_sup, supervisor, []);
                    batch ->
                        child(Ref,
                              eqw_batch_poller_sup, supervisor, [])
                end,
    supervisor:start_child(?MODULE, PollerSup).

del_child(Ref) ->
    supervisor:terminate_child(?MODULE, Ref).

%% supervisor callbacks -------------------------------------------------------

init(no_args) ->
    %%TODO: This choice of strategy is questionable
    Strategy = {one_for_one, 1, 5},
    {ok, {Strategy, []}}.

%% Internal -------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, transient, 2000, Type, [Mod]}.
