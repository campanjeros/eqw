%%  Description: eqw_worker_sup_sup
%%

-module(eqw_worker_sup).

-behaviour(supervisor).

%% Management Api
-export([start_link/0, add_child/1, del_child/1]).

%% supervisor callbacks
-export([init/1]).

%% Management Api -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

add_child(Args) ->
    supervisor:start_child(?MODULE, Args).

del_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% supervisor callbacks -------------------------------------------------------

init(no_arg) ->
    Worker = child(eqw_worker, eqw_worker, worker, []),
    Strategy = {simple_one_for_one, 1, 5},
    {ok, {Strategy, [Worker]}}.

%% Internal -------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, transient, 2000, Type, [Mod]}.
