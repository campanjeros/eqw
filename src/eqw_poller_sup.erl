%%  Description: eqw_poller_sup
%%

-module(eqw_poller_sup).

-behaviour(supervisor).

-export([start_link/0, add_child/2, del_child/2]).

%% supervisor callbacks
-export([init/1]).

%% Management Api -------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, no_arg).

add_child(Pid, Args) ->
    file:write_file("/tmp/log", "adding a child", [append]),
    supervisor:start_child(Pid, Args).

del_child(ParentPid, ChildPid) ->
    supervisor:terminate_child(ParentPid, ChildPid).

%% supervisor callbacks -------------------------------------------------------

init(no_arg) ->
    Poller = child(eqw_poller, eqw_poller, worker, []),
    Strategy = {simple_one_for_one, 1, 5},
    {ok, {Strategy, [Poller]}}.

%% Internal -------------------------------------------------------------------

child(Name, Mod, Type, Args) ->
    {Name, {Mod, start_link, Args}, transient, 3000, Type, [Mod]}.
