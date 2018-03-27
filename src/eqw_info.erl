%% Description: eqw_info
%%

-module(eqw_info).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([status/0, inc/1, inc/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management API -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API ------------------------------------------------------------------------

status() ->
    gen_server:call(?MODULE, status).

inc(Counter) ->
    inc(Counter, 1).

inc(Counter, Steps) ->
    gen_server:cast(?MODULE, {inc, Counter, Steps}).

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{tbl => ets:new(eqw_info, [])}}.

handle_call(status, _, #{tbl := Tbl} = State) ->
    Stats = ets:match(Tbl, '$0'),
    {reply, Stats, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({inc, Counter, Steps}, #{tbl := Tbl} = State) ->
    update_counter(Tbl, Counter, Steps),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

update_counter(Tbl, Key, Steps) ->
    case ets:lookup(Tbl, Key) of
        [] ->
            ets:insert(Tbl, {Key, Steps});
        _ ->
            ets:update_counter(Tbl, Key, Steps)
    end.
