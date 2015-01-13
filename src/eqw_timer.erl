%% Description: eqw_timer
%%

-module(eqw_timer).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Api ------------------------------------------------------------------------

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{}}.

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
