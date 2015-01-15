%%  Description: eqw_app
%%

-module(eqw_app).

-behaviour(application).

%% application callbacks
-export([start/2, prep_stop/1, stop/1]).

%% application callbacks ------------------------------------------------------

start(normal, no_arg) ->
    eqw_sup:start_link().

prep_stop(State) ->
    State.

stop(_) ->
    ok.

%% Internal -------------------------------------------------------------------
