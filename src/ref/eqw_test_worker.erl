-module(eqw_test_worker).

-behaviour(gen_eqw).

-export([init/1, handle_msg/2]).


init(_) ->
    {ok, no_state}.

handle_msg(Msg, _) ->
    io:format("msg: ~p~n", [proplists:get_value(body, Msg)]),
    ok.
