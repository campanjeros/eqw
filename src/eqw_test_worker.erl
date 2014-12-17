-module(eqw_test_worker).

-behaviour(eqw_queue_handler).

-export([init/1, handle_msg/2]).


init(_) ->
    {ok, 0, 5}.

handle_msg(Msg, Cnt) ->
    io:format("~p: ~p~n", [Cnt, proplists:get_value(body, Msg)]),
    {ok, Cnt + 1}.
