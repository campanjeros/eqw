-module(eqw_test_worker).

-behaviour(eqw_queue_handler).

-export([init/1, handle_msg/2]).


init(Pid) ->
    {ok, Pid, 50}.

handle_msg(_Msg, Receiver) ->
    %% io:format("msg: ~p~n", [proplists:get_value(body, Msg)]),
    Receiver ! {num, 1},
    ok.
