-module(print_worker).

-behaviour(gen_eqw).

-export([init/1, handle_msg/2]).

-export([start/0, start/2, stop/1]).

start() ->
    start(10, 10).

start(Pollers, Workers) ->
    eqw:start(),
    Opts = [{num_pollers, Pollers}, {max_workers, Workers}],
    eqw:add_pool(sqs, "q1", ?MODULE, no_args, Opts).

stop(Ref) ->
    eqw:del_pool(Ref).

init(no_args) ->
    {ok, no_state}.

handle_msg(Msg, no_state) ->
    io:format("msg: ~p~n~n", [Msg]),
    ok.
