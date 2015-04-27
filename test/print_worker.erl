-module(print_worker).

-behaviour(gen_eqw).

-export([init/1, handle_msg/2]).

-export([start/1, start/3, stop/1]).

start(QueueName) ->
    start(QueueName, 10, 10).

start(QueueName, Pollers, Workers) ->
    eqw:start(),
    Opts = #{num_pollers => Pollers, max_workers => Workers},
    eqw:add_pool(sqs, #{queue_name => QueueName}, ?MODULE, no_args, Opts).

stop(Ref) ->
    eqw:del_pool(Ref).

init(no_args) ->
    {ok, #{}}.

handle_msg(Msg, #{}) ->
    io:format("msg: ~p~n~n", [Msg]),
    ok.
