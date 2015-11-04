-module(print_worker).

-behaviour(gen_eqw).

-export([init/1, handle_msg/3]).

-export([start/0, start/2, stop/1]).

start() ->
    start(2, 10).

start(Pollers, Workers) ->
    eqw:start(),
    Opts = #{num_pollers => Pollers, max_workers => Workers},
    eqw:add_pool(mem_bridge, no_args, ?MODULE, no_args, Opts).

stop(Ref) ->
    eqw:del_pool(Ref).

init(no_args) ->
    {ok, #{}}.

handle_msg(Msg, _, #{}) ->
    io:format("msg: ~p~n~n", [Msg]),
    ok.
