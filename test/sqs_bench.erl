-module(sqs_bench).

-behaviour(gen_eqw).

-export([start/5]).

-export([init/1, handle_msg/2]).

start(Queue, Pollers, MaxWorkers, NumMessages, MsgSize) ->
    eqw:start(),
    Pid = start_counter(NumMessages),
    Opts = #{num_pollers => Pollers, max_workers => MaxWorkers},
    Pool = eqw:add_pool(eqw_sqs, Queue, ?MODULE, [Pid], Opts),
    T0 = os:timestamp(),
    send_bulk(Queue,NumMessages, MsgSize),
    SendTs = timer:now_diff(os:timestamp(), T0)/1000000,
    io:format("sent ~p messages (size: ~p) in ~ps~n",
              [NumMessages, MsgSize, SendTs]),
    receive
        {Pid, done} ->
            RecvTs = timer:now_diff(os:timestamp(), T0)/1000000,
            io:format("received ~p messages in ~ps~n", [NumMessages, RecvTs]),
            io:format("pool stats: ~p~n", [eqw:stats()]),
            eqw:del_pool(Pool)
    end.

send_bulk(Queue, NumMessages, MsgSize) ->
    Msg = random_message(MsgSize),
    Msgs = [ Msg || _ <- lists:seq(1, NumMessages) ],
    send_all(Queue, Msgs, []).

send_all(_, [], Pids) ->
    lists:sum([ receive P -> 1 end || P <- Pids ]);
send_all(Queue, [Msg|Msgs], Pids) ->
    S = self(),
    P = spawn(fun() -> erlcloud_sqs:send_message(Queue, Msg), S ! self() end),
    send_all(Queue, Msgs, [P|Pids]).

random_message(Size) ->
    random:seed(os:timestamp()),
    [ $a+random:uniform($z-$a) || _ <- lists:seq(1, Size) ].

%% ----------------------------------------------------------------------------

init([Pid]) ->
    {ok, Pid}.

handle_msg(_Msg, Pid) ->
    Pid ! {inc, 1},
    ok.

%% ----------------------------------------------------------------------------

start_counter(CountToReach) ->
    S = self(),
    spawn(fun() -> counter(S, CountToReach, 0) end).

counter(Parent, CountToReach, CountToReach) ->
    Parent ! {self(), done};
counter(Parent, CountToReach, Count) ->
    receive
        {inc, Step} ->
            counter(Parent, CountToReach, Count+Step)
    end.

split_list(List, Size) ->
    split_list(List, Size, []).

split_list(List, Size, Parts) when length(List) > Size ->
    {Part, Rest} = lists:split(Size, List),
    split_list(Rest, Size, [Part|Parts]);
split_list(List, _, Parts) ->
    lists:reverse([List|Parts]).
