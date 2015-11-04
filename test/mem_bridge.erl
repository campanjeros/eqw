%% In memory queue
%%

-module(mem_bridge).

-export([setup/1, recv/2, ack/2, timeout/2, send/2]).

setup(Args) ->
    Pid = spawn_link(fun() -> init(Args) end),
    {ok, Pid}.

recv(NumMsgs, Pid) ->
    Pid ! {recv, self(), Ref = make_ref(), NumMsgs},
    receive
        {Ref, Msgs} ->
            {ok, Msgs}
    end.

%% TODO: Implement this functionality
ack(_Msg, _State) ->
    ok.

%% TODO: Implement this functionality
timeout(_Msg, _State) ->
    ok.

send(Msgs, Pid) ->
    Pid ! {send, Msgs},
    ok.

%% Internal -------------------------------------------------------------------

init(_) ->
    loop(queue_new(), []).

loop(Queue, Pending) ->
    receive
        {send, Msgs} when length(Pending) == 0 ->
            loop(queue_en_batch(Msgs, Queue), Pending);
        {send, Msgs} ->
            NewQueue = queue_en_batch(Msgs, Queue),
            {FinalQueue, NewPending} = handle_pending(NewQueue, Pending),
            loop(FinalQueue, NewPending);
        {recv, Pid, Ref, Num} ->
            case queue_len(Queue) > 0 of
                true ->
                    {Msgs, NewQueue} = queue_de_batch(Num, Queue),
                    Pid ! {Ref, Msgs},
                    loop(NewQueue, Pending);
                false ->
                    loop(Queue, [{Pid, Ref, Num}|Pending])
            end;
        status ->
            io:format("queue-len: ~p, pending: ~p~n",
                      [queue_len(Queue), length(Pending)]),
            loop(Queue, Pending);
        _ ->
            loop(Queue, Pending)
    end.

handle_pending({[], []}, Pending) -> {{[], []}, Pending};
handle_pending(Queue, [{Pid, Ref, Num}|Rest]) ->
    {Msgs, NewQueue} = queue_de_batch(Num, Queue),
    Pid ! {Ref, Msgs},
    handle_pending(NewQueue, Rest);
handle_pending(Queue, []) -> {Queue, []}.

queue_en_batch(Msgs, Queue) ->
    lists:foldl(fun(M, Q) -> queue_en(M, Q) end, Queue, Msgs).

queue_de_batch(Num, Queue) ->
    queue_de_batch(Num, Queue, []).

queue_de_batch(0, Queue, Res) -> {lists:reverse(Res), Queue};
queue_de_batch(_, {[], []}, Res) -> {lists:reverse(Res), {[], []}};
queue_de_batch(Num, Queue, Res) ->
    {Elem, NewQueue} = queue_de(Queue),
    queue_de_batch(Num - 1, NewQueue, [Elem|Res]).

queue_new() -> {[], []}.

queue_en(Msg, {In, Out}) -> {[Msg|In], Out}.

queue_de({[], []}) -> empty;
queue_de({In, []}) -> queue_de({[], lists:reverse(In)});
queue_de({In, [Elem|Out]}) -> {Elem, {In, Out}}.

queue_len({In, Out}) -> length(In) + length(Out).
