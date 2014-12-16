-module(eqw_sqs).

-behaviour(eqw_bridge).

-export([setup/0, recv/2, ack/2, timeout/2]).

setup() ->
    QueueName = "gaw_stats-worker-queue",
    [{queue_url, _}] = erlcloud_sqs:create_queue(QueueName),
    {ok, QueueName, [{timer_interval, timer:seconds(15)}]}.

recv(_Num, QueueName) ->
    [{messages, Msgs}] = erlcloud_sqs:receive_messages(QueueName),
    {ok, Msgs}.

ack(Msg, QueueName) ->
    Receipt = proplists:get_value(receipt_handle, Msg),
    ok = erlcloud_sqs:delete_message(QueueName, Receipt).

timeout(Msg, QueueName) ->
    Receipt = proplists:get_value(receipt_handle, Msg),
    erlcloud_sqs:change_message_visibility(QueueName, Receipt, 30).
