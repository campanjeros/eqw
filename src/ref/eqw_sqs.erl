-module(eqw_sqs).

-behaviour(gen_eqw_bridge).

-export([setup/1, recv/2, ack/2, timeout/2]).

setup(QueueName) ->
    [{queue_url, _}] = erlcloud_sqs:create_queue(QueueName),
    {ok, QueueName}.

recv(Num, QueueName) ->
    [{messages, Msgs}] = erlcloud_sqs:receive_message(QueueName, all,
                                                      Num, 30, 20),
    {ok, Msgs}.

ack(Msg, QueueName) ->
    Receipt = proplists:get_value(receipt_handle, Msg),
    ok = erlcloud_sqs:delete_message(QueueName, Receipt).

timeout(Msg, QueueName) ->
    Receipt = proplists:get_value(receipt_handle, Msg),
    erlcloud_sqs:change_message_visibility(QueueName, Receipt, 30).
