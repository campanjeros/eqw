# eqw
Erlang Queue Worker behaviour - owner: @cvik

This application provides an abstract interface to do work on items pull/popped from a generic queue. It does this by providing two behaviours:
  - gen_eqw
    * Generic behaviour for processing items from a queue
  - gen_eqw_bridge
    * Generic queue interface meant to support various queue systems like RabbitMQ, SQS, Kafka, etc
