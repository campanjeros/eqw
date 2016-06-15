-module(gen_eqw_bridge).

%% Callbacks ------------------------------------------------------------------

%% Sets up or connects to the queue, preferable synchronous receive
-callback setup(Args::any()) -> {ok, State::any()}
                              | {error, Reason::any()}.

%% Called to fetch/pop messages from the queue
-callback recv(Number::integer(), State::any()) -> {ok, [Msg::any()]}
                                                 | {error, Reason::any()}.

%% Called to acknowledge/delete a message that has been received
-callback ack(Msg::any(), State::any()) -> ok | {error, Reason::any()}.

%% Called on a regular interval, specified by init_queue/1
%% Could be used to reset timeout on received items
-callback timeout(Msg::any(), State::any()) -> ok.

%% Should wrap a send-batch call for the specific queue-system
-callback send([Msg::any()], State::any()) -> ok | {error, Reason::any()}.

%% returns queue system metadata
%% return value is a map with two keys; 'request_queue' and 'response_queue'.
%% both map values are maps. The keys 'name', 'length' and 'handling' are
%% mandatory, but there can be more fields if appropriate for the queue system.
%% Value of 'name' is a string, length (number of messages in the queue) is an
%% integer, and 'handling' (the number of messages that has been received but
%% not yet deleted) is an integer.
-callback metadata(State::any()) -> map().
