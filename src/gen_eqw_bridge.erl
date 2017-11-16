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

%% Should wrap a send/send-batch call for the specific queue-system
%% Reports send failures in FailedMsgs list.
-callback send([Msg::any()], State::any()) -> {ok, [FailedMsg::any()]}
                                            | {error, Reason::any()}.

%% Called before send. Used to hide transport-specific encoding from end user.
-callback encode(Msg::any(), State::any()) -> EncodedMsg::any().

%% Called after recv. Used to hide transport-specific encoding from end user.
-callback decode(Msg::any(), State::any()) -> DecodedMsg::any().

%% returns queue system metadata
%% return value is a flat map with data appropriate for the queue system.
-callback metadata(State::any()) -> #{atom() => any()}.
