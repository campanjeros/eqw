-module(eqw_bridge).

-export([]).

-type opt() :: [{timer, timeout()}].

%% Callbacks ------------------------------------------------------------------

%% Sets up or connects to the queue, preferable synchronous receive
-callback setup() -> {ok, State::any()} |
                     {ok, State::any(), [opt()]} |
                     _{error, Reason::any()}.

%% Called to fetch/pop messages from the queue
-callback recv(Number::integer(), State::any()) -> {ok, [Msg::any()]}.

%% Called to acknowledge/delete a message that has been received
-callback ack(Msg::any(), State::any()) -> ok | {error, Reason::any()}.

%% Called on a regular interval, specified by init_queue/1
%% Could be used to reset timeout on received items
-callback timeout(Msg::any(), State::any()) -> ok.
