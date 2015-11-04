%% gen_eqw
%%

-module(gen_eqw).

%% Callbacks ------------------------------------------------------------------

%% Should also tell about how many workers should handle request at max
-callback init(Args::any()) -> {ok, State::any()} |
                               {stop, Reason::any()}.

-callback handle_msg(Msg::any(), PoolRef::reference(), State::any()) -> ok.

