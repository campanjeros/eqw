%% gen_eqw
%%

-module(gen_eqw).

%% Callbacks ------------------------------------------------------------------

-callback init(Args::any()) -> {ok, State::any()} |
                               {stop, Reason::any()}.

%% Optional callback, only used if the pool uses a batch-poller
-callback batch_split_key(Msg::any(), State::any()) ->
    {ok, Key::any(), ParsedMsg::any()} |
    {error, any()}.

-callback handle_msg(Msg::any(), PoolRef::reference(), State::any()) -> ok |
                                                                        error.
