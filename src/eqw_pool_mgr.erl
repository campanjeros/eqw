%% Description: eqw_pool_mgr
%%
%% TODO: Handle default options better
%% TODO: Tweak separate pool options

-module(eqw_pool_mgr).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([add_pool/5, del_pool/1,
         pause_pool/1, resume_pool/1,
         list_pools/0, pool_info/1, metadata/1,
         send_to_pool/2,
         add_async/4, async_result/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Management Api -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Api ------------------------------------------------------------------------

add_pool(Bridge, BridgeArgs, Worker, WorkerArgs, Opts) ->
    Args = {{Bridge, BridgeArgs}, {Worker, WorkerArgs}, Opts},
    gen_server:call(?MODULE, {add_pool, Args}, timer:seconds(60)).

del_pool(PoolRef) ->
    gen_server:call(?MODULE, {del_pool, PoolRef}).

pause_pool(PoolRef) ->
    gen_server:call(?MODULE, {pause_pool, PoolRef}).

resume_pool(PoolRef) ->
    gen_server:call(?MODULE, {resume_pool, PoolRef}).

list_pools() ->
    gen_server:call(?MODULE, list_pools).

pool_info(PoolRef) ->
    gen_server:call(?MODULE, {pool_info, PoolRef}).

add_async(PoolRef, MsgRef, Msg, TimerPid) ->
    gen_server:call(?MODULE, {add_async, PoolRef, MsgRef, Msg, TimerPid}).

async_result(PoolRef, MsgRef, Val) ->
    gen_server:call(?MODULE, {async_result, PoolRef, MsgRef, Val}).

metadata(PoolRef) ->
    case pool_info(PoolRef) of
        {error, not_found} ->
            {error, pool_not_found};
        #{bridge:={BridgeMod, BridgeState}} ->
            case catch BridgeMod:metadata(BridgeState) of
                {'EXIT', {Reason,_}} ->
                    {error, Reason};
                Metadata ->
                    {ok, Metadata}
            end
    end.

-spec send_to_pool(reference(), list()) -> ok | {error, any()}.
send_to_pool(PoolRef, Msgs) ->
    case pool_info(PoolRef) of
        {error, not_found} ->
            {error, pool_not_found};
        #{bridge:={Bridge, BridgeState}, opts:=#{send_retries:=Retries}} ->
            EncodedMsgs = [encode(Bridge, BridgeState, M) || M <- Msgs],
            send_with_retry(EncodedMsgs, Bridge, BridgeState, Retries)
    end.

send_with_retry(FailedMsgs, _, _, 0) ->
    {error, {send_failed, FailedMsgs}};
send_with_retry(Msgs, Bridge, BridgeState, Retries) ->
    case send(Msgs, Bridge, BridgeState) of
        {ok, []} ->
            ok;
        {ok, FailedMsgs} ->
            send_with_retry(FailedMsgs, Bridge, BridgeState, Retries-1);
        {error, _} ->
            send_with_retry(Msgs, Bridge, BridgeState, Retries-1)
    end.

send(Msgs, Bridge, BridgeState) ->
    try {ok, _} = Bridge:send(Msgs, BridgeState)
    catch C:R -> {error, {C,R}}
    end.

%% gen_server callbacks -------------------------------------------------------

init(_) ->
    {ok, #{pools => #{},
           async_msgs => #{},
           default_options => #{num_pollers => 2,
                                max_workers => 5,
                                timer_interval => timer:seconds(15),
                                send_retries => 5,
                                poll_interval => 50}}}.

handle_call({add_pool, Args}, _, State) ->
    #{pools := Pools, default_options := DefaultOpts} = State,
    Ref = make_ref(),
    case add_pool(Ref, Args, DefaultOpts) of
        {ok, Pool} ->
            {reply, {ok, Ref}, State#{pools := Pools#{Ref=>Pool}}};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call({del_pool, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, #{pid := Pid}} ->
            ok = eqw_pool_sup:del_child(Pid),
            {reply, ok, State#{pools := maps:remove(Ref, Pools)}}
    end;
handle_call({pause_pool, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, #{pid := Pid} = Pool} ->
            ok = pause_pool_pollers(Pid),
            NewPools = maps:update(Ref, Pool#{state := paused}, Pools),
            {reply, ok, State#{pools := NewPools}}
    end;
handle_call({resume_pool, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, #{pid := Pid} = Pool} ->
            ok = resume_pool_pollers(Pid),
            NewPools = maps:update(Ref, Pool#{state := polling}, Pools),
            {reply, ok, State#{pools := NewPools}}
    end;
handle_call(list_pools, _, #{pools := Pools} = State) ->
    {reply, maps:keys(Pools), State};
handle_call({pool_info, Ref}, _, #{pools := Pools} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, Pool} ->
            {reply, Pool, State}
    end;
handle_call({add_async, Ref, MsgRef, Msg, TPid}, _,
            #{pools := Pools, async_msgs := AsyncMsgs} = State) ->
    case maps:find(Ref, Pools) of
        error ->
            {reply, {error, not_found}, State};
        {ok, _Pool} ->
            NewState =
                State#{async_msgs => AsyncMsgs#{{Ref, MsgRef} => {Msg, TPid}}},
            {reply, ok, NewState}
    end;
handle_call({async_result, Ref, MsgRef, Val}, _,
            #{pools := Pools, async_msgs := AsyncMsgs} = State) ->
    case {maps:find(Ref, Pools), maps:find({Ref, MsgRef}, AsyncMsgs)} of
        {{ok, Pool}, {ok, MsgInfo}} ->
            Ret = handle_async_result(Pool, MsgInfo, Val),
            NewState =
                State#{async_msgs => maps:remove({Ref, MsgRef}, AsyncMsgs)},
            {reply, Ret, NewState};
        _ ->
            {reply, {error, not_found}, State}
    end;
handle_call(Msg, _, State) ->
    io:format("wtf-msg: ~p~nstate: ~p~n", [Msg, State]),
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal -------------------------------------------------------------------

add_pool(PoolRef, {{Bridge, BridgeArgs}, Worker, Opts}, DefaultOpts) ->
    case init_pool(Bridge, BridgeArgs) of
        {ok, BridgeState} ->
            NewOpts = maps:merge(DefaultOpts, Opts),
            #{num_pollers := NumPollers} = NewOpts,
            {ok, Pool} = new_pool(),
            NewBridge = {Bridge, BridgeState},
            [new_poller(Pool, PoolRef, NewBridge, Worker, NewOpts) ||
              _ <- lists:seq(1, NumPollers)],
            {ok, #{pid => Pool,
                   bridge => NewBridge,
                   worker => Worker,
                   opts => NewOpts,
                   state => polling}};
        {error, Error} ->
            {error, Error}
    end.

init_pool(Bridge, BridgeArgs) ->
    case catch Bridge:setup(BridgeArgs) of
        {error, Reason} ->
            {error, {bridge_setup_error, Reason}};
        {'EXIT', Error} ->
            {error, {bridge_setup_crash, Error}};
        {ok, BridgeState} ->
            {ok, BridgeState}
    end.

new_pool() ->
    eqw_pool_sup:add_child([]).

new_poller(Pool, PoolRef, Bridge, Worker, Opts) ->
    eqw_poller:new(Pool, PoolRef, Bridge, Worker, Opts).

pause_pool_pollers(Pool) ->
    PollerPids = get_pool_pollers(Pool),
    [eqw_poller:pause(P) || P <- PollerPids],
    ok.

resume_pool_pollers(Pool) ->
    PollerPids = get_pool_pollers(Pool),
    [eqw_poller:resume(P) || P <- PollerPids],
    ok.

get_pool_pollers(Pool) ->
    [P || {_, P, _, _} <- supervisor:which_children(Pool)].

encode(Bridge, BridgeState, Msg) ->
    case catch Bridge:encode(Msg, BridgeState) of
        {'EXIT', Reason} ->
            eqw_info:inc(bridge_msg_encode_crash, 1),
            exit({gen_eqw_bridge, Bridge, Reason});
        EncodedMsg ->
            EncodedMsg
    end.

handle_async_result(#{bridge:={BridgeMod, BridgeState}},
                    {Msg, TPid}, Val) ->
    eqw_timer:stop(TPid),
    eqw_info:inc(bridge_async_msg_finished),
    ack_async_result(BridgeMod, BridgeState, Msg, Val).

ack_async_result(BridgeMod, BridgeState, Msg, ok) ->
    case catch BridgeMod:ack(Msg, BridgeState) of
        ok ->
            eqw_info:inc(bridge_msg_ack),
            ok;
        {error, Error} ->
            eqw_info:inc(bridge_msg_ack_error),
            {error, Error};
        Other ->
            eqw_info:inc(bridge_msg_ack_error),
            Other
    end;
ack_async_result(_BridgeMod, _BridgeState, _Msg, error) ->
    eqw_info:inc(bridge_msg_handled),
    error;
ack_async_result(_BridgeMod, _BridgeState, _Msg, Other) ->
    eqw_info:inc(bridge_msg_handled_unknown_return),
    Other.
