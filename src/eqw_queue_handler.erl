-module(eqw_queue_handler).

-behaviour(gen_fsm).

%% Management API
-export([start_link/3]).

%% gen_fsm default callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

%% gen_fsm state callbacks
-export([polling/2]).

%%-record(config, {timer, timeout, pool_size}).
%%-type opt() :: timeout | pool_size.

-record(state, {bridge, cfg, mod, mod_state}).

%% Callbacks ------------------------------------------------------------------

%% Management API -------------------------------------------------------------
start_link(Bridge, Module, Args) ->
    case catch Bridge:init_queue() of
        {ok, Config} ->
            gen_fsm:start_link(?MODULE, {Bridge, Config, Module, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

%% Api ------------------------------------------------------------------------

%% gen_fsm standard callbacks -------------------------------------------------

init({Bridge, Module, Args}) ->
    case catch Bridge:setup() of
        {ok, Config} ->
            case catch Module:init(Args) of
                {ok, ModState} ->
                    gen_event:send_event(self(), poll),
                    {ok, polling, #state{bridge=Bridge, cfg=Config,
                                         mod=Module, mod_state=ModState}};
                {stop, Reason} ->
                    {stop, Reason};
                {'EXIT', Reason} ->
                    exit({eqw_queue_handler, init, Reason})
            end;
        {error, Reason} ->
            {stop, {bridge_setup, Reason}};
        {'EXIT', Reason} ->
            exit({Bridge, setup, Reason})
    end.

handle_event(_, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_, _, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_, StateName, State) ->
    {next_state, StateName, State}.

terminate(_, _, _) ->
    ok.

code_change(_, StateName, State, _) ->
    {ok, StateName, State}.

%% gen_fsm state callbacks ----------------------------------------------------

polling(poll, State) ->
    {next_state, listening, State}.

