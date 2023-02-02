-module(shinobi).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called shinobi.

-behaviour(gen_server).
%% Callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% Export at least the API:
-export([prepare/0, register_command/3, operation/2, ambush/2, report/1]).

% You may have other exports as well
-export([]).

-type result() :: {success, term()}
                | {failure, term()}
                .

-type keikaku() :: {rudiment, atom(), term()}
                 | {trap, non_neg_integer()}
                 | {progression, nonempty_list(keikaku())}
                 | {race, list(keikaku())}
                 | {side_by_side, list(keikaku())}
                 | {feint, keikaku()}
                 | {relay, keikaku(), fun((result()) -> keikaku())}
                 .

-type operation_status() :: { ongoing, integer() }
                          | { success, term() }
                          | { failure, term() }
                          .

% You may change these types to something more specific, e.g. pid()
-type shinobi() :: pid().
-type operation_id() :: pid().

-record(state, {commands = #{}}).

%%% Public API
-spec prepare() -> {ok, Shinobi :: shinobi()} | {error, Error :: any()}.
prepare() ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec register_command(Shinobi :: shinobi(), Cmd :: atom(), Fun :: fun((term()) -> term())) -> ok | {error, already_defined}.
register_command(Shinobi, Cmd, Fun) ->
    case gen_server:call(Shinobi, {register, Cmd, Fun}) of
        ok -> 
            ok;
        {error, already_defined} ->
            {error, already_defined}
    end.

-spec operation(shinobi(), keikaku()) -> {ok, operation_id()} | {error, Error :: any()}.
operation(Shinobi, Keikaku) ->
gen_server:call(Shinobi, Keikaku).

-spec ambush(operation_id(), fun((result()) -> any())) -> any().
ambush(_OperationId, _Fun) ->
    undefined.

-spec report(operation_id()) -> operation_status().
report(OperationId) ->
    operation:get_state(OperationId).

% -spec validate(keikaku()) -> keikaku().
validate({rudiment, Cmd, Arg}, State) ->
    case maps:is_key(Cmd, State#state.commands) of
        true ->
            Fun = maps:get(Cmd, State#state.commands),
            {rudiment, Fun, Arg};
        false ->
            {error, unknown_cmd}
    end;

validate({trap, Limit}, _State) ->
    if 
        Limit < 0 ->
            {error, invalid_limit};
        true ->
            {trap, Limit}
    end;

            
validate(_Keikaku, _State) ->
    undefined.

%%% Callbacks gen_server

init([]) ->
    {ok, #state{}}.

% Register a command to the server
handle_call({register, Cmd, Fun}, _From, State) ->
    case maps:is_key(Cmd, State#state.commands) of
        true ->
            {reply, {error, already_defined}, State};
        false ->
            NewCommands = maps:put(Cmd, Fun, State#state.commands),
            NewState = State#state{commands = NewCommands},
            {reply, ok , NewState}
    end;


handle_call({rudiment, Cmd, Arg}, _From, State) ->
    case validate({rudiment, Cmd, Arg}, State) of
        {error, Reason} -> {reply, {error, Reason}, State};

        NewKeikaku -> case kunai:start(NewKeikaku) of
                          {ok, OperationId} ->
                              {reply, {ok, OperationId}, State};
                          {error, Reason} -> 
                              {reply, {error, Reason}, State}
                      end
    end;

handle_call({trap, Limit}, _From, State) ->
    case validate({trap, Limit}, State) of
        {error, Reason} -> {reply, {error, Reason}, State};
        NewKeikaku -> case kunai:start(NewKeikaku) of
                          {ok, OperationId} ->
                              {reply, {ok, OperationId}, State};
                          {error, Reason} -> 
                              {reply, {error, Reason}, State}
                      end
    end;

%%% Operations %%%
% handle_call({rudiment, Cmd, Arg}, _From, State) ->
%     case maps:is_key(Cmd, State#state.commands) of
%         true ->
%             case kunai:prepare() of
%                 {ok, OperationId} ->
%                     Fun = maps:get(Cmd, State#state.commands),
%                     kunai:start(OperationId, {rudiment, Fun, Arg}),
%                     {reply, {ok, OperationId}, State};
%                 {error, Reason} ->
%                     {reply, {error, Reason}, State}
%             end;
%         false ->
%             {reply, {error, unknow_cmd}, State}
%     end;

% handle_call({trap, Cmd, Arg}, _From, State) ->
%     case validate({rudiment, Cmd, Arg}) of
%         NewKeikaku -> case kunai:start(NewKeikaku) of
%                           {ok, OperationId} ->
%                               {reply, {ok, OperationId}, State};
%                           {error, Reason} -> 
%                               {reply, {error, Reason}, State}
%                       end;
%         {error, Reason} -> {reply, {error, Reason}, State}
%     end;
% handle_call({trap, Limit}, _From, State) ->
%     if 
%         Limit < 0 ->
%             {reply, {error, invalid_limit}, State};
%         true ->
%             case kunai:prepare() of 
%                 {ok, OperationId} ->
%                     kunai:start(OperationId, {trap, Limit});
%                 {error, Reason} ->
%                     {reply, {error, Reason}, State}
%             end
%     end;
%
% handle_call({progression, SubKeikakus}, _From, State) ->
%     if SubKeikakus == [] ->
%            {reply, {error, invalid_subkeikakus}, State};
%        true ->
%            case kunai:prepare() of 
%                {ok, OperationId} ->
%                    kunai:start(OperationId, {progession, SubKeikakus}),
%                    {reply, {ok, OperationId}, State};
%                 {error, Reason} ->
%                    {reply, {error, Reason}, State}
%             end
%     end;
%
% handle_call({race, SubKeikakus}, _From, State) ->
%    case kunai:prepare() of 
%        {ok, OperationId} ->
%            kunai:start(OperationId, {race, SubKeikakus}),
%            {reply, {ok, OperationId}, State};
%         {error, Reason} ->
%            {reply, {error, Reason}, State}
%     end;
%
% handle_call({side_by_side, SubKeikakus}, _From, State) ->
%    case kunai:prepare() of 
%        {ok, OperationId} ->
%            kunai:start(OperationId, {side_by_side, SubKeikakus}),
%            {reply, {ok, OperationId}, State};
%         {error, Reason} ->
%            {reply, {error, Reason}, State}
%     end;
%
% handle_call({feint, SubKeikakus}, _From, State) ->
%    case kunai:prepare() of 
%        {ok, OperationId} ->
%            kunai:start(OperationId, {feint, SubKeikakus}),
%            {reply, {ok, OperationId}, State};
%         {error, Reason} ->
%            {reply, {error, Reason}, State}
%     end;
%
% handle_call({relay, SubKeikaku, FunOperation}, _From, State) ->
%    case kunai:prepare() of 
%        {ok, OperationId} ->
%            kunai:start(OperationId, {relay, SubKeikaku, FunOperation}),
%            {reply, {ok, OperationId}, State};
%         {error, Reason} ->
%            {reply, {error, Reason}, State}
%     end;
    
%%%%%%%%%%%%%%%%%%
handle_call(Request, _From, State) -> 
    {reply, Request, State}.

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
