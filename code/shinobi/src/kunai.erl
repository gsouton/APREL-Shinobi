-module(kunai).

%generic server and callbacks
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
% api
-export([prepare/0, start/1, get_state/1]).
-export([process_keikaku/2, rudiment/3, trap/2, count_tasks/1]).

prepare() ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

start(KeiKaku) ->
    case gen_server:start_link(?MODULE, KeiKaku, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

% start(OperationId, Keikaku) ->
%     gen_server:call(OperationId, Keikaku).

get_state(OperationId) ->
    gen_server:call(OperationId, get_state).

count_tasks({rudiment, _,_}) ->
    1;
count_tasks({trap, _}) ->
    1;
count_tasks({progression, []}) ->
    0;
count_tasks({progression, [Head|Tail]}) ->
    count_tasks(Head) + count_tasks({progression, Tail});

count_tasks({race, []}) ->
    0;

count_tasks({race, [Head|Tail]}) ->
    count_tasks(Head) + count_tasks({race, Tail}).
    

rudiment(OperationId, Fun, Arg) ->
    try apply(Fun, Arg) of
        Result ->
            io:format("cast to server: ~p",[OperationId]),
            gen_server:cast(OperationId, {success, Result}),
            {success, Result}
    catch
        _:Reason ->
            gen_server:cast(OperationId, {failure, Reason}),
            {failure, Reason}
    end.

trap(OperationId, Limit) ->
    timer:sleep(Limit * 10),
    gen_server:cast(OperationId, {failure, timeout}),
    {failure, timeout}.

progression(OperationId, {progression, [Head | []]}) ->
    process_keikaku(OperationId, Head);

progression(OperationId, {progression, [Head|Tail]}) ->
    case process_keikaku(OperationId, Head) of
        {success, _} -> 
            progression(OperationId, {progression, Tail});
        {failure, Reason} ->
            {failure, Reason}
    end.

% race(OperationId, {race, []}) ->
%     gen_server:cast(OperationId, {failure, []}),
%     {failure, []};
%
% race(OperationId, {race, SubKeikakus}) ->
%     lists:foreach(fun() -> spawn(?MODULE, process_keikaku, [OperationId, Keikaku])).


process_keikaku(OperationId, {rudiment, Fun, Arg}) ->
    rudiment(OperationId, Fun, Arg);

process_keikaku(OperationId, {trap, Limit}) ->
    trap(OperationId, Limit);

process_keikaku(OperationId, {progression, SubKeikakus}) ->
    progression(OperationId, {progression, SubKeikakus}).

% process_keikaku(OperationId, {race, SubKeikakus}) ->
%     race(OperationId, {race, SubKeikakus}).


init(Keikaku) ->
    io:format("Init: ~p\n", [Keikaku]),
    NumTasks = count_tasks(Keikaku),
    io:format("NumTasks: ~p\n", [NumTasks]),
    if NumTasks == 0 ->
           {ok, {failure, []}};
       true ->
            io:format("Spawning: process_keikaku\n"),
            spawn(?MODULE, process_keikaku, [self(), Keikaku]),
            {ok, {ongoing, NumTasks}}
    end.



handle_call(get_state, _From, State) ->
    {reply, State, State};
% handle_call({rudiment, _Fun, _Arg}, _From, {State, _Data}) ->
%     case State of
%         ready ->
%             io:format("Spawning rudiment"),
%             io:format("From: ~p\n", [_From]),
%             % spawn(?MODULE, rudiment, [From, Fun, Arg]),
%             {reply, ok, {ongoing, 1}};
%         _ ->
%             {reply, {error, operator_already_assigned}, {State, _Data}}
%     end;
%
% handle_call({trap, Limit}, From, {State, _Data}) ->
%     case State of
%         ready ->
%             trap(From, Limit),
%             {noreply, {ongoing, 1}};
%         _ ->
%             {noreply, {State, _Data}}
%     end;
handle_call(Request, _From, State) ->
    {reply, Request, State}.

handle_cast({failure, Reason}, _State) ->
    {noreply, {failure, Reason}};

handle_cast({success, Result}, {State, Data}) ->
    if Data == 1 ->
           {noreply, {success, Result}};
       true ->
           {noreply, {State, Data-1}}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
