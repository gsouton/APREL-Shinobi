-module(kunai).

%generic server and callbacks
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
% api
-export([start/1, get_state/1]).
-export([process_keikaku/2, rudiment/3, trap/2, count_tasks/1]).


start(KeiKaku) ->
    case gen_server:start_link(?MODULE, KeiKaku, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

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
            % io:format("cast to server: ~p",[OperationId]),
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

% race(OperationId, {race, [Head | Tail]}, ListKunai) ->
%     Pid = kunai:start(Head),
%     kunai:ambush(Pid, fun(Result) -> gen_server:call(OperationId, {Result, Pid})j
%     race(OperationId {race, Tail}, [Pid | ListKunai]).

% process_race(OperationId, 

process_keikaku(OperationId, {rudiment, Fun, Arg}) ->
    rudiment(OperationId, Fun, Arg);

process_keikaku(OperationId, {trap, Limit}) ->
    trap(OperationId, Limit);

process_keikaku(OperationId, {progression, SubKeikakus}) ->
    progression(OperationId, {progression, SubKeikakus});

process_keikaku(OperationId, {race, SubKeikakus}) ->
    undefined.
    % race(OperationId, {race, SubKeikakus}, []).


init(Keikaku) ->
    io:format("Init: ~p\n", [Keikaku]),
    NumTasks = count_tasks(Keikaku),
    io:format("NumTasks: ~p\n", [NumTasks]),
    if NumTasks == 0 ->
           {ok, #{state => {failure, []}, ambushes => []}};
       true ->
            io:format("Spawning: process_keikaku\n"),
            spawn(?MODULE, process_keikaku, [self(), Keikaku]),
            {ok, #{state => {ongoing, NumTasks}, ambushes =>[]}}
    end.

handle_call(get_state, _From, State) ->
    {reply, maps:get(state, State), State};

handle_call(Request, _From, State) ->
    {reply, Request, State}.

handle_cast({failure, Reason}, State) ->
    {noreply, maps:update(state, {failure, Reason}, State)};

handle_cast({success, Result}, State) ->
    {_, Data} = maps:get(state, State),
    if Data == 1 ->
           {noreply, maps:update(state, {success, Result}, State)};
       true ->
           {noreply, maps:update(state, {State, Data-1}, State)}
    end;

handle_cast({ambush, Fun}, State) ->
    case maps:get(state, State) of
        {success, R} ->
            spawn(fun() -> apply(Fun, [R]) end),
            {noreply, State};
        {failure, R} ->
            spawn(fun() -> apply(Fun, [R]) end),
            {noreply, State};
        _ -> 
            {noreply, State}
    end;


handle_cast(_Request, State) ->
    {noreply, State}.

% handle_info({ambush, Fun}, State) ->
%     io:format("Receiving ambush\n"),
%     case State of
%         {success, Result} -> 
%             spawn(?MODULE, Fun, [Result]),
%             {noreply, State};
%         {failure, Result} ->
%             spawn(?MODULE, Fun, [Result]),
%             {noreply, State};
%         _ -> 
%             {noreply, State}
%     end;
        
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
