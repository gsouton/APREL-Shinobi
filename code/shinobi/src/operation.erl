-module(operation).

%generic server and callbacks
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
% api
-export([prepare/0, start/2]).

prepare() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.


start(OperationId, Keikaku) ->
    gen_server:handle_cast(OperationId, Keikaku).

% spawn a process to execute Fun(Arg)
% updates the state on termination
% rudiment(OperationId, Fun, Arg) ->
%     spawn(fun() ->
%              try Fun(Arg) of
%                  R -> gen_server:cast(OperationId, {success, R})
%              catch
%                  error:Reason -> gen_server:cast(OperationId, {failure, Reason})
%              end
%           end).

init([]) ->
    {ok, {ready, 0}}.

handle_call(Request, _From, State) ->
    {reply, Request, State}.

handle_cast({rudiment, _Fun, _Arg}, {State, _}) ->
    if State == ready ->
           % rudiment(Fun, Arg),
           {noreply, {ongoing, 1}};
       true ->
           io:format("")
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
