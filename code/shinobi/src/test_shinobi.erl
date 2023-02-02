-module(test_shinobi).

-include_lib("eunit/include/eunit.hrl").

% You are allowed to split your test code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_shinobi.

-export([test_all/0, test_everything/0]).
-export([prepare_shinobi/0, register_command/0]). % You may have other exports as well

test_all() ->
  eunit:test([prepare_shinobi(),
              register_command(),
              register_multiple_commands(),
              register_command_should_fail()],
             [verbose]).

test_everything() ->
  test_all().

prepare_shinobi() ->
  {"Prepare shinobi, and nothing else",
   fun() -> ?assertMatch({ok, _}, shinobi:prepare()) end}.

register_command() ->
  {"Register a simple command",
   fun() ->
      {_, S} = shinobi:prepare(),
      ?assertMatch(ok, shinobi:register_command(S, hello_world, whatever))
   end}.

register_multiple_commands() ->
  {"Registering multiple command",
   fun() ->
      {_, S} = shinobi:prepare(),
      Res1 = shinobi:register_command(S, hello_world, whatever),
      Res2 = shinobi:register_command(S, goodbye_world, whatever),
      ?assertEqual(ok, Res1),
      ?assertEqual(ok, Res2)
   end}.

register_command_should_fail() ->
  {"Registering multiple command",
   fun() ->
      {_, S} = shinobi:prepare(),
      Res1 = shinobi:register_command(S, hello_world, whatever),
      Res2 = shinobi:register_command(S, hello_world, whatever),
      ?assertEqual(ok, Res1),
      ?assertEqual({error, already_defined}, Res2)
   end}.
