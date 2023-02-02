-module(test_kunai).

-include_lib("eunit/include/eunit.hrl").

-export([test_all/0, test_everything/0]).

test_all() ->
    eunit:test([count_rudiment(),
                count_trap(),
                count_progression(),
                count_progression2(),
                rudiment(),
                rudiment2(),
                trap(),
                progression(),
                progression2()],
               [verbose]).

test_everything() ->
    test_all().

count_rudiment() ->
    {"Count rudiment",
     fun() ->
        CountRudiment = kunai:count_tasks({rudiment, a, a}),
        ?assertEqual(1, CountRudiment)
     end}.

count_trap() ->
    {"Count trap",
     fun() ->
        CountTrap = kunai:count_tasks({trap, a}),
        ?assertEqual(1, CountTrap)
     end}.

count_progression() ->
    {"Count progression",
     fun() ->
        SubKeiKakus5 =
            [{rudiment, a, a}, {rudiment, a, a}, {trap, a}, {rudiment, a, a}, {trap, a}],
        Count5 = kunai:count_tasks({progression, SubKeiKakus5}),
        ?assertEqual(5, Count5)
     end}.

count_progression2() ->
    {"Count progression",
     fun() ->
        SubKeiKakus5 =
            [{rudiment, a, a}, {rudiment, a, a}, {trap, a}, {rudiment, a, a}, {trap, a}],
        SubKeiKakus = [{progression, SubKeiKakus5}, {progression, SubKeiKakus5}, {trap, a}],
        Count = kunai:count_tasks({progression, SubKeiKakus}),
        ?assertEqual(11, Count)
     end}.

rudiment() ->
    {"Test rudiment",
     fun() ->
        {_, S} = kunai:start({rudiment, fun() -> ok end, []}),
        timer:sleep(10),
        State = kunai:get_state(S),
        ?assertEqual({success, ok}, State)
     end}.


rudiment2() ->
    {"Test rudiment2",
     fun() ->
        {_, S} = kunai:start({rudiment, fun() -> timer:sleep(500) end, []}),
        ?assertEqual({ongoing, 1}, kunai:get_state(S)),
        timer:sleep(600),
        ?assertEqual({success, ok}, kunai:get_state(S))
     end}.

trap() ->
    {"Test trap",
     fun() ->
        Limit = 10,
        {_, S} = kunai:start({trap, Limit}),
        ?assertEqual({ongoing, 1}, kunai:get_state(S)),
        timer:sleep(100),
        ?assertEqual({failure, timeout}, kunai:get_state(S))
     end}.

progression() ->
    {"Test progression",
     fun() ->
        Rudiment = {rudiment, fun() -> timer:sleep(100), ok end, []},
        ProgList = [Rudiment, Rudiment, Rudiment, Rudiment],
        Progression = {progression, ProgList},
        {_, S} = kunai:start(Progression),
        ?assertEqual({ongoing, 4}, kunai:get_state(S)),
        timer:sleep(700),
        ?assertEqual({success, ok}, kunai:get_state(S))
     end}.

progression2() ->
    {"Test progression2",
     fun() ->
        Rudiment = {rudiment, fun() -> timer:sleep(100), ok end, []},
        Fail = {rudiment, fun() -> throw(fail) end, []},
        ProgList = [Rudiment, Fail, Rudiment, Rudiment],
        Progression = {progression, ProgList},
        {_, S} = kunai:start(Progression),
        ?assertEqual({ongoing, 4}, kunai:get_state(S)),
        timer:sleep(700),
        ?assertEqual({failure, fail}, kunai:get_state(S))
     end}.

% prepare_kunai() ->
%     {"start_kunai", fun() -> ?assertMatch({ok, _}, kunai:prepare()) end}.
%
% start_rudiment() ->
%     {"Start rudiment",
%      fun() ->
%         {_, S} = kunai:prepare(),
%         kunai:start(S, {rudiment, fun() -> ok end, []}),
%         timer:sleep(10),
%         State = kunai:get_state(S),
%         ?assertEqual({success, ok}, State)
%      end}.
