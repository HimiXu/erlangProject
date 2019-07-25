%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 12:49
%%%-------------------------------------------------------------------
%%%
-module(missile_test).
-author("raz").

%% API
-export([test/0]).

test() ->
  missile_server:start_link(),
  {M1, _, _} = missile:start_link({0, {0, 0}, {10, -7}, {0, 1000}, testserver, make_ref()}),
  {M2, _, _} = missile:start_link({0, {0, 0}, {0, -7}, {100, 1000}, testserver, make_ref()}),
  startServerClock(),
  startMissileClock([M1, M2]).

startServerClock() ->
  spawn(fun F() -> timer:sleep(500), missile_server:tick(), F() end).

startMissileClock(Refs) ->
  spawn(fun F() -> timer:sleep(1000), lists:foreach(fun(Ref) -> missile:update(Ref, 1) end, Refs), F() end).