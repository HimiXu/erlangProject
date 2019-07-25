%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 12:49
%%%-------------------------------------------------------------------
-module(missile_test).
-author("raz").

%% API
-export([test/0]).

test()->
  missile_server:start_link(),
  missile:start_link({0, {0,0}, {10,-7}, {0,1000}, testserver}),
  startServerClock(),
  startMissileClock().

startServerClock() ->
  spawn(fun F()-> timer:sleep(500), missile_server:tick(), F() end).

startMissileClock() ->
  spawn(fun F()-> timer:sleep(1000), missile:update(1), F() end).