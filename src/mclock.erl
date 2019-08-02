%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 11:27
%%%-------------------------------------------------------------------
-module(mclock).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/2]).
-export([tick/1, register/2, unregister/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).

start_link(TimeDiff, Mod) ->
  ClockPID = spawn(fun F() -> timer:sleep(TimeDiff * 20), mclock:tick(TimeDiff), F() end),
  gen_statem:start_link({local, mclock}, ?MODULE, {ClockPID, TimeDiff, Mod}, []).

tick(TimeDiff) ->
  gen_statem:cast(mclock, {tick, TimeDiff}).

register(Type, Ref) ->
  gen_statem:cast(mclock, {register, Type, Ref}).

unregister(Type, Ref) ->
  gen_statem:cast(mclock, {unregister, Type, Ref}).

init({ClockPID, TimeDiff, Mod}) ->
  {ok, idle, {ClockPID, TimeDiff, Mod, [], []}}.

callback_mode() ->
  state_functions.
idle(cast, {tick, TimeDiff}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles}) ->
  CoinFlip = rand:uniform(100),
  if
    (CoinFlip > 98)  -> generateMissile();
    true -> none
  end,
  lists:foreach(fun(Ref) -> missile:tick(Ref, TimeDiff) end, Missiles),
  lists:foreach(fun(Ref) -> antimissile:tick(Ref, TimeDiff) end, AntiMissiles),
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles}};
idle(cast, {register, missile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, [Ref | Missiles], AntiMissiles}};
idle(cast, {register, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, [Ref | AntiMissiles]}};
idle(cast, {unregister, missile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles -- [Ref], AntiMissiles}};
idle(cast, {unregister, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles -- [Ref]}}.

terminate(Reason, _State, _Data) ->
  io:format("Missiles clock terminated. Reason: ~p~n", [Reason]),
  ok.

generateMissile() ->
  GRAVITY = 0.065,
  VelX = rand:uniform(21) - 11,
  VelY = rand:uniform(10) / 10,
  PosX = rand:uniform(1200),
  missile:start_link({{{0, GRAVITY}, {VelX, VelY}, {PosX, 0}}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [], [], 800}, make_ref()}).