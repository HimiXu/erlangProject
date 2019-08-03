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
-export([start_link/3]).
-export([tick/1, register/2, unregister/2, generateMissile/3, generateAntiMissile/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).

start_link(TimeDiff, Mod, Region) ->
  ClockPID = spawn(fun F() -> timer:sleep(TimeDiff * 20), mclock:tick(TimeDiff), F() end),
  gen_statem:start_link({local, mclock}, ?MODULE, {ClockPID, TimeDiff, Mod, Region}, []).

tick(TimeDiff) ->
  gen_statem:cast(mclock, {tick, TimeDiff}).

register(Type, Ref) ->
  gen_statem:cast(mclock, {register, Type, Ref}).

unregister(Type, Ref) ->
  gen_statem:cast(mclock, {unregister, Type, Ref}).


init({ClockPID, TimeDiff, Mod, Region}) ->
  {ok, idle, {ClockPID, TimeDiff, Mod, Region, [], []}}.

callback_mode() ->
  state_functions.
idle(cast, {tick, TimeDiff}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles}) ->
  CoinFlip = rand:uniform(100),
  if
    (CoinFlip > 98) and (Mod =:= generate) -> generateMissile(Region);
    true -> none
  end,
  lists:foreach(fun(Ref) -> missile:tick(Ref, TimeDiff) end, Missiles),
  lists:foreach(fun(Ref) -> antimissile:tick(Ref, TimeDiff) end, AntiMissiles),
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles}};
idle(cast, {register, missile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, [Ref | Missiles], AntiMissiles}};
idle(cast, {register, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, [Ref | AntiMissiles]}};
idle(cast, {unregister, missile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles -- [Ref], AntiMissiles}};
idle(cast, {unregister, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles -- [Ref]}}.

terminate(Reason, _State, _Data) ->
  io:format("Missiles clock terminated. Reason: ~p~n", [Reason]),
  ok.

generateMissile({RxMin, RxMax}) ->
  GRAVITY = 0.065,
  VelY = rand:uniform(10) / 10,
  PosX = rand:uniform(RxMin + RxMax) - RxMin,
  if
    PosX < 600 -> VelX = rand:uniform(8);
    true -> VelX = (-1)*rand:uniform(8)
  end,
  generateMissile(make_ref(), {0, GRAVITY}, {VelX, VelY}, {PosX, 0}).
generateMissile(Ref, Velocity, Position) ->
  GRAVITY = 0.065,
  generateMissile(Ref, {0, GRAVITY}, Velocity, Position).
generateMissile(Ref, Acceleration, Velocity, Position) ->
  missile:start_link({{Acceleration, Velocity, Position}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [], [], 800}, Ref}).
generateAntiMissile(Ref, Velocity, Position) ->
  antimissile:start_link({{{0, 0}, Velocity, Position}, 1200, Ref}).