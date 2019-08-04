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
-export([tick/1, register/2, unregister/2, generateMissile/3,generateMissile/4 ,generateAntiMissile/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).

start_link(TimeDiff, Mod, Region) ->
  MissileScale=5, %initial values
  MissileSpeed=5,
  GRAVITY=5,
  ClockPID = spawn(fun F() -> timer:sleep(TimeDiff * 20), mclock:tick(TimeDiff), F() end),
  gen_statem:start_link({local, mclock}, ?MODULE, {ClockPID, TimeDiff, Mod, Region, MissileScale, MissileSpeed, GRAVITY}, []).

tick(TimeDiff) ->
  gen_statem:cast(mclock, {tick, TimeDiff}).

register(Type, Ref) ->
  gen_statem:cast(mclock, {register, Type, Ref}).

unregister(Type, Ref) ->
  gen_statem:cast(mclock, {unregister, Type, Ref}).


init({ClockPID, TimeDiff, Mod, Region, MissileScale, MissileSpeed, GRAVITY}) ->
  {ok, idle, {ClockPID, TimeDiff, Mod, Region, [], [], MissileScale, MissileSpeed, GRAVITY}}.

callback_mode() ->
  state_functions.
idle(cast, {tick, TimeDiff}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  CoinFlip = rand:uniform(1000),
  if
    (CoinFlip > 990-(MissileScale*9)) and (Mod =:= generate) -> generateMissile(Region, MissileSpeed, GRAVITY);
    true -> none
  end,
  lists:foreach(fun(Ref) -> missile:tick(Ref, TimeDiff) end, Missiles),
  lists:foreach(fun(Ref) -> antimissile:tick(Ref, TimeDiff) end, AntiMissiles),
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {settingUpdate, NewMissileScale, NewMissileSpeed, NewGRAVITY}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, _MissileScale, _MissileSpeed, _GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, NewMissileScale, NewMissileSpeed, NewGRAVITY}};

idle(cast, {register, missile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, [Ref | Missiles], AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {register, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, [Ref | AntiMissiles], MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {unregister, missile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles -- [Ref], AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {unregister, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Region, Missiles, AntiMissiles -- [Ref], MissileScale, MissileSpeed, GRAVITY}}.

terminate(Reason, _State, _Data) ->
  io:format("Missiles clock terminated. Reason: ~p~n", [Reason]),
  ok.

generateMissile({RxMin, RxMax}, MissileSpeed, GravityScale) ->
  GRAVITY = GravityScale*0.01,     % 0.065,
  VelY = rand:uniform(MissileSpeed) / 10,
  PosX = rand:uniform(RxMin + RxMax) - RxMin,
  if
    PosX < 600 -> VelX = rand:uniform(MissileSpeed);
    true -> VelX = (-1)*rand:uniform(MissileSpeed)
  end,
  generateMissile(make_ref(), {0, GRAVITY}, {VelX, VelY}, {PosX, 0}).

%%generateMissile(Ref, Velocity, Position, Acceleration) ->
%%  generateMissile(Ref, Acceleration, Velocity, Position).

generateMissile(Ref, Acceleration, Velocity, Position) ->
  missile:start_link({{Acceleration, Velocity, Position}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [], [], 800}, Ref}).

generateAntiMissile(Ref, Velocity, Position) ->
  antimissile:start_link({{{0, 0}, Velocity, Position}, 1200, Ref}).