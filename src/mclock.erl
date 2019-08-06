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
-export([tick/1, register/2, unregister/2, generateMissile/3, generateMissile/4, generateAntiMissile/3, setMod/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).


%%% MODS: 0 none, 1 left, 2 right, 3 both
start_link(TimeDiff, Mod) ->
  MissileScale = 5, %initial values
  MissileSpeed = 5,
  GRAVITY = 5,
  ClockPID = spawn(fun F() -> timer:sleep(TimeDiff * 20), mclock:tick(TimeDiff), F() end),
  gen_statem:start_link({local, mclock}, ?MODULE, {ClockPID, TimeDiff, Mod, MissileScale, MissileSpeed, GRAVITY}, []).

tick(TimeDiff) ->
  gen_statem:cast(mclock, {tick, TimeDiff}).

register(Type, Ref) ->
  gen_statem:cast(mclock, {register, Type, Ref}).

unregister(Type, Ref) ->
  gen_statem:cast(mclock, {unregister, Type, Ref}).

setMod(Region) ->
  gen_statem:cast(mclock, {setMod, Region}).
init({ClockPID, TimeDiff, Mod, MissileScale, MissileSpeed, GRAVITY}) ->
  {ok, idle, {ClockPID, TimeDiff, Mod, [], [], MissileScale, MissileSpeed, GRAVITY}}.

callback_mode() ->
  state_functions.
idle(cast, {tick, TimeDiff}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  CoinFlip = rand:uniform(1000),
  if
    (CoinFlip > 990 - (MissileScale * 9)) and (Mod > 0) and (Mod < 3) -> generateMissile(Mod, MissileSpeed, GRAVITY);
    (CoinFlip > 990 - (MissileScale * 9)) and (Mod > 0) and (Mod =:= 3) ->
      generateMissile(Mod, MissileSpeed, GRAVITY), generateMissile(Mod, MissileSpeed, GRAVITY);
    true -> none
  end,
  lists:foreach(fun(Ref) -> missile:tick(Ref, TimeDiff) end, Missiles),
  lists:foreach(fun(Ref) -> antimissile:tick(Ref, TimeDiff) end, AntiMissiles),
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {settingUpdate, NewMissileScale, NewMissileSpeed, NewGRAVITY}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, _MissileScale, _MissileSpeed, _GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, NewMissileScale, NewMissileSpeed, NewGRAVITY}};

idle(cast, {register, missile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, [Ref | Missiles], AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {register, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, [Ref | AntiMissiles], MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {unregister, missile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles -- [Ref], AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};

idle(cast, {unregister, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles -- [Ref], MissileScale, MissileSpeed, GRAVITY}};
idle(cast, {setMod, NewRegion}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  NewMod = case NewRegion of
             a -> 1;
             b -> 2;
             c -> 0;
             d -> 0
           end,
  {next_state, idle, {ClockPID, TimeDiff, Mod + NewMod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}}.
terminate(Reason, _State, _Data) ->
  io:format("Missiles clock terminated. Reason: ~p~n", [Reason]),
  ok.

generateMissile(Mod, MissileSpeed, GravityScale) ->
  GRAVITY = GravityScale * 0.01,     % 0.065,
  VelY = rand:uniform(MissileSpeed) / 10,
  Left = rand:uniform(600),
  Right = rand:uniform(600) + 600,
  Both = rand:uniform(1200),
  PosX = case Mod of
           1 -> Left;
           2 -> Right;
           3 -> Both
         end,
  if
    PosX < 600 -> VelX = rand:uniform(MissileSpeed);
    true -> VelX = (-1) * rand:uniform(MissileSpeed)
  end,
  generateMissile(make_ref(), {0, GRAVITY}, {VelX, VelY}, {PosX, -10}).

%%generateMissile(Ref, Velocity, Position, Acceleration) ->
%%  generateMissile(Ref, Acceleration, Velocity, Position).

generateMissile(Ref, Acceleration, Velocity, Position) ->
  missile:start_link({{Acceleration, Velocity, Position}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [], [], 800}, Ref}).

generateAntiMissile(Ref, Velocity, Position) ->
  antimissile:start_link({{{0, 0}, Velocity, Position}, 1200, Ref}).