-module(mclock).
-author("raz & ofir").
-behaviour(gen_statem).

%% API
-export([start_link/2]).
-export([tick/1, register/2, unregister/2, generateMissile/3, generateMissile/4, generateAntiMissile/3, setMod/1, stop/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MODS: 0 none, 1 left, 2 right, 3 both %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(TimeDiff, Mod) ->
  %initial values
  MissileScale = 5,
  MissileSpeed = 5,
  GRAVITY = 5,
  ClockPID = spawn(fun F() -> timer:sleep(TimeDiff * 20), mclock:tick(TimeDiff), F() end),
  gen_statem:start_link({local, mclock}, ?MODULE, {ClockPID, TimeDiff, Mod, MissileScale, MissileSpeed, GRAVITY}, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Timer tick %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tick(TimeDiff) ->
  gen_statem:cast(mclock, {tick, TimeDiff}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Missile/Antimissile register %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register(Type, Ref) ->
  gen_statem:cast(mclock, {register, Type, Ref}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Missile/Antimissile unregister %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unregister(Type, Ref) ->
  gen_statem:cast(mclock, {unregister, Type, Ref}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% stop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop() ->
  gen_statem:stop(mclock).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% change MOD of operation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setMod(Region) ->
  gen_statem:cast(mclock, {setMod, Region}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init({ClockPID, TimeDiff, Mod, MissileScale, MissileSpeed, GRAVITY}) ->
  {ok, idle, {ClockPID, TimeDiff, Mod, [], [], MissileScale, MissileSpeed, GRAVITY}}.

callback_mode() ->
  state_functions.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Timer tick %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {tick, TimeDiff}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  %% randomize the missile generation per tick
  CoinFlip = rand:uniform(3000),
  if
    (CoinFlip > 2990 - (MissileScale * 9)) and (Mod > 0) and (Mod < 3) ->
      %% mclock of regions A or B: generate a missile
      generateMissile(Mod, MissileSpeed, GRAVITY);
    (CoinFlip > 2990 - (MissileScale * 9)) and (Mod > 0) and (Mod =:= 3) ->
      %% mclock of regions A and B: generate 2 missiles
      generateMissile(Mod, MissileSpeed, GRAVITY), generateMissile(Mod, MissileSpeed, GRAVITY);
    true -> none
  end,
  %% broadcast the tick
  lists:foreach(fun(Ref) -> missile:tick(Ref, TimeDiff) end, Missiles),
  lists:foreach(fun(Ref) -> antimissile:tick(Ref, TimeDiff) end, AntiMissiles),
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% settings update :                            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MissileScale : number of missiles statistics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MissileSpeed : missiles velocity statistics  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gravity : acceleration pull                  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {settingUpdate, NewMissileScale, NewMissileSpeed, NewGRAVITY}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, _MissileScale, _MissileSpeed, _GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, NewMissileScale, NewMissileSpeed, NewGRAVITY}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% missile registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {register, missile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, [Ref | Missiles], AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% antimissile registration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {register, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, [Ref | AntiMissiles], MissileScale, MissileSpeed, GRAVITY}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% missile unregistration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {unregister, missile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles -- [Ref], AntiMissiles, MissileScale, MissileSpeed, GRAVITY}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% antimissile unregistration %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {unregister, antimissile, Ref}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  {next_state, idle, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles -- [Ref], MissileScale, MissileSpeed, GRAVITY}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% set MOD by region %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {setMod, NewRegion}, {ClockPID, TimeDiff, Mod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}) ->
  NewMod = case NewRegion of
             a -> 1;
             b -> 2;
             c -> 0;
             d -> 0
           end,
  {next_state, idle, {ClockPID, TimeDiff, Mod + NewMod, Missiles, AntiMissiles, MissileScale, MissileSpeed, GRAVITY}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


terminate(_Reason, _State, _Data) ->
%%  io:format("Missiles clock terminated. Reason: ~p~n", [Reason]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% generate missile randomly %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    PosX < 600 -> VelX = rand:uniform(MissileSpeed) / 5;
    true -> VelX = (-1) * rand:uniform(MissileSpeed) / 5
  end,
  generateMissile(make_ref(), {0, GRAVITY}, {VelX, VelY}, {PosX, -10}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% generate missile as requested %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generateMissile(Ref, Acceleration, Velocity, Position) ->
  missile:start_link({{Acceleration, Velocity, Position}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}, {washington, 483, 425}]
    , [], [], 800}, Ref}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% generate antimissile as requested %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generateAntiMissile(Ref, Velocity, Position) ->
  antimissile:start_link({{{0, 0}, Velocity, Position}, 1200, Ref}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%