%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 12:42
%%%-------------------------------------------------------------------
-module(radar).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/1, hit/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).


%%start_link({Position, MissileTimeDiff, Launchers, RefreshT, Ref}) ->
%%  ClockPID = spawn(fun F() -> timer:sleep(RefreshT * 1000), radar:tick(Ref), F() end),
%%  Name = list_to_atom(lists:append("radar", ref_to_list(Ref))),
%%  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, MissileTimeDiff, Launchers, ClockPID, Ref}, [])}.
%%
%%hit(Ref) ->
%%  Name = list_to_atom(lists:append("radar", ref_to_list(Ref))),
%%  gen_statem:cast(Name, hit).
%%tick(Ref) ->
%%  Name = list_to_atom(lists:append("radar", ref_to_list(Ref))),
%%  gen_statem:cast(Name, tick).

start_link({Position, MissileTimeDiff, Launchers, _RefreshT, Ref}) -> %TODO: delete _RefreshT here
  Name = list_to_atom(lists:append("radar", [Ref])),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, MissileTimeDiff, Launchers, Ref}, [])}.

hit(Ref) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  gen_statem:cast(Name, hit).
tick(Ref) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  gen_statem:cast(Name, tick).

init({Position, MissileTimeDiff, Launchers, Ref}) ->
  RadarErrorSlider = 5,
  RadarRangeSlider = 5,
  RadarRefreshDelay = 5,
  GravitySlider = 5,
  spawn_link(fun() -> timer:sleep(round(RadarRefreshDelay * 200)), radar:tick(Ref) end),
  node_server:updateStatus({radar, Ref, {alive, Position}}),
  Sight = calcSight(RadarRangeSlider, Position),
  {ok, idle, {Position, Sight, MissileTimeDiff, Launchers, Ref, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider, []}}.

callback_mode() ->
  state_functions.

idle(cast, {settingUpdate, NewRadarErrorSlider, NewRadarRangeSlider, NewRadarRefreshDelay, NewGravitySlider}, {Position, _Sight, MissileTimeDiff, Launchers, Ref, _RadarErrorSlider, _RadarRangeSlider, _RadarRefreshDelay, _GravitySlider, Missiles}) ->
  NewSight = calcSight(NewRadarRangeSlider, Position),
  {next_state, idle, {Position, NewSight, MissileTimeDiff, Launchers, Ref, NewRadarErrorSlider, NewRadarRangeSlider, NewRadarRefreshDelay, NewGravitySlider, Missiles}};

idle(cast, tick, {Position, Sight, MissileTimeDiff, Launchers, Ref, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider, Missiles}) ->
  spawn_link(fun() -> timer:sleep(round(RadarRefreshDelay * 0.2 * 1500)), radar:tick(Ref) end),
  MissilesInSight = node_server:getMissiles(Sight),
  NewMissilesInSight = [{Vel, Pos} || {Vel, Pos, MRef} <- MissilesInSight, lists:member(MRef, Missiles) =:= false],
  LaunchTargets = calcTargets(NewMissilesInSight, MissileTimeDiff, [], RadarErrorSlider, GravitySlider),
  lists:foreach(fun(Target) -> Launcher = lists:nth(rand:uniform(length(Launchers)), Launchers),
    launcher:launch(Launcher, Target) end, LaunchTargets),
  NewMissiles = [MRef || {_V, _P, MRef} <- MissilesInSight],
  {next_state, idle, {Position, Sight, MissileTimeDiff, Launchers, Ref, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider, NewMissiles}};

idle(cast, hit, {Position, _Sight, _MissileTimeDiff, _Launchers, Ref, _RadarErrorSlider, _RadarRangeSlider, _RadarRefreshDelay, _GravitySlider, _Missiles}) ->
  node_server:updateStatus({launcher, Ref, {destroyed, Position}}),
  {stop, normal}.

terminate(_Reason, _State, _Data) ->
  ok.

calcTargets([], _MissileTimeDiff, Targets, _RadarErrorSlider, _GravitySlider) -> Targets;
calcTargets([{{Vx, Vy}, {Px, Py}} | Missiles], MissileTimeDiff, Targets, RadarErrorSlider, GravitySlider) ->
  DeltaT = lists:nth(rand:uniform(20), lists:seq(23, 42)) * MissileTimeDiff,
  %% TODO add define
  GRAVITY = GravitySlider * 0.01,
  ErrorX = (rand:uniform(75 + RadarErrorSlider * 5) - 50) / 20,
  ErrorY = (rand:uniform(75 + RadarErrorSlider * 5) - 50) / 20,
  PxT = Px + (Vx + ErrorX) * DeltaT,
  PyT = Py + (Vy + ErrorY) * DeltaT + (GRAVITY * DeltaT * DeltaT) / 2,
  calcTargets(Missiles, MissileTimeDiff, [{{PxT, PyT}, DeltaT} | Targets], RadarErrorSlider, GravitySlider).

calcSight(RadarRangeSlider, {Px, Py}) ->
  {Py - 80 * RadarRangeSlider, Py - 50 * RadarRangeSlider, Py, Px, 60 * RadarRangeSlider}.
