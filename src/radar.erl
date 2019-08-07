-module(radar).
-author("Ofir & Raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/1, hit/1, radarTimer/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Start link of radar%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link({Position, MissileTimeDiff, Launchers, _RefreshT, Ref}) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, MissileTimeDiff, Launchers, Ref}, [])}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%cast hit%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hit(Ref) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  gen_statem:cast(Name, hit).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%cast tick- in each tick the radar scans the area for missiles%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tick(Ref) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  gen_statem:cast(Name, tick).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%initial Radar%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({Position, MissileTimeDiff, Launchers, Ref}) ->
  RadarErrorSlider = 5, %initial settings for the radar
  RadarRangeSlider = 5,
  RadarRefreshDelayInit = 5,
  GravitySlider = 5,
  TimerPID = spawn_link(radar, radarTimer, [RadarRefreshDelayInit, Ref]), %initial the radar timer
  node_server:updateStatus({radar, Ref, {alive, Position}}), %set the new status of the radar
  Sight = calcSight(RadarRangeSlider, Position), %define the sight of the radar according to the slider value
  {ok, idle, {TimerPID, Position, Sight, MissileTimeDiff, Launchers, Ref, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelayInit, GravitySlider, #{}}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

callback_mode() ->
  state_functions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Update radar setting event%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {settingUpdate, NewRadarErrorSlider, NewRadarRangeSlider, NewRadarRefreshDelay, NewGravitySlider}, {TimerPID, Position, _Sight, MissileTimeDiff, Launchers, Ref, _RadarErrorSlider, _RadarRangeSlider, _RadarRefreshDelay, _GravitySlider, Missiles}) ->
  NewSight = calcSight(NewRadarRangeSlider, Position), %change the sight of the radar according to new slider values
  {next_state, idle, {TimerPID, Position, NewSight, MissileTimeDiff, Launchers, Ref, NewRadarErrorSlider, NewRadarRangeSlider, NewRadarRefreshDelay, NewGravitySlider, Missiles}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%tick event- scan for missiles, calculate the anti missile target, and order launcher to send the anti-missile%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, tick, {TimerPID, Position, Sight, MissileTimeDiff, Launchers, Ref, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider, Missiles}) ->
  TimerPID ! RadarRefreshDelay,
  MissilesInSight = node_server:getMissiles(Sight), %scan all missiles in sight
  %in the next part we ensure that missile has not detected more then one time by the radar
  NewMissilesInSight = [{Vel, Pos} || {Vel, Pos, MRef} <- MissilesInSight, maps:is_key(MRef, Missiles) =:= false],
  NewMissiles = lists:foldl(fun({_Vel, _Pos, MRef}, Map) -> Counter = maps:get(MRef, Map, new),
    if
      Counter =:= new -> Map#{MRef => 0};
      Counter > 1000 -> maps:remove(MRef, Map);
      true -> Map#{MRef => Counter + 1}
    end end, Missiles, MissilesInSight),
  %calculate the target of the the anti-missiles and launch them
  LaunchTargets = calcTargets(NewMissilesInSight, MissileTimeDiff, [], RadarErrorSlider, GravitySlider),
  lists:foreach(fun(Target) -> Launcher = lists:nth(rand:uniform(length(Launchers)), Launchers),
    node_server:launch(Launcher, Target) end, LaunchTargets),
  {next_state, idle, {TimerPID, Position, Sight, MissileTimeDiff, Launchers, Ref, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider, NewMissiles}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%event-the radar has been hit%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, hit, {_TimerPID, Position, _Sight, _MissileTimeDiff, _Launchers, Ref, _RadarErrorSlider, _RadarRangeSlider, _RadarRefreshDelay, _GravitySlider, _Missiles}) ->
  node_server:updateStatus({launcher, Ref, {destroyed, Position}}), %update the status to destroyed
  {stop, normal}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminate%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_Reason, _State, _Data) ->
  ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%calculate the target of the anti missile%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcTargets([], _MissileTimeDiff, Targets, _RadarErrorSlider, _GravitySlider) -> Targets;
calcTargets([{{Vx, Vy}, {Px, Py}} | Missiles], MissileTimeDiff, Targets, RadarErrorSlider, GravitySlider) ->
  %%we choose a random time in which we will estimate where the missile will be according to velocity equation
  DeltaT = lists:nth(rand:uniform(20), lists:seq(13, 32)) * MissileTimeDiff,
  GRAVITY = GravitySlider * 0.01, %gravity changes according to the gravity slider
  %%we will add some error to the velocity of the missiles
  ErrorX = (rand:uniform(75 + RadarErrorSlider * 5) - 50) / 40,
  ErrorY = (rand:uniform(75 + RadarErrorSlider * 5) - 50) / 40,
  %%the estimation where the missile will be according to velocity equation
  PxT = Px + (Vx + ErrorX) * DeltaT,
  PyT = Py + (Vy + ErrorY) * DeltaT + (GRAVITY * DeltaT * DeltaT) / 2,
  calcTargets(Missiles, MissileTimeDiff, [{{PxT, PyT}, DeltaT} | Targets], RadarErrorSlider, GravitySlider).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%Define the sight of radar according to RadarRangeSlider and its position%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcSight(RadarRangeSlider, {Px, Py}) ->
  {Py - 80 * RadarRangeSlider, Py - 50 * RadarRangeSlider, Py, Px, 60 * RadarRangeSlider}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%The radar timer%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
radarTimer(RadarRefreshDelay, Ref) ->
  timer:sleep(round(RadarRefreshDelay)),
  receive
    RadarRefreshDelayNew -> radar:tick(Ref), radarTimer(RadarRefreshDelayNew, Ref)
  after 1 ->
    radar:tick(Ref),
    radarTimer(RadarRefreshDelay, Ref)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
