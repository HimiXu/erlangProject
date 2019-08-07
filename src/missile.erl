-module(missile).
-author("raz & ofir").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/2, interception/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([falling/3]).

start_link({{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}, [])}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Timer tick %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tick(Ref, TimeDiff) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:cast(Name, {tick, TimeDiff}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% missile intercepted %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interception(Ref) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:cast(Name, interception).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init({{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  mclock:register(missile, Ref),
  {ok, falling, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}}.

callback_mode() ->
  state_functions.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Timer tick - update status %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
falling(cast, {tick, TimeDiff}, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  %% calculate next position
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  %% calculate next velocity
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  %% asses hit in new location
  HitState = assesHit(NextPosition, Cities, Launchers, Radars, Ground),
  if
    %% missile hit nothing
    HitState =:= nohit ->
      %% calculate current angle
      Angle = calcAngle(Velocity),
      %% set status to falling
      NextState = falling,
      %% prepare data for server
      Status = {NextState, NextVelocity, NextPosition, Angle, Acceleration},
      %% update server on current status and review its reaction
      Result = node_server:updateStatus({missile, Ref, Status}),
      if
        %% missile is in region bounds, continue
        Result =:= continue ->
          {next_state, NextState, {{Acceleration, NextVelocity, NextPosition}, {Cities, Launchers, Radars, Ground}, Ref}};
        %% missile is out of region bounds, kill
        Result =:= kill ->
          %% unregister from mclock
          mclock:unregister(missile, Ref),
          {stop, normal}
      end;
    %% missile hit something
    true ->
      %% unregister from mclock
      mclock:unregister(missile, Ref),
      %% update server that the missile has exploded
      node_server:updateStatus({missile, Ref, {exploded, NextPosition}}),
      %% notify the object that was hit
      case HitState of
        {hitcity, CityName} ->
          city:hit(CityName);
        {hitlauncher, LauncherRef} ->
          launcher:hit(LauncherRef);
        {hitradar, RadarRef} ->
          radar:hit(RadarRef);
        hitground -> none
      end,
      {stop, normal}
  end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% missile intercepted %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
falling(cast, interception, {{_Acceleration, _Velocity, Position}, _Targets, Ref}) ->
  %% update server that missile was intercepted
  node_server:updateStatus({missile, Ref, {intercepted, Position}}),
  %% unregister from mclock
  mclock:unregister(missile, Ref),
  {stop, normal}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


terminate(_Reason, _State, _Data) ->
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% next position calculation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updatePosition({Vx, Vy}, {Px, Py}, TimeDiff) ->
  {Px + Vx * TimeDiff, Py + Vy * TimeDiff}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% next velocity calculation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updateVelocity({Ax, Ay}, {Vx, Vy}, TimeDiff) ->
  {Vx + Ax * TimeDiff, Vy + Ay * TimeDiff}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% find targets hit %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assesHit({_Px, Py}, [], [], [], PyG) ->
  if
    %% lists are empty, check if hit ground, otherwise return no hit
    Py >= PyG -> hitground;
    true -> nohit
  end;
%% check radar hits
assesHit({Px, Py}, [], [], [{RadarRef, PxR, PyR} | Radars], PyG) ->
  if
    (abs(Px - PxR) < 25) and (abs(Py - PyR) < 25) -> {hitradar, RadarRef};
    true -> assesHit({Px, Py}, [], [], Radars, PyG)
  end;
%% check launcher hits
assesHit({Px, Py}, [], [{LauncherRef, PxL, PyL} | Launchers], Radars, PyG) ->
  if
    (abs(Px - PxL) < 25) and (abs(Py - PyL) < 25) -> {hitlauncher, LauncherRef};
    true -> assesHit({Px, Py}, [], Launchers, Radars, PyG)
  end;
%% check city hits
assesHit({Px, Py}, [{CityName, PxC, PyC} | Cities], Launchers, Radars, PyG) ->
  if
    (abs(Px - PxC) < 50) and (abs(Py - PyC) < 50) -> {hitcity, CityName};
    true -> assesHit({Px, Py}, Cities, Launchers, Radars, PyG)
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% calculate angle of missile %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcAngle({Vx, Vy}) ->
  %% avoid zero division (YUCK)
  VxP = if abs(Vx) < 0.1 ->
    0.01;
          true -> Vx
        end,
  if
    (VxP >= 0 andalso Vy >= 0) orelse (VxP > 0 andalso Vy < 0) -> (0.5 * math:pi()) - math:atan(Vy / VxP);
    true -> (1.5 * math:pi()) - math:atan(Vy / VxP)
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%