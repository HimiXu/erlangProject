%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 11:45
%%%-------------------------------------------------------------------
%%%
-module(missile).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/2, interception/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([falling/3]).

start_link({{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}, [])}.

tick(Ref, TimeDiff) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:cast(Name, {tick, TimeDiff}).
interception(Ref) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:cast(Name, interception).

init({{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  mclock:register(missile, Ref),
  {ok, falling, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}}.

callback_mode() ->
  state_functions.
falling(cast, {tick, TimeDiff}, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  HitState = assesHit(NextPosition, Cities, Launchers, Radars, Ground),
  if
    HitState =:= nohit -> Angle = calcAngle(Velocity),
      NextState = falling,
      Status = {NextState, NextVelocity, NextPosition, Angle, Acceleration},
      Result = node_server:updateStatus({missile, Ref, Status}),
      if
        Result =:= continue ->
          {next_state, NextState, {{Acceleration, NextVelocity, NextPosition}, {Cities, Launchers, Radars, Ground}, Ref}};
        Result =:= kill -> {stop, normal}
      end;
    true -> mclock:unregister(missile, Ref),
      node_server:updateStatus({missile, Ref, {exploded, NextPosition}}),
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
falling(cast, interception, {{_Acceleration, _Velocity, Position}, _Targets, Ref}) ->
  io:format("Interception~n"),
  node_server:updateStatus({missile, Ref, {intercepted, Position}}),
  mclock:unregister(missile, Ref),
  {stop, normal}.
terminate(_Reason, _State, _Data) ->
  ok.

updatePosition({Vx, Vy}, {Px, Py}, TimeDiff) ->
  {Px + Vx * TimeDiff, Py + Vy * TimeDiff}.
updateVelocity({Ax, Ay}, {Vx, Vy}, TimeDiff) ->
  {Vx + Ax * TimeDiff, Vy + Ay * TimeDiff}.

assesHit({_Px, Py}, [], [], [], PyG) ->
  if
    Py >= PyG -> hitground;
    true -> nohit
  end;
assesHit({Px, Py}, [], [], [{RadarRef, PxR, PyR} | Radars], PyG) ->
  if
    (abs(Px - PxR) < 25) and (abs(Py - PyR) < 25) -> {hitradar, RadarRef};
    true -> assesHit({Px, Py}, [], [], Radars, PyG)
  end;
assesHit({Px, Py}, [], [{LauncherRef, PxL, PyL} | Launchers], Radars, PyG) ->
  if
    (abs(Px - PxL) < 25) and (abs(Py - PyL) < 25) -> {hitlauncher, LauncherRef};
    true -> assesHit({Px, Py}, [], Launchers, Radars, PyG)
  end;
assesHit({Px, Py}, [{CityName, PxC, PyC} | Cities], Launchers, Radars, PyG) ->
  if
    (abs(Px - PxC) < 50) and (abs(Py - PyC) < 50) -> {hitcity, CityName};
    true -> assesHit({Px, Py}, Cities, Launchers, Radars, PyG)
  end.

calcAngle({Vx, Vy}) ->
  VxP = if abs(Vx) < 0.1 ->
    (Vx / abs(Vx)) * 0.01;
          true -> Vx
        end,
  if
    (VxP >= 0 andalso Vy >= 0) orelse (VxP > 0 andalso Vy < 0) -> (0.5 * math:pi()) - math:atan(Vy / VxP);
    true -> (1.5 * math:pi()) - math:atan(Vy / VxP)
  end.