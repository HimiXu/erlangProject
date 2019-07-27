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
-export([falling/3, exploded/3, intercepted/3]).

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
  mclock:register(missile,Ref),
  {ok, falling, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}}.

callback_mode() ->
  state_functions.
falling(cast, {tick, TimeDiff}, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  HitState = assesHit(NextPosition, Cities, Launchers, Radars, Ground),
  case HitState of
    nohit -> Angle = calcAngle(Velocity),
      NextState = falling,
      Status = {NextState, NextVelocity, NextPosition, Angle};
    {hitcity, CityName} -> NextState = exploded,
      Status = {NextState, NextPosition},
      mclock:unregister(missile,Ref),
      city:hit(CityName);
    {hitlauncher, LauncherRef} -> NextState = exploded,
      Status = {NextState, NextPosition},
      mclock:unregister(missile,Ref),
      launcher:hit(LauncherRef);
    {hitradar, RadarRef} -> NextState = exploded,
      Status = {NextState, NextPosition},
      mclock:unregister(missile,Ref),
      radar:hit(RadarRef);
    hitground -> NextState = exploded,
      Status = {NextState, NextPosition},
      mclock:unregister(missile,Ref)
  end,
  %% TODO
  node_server:updateStatus({missile, Ref, Status}),
  {next_state, NextState, {{Acceleration, NextVelocity, NextPosition}, {Cities, Launchers, Radars, Ground}, Ref}};
falling(cast, interception, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}) ->
  node_server:updateStatus({missile, Ref, {intercepted, Position}}),
  mclock:unregister(missile,Ref),
  {next_state, intercepted, {{Acceleration, Velocity, Position}, {Cities, Launchers, Radars, Ground}, Ref}}.
exploded(enter, _State, {_, _, _Ref}) ->
  {stop, exploded}.
intercepted(enter, _State, {_, _, _Ref}) ->
  {stop, intercepted}.
terminate(Reason, _State, {_, _, Ref}) ->
  io:format("Missile ~p terminated. Reason: ~p~n", [Ref, Reason]),
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
    (abs(Px - PxC) < 25) and (abs(Py - PyC) < 25) -> {hitcity, CityName};
    true -> assesHit({Px, Py}, Cities, Launchers, Radars, PyG)
  end.

calcAngle({Vx, Vy}) ->
  math:atan(Vy / Vx).