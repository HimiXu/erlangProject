%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jul 2019 21:49
%%%-------------------------------------------------------------------
-module(antimissile).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([intercepting/3]).

start_link({{Acceleration, Velocity, Position}, PxMax, Ref}) ->
  Name = list_to_atom(lists:append("anti-missile", ref_to_list(Ref))),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {{Acceleration, Velocity, Position}, PxMax, Ref}, [])}.

tick(Ref, TimeDiff) ->
  Name = list_to_atom(lists:append("anti-missile", ref_to_list(Ref))),
  gen_statem:cast(Name, {tick, TimeDiff}).

init({{Acceleration, Velocity, Position}, PxMax, Ref}) ->
  mclock:register(antimissile, Ref),
  {ok, intercepting, {{Acceleration, Velocity, Position}, PxMax, Ref}}.

callback_mode() ->
  state_functions.
intercepting(cast, {tick, TimeDiff}, {{Acceleration, Velocity, Position}, PxMax, Ref}) ->
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  MissilesNearby = node_server:getMissiles(),
  HitState = assesHit(NextPosition, MissilesNearby, PxMax),
  case HitState of
    nohit -> Angle = calcAngle(Velocity),
      NextState = intercepting,
      Status = {NextState, NextVelocity, NextPosition, Angle},
      node_server:updateStatus({antimissile, Ref, Status}),
      {next_state, NextState, {{Acceleration, NextVelocity, NextPosition}, PxMax, Ref}};

    {hitmissile, MissileRef} -> NextState = successful,
      Status = {NextState, NextPosition},
      mclock:unregister(antimissile, Ref),
      missile:interception(MissileRef),
      node_server:updateStatus({antimissile, Ref, Status}),
      {stop, normal};
    hitsky -> NextState = out,
      mclock:unregister(antimissile, Ref),
      Status = {NextState, NextPosition},
      node_server:updateStatus({antimissile, Ref, Status}),
      {stop, normal}
  end.
terminate(_Reason, _State, _Data) ->
  ok.

updatePosition({Vx, Vy}, {Px, Py}, TimeDiff) ->
  {Px + Vx * TimeDiff, Py + Vy * TimeDiff}.
updateVelocity({Ax, Ay}, {Vx, Vy}, TimeDiff) ->
  {Vx + Ax * TimeDiff, Vy + Ay * TimeDiff}.

assesHit({Px, Py}, [], PxMax) ->
  if
    (0 >= Py) or (0 >= Px) or (PxMax =< Px) -> hitsky;
    true -> nohit
  end;
assesHit({Px, Py}, [{MissileRef, PxM, PyM} | Missiles], PxMax) ->
  if
    (abs(Px - PxM) < 15) and (abs(Py - PyM) < 15) -> {hitmissile, MissileRef};
    true -> assesHit({Px, Py}, Missiles, PxMax)
  end.

calcAngle({Vx, Vy}) ->
  math:atan(Vy / Vx).