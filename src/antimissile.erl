-module(antimissile).
-author("raz & ofir").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([intercepting/3]).

start_link({{Acceleration, Velocity, Position}, PxMax, Ref}) ->
  Name = list_to_atom(lists:append("anti-missile", ref_to_list(Ref))),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {{Acceleration, Velocity, Position}, PxMax, Ref}, [])}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Timer tick %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tick(Ref, TimeDiff) ->
  Name = list_to_atom(lists:append("anti-missile", ref_to_list(Ref))),
  gen_statem:cast(Name, {tick, TimeDiff}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init({{Acceleration, Velocity, Position}, PxMax, Ref}) ->
  mclock:register(antimissile, Ref),
  {ok, intercepting, {{Acceleration, Velocity, Position}, PxMax, Ref}}.

callback_mode() ->
  state_functions.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Timer tick %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
intercepting(cast, {tick, TimeDiff}, {{Acceleration, Velocity, Position}, PxMax, Ref}) ->
  %% calculate next position
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  %% calculate next velocity
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  %% get missiles nearby from server
  MissilesNearby = node_server:getMissiles(),
  %% find if antimissile hit any missile
  HitState = assesHit(NextPosition, MissilesNearby, PxMax),
  case HitState of
    %% no missile was intercepted
    nohit ->
      %% calculate current angle
      Angle = calcAngle(Velocity),
      NextState = intercepting,
      %% prepare data for server
      Status = {NextState, NextVelocity, NextPosition, Angle},
      %% update server on current status and review reaction
      Result = node_server:updateStatus({antimissile, Ref, Status}),
      if
        %% anti missile in region bounds, continue
        Result =:= continue ->
          {next_state, NextState, {{Acceleration, NextVelocity, NextPosition}, PxMax, Ref}};
        %% anti missile out of region, kill
        Result =:= kill ->
          %% unregister from mclock
          mclock:unregister(antimissile, Ref),
          {stop, normal}
      end;
    %% missile intercepted
    {hitmissile, MissileRef} ->
      NextState = successful,
      %% prepare data for server
      Status = {NextState, NextPosition},
      %% unregister from mclock
      mclock:unregister(antimissile, Ref),
      %% notify missile that it was intercepted
      missile:interception(MissileRef),
      %% update the server on interception status
      node_server:updateStatus({antimissile, Ref, Status}),
      {stop, normal};
    %% antimissile out of bounds
    hitsky ->
      NextState = out,
      %% unregister from mclock
      mclock:unregister(antimissile, Ref),
      %% update the server that anti missile is out of bounds
      Status = {NextState, NextPosition},
      node_server:updateStatus({antimissile, Ref, Status}),
      {stop, normal}
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


terminate(_Reason, _State, _Data) ->
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% calculate next position %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updatePosition({Vx, Vy}, {Px, Py}, TimeDiff) ->
  {Px + Vx * TimeDiff, Py + Vy * TimeDiff}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% calculate next velocity %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updateVelocity({Ax, Ay}, {Vx, Vy}, TimeDiff) ->
  {Vx + Ax * TimeDiff, Vy + Ay * TimeDiff}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  asses missile interception %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assesHit({Px, Py}, [], PxMax) ->
  if
    %% check if out of bounds
    (0 >= Py) or (0 >= Px) or (PxMax =< Px) -> hitsky;
    true -> nohit
  end;
%% check if any missile was intercepted
assesHit({Px, Py}, [{MissileRef, PxM, PyM} | Missiles], PxMax) ->
  if
    (abs(Px - PxM) < 15) and (abs(Py - PyM) < 15) -> {hitmissile, MissileRef};
    true -> assesHit({Px, Py}, Missiles, PxMax)
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% calculate antimissile angle %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcAngle({Vx, Vy}) ->
  %% zero division guard (YUCKY)
  VxP = if abs(Vx) < 0.1 ->
    0.01;
          true -> Vx
        end,
  if
    (VxP > 0 andalso Vy >= 0) orelse (VxP > 0 andalso Vy < 0) -> (0.5 * math:pi()) - math:atan(Vy / VxP);
    true -> (1.5 * math:pi()) - math:atan(Vy / VxP)
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%