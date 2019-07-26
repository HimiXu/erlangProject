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

start_link({{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}, [])}.

tick(Ref, TimeDiff) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:cast(Name, {tick, TimeDiff}).
interception(Ref) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:stop(Name).

init({{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}) ->
  mclock:register(Ref),
  {ok, falling, {{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}}.

callback_mode() ->
  state_functions.
falling(cast, {tick, TimeDiff}, {{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}) ->
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  HitState = assesHit(NextPosition, Cities, Ground),
  case HitState of
    nohit -> Angle = calcAngle(Velocity),
      NextState = falling,
      Status = {NextState, NextVelocity, NextPosition, Angle};
    {hitcity, CityName} -> NextState = exploded,
      Status = {NextState, NextPosition},
      %% TODO
      city:hit(CityName);
    hitground -> NextState = exploded,
      Status = {NextState, NextPosition}
  end,
  %% TODO
  node_server:updateStatus({Ref, Status}),
  {next_state, NextState, {{Acceleration, NextVelocity, NextPosition}, {Cities, Ground}, Ref}};
falling(cast, interception, {{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}) ->
  node_server:updateStatus({Ref, {intercepted, Position}}),
  {next_state, intercepted, {{Acceleration, Velocity, Position}, {Cities, Ground}, Ref}}.
exploded(enter, _State, {_, _, Ref}) ->
  mclock:unregister(Ref),
  {stop, exploded}.
intercepted(enter, _State, {_, _, Ref}) ->
  mclock:unregister(Ref),
  {stop, intercepted}.
terminate(Reason, _State, {_, _, Ref}) ->
  io:format("Missile ~p terminated. Reason: ~p~n", [Ref, Reason]),
  ok.

updatePosition({Vx, Vy}, {Px, Py}, TimeDiff) ->
  {Px + Vx * TimeDiff, Py + Vy * TimeDiff}.
updateVelocity({Ax, Ay}, {Vx, Vy}, TimeDiff) ->
  {Vx + Ax * TimeDiff, Vy + Ay * TimeDiff}.

assesHit({_Px, Py}, [], PyG) ->
  if
    PyG =< Py -> hitground;
    true -> nohit
  end;
assesHit({Px, Py}, [{CityName, PxC, PyC} | Cities], PyG) ->
  if
    (abs(Px - PxC) < 5) and (abs(Py - PyC) < 5) -> {hitcity, CityName};
    true -> assesHit({Px, Py}, Cities, PyG)
  end.

calcAngle({Vx, Vy}) ->
  math:atan(Vy / Vx).