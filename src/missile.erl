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
-export([update/2, collision/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([falling/3]).

start_link({Angle, Acceleration, Velocity, Position, Server, Ref}) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  {Ref,Name,gen_statem:start_link({local, Name}, ?MODULE, {Angle, Acceleration, Velocity, Position, Server, Ref}, [])}.

update(Ref, TimeDiff) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:cast(Name, {update, TimeDiff}).
collision(Ref) ->
  Name = list_to_atom(lists:append("missile", ref_to_list(Ref))),
  gen_statem:stop(Name).

init({Angle, Acceleration, Velocity, Position, Server, Ref}) ->
  {ok, falling, {Angle, Acceleration, Velocity, Position, Server, Ref}}.

callback_mode() ->
  state_functions.
falling(cast, {update, TimeDiff}, {Angle, Acceleration, Velocity, Position, Server, Ref}) ->
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  updateServer(NextPosition, Server, Ref),
  io:format("Missile ~p new position is: ~p~n", [Ref, NextPosition]),
  {next_state, falling, {Angle, Acceleration, NextVelocity, NextPosition, Server, Ref}}.

terminate(_Reason, _State, {_, _, _, _, _, Ref}) ->
  io:format("Missile ~p exploded ~n", [Ref]),
  ok.

%% Basic linear modeling
updatePosition({Vx, Vy}, {Px, Py}, Cycle) ->
  {Px + Vx * Cycle, Py + Vy * Cycle}.
updateVelocity({Ax, Ay}, {Vx, Vy}, Cycle) ->
  {Vx + Ax * Cycle, Vy + Ay * Cycle}.


updateServer(NextPosition, Server, Ref) ->
  gen_server:cast(Server, {updateMissile, NextPosition, Ref}).
