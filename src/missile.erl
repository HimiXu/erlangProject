%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 11:45
%%%-------------------------------------------------------------------
-module(missile).
-author("raz").
-behaviour(gen_statem).
-define(NAME, missile).

%% API
-export([start_link/1]).
-export([update/1, collision/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([falling/3]).

start_link({Angle, Acceleration, Velocity, Position, Server}) ->
  gen_statem:start_link({local, ?NAME}, ?MODULE, {Angle, Acceleration, Velocity, Position, Server}, []).

update(TimeDiff) ->
  gen_statem:cast(?NAME, {update, TimeDiff}).
collision() ->
  gen_statem:stop(?NAME).

init({Angle, Acceleration, Velocity, Position, Server}) ->
  {ok, falling, {Angle, Acceleration, Velocity, Position, Server}}.

callback_mode() ->
  state_functions.
falling(cast, {update, TimeDiff}, {Angle, Acceleration, Velocity, Position, Server}) ->
  NextPosition = updatePosition(Velocity, Position, TimeDiff),
  NextVelocity = updateVelocity(Acceleration, Velocity, TimeDiff),
  updateServer(NextPosition, Server),
  io:format("Missile new position is: ~p~n",[NextPosition]),
  {next_state, falling, {Angle, Acceleration, NextVelocity, NextPosition, Server}}.

terminate(_Reason, _, _Data) ->
  io:format("Missile exploded \n"),
  ok.

%% Basic linear modeling
updatePosition({Vx, Vy}, {Px, Py}, Cycle) ->
  {Px + Vx * Cycle, Py + Vy * Cycle}.
updateVelocity({Ax, Ay}, {Vx, Vy}, Cycle) ->
  {Vx + Ax * Cycle, Vy + Ay * Cycle}.


updateServer(NextPosition, Server) ->
  gen_server:cast(Server, {updateMissile,NextPosition}).
