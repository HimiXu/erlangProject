%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 12:09
%%%-------------------------------------------------------------------
-module(launcher).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([launch/2, hit/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3, destroyed/3]).

start_link({Position, PxMax, Ref}) ->
  Name = list_to_atom(lists:append("launcher", ref_to_list(Ref))),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, PxMax, Ref}, [])}.

hit(Ref) ->
  Name = list_to_atom(lists:append("launcher", ref_to_list(Ref))),
  gen_statem:cast(Name, hit).
launch(Ref, {TargetPosition, DeltaT}) ->
  Name = list_to_atom(lists:append("launcher", ref_to_list(Ref))),
  gen_statem:cast(Name, {launch, TargetPosition, DeltaT}).

init({Position, PxMax, Ref}) ->
  node_server:updateStatus({launcher, Ref, {alive, Position}}),
  {ok, idle, {Position, PxMax, Ref}}.

callback_mode() ->
  state_functions.
idle(cast, {launch, TargetPosition, DeltaT}, {Position, PxMax, Ref}) ->
  Velocity = calcVelocity(Position, TargetPosition, DeltaT),
  antimissile:start_link({{{0, 0}, Velocity, Position}, PxMax, make_ref()}),
  {next_state, idle, {Position, PxMax, Ref}};
idle(cast, hit, {Position, PxMax, Ref}) ->
  node_server:updateStatus({launcher, Ref, {destroyed, Position}}),
  {next_state, destoryed, {Position, PxMax, Ref}}.
destroyed(cast, _, {Position, PxMax, Ref}) ->
  {next_state, destoryed, {Position, PxMax, Ref}}.

terminate(Reason, _State, {_, _, Ref}) ->
  io:format("Launcher ~p terminated. Reason: ~p~n", [Ref, Reason]),
  ok.

calcVelocity({Px, Py}, {PxT, PyT}, DeltaT) ->
  DeltaX = PxT - Px,
  DeltaY = PyT - Py,
  {DeltaX / DeltaT, DeltaY / DeltaT}.