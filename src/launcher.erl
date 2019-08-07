
-module(launcher).
-author("raz & ofir").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([launch/2, hit/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).


start_link({Position, PxMax, Ref}) ->
  Name = list_to_atom(lists:append("launcher", [Ref])),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, PxMax, Ref}, [])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Launcher was hit %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hit(Ref) ->
  Name = list_to_atom(lists:append("launcher", [Ref])),
  gen_statem:cast(Name, hit).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Missile to intercept %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
launch(Ref, {TargetPosition, DeltaT}) ->
  Name = list_to_atom(lists:append("launcher", [Ref])),
  gen_statem:cast(Name, {launch, TargetPosition, DeltaT}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init({Position, PxMax, Ref}) ->
  node_server:updateStatus({launcher, Ref, {alive, Position}}),
  {ok, idle, {Position, PxMax, Ref}}.

callback_mode() ->
  state_functions.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Missile to intercept %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, {launch, TargetPosition, DeltaT}, {Position, PxMax, Ref}) ->
  % calculate the velocity required to hit the target
  Velocity = calcVelocity(Position, TargetPosition, DeltaT),
  % generate the anti missile from the launcher with the velocity calculated
  antimissile:start_link({{{0, 0}, Velocity, Position}, PxMax, make_ref()}),
  {next_state, idle, {Position, PxMax, Ref}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Launcher was hit %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast, hit, {Position, _PxMax, Ref}) ->
  node_server:updateStatus({launcher, Ref, {destroyed, Position}}),
  {stop,normal}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


terminate(_Reason, _State, _Data) ->
%%  io:format("Launcher ~p terminated. Reason: ~p~n", [Ref, Reason]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Velocity calculation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcVelocity({Px, Py}, {PxT, PyT}, DeltaT) ->
  DeltaX = PxT - Px,
  DeltaY = PyT - Py,
  {DeltaX / DeltaT, DeltaY / DeltaT}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%