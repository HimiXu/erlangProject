%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 12:42
%%%-------------------------------------------------------------------
-module(radar).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/1, hit/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3, destroyed/3]).


%%start_link({Position, MissileTimeDiff, Launchers, RefreshT, Ref}) ->
%%  ClockPID = spawn(fun F() -> timer:sleep(RefreshT * 1000), radar:tick(Ref), F() end),
%%  Name = list_to_atom(lists:append("radar", ref_to_list(Ref))),
%%  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, MissileTimeDiff, Launchers, ClockPID, Ref}, [])}.
%%
%%hit(Ref) ->
%%  Name = list_to_atom(lists:append("radar", ref_to_list(Ref))),
%%  gen_statem:cast(Name, hit).
%%tick(Ref) ->
%%  Name = list_to_atom(lists:append("radar", ref_to_list(Ref))),
%%  gen_statem:cast(Name, tick).

start_link({Position, MissileTimeDiff, Launchers, RefreshT, Ref}) ->
  ClockPID = spawn(fun F() -> timer:sleep(RefreshT * 1500), radar:tick(Ref), F() end),
  Name = list_to_atom(lists:append("radar", [Ref])),
  {Ref, Name, gen_statem:start_link({local, Name}, ?MODULE, {Position, MissileTimeDiff, Launchers, ClockPID, Ref}, [])}.

hit(Ref) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  gen_statem:cast(Name, hit).
tick(Ref) ->
  Name = list_to_atom(lists:append("radar", [Ref])),
  gen_statem:cast(Name, tick).

init({Position, MissileTimeDiff, Launchers, ClockPID, Ref}) ->
  node_server:updateStatus({radar, Ref, {alive, Position}}),
  Sight = calcSight(Position),
  {ok, idle, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}}.

callback_mode() ->
  state_functions.
idle(cast, tick, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}) ->
  MissilesInSight = node_server:getMissiles(Sight),
  LaunchTargets = calcTargets(MissilesInSight, MissileTimeDiff, []),
  lists:foreach(fun(Target) -> Launcher = lists:nth(rand:uniform(length(Launchers)), Launchers),
    launcher:launch(Launcher, Target) end, LaunchTargets),
  {next_state, idle, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}};
idle(cast, hit, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}) ->
  node_server:updateStatus({launcher, Ref, {destroyed, Position}}),
  {next_state, destroyed, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}}.
destroyed(cast, _, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}) ->
  {next_state, destroyed, {Position, Sight, MissileTimeDiff, Launchers, ClockPID, Ref}}.

terminate(Reason, _State, {_, _, _, _, _, Ref}) ->
  io:format("Radar ~p terminated. Reason: ~p~n", [Ref, Reason]),
  ok.

calcTargets([], _MissileTimeDiff, Targets) -> Targets;
calcTargets([{{Vx, Vy}, {Px, Py}} | Missiles], MissileTimeDiff, Targets) ->
  DeltaT = lists:nth(rand:uniform(20), lists:seq(12, 31)) * MissileTimeDiff,
  %% TODO add define
  GRAVITY = 0.065,
  ErrorX = (rand:uniform(100) - 50) / 20,
  ErrorY = (rand:uniform(100) - 50) / 20,
  PxT = Px + (Vx + ErrorX) * DeltaT,
  PyT = Py + (Vy + ErrorY) * DeltaT + (GRAVITY * DeltaT * DeltaT) / 2,
  calcTargets(Missiles, MissileTimeDiff, [{{PxT, PyT}, DeltaT} | Targets]).

calcSight({Px, Py}) ->
  {Py - 700, Py - 400, Py, Px, 600}.
