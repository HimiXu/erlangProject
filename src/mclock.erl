%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 11:27
%%%-------------------------------------------------------------------
-module(mclock).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([tick/1, register/2, unregister/2]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3]).

start_link(TimeDiff) ->
  ClockPID = spawn(fun F() -> timer:sleep(TimeDiff*20), mclock:tick(TimeDiff), F() end),
  gen_statem:start_link({local, mclock}, ?MODULE, {ClockPID, TimeDiff}, []).

tick(TimeDiff) ->
  gen_statem:cast(mclock, {tick, TimeDiff}).

register(Type, Ref) ->
  gen_statem:cast(mclock, {register, Type, Ref}).

unregister(Type, Ref) ->
  gen_statem:cast(mclock, {unregister, Type, Ref}).

init({ClockPID, TimeDiff}) ->
  {ok, idle, {ClockPID, TimeDiff, [], []}}.

callback_mode() ->
  state_functions.
idle(cast, {tick, TimeDiff}, {ClockPID, TimeDiff, Missiles, AntiMissiles}) ->
  lists:foreach(fun(Ref) -> missile:tick(Ref, TimeDiff) end, Missiles),
  lists:foreach(fun(Ref) -> antimissile:tick(Ref, TimeDiff) end, AntiMissiles),
  {next_state, idle, {ClockPID, TimeDiff, Missiles, AntiMissiles}};
idle(cast, {register, missile, Ref}, {ClockPID, TimeDiff, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, [Ref | Missiles], AntiMissiles}};
idle(cast, {register, antimissile, Ref}, {ClockPID, TimeDiff, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Missiles, [Ref | AntiMissiles]}};
idle(cast, {unregister, missile, Ref}, {ClockPID, TimeDiff, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Missiles -- [Ref], AntiMissiles}};
idle(cast, {unregister, antimissile, Ref}, {ClockPID, TimeDiff, Missiles, AntiMissiles}) ->
  {next_state, idle, {ClockPID, TimeDiff, Missiles, AntiMissiles -- [Ref]}}.

terminate(Reason, _State, _Data) ->
  io:format("Missiles clock terminated. Reason: ~p~n", [Reason]),
  ok.