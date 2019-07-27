%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 12:38
%%%-------------------------------------------------------------------
-module(city).
-author("raz").
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([hit/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([idle/3, destroyed/3]).

start_link({Position, Name}) ->
  gen_statem:start_link({local, Name}, ?MODULE, {Position, Name}, []).

hit(Name) ->
  gen_statem:cast(Name, hit).

init({Position, Name}) ->
  node_server:updateStatus({city, Name, {alive, Position}}),
  {ok, idle, {Position, Name}}.

callback_mode() ->
  state_functions.
idle(cast, hit, {Position, Name}) ->
  node_server:updateStatus({city, Name, {destroyed, Position}}),
  {next_state, destroyed, {Position, Name}}.
destroyed(cast, _, {Position, Name}) ->
  {next_state, destroyed, {Position, Name}}.

terminate(Reason, _State, {_, _, Ref}) ->
  io:format("City ~p terminated. Reason: ~p~n", [Ref, Reason]),
  ok.
