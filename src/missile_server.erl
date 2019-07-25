%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 12:54
%%%-------------------------------------------------------------------
%%%
-module(missile_server).
-author("raz").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([tick/0]).

start_link() ->
  gen_server:start_link({local, testserver}, missile_server, [], []).

tick() ->
  gen_server:cast(testserver, tick).

init(_Args) ->
  {ok, #{}}.

handle_cast(tick, M) ->
  ListM = maps:to_list(M),
  Collisions = [RefA || {RefA,{PositionA,_}} <- ListM, {RefB,{PositionB,_}}<- ListM, RefA =/= RefB, PositionA =:= PositionB ],
  NewM = lists:foldl(fun(ToRemove,Map) -> maps:remove(ToRemove,Map) end, M, Collisions),
  lists:foreach(fun(Ref) -> missile:collision(Ref), io:format("Missile ~p exploded on server~n",[Ref]) end,Collisions),
  {noreply,NewM};

handle_cast({updateMissile, NextPosition, Ref}, M) ->
  MissileInfo = maps:get(Ref, M, empty),
  if MissileInfo =:= empty ->
    Count = 0;
    true ->
      {_, Count} = MissileInfo
  end,
  NewM = M#{Ref => {NextPosition, Count + 1}},
  io:format("Missile ~p new position on server is: ~p, Current count: ~p~n", [Ref, NextPosition, Count + 1]),
  {noreply, NewM}.

handle_call(call, _From, M) ->
  {reply, M, M}.