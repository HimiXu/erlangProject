%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 12:54
%%%-------------------------------------------------------------------
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
  MissileInfo = maps:get(missile, M, empty),
  if MissileInfo =/= empty ->
    {_, Count} = MissileInfo,
    if Count =:= 10 ->
      NewM = maps:remove(missile, M),
      missile:collision(),
      io:format("Missile exploded on server~n"),
      {noreply, NewM};
      true ->
        {noreply, M}
    end;
    true ->
      {noreply, M}
  end;

handle_cast({updateMissile, NextPosition}, M) ->
  MissileInfo = maps:get(missile, M, empty),
  if MissileInfo =:= empty ->
    Count = 0;
    true ->
      {_, Count} = MissileInfo
  end,
  NewM = M#{missile => {NextPosition, Count + 1}},
  io:format("Missile new position on server is: ~p, Current count: ~p~n", [NextPosition, Count + 1]),
  {noreply, NewM}.

handle_call(call, _From, M) ->
  {reply, M, M}.