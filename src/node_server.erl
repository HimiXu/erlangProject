%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 15:25
%%%-------------------------------------------------------------------
-module(node_server).
-author("raz").
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([handle_cast/2, handle_call/3, init/1]).
-export([updateStatus/1, getMissiles/0, getMissiles/1, getMissiles/2, update/0]).

-include_lib("stdlib/include/qlc.hrl").

start_link({Nodes, Region, NodeNum}) ->
  gen_server:start_link({local, node_server}, node_server, {Nodes, Region, NodeNum}, []).


update() ->
  gen_server:call(node_server, update).

updateStatus({missile, Ref, {falling, NextVelocity, NextPosition, Angle, Acceleration}}) ->
  gen_server:call(node_server, {updateStatus, missile, Ref, {NextVelocity, NextPosition, Angle, Acceleration}});
updateStatus({antimissile, Ref, {intercepting, NextVelocity, NextPosition, Angle}}) ->
  gen_server:call(node_server, {updateStatus, antimissile, Ref, {NextVelocity, NextPosition, Angle}});
updateStatus({Type, RefOrName, Status}) ->
  gen_server:cast(node_server, {updateStatus, Type, RefOrName, Status}).

getMissiles() ->
  gen_server:call(node_server, getMissiles).

getMissiles(Sight) ->
  gen_server:call(node_server, {getMissiles, Sight}).
getMissiles(sight, Sight) ->
  gen_server:call(node_server, {getMissiles, sight, Sight}).

init({Nodes, Region, NodeNum}) ->
  MissilesTable = ets:new(missiles, [set]),
  AntiMissilesTable = ets:new(antimissiles, [set]),
  CitiesTable = ets:new(cities, [set]),
  RadarsTable = ets:new(radars, [set]),
  LaunchersTable = ets:new(launchers, [set]),
  Tables = #{mt => MissilesTable,
    amt => AntiMissilesTable,
    ct => CitiesTable,
    rt => RadarsTable,
    lt => LaunchersTable,
    explosions => [],
    interceptions => []},
  script:script(NodeNum),
  {ok, {Tables, Nodes, Region, NodeNum}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SETTINGS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateSetting, {{missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
  {radarError, RadarErrorSlider}, {radarRange, RadarRangeSlider} ,{radarRefreshDelay, RadarRefreshDelay}}}, {Tables, Nodes, Region, NodeNum}) ->
  script:changeSettings_script(NodeNum, {missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
    {radarError, RadarErrorSlider}, {radarRange, RadarRangeSlider} ,{radarRefreshDelay, RadarRefreshDelay}),
  {noreply, {Tables, Nodes, Region, NodeNum}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateStatus, city, Name, {Status, Position}}, {Tables, Nodes, Region, NodeNum}) ->
  CitiesTable = maps:get(ct, Tables, error),
  io:format("City ~p at ~p status: ~p~n", [Name, Position, Status]),
  ets:insert(CitiesTable, {Name, {Status, Position}}),
  {noreply, {Tables, Nodes, Region, NodeNum}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, launcher, Ref, {Status, Position}}, {Tables, Nodes, Region, NodeNum}) ->
  LaunchersTable = maps:get(lt, Tables, error),
  io:format("Launcher ~p at ~p status: ~p~n", [Ref, Position, Status]),
  ets:insert(LaunchersTable, {Ref, {Status, Position}}),
  {noreply, {Tables, Nodes, Region, NodeNum}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, radar, Ref, {Status, Position}}, {Tables, Nodes, Region, NodeNum}) ->
  RadarsTable = maps:get(rt, Tables, error),
  io:format("Radar ~p at ~p status: ~p~n", [Ref, Position, Status]),
  ets:insert(RadarsTable, {Ref, {Status, Position}}),
  {noreply, {Tables, Nodes, Region, NodeNum}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%----------------------------------------------------------------------------%
handle_cast({updateStatus, missile, Ref, {exploded, Position}}, {Tables, Nodes, Region, NodeNum}) ->
  MissilesTable = maps:get(mt, Tables, error),
  ets:delete(MissilesTable, Ref),
  Explosions = maps:get(explosions, Tables, error),
  {noreply, {Tables#{explosions => [{Position, 0} | Explosions]}, Nodes, Region, NodeNum}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, missile, Ref, {intercepted, Position}}, {Tables, Nodes, Region, NodeNum}) ->
  MissilesTable = maps:get(mt, Tables, error),
%%  io:format("Missile ~p intercepted at ~p~n", [Ref, Position]),
  ets:delete(MissilesTable, Ref),
  Interceptions = maps:get(interceptions, Tables, error),
  {noreply, {Tables#{interceptions => [{Position, 0} | Interceptions]}, Nodes, Region, NodeNum}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%----------------------------------------------------------------------------%
handle_cast({updateStatus, antimissile, Ref, {out, _Position}}, {Tables, Nodes, Region, NodeNum}) ->
  AntiMissilesTable = maps:get(amt, Tables, error),
%%  io:format("Anti-missile ~p is out of bounds ~n", [Ref]),
  ets:delete(AntiMissilesTable, Ref),
  {noreply, {Tables, Nodes, Region, NodeNum}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, antimissile, Ref, {successful, _Position}}, {Tables, Nodes, Region, NodeNum}) ->
  AntiMissilesTable = maps:get(amt, Tables, error),
%%  io:format("Anti-missile ~p successfully intercepted missile at ~p~n", [Ref, Position]),
  ets:delete(AntiMissilesTable, Ref),
  {noreply, {Tables, Nodes, Region, NodeNum}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(update, _From, {Tables, Nodes, Region, NodeNum}) ->
  Missiles = qlc:e(qlc:q([{round(X), round(Y), Angle} || {_Ref, {falling, _Velocity, {X, Y}, Angle}} <- ets:table(maps:get(mt, Tables, error))])),
  AntiMissiles = qlc:e(qlc:q([{round(X), round(Y), Angle} || {_Ref, {intercepting, _Velocity, {X, Y}, Angle}} <- ets:table(maps:get(amt, Tables, error))])),
  Cities = qlc:e(qlc:q([{Name, Status} || {Name, {Status, _Position}} <- ets:table(maps:get(ct, Tables, error))])),
  Radars = qlc:e(qlc:q([{Name, Status} || {Name, {Status, _Position}} <- ets:table(maps:get(rt, Tables, error))])),
  Launchers = qlc:e(qlc:q([{Name, Status} || {Name, {Status, _Position}} <- ets:table(maps:get(lt, Tables, error))])),
  Explosions = lists:map(fun({{X, Y}, _Counter}) -> {round(X), round(Y)} end, maps:get(explosions, Tables, error)),
  Interceptions = lists:map(fun({{X, Y}, _Counter}) ->
    {round(X), round(Y)} end, maps:get(interceptions, Tables, error)),
  MAX_FRAMES = 4,
  NewExplosions = [{{X, Y}, Counter + 1} || {{X, Y}, Counter} <- maps:get(explosions, Tables, error), Counter < MAX_FRAMES],
  NewInterceptions = [{{X, Y}, Counter + 1} || {{X, Y}, Counter} <- maps:get(interceptions, Tables, error), Counter < MAX_FRAMES],
  Packet = {Launchers, Radars, Cities, AntiMissiles, Missiles, Interceptions, Explosions},
  {reply, Packet, {Tables#{explosions => NewExplosions, interceptions => NewInterceptions}, Nodes, Region, NodeNum}};

handle_call(getMissiles, _From, {Tables, Nodes, Region, NodeNum}) ->
  MissilesTable = maps:get(mt, Tables, error),
  Missiles = ets:tab2list(MissilesTable),
  {reply, lists:map(fun({Ref, {falling, _Velocity, {Px, Py}, _Angle}}) ->
    {Ref, Px, Py} end, Missiles), {Tables, Nodes, Region, NodeNum}};

handle_call({getMissiles, sight, {PyTop, PyMid, PyBot, PxMid, Width}}, _From, {Tables, Nodes, Region, NodeNum}) ->
  MissilesTable = maps:get(mt, Tables, error),
  Missiles = ets:tab2list(MissilesTable),
  MissilesData = lists:map(fun({_Ref, {falling, Velocity, Position, _Angle}}) -> {Velocity, Position} end, Missiles),
  MissilesInSight = (lists:filter(fun({_Velocity, {Px, Py}}) ->
    ((Py > PyTop) and (Py =< PyMid) and (Px < PxMid + Width / 2) and (Px > PxMid - Width / 2))
      or
      ((Py >= PyMid) and (Py < PyBot) and
        (((Px < PxMid) and (Px > PxMid - Width / 2) and (Py - PyBot < Px - PxMid))
          or
          ((Px >= PxMid) and (Px < PxMid + Width / 2) and (Py - PyBot < - Px + PxMid))))
                                  end, MissilesData)),
  {reply, MissilesInSight, {Tables, Nodes, Region, NodeNum}};

handle_call({getMissiles, {PyTop, PyMid, PyBot, PxMid, Width}}, _From, {Tables, Nodes, Region, NodeNum}) ->
  MissilesTable = maps:get(mt, Tables, error),
  Missiles = ets:tab2list(MissilesTable),
  OtherNodes = [Node || {Node, _, Ry} <- Nodes, Node =/= node(), Ry =/= 800],
  MissilesData = lists:map(fun({_Ref, {falling, Velocity, Position, _Angle}}) -> {Velocity, Position} end, Missiles),
  MissilesInSight = (lists:filter(fun({_Velocity, {Px, Py}}) ->
    ((Py > PyTop) and (Py =< PyMid) and (Px < PxMid + Width / 2) and (Px > PxMid - Width / 2))
      or
      ((Py >= PyMid) and (Py < PyBot) and
        (((Px < PxMid) and (Px > PxMid - Width / 2) and (Py - PyBot < Px - PxMid))
          or
          ((Px >= PxMid) and (Px < PxMid + Width / 2) and (Py - PyBot < - Px + PxMid))))
                                  end, MissilesData)),
  MissilesCaught = lists:foldl(fun(Node, AllMissiles) ->
    AllMissiles ++ rpc:call(Node, node_server, getMissiles, [sight, {PyTop, PyMid, PyBot, PxMid, Width}]) end, MissilesInSight, OtherNodes),
  {reply, MissilesCaught, {Tables, Nodes, Region, NodeNum}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CALLS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({updateStatus, missile, Ref, {Velocity, {Px, Py}, Angle, Acceleration}}, _From, {Tables, Nodes, {Rx, Ry}, NodeNum}) ->
  if
    (Px < 600) and (Py < 400) -> {Node, _, _} = lists:nth(1, Nodes);
    (Px < 600) and (400 =< Py) and (Py < 800) -> {Node, _, _} = lists:nth(3, Nodes);
    (600 =< Px) and (Px < 1200) and (Py < 400) -> {Node, _, _} = lists:nth(2, Nodes);
    (600 =< Px) and (Px < 1200) and (400 =< Py) and (Py < 800) -> {Node, _, _} = lists:nth(4, Nodes);
    true -> Node = []
  end,
  MissilesTable = maps:get(mt, Tables, error),
  if
    (Node =:= node()) or (Node =:= [])->
      ets:insert(MissilesTable, {Ref, {falling, Velocity, {Px, Py}, Angle}}),
      {reply, continue, {Tables, Nodes, {Rx, Ry},NodeNum}};
    true ->
      io:format("Missile ~p entered node ~p~n", [Ref, Node]),
      rpc:cast(Node, mclock, generateMissile, [Ref, Acceleration, Velocity, {Px, Py}]),
      ets:delete(MissilesTable, Ref),
      {reply, kill, {Tables, Nodes, {Rx, Ry}, NodeNum}}
  end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTI-MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({updateStatus, antimissile, Ref, {Velocity, {Px, Py}, Angle}}, _From, {Tables, Nodes, {Rx, Ry}, NodeNum}) ->
  if
    (Px < 600) and (Py < 400) -> {Node, _, _} = lists:nth(1, Nodes);
    (Px < 600) and (400 =< Py) and (Py < 800) -> {Node, _, _} = lists:nth(3, Nodes);
    (600 =< Px) and (Px < 1200) and (Py < 400) -> {Node, _, _} = lists:nth(2, Nodes);
    (600 =< Px) and (Px < 1200) and (400 =< Py) and (Py < 800) -> {Node, _, _} = lists:nth(4, Nodes);
    true -> Node = []
  end,
  AntiMissilesTable = maps:get(amt, Tables, error),
  if
    (Node =:= node()) or (Node =:= []) ->
      ets:insert(AntiMissilesTable, {Ref, {intercepting, Velocity, {Px, Py}, Angle}}),
      {reply, continue, {Tables, Nodes, {Rx, Ry}, NodeNum}};
    true ->
      io:format("Antimissile ~p entered node ~p~n", [Ref, Node]),
      rpc:cast(Node, mclock, generateAntiMissile, [Ref, Velocity, {Px, Py}]),
      ets:delete(AntiMissilesTable, Ref),
      {reply, kill, {Tables, Nodes, {Rx, Ry}, NodeNum}}
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTI-MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CALLS  And casts of graphic%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

