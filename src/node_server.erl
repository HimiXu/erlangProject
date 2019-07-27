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
-export([start_link/0]).
-export([handle_cast/2, handle_call/3, init/1]).
-export([updateStatus/1, getMissiles/0, getMissiles/1]).

start_link() ->
  gen_server:start_link({local, node_server}, node_server, [], []).

updateStatus({Type, RefOrName, Status}) ->
  gen_server:cast(node_server, {updateStatus, Type, RefOrName, Status}).

getMissiles() ->
  gen_server:call(node_server, getMissiles).

getMissiles(Sight) ->
  gen_server:call(node_server, {getMissiles, Sight}).

init(_Args) ->
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
  {ok, Tables}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateStatus, city, Name, {Status, Position}}, Tables) ->
  CitiesTable = maps:get(Tables, ct, error),
  io:format("City ~p at ~p status: ~p~n", [Name, Position, Status]),
  ets:insert(CitiesTable, {Name, {Status, Position}}),
  {noreply, Tables};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, launcher, Ref, {Status, Position}}, Tables) ->
  LaunchersTable = maps:get(Tables, lt, error),
  io:format("Launcher ~p at ~p status: ~p~n", [Ref, Position, Status]),
  ets:insert(LaunchersTable, {Ref, {Status, Position}}),
  {noreply, Tables};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, radar, Ref, {Status, Position}}, Tables) ->
  RadarsTable = maps:get(Tables, rt, error),
  io:format("Radar ~p at ~p status: ~p~n", [Ref, Position, Status]),
  ets:insert(RadarsTable, {Ref, {Status, Position}}),
  {noreply, Tables};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateStatus, missile, Ref, {falling, Velocity, Position, Angle}}, Tables) ->
  MissilesTable = maps:get(Tables, mt, error),
  io:format("Missile ~p is falling at ~p with velocity ~p~n", [Ref, Position, Velocity]),
  ets:insert(MissilesTable, {Ref, {falling, Velocity, Position, Angle}}),
  {noreply, Tables};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, missile, Ref, {exploded, Position}}, Tables) ->
  MissilesTable = maps:get(Tables, mt, error),
  io:format("Missile ~p exploded at ~p~n", [Ref, Position]),
  ets:delete(MissilesTable, Ref),
  Explosions = maps:get(Tables, explosions, error),
  {noreply, Tables#{explosions => [Position | Explosions]}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, missile, Ref, {intercepted, Position}}, Tables) ->
  MissilesTable = maps:get(Tables, mt, error),
  io:format("Missile ~p intercepted at ~p~n", [Ref, Position]),
  ets:delete(MissilesTable, Ref),
  Interceptions = maps:get(Tables, interceptions, error),
  {noreply, Tables#{explosions => [Position | Interceptions]}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateStatus, antimissile, Ref, {intercepting, Velocity, Position, Angle}}, Tables) ->
  AntiMissilesTable = maps:get(Tables, amt, error),
  io:format("Anti-missile ~p is intercepting at ~p with velocity ~p~n", [Ref, Position, Velocity]),
  ets:insert(AntiMissilesTable, {Ref, {intercepting, Velocity, Position, Angle}}),
  {noreply, Tables};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, antimissile, Ref, {out, _Position}}, Tables) ->
  AntiMissilesTable = maps:get(Tables, amt, error),
  io:format("Anti-missile ~p is out of bounds ~n", [Ref]),
  ets:delete(AntiMissilesTable, Ref),
  {noreply, Tables};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, antimissile, Ref, {successful, Position}}, Tables) ->
  AntiMissilesTable = maps:get(Tables, amt, error),
  io:format("Anti-missile ~p successfully intercepted missile at ~p~n", [Ref, Position]),
  ets:delete(AntiMissilesTable, Ref),
  {noreply, Tables}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(getMissiles, _From, Tables) ->
  MissilesTable = maps:get(Tables, mt, error),
  Missiles = ets:tab2list(MissilesTable),
  {reply, lists:map(fun({Ref, {falling, _Velocity, {Px, Py}, _Angle}}) -> {Ref, Px, Py} end, Missiles), Tables};

handle_call({getMissiles, {PyTop, PyMid, PyBot, PxMid, Width}}, _From, Tables) ->
  MissilesTable = maps:get(Tables, mt, error),
  Missiles = ets:tab2list(MissilesTable),
  MissilesData = lists:map(fun({_Ref, {falling, Velocity, Position, _Angle}}) -> {Velocity, Position} end, Missiles),
  MissilesInSight = lists:filter(fun({_Velocity, {Px, Py}}) ->
    ((Py > PyTop) and (Py =< PyMid) and (Px < PxMid + Width / 2) and (Px > PxMid - Width / 2))
      or
      ((Py >= PyMid) and (Py < PyBot) and
        (((Px < PxMid) and (Px > PxMid - Width / 2) and (Py - PyBot < Px - PxMid))
          or
          ((Px >= PxMid) and (Px < PxMid + Width / 2) and (Py - PyBot < - Px + PxMid))))
                                 end, MissilesData),
  {reply, MissilesInSight, Tables}.