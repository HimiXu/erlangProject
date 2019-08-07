-module(node_server).
-author("raz").
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([handle_cast/2, handle_call/3, init/1]).
-export([updateStatus/1, getMissiles/0, getMissiles/1, getMissiles/2, update/2, takeover/1, updateMissiles/1, launch/2]).

-include_lib("stdlib/include/qlc.hrl").

start_link({Node1, Node2, Node3, Node4, Region}) ->
  gen_server:start_link({local, node_server}, node_server, {Node1, Node2, Node3, Node4, Region}, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UPDATE MASTER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update(RegionRequested, NodesAndRegionsNew) ->
  gen_server:call(node_server, {update, RegionRequested, NodesAndRegionsNew}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NODE TAKEOVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
takeover({Region, Backup}) ->
  gen_server:cast(node_server, {takeover, Backup, Region}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FSMs STATUS UPDATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
updateStatus({missile, Ref, {falling, NextVelocity, NextPosition, Angle, Acceleration}}) ->
  gen_server:call(node_server, {updateStatus, missile, Ref, {NextVelocity, NextPosition, Angle, Acceleration}});
updateStatus({antimissile, Ref, {intercepting, NextVelocity, NextPosition, Angle}}) ->
  gen_server:call(node_server, {updateStatus, antimissile, Ref, {NextVelocity, NextPosition, Angle}});
updateStatus({Type, RefOrName, Status}) ->
  gen_server:cast(node_server, {updateStatus, Type, RefOrName, Status}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LAUNCH ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
launch(Launcher, Target) ->
  gen_server:cast(node_server, {launch, Launcher, Target}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILES DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FOR ANTI MISSILES
getMissiles() ->
  gen_server:call(node_server, getMissiles).


%%% FOR RADARS
getMissiles(Sight) ->
  gen_server:call(node_server, {getMissiles, Sight}).
getMissiles(sight, Sight) ->
  gen_server:call(node_server, {getMissiles, sight, Sight}).
updateMissiles(MissilesInSightFromOthers) ->
  gen_server:cast(node_server, {updateMissiles, MissilesInSightFromOthers}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init({Node1, Node2, Node3, Node4, Region}) ->
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
  script:script(Region),
  {ok, {Tables, [{a, Node1}, {b, Node2}, {c, Node3}, {d, Node4}], [Region], []}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SETTINGS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateSetting, {}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};

handle_cast({updateSetting, {0, {missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
  {radarError, RadarErrorSlider}, {radarRange, RadarRangeSlider}, {radarRefreshDelay, RadarRefreshDelay}}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  lists:foreach(fun(Region) ->
    script:changeSettings_script(Region, {missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
      {radarError, RadarErrorSlider}, {radarRange, RadarRangeSlider}, {radarRefreshDelay, RadarRefreshDelay}) end, Regions),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};

handle_cast({updateSetting, {1, {missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
  {radarError, _RadarErrorSlider}, {radarRange, _RadarRangeSlider}, {radarRefreshDelay, _RadarRefreshDelay}}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  lists:foreach(fun(Region) ->
    script:changeSettings_script(Region, {missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
      {radarError, -5}, {radarRange, 20}, {radarRefreshDelay, 0.5}) end, Regions),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% restart cities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({restart}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  Cities = ets:tab2list(maps:get(ct, Tables)),
  %% respawn every destroyed city
  lists:foreach(fun({City, {Status, Position}}) ->
    if Status =/= alive -> city:start_link({Position, City}); true -> none end end, Cities),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LAUNCH ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({launch, Launcher, Target}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  %% get the node of the launcher
  [NodeResp] =
    case Launcher of
      1 -> [Node || {Region, Node} <- NodesAndRegions, Region =:= d];
      2 -> [Node || {Region, Node} <- NodesAndRegions, Region =:= c];
      3 -> [Node || {Region, Node} <- NodesAndRegions, Region =:= d];
      4 -> [Node || {Region, Node} <- NodesAndRegions, Region =:= c]
    end,
  if
    %% if the node is self, then launch
    NodeResp =:= node() -> launcher:launch(Launcher, Target);
    %% if the node is another node, then rpc call launch
    true -> rpc:cast(NodeResp, launcher, launch, [Launcher, Target])
  end,
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateStatus, city, Name, {Status, Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  CitiesTable = maps:get(ct, Tables, error),
  %%io:format("City ~p at ~p status: ~p~n", [Name, Position, Status]),
  ets:insert(CitiesTable, {Name, {Status, Position}}),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, launcher, Ref, {Status, Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  LaunchersTable = maps:get(lt, Tables, error),
  %%io:format("Launcher ~p at ~p status: ~p~n", [Ref, Position, Status]),
  ets:insert(LaunchersTable, {Ref, {Status, Position}}),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};

%----------------------------------------------------------------------------%
handle_cast({updateStatus, radar, Ref, {Status, Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  RadarsTable = maps:get(rt, Tables, error),
  %%io:format("Radar ~p at ~p status: ~p~n", [Ref, Position, Status]),
  ets:insert(RadarsTable, {Ref, {Status, Position}}),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROPERTY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MISSILE EXPLODED
%----------------------------------------------------------------------------%
handle_cast({updateStatus, missile, Ref, {exploded, Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  MissilesTable = maps:get(mt, Tables, error),
  ets:delete(MissilesTable, Ref),
  Explosions = maps:get(explosions, Tables, error),
  {noreply, {Tables#{explosions => [{Position, 0} | Explosions]}, NodesAndRegions, Regions, MissilesInSightFromOthers}};

%% MISSILE INTERCEPTED
%----------------------------------------------------------------------------%
handle_cast({updateStatus, missile, Ref, {intercepted, Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  MissilesTable = maps:get(mt, Tables, error),
%%  io:format("Missile ~p intercepted at ~p~n", [Ref, Position]),
  ets:delete(MissilesTable, Ref),
  Interceptions = maps:get(interceptions, Tables, error),
  {noreply, {Tables#{interceptions => [{Position, 0} | Interceptions]}, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANTIMISSILE OUT OF BOUNDS
%----------------------------------------------------------------------------%
handle_cast({updateStatus, antimissile, Ref, {out, _Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  AntiMissilesTable = maps:get(amt, Tables, error),
%%  io:format("Anti-missile ~p is out of bounds ~n", [Ref]),
  ets:delete(AntiMissilesTable, Ref),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};

%% ANTIMISSILE INTERCEPTED A MISSILE SUCCESFULLY
%----------------------------------------------------------------------------%
handle_cast({updateStatus, antimissile, Ref, {successful, _Position}}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  AntiMissilesTable = maps:get(amt, Tables, error),
%%  io:format("Anti-missile ~p successfully intercepted missile at ~p~n", [Ref, Position]),
  ets:delete(AntiMissilesTable, Ref),
  {noreply, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CRASHES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({takeover, {Launchers, Radars, Cities, AntiMissiles, Missiles, Interceptions, Explosions}, Region}, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  spawn(script, recovery, [{Launchers, Radars, Cities, AntiMissiles, Missiles}, Region]),
  NewExplosions = maps:get(explosions, Tables, error) ++ Explosions,
  NewInterceptions = maps:get(interceptions, Tables, error) ++ Interceptions,
  {noreply, {Tables#{explosions => NewExplosions, interceptions => NewInterceptions}, NodesAndRegions, [Region | Regions], MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CRASHES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RADARS UPDATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({updateMissiles, NewMissilesInSightFromOthers}, {Tables, NodesAndRegions, Regions, _MissilesInSightFromOthers}) ->
  {noreply, {Tables, NodesAndRegions, Regions, NewMissilesInSightFromOthers}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GRAPHICAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({update, RegionRequested, NodesAndRegionsNew}, _From, {Tables, _NodesAndRegionsOld, Regions, MissilesInSightFromOthers}) ->
  %% set region coordinates based on tag
  {RxL, RxR, RyU, RyD} = case RegionRequested of
                           a -> {0, 600, 0, 400};
                           b -> {600, 1200, 0, 400};
                           c -> {0, 600, 400, 800};
                           d -> {600, 1200, 400, 800}
                         end,
  %% get all data in region
  Missiles = qlc:e(qlc:q([{round(X), round(Y), Angle, Velocity, Ref} || {Ref, {falling, Velocity, {X, Y}, Angle}} <- ets:table(maps:get(mt, Tables, error))
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU])),
  AntiMissiles = qlc:e(qlc:q([{round(X), round(Y), Angle, Velocity, Ref} || {Ref, {intercepting, Velocity, {X, Y}, Angle}} <- ets:table(maps:get(amt, Tables, error))
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU])),
  Cities = qlc:e(qlc:q([{Name, Status, {X, Y}} || {Name, {Status, {X, Y}}} <- ets:table(maps:get(ct, Tables, error))
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU])),
  Radars = qlc:e(qlc:q([{Name, Status, {X, Y}} || {Name, {Status, {X, Y}}} <- ets:table(maps:get(rt, Tables, error))
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU])),
  Launchers = qlc:e(qlc:q([{Name, Status, {X, Y}} || {Name, {Status, {X, Y}}} <- ets:table(maps:get(lt, Tables, error))
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU])),
  Explosions = [{{round(X), round(Y)}, Counter} || {{X, Y}, Counter} <- maps:get(explosions, Tables, error)
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU],
  Interceptions = [{{round(X), round(Y)}, Counter} || {{X, Y}, Counter} <- maps:get(interceptions, Tables, error)
    , X =< RxR, X > RxL, Y =< RyD, Y > RyU],
  %% maximum explosions and interceptions frames
  MAX_FRAMES = 4,
  %% update explosions and interceptions counters (by region)
  NewExplosions =
    [{{X, Y}, Counter + 1} || {{X, Y}, Counter} <- Explosions] ++
    [{{X, Y}, Counter} || {{X, Y}, Counter} <- maps:get(explosions, Tables, error),
      lists:member({{round(X), round(Y)}, Counter}, Explosions) =:= false],
  NewInterceptions =
    [{{X, Y}, Counter + 1} || {{X, Y}, Counter} <- Interceptions] ++
    [{{X, Y}, Counter} || {{X, Y}, Counter} <- maps:get(interceptions, Tables, error),
      lists:member({{round(X), round(Y)}, Counter}, Interceptions) =:= false],
  %% prepare the packet
  Packet = {Launchers, Radars, Cities, AntiMissiles, Missiles, Interceptions, Explosions},
  {reply, Packet, {Tables#{explosions => filter(NewExplosions, MAX_FRAMES), interceptions => filter(NewInterceptions, MAX_FRAMES)}, NodesAndRegionsNew, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GRAPHICAL DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTIMISSILE CALL GET MISSILES IN NODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(getMissiles, _From, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  MissilesTable = maps:get(mt, Tables, error),
  Missiles = ets:tab2list(MissilesTable),
  {reply, lists:map(fun({Ref, {falling, _Velocity, {Px, Py}, _Angle}}) ->
    {Ref, Px, Py} end, Missiles), {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RADARS GET MISSILES IN RANGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({getMissiles, sight, {PyTop, PyMid, PyBot, PxMid, Width}}, _From, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  MissilesInSight = qlc:e(qlc:q([{Velocity, {Px, Py}, Ref} || {Ref, {falling, Velocity, {Px, Py}, _Angle}} <- ets:table(maps:get(mt, Tables, error))
    , ((Py > PyTop) and (Py =< PyMid) and (Px < PxMid + Width / 2) and (Px > PxMid - Width / 2))
      or
      ((Py >= PyMid) and (Py < PyBot) and
        (((Px < PxMid) and (Px > PxMid - Width / 2) and (Py - PyBot < Px - PxMid))
          or
          ((Px >= PxMid) and (Px < PxMid + Width / 2) and (Py - PyBot < - Px + PxMid))))])),
  {reply, MissilesInSight, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};

%% ORIGINAL CALL
%%% ------------------------------------------------------------------------------------------------------------------
handle_call({getMissiles, {PyTop, PyMid, PyBot, PxMid, Width}}, _From, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
%% get other nodes
  OtherNodes = [Node || {_Region, Node} <- NodesAndRegions, Node =/= node()],
  %% get all missiles in range in node
  MissilesInSight = qlc:e(qlc:q([{Velocity, {Px, Py}, Ref} || {Ref, {falling, Velocity, {Px, Py}, _Angle}} <- ets:table(maps:get(mt, Tables, error))
    , ((Py > PyTop) and (Py =< PyMid) and (Px < PxMid + Width / 2) and (Px > PxMid - Width / 2))
      or
      ((Py >= PyMid) and (Py < PyBot) and
        (((Px < PxMid) and (Px > PxMid - Width / 2) and (Py - PyBot < Px - PxMid))
          or
          ((Px >= PxMid) and (Px < PxMid + Width / 2) and (Py - PyBot < - Px + PxMid))))])),
  %% get on the next cycle (maybe) all missiles in range in other nodes
  spawn(fun() ->
    MissilesCaught =
      lists:foldl(fun(Node, AllMissiles) ->
        AllMissiles ++
        try
          gen_server:call({node_server, Node}, {getMissiles, sight, {PyTop, PyMid, PyBot, PxMid, Width}})
        catch
          _:_ -> []
        end
                  end, [], OtherNodes),
    node_server:updateMissiles(MissilesCaught) end),
  {reply, MissilesInSight ++ MissilesInSightFromOthers, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RADARS GET MISSILES IN RANGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE UPDATE STATUS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({updateStatus, missile, Ref, {Velocity, {Px, Py}, Angle, Acceleration}}, _From, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  %% get region of missile
  Region =
    if
      (Px < 600) and (Py < 400) -> a;
      (600 =< Px) and (Px < 1200) and (Py < 400) -> b;
      (Px < 600) and (400 =< Py) and (Py < 800) -> c;
      true -> d
    end,
  %% get node responsible for region
  [Node] = [Nd || {Rg, Nd} <- NodesAndRegions, Rg =:= Region],
  MissilesTable = maps:get(mt, Tables, error),
  if
    %% missile is in region, just update its position and return continue
    (Node =:= node()) or (Node =:= []) ->
      ets:insert(MissilesTable, {Ref, {falling, Velocity, {Px, Py}, Angle}}),
      {reply, continue, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
    %% missile is not in region, tell the responsible node to open this missile process there and kill the current
    true ->
      try
        rpc:cast(Node, mclock, generateMissile, [Ref, Acceleration, Velocity, {Px, Py}]),

        ets:delete(MissilesTable, Ref),
        {reply, kill, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}}
      catch _:_ ->
        %% in case of node crash
        ets:insert(MissilesTable, {Ref, {falling, Velocity, {Px, Py}, Angle}}),
        {reply, continue, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}}
      end
  end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISSILE UPDATE STATUS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTI-MISSILE UPDATE STATUS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({updateStatus, antimissile, Ref, {Velocity, {Px, Py}, Angle}}, _From, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}) ->
  %% get region of missile
  Region =
    if
      (Px < 600) and (Py < 400) -> a;
      (600 =< Px) and (Px < 1200) and (Py < 400) -> b;
      (Px < 600) and (400 =< Py) and (Py < 800) -> c;
      true -> d
    end,
  %% get node responsible for region
  [Node] = [Nd || {Rg, Nd} <- NodesAndRegions, Rg =:= Region],
  AntiMissilesTable = maps:get(amt, Tables, error),
  if
  %% missile is in region, just update its position and return continue
    (Node =:= node()) or (Node =:= []) ->
      ets:insert(AntiMissilesTable, {Ref, {intercepting, Velocity, {Px, Py}, Angle}}),
      {reply, continue, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}};
    true ->
      try
        rpc:cast(Node, mclock, generateAntiMissile, [Ref, Velocity, {Px, Py}]),
        ets:delete(AntiMissilesTable, Ref),
        {reply, kill, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}}
      catch
        %% in case of node crash
      _:_ -> ets:insert(AntiMissilesTable, {Ref, {intercepting, Velocity, {Px, Py}, Angle}}),
          {reply, continue, {Tables, NodesAndRegions, Regions, MissilesInSightFromOthers}}
      end
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ANTI-MISSILE UPDATE STATUS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


filter(NewExplosions, MAX_FRAMES) ->
  %% filter all explosion that reached max frames
  lists:filter(fun({_Pos, Counter}) -> Counter < MAX_FRAMES end, NewExplosions).