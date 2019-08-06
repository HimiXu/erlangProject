%%%-------------------------------------------------------------------
%%% @author ofir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2019 9:48 AM
%%%-------------------------------------------------------------------
-module(graphicConnection).
-author("ofir").

%% API
-export([init/1]).


init([Node1, Node2, Node3, Node4]) ->                 %%TODO: see what kind of information of the Nodes server you need
  ChangeSettingsControllerPid = spawn_link(fun() ->
    changeSettingsControllerPid([{1, Node1}, {2, Node2}, {3, Node3}, {4, Node4}]) end),
  register(graphicConnectionPID, ChangeSettingsControllerPid), %TODO: maybe we will register it  as global
  timer:sleep(1000), %%TODO: see how much time is needed for the unit to be ready.
  ReceiverPID = spawn_link(fun() -> sendingPacketsController(
    [{a, Node1},
      {b, Node2},
      {c, Node3},
      {d, Node4}]) end), %Sending Packets Controller
  %% set refresh rate
  spawn_link(fun F() -> timer:sleep(20), ReceiverPID ! tick, F() end),
  {ok, self()}.


sendingPacketsController(NodesAndRegions) ->
  %% send nodes and regions,
  nodeUpdatePid ! NodesAndRegions,
  %% get data from servers
  ReceiverPID = self(),
  NewNodesAndRegions =
    receive
    %% receive order - send all the nodes update signal
      tick -> lists:foreach(fun({Region, Node}) ->
        spawn(fun() -> getQuarterDataAndSend({Region, Node, NodesAndRegions}, ReceiverPID) end) end, NodesAndRegions),
        getNodes([], 0)
    after 1000 ->
      NodesAndRegions
    end,
  %% get data on rising node
  receive
    {nodeUp, Node} -> backup_servers:nodeUp(Node)
  after 1 ->
    continue
  end,
  sendingPacketsController(NewNodesAndRegions).

getQuarterDataAndSend({Region, Node, NodesAndRegions}, ReceiverPID) ->
  Data =
    try
      gen_server:call({node_server, Node}, {update, Region, NodesAndRegions})
    catch _:_ -> crash
    end,
  {PacketData, NewNode} =
    if
      Data =:= crash -> {{[], [], [], [], [], [], []}, backup_servers:nodeDown(Region, Node)};
      true -> backup_servers:stash(Region, Data),
        {filter(Data), Node}
    end,
  ReceiverPID ! {Region, NewNode},
  gen_statem:cast(graphic, PacketData).

getNodes(NewNodesAndRegions, 4) ->
  NewNodesAndRegions;
getNodes(NewNodesAndRegions, Size) ->
  receive
    {Region, Node} -> getNodes([{Region, Node} | NewNodesAndRegions], Size + 1)
  end.


changeSettingsControllerPid(Nodes) -> %%TODO: set the connection to server and to graphic when is possible
  receive
    {restart} ->
      lists:foreach(fun({NodeNum, Node}) -> try gen_server:cast({node_server, Node}, {restart}) of
                                              ok -> ok
                                            catch _:_ ->
        io:format("Couldn't restart - node ~p is node exist~n", [NodeNum])
                                            end end, Nodes);
    Settings ->
      lists:foreach(fun({NodeNum, Node}) -> try gen_server:cast({node_server, Node}, {updateSetting, Settings}) of
                                              ok -> ok
                                            catch _:_ ->
        io:format("Couldn't update Setting - node ~p is node exist~n", [NodeNum])
                                            end end, Nodes)
  end,
  changeSettingsControllerPid(Nodes).

filter({Launchers, Radars, Cities, AntiMissiles, Missiles, Interceptions, Explosions}) ->
  FilteredLaunchers = lists:map(fun({Name, Status, _Position}) -> {Name, Status} end, Launchers),
  FilteredRadars = lists:map(fun({Name, Status, _Position}) -> {Name, Status} end, Radars),
  FilteredCities = lists:map(fun({Name, Status, _Position}) -> {Name, Status} end, Cities),
  FilteredAntiMissiles = lists:map(fun({X, Y, Angle, _Velocity, _Ref}) -> {X, Y, Angle} end, AntiMissiles),
  FilteredMissiles = lists:map(fun({X, Y, Angle, _Velocity, _Ref}) -> {X, Y, Angle} end, Missiles),
  FilteredInterceptions = lists:map(fun({{X, Y}, _Counter}) -> {X, Y} end, Interceptions),
  FilteredExplosions = lists:map(fun({{X, Y}, _Counter}) -> {X, Y} end, Explosions),
  {FilteredLaunchers, FilteredRadars, FilteredCities, FilteredAntiMissiles, FilteredMissiles, FilteredInterceptions, FilteredExplosions}.
