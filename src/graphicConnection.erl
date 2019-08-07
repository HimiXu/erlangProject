-module(graphicConnection).
-author("ofir & raz").

%% API
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% initial the graphic  Connection %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Node1, Node2, Node3, Node4]) ->
  ChangeSettingsControllerPid = spawn_link(fun() ->
    changeSettingsControllerPid([{1, Node1}, {2, Node2}, {3, Node3}, {4, Node4}], {}) end),

  register(graphicConnectionPID, ChangeSettingsControllerPid),
  timer:sleep(1000),
  ReceiverPID = spawn_link(fun() -> sendingPacketsController(
    [{a, Node1},
      {b, Node2},
      {c, Node3},
      {d, Node4}]) end), %Sending Packets Controller
  nodeUpdatePid ! [{a, Node1}, {b, Node2}, {c, Node3}, {d, Node4}],
  %% set refresh rate
  spawn_link(fun F() -> timer:sleep(20), ReceiverPID ! tick, F() end),
  {ok, self()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%A process that control the send of packets to the graphic unit%%%%%%%%%%%%%%%%%%%%%%%%
sendingPacketsController(NodesAndRegions) ->
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%this function ask for data update from the Node_Server, and send it to the graphic unit%%%%%%%%%%%%%%%%%%%%%%%%
getQuarterDataAndSend({Region, Node, NodesAndRegions}, ReceiverPID) ->
  Data =
    try
      gen_server:call({node_server, Node}, {update, Region, NodesAndRegions})
    catch _:_ -> crash
    end,
  {PacketData, NewNode} =
    if
      Data =:= crash -> Reply = backup_servers:nodeDown(Region, Node),
        graphicConnectionPID ! apply,
        nodeUpdatePid ! {Region,Reply},
        io:format("~p~n", [{Region,Reply}]),
        {{[], [], [], [], [], [], []}, Reply};
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%this function gets an update of the simulator settings from buttons and then send it to the nodes servers%%%%%%%%%%%%
changeSettingsControllerPid(Nodes, CurrentSettings) ->
  NewSettings =
    receive
      {restart} ->
        lists:foreach(fun({NodeNum, Node}) ->
          try gen_server:cast({node_server, Node}, {restart}) of
            ok -> ok
          catch _:_ ->
            io:format("Couldn't restart - node ~p is node exist~n", [NodeNum])
          end end, Nodes),
        CurrentSettings;
      apply ->
        lists:foreach(fun({NodeNum, Node}) ->
          try gen_server:cast({node_server, Node}, {updateSetting, CurrentSettings}) of
            ok -> ok
          catch _:_ ->
            io:format("Couldn't update Setting - node ~p is node exist~n", [NodeNum])
          end end, Nodes),
        CurrentSettings;
      Settings ->
        lists:foreach(fun({NodeNum, Node}) ->
          try gen_server:cast({node_server, Node}, {updateSetting, Settings}) of
            ok -> ok
          catch _:_ ->
            io:format("Couldn't update Setting - node ~p is node exist~n", [NodeNum])
          end end, Nodes),
        Settings
    end,
  changeSettingsControllerPid(Nodes, NewSettings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%filter the Data to fit to graphic needs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter({Launchers, Radars, Cities, AntiMissiles, Missiles, Interceptions, Explosions}) ->
  FilteredLaunchers = lists:map(fun({Name, Status, _Position}) -> {Name, Status} end, Launchers),
  FilteredRadars = lists:map(fun({Name, Status, _Position}) -> {Name, Status} end, Radars),
  FilteredCities = lists:map(fun({Name, Status, _Position}) -> {Name, Status} end, Cities),
  FilteredAntiMissiles = lists:map(fun({X, Y, Angle, _Velocity, _Ref}) -> {X, Y, Angle} end, AntiMissiles),
  FilteredMissiles = lists:map(fun({X, Y, Angle, _Velocity, _Ref}) -> {X, Y, Angle} end, Missiles),
  FilteredInterceptions = lists:map(fun({{X, Y}, _Counter}) -> {X, Y} end, Interceptions),
  FilteredExplosions = lists:map(fun({{X, Y}, _Counter}) -> {X, Y} end, Explosions),
  {FilteredLaunchers, FilteredRadars, FilteredCities, FilteredAntiMissiles, FilteredMissiles, FilteredInterceptions, FilteredExplosions}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
