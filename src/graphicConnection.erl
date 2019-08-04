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
  ChangeSettingsControllerPid = spawn_link(fun() -> changeSettingsControllerPid() end),
  register(graphicConnectionPID, ChangeSettingsControllerPid), %TODO: maybe we will register it  as global
  timer:sleep(1000), %%TODO: see how much time is needed for the unit to be ready.
  ReceiverPID = spawn_link(fun() -> sendingPacketsController(
    [{{0, 600, 0, 400}, Node1},
      {{600, 1200, 0, 400}, Node2},
      {{0, 600, 400, 800}, Node3},
      {{600, 1200, 400, 800}, Node4}]) end), %Sending Packets Controller
  %% set refresh rate
  spawn_link(fun F() -> timer:sleep(20), ReceiverPID ! tick, F() end),
  {ok, self()}.


sendingPacketsController(NodesAndRegions) ->
  ReceiverPID = self(),
  NewNodesAndRegions =
    receive
    %% receive order - send all the nodes update signal
      tick -> lists:foreach(fun({Region, Node}) ->
        spawn(fun() -> getQuarterDataAndSend({Region, Node}, ReceiverPID) end) end, NodesAndRegions),
        getNodes([], 0)
    after 1000 ->
      NodesAndRegions
    end,
  sendingPacketsController(NodesAndRegions).

getQuarterDataAndSend({Region, Node}, ReceiverPID) ->
  PacketData =
    try
      gen_server:call({node_server, Node}, {update, Region})
    catch _:_ -> {[], [], [], [], [], [], []}
    end,
  if PacketData =:= {[], [], [], [], [], [], []} ->
    NewNode = backup_servers:nodeDown(Region, Node);
    true -> NewNode = Node
  end,
  ReceiverPID ! {Region, NewNode},
  gen_statem:cast(graphic, PacketData).

getNodes(NewNodesAndRegions, 4) ->
  NewNodesAndRegions;
getNodes(NewNodesAndRegions, Size) ->
  receive
    {Region, Node} -> getNodes([{Region, Node} | NewNodesAndRegions], Size + 1)
  end.


changeSettingsControllerPid() -> %%TODO: set the connection to server and to graphic when is possible
  receive
    Settings ->
      Length = length(nodes()),
      if Length >= 1 -> try gen_server:cast({node_server, lists:nth(1, nodes())}, {updateSetting, Settings}) of
                          ok -> ok
                        catch _:_ -> io:format("Couldn't update Setting- faild to call node 1~n", [])
                        end;
        true -> io:format("Couldn't update Setting- node 1 is node exist~n", [])
      end,
      if Length >= 2 -> try gen_server:cast({node_server, lists:nth(2, nodes())}, {updateSetting, Settings}) of
                          ok -> ok
                        catch _:_ -> io:format("Couldn't update Setting- faild to call to node 2~n", [])
                        end;
        true -> io:format("Couldn't update Setting- node 2 is node exist~n", [])
      end,
      if Length >= 3 -> try gen_server:cast({node_server, lists:nth(3, nodes())}, {updateSetting, Settings}) of
                          ok -> ok
                        catch _:_ -> io:format("Couldn't update Setting- faild to call to node 3~n", [])
                        end;
        true -> io:format("Couldn't update Setting- node 3 is node exist~n", [])
      end,
      if Length =:= 4 -> try gen_server:cast({node_server, lists:nth(4, nodes())}, {updateSetting, Settings}) of
                           ok -> ok
                         catch _:_ -> io:format("Couldn't update Setting- faild to call to node 4~n", [])
                         end;
        true -> io:format("Couldn't update Setting- node 4 is node exist~n", [])
      end
  end,
  changeSettingsControllerPid().