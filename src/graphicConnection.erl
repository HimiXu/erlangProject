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


init(ServerInfo) ->                 %%TODO: see what kind of information of the Nodes server you need]
  ChangeSettingsControllerPid = spawn_link(fun()->changeSettingsControllerPid(ServerInfo) end),
  graphic:start_link(ChangeSettingsControllerPid), %%start graphic FSM
  timer:sleep(1000), %%TODO: see how much time is needed for the unit to be ready.
  SendingPacketsControllerPid = spawn_link(fun()->sendingPacketsController(ServerInfo) end).




sendingPacketsController(ServerInfo) -> %TODO: remove Iteration
  timer:sleep(30), %%TODO: set The refresh time between sends

  spawn_link(fun()-> getQuarterDataAndSend(1, ServerInfo) end),
  spawn_link(fun()-> getQuarterDataAndSend(2, ServerInfo) end),
  spawn_link(fun()-> getQuarterDataAndSend(3, ServerInfo) end),
  spawn_link(fun()-> getQuarterDataAndSend(4, ServerInfo) end),
  sendingPacketsController(ServerInfo).

getQuarterDataAndSend(Quarter, _ServerInfo) -> %%TODO: set the connection to server when is possible
  case Quarter of
    1 ->
      PacketData= node_server:update();
    2 ->
      PacketData= {[], [],[], [],[], [], []};
    3 ->
      PacketData= {[], [],[], [],[], [], []};
    4 ->
      PacketData= {[], [],[], [],[], [], []}
  end,
  gen_statem:cast(graphic, PacketData).



changeSettingsControllerPid(ServerInfo) -> %%TODO: set the connection to server and to graphic when is possible
  receive
    Settings ->
      io:format("Settings==============================================~p  microseconds~n",
        [Settings])
  end,
  changeSettingsControllerPid(ServerInfo).