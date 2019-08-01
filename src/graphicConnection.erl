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
-export([init/0]).


init() ->                 %%TODO: see what kind of information of the Nodes server you need]
  ChangeSettingsControllerPid = spawn_link(fun()->changeSettingsControllerPid() end),
  register(graphicConnectionPID, ChangeSettingsControllerPid), %TODO: maybe we will register it  as global
  timer:sleep(1000), %%TODO: see how much time is needed for the unit to be ready.
  SendingPacketsControllerPid = spawn_link(fun()->sendingPacketsController() end). %Sending Packets Controller




sendingPacketsController() -> %TODO: remove Iteration
  timer:sleep(20), %%TODO: set The refresh time between sends

  spawn_link(fun()-> getQuarterDataAndSend(1) end),
  spawn_link(fun()-> getQuarterDataAndSend(2) end),
  spawn_link(fun()-> getQuarterDataAndSend(3) end),
  spawn_link(fun()-> getQuarterDataAndSend(4) end),
  sendingPacketsController().

getQuarterDataAndSend(Quarter) -> %%TODO: set the connection to server when is possible
  Length=length(nodes()),
  case Quarter of
    1 ->
      PacketData=
        if
          Length >= 1 -> gen_server:call({node_server, lists:nth(1,nodes())}, update); %TODO: check: maybe we need to define the servers as global and call here with {global,GlobalName}
          true -> {[], [],[], [],[], [], []}
        end;
    2 ->
      PacketData=
        if
          Length >= 2 -> gen_server:call({node_server, lists:nth(2,nodes())}, update);
          true -> {[], [],[], [],[], [], []}
        end;
    3 ->
      PacketData=
        if
          Length >= 3 -> gen_server:call({node_server, lists:nth(3,nodes())}, update);
          true -> {[], [],[], [],[], [], []}
        end;
    4 ->
      PacketData=
        if
          Length =:= 4 -> gen_server:call({node_server, lists:nth(4,nodes())}, update);
          true -> {[], [],[], [],[], [], []}
        end
  end,
  io:format("Data~p================================================= ~p~n", [PacketData]),
  gen_statem:cast(graphic, PacketData).



changeSettingsControllerPid() -> %%TODO: set the connection to server and to graphic when is possible
  receive
    Settings ->
      io:format("Settings==============================================~p  microseconds~n",
        [Settings])
  end,
  changeSettingsControllerPid().