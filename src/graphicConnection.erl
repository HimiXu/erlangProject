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


init() ->                 %%TODO: see what kind of information of the Nodes server you need
  ChangeSettingsControllerPid = spawn_link(fun()->changeSettingsControllerPid() end),
  register(graphicConnectionPID, ChangeSettingsControllerPid), %TODO: maybe we will register it  as global
  timer:sleep(1000), %%TODO: see how much time is needed for the unit to be ready.
  spawn_link(fun()->sendingPacketsController() end), %Sending Packets Controller
  {ok, self()}.


sendingPacketsController() ->
  timer:sleep(20), %%TODO: set The refresh time between sends

  spawn_link(fun()-> getQuarterDataAndSend(1) end),
  spawn_link(fun()-> getQuarterDataAndSend(2) end),
  spawn_link(fun()-> getQuarterDataAndSend(3) end),
  spawn_link(fun()-> getQuarterDataAndSend(4) end),
  sendingPacketsController().

getQuarterDataAndSend(Quarter) ->
  Length=length(nodes()),
  case Quarter of
    1 ->
      PacketData=
        if
          Length >= 1 ->       try  gen_server:call({node_server, lists:nth(1,nodes())}, update) of
                                 Data -> Data
                               catch  _:_->  {[], [],[], [],[], [], []}
                               end;
          true -> {[], [],[], [],[], [], []}
        end;
    2 ->
      PacketData=
        if
          Length >= 2 ->  try  gen_server:call({node_server, lists:nth(2,nodes())}, update) of
                            Data -> Data
                          catch  _:_->  {[], [],[], [],[], [], []}
                          end;
          true -> {[], [],[], [],[], [], []}
        end;
    3 ->
      PacketData=
        if
          Length >= 3 ->  try  gen_server:call({node_server, lists:nth(3,nodes())}, update) of
                            Data -> Data
                          catch  _:_->  {[], [],[], [],[], [], []}
                          end;
          true -> {[], [],[], [],[], [], []}
        end;
    4 ->
      PacketData=
        if
          Length =:= 4 ->  try  gen_server:call({node_server, lists:nth(4,nodes())}, update) of
                             Data -> Data
                           catch  _:_->  {[], [],[], [],[], [], []}
                           end;
          true -> {[], [],[], [],[], [], []}
        end
  end,
  gen_statem:cast(graphic, PacketData).



changeSettingsControllerPid() -> %%TODO: set the connection to server and to graphic when is possible
  receive
    Settings ->
      Length=length(nodes()),
            if Length >= 1 ->       try  gen_server:cast({node_server, lists:nth(1,nodes())}, {updateSetting, Settings}) of
                                     ok -> ok
                                   catch  _:_->  io:format("Couldn't update Setting- faild to call node 1~n", [])
                                   end;
              true -> io:format("Couldn't update Setting- node 1 is node exist~n", [])
            end,
            if Length >= 2 ->  try  gen_server:cast({node_server, lists:nth(2,nodes())}, {updateSetting, Settings}) of
                                 ok -> ok
                              catch  _:_->  io:format("Couldn't update Setting- faild to call to node 2~n", [])
                              end;
              true -> io:format("Couldn't update Setting- node 2 is node exist~n", [])
            end,
            if Length >= 3 ->  try  gen_server:cast({node_server, lists:nth(3,nodes())}, {updateSetting, Settings}) of
                                 ok -> ok
                              catch  _:_->  io:format("Couldn't update Setting- faild to call to node 3~n", [])
                              end;
              true -> io:format("Couldn't update Setting- node 3 is node exist~n", [])
            end,
            if Length =:= 4 ->  try  gen_server:cast({node_server, lists:nth(4,nodes())}, {updateSetting, Settings}) of
                                  ok -> ok
                               catch  _:_->  io:format("Couldn't update Setting- faild to call to node 4~n", [])
                               end;
              true -> io:format("Couldn't update Setting- node 4 is node exist~n", [])
            end
  end,
  changeSettingsControllerPid().