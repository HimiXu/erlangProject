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
  graphic:start_link(), %%start graphic FSM
  timer:sleep(1000), %%TODO: see how much time is needed for the unit to be ready.
  SendingPacketsControllerPid = spawn_link(fun()->sendingPacketsController(ServerInfo, 0) end),
  ChangeSettingsControllerPid = spawn_link(fun()->changeSettingsControllerPid(ServerInfo) end).



sendingPacketsController(ServerInfo, Iteration) -> %TODO: remove Iteration
  timer:sleep(10), %%TODO: set The refresh time between sends
  spawn_link(fun()-> getQuarterDataAndSend(1, ServerInfo, Iteration) end),
  spawn_link(fun()-> getQuarterDataAndSend(2, ServerInfo, Iteration) end),
  spawn_link(fun()-> getQuarterDataAndSend(3, ServerInfo, Iteration) end),
  spawn_link(fun()-> getQuarterDataAndSend(4, ServerInfo, Iteration) end),
  sendingPacketsController(ServerInfo, Iteration).

getQuarterDataAndSend(Quarter, _ServerInfo, Iteration) -> %%TODO: set the connection to server when is possible
  case Quarter of
    1 ->
      io:format ("1", []),
      M=[{Iteration, Iteration, 0.5}, {50, Iteration, 0.5}, {100, Iteration, 0.5}, {120, Iteration, 0.5}, {130, Iteration, 0.5},{150, Iteration, 0.5}, {160, Iteration, 0.5}, {170, Iteration, 0.5}, {190, Iteration, 0.5},{200, Iteration, 0.5}],
      AM=[{Iteration, Iteration+20, 0.5},{Iteration, Iteration, 0.5}, {50, Iteration, 0.5}, {20, Iteration, 0.5}, {20, Iteration, 0.5}, {12, Iteration, 0.5},{20, Iteration, 0.5}, {34, Iteration, 0.5}, {50, Iteration, 0.5}, {30, Iteration, 0.5},{12, Iteration, 0.5},{Iteration+200, Iteration, 0.5},
        {110, Iteration, 0.5}, {130, Iteration, 0.5}, {40, Iteration, 0.5}, {130, Iteration+50, 0.5},{150, Iteration, 0.5}, {160, Iteration+80, 0.5}, {170, Iteration+90, 0.5}, {190, Iteration+95, 0.5},{200, Iteration+85, 0.5}],
      E=[{Iteration, Iteration+200}],
      CitiesData = [{budapest,alive}, {jerusalem, alive}, {london, alive}, {moscow, alive}, {newYork, alive}, {paris, destroyed}, {rome, destroyed},{stockholm, destroyed}, {sydney, alive}, {washington, alive}],
      LaunchersData= [{launcher1, alive}, {launcher2, alive}, {launcher3, destroyed}, {launcher4, alive}],
      RadarsData= [{radar1, alive}, {radar2, destroyed}, {radar3, destroyed}, {radar4, alive}];
    2 ->
      io:format ("2", []),
      M=[{Iteration+50, Iteration+50, 0.5}, {Iteration+50, 700-Iteration, 0.5},{Iteration+50, 640-Iteration, 0.5}, {Iteration+50, 640-Iteration, 0.5}, {Iteration+50, 600-Iteration, 0.5}, {Iteration+50, 500-Iteration, 0.5}],
      AM=[{Iteration, Iteration+50, 0.5}],
      E=[{Iteration, Iteration+230}],
      CitiesData = [],
      LaunchersData= [],
      RadarsData= [];
    3 ->
      io:format ("3", []),
      M=[{Iteration+70, Iteration+70, 0.5}, {Iteration+70, 70, 0.5}, {Iteration+70, 100, 0.5}, {Iteration+70, 120, 0.5}, {Iteration+70, 150, 0.5}, {Iteration+70, 170, 0.5}, {Iteration+70,190, 0.5}, {70, Iteration+70, 0.5}, {120, Iteration+70, 0.5}],
      AM=[{Iteration, Iteration+70, 0.5}],
      E=[{Iteration, Iteration+250}],
      CitiesData = [],
      LaunchersData= [],
      RadarsData= [];
    4 ->
      io:format ("4", []),
      M=[{Iteration+90, Iteration+90, 0.5}, {Iteration+150, Iteration+70, 0.5}, {Iteration+150, 70, 0.5}, {Iteration+150, 100, 0.5}, {Iteration+150, 120, 0.5}, {Iteration+150, 150, 0.5}, {Iteration+150, 170, 0.5}, {Iteration+150,190, 0.5}, {150, Iteration+70, 0.5}, {150, Iteration+90, 0.5}],
      AM=[{Iteration, Iteration+90, 0.5}],
      E=[{Iteration, Iteration+270}],
      CitiesData = [],
      LaunchersData= [],
      RadarsData= []
  end,
  gen_statem:cast(graphic, {LaunchersData, RadarsData,CitiesData, AM,M, [], E}).


changeSettingsControllerPid(ServerInfo) -> %%TODO: set the connection to server and to graphic when is possible
  changeSettingsControllerPid(ServerInfo).