%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 18:18
%%%-------------------------------------------------------------------
-module(script).
-author("raz").

%% API
-export([script/1, crash_script/1, changeSetting_scripts/7]).

%cities Locations: {{919, 755}, budapest}, {370, 494}, newYork},{{1079, 688}, paris}, {{550, 778}, jerusalem}, {{1078, 574}, moscow},
% {{431, 637}, london}, {{725, 684}, rome},{{925, 646}, stockholm}, {{127, 595}, sydney}, {{483, 425}, washington}
%radars Locations: {{1166, 671}, radar1}, {{753, 596}, radar2}, {{189, 652}, radar3}, {{658, 440}, radar4}
%launcher Locations: {{808, 572}, launcher1}, {{365, 565}, launcher2}, {{1143, 753}, launcher3}, {{48, 708}, launcher4}
%% Node1 - top left, Node 2 - top right, Node 3 - bottom left, Node 4 - bottom right
script(NodeNum) ->
%%  if
%%    NodeNum =:= 1 ->
%%      city:start_link({{919, 755}, budapest}),
%%      city:start_link({{370, 494}, newYork}),
%%      city:start_link({{1079, 688}, paris}),
%%      city:start_link({{550, 778}, jerusalem}),
%%      city:start_link({{1078, 574}, moscow}),
%%      city:start_link({{431, 637}, london}),
%%      city:start_link({{725, 684}, rome}),
%%      city:start_link({{925, 646}, stockholm}),
%%      city:start_link({{127, 595}, sydney});
%%    true -> nope
%%  end,
%%  {Ref0, _, _} = launcher:start_link({{808, 572}, 1200, 1}),
%%  {Ref1, _, _} = launcher:start_link({{365, 565}, 1200, 2}),
%%  radar:start_link({{1166, 671}, 1, [Ref0, Ref1], 1, 1}),
%%  radar:start_link({{753, 596}, 1, [Ref0, Ref1], 1, 2}),
%%  radar:start_link({{189, 652}, 1, [Ref0, Ref1], 1, 3}),
%%  radar:start_link({{658, 440}, 1, [Ref0, Ref1], 1, 4}),
%%  mclock:start_link(1, generate).
  case NodeNum of
    1 -> %% AREA {0,600}/{0,400}
      mclock:start_link(1, generate, {0, 600});
    2 -> %% AREA {600,1200}/{0,400}
      mclock:start_link(1, generate, {600, 1200});
    3 -> %% AREA {0,600}/{400/800}
      city:start_link({{370, 494}, newYork}),
      city:start_link({{550, 778}, jerusalem}),
      city:start_link({{431, 637}, london}),
      city:start_link({{127, 595}, sydney}),
      {RefL, _, _} = launcher:start_link({{365, 565}, 1200, 2}),
      radar:start_link({{189, 652}, 1, [RefL], 1, 3}),
      mclock:start_link(1, no_generate, {0, 600});
    4 -> %% AREA {600,1200}/{400/800}
      city:start_link({{919, 755}, budapest}),
      city:start_link({{1079, 688}, paris}),
      city:start_link({{1078, 574}, moscow}),
      city:start_link({{725, 684}, rome}),
      city:start_link({{925, 646}, stockholm}),
      {RefL, _, _} = launcher:start_link({{808, 572}, 1200, 1}),
      radar:start_link({{1166, 671}, 1, [RefL], 1, 1}),
      radar:start_link({{753, 596}, 1, [RefL], 1, 2}),
      radar:start_link({{658, 440}, 1, [RefL], 1, 4}),
      mclock:start_link(1, no_generate, {600, 1200})
  end.

crash_script(Data) ->
  Missiles = maps:get(missile, Data),
  AntiMissiles = maps:get(antimissile, Data),
  Launchers = maps:get(launcher, Data),
  Radars = maps:get(radar, Data),
  Cities = maps:get(city, Data),
  lists:foreach(fun({Ref, {falling, Velocity, Position, _Angle}}) ->
    mclock:generateMissile(Ref, {0, 0.065}, Velocity, Position) end, Missiles), %%TODO: see how the change {0, 0.065} to be the actual ACCELERATION
  lists:foreach(fun({Ref, {intercepting, Velocity, Position, _Angle}}) ->
    mclock:generateAntiMissile(Ref, Velocity, Position) end, AntiMissiles),
  LauncherRefs = lists:map(fun({Ref, {alive, Position}}) ->
    {RefL, _, _} = launcher:start_link({Position, 1200, Ref}),
    RefL end, Launchers),
  lists:foreach(fun({Ref, {alive, Position}}) ->
    radar:start_link({Position, 1, LauncherRefs, 1, Ref})
                end, Radars),
  lists:foreach(fun({Name, {alive, Position}}) ->
    city:start_link({Position, Name}) end, Cities).

changeSetting_scripts(NodeNum,{missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
  {radarError, RadarErrorSlider}, {radarRange, RadarRangeSlider} ,{radarRefreshDelay, RadarRefreshDelay}) ->
  case NodeNum of
    1 -> %% AREA {0,600}/{0,400}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider});
    2 -> %% AREA {600,1200}/{0,400}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider});
    3 -> %% AREA {0,600}/{400/800}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider});
    4 -> %% AREA {600,1200}/{400/800}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider})
  end.



%%  {Ref0, _, _} = launcher:start_link({{550, 1000}, 1000, make_ref()}),
%%  {Ref1, _, _} = launcher:start_link({{450, 1000}, 1000, make_ref()}),
%%  {Ref2, _, _} = radar:start_link({{950, 1000}, 1, [Ref0, Ref1], 1, make_ref()}),
%%  radar:start_link({{420, 1000}, 1, [Ref0, Ref1], 1, make_ref()}),


