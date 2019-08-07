-module(script).
-author("Ofir & Raz").

%% API
-export([script/1, changeSettings_script/7, recovery/2, startCities/1]).

%cities Locations: {{919, 755}, budapest}, {370, 494}, newYork},{{1079, 688}, paris}, {{550, 778}, jerusalem}, {{1078, 574}, moscow},
% {{431, 637}, london}, {{725, 684}, rome},{{925, 646}, stockholm}, {{127, 595}, sydney}, {{483, 425}, washington}
%radars Locations: {{1166, 671}, radar1}, {{753, 596}, radar2}, {{189, 652}, radar3}, {{658, 440}, radar4}
%launcher Locations: {{808, 572}, launcher1}, {{365, 565}, launcher2}, {{1143, 753}, launcher3}, {{48, 708}, launcher4}
%% Node1 - top left, Node 2 - top right, Node 3 - bottom left, Node 4 - bottom right

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%script for start of simulation%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
script(Region) ->
  case Region of
    a -> %% AREA {0,600}/{0,400}
      mclock:start_link(1, 1);
    b -> %% AREA {600,1200}/{0,400}
      mclock:start_link(1, 2);
    c -> %% AREA {0,600}/{400/800}
      city:start_link({{483, 425}, washington}),
      city:start_link({{370, 494}, newYork}),
      city:start_link({{550, 778}, jerusalem}),
      city:start_link({{431, 637}, london}),
      city:start_link({{127, 595}, sydney}),
      launcher:start_link({{365, 565}, 1200, 2}),
      launcher:start_link({{48, 708}, 1200, 4}),
      radar:start_link({{189, 652}, 1, [1, 2, 3, 4], 1, 3}),
      mclock:start_link(1, 0);
    d -> %% AREA {600,1200}/{400/800}
      city:start_link({{919, 755}, budapest}),
      city:start_link({{1079, 688}, paris}),
      city:start_link({{1078, 574}, moscow}),
      city:start_link({{725, 684}, rome}),
      city:start_link({{925, 646}, stockholm}),
      launcher:start_link({{808, 572}, 1200, 1}),
      launcher:start_link({{1143, 753}, 1200, 3}),
      radar:start_link({{1166, 671}, 1, [1, 2, 3, 4], 1, 1}),
      radar:start_link({{753, 596}, 1, [1, 2, 3, 4], 1, 2}),
      radar:start_link({{658, 440}, 1, [1, 2, 3, 4], 1, 4}),
      mclock:start_link(1, 0)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%script for start of cities%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
startCities(Region) ->
  case Region of
    c -> %% AREA {0,600}/{400/800}
      city:start_link({{483, 425}, washington}),
      city:start_link({{370, 494}, newYork}),
      city:start_link({{550, 778}, jerusalem}),
      city:start_link({{431, 637}, london}),
      city:start_link({{127, 595}, sydney});
    d -> %% AREA {600,1200}/{400/800}
      city:start_link({{919, 755}, budapest}),
      city:start_link({{1079, 688}, paris}),
      city:start_link({{1078, 574}, moscow}),
      city:start_link({{725, 684}, rome}),
      city:start_link({{925, 646}, stockholm});
    _Other ->
      none
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Recovery script- handle recovery of Node%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recovery({Launchers, Radars, Cities, AntiMissiles, Missiles}, Region) ->
  lists:foreach(fun({X, Y, _Angle, Velocity, Ref}) ->
    mclock:generateMissile(Ref, {0, 0.065}, Velocity, {X, Y}) end, Missiles),
  lists:foreach(fun({X, Y, _Angle, Velocity, Ref}) ->
    mclock:generateAntiMissile(Ref, Velocity, {X, Y}) end, AntiMissiles),
  lists:map(fun({Ref, _Status, Position}) ->
    launcher:start_link({Position, 1200, Ref}),
    Ref end, Launchers),
  lists:foreach(fun({Ref, _Status, Position}) ->
    radar:start_link({Position, 1, [1, 2, 3, 4], 1, Ref})
                end, Radars),
  lists:foreach(fun({Name, Status, Position}) ->
    if Status =:= alive ->
      city:start_link({Position, Name});
      true -> city:start_link({Position, Name}),
        city:hit(Name) end end, Cities),
  mclock:setMod(Region).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%The script for changing the setting of the simulation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
changeSettings_script(Region, {missilesSpeed, MissilesSpeedSlider}, {missilesQuantity, MissilesQuantitySlider}, {gravity, GravitySlider},
    {radarError, RadarErrorSlider}, {radarRange, RadarRangeSlider}, {radarRefreshDelay, RadarRefreshDelay}) ->
  case Region of
    a -> %% AREA {0,600}/{0,400}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider});
    b -> %% AREA {600,1200}/{0,400}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider});
    c -> %% AREA {0,600}/{400/800}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider}),
      gen_statem:cast(list_to_atom(lists:append("radar", [3])), {settingUpdate, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider});
    d -> %% AREA {600,1200}/{400/800}
      gen_statem:cast(mclock, {settingUpdate, MissilesQuantitySlider, MissilesSpeedSlider, GravitySlider}),
      gen_statem:cast(list_to_atom(lists:append("radar", [1])), {settingUpdate, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider}),
      gen_statem:cast(list_to_atom(lists:append("radar", [2])), {settingUpdate, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider}),
      gen_statem:cast(list_to_atom(lists:append("radar", [4])), {settingUpdate, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, GravitySlider})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



