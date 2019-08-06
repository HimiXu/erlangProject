%%%-------------------------------------------------------------------
%%% @author ofir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 9:42 PM
%%%-------------------------------------------------------------------
-module(graphic).
-author("ofir").

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([
  init/1,
  packetsDeliver/3,
  terminate/3,
  code_change/4,
  callback_mode/0
]).
-include_lib("wx/include/wx.hrl").



start_link(ServerInfo) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [ServerInfo], []).


init([ServerInfo]) ->
  wx:new(),
  WxEnv = wx:get_env(),
  Frame = wxFrame:new(wx:null(), -1, "world :D", [{size,{1200, 940}}]),
  Panel  = wxPanel:new(Frame),
  Canvas = wxPanel:new(Panel, [{size, {1200, 800}}]),
  CitiesImages = loadCitiesImages(),
  MissileAndExplosionImages = loadMissileAndExplosionImages(),
  RadarsImages=loadRadarsImages(),
  LaunchersImages=loadLaunchersImages(),
  spawn_link(fun()-> draw_buttons(Panel, WxEnv, ServerInfo) end),
  spawn_link(fun()-> nodesWindow() end),
  wxFrame:show(Frame),
  wxPanel:connect(Canvas, paint, []),
  Background =wxImage:new("include/worldBackground.png"), %background
  ClientDC = wxClientDC:new(Canvas),
  BackgroundBitmap = wxBitmap:new(Background), %convert to bitmap
  wxDC:drawBitmap(ClientDC, BackgroundBitmap, {0,0}),
  wxImage:destroy(Background),
  wxClientDC:destroy(ClientDC),
  Data= #{citiesImages =>CitiesImages, missileAndExplosionImages=> MissileAndExplosionImages,
    radarsImages => RadarsImages, launchersImages => LaunchersImages,
    wxEnv => WxEnv, backgroundBitmap =>BackgroundBitmap, canvas => Canvas, numOfPacketsDelivered => 0},
  {ok, packetsDeliver, Data}.


callback_mode() ->
  state_functions.

packetsDeliver(info, _OldState, Data) ->
  {keep_state, Data};

packetsDeliver(cast, PacketData, Data) ->
%%  io:format("DATA==============================================~p~n",
%%    [PacketData]),
  NumOfPacketsDelivered=maps:get(numOfPacketsDelivered, Data),
  NewData=
    if
      NumOfPacketsDelivered =:= 0 -> (preparationForPacketDeliver (Data));
      true -> Data#{ numOfPacketsDelivered := NumOfPacketsDelivered +1}
    end,
  draw(PacketData,NewData),
  NewNumOfPacketsDelivered=maps:get(numOfPacketsDelivered, NewData),
    if
      NewNumOfPacketsDelivered =:= 4 ->
        finishDrawing(NewData),
        {keep_state, NewData#{numOfPacketsDelivered := 0}};
      true ->
        {keep_state, NewData}
    end.



terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%returns map of bitmaps of the cities images
loadCitiesImages()->
  #{
    budapest    => {wxBitmap:new(wxImage:new("include/budapestE.png")),847, 687},
    jerusalem   => {wxBitmap:new(wxImage:new("include/jerusalemE.png")), 478, 726},
    london      => {wxBitmap:new(wxImage:new("include/londonE.png")), 398, 590},
    moscow      => {wxBitmap:new(wxImage:new("include/moscowE.png")), 1023, 501},
    newYork     => {wxBitmap:new(wxImage:new("include/newYorkE .png")), 328, 437},
    paris       => {wxBitmap:new(wxImage:new("include/ParisE.png")), 1035, 608},
    rome        => {wxBitmap:new(wxImage:new("include/romeE.png")), 676, 641},
    stockholm   => {wxBitmap:new(wxImage:new("include/stockholmE.png")), 870, 592},
    sydney      => {wxBitmap:new(wxImage:new("include/sydneyE.png")), 74, 536},
    washington  => {wxBitmap:new(wxImage:new("include/WashingtonE.png")), 423, 371}
  }.

%%returns map of bitmaps of the radars images
loadRadarsImages()->
  #{
    1    => {wxBitmap:new(wxImage:new("include/radar1.png")), 1138, 625},
    2    => {wxBitmap:new(wxImage:new("include/radar2.png")), 736, 571},
    3    => {wxBitmap:new(wxImage:new("include/radar3.png")), 169, 629},
    4    => {wxBitmap:new(wxImage:new("include/radar4.png")), 643, 410}
  }.

%%returns map of bitmaps of the launchers images
loadLaunchersImages()->
  #{
    1 => {wxBitmap:new(wxImage:new("include/launcher1.png")), 777, 544},
    2 => {wxBitmap:new(wxImage:new("include/launcher2.png")), 326, 534},
    3 => {wxBitmap:new(wxImage:new("include/launcher3.png")), 1105, 728},
    4 => {wxBitmap:new(wxImage:new("include/launcher4.png")), 16, 680}
  }.

%%returns bitmaps of the Missile And Explosion images
loadMissileAndExplosionImages()->
  {
    wxImage:new("include/interceptor.png"),
    wxImage:new("include/enemyMissile.png"),
    wxBitmap:new(wxImage:new("include/interception.png")),
    wxBitmap:new(wxImage:new("include/explode.png"))
  }.


preparationForPacketDeliver (Data) ->
  BackgroundBitmap=maps:get(backgroundBitmap, Data),
  Canvas=maps:get(canvas, Data),
  ClientDC = wxClientDC:new(Canvas),
  BufferDC = wxBufferedDC:new(ClientDC),
  wxDC:drawBitmap(BufferDC, BackgroundBitmap, {0,0}),
  Data#{ clientDC => ClientDC ,bufferDC => BufferDC, numOfPacketsDelivered => 1}.


%draw Cities function
drawCitiesOrRadarsOrLaunchers(_WxEnv,_BufferDC, _Images, []) ->
  finishedDrawingElements;

drawCitiesOrRadarsOrLaunchers(WxEnv,BufferDC, Images, [ElementData|OtherElementsData]) ->
  %wx:set_env(WxEnv),
  case element(2, ElementData) of
    destroyed ->
      drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, Images, OtherElementsData);
    alive ->
      Object=maps:get(element(1, ElementData), Images),
      wxDC:drawBitmap(BufferDC, element(1, Object), {element(2, Object),element(3, Object)}),
      drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, Images, OtherElementsData)
  end.


%draw AntiMissiles function
drawAntiMissiles (_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingAntiMissiles;

drawAntiMissiles (WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y, Angle}|OtherAntiMissiles]) ->
  %wx:set_env(WxEnv),
  Image= wxImage:rotate (element(1,MissileAndExplosionImages), Angle, {15, 24}), %%rotate(This, Angle, Centre_of_rotation) %%TODO: check the angle unit, make sure you choose the right center
  Bitmap= wxBitmap:new(Image),
  wxDC:drawBitmap(BufferDC, Bitmap, {X,Y}), %%TODO: check maybe it is the opposite
  wxBitmap:destroy(Bitmap),
  wxImage:destroy(Image),
  drawAntiMissiles (WxEnv, BufferDC, MissileAndExplosionImages, OtherAntiMissiles).


%draw Missiles function
drawMissiles (_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingMissiles;

drawMissiles (WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y, Angle}|OtherMissiles]) ->
  %wx:set_env(WxEnv),
  Image= wxImage:rotate (element(2,MissileAndExplosionImages), Angle, {13, 21}), %%rotate(This, Angle, Centre_of_rotation) %%TODO: check the angle unit, make sure you choose the right center
  Bitmap= wxBitmap:new(Image),
  wxDC:drawBitmap(BufferDC, Bitmap, {X,Y}), %%TODO: check maybe it is the opposite
  wxBitmap:destroy(Bitmap),
  wxImage:destroy(Image),
  drawMissiles (WxEnv, BufferDC, MissileAndExplosionImages, OtherMissiles).


%draw Interceptions function
drawInterceptions(_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingExplosions;

drawInterceptions(WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y}|OtherInterceptions]) ->
  %wx:set_env(WxEnv),
  wxDC:drawBitmap(BufferDC, element(3,MissileAndExplosionImages), {X,Y}),
  drawInterceptions(WxEnv, BufferDC, MissileAndExplosionImages, OtherInterceptions).


%draw Explosions function
drawExplosions(_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingExplosions;

drawExplosions(WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y}|OtherExplosions]) ->
  %wx:set_env(WxEnv),
  wxDC:drawBitmap(BufferDC, element(4,MissileAndExplosionImages), {X,Y}),
  drawExplosions(WxEnv, BufferDC, MissileAndExplosionImages, OtherExplosions).


draw(PacketData, Data) ->
  CitiesImages=maps:get(citiesImages, Data),
  MissileAndExplosionImages=maps:get(missileAndExplosionImages, Data),
  RadarsImages=maps:get(radarsImages, Data),
  LaunchersImages=maps:get(launchersImages, Data),
  WxEnv=maps:get(wxEnv, Data),
  BufferDC=maps:get(bufferDC, Data),
  drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, LaunchersImages, element(1, PacketData)),
  drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, RadarsImages, element(2, PacketData)),
  drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, CitiesImages, element(3, PacketData)),
  drawAntiMissiles (WxEnv, BufferDC, MissileAndExplosionImages, element(4, PacketData)),
  drawMissiles (WxEnv, BufferDC, MissileAndExplosionImages, element(5, PacketData)),
  drawInterceptions(WxEnv, BufferDC, MissileAndExplosionImages, element(6, PacketData)),
  drawExplosions(WxEnv, BufferDC, MissileAndExplosionImages, element(7, PacketData)).

finishDrawing(Data) ->
  BufferDC=maps:get(bufferDC, Data),
  ClientDC=maps:get(clientDC, Data),
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC).



draw_buttons(Panel, WxEnv, ServerInfo)->
  wx:set_env(WxEnv),
  MissilesSpeedSlider = wxSlider:new (Panel, 1, 5, 1, 10, [{pos, {200, 800}}, {size, {100, 20}}]),
  MissilesQuantitySlider = wxSlider:new (Panel, 2, 5, 0, 10, [{pos, {200, 840}}, {size, {100, 20}}]),
  GravitySlider = wxSlider:new (Panel, 3, 5, 0, 10, [{pos, {200, 880}}, {size, {100, 20}}]),
  RadarErrorSlider = wxSlider:new (Panel, 4, 5, 1, 10, [{pos, {500, 800}}, {size, {100, 20}}]),
  RadarRangeSlider = wxSlider:new (Panel, 5, 5, 1, 10, [{pos, {500, 840}}, {size, {100, 20}}]),
  RadarRefreshDelay = wxSlider:new (Panel, 6, 5, 1, 10, [{pos, {500, 880}}, {size, {100, 20}}]),
  wxButton:new(Panel, 7, [{label, "Apply Settings"},{pos, {840,850}},{size, {170,50}}]),
  wxStaticText:new(Panel, 8, "MissilesSpeed:", [{pos, {50, 800}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 9, "Missiles Quantity:", [{pos, {50, 840}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 10, "Gravity:", [{pos, {50, 880}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 11, "Radar Error:", [{pos, {350, 800}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 12, "Radar Range:", [{pos, {350, 840}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 13, "Radar Refresh Delay:", [{pos, {350, 880}}, {size, {200, 20}}] ),
  ModeRadioBox=wxRadioBox:new(Panel, 14, "Mode", {650,800}, {170, 70}, ["Normal Mode", "God Mode"]),
  wxButton:new(Panel, 15, [{label, "Restart"},{pos, {1020,850}},{size, {170,50}}]),
  wxPanel:connect(Panel, command_button_clicked),
  loop(ServerInfo, MissilesSpeedSlider, MissilesQuantitySlider, GravitySlider, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, ModeRadioBox).

loop(ServerInfo, MissilesSpeedSlider, MissilesQuantitySlider, GravitySlider, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, ModeRadioBox) ->
  Settings = receive
               #wx{id = 7, event=#wxCommand{type = command_button_clicked}} ->
                 {wxRadioBox:getSelection(ModeRadioBox), {missilesSpeed, wxSlider:getValue(MissilesSpeedSlider)}, {missilesQuantity, wxSlider:getValue(MissilesQuantitySlider)}, {gravity, wxSlider:getValue(GravitySlider)},
                   {radarError, wxSlider:getValue(RadarErrorSlider)}, {radarRange, wxSlider:getValue(RadarRangeSlider)} ,{radarRefreshDelay, wxSlider:getValue(RadarRefreshDelay)}};
               #wx{id = 15, event=#wxCommand{type = command_button_clicked}} -> {restart}
             end,
  ServerInfo ! Settings,
  loop(ServerInfo, MissilesSpeedSlider, MissilesQuantitySlider, GravitySlider, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay, ModeRadioBox).


nodesWindow() ->
  register(nodeUpdatePid, self()),
  wx:new(),
  Frame = wxFrame:new(wx:null(), -1, "Nodes", [{size,{405, 207}}]),
  Panel  = wxPanel:new(Frame),
  Canvas = wxPanel:new(Panel, [{size, {405, 207}}]),
  wxPanel:connect(Canvas, paint, []),
  wxFrame:show(Frame),
  ClientDC = wxClientDC:new(Canvas),
  BufferDC = wxBufferedDC:new(ClientDC),
  wxDC:drawBitmap(BufferDC, wxBitmap:new(wxImage:new("include/NodesBackground.png")) , {0,0}),
  T1 = wxStaticText:new(Canvas, 101, "Not set", [{pos, {15, 40}}, {size, {175, 20}}] ),
  T2 = wxStaticText:new(Canvas, 103, "Not set", [{pos, {215, 40}}, {size, {175, 20}}] ),
  T3 = wxStaticText:new(Canvas, 102, "Not set", [{pos, {15, 110}}, {size, {175, 20}}] ),
  T4 = wxStaticText:new(Canvas, 104, "Not set", [{pos, {215, 110}}, {size, {175, 20}}] ),
  wxBufferedDC:destroy(BufferDC),
  wxClientDC:destroy(ClientDC),
  drawNodesLoop(Canvas, T1, T2, T3, T4).



drawNodesLoop(Canvas, T1, T2, T3, T4) ->
  receive
    [{a, Node1}, {b, Node2}, {c, Node3}, {d, Node4}] ->
      ClientDC = wxClientDC:new(Canvas),
      BufferDC = wxBufferedDC:new(ClientDC),
      wxDC:drawBitmap(BufferDC, wxBitmap:new(wxImage:new("include/NodesBackground.png")) , {0,0}),
      wxStaticText:setLabel(T1, atom_to_list(Node1)),
      wxStaticText:setLabel(T2, atom_to_list(Node2)),
      wxStaticText:setLabel(T3, atom_to_list(Node3)),
      wxStaticText:setLabel(T4, atom_to_list(Node4)),
      wxBufferedDC:destroy(BufferDC),
      wxClientDC:destroy(ClientDC)
  end,
  drawNodesLoop(Canvas, T1, T2, T3, T4).
