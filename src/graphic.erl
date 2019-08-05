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
  spawn_link(fun()-> nodesWindow(WxEnv) end),
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
  wxStaticText:new(Panel, 11, "Radar Error:", [{pos, {350, 840}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 12, "Radar Range:", [{pos, {350, 880}}, {size, {200, 20}}] ),
  wxStaticText:new(Panel, 13, "Radar Refresh Delay:", [{pos, {350, 800}}, {size, {200, 20}}] ),
  wxPanel:connect(Panel, command_button_clicked),
  loop(ServerInfo, MissilesSpeedSlider, MissilesQuantitySlider, GravitySlider, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay).

loop(ServerInfo, MissilesSpeedSlider, MissilesQuantitySlider, GravitySlider, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay) ->
  Settings = receive
               #wx{id = 7, event=#wxCommand{type = command_button_clicked}} ->
                 {{missilesSpeed, wxSlider:getValue(MissilesSpeedSlider)}, {missilesQuantity, wxSlider:getValue(MissilesQuantitySlider)}, {gravity, wxSlider:getValue(GravitySlider)},
                   {radarError, wxSlider:getValue(RadarErrorSlider)}, {radarRange, wxSlider:getValue(RadarRangeSlider)} ,{radarRefreshDelay, wxSlider:getValue(RadarRefreshDelay)}}
             end,
  ServerInfo ! Settings,
  loop(ServerInfo, MissilesSpeedSlider, MissilesQuantitySlider, GravitySlider, RadarErrorSlider, RadarRangeSlider, RadarRefreshDelay).


nodesWindow(WxEnv) ->
  register(nodeUpdatePid, self()),
  wx:set_env(WxEnv),
  Frame = wxFrame:new(wx:null(), -1, "Nodes", [{size,{200, 240}}]),
  Panel  = wxPanel:new(Frame),
  Canvas = wxPanel:new(Panel, [{size, {200, 240}}]),
  wxPanel:connect(Canvas, paint, []),
  wxFrame:show(Frame),
  NodesBitmaps=loadNodesImages(),
  drawNodesLoop(Canvas, NodesBitmaps).



drawNodesLoop(Canvas, NodesBitmaps) ->
  receive
    {Quarter1Node, Quarter2Node, Quarter3Node, Quarter4Node} ->
      ClientDC = wxClientDC:new(Canvas),
      BufferDC = wxBufferedDC:new(ClientDC),
      wxDC:drawBitmap(BufferDC, maps:get(Quarter1Node, NodesBitmaps) , {0,0}),
      wxDC:drawBitmap(BufferDC, maps:get(Quarter2Node, NodesBitmaps) , {100,0}),
      wxDC:drawBitmap(BufferDC, maps:get(Quarter3Node, NodesBitmaps) , {0,100}),
      wxDC:drawBitmap(BufferDC, maps:get(Quarter4Node, NodesBitmaps) , {100,100}),
      wxBufferedDC:destroy(BufferDC),
      wxClientDC:destroy(ClientDC)
  after 20 ->
    ClientDC = wxClientDC:new(Canvas),
    BufferDC = wxBufferedDC:new(ClientDC),
    wxDC:drawBitmap(BufferDC, maps:get(1, NodesBitmaps) , {0,0}),
    wxDC:drawBitmap(BufferDC, maps:get(2, NodesBitmaps) , {100,0}),
    wxDC:drawBitmap(BufferDC, maps:get(3, NodesBitmaps) , {0,100}),
    wxDC:drawBitmap(BufferDC, maps:get(4, NodesBitmaps) , {100,100}),
    wxBufferedDC:destroy(BufferDC),
    wxClientDC:destroy(ClientDC)
  end,
  drawNodesLoop(Canvas, NodesBitmaps).



loadNodesImages()->
  #{
    1    =>  wxBitmap:new(wxImage:new("include/node1.png")),
    2    =>  wxBitmap:new(wxImage:new("include/node2.png")),
    3    =>  wxBitmap:new(wxImage:new("include/node3.png")),
    4    =>  wxBitmap:new(wxImage:new("include/node4.png"))
  }.