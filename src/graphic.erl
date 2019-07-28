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
-export([start_link/0]).

%% gen_statem callbacks
-export([
  init/1,
  packetsDeliver/3,
  terminate/3,
  code_change/4,
  callback_mode/0
]).
-include_lib("wx/include/wx.hrl").



start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  wx:new(),
  WxEnv = wx:get_env(),
  Frame = wxFrame:new(wx:null(), -1, "world :D", [{size,{1200, 850}}]),
  Panel  = wxPanel:new(Frame),
  Canvas = wxPanel:new(Panel, [{size, {1200, 800}}]),
  CitiesImages = loadCitiesImages(),
  MissileAndExplosionImages = loadMissileAndExplosionImages(),
  RadarsImages=loadRadarsImages(),
  LaunchersImages=loadLaunchersImages(),
  %%spawn_link(fun()->draw_buttuns(Panel,WxEnv,PlayersPNGs) end), %
  wxFrame:show(Frame),
  wxPanel:connect(Canvas, paint, []),
  Background =wxImage:new("include/worldBackground.png"), %background
  ClientDC = wxClientDC:new(Canvas),
  BackgroundBitmap = wxBitmap:new(Background), %convert to bitmap
  wxDC:drawBitmap(ClientDC, BackgroundBitmap, {0,0}),
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
  io:format ("Key0", []),
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
    budapest    => wxBitmap:new(wxImage:new("include/budapestE.png")),
    jerusalem   => wxBitmap:new(wxImage:new("include/jerusalemE.png")),
    london      => wxBitmap:new(wxImage:new("include/londonE.png")),
    moscow      => wxBitmap:new(wxImage:new("include/moscowE.png")),
    newYork     => wxBitmap:new(wxImage:new("include/newYorkE .png")),
    paris       => wxBitmap:new(wxImage:new("include/ParisE.png")),
    rome        => wxBitmap:new(wxImage:new("include/romeE.png")),
    stockholm   => wxBitmap:new(wxImage:new("include/stockholmE.png")),
    sydney      => wxBitmap:new(wxImage:new("include/sydneyE.png")),
    washington  => wxBitmap:new(wxImage:new("include/WashingtonE.png"))
  }.

%%returns map of bitmaps of the radars images
loadRadarsImages()->
  #{
    radar1    => wxBitmap:new(wxImage:new("include/radar1.png")),
    radar2    => wxBitmap:new(wxImage:new("include/radar2.png")),
    radar3    => wxBitmap:new(wxImage:new("include/radar3.png")),
    radar4    => wxBitmap:new(wxImage:new("include/radar4.png"))
  }.

%%returns map of bitmaps of the launchers images
loadLaunchersImages()->
  #{
    launcher1 => wxBitmap:new(wxImage:new("include/launcher1.png")),
    launcher2 => wxBitmap:new(wxImage:new("include/launcher2.png")),
    launcher3 => wxBitmap:new(wxImage:new("include/launcher3.png")),
    launcher4 => wxBitmap:new(wxImage:new("include/launcher4.png"))
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
  wx:set_env(WxEnv),
  case element(2, ElementData) of
    destroyed ->
      drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, Images, OtherElementsData);
    alive ->
      wxDC:drawBitmap(BufferDC, maps:get(element(1, ElementData),Images), {0,0}),
      drawCitiesOrRadarsOrLaunchers (WxEnv, BufferDC, Images, OtherElementsData)
  end.

%draw AntiMissiles function
drawAntiMissiles (_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingAntiMissiles;

drawAntiMissiles (WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y, Angle}|OtherAntiMissiles]) ->
  wx:set_env(WxEnv),
  Image= wxImage:rotate (element(1,MissileAndExplosionImages), Angle, {31, 48}), %%rotate(This, Angle, Centre_of_rotation) %%TODO: check the angle unit, make sure you choose the right center
  Bitmap= wxBitmap:new(Image),
  wxDC:drawBitmap(BufferDC, Bitmap, {X,Y}), %%TODO: check maybe it is the opposite
  wxBitmap:destroy(Bitmap),
  drawAntiMissiles (WxEnv, BufferDC, MissileAndExplosionImages, OtherAntiMissiles).


%draw Missiles function
drawMissiles (_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingMissiles;

drawMissiles (WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y, Angle}|OtherMissiles]) ->
  wx:set_env(WxEnv),
  Image= wxImage:rotate (element(2,MissileAndExplosionImages), Angle, {26, 42}), %%rotate(This, Angle, Centre_of_rotation) %%TODO: check the angle unit, make sure you choose the right center
  Bitmap= wxBitmap:new(Image),
  wxDC:drawBitmap(BufferDC, Bitmap, {X,Y}), %%TODO: check maybe it is the opposite
  wxBitmap:destroy(Bitmap),
  drawAntiMissiles (WxEnv, BufferDC, MissileAndExplosionImages, OtherMissiles).


%draw Interceptions function
drawInterceptions(_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingExplosions;

drawInterceptions(WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y}|OtherInterceptions]) ->
  wx:set_env(WxEnv),
  wxDC:drawBitmap(BufferDC, element(3,MissileAndExplosionImages), {X,Y}),
  drawInterceptions(WxEnv, BufferDC, MissileAndExplosionImages, OtherInterceptions).


%draw Explosions function
drawExplosions(_WxEnv, _BufferDC, _MissileAndExplosionImages, []) ->
  finishedDrawingExplosions;

drawExplosions(WxEnv, BufferDC, MissileAndExplosionImages, [{X, Y}|OtherExplosions]) ->
  wx:set_env(WxEnv),
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
