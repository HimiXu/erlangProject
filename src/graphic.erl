-module(graphic).
-behevior(gen_fsm).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

test() ->
	Pid= spawn_link(fun()->init([]) end),
	CitiesData = [alive, alive, alive, alive, alive, destroyed, destroyed, destroyed, alive, alive],
	loopTest(Pid, 0, CitiesData).

loopTest(Pid, Iteration, CitiesData) ->
	Pid ! frameDateStart,
	Pid ! {citiesData, CitiesData},
	Pid ! {missaleData, {interceptor, Iteration, Iteration, 30}},
	Pid ! {missaleData, {enemyMissile,Iteration+70, Iteration+70, 70}},
	Pid ! {explosionsData, {explode, Iteration+120, Iteration+120}},
	Pid ! frameDataEnd,
	timer:sleep(1000),
	loopTest(Pid, Iteration+1, CitiesData).

start_link()->
	gen_fsm:start_link(?MODULE,[],[]).

init([])->
	wx:new(),
	WxEnv = wx:get_env(),
	Frame = wxFrame:new(wx:null(), -1, "world :D", [{size,{1200, 850}}]),
	Panel  = wxPanel:new(Frame),
	Canvas = wxPanel:new(Panel, [{size, {1200, 800}}]),
	CitiesImages = loadCitiesImages(),
	MissileAndExplosionImages = loadMissileAndExplosionImages(),
	%%spawn_link(fun()->draw_buttuns(Panel,WxEnv,PlayersPNGs) end), %
	wxFrame:show(Frame),
	wxPanel:connect(Canvas, paint, []),
	Background =wxImage:new("include/worldBackground.png"), %background
	ClientDC = wxClientDC:new(Canvas),
	BackgroundBitmap = wxBitmap:new(Background), %convert to bitmap
	wxDC:drawBitmap(ClientDC, BackgroundBitmap, {0,0}),
	wxClientDC:destroy(ClientDC),
	%%{ok,appear,[Background,Canvas,0],0}.
	frameLoop(WxEnv,BackgroundBitmap, Canvas, CitiesImages, MissileAndExplosionImages). %%TODO maybe use data structure instead of delivering the images through function call



%in this function we update the graphics
frameLoop (WxEnv, BackgroundBitmap, Canvas, CitiesImages, MissileAndExplosionImages) ->
	ClientDC = wxClientDC:new(Canvas),
	BufferDC = wxBufferedDC:new(ClientDC), %%TODO: understand the function of this buffer
	wxDC:drawBitmap(BufferDC, BackgroundBitmap, {0,0}),
  receive
		frameDateStart ->
			io:format("key",[]),
			updateDataLoop(WxEnv,BufferDC, CitiesImages, MissileAndExplosionImages)
	end,
	wxBufferedDC:destroy(BufferDC),
	wxClientDC:destroy(ClientDC),
	frameLoop(WxEnv, BackgroundBitmap, Canvas, CitiesImages, MissileAndExplosionImages).

updateDataLoop(WxEnv, BufferDC, CitiesImages, MissileAndExplosionImages) ->
	receive
		{citiesData, CitiesData} ->
			spawn_link(fun()->drawCities(WxEnv, BufferDC, CitiesImages, CitiesData, 10) end), %%TODO: check the function of this. maybe link is not needed.
			%%drawCities (BufferDC, CitiesImages, CitiesData, 10),
			updateDataLoop (WxEnv, BufferDC, CitiesImages, MissileAndExplosionImages);
		{missaleData, MissileLocationData} ->
			spawn_link(fun()->drawMissiles (WxEnv, BufferDC, MissileAndExplosionImages, MissileLocationData) end),
			%%drawMissiles (BufferDC, MissileAndExplosionImages, MissileLocationData),
			updateDataLoop (WxEnv, BufferDC, CitiesImages, MissileAndExplosionImages);
		{explosionsData,ExplosionLocationData} ->
			spawn_link(fun()->drawExplosions(WxEnv, BufferDC, MissileAndExplosionImages, ExplosionLocationData) end),
			%%drawExplosions(BufferDC, MissileAndExplosionImages, ExplosionLocationData),
			updateDataLoop (WxEnv, BufferDC, CitiesImages, MissileAndExplosionImages);
		frameDateEnd -> frameDataEnd %%TODO: maybe we should ensure that all the processes finished their work their work.
	end.



%draw Cities function
drawCities (_WxEnv,_BufferDC, _CitiesImages, _CitiesData, 0) ->
	finishedDrawingCities;

drawCities (WxEnv,BufferDC, CitiesImages, [CityDate|OtherCitiesDate], NumberOfCity) ->
	wx:set_env(WxEnv),
	case CityDate of
		destroyed ->
			drawCities (WxEnv, BufferDC, CitiesImages, OtherCitiesDate, NumberOfCity-1);
		alive ->
			wxDC:drawBitmap(BufferDC, element(NumberOfCity,CitiesImages), {0,0}),
			drawCities (WxEnv, BufferDC, CitiesImages, OtherCitiesDate, NumberOfCity-1)
	end.

%draw Missiles function
drawMissiles (WxEnv, BufferDC, MissileAndExplosionImages, {MissileType, X, Y, Angle}) ->
	wx:set_env(WxEnv),
	case MissileType of
		interceptor ->
			Image= wxImage:rotate (element(1,MissileAndExplosionImages), Angle, {31, 48}); %%rotate(This, Angle, Centre_of_rotation) %%TODO: check the angle unit, make sure you choose the right center
		enemyMissile ->
			Image= wxImage:rotate (element(2,MissileAndExplosionImages), Angle, {31, 48})
	end,
	Bitmap= wxBitmap:new(Image),
	wxDC:drawBitmap(BufferDC, Bitmap, {X,Y}), %%TODO: check maybe it is the opposite
	wxBitmap:destroy(Bitmap).

%draw Explosions function
drawExplosions(WxEnv, BufferDC, MissileAndExplosionImages, {ExplosionType, X, Y}) ->
	wx:set_env(WxEnv),
	case ExplosionType of
		interception ->
			wxDC:drawBitmap(BufferDC, element(3,MissileAndExplosionImages), {X, Y});
		explode ->
	wxDC:drawBitmap(BufferDC, element(4,MissileAndExplosionImages), {400,400})
	end.

%%returns bitmaps of the cities images
loadCitiesImages()->
	{
		wxBitmap:new(wxImage:new("include/budapestE.png")),
		wxBitmap:new(wxImage:new("include/jerusalemE.png")),
		wxBitmap:new(wxImage:new("include/londonE.png")),
		wxBitmap:new(wxImage:new("include/moscowE.png")),
		wxBitmap:new(wxImage:new("include/newYorkE .png")),
		wxBitmap:new(wxImage:new("include/ParisE.png")),
		wxBitmap:new(wxImage:new("include/romeE.png")),
		wxBitmap:new(wxImage:new("include/stockholmE.png")),
		wxBitmap:new(wxImage:new("include/sydneyE.png")),
		wxBitmap:new(wxImage:new("include/WashingtonE.png"))
	}.


%%returns bitmaps of the Missile And Explosion images
loadMissileAndExplosionImages()->
	{
		wxImage:new("include/interceptor.png"),
		wxImage:new("include/enemyMissile.png"),
		wxBitmap:new(wxImage:new("include/interception.png")),
		wxBitmap:new(wxImage:new("include/explode.png"))
	}.