%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2019 18:18
%%%-------------------------------------------------------------------
-module(test).
-author("raz").

%% API
-export([test/0]).

%cities Locations: {{919, 755}, budapest}, {370, 494}, newYork},{{1079, 688}, paris}, {{550, 778}, jerusalem}, {{1078, 574}, moscow},
% {{431, 637}, london}, {{725, 684}, rome},{{925, 646}, stockholm}, {{127, 595}, sydney}, {{483, 425}, washington}
%radars Locations: {{1166, 671}, radar1}, {{753, 596}, radar2}, {{189, 652}, radar3}, {{658, 440}, radar4}
%launcher Locations: {{808, 572}, launcher1}, {{365, 565}, launcher2}, {{1143, 753}, launcher3}, {{48, 708}, launcher4}

test() ->
  node_server:start_link(),
  graphicConnection:init([]),

  city:start_link({{919, 755}, budapest}),
  city:start_link({{370, 494}, newYork}),
  city:start_link({{1079, 688}, paris}),
  city:start_link({{550, 778}, jerusalem}),
  city:start_link({{1078, 574}, moscow}),
  city:start_link({{431, 637}, london}),
  city:start_link({{725, 684}, rome}),
  city:start_link({{925, 646}, stockholm}),
  city:start_link({{127, 595}, sydney}),

%%  {Ref0, _, _} = launcher:start_link({{550, 1000}, 1000, make_ref()}),
%%  {Ref1, _, _} = launcher:start_link({{450, 1000}, 1000, make_ref()}),
%%  {Ref2, _, _} = radar:start_link({{950, 1000}, 1, [Ref0, Ref1], 1, make_ref()}),
%%  radar:start_link({{420, 1000}, 1, [Ref0, Ref1], 1, make_ref()}),

  {Ref0, _, _} = launcher:start_link({{808, 572}, 1200, 1}),
  {Ref1, _, _} = launcher:start_link({{365, 565}, 1200, 2}),
  {Ref2, _, _} = radar:start_link({{1166, 671}, 1, [Ref0, Ref1], 1, 1}),
  radar:start_link({{753, 596}, 1, [Ref0, Ref1], 1, 2}),
  radar:start_link({{189, 652}, 1, [Ref0, Ref1], 1,3 }),
  radar:start_link({{658, 440}, 1, [Ref0, Ref1], 1,4 }),
  mclock:start_link(1),
  GRAVITY = 0.1,
  missile:start_link({{{0, GRAVITY}, {2, 0}, {0, 0}}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [{Ref0, 808, 572}, {Ref1, 365, 565}], [{Ref2, 1166, 671}], 1000}, make_ref()}),
  missile:start_link({{{0, GRAVITY}, {3, 0}, {0, 0}}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [{Ref0, 808, 572}, {Ref1, 365, 565}], [{Ref2, 1166, 671}], 1000}, make_ref()}),
  missile:start_link({{{0, GRAVITY}, {4, 0}, {0, 0}}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [{Ref0, 808, 572}, {Ref1, 365, 565}], [{Ref2, 1166, 671}], 1000}, make_ref()}),
  missile:start_link({{{0, GRAVITY}, {11, 0}, {0, 0}}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [{Ref0, 808, 572}, {Ref1, 365, 565}], [{Ref2, 1166, 671}], 1000}, make_ref()}),
  timer:sleep(1000),
  missile:start_link({{{0, GRAVITY}, {14, 0}, {0, 0}}, {[
    {budapest, 919, 755}, {newYork, 370, 494}, {paris, 1079, 688}, {jerusalem, 550, 778}, {moscow, 1078, 574},
    {london, 431, 637}, {rome, 725, 684}, {stockholm, 925, 646}, {sydney, 127, 595}]
    , [{Ref0, 808, 572}, {Ref1, 365, 565}], [{Ref2, 1166, 671}], 1000}, make_ref()}).
