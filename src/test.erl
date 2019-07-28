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



test() ->
  node_server:start_link(),
  city:start_link({{100, 1000}, berlin}),
  city:start_link({{200, 1000}, newyork}),
  city:start_link({{300, 1000}, paris}),
  city:start_link({{400, 1000}, losangeles}),
  city:start_link({{500, 1000}, singapore}),
  city:start_link({{600, 1000}, london}),
  city:start_link({{700, 1000}, tokyo}),
  city:start_link({{800, 1000}, hongkong}),
  city:start_link({{900, 1000}, telaviv}),
  {Ref0, _, _} = launcher:start_link({{550, 1000}, 1000, make_ref()}),
  {Ref1, _, _} = launcher:start_link({{450, 1000}, 1000, make_ref()}),
  {Ref2, _, _} = radar:start_link({{950, 1000}, 1, [Ref0, Ref1], 1, make_ref()}),
  radar:start_link({{420, 1000}, 1, [Ref0, Ref1], 1, make_ref()}),
  mclock:start_link(1),
  missile:start_link({{{0, 0.8}, {10, 0}, {0, 0}}, {[
    {berlin, 100, 1000}, {newyork, 200, 1000}, {paris, 300, 1000}, {losangeles, 400, 1000}, {singapore, 500, 1000},
    {london, 600, 1000}, {tokyo, 700, 1000}, {hongkong, 800, 1000}, {telaviv, 900, 1000}]
    , [{Ref0, 550, 1000}, {Ref1, 450, 1000}], [{Ref2, 650, 1000}], 1000}, make_ref()}),
  missile:start_link({{{0, 0.8}, {9, 0}, {0, 0}}, {[
    {berlin, 100, 1000}, {newyork, 200, 1000}, {paris, 300, 1000}, {losangeles, 400, 1000}, {singapore, 500, 1000},
    {london, 600, 1000}, {tokyo, 700, 1000}, {hongkong, 800, 1000}, {telaviv, 900, 1000}]
    , [{Ref0, 550, 1000}, {Ref1, 450, 1000}], [{Ref2, 650, 1000}], 1000}, make_ref()}),
  missile:start_link({{{0, 0.8}, {18, 0}, {0, 0}}, {[
    {berlin, 100, 1000}, {newyork, 200, 1000}, {paris, 300, 1000}, {losangeles, 400, 1000}, {singapore, 500, 1000},
    {london, 600, 1000}, {tokyo, 700, 1000}, {hongkong, 800, 1000}, {telaviv, 900, 1000}]
    , [{Ref0, 550, 1000}, {Ref1, 450, 1000}], [{Ref2, 650, 1000}], 1000}, make_ref()}),
  missile:start_link({{{0, 0.8}, {11, 0}, {0, 0}}, {[
    {berlin, 100, 1000}, {newyork, 200, 1000}, {paris, 300, 1000}, {losangeles, 400, 1000}, {singapore, 500, 1000},
    {london, 600, 1000}, {tokyo, 700, 1000}, {hongkong, 800, 1000}, {telaviv, 900, 1000}]
    , [{Ref0, 550, 1000}, {Ref1, 450, 1000}], [{Ref2, 650, 1000}], 1000}, make_ref()}),
  timer:sleep(1000),
  missile:start_link({{{0, 0.8}, {14, 0}, {0, 0}}, {[
    {berlin, 100, 1000}, {newyork, 200, 1000}, {paris, 300, 1000}, {losangeles, 400, 1000}, {singapore, 500, 1000},
    {london, 600, 1000}, {tokyo, 700, 1000}, {hongkong, 800, 1000}, {telaviv, 900, 1000}]
    , [{Ref0, 550, 1000}, {Ref1, 450, 1000}], [{Ref2, 650, 1000}], 1000}, make_ref()}).
