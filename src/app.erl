%%%-------------------------------------------------------------------
%%% @author ofir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2019 10:04 PM
%%%-------------------------------------------------------------------
-module(app).
-author("ofir").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

start(normal, [Node1, Node2, Node3, Node4]) ->
  	rpc:call(Node1, node_server,start_link,[1]),
  	rpc:call(Node2, node_server,start_link,[2]),
  	rpc:call(Node3, node_server,start_link,[3]),
  	rpc:call(Node4, node_server,start_link,[4]),
  spawn_link(fun()-> graphic:init([graphicConnectionPID]) end),
  spawn_link(fun()-> graphicConnection:init() end).


stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
