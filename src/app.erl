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
  spawn_link(fun() -> graphic:start_link(graphicConnectionPID) end),
  spawn_link(fun() ->
    rpc:call(Node1, node_server, start_link, [{Node1,Node2,Node3,Node4,a}]) end),
  spawn_link(fun() ->
    rpc:call(Node2, node_server, start_link, [{Node1,Node2,Node3,Node4,b}]) end),
  spawn_link(fun() ->
    rpc:call(Node3, node_server, start_link, [{Node1,Node2,Node3,Node4,c}]) end),
  spawn_link(fun() ->
    rpc:call(Node4, node_server, start_link, [{Node1,Node2,Node3,Node4,d}]) end),
  spawn_link(fun() -> graphicConnection:init([Node1, Node2, Node3, Node4]) end),
  backup_servers:start_link(Node1, Node2, Node3, Node4).


stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
