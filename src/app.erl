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


%%start(normal, [Node1, Node2, Node3, Node4]) ->
%%  %%TODO: check how to call to Application
%%  spawn_link(fun()-> rpc:call(Node1, quarterSupervisor, start_link,[1]) end),%  spawn_link(fun()-> rpc:call(Node1, application,start,[quarter1]) end),
%%  spawn_link(fun()-> rpc:call(Node2, quarterSupervisor, start_link,[2]) end),%  spawn_link(fun()-> rpc:call(Node2, application,start,[quarter2]) end),
%%  spawn_link(fun()-> rpc:call(Node3, quarterSupervisor, start_link,[3]) end),%  spawn_link(fun()-> rpc:call(Node3, application,start,[quarter3]) end),
%%  spawn_link(fun()-> rpc:call(Node4, quarterSupervisor, start_link,[4]) end),%  spawn_link(fun()-> rpc:call(Node4, application,start,[quarter4]) end),
%%  case graphicSupervisor:start_link() of
%%    {ok, Pid} ->
%%      {ok, Pid};
%%    Error ->
%%      Error
%%  end.



start(normal, [Node1, Node2, Node3, Node4]) ->
  spawn_link(fun() -> graphic:start_link(graphicConnectionPID) end),
  spawn_link(fun() ->
    rpc:call(Node1, node_server, start_link, [{[{Node1, 600, 400}, {Node2, 1200, 400}, {Node3, 600, 800}, {Node4, 1200, 800}], {600, 400}, 1}]) end),
  spawn_link(fun() ->
    rpc:call(Node2, node_server, start_link, [{[{Node1, 600, 400}, {Node2, 1200, 400}, {Node3, 600, 800}, {Node4, 1200, 800}], {1200, 400}, 2}]) end),
  spawn_link(fun() ->
    rpc:call(Node3, node_server, start_link, [{[{Node1, 600, 400}, {Node2, 1200, 400}, {Node3, 600, 800}, {Node4, 1200, 800}], {600, 800}, 3}]) end),
  spawn_link(fun() ->
    rpc:call(Node4, node_server, start_link, [{[{Node1, 600, 400}, {Node2, 1200, 400}, {Node3, 600, 800}, {Node4, 1200, 800}], {1200, 800}, 4}]) end),
  spawn_link(fun() -> graphicConnection:init() end).


stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
