-module(app).
-author("Ofir & Raz").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%%%%%%%%%%%%%Start function- This function create all 4 gen_server at the remote nodes, set the graphic and backup%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(normal, [Node1, Node2, Node3, Node4]) ->
  %start graphic on this node
  spawn_link(fun() -> graphic:start_link(graphicConnectionPID) end),
  %%starts the gen servers on the remote nodes
  spawn_link(fun() ->
    rpc:call(Node1, node_server, start_link, [{Node1,Node2,Node3,Node4,a}]) end),
  spawn_link(fun() ->
    rpc:call(Node2, node_server, start_link, [{Node1,Node2,Node3,Node4,b}]) end),
  spawn_link(fun() ->
    rpc:call(Node3, node_server, start_link, [{Node1,Node2,Node3,Node4,c}]) end),
  spawn_link(fun() ->
    rpc:call(Node4, node_server, start_link, [{Node1,Node2,Node3,Node4,d}]) end),
  %start graphic Connection  on this node
  spawn_link(fun() -> graphicConnection:init([Node1, Node2, Node3, Node4]) end),
  %start the backup server on this node
  backup_servers:start_link(Node1, Node2, Node3, Node4).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_State) ->
  ok.

