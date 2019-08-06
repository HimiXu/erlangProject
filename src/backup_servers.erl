%%%-------------------------------------------------------------------
%%% @author raz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 20:37
%%%-------------------------------------------------------------------
-module(backup_servers).
-author("raz").
-behaviour(gen_server).

%% API
-export([start_link/4, handle_cast/2, handle_call/3]).
-export([stash/2, nodeDown/2, nodeUp/1]).
-export([init/1, terminate/3]).

start_link(Node1, Node2, Node3, Node4) ->
  gen_server:start_link({local, backup_server}, ?MODULE, {Node1, Node2, Node3, Node4}, []).

init({Node1, Node2, Node3, Node4}) ->
  {ok, {#{}, #{a => Node1,
    b => Node2,
    c => Node3,
    d => Node4}, [Node1, Node2, Node3, Node4], []}}.

stash(Region, Backup) ->
  gen_server:cast(backup_server, {backup, Region, Backup}).

nodeDown(Region, Node) ->
  gen_server:call(backup_server, {nodeDown, Region, Node}).

nodeUp(Node) ->
  gen_server:cast(backup_server, {nodeUp, Node}).

handle_cast({backup, Region, Backup}, {Backups, RegionsNode, Nodes, DutyFIFO}) ->
  {noreply, {Backups#{Region => Backup}, RegionsNode, Nodes, DutyFIFO}};

handle_cast({nodeUp, Node}, {Backups, RegionsNode, Nodes, [{FirstNode, FirstRegion} | DutyFIFO]}) ->
  %% get region backup
  Backup = maps:get(FirstRegion, Backups),
  %% release first node in queue from duty
  rpc:cast(FirstNode, node_server, release, [FirstRegion]),
  %% TODO initialization from existing
  rpc:cast(FirstNode, node_server, init, [FirstRegion, Backup]),
  %% make the new node take over
  {reply, {FirstRegion, Backup}, {Backups, RegionsNode, [Node | Nodes], DutyFIFO}}.

handle_call({nodeDown, Region, Node}, _From, {Backups, RegionsNode, Nodes, DutyFIFO}) ->
  %% get region backup
  Backup = maps:get(Region, Backups),
  %% remove node from list
  NodesUp = Nodes -- [Node],
  %% pick randomly a node
  Random =
    try
      rand:uniform(length(NodesUp))
    catch
      _:_ -> 1
    end,
  NodeTakeOver = lists:nth(Random, NodesUp),
%% make node in charge
  io:format("Node ~p takes over region ~p~n",[NodeTakeOver,Region]),
  rpc:cast(NodeTakeOver, node_server, takeover, [{Region, Backup}]),
%% reply with the new node in charge of Region
  {reply, NodeTakeOver, {Backups, RegionsNode#{Region => NodeTakeOver}, NodesUp, DutyFIFO ++ [{NodeTakeOver, Region}]}}.

terminate(_Reason, _State, _Data) ->
  ok.