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
  gen_server:start_link({local, backup}, ?MODULE, {Node1, Node2, Node3, Node4}, []).

init({Node1, Node2, Node3, Node4}) ->
  {ok, {#{{0, 600, 0, 400} => Node1,
    {600, 1200, 0, 400} => Node2,
    {0, 600, 400, 800} => Node3,
    {600, 1200, 400, 800} => Node4}, #{}, [Node1, Node2, Node3, Node4], []}}.

stash(Region, Backup) ->
  gen_server:cast(backup, {backup, Region, Backup}).

nodeDown(Region, Node) ->
  gen_server:call(backup, {nodeDown, Region, Node}).

nodeUp(Node) ->
  gen_server:cast(backup, {nodeUp, Node}).


handle_cast({backup, Region, Backup}, {Backups, RegionsNode, DutyFIFO}) ->
  {noreply, {Backups#{Region => Backup}, RegionsNode, DutyFIFO}}.



handle_call({nodeDown, Region, Node}, _From, {Backups, RegionsNode, Nodes, DutyFIFO}) ->
  %% get region backup
  Backup = maps:get(Region, Backups),
  %% remove node from list
  NodesUp = Nodes -- [Node],
  %% pick randomly a node
  NodeTakeOver = lists:nth(rand:uniform(length(NodesUp)), NodesUp),
  %% make node in charge
  rpc:cast(NodeTakeOver, node_server, takeover, [{Region, Backup}]),
  %% reply with the new node in charge of Region
  {reply, NodeTakeOver, {Backups, RegionsNode#{Region => NodeTakeOver}, NodesUp, DutyFIFO ++ [{NodeTakeOver, Region}]}};

handle_call({nodeUp, Node}, _From, {Backups, RegionsNode, Nodes, [{FirstNode, FirstRegion} | DutyFIFO]}) ->
  %% get region backup
  Backup = maps:get(FirstRegion, Backups),
  %% release first node in queue from duty
  rpc:cast(FirstNode, node_server, release, [FirstRegion]),
  %% make the new node take over
  {reply, {FirstRegion, Backup}, {Backups, RegionsNode, [Node | Nodes], DutyFIFO}}.

terminate(_Reason, _State, _Data) ->
  ok.