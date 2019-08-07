-module(backup_servers).
-author("Ofir & Raz").
-behaviour(gen_server).

%% API
-export([start_link/4, handle_cast/2, handle_call/3]).
-export([stash/2, nodeDown/2, nodeUp/1]).
-export([init/1, terminate/3]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%start the %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Node1, Node2, Node3, Node4) ->
  gen_server:start_link({local, backup_server}, ?MODULE, {Node1, Node2, Node3, Node4}, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%initial%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({Node1, Node2, Node3, Node4}) ->
  %connects the node the certain region
  {ok, {#{}, #{a => Node1,
    b => Node2,
    c => Node3,
    d => Node4}, [Node1, Node2, Node3, Node4], []}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%stash- save the Data in backup%%%%%%%%%%%%%%%%%%%%%%%
stash(Region, Backup) ->
  gen_server:cast(backup_server, {backup, Region, Backup}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%handle call that Node is down%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeDown(Region, Node) ->
  gen_server:call(backup_server, {nodeDown, Region, Node}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Cast nodeUp%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodeUp(Node) ->
  gen_server:cast(backup_server, {nodeUp, Node}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%handle cast of backup%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({backup, Region, Backup}, {Backups, RegionsNode, Nodes, DutyFIFO}) ->
  {noreply, {Backups#{Region => Backup}, RegionsNode, Nodes, DutyFIFO}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%handle cast of node up%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({nodeUp, Node}, {Backups, RegionsNode, Nodes, [{FirstNode, FirstRegion} | DutyFIFO]}) ->
  %% get region backup
  Backup = maps:get(FirstRegion, Backups),
  %% release first node in queue from duty
  rpc:cast(FirstNode, node_server, release, [FirstRegion]),
  rpc:cast(FirstNode, node_server, init, [FirstRegion, Backup]),
  %% make the new node take over
  {reply, {FirstRegion, Backup}, {Backups, RegionsNode, [Node | Nodes], DutyFIFO}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%handle call of Node Down%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminate%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_Reason, _State, _Data) ->
  ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%