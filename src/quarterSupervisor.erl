%%%-------------------------------------------------------------------
%%% @author ofir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2019 2:33 AM
%%%-------------------------------------------------------------------
-module(quarterSupervisor).
-author("ofir").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


start_link(QuarterNumber) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [QuarterNumber]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([QuarterNumber]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = transient,
  Shutdown = 2000,
  Type = worker,

  AChild = {node_server, { node_server , start_link, [QuarterNumber]},
    Restart, Shutdown, Type, [dynamic]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
