%%%-------------------------------------------------------------------
%%% @author ofir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2019 1:44 AM
%%%-------------------------------------------------------------------
-module(graphicSupervisor).
-author("ofir").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



start_link() ->
  supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = transient,
  Shutdown = 2000,
  Type = worker,

  AChild = {graphic, {graphic, start_link, [graphicConnectionPID]},
    Restart, Shutdown, Type, [dynamic]},
  BChild = {graphicConnection, {graphicConnection, init, []},
    Restart, Shutdown, Type, [dynamic]},

  {ok, {SupFlags, [AChild, BChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
