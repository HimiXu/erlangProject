%%%-------------------------------------------------------------------
%%% @author ofir
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2019 2:18 AM
%%%-------------------------------------------------------------------
-module(quarter1).
-author("ofir").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(normal, _StartArgs) ->
node_server:start_link(1);
start({failover, _Node}, _StartArgs)->
  node_server:start_link(1).

stop(_State) ->
  ok.

