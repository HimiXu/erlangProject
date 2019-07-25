-module(test_gen_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([getinfo/0]).
-export([init/1, handle_call/3]).

start_link() ->
  gen_server:start_link({local, test_otp}, test_otp, [], []).

getinfo() ->
  gen_server:call(test_otp, getinfo).

init(_Args) ->
  {ok, #{info => "hello"}}.

handle_call(getinfo, _From, M) ->
  Res = maps:get(info, M),
  {reply, Res, M}.

