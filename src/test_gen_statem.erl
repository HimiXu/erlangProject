-module(test_gen_statem).
-behaviour(gen_statem).
-define(NAME, test_gen_statem).

-export([start_link/0]).
-export([button/1]).
-export([init/0,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

start_link() ->
  gen_statem:start_link({local,?NAME}, ?MODULE, []).

init() ->
  {ok, idle},
  io:format("idle\n").

eat(Food) ->
  gen_statem:cast(?NAME, {eat,Food}).


callback_mode() ->
  state_functions.

eat(cast,{eat,Food},State) ->
  if Food == humus ->
    case State of
      idle -> io:format("oi vei\n"), {next_state, pooping};
      pooping -> {next_state,dead}
    end;
    true ->
      case State of
        idle -> io:format("food...\n"), {next_state, idle}
      end
  end.



terminate(_Reason, State, _Data) ->
  State =/= locked andalso do_lock(),
  ok.