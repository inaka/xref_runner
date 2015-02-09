-module(undefined_function_calls).

-export([bad/0, bad/1]).

bad() ->
  undefined_function_calls:undefined_here(),
  undefined_function_calls:bad(1).

bad(1) ->
  undefined_functions:undefined_there(),
  bad(2);
bad(2) ->
  lists:foreach(
    fun other_module:undefined_somewhere_else/1, lists:seq(1, 10)).
