-module(undefined_functions).

-export([bad/0, bad/1]).

bad() ->
  undefined_functions:undefined_here(),
  undefined_functions:bad(1).

bad(1) ->
  other_module:undefined_somewhere_else(),
  bad(2);
bad(2) ->
  lists:foreach(
    fun other_module:undefined_somewhere_else/1, lists:seq(1, 10)).
