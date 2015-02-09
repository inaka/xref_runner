-module(deprecated_function_calls).

-deprecated({internal, '_'}).

-export([bad/0, bad/1, good/0, internal/0]).

bad() ->
  deprecated_functions:deprecated().

bad(1) ->
  deprecated_function_calls:internal(),
  deprecated_functions:deprecated(1).

good() ->
  deprecated_functions:not_deprecated().

internal() -> {this, is, deprecated}.
