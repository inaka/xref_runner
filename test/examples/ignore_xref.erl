-module(ignore_xref).

-deprecated({internal, '_'}).
-ignore_xref({deprecated_functions, deprecated, 1}).
-ignore_xref({ignored, 0}).

-export([bad/0, bad/1, good/0, internal/0, ignored/0]).

bad() ->
  deprecated_functions:deprecated().

bad(1) ->
  ignore_xref:internal(),
  deprecated_functions:deprecated(1).

good() ->
  deprecated_functions:not_deprecated().

internal() -> {this, is, deprecated}.

ignored() ->
  ignore_xref:ignored().
