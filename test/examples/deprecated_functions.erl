-module(deprecated_functions).

-deprecated({deprecated, 0}).
-deprecated({deprecated, '_', eventually}).

-export([deprecated/0, deprecated/1, not_deprecated/0]).

deprecated() -> {this, function, is, deprecated}.

deprecated(SomeDay) -> {this, function, will, be, removed, SomeDay}.

not_deprecated() -> {this, function, isnt, deprecated}.
