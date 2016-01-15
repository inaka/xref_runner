-module(locals_not_used).

-dialyzer([{nowarn_function, [local_not/1]}]).

-export([exported/0]).

exported() -> _ = local(true), {it, is, exports_not_used:exported()}.

local(Used) -> lists:foreach(fun inline/1, [true]), {it, is, Used}.

local_not(Used) -> {it, is, not Used}.

inline(Used) -> {it, is, Used, inline}.
