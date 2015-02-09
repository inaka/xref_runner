-module(locals_not_used).

-export([exported/0]).

exported() -> local(true), {it, is, exports_not_used:exported()}.

local(Used) -> lists:foreach(fun inline/1, [true]), {it, is, Used}.

local_not(Used) -> {it, is, not Used}.

inline(Used) -> {it, is, Used, inline}.
