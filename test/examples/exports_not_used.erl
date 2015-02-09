-module(exports_not_used).

-export([exported/0, export_not/1]).

exported() -> {it, is, exported}.

exported_and_locally(Used) -> {it, is, exported, 'and', locally, Used}.

export_not(Used) -> local(Used), {it, is, not Used}.

local(Used) -> exported_and_locally(Used).
