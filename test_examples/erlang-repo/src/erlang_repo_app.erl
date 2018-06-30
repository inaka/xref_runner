-module(erlang_repo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_, [term()]) -> {ok, pid()}.
start(_Type, _Args) ->
  erlang_repo_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
  ok.

