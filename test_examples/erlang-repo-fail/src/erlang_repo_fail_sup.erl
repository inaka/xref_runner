-module(erlang_repo_fail_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [],
    erlang_repo_fail_app:non_exist_function(),
    {ok, {{one_for_one, 1, 5}, Procs}}.