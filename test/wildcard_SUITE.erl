-module(wildcard_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    empty_list_dir/1,
    not_exist_dir/1,
    exist_dir/1,
    match_one_character_dir/1,
    match_one_character_dir_with_subdir/1,
    match_any_characters_dir/1,
    match_any_characters_dir_with_subdir/1,
    match_all_directories_and_subdirectories/1,
    match_alternatives_with_subdir/1
]).

all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, 1} <- Exports, F /= module_info].

init_per_testcase(_Name, Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [
        filename:join([PrivDir, X, Y]) ||
            X <- ["a", "b", "c", "dd"],
            Y <- ["", "ebin"]
    ],
    [file:make_dir(Dir) || Dir <- Dirs],
    [{dirs, Dirs} | Config].

end_per_testcase(_Name, Config) ->
    Dirs = ?config(dirs, Config),
    [file:del_dir(Dir) || Dir <- Dirs],
    ok.

empty_list_dir(_Config) ->
    [] = xref_runner:find_dirs([]).

not_exist_dir(_Config) ->
    [] = xref_runner:find_dirs(["not_exist"]).

exist_dir(Config) ->
    PrivDir = filename:join([?config(priv_dir, Config)]),
    [PrivDir] = xref_runner:find_dirs([PrivDir]).

match_one_character_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [filename:join([PrivDir, "?"])],
    Found = xref_runner:find_dirs(Dirs),
    ["a", "b", "c"] = basenames(Found).

match_one_character_dir_with_subdir(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [filename:join([PrivDir, "?", "ebin"])],
    Found = xref_runner:find_dirs(Dirs),
    ["a", "b", "c"] = basenames(dirnames(Found)).

match_any_characters_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [filename:join([PrivDir, "*"])],
    Found = xref_runner:find_dirs(Dirs),
    ["a", "b", "c", "dd"] = basenames(Found).

match_any_characters_dir_with_subdir(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [filename:join([PrivDir, "*", "ebin"])],
    Found = xref_runner:find_dirs(Dirs),
    ["a", "b", "c", "dd"] = basenames(dirnames(Found)).

match_all_directories_and_subdirectories(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [filename:join([PrivDir, "**", "ebin"])],
    Found = xref_runner:find_dirs(Dirs),
    ["a", "b", "c", "dd"] = basenames(dirnames(Found)).

match_alternatives_with_subdir(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Dirs = [filename:join([PrivDir, "{a,dd}", "ebin"])],
    Found = xref_runner:find_dirs(Dirs),
    ["a", "dd"] = basenames(dirnames(Found)).

basenames(Dirs) ->
    [filename:basename(Dir) || Dir <- Dirs].

dirnames(Dirs) ->
    [filename:dirname(Dir) || Dir <- Dirs].
