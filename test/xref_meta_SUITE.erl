-module(xref_meta_SUITE).
-author('euen@inaka.net').

-export([all/0]).
-export([dialyzer/1, elvis/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [dialyzer | elvis].
all() -> [dialyzer, elvis].

-spec dialyzer(config()) -> {comment, []}.
dialyzer(_Config) ->
  BaseDir = code:lib_dir(xref_runner),
  DefaultRebar3PltLoc = filename:join(BaseDir, "../../../default"),
  Plts = filelib:wildcard(filename:join(DefaultRebar3PltLoc, "*_plt")),
  Dirs = [filename:join(BaseDir, Dir) || Dir <- ["ebin", "test"]],
  Warnings = [error_handling, race_conditions, unmatched_returns],
  ct:comment("Dialyzer must emit no warnings"),
  Opts =
    [ {analysis_type, succ_typings}
    , {plts,          Plts}
    , {files_rec,     Dirs}
    , {check_plt,     true}
    , {warnings,      Warnings}
    , {get_warnings,  true}
    ],
  [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
  {comment, ""}.

-spec elvis(config()) -> {comment, []}.
elvis(_Config) ->
  BaseDir = code:lib_dir(xref_runner),
  ConfigFile = filename:join(BaseDir, "../../../../elvis.config"),
  ElvisConfig = [ fix_dirs(Group)
                || Group <- elvis_config:load_file(ConfigFile)],
  ct:comment("Elvis rocks!"),
  ok = elvis_core:rock(ElvisConfig),
  {comment, ""}.

fix_dirs(#{dirs := Dirs} = Group) ->
  NewDirs = [filename:join(code:lib_dir(xref_runner), Dir) || Dir <- Dirs],
  Group#{dirs := NewDirs}.