%% -------------------------------------------------------------------
%% @doc XRef Runner.
%% This module is basically copied from rebar's rebar_xref, which in turn
%% borrows heavily from http://github.com/etnt/echeckxref project as
%% written by Torbjorn Tornkvist, Daniel Luna and others.
%% @see http://github.com/etnt/echeckxref
%% @end
%% -------------------------------------------------------------------
-module(xref_runner).
-author('tobbe@kreditor.se').
-author('daniel@lunas.se').
-author('elbrujohalcon@inaka.net').

-type check() :: undefined_function_calls
               | undefined_functions
               | locals_not_used
               | exports_not_used
               | deprecated_function_calls
               | deprecated_functions.

-type xref_default() :: {builtins | recurse | verbose | warnings, boolean()}.

-type config() :: #{ extra_paths    => [file:name_all()]
                   , xref_defaults  => [xref_default()]
                   , dirs           => [file:name_all()]
                   }.
-type warning() :: #{ filename      => file:name_all()
                    , line          => non_neg_integer()
                    , source        => mfa()
                    , target        => mfa()
                    , check         => check()
                    }.

-export_type([check/0, xref_default/0, config/0, warning/0]).
-export([main/1, check/0, check/1, check/2]).

%% @doc Allows us to runs xref_runner as script.
%%      This can be generated executing make escript
-spec main([string()]) -> ok.
main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {[], []}} ->
            check();
        {ok, {Options, Commands}} ->
            process_options(Options, Commands);
        {error, {Reason, Data}} ->
            xref_runner_utils:error_prn("~s ~p~n", [Reason, Data]),
            help()
    end.

%% @doc Runs a list of checks.
%%      To decide which checks to run and what options to use, it reads the
%%      xref.config file in the current folder expecting it to be something
%%      like a regular config file (i.e. [{app_name, [...]}].).
%%      The runner will then read the configuration for the xref application.
%%      It must contain two (optional) keys:
%%      - checks: the list of checks to perform
%%      - config: the configuration to use for all of them
-spec check() -> [warning()].
check() ->
  check("xref.config").

-spec check(file:name_all()) -> [warning()].
check(Path) ->
  {Checks, Config} =
    case file:consult(Path) of
      {ok, [FullConfig]} ->
        case proplists:get_value(xref, FullConfig) of
          undefined -> {all_checks(), #{}};
          XrefConfig ->
            { proplists:get_value(checks, XrefConfig, all_checks())
            , proplists:get_value(config, XrefConfig, #{})
            }
        end;
      {error, enoent} -> {all_checks(), #{}}
    end,
  lists:append([check(Check, Config) || Check <- Checks]).

%% @doc Runs a check on the dirs and with the options provided on Config.
-spec check(check(), config()) -> [warning()].
check(Check, Config) ->
  XrefDefaults = maps:get(xref_defaults, Config, []),
  Dirs = maps:get(dirs, Config, [ebin()]),

  lists:foreach(fun code:add_path/1, Dirs),

  {ok, Xref} = xref:start(?MODULE),
  try
    ok = xref:set_library_path(Xref, code_path(Config)),

    xref:set_default(Xref, XrefDefaults),

    lists:foreach(
      fun(Dir) ->
        {ok, _} = xref:add_directory(Xref, Dir)
      end, Dirs),

    {ok, Results} = xref:analyze(Xref, Check),

    FilteredResults = filter_xref_results(Check, Results),

    ResultToPrint = [result_to_warning(Check, Result) || Result <- FilteredResults],
    io:format("~p", [ResultToPrint]),
    ResultToPrint
  after
    stopped = xref:stop(Xref)
  end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Config = "Provide the path to the configuration file. "
               ++ "When none is provided xref_runner checks if there's "
               ++ "an ./xref.config file.",
    [
     {help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Config}
    ].

-spec help() -> ok.
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "xref_runner", standard_io).

-spec process_options([atom()], [string()]) -> ok.
process_options(Options, Commands) ->
    try
        Config = xref_runner_config:default(),
        AtomCommands = lists:map(fun list_to_atom/1, Commands),
        process_options(Options, AtomCommands, Config)
    catch
        throw:Exception -> {error, Exception}
    end.

-spec process_options([atom()], [string()], xref_runner_config:config()) -> ok.
process_options([help | Opts], Cmds, Config) ->
    help(),
    process_options(Opts, Cmds, Config);
process_options([{config, Path} | Opts], Cmds, Config) ->
    case file:consult(Path) of
      {ok, _} ->
          check(Path),
          process_options(Opts, Cmds, Config);
      {error, Reason} ->
          xref_runner_utils:error_prn("~p.", [Reason])
    end;
process_options([], _Cmds, _Config) ->
  ok.

all_checks() ->
  [ undefined_function_calls
  , undefined_functions
  , locals_not_used
  , exports_not_used
  , deprecated_function_calls
  , deprecated_functions
  ].

ebin() ->
  case filelib:is_dir("ebin") of
    true -> filename:absname("ebin");
    false -> filename:absname(".")
  end.

code_path(Config) ->
  ExtraPaths = maps:get(extra_paths, Config, []),
  [P || P <- code:get_path() ++ ExtraPaths, filelib:is_dir(P)].

filter_xref_results(Check, Results) ->
  SourceModules =
    lists:usort([source_module(Result) || Result <- Results]),

  Ignores =
    lists:flatmap(
      fun(Module) -> get_ignorelist(Module, Check) end, SourceModules),

  [Result || Result <- Results,
             not lists:member(parse_xref_result(Result), Ignores)].

source_module({Mt, _Ft, _At}) -> Mt;
source_module({{Ms, _Fs, _As}, _Target}) -> Ms.

%%
%% Ignore behaviour functions, and explicitly marked functions
%%
%% Functions can be ignored by using
%% -ignore_xref([{F, A}, {M, F, A}...]).
get_ignorelist(Mod, Check) ->
  %% Get ignore_xref attribute and combine them in one list
  Attributes =
    try
      Mod:module_info(attributes)
    catch
      _Class:_Error -> []
    end,

  IgnoreXref =
    [mfa(Mod, Value) || {ignore_xref, Values} <- Attributes, Value <- Values],

  BehaviourCallbacks = get_behaviour_callbacks(Check, Mod, Attributes),

  %% And create a flat {M,F,A} list
  IgnoreXref ++ BehaviourCallbacks.

get_behaviour_callbacks(exports_not_used, Mod, Attributes) ->
  Behaviours = [Value || {behaviour, Values} <- Attributes, Value <- Values],
  [{Mod, {Mod, F, A}}
   || B <- Behaviours, {F, A} <- B:behaviour_info(callbacks)];
get_behaviour_callbacks(_Check, _Mod, _Attributes) ->
  [].

mfa(M, {F, A}) -> {M, {M, F, A}};
mfa(M, MFA) -> {M, MFA}.

parse_xref_result({{SM, _, _}, MFAt}) -> {SM, MFAt};
parse_xref_result({TM, _, _} = MFAt) -> {TM, MFAt}.

result_to_warning(Check, {MFASource, MFATarget}) ->
  {Filename, Line} = get_source(MFASource),
  #{ filename => Filename
   , line     => Line
   , source   => MFASource
   , target   => MFATarget
   , check    => Check
   };
result_to_warning(Check, MFA) ->
  {Filename, Line} = get_source(MFA),
  #{ filename => Filename
   , line     => Line
   , source   => MFA
   , check    => Check
   }.

%%
%% Given a MFA, find the file and LOC where it's defined. Note that
%% xref doesn't work if there is no abstract_code, so we can avoid
%% being too paranoid here.
%%
get_source({M, F, A}) ->
  case code:get_object_code(M) of
    error -> {"", 0};
    {M, Bin, _} -> find_function_source(M, F, A, Bin)
  end.

find_function_source(M, F, A, Bin) ->
  AbstractCode = beam_lib:chunks(Bin, [abstract_code]),
  {ok, {M, [{abstract_code, {raw_abstract_v1, Code}}]}} = AbstractCode,

  %% Extract the original source filename from the abstract code
  [{attribute, 1, file, {Source, _}} | _] = Code,

  %% Extract the line number for a given function def
  Fn = [E || E <- Code,
             element(1, E) == function,
             element(3, E) == F,
             element(4, E) == A],

  case Fn of
    [{function, Line, F, _, _}] -> {Source, Line};
    %% do not crash if functions are exported, even though they
    %% are not in the source.
    %% parameterized modules add new/1 and instance/1 for example.
    [] -> {Source, 0}
  end.
