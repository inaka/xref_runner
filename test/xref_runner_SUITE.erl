-module(xref_runner_SUITE).
-author('elbrujohalcon@inaka.net').

-export([ all/0
        , undefined_function_calls/1
        , undefined_functions/1
        , locals_not_used/1
        , exports_not_used/1
        , deprecated_function_calls/1
        , deprecated_functions/1
        , ignore_xref/1
        , check_with_config_file/1
        , check_with_no_config_file/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, F /= module_info].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec undefined_function_calls(config()) -> {comment, string()}.
undefined_function_calls(_Config) ->
  Path = filename:dirname(code:which(undefined_function_calls)),
  Config = #{ dirs => [Path] },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(undefined_function_calls, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "undefined_function_calls.erl"],

  ct:comment(
    "It contains a warning for undefined_function_calls:undefined_here()"),
  [W1] =
    [ W || #{ line      := 5 % Where the function is defined
            , source    := {undefined_function_calls, bad, 0}
            , target    := {undefined_function_calls, undefined_here, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for undefined_functions:undefined_there()"),
  [W2] =
    [ W || #{ line      := 9 % Where the function is defined
            , source    := {undefined_function_calls, bad, 1}
            , target    := {undefined_functions, undefined_there, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for other_module:undefined_somewhere_else(_)"),
  [W3] =
    [ W || #{ line      := 9 % Where the function is defined
            , source    := {undefined_function_calls, bad, 1}
            , target    := {other_module, undefined_somewhere_else, 1}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1, W2, W3],

  {comment, ""}.

-spec undefined_functions(config()) -> {comment, string()}.
undefined_functions(_Config) ->
  Path = filename:dirname(code:which(undefined_functions)),
  Config = #{ dirs => [Path]
            , xref_defaults => []
            },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(undefined_functions, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "undefined_functions.erl"],

  ct:comment(
    "It contains a warning for undefined_functions:undefined_here()"),
  [W1] =
    [ W || #{ line      := 0 % It's a module level bug
            , source    := {undefined_functions, undefined_here, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for undefined_functionse:undefined_there()"),
  [W2] =
    [ W || #{ line      := 0 % It's a module level bug
            , source    := {undefined_functions, undefined_there, 0}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1, W2],

  {comment, ""}.

-spec locals_not_used(config()) -> {comment, string()}.
locals_not_used(_Config) ->
  Path = filename:dirname(code:which(locals_not_used)),
  Config = #{ dirs => [Path] },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(locals_not_used, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "locals_not_used.erl"],

  ct:comment(
    "It contains a warning for locals_not_used:local_not()"),
  [W1] =
    [ W || #{ line      := 9
            , source    := {locals_not_used, local_not, 1}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1],

  {comment, ""}.

-spec exports_not_used(config()) -> {comment, string()}.
exports_not_used(_Config) ->
  Path = filename:dirname(code:which(exports_not_used)),
  Config = #{ dirs => [Path] },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(exports_not_used, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "exports_not_used.erl"],

  ct:comment(
    "It contains a warning for exports_not_used:export_not()"),
  [W1] =
    [ W || #{ line      := 9
            , source    := {exports_not_used, export_not, 1}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1],

  {comment, ""}.

-spec deprecated_function_calls(config()) -> {comment, string()}.
deprecated_function_calls(_Config) ->
  Path = filename:dirname(code:which(deprecated_function_calls)),
  Config = #{ dirs => [Path] },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(deprecated_function_calls, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "deprecated_function_calls.erl"],

  ct:comment(
    "It contains a warning for deprecated_function_calls:internal()"),
  [W1] =
    [ W || #{ line      := 10 % Where the function is defined
            , source    := {deprecated_function_calls, bad, 1}
            , target    := {deprecated_function_calls, internal, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for deprecated_functions:deprecated()"),
  [W2] =
    [ W || #{ line      := 7 % Where the function is defined
            , source    := {deprecated_function_calls, bad, 0}
            , target    := {deprecated_functions, deprecated, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for deprecated_functions:deprecated(_)"),
  [W3] =
    [ W || #{ line      := 10 % Where the function is defined
            , source    := {deprecated_function_calls, bad, 1}
            , target    := {deprecated_functions, deprecated, 1}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1, W2, W3],

  {comment, ""}.

-spec deprecated_functions(config()) -> {comment, string()}.
deprecated_functions(_Config) ->
  Path = filename:dirname(code:which(deprecated_functions)),
  Config = #{ dirs => [Path] },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(deprecated_functions, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "deprecated_functions.erl"],

  ct:comment(
    "It contains a warning for deprecated_functions:deprecated()"),
  [W1] =
    [ W || #{ line      := 8 % Where the function is defined
            , source    := {deprecated_functions, deprecated, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for deprecated_functionse:deprecated_there()"),
  [W2] =
    [ W || #{ line      := 10 % Where the function is defined
            , source    := {deprecated_functions, deprecated, 1}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1, W2],

  {comment, ""}.

-spec ignore_xref(config()) -> {comment, string()}.
ignore_xref(_Config) ->
  Path = filename:dirname(code:which(ignore_xref)),
  Config = #{ dirs => [Path] },

  ct:comment("It runs"),
  AllWarnings = xref_runner:check(deprecated_function_calls, Config),
  Warnings =
    [W || W = #{filename := F} <- AllWarnings
        , filename:basename(F) == "ignore_xref.erl"],

  ct:comment(
    "It contains a warning for ignore_xref:internal()"),
  [W1] =
    [ W || #{ line      := 12 % Where the function is defined
            , source    := {ignore_xref, bad, 1}
            , target    := {ignore_xref, internal, 0}
            } = W <- Warnings],

  ct:comment(
    "It contains a warning for deprecated_functions:deprecated()"),
  [W2] =
    [ W || #{ line      := 9 % Where the function is defined
            , source    := {ignore_xref, bad, 0}
            , target    := {deprecated_functions, deprecated, 0}
            } = W <- Warnings],

  ct:comment("It contains no other warnings"),
  [] = Warnings -- [W1, W2],

  {comment, ""}.

-spec check_with_no_config_file(config()) -> {comment, string()}.
check_with_no_config_file(_Config) ->

  ct:comment("Make sure there is no config"),
  false = filelib:is_regular("xref.config"),

  ct:comment("Run the checks in the wrong folder"),
  [] = xref_runner:check(),

  ct:comment("cd to the right folder"),
  OldCwd = file:get_cwd(),
  try
    Path = filename:dirname(code:which(ignore_xref)),
    file:set_cwd(Path),
    ct:pal("~p", [Path]),

    ct:comment("Run the checks with an empty ebin folder"),
    case filelib:is_dir("ebin") of
      true -> file:del_dir("ebin");
      false -> ok
    end,
    file:make_dir("ebin"),
    [] = xref_runner:check(),

    ct:comment("Run the checks in the right folder, without ebin"),
    ok = file:del_dir("ebin"),
    Results = xref_runner:check(), %% All the warnings from the other tests
    [_|_] = [1 || #{check := undefined_function_calls} <- Results],
    [_|_] = [1 || #{check := undefined_functions} <- Results],
    [_|_] = [1 || #{check := locals_not_used} <- Results],
    [_|_] = [1 || #{check := exports_not_used} <- Results],
    [_|_] = [1 || #{check := deprecated_function_calls} <- Results],
    [_|_] = [1 || #{check := deprecated_functions} <- Results],

    {comment, ""}
  after
    file:set_cwd(OldCwd)
  end.


-spec check_with_config_file(config()) -> {comment, string()}.
check_with_config_file(_Config) ->
  Config =
    [{xref, [{config, #{}}]}],
  {comment, ""}.
