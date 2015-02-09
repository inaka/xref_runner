%% -------------------------------------------------------------------
%% This module is basically copied from rebar's rebar_xref, which in turn
%% borrows heavily from rebar's  http://github.com/etnt/eCheck project as
%% written by Torbjorn Tornkvist <tobbe@kreditor.se>, Daniel Luna
%% <daniel@lunas.se> and others.
%% -------------------------------------------------------------------
-module(xref_runner).

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
                    }.

-export_type([check/0, xref_default/0, config/0, warning/0]).
-export([run/2]).

-spec run(check(), config()) -> [warning()].
run(Check, Config) ->
  XrefDefaults = maps:get(xref_defaults, Config, []),
  Dirs = maps:get(dirs, Config, ["ebin"]),

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

    [result_to_warning(Result) || Result <- FilteredResults]
  after
    stopped = xref:stop(Xref)
  end.

%% ===================================================================
%% Internal functions
%% ===================================================================

code_path(#{extra_paths := ExtraPaths}) ->
  [P || P <- code:get_path() ++ ExtraPaths].

filter_xref_results(Check, Results) ->
  SourceModules =
    lists:usort([source_module(Result) || Result <- Results]) -- [undefined],

  Ignores =
    lists:flatmap(
      fun(Module) -> get_ignorelist(Module, Check) end, SourceModules),

  [Result || Result <- Results,
             not lists:member(parse_xref_result(Result), Ignores)].

source_module({Mt,_Ft,_At}) -> Mt;
source_module({{Ms,_Fs,_As},_Target}) -> Ms;
source_module(_) -> undefined.

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

  BehaviourCallbacks = get_behaviour_callbacks(Check, Attributes),

  %% And create a flat {M,F,A} list
  IgnoreXref ++ BehaviourCallbacks.

get_behaviour_callbacks(exports_not_used, Attributes) ->
  Behaviours = [Value || {behaviour, Values} <- Attributes, Value <- Values],
  [B:behaviour_info(callbacks) || B <- Behaviours];
get_behaviour_callbacks(_Check, _Attributes) ->
  [].

mfa(M, {F, A}) -> {M, F, A};
mfa(_M, MFA) -> MFA.

parse_xref_result({_, MFAt}) -> MFAt;
parse_xref_result(MFAt) -> MFAt.

result_to_warning({MFASource, MFATarget}) ->
  {Filename, Line} = get_source(MFASource),
  #{ filename => Filename
   , line     => Line
   , source   => MFASource
   , target   => MFATarget
   };
result_to_warning(MFA) ->
  {Filename, Line} = get_source(MFA),
  #{ filename => Filename
   , line     => Line
   , source   => MFA
   , target   => MFA
   }.

%%
%% Given a MFA, find the file and LOC where it's defined. Note that
%% xref doesn't work if there is no abstract_code, so we can avoid
%% being too paranoid here.
%%
get_source({M, F, A}) ->
  case code:get_object_code(M) of
    error -> {"", 0};
    {M, Bin, _} -> find_function_source(M,F,A,Bin)
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
