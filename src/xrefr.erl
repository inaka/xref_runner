-module(xrefr).
-author('euen@inakanerworks.com').

-export([ main/1
        , help/0
        , process_options/1
        ]).

%% @doc Allows us to runs xref_runner as script.
%%      This can be generated executing make escript
-spec main([string()]) -> ok.
main(Args) ->
  OptSpecList = option_spec_list(),
  ct:pal("main ~p", [getopt:parse(OptSpecList, Args)]),
  case getopt:parse(OptSpecList, Args) of
    {ok, {Options, _Commands}} ->
      process_options(Options);
    {error, {Reason, Data}} ->
      error_prn("~s ~p~n", [Reason, Data]),
      help(),
      maybe_halt()
  end.

maybe_halt() ->
  case application:get_env(xref_runner, halt_behaviour, halt) of
    halt -> erlang:halt(1);
    exception -> throw(halt)
  end.

% main("-c xref-test.config")

-spec help() -> ok.
help() ->
  OptSpecList = option_spec_list(),
  getopt:usage(OptSpecList, "xref_runner", standard_io).

-spec option_spec_list() -> [getopt:option_spec()].
option_spec_list() ->
    Config = "Provide the path to the configuration file. "
               ++ "When none is provided xref_runner checks if there's "
               ++ "an ./xref.config file.",
    [
     {help, $h, "help", undefined, "Show this help information."},
     {config, $c, "config", string, Config}
    ].

-spec process_options([atom()]) -> ok.
process_options([help | _Opts]) ->
  help();
process_options([{config, Path} | _Opts]) ->
  XrefWarnings = xref_runner:check(Path),
  warnings_prn(Path, XrefWarnings),
  XrefWarnings;
process_options([]) ->
  xref_runner:check().

generate_comment(RepoDir, XrefWarning) ->
  ct:pal("generate_comment: ~p", [{RepoDir, XrefWarning}]),
  #{ filename := Filename
   , line     := Line
   , source   := Source
   , check    := Check
   } = XrefWarning,
  Target = maps:get(target, XrefWarning, undefined),
  [ re:replace(Filename, [$^ | RepoDir], "", [{return, binary}])
  , ":", integer_to_list(Line)
  , " ", generate_comment_text(Check, Source, Target)
  ].

generate_comment_text(Check, {SM, SF, SA}, TMFA) ->
  SMFA = io_lib:format("`~p:~p/~p`", [SM, SF, SA]),
  generate_comment_text(Check, SMFA, TMFA);
generate_comment_text(Check, SMFA, {TM, TF, TA}) ->
  TMFA = io_lib:format("`~p:~p/~p`", [TM, TF, TA]),
  generate_comment_text(Check, SMFA, TMFA);
generate_comment_text(undefined_function_calls, SMFA, TMFA) ->
  io_lib:format("~s calls undefined function ~s", [SMFA, TMFA]);
generate_comment_text(undefined_functions, SMFA, _TMFA) ->
  io_lib:format("~s is not defined as a function", [SMFA]);
generate_comment_text(locals_not_used, SMFA, _TMFA) ->
  io_lib:format("~s is an unused local function", [SMFA]);
generate_comment_text(exports_not_used, SMFA, _TMFA) ->
  io_lib:format("~s is an unused export", [SMFA]);
generate_comment_text(deprecated_function_calls, SMFA, TMFA) ->
  io_lib:format("~s calls deprecated function ~s", [SMFA, TMFA]);
generate_comment_text(deprecated_functions, SMFA, _TMFA) ->
  io_lib:format("~s is deprecated", [SMFA]).

warnings_prn(Path, Comments) ->
  Messages = [generate_comment(Path, Comment) || Comment <- Comments],
  lists:foreach(fun warning_prn/1, Messages).

-spec warning_prn(string()) -> ok.
warning_prn(Message) ->
  FullMessage = Message ++ "~n",
  io:format(FullMessage, []).

-spec error_prn(string(), [term()]) -> ok.
error_prn(Message, Args) ->
  FullMessage = "Error: " ++ Message ++ "~n",
  io:format(FullMessage, Args).
