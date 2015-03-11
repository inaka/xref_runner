-module(xref_runner_utils).

-export([
         error_prn/1,
         error_prn/2,
         parse_colors/1
        ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Gives format to the error to be displayed.
-spec error_prn(string()) -> ok.
error_prn(Message) ->
    error_prn(Message, []).

-spec error_prn(string(), [term()]) -> ok.
error_prn(Message, Args) ->
    ColoredMessage = "{{red}}Error: {{reset}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

print(Message, Args) ->
    Output = io_lib:format(Message, Args),
    EscapedOutput = escape_format_str(Output),
    io:format(parse_colors(EscapedOutput)).


-spec parse_colors(string()) -> string().
parse_colors(Message) ->
    Colors = #{"red" => "\e[0;31m",
               "red-bold" => "\e[1;31m",
               "green" => "\e[0;32m",
               "green-bold" => "\e[1;32m",
               "white" => "\e[0;37m",
               "white-bold" => "\e[1;37m",
               "reset" => "\e[0m"},
    Opts = [global, {return, list}],
    Fun = fun(Key, Acc) ->
                  Regex = ["{{", Key, "}}"],
                  Color = maps:get(Key, Colors),
                  re:replace(Acc, Regex, Color, Opts)
          end,
    lists:foldl(Fun, Message, maps:keys(Colors)).

-spec escape_format_str(string()) -> string().
escape_format_str(String) ->
    Binary = list_to_binary(String),
    Result = re:replace(Binary, "[^~]~", "~~", [global]),
    ResultBin = iolist_to_binary(Result),
    binary_to_list(ResultBin).
