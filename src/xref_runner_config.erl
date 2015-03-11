-module(xref_runner_config).

-export([
         default/0,
         load_file/1,
         load/1
        ]).

-export_type([
              config/0
             ]).

-type config() :: map().

-define(DEFAULT_CONFIG_PATH, "./xref.config").
-define(DEFAULT_FILTER, "*.erl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default() -> config().
default() ->
    case file:consult(?DEFAULT_CONFIG_PATH) of
        {ok, [Config]} ->
            load(Config);% [#{dirs => ["test"]}]
        {error, enoent} ->
            Config = application:get_env(elvis, config, []),
            ensure_config_list(Config);
        {error, Reason} ->
            throw(Reason)
    end.

-spec load_file(string()) -> config().
load_file(Path) ->
    ct:pal("consult ~p", [file:consult(Path)]),
    case file:consult(Path) of
        {ok, [Config]} ->
            load(Config);
        {error, Reason} ->
            throw(Reason)
    end.

-spec load(term()) -> config().
load(AppConfig) ->
    XrefConfig = proplists:get_value(xref, AppConfig, []),
    Config =  proplists:get_value(config, XrefConfig, []),
    ensure_config_list(Config).

ensure_config_list(Config) when is_map(Config) ->
    [Config];
ensure_config_list(Config) ->
    Config.