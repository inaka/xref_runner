-module(xref_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ktn_meta_SUITE
        , [ dialyzer/1
          , elvis/1
          ]
        }]).

-export([all/0, init_per_suite/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [dialyzer | elvis].
all() -> [dialyzer, elvis].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> [{base_dir, "../.."} | Config].
