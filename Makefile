PROJECT = xref_runner

DEPS = lager sync eper

dep_eper = git git://github.com/massemanet/eper.git 0.90.0
dep_lager = git git://github.com/basho/lager.git 2.1.0
dep_sync = git git://github.com/inaka/sync.git 0.1

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 32768}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

include erlang.mk

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_SUITES = xref_runner
CT_OPTS = -cover test/xref_runner.coverspec

SHELL_OPTS= -name ${PROJECT}@`hostname` -s sync
