PROJECT = xref_runner

DEPS = getopt
LOCAL_DEPS := tools
TEST_DEPS := katana mixer
BUILD_DEPS = inaka_mk hexer_mk

# Prevents erlang.mk from downloading katana's xref_runner dependency
IGNORE_DEPS = xref_runner

dep_getopt = hex 0.8.2
dep_katana = hex 0.2.19
dep_mixer = git https://github.com/inaka/mixer.git 0.1.4
dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.0.2

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# To avoid eunit autocompile
TEST_ERLC_OPTS = +debug_info

# Commont Test Config
CT_OPTS += -cover test/cover.spec -vvv

ESCRIPT_NAME := xrefr

install: escript
	cp xrefr /usr/local/bin
