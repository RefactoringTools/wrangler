APPNAME = wrangler
include vsn.mk

ERL     = erl
ERLC    = erlc
prefix  = /usr/local
datadir = ${prefix}/share
LIB_DIR = ${datadir}/wrangler

DOC_OPTS={def,{version,\"$(VERSION)\"}}

########################################
## Main part

BUILD_FIRST = ebin/wrangler_parse.beam ebin/wrangler_expand_rule.beam ebin/api_refac.beam \
   ebin/wrangler_scan.beam ebin/wrangler_epp_dodger.beam ebin/wrangler_syntax.beam \
   ebin/wrangler_syntax_lib.beam ebin/wrangler_misc.beam ebin/api_ast_traverse.beam

ERL_SRC = $(wildcard src/*.erl)
ERL_OBJ = $(BUILD_FIRST) $(patsubst src/%.erl,ebin/%.beam,$(ERL_SRC))

.PHONY: default all conf erl c

default: conf erl c

all: default docs

conf: configure c_src/suffixtree/Makefile c_src/gsuffixtree/Makefile

c_src/suffixtree/Makefile c_src/gsuffixtree/Makefile: \
		c_src/suffixtree/Makefile.in c_src/gsuffixtree/Makefile.in
	./configure

erl: $(ERL_OBJ)

c:
	@cd ./c_src; make; cd ../..

########################################
## Rules

configure: configure.ac
	$(MAKE) distclean
	autoconf

.SUFFIXES: .erl .yrl

## Erlang
ebin/%.beam: src/%.erl
	$(ERLC) -pa ebin -I include -W -o ebin +debug_info $<

ebin/%.beam: include/wrangler.hrl include/wrangler_internal.hrl

src/%.erl: src/%.yrl
	$(ERLC) -o src $<

########################################


clean:
	@cd ./c_src; make clean; cd ../..
	@-rm -f $(ERL_OBJ) ebin/erl_parse.beam
	@-rm -f priv/suffixtree priv/gsuffixtree
	@-rm -f doc/*.html doc/edoc-info doc/erlang.png doc/stylesheet.css
	@-rm -f doc/*.erl

distclean: clean
	@-rm -f c_src/suffixtree/Makefile c_src/gsuffixtree/Makefile
	@-rm -f config.* configure.scan
	@-rm -rf autom4te.cache

install: default
	@echo "* Installing Emacs Lisp Library"
	install -m 775 -d $(LIB_DIR)/elisp
	install -m 775 elisp/*.el $(LIB_DIR)/elisp
	@echo
	@echo "* Installing Erlang Library"
	install -m 775 -d $(LIB_DIR)/ebin
	install -m 775 ebin/*.beam $(LIB_DIR)/ebin
	install -m 775 ebin/*.app $(LIB_DIR)/ebin
	install -m 775 -d $(LIB_DIR)/src
	install -m 775 src/*.erl $(LIB_DIR)/src
	install -m 775 -d $(LIB_DIR)/include
	install -m 775 include/*.hrl $(LIB_DIR)/include
	install -m 775 -d $(LIB_DIR)/priv
	install -m 775 priv/suffixtree* $(LIB_DIR)/priv
	install -m 755 priv/gsuffixtree* $(LIB_DIR)/priv
	install -m 775 priv/side_effect_plt $(LIB_DIR)/priv
	install -m 775 priv/dialyzer_plt $(LIB_DIR)/priv
	@echo
	@echo "*** Successfully installed. See README for usage instructions."
	@echo

wc:
	@echo "* Emacs Lisp"
	@wc -l */*.el | sort -nr
	@echo "* Erlang"
	@wc -l */*.erl | sort -nr
	@echo "* C"
	@wc -l */*.c | sort -nr


EXAMPLE_CODE = inspec_examples.erl refac_list.erl \
    refac_apply_to_remote_call.erl refac_remove_arg.erl \
    refac_batch_rename_fun.erl refac_remove_import.erl refac_intro_import.erl \
    refac_specialise.erl refac_keysearch_to_keyfind.erl refac_swap_args.erl

docs:
	$(ERL) -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop
	(cd src ; cp $(EXAMPLE_CODE) ../doc/)

privdocs:
	$(MAKE) DOC_OPTS='$(DOC_OPTS)',private docs
