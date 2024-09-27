## This file is waived into the public domain, as-is, no warranty provided.
##
## If the public domain doesn't exist where you live, consider
## this a license which waives all copyright and neighboring intellectual
## restrictions laws mechanisms, to the fullest extent possible by law,
## as-is, no warranty provided.
##
## No attribution is required and you are free to copy-paste and munge
## into your own project.

bin_SCRIPTS =

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
GUILE_DEBUG_FLAGS = @guile_debug_flags@
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) $(GUILE_DEBUG_FLAGS) -o "$@" "$<"

SOURCES = goblins.scm \
          goblins/abstract-types.scm \
          goblins/actor-lib/cell.scm \
          goblins/actor-lib/common.scm \
          goblins/actor-lib/facet.scm \
          goblins/actor-lib/io.scm \
          goblins/actor-lib/inbox.scm \
          goblins/actor-lib/joiners.scm \
          goblins/actor-lib/let-on.scm \
          goblins/actor-lib/methods.scm \
          goblins/actor-lib/nonce-registry.scm \
          goblins/actor-lib/opportunistic.scm \
          goblins/actor-lib/pubsub.scm \
          goblins/actor-lib/pushdown.scm \
          goblins/actor-lib/queue.scm \
          goblins/actor-lib/sealers.scm \
          goblins/actor-lib/selfish-spawn.scm \
          goblins/actor-lib/simple-mint.scm \
          goblins/actor-lib/swappable.scm \
          goblins/actor-lib/ticker.scm \
          goblins/actor-lib/ward.scm \
          goblins/base-io-ports.scm \
          goblins/config.scm \
          goblins/contrib/syrup.scm \
          goblins/core.scm \
          goblins/core-types.scm \
          goblins/default-vat-scheduler.scm \
          goblins/define-actor.scm \
          goblins/ghash.scm \
          goblins/inbox.scm \
          goblins/migrations.scm \
          goblins/ocapn/captp.scm \
          goblins/ocapn/ids.scm \
          goblins/ocapn/marshalling.scm \
          goblins/ocapn/netlayer/base-port.scm \
          goblins/ocapn/netlayer/fake.scm \
          goblins/ocapn/netlayer/onion-socks.scm \
          goblins/ocapn/netlayer/onion.scm \
          goblins/ocapn/netlayer/libp2p.scm \
          goblins/ocapn/netlayer/prelay.scm \
          goblins/ocapn/netlayer/prelay-utils.scm \
          goblins/ocapn/netlayer/tcp-tls.scm \
          goblins/ocapn/netlayer/testuds.scm \
          goblins/ocapn/netlayer/utils.scm \
          goblins/persistence-store/memory.scm \
          goblins/persistence-store/syrup.scm \
          goblins/utils/assert-type.scm \
          goblins/utils/base32.scm \
          goblins/utils/bytes-stuff.scm \
          goblins/utils/crypto.scm \
          goblins/utils/graphviz.scm \
          goblins/utils/random-name.scm \
          goblins/utils/simple-dispatcher.scm \
          goblins/utils/simple-sealers.scm \
          goblins/utils/weak-box.scm \
          goblins/utils/ring-buffer.scm \
          goblins/vat.scm \
          goblins/vrun.scm \
          goblins/repl.scm \
          tests/repl-driver.scm \
          tests/utils.scm

TESTS = tests/actor-lib/test-cell.scm \
        tests/actor-lib/test-common.scm \
        tests/actor-lib/test-facet.scm \
        tests/actor-lib/test-io.scm \
        tests/actor-lib/test-inbox.scm \
        tests/actor-lib/test-joiners.scm \
        tests/actor-lib/test-let-on.scm \
        tests/actor-lib/test-nonce-registry.scm \
        tests/actor-lib/test-opportunistic.scm \
        tests/actor-lib/test-pubsub.scm \
        tests/actor-lib/test-pushdown.scm \
        tests/actor-lib/test-queue.scm \
        tests/actor-lib/test-sealers.scm \
        tests/actor-lib/test-selfish-spawn.scm \
        tests/actor-lib/test-simple-mint.scm \
        tests/actor-lib/test-swappable.scm \
        tests/actor-lib/test-ticker.scm \
        tests/actor-lib/test-ward.scm \
        tests/test-migrations.scm \
        tests/ocapn/test-marshalling.scm \
        tests/ocapn/test-captp.scm \
        tests/ocapn/netlayer/test-fake.scm \
        tests/ocapn/netlayer/test-libp2p.scm \
        tests/ocapn/netlayer/test-prelay.scm \
        tests/ocapn/netlayer/test-prelay-utils.scm \
        tests/persistence-store/test-syrup.scm \
        tests/ocapn/test-ids.scm \
        tests/test-await.scm \
        tests/test-core.scm \
        tests/test-define-actor.scm \
        tests/test-ghash.scm \
        tests/test-inbox.scm \
        tests/test-vat.scm \
        tests/test-repl.scm \
        tests/utils/test-base32.scm \
        tests/utils/test-bytes-stuff.scm \
        tests/utils/test-ring-buffer.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/goblins.texi
doc_goblins_TEXINFOS = doc/actor-lib-reference.texi	\
                       doc/persistence.texi	\
                       doc/api-reference.texi		\
                       doc/debugger.texi		\
                       doc/ocapn.texi			\
                       doc/apache-2.0.texi
infoimagedir = $(infodir)/images
dist_infoimage_DATA = doc/images/questie-lamport-diagram.png

dvi: # Don't build dvi docs
html-local:
	$(GUILE) --no-auto-compile doc/build-html.scm

run-tests:
	$(top_builddir)/pre-inst-env \
		$(GUILE) --no-auto-compile -L "$(top_srcdir)" \
			build-aux/run-test-suite.scm $(TESTS)

EXAMPLES = examples/cauldron.scm		\
           examples/try-captp-onion.scm		\
           examples/try-base-netlayer.scm	\
           examples/prelay-server.scm

EXTRA_DIST += README.org \
              README \
              HACKING \
              COPYING \
              LICENSE.txt \
              AUTHORS \
              NEWS \
              AUTHORS \
              ChangeLog \
              bootstrap.sh \
              guix.scm \
              coverage.scm \
              manifest.scm \
              live-guile.sh \
              .gitlab-ci.yml \
              build-aux/test-driver.scm \
              build-aux/run-test-suite.scm \
              doc/goblins.css \
              doc/build-html.scm \
              $(EXAMPLES) \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log) \
  $(TESTS:%.scm=%.xml)
