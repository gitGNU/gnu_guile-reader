SUBDIRS = module doc

lib_LTLIBRARIES = libguile-reader.la
libguile_reader_la_SOURCES = reader.c token-readers.c reader-lib.c
libguile_reader_la_LDFLAGS = -module -version-info 1:0:0

readerincludedir = $(includedir)/guile-reader

readerinclude_HEADERS = reader.h reader-config.h \
			token-readers.h reader-lib.h
BUILT_SOURCES = reader-lib.c.x token-readers.c.x reader.c.x

# We build a `guile' executable here so that it is linked against
# `libguile-reader' and Libtool's magic ensures that we can use it to
# run the test suite.
noinst_PROGRAMS = guile
guile_LDFLAGS = -dlpreopen libguile-reader.la $(GUILE_LDFLAGS)

TESTS = testsuite.scm
EXTRA_DIST = symbol-token-reader.c test-repl.scm $(TESTS)

AM_CFLAGS = -Wmissing-prototypes -Wall -g -O0

if HAVE_INSTALLED_LIGHTNING

# Tell GCC to not complain about what Lightning does.
libguile_reader_la_CFLAGS = -Wno-unused-value

endif

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)


%.c.x: %.c
	guile-snarf-1.7 -o $@ $< $(snarfcppopts)

%.doc: %.c
	$(CPP) $(CPPFLAGS) -DSCM_MAGIC_SNARF_DOCS $< > $@

# Token reader lookup.
BUILT_SOURCES += token-reader-lookup.c token-reader-list.c
token-reader-lookup.c: token-readers.h
	./extract-token-readers.sh | \
	gperf -t -C -N _scm_token_reader_lookup > $@

token-reader-list.c: token-readers.h
	./extract-token-readers.sh --no-gperf-header | \
	cut -f 1 -d , | sort | sed -e's/^\(.*\)$$/"\1",/g' > $@
