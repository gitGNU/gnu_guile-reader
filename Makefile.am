SUBDIRS = module doc

lib_LTLIBRARIES = libguile-reader.la
libguile_reader_la_SOURCES = reader.c token-readers.c
libguile_reader_la_LDFLAGS = -module -interface 0:0:0

readerincludedir = $(includedir)/guile-reader

readerinclude_HEADERS = reader.h reader-config.h token-readers.h
BUILT_SOURCES = reader.c.x

# We build a `guile' executable here so that it is linked against
# `libguile-reader' and Libtool's magic ensures that we can use it to
# run the test suite.
noinst_PROGRAMS = guile
guile_LDFLAGS = -dlpreopen libguile-reader.la $(GUILE_LDFLAGS)

TESTS = testsuite.scm
EXTRA_DIST = test-repl.scm $(TESTS)

AM_CFLAGS = -Wall -g -O0

# Tell GCC to not complain about what Lightning does.
#libguile_reader_la_CFLAGS = -Wno-unused-value

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)


%.c.x: %.c
	guile-snarf-1.7 -o $@ $< $(snarfcppopts)

%.doc: %.c
	$(CPP) $(CPPFLAGS) -DSCM_MAGIC_SNARF_DOCS $< > $@

# Gperf experiments.
#BUILT_SOURCES += token-reader-lookup.c
#token-reader-lookup.c: $(libguile_reader_la_SOURCES)
#	./extract-token-readers.sh $^ | gperf -t -C -N _scm_token_reader_lookup > $@

