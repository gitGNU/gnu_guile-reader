lib_LTLIBRARIES = libguile-reader.la
libguile_reader_la_SOURCES = reader.c token-readers.c
libguile_reader_la_LDFLAGS = -module

readerincludedir = $(includedir)/guile-reader

moduledir = $(GUILE_SITE)
module_EXTRA = reader.scm
readerinclude_HEADERS = reader.h reader-config.h token-readers.h
BUILT_SOURCES = reader.c.x


EXTRA_DIST = test-repl.scm
TESTS = testsuite.scm

AM_CFLAGS = -Wall -g -O0

# Tell GCC to not complain about what Lightning does.
#libguile_reader_la_CFLAGS = -Wno-unused-value

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)


%.c.x: %.c
	guile-snarf-1.7 -o $@ $< $(snarfcppopts)

# Gperf experiments.
#BUILT_SOURCES += token-reader-lookup.c
#token-reader-lookup.c: $(libguile_reader_la_SOURCES)
#	./extract-token-readers.sh $^ | gperf -t -C -N _scm_token_reader_lookup > $@

