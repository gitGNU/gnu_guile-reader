lib_LTLIBRARIES = libguile-reader.la
libguile_reader_la_SOURCES = reader.c token-readers.c
libguile_reader_la_LDFLAGS = -module

moduledir = $(GUILE_SITE)
module_EXTRA = reader.scm
include_HEADERS = reader.h token-readers.h


EXTRA_DIST = test-repl.scm

AM_CFLAGS = -Wall -g -O0

# Tell GCC to not complain about what Lightning does.
reader_c_CFLAGS = -Wno-unused-value

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)


%.c.x: %.c
	guile-snarf-1.7 -o $@ $< $(snarfcppopts)

# Gperf experiments.
#BUILT_SOURCES = token-reader-lookup.c
#token-reader-lookup.c: $(libguile_reader_la_SOURCES)
#	./extract-token-readers.sh $^ | gperf -t -C -N _scm_token_reader_lookup > $@

