lib_LTLIBRARIES = libguile-dynamic-reader.la
libguile_dynamic_reader_la_SOURCES = reader.c token-readers.c
libguile_dynamic_reader_la_LDFLAGS = -module

moduledir = $(GUILE_SITE)
module_EXTRA = reader.scm
include_HEADERS = reader.h token-readers.h


EXTRA_DIST = test-repl.scm

AM_CFLAGS = -Wall -g -O0

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)


%.c.x: %.c
	guile-snarf-1.7 -o $@ $< $(snarfcppopts)

