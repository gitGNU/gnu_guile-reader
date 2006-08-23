lib_LTLIBRARIES = libguile-reader.la
libguile_reader_la_SOURCES = reader.c token-readers.c reader-lib.c
libguile_reader_la_LDFLAGS = -module -version-info 1:0:0

readerincludedir = $(includedir)/guile-reader

readerinclude_HEADERS = reader.h reader-config.h	\
			token-readers.h reader-lib.h
BUILT_SOURCES = token-reader-lookup.c token-reader-list.c	\
		make-reader-flags.c				\
                reader-lib.c.x token-readers.c.x reader.c.x
CLEANFILES = $(BUILT_SOURCES)

EXTRA_DIST = symbol-token-reader.c example-repl.scm $(TESTS)		\
	     extract-token-readers.sh extract-make-reader-flags.sh	\
	     compat.h

# Some header files are generated (in builddir) and others are in srcdir.
AM_CFLAGS = -I$(top_srcdir) -I$(top_builddir)

AM_CFLAGS += -Wmissing-prototypes -Wall -g -O0

if HAVE_INSTALLED_LIGHTNING

# Tell GCC to not complain about what Lightning does.
libguile_reader_la_CFLAGS = -Wno-unused-value

endif

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(AM_CFLAGS)


%.c.x: %.c
	$(guile_snarf) -o $@ $< $(snarfcppopts)

%.doc: %.c
	$(CPP) $(CPPFLAGS) -DSCM_MAGIC_SNARF_DOCS $< > $@

# Token reader lookup.
token-reader-lookup.c: $(top_srcdir)/token-readers.h
	cat $< | \
	$(top_srcdir)/extract-token-readers.sh | \
	$(GPERF) -t -C -N _scm_token_reader_lookup > $@

token-reader-list.c: $(top_srcdir)/token-readers.h
	cat $< | \
	$(top_srcdir)/extract-token-readers.sh --no-gperf-header | \
	cut -f 1 -d , | sort | sed -e's/^\(.*\)$$/"\1",/g' > $@

# `make-reader' flags.
make-reader-flags.c: $(top_srcdir)/reader.h
	cat $< | \
	$(top_srcdir)/extract-make-reader-flags.sh | \
	$(GPERF) -t -C -N _scm_to_make_reader_flag > $@

# Jump to the sub-directories.
SUBDIRS = . module doc tests
