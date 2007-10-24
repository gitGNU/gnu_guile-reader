lib_LTLIBRARIES = libguile-reader.la
libguile_reader_la_SOURCES = reader.c token-readers.c reader-lib.c
libguile_reader_la_LDFLAGS = -module -version-info 1:0:0 $(GUILE_LDFLAGS)

readerincludedir = $(includedir)/guile-reader

readerinclude_HEADERS = reader.h reader-config.h	\
			token-readers.h reader-lib.h
BUILT_SOURCES = token-reader-lookup.c token-reader-list.c	\
		make-reader-flags.c				\
                reader-lib.x token-readers.x reader.x
CLEANFILES = $(BUILT_SOURCES)

EXTRA_DIST = symbol-token-reader.c example-repl.scm $(TESTS)		\
	     extract-token-readers.sh extract-make-reader-flags.sh	\
	     compat.h

# Some header files are generated (in builddir) and others are in srcdir.
AM_CPPFLAGS = -I$(srcdir) -I$(builddir) -I$(top_builddir)
AM_CFLAGS   = $(GCC_CFLAGS)


snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(AM_CPPFLAGS)


.c.x:
	$(guile_snarf) -o $@ $< $(snarfcppopts)


# Token reader lookup.
token-reader-lookup.c: $(srcdir)/token-readers.h
	cat $< | \
	$(srcdir)/extract-token-readers.sh | \
	$(GPERF) -t -C -N _scm_token_reader_lookup > $@

token-reader-list.c: $(srcdir)/token-readers.h
	cat $< | \
	$(srcdir)/extract-token-readers.sh --no-gperf-header | \
	cut -f 1 -d , | sort | sed -e's/^\(.*\)$$/"\1",/g' > $@

# `make-reader' flags.
make-reader-flags.c: $(srcdir)/reader.h
	cat $< | \
	$(srcdir)/extract-make-reader-flags.sh | \
	$(GPERF) -t -C -N _scm_to_make_reader_flag > $@
