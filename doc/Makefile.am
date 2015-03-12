info_TEXINFOS = guile-reader.texi

EXTRA_DIST = extract-doc.scm extract-c-doc.scm

GUILE_FOR_BUILD =				\
  GUILE_AUTO_COMPILE=0				\
  $(GUILE) -q -L $(top_srcdir)/modules

# Basic command to run the C doc extractor.
extract_c_doc =							\
  $(GUILE_FOR_BUILD) -l "$(srcdir)/extract-c-doc.scm"		\
	             -e '(apply main (cdr (command-line)))'	\
		     --

AM_CPPFLAGS = -I$(top_builddir) -I$(top_srcdir)		\
	      -I$(top_srcdir)/src -I$(top_builddir)/src	\
              $(CPPFLAGS) $(GUILE_CFLAGS)

# Automatically extracted token reader documentation.
BUILT_SOURCES = version.texi					\
                token-reader-doc.texi reader-lib-doc.texi	\
		make-token-reader-doc.texi			\
		token-reader-api-doc.texi			\
		make-reader-doc.texi				\
		default-reader-doc.texi				\
		make-guile-reader-doc.texi

CLEANFILES = $(BUILT_SOURCES)

token-reader-doc.texi reader-lib-doc.texi: $(top_srcdir)/src/token-readers.h
	main='(module-ref (resolve-module '\''(extract-doc)) '\'main')' &&	\
	GUILE_AUTO_COMPILE=0							\
	$(top_builddir)/pre-inst-guile -l $(srcdir)/extract-doc.scm		\
	   -c "(apply $$main (cdr (command-line)))" $<

#
# Doc snarffed from C files.
#

make-token-reader-doc.texi: $(top_srcdir)/src/reader.c
	$(extract_c_doc) "$^" "$(CPP)" "$(AM_CPPFLAGS)" \
		make-token-reader > $@

token-reader-api-doc.texi: $(top_srcdir)/src/reader.c
	$(extract_c_doc) "$^" "$(CPP)" "$(AM_CPPFLAGS)"		\
	   standard-token-reader token-reader-specification	\
	   token-reader-procedure 'token-reader-escape?' > $@

make-reader-doc.texi: $(top_srcdir)/src/reader.c
	$(extract_c_doc) "$^" "$(CPP)" "$(AM_CPPFLAGS)"		\
	   make-reader %reader-standard-fault-handler > $@

default-reader-doc.texi: $(top_srcdir)/src/reader-lib.c
	$(extract_c_doc) "$^" "$(CPP)" "$(AM_CPPFLAGS)"			\
		default-sharp-reader default-reader-token-readers	\
	 	default-sharp-reader-token-readers > $@

make-guile-reader-doc.texi: $(top_srcdir)/src/reader-lib.c
	$(extract_c_doc) "$^" "$(CPP)" "$(AM_CPPFLAGS)" \
		make-guile-reader > $@
