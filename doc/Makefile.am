info_TEXINFOS = guile-reader.texi

EXTRA_DIST = extract-doc.scm extract-c-doc.scm.in


# Automatically extracted token reader documentation.
BUILT_SOURCES = version.texi					\
                token-reader-doc.texi reader-lib-doc.texi	\
		make-token-reader-doc.texi			\
		token-reader-api-doc.texi			\
		make-reader-doc.texi				\
		default-reader-doc.texi				\
		make-guile-reader-doc.texi

CLEANFILES = $(BUILT_SOURCES)

token-reader-doc.texi reader-lib-doc.texi: $(top_srcdir)/token-readers.h
	main='(module-ref (resolve-module '\''(extract-doc)) '\'main')' &&  \
	$(top_builddir)/pre-inst-guile -l $(srcdir)/extract-doc.scm	    \
	   -c "(apply $$main (cdr (command-line)))" $<

#
# Doc snarffed from C files.
#

make-token-reader-doc.texi: $(top_srcdir)/reader.c
	$(top_builddir)/doc/extract-c-doc.scm $^ make-token-reader > $@

token-reader-api-doc.texi: $(top_srcdir)/reader.c
	$(top_builddir)/doc/extract-c-doc.scm $^			\
	   standard-token-reader token-reader-specification	\
	   token-reader-procedure 'token-reader-escape?' > $@

make-reader-doc.texi: $(top_srcdir)/reader.c
	$(top_builddir)/doc/extract-c-doc.scm $^		\
	   make-reader %reader-standard-fault-handler > $@

default-reader-doc.texi: $(top_srcdir)/reader-lib.c
	$(top_builddir)/doc/extract-c-doc.scm $^ default-reader		\
		default-sharp-reader default-reader-token-readers	\
	 	default-sharp-reader-token-readers > $@

make-guile-reader-doc.texi: $(top_srcdir)/reader-lib.c
	$(top_builddir)/doc/extract-c-doc.scm $^ make-guile-reader > $@
