info_TEXINFOS = guile-reader.texi

EXTRA_DIST = extract-doc.scm


# Automatically extracted token reader documentation.
BUILT_SOURCES = token-reader-doc.texi reader-lib-doc.texi version.texi

token-reader-doc.texi reader-lib-doc.texi: $(top_srcdir)/token-readers.h
	main='(module-ref (resolve-module '\''(extract-doc)) '\'main')' &&  \
	$(top_builddir)/pre-inst-guile -l $(srcdir)/extract-doc.scm	    \
	   -c "(apply $$main (cdr (command-line)))" $<
