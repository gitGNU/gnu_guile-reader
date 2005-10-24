info_TEXINFOS = guile-reader.texi

EXTRA_DIST = extract-doc.scm


# Automatically extracted token reader documentation.
BUILT_SOURCES = token-reader-doc.texi

token-reader-doc.texi: $(top_srcdir)/token-readers.h
	./extract-doc.scm > $@
