EXTRA_DIST = system/reader.in

nobase_dist_guilemodule_DATA =			\
  system/reader/confinement.scm			\
  system/reader/compat.scm			\
  system/reader/library.scm			\
  system/documentation/output.scm		\
  system/documentation/c-snarf.scm

nobase_nodist_guilemodule_DATA =		\
  system/reader.scm


CLEANFILES = system/reader.scm

.in.scm:
	$(MKDIR_P) `dirname "$@"`
	$(SED) -e 's|[@]libdir[@]|$(libdir)|g' < "$<" > "$@.tmp"
	mv "$@.tmp" "$@"
