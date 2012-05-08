EXTRA_DIST = system/reader.in

nobase_dist_guilemodule_DATA =			\
  system/reader/confinement.scm			\
  system/reader/compat.scm			\
  system/reader/library.scm			\
  system/documentation/output.scm		\
  system/documentation/c-snarf.scm

nobase_nodist_guilemodule_DATA =		\
  system/reader.scm


.in.scm:
	$(MKDIR_P) `dirname "$@"`
	$(SED) -e 's|[@]libdir[@]|$(libdir)|g' < "$<" > "$@.tmp"
	mv "$@.tmp" "$@"

if HAVE_GUILE2

nobase_nodist_guilemodule_DATA +=		\
  $(nobase_dist_guilemodule_DATA:%.scm=%.go)	\
  system/reader.go

GUILE_WARNINGS =				\
  -Wunbound-variable -Warity-mismatch -Wformat	\
  -Wunused-variable

.scm.go:
	$(MKDIR_P) `dirname "$@"`
	GUILE_AUTO_COMPILE=0					\
	GUILE_READER_LIBDIR="$(abs_top_builddir)/src"		\
	$(GUILD) compile --target="$(host)" $(GUILE_WARNINGS)	\
	  -L "$(abs_builddir)" -L "$(abs_srcdir)"		\
	  -o "$@" "$<"

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guilereader_install_go_files = install-nobase_nodist_guilemoduleDATA
$(guilereader_install_go_files): install-nobase_dist_guilemoduleDATA

endif HAVE_GUILE2

CLEANFILES = $(nobase_nodist_guilemodule_DATA)
