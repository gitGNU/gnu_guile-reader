#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(guile-reader, 0.4, ludo@gnu.org)
AC_CONFIG_AUX_DIR(build-aux)

AM_INIT_AUTOMAKE([gnu no-define check-news])

AC_CONFIG_SRCDIR([src/reader.c])
AC_CONFIG_HEADER([config.h src/reader-config.h])

AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])
   

# Checks for programs.
AC_PROG_CC
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL

if test "x$GCC" = "xyes"; then
  # Enable useful GCC compilation flags.
  GCC_CFLAGS="-Wall"
  GCC_CFLAGS="-Wcast-align -Wpointer-arith $GCC_CFLAGS"
  GCC_CFLAGS="-Winline -Wdeclaration-after-statement $GCC_CFLAGS"
else
  GCC_CFLAGS=""
fi
AC_SUBST([GCC_CFLAGS])

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h strings.h])

LIGHTNING_CONFIGURE_IF_NOT_FOUND(
  [if test "x$GCC" = "xyes"; then
    # lightning's macros yield a lot of unused return values.  In addition,
    # `jit_get_ip ()' yields this warning:
    #
    #    dereferencing type-punned pointer will break strict-aliasing rules
    #
    # ... hence the `-fno-strict-aliasing'.
    GCC_CFLAGS="$GCC_CFLAGS -Wno-unused-value -fno-strict-aliasing"
   fi],
  [AC_MSG_WARN([GNU lightning not found.  Make sure it is not installable on your platform!])])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

AC_CHECK_SIZEOF([void *])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([strncasecmp])

# Gperf
AC_PATH_PROG([GPERF], [gperf], [not-found])
if test "x$GPERF" = "xnot-found"; then
   AC_MSG_ERROR([GNU Gperf not found.  Please install it first.])
fi

# Guile!
GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.8.x or later.])
fi
GUILE_SITE_DIR

clean_LDFLAGS="$LDFLAGS"
clean_CFLAGS="$CFLAGS"
LDFLAGS="$LDFLAGS $GUILE_LDFLAGS"
CFLAGS="$CFLAGS $GUILE_CFLAGS"

AC_CHECK_FUNC([scm_take_u8vector], [have_guile_1_8=yes], [have_guile_1_8=no])
if test "x$have_guile_1_8" != "xyes"; then
  AC_MSG_ERROR([You need Guile 1.8.x or higher.])
fi

# `scm_c_locale_stringn_to_number ()' was introduced after 1.8.0.  It is
# equivalent to the former `scm_i_mem2number ()'.
AC_CHECK_FUNCS([scm_c_locale_stringn_to_number scm_i_mem2number])

CFLAGS="$clean_CFLAGS"
LDFLAGS="$clean_LDFLAGS"


pkgdatadir="$datadir/$PACKAGE_NAME"
if test "x$guilemoduledir" = "x"; then
  guilemoduledir="$pkgdatadir"
fi
AC_SUBST([guilemoduledir])

if test "$guilemoduledir" != "$GUILE_SITE"; then
   # Guile won't be able to locate the module "out of the box", so
   # warn the user.
   AC_MSG_WARN([`guilemoduledir' ($guilemoduledir) is different from `GUILE_SITE' ($GUILE_SITE).])
   AC_MSG_WARN([Make sure to adjust the `GUILE_LOAD_PATH' environment variable accordingly,])
   AC_MSG_WARN([or re-run `configure' with `--with-guilemoduledir=$GUILE_SITE'.])
fi

AC_CONFIG_FILES([Makefile
		 pre-inst-guile
		 src/Makefile
		 tests/Makefile
	         module/Makefile
		 doc/Makefile])

AC_CONFIG_COMMANDS([pre-inst-guile-exec], [chmod +x pre-inst-guile])

AC_OUTPUT
