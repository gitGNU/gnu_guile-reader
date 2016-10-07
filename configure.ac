#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(guile-reader, 0.6.1, guile-reader-devel@nongnu.org)
AC_CONFIG_AUX_DIR(build-aux)

AM_INIT_AUTOMAKE([1.11 color-tests parallel-tests silent-rules \
  gnu no-define check-news -Wall -Werror])

AC_CONFIG_SRCDIR([src/reader.c])
AC_CONFIG_HEADER([config.h src/reader-config.h])

AC_ARG_WITH([guilemoduledir],
  [AS_HELP_STRING([--with-guilemoduledir=DIR],
    [install Guile modules under DIR])],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])
   

# Checks for programs.
AC_PROG_CC
AC_PROG_SED
AC_PROG_MKDIR_P
AM_PROG_AR

# Libtool.
LT_PREREQ([2.2.6])
LT_INIT([disable-static dlopen])

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

# GNU lightning.

# We don't bundle GNU lightning with Guile-Reader, as `lightningize'
# would do.  Instead we simply use the available version of lightning,
# if any.  This rules out use of `LIGHTNING_CONFIGURE_IF_NOT_FOUND'.
AC_CHECK_HEADER([lightning.h], [have_lightning=yes], [have_lightning=no])
if test "x$have_lightning" = "xyes"; then
   if test "x$GCC" = "xyes"; then
      # lightning's macros yield a lot of unused return values.  In addition,
      # `jit_get_ip ()' yields this warning:
      #
      #    dereferencing type-punned pointer will break strict-aliasing rules
      #
      # ... hence the `-fno-strict-aliasing'.
      GCC_CFLAGS="$GCC_CFLAGS -Wno-unused-value -fno-strict-aliasing"
   fi

   dnl The following test fails with lightning version < 1.2b and with
   dnl 2.x, which is not supported (yet?).
   AC_MSG_CHECKING([whether GNU lightning supports `jit_allocai'])
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM([[#include <lightning.h>]],
                      [[int x = jit_allocai (7);]])],
     [AC_MSG_RESULT([yes])],
     [AC_MSG_RESULT([no])
      AC_MSG_WARN([GNU lightning is too old or too recent (try version 1.2); not using it])
      have_lightning=no])
else
  AC_MSG_WARN([GNU lightning not found; GNU lightning enhancements not compiled.])
fi
if test "x$have_lightning" = "xyes"; then
   AC_DEFINE([HAVE_LIGHTNING], [1], [Define if GNU lightning can be used])
fi

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
GUILE_PKG([2.2 2.0 1.8])
GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.8.x or later.])
fi
AC_PATH_PROGS([GUILD], [guild guile-tools])
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

# `scm_i_' functions may eventually be no longer exported, so avoid
# relying on it.
AC_CHECK_FUNCS([scm_i_input_error scm_i_read_array])

# These symbols (which are not actual functions) vanished in 1.9.3.
AC_CHECK_FUNCS([scm_charnames scm_charnums])

# Functions for Unicode support that appeared in Guile 2.0.
AC_CHECK_FUNCS([scm_from_stringn scm_get_byte_or_eof \
  scm_unget_byte scm_from_utf32_stringn scm_gc_malloc_pointerless])

AM_CONDITIONAL([HAVE_GUILE2], [test "x$ac_cv_func_scm_from_utf32_stringn" = "xyes"])

# This type appeared in Guile 2.0.
AC_CHECK_TYPE([scm_t_wchar],
  [AC_DEFINE([SCM_READER_HAVE_SCM_T_WCHAR], [1],
    [Define if the `scm_t_wchar' type exists.])], [:],
  [#include <libguile.h>])

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
	         modules/Makefile
		 doc/Makefile])

AC_CONFIG_COMMANDS([pre-inst-guile-exec], [chmod +x pre-inst-guile])

AC_OUTPUT
