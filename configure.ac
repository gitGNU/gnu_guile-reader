#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(guile-reader, 0.1, ludovic.courtes@laas.fr)
AM_INIT_AUTOMAKE(guile-reader, 0.1)

AC_CONFIG_SRCDIR([reader.c])
AC_CONFIG_HEADER([config.h reader-config.h])

# Checks for programs.
AC_PROG_CC
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h strings.h lightning.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([strncasecmp])

# Guile!
GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.7.x or later.])
fi
GUILE_SITE_DIR

AC_CONFIG_FILES([Makefile
	         module/Makefile
		 module/system/Makefile
		 doc/Makefile])

AC_OUTPUT
