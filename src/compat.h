/* A Scheme reader compiler for Guile.

   Copyright (C) 2006, 2008, 2012, 2016 Ludovic Courtès <ludo@gnu.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA  */
#ifndef GUILE_READER_COMPAT_H
#define GUILE_READER_COMPAT_H

/* Compatibility tricks among Guile versions.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <reader-config.h>

#ifndef HAVE_SCM_C_LOCALE_STRINGN_TO_NUMBER
# ifdef HAVE_SCM_I_MEM2NUMBER
#  define scm_c_locale_stringn_to_number scm_i_mem2number
# else
#  error "`scm_i_mem2number ()' was not found."
# endif
#endif

#ifndef HAVE_SCM_GET_BYTE_OR_EOF

extern int scm_get_byte_or_eof (SCM port);
extern void scm_unget_byte (int c, SCM port);

#endif

#ifndef HAVE_SCM_FROM_STRINGN
# define scm_from_stringn(buf, count, enc, handler)	\
    scm_from_locale_stringn ((buf), (count))
#endif

#ifndef HAVE_SCM_FROM_UTF32_STRINGN

extern SCM scm_from_utf32_stringn (const scm_t_wchar *str, size_t len);

#endif

extern void increase_port_column (SCM port, size_t increment);


/* Guile >= 2.1.4 has a new port API.  */
#ifndef SCM_PORT
# define SCM_PORT(port)  (SCM_PTAB_ENTRY (port))
# define PORT_ENCODING(port)  (SCM_PTAB_ENTRY (port)->encoding)
# define PORT_CONVERSION_STRATEGY(port) (SCM_PTAB_ENTRY (port)->ilseq_handler)
#else

extern const char *port_encoding (SCM port);
extern scm_t_string_failed_conversion_handler
port_conversion_strategy (SCM port);

# define PORT_ENCODING(port)  (port_encoding (port))
# define PORT_CONVERSION_STRATEGY(port) (port_conversion_strategy (port))
#endif

#endif
