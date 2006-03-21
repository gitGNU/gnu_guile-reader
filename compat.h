/* A Scheme reader compiler for Guile.

   Copyright (C) 2006  Ludovic Courtès  <ludovic.courtes@laas.fr>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA  */
#ifndef __GUILE_READER_COMPAT_H__
#define __GUILE_READER_COMPAT_H__

/* Compatibility tricks among Guile versions.  */

#include "config.h"

#ifndef HAVE_SCM_C_LOCALE_STRINGN_TO_NUMBER
# ifdef HAVE_SCM_I_MEM2NUMBER
#  define scm_c_locale_stringn_to_number scm_i_mem2number
# else
#  error "`scm_i_mem2number ()' was not found."
# endif
#endif

#endif

/* arch-tag: d7a0f4a6-4480-40b4-a5d3-ed3dbebae0fa
 */
