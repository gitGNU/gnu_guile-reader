/* A Scheme reader compiler for Guile.

   Copyright (C) 2005, 2006, 2007, 2010, 2012  Ludovic Courtès  <ludo@gnu.org>

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


/* This file contains a template for symbol token readers.  Several of them
   are instantiated from `token-readers.c'.  This is not as cool as a could
   have been a run-time `make-symbol-token-reader' but anyway.  Maybe someday
   a guy will launch a `guile-symbol-token-reader' project.  ;-)  */

#if (!defined SYMBOL_TR_NAME) || (!defined NUMBER_TR_NAME)
# error "This file should only be included from `token-readers.c'."
#endif


#include "compat.h"


/* Exponent markers, as defined in section 7.1.1 of R5RS, ``Lexical
   Structure''.  */
#define CHAR_IS_EXPONENT_MARKER(_chr)				\
  (((_chr) == 'e') || ((_chr) == 's') || ((_chr) == 'f')	\
   || ((_chr) == 'd') || ((_chr) == 'l'))

SCM
NUMBER_TR_NAME (int chr, SCM port, scm_reader_t scm_reader,
		scm_reader_t top_level_reader)
{
  SCM result, str = SCM_EOL;
  char local_buffer[SYMBOL_BUFFER_SIZE], *buffer;
  size_t bytes_read;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  scm_ungetc (chr, port);
  buffer = read_complete_token (port, local_buffer, sizeof local_buffer,
				DELIMITERS, &bytes_read);

  str = scm_from_stringn (buffer, bytes_read, pt->encoding, pt->ilseq_handler);

  result = scm_string_to_number (str, SCM_UNDEFINED);
  if (scm_is_false (result))
    {
      /* Return a symbol instead of a number */
      SYMBOL_TR_TRANSFORM_CHARACTERS (str);
      result = scm_string_to_symbol (str);
    }

  SCM_COL (port) += scm_c_string_length (str);
  return result;
}

#undef CHAR_IS_EXPONENT_MARKER

SCM
SYMBOL_TR_NAME (int chr, SCM port, scm_reader_t reader,
		scm_reader_t top_level_reader)
{
  SCM result;
  int ends_with_colon = 0;
  size_t bytes_read;
  int postfix = 0;				  /* XXX: keyword style */
  char local_buffer[SYMBOL_BUFFER_SIZE], *buffer;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  SCM str;

  scm_ungetc (chr, port);
  buffer = read_complete_token (port, local_buffer, sizeof local_buffer,
				DELIMITERS, &bytes_read);

  /* Are we actually reading a number rather than a symbol? */
  if (((buffer[0] == '+') || (buffer[0] == '-'))
      && ((isdigit (buffer[1])) || (tolower (buffer[1]) == 'i')))
    {
      /* Well, yes, this is a number:  call `scm_read_number ()' to
	 the rescue.  XXX:  This is a bit hackish, indeed.  */
      scm_unget_byte (buffer[1], port);
      return NUMBER_TR_NAME (chr, port, reader,
			     top_level_reader);
    }

  if (bytes_read > 0)
    ends_with_colon = buffer[bytes_read - 1] == ':';

  if (postfix && ends_with_colon && (bytes_read > 1))
    {
      str = scm_from_stringn (buffer, bytes_read - 1,
			      pt->encoding, pt->ilseq_handler);

      SYMBOL_TR_TRANSFORM_CHARACTERS (str);
      result = scm_symbol_to_keyword (scm_string_to_symbol (str));
    }
  else
    {
      str = scm_from_stringn (buffer, bytes_read,
			      pt->encoding, pt->ilseq_handler);

      SYMBOL_TR_TRANSFORM_CHARACTERS (str);
      result = scm_string_to_symbol (str);
    }

  SCM_COL (port) += scm_c_string_length (str);

#ifndef HAVE_SCM_GC_MALLOC_POINTERLESS
  if (buffer != local_buffer)
    free (buffer);
#endif

  return result;
}

/* arch-tag: 1fc534ea-5046-47ab-aa18-da259bbfa733
 */
