/* A Scheme reader compiler for Guile.

   Copyright (C) 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>

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


/* This file contains a template for symbol token readers.  Several of them
   are instantiated from `token-readers.c'.  This is not as cool as a could
   have been a run-time `make-symbol-token-reader' but anyway.  Maybe someday
   a guy will launch a `guile-symbol-token-reader' project.  ;-)  */

#if (!defined SYMBOL_TR_NAME) || (!defined NUMBER_TR_NAME)
# error "This file should only be included from `token-readers.c'."
#endif


/* Exponent markers, as defined in section 7.1.1 of R5RS, ``Lexical
   Structure''.  */
#define CHAR_IS_EXPONENT_MARKER(_chr)				\
  (((_chr) == 'e') || ((_chr) == 's') || ((_chr) == 'f')	\
   || ((_chr) == 'd') || ((_chr) == 'l'))

SCM
NUMBER_TR_NAME (int chr, SCM port, scm_reader_t scm_reader,
		scm_reader_t top_level_reader)
{
  int c;
  SCM result, result_str;
  char c_num[1024];
  size_t c_num_len = 0;
  unsigned saw_point = 0, saw_plus_or_minus = 0, saw_leading_sign = 0;
  unsigned saw_exponent = 0, saw_at_sign = 0;
  unsigned last_char_is_i = 0;
  unsigned return_symbol = 0;

  result_str = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));
  c = chr;

  if ((c == '+') || (c == '-'))
    saw_leading_sign = 1;

  while (c != EOF)
    {
      if ((c == EOF) || (CHAR_IS_DELIMITER (c)))
	{
	  /* We need to special-case characters that cannot normally be part
	     of a symbol name.  There are actually few of them (see R5RS,
	     section 7.1.1, ``Lexical structure'').  */
	  if (c != EOF)
	    scm_ungetc (c, port);
	  break;
	}

      last_char_is_i = ((c == 'i') || (c == 'I')) ? 1 : 0;

      if (c == '.')
	{
	  if (saw_point)
	    /* We've already seen a point before.  */
	    return_symbol = 1;
	  else
	    saw_point = 1;
	}
      else if (c == '@')
	saw_at_sign++;
      else if ((c == '+') || (c == '-'))
	saw_plus_or_minus++;
      else if (CHAR_IS_EXPONENT_MARKER (c))
	saw_exponent++;
      else if (!isdigit (c))
	return_symbol = 1;

      c_num[c_num_len++] = (char)c;

      if (c_num_len + 1 >= sizeof (c_num))
	{
	  result_str =
	    scm_string_append (scm_list_2
			       (result_str,
				scm_from_locale_stringn (c_num, c_num_len)));
	  c_num_len = 0;
	}

      c = scm_getc (port);
    }

  if (last_char_is_i)
    {
      if (saw_plus_or_minus == 1)
	/* Oh, this is a complex number!  */
	return_symbol = 0;
      else
	return_symbol = 1;
    }
#if 0 /* Commented out: an complicated approach that tries to determine
	 whether we actually read a number or not.  We'd better let
	 `scm_string_to_number ()' do its job instead.  */
  else
    {
      if ((saw_plus_or_minus) && (!saw_leading_sign))
	return_symbol = 1;
      else if (saw_plus_or_minus > 1)
	return_symbol = 1;
      else if (saw_exponent > 1)
	return_symbol = 1;
      else if (saw_at_sign > 1)
	/* One `@' sign may be used to represent a complex number.  */
	return_symbol = 1;
    }
#endif

  result_str =
    scm_string_append (scm_list_2
		       (result_str,
			scm_from_locale_stringn (c_num, c_num_len)));

  if (scm_c_string_length (result_str) == 0)
    scm_i_input_error(__FUNCTION__, port,
		      "invalid number syntax", SCM_EOL);

  if (!return_symbol)
    {
      result = scm_string_to_number (result_str, SCM_I_MAKINUM (10));
      if (result == SCM_BOOL_F)
	/* We must have done something wrong: this must be a symbol rather
	   than a number.  */
	return_symbol = 1;
    }

  if (return_symbol)
    /* The token wasn't actually a number so we'll return a symbol, just like
       Guile's default reader does (e.g. it reads `123.123.123' as a
       symbol).  */
    return (scm_string_to_symbol (result_str));

  return result;
}

#undef CHAR_IS_EXPONENT_MARKER

SCM
SYMBOL_TR_NAME (int chr, SCM port, scm_reader_t reader,
		scm_reader_t top_level_reader)
{
  int c;
  SCM result;
  char c_id[1024];
  size_t c_id_len = 0;

  result = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));
  c = chr;
  while (c != EOF)
    {
      SYMBOL_TR_TRANSFORM_CHARACTER (c);

      if (!CHAR_IS_DELIMITER (c))
	c_id[c_id_len++] = (char)c;
      else
	{
	  scm_ungetc (c, port);
	  break;
	}

      if (c_id_len == 2)
	{
	  /* Are we actually reading a number rather than a symbol? */
	  if (((c_id[0] == '+') || (c_id[0] == '-'))
	      && ((isdigit (c_id[1])) || (tolower (c_id[1]) == 'i')))
	    {
	      /* Well, yes, this is a number:  call `scm_read_number ()' to
		 the rescue.  XXX:  This is a bit hackish, indeed.  */
	      scm_ungetc (c_id[1], port);
	      return NUMBER_TR_NAME (chr, port, reader,
				     top_level_reader);
	    }
	}

      if (c_id_len + 1 >= sizeof (c_id))
	{
	  result =
	    scm_string_append (scm_list_2
			       (result,
				scm_from_locale_stringn (c_id, c_id_len)));
	  c_id_len = 0;
	}

      c = scm_getc (port);
    }

  if (c_id_len)
    result =
      scm_string_append (scm_list_2
			 (result,
			  scm_from_locale_stringn (c_id, c_id_len)));

  return (scm_string_to_symbol (result));
}

/* arch-tag: 1fc534ea-5046-47ab-aa18-da259bbfa733
 */
