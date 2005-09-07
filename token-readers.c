/* A Scheme reader compiler for Guile.

   Copyright (C) 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>

   Part of the code in here (a few `scm_token_reader_t' functions below) is
   based on Guile's code (file `read.c') which contain the following
   copyright line:

   Copyright (C) 1995,1996,1997,1999,2000,2001,2003, 2004 Free Software
   Foundation, Inc.


   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


/* A library of stardard token readers that can be assembled to create a
   reader equivalent to that of Guile.  */

#include <libguile.h>
#include <ctype.h>
#include <strings.h>
#include <assert.h>

#include "reader.h"
#include "token-readers.h"



/* `isblank' is only in C99.  */
#define CHAR_IS_BLANK(_chr)					\
  (((_chr) == ' ') || ((_chr) == '\t') || ((_chr) == '\n'))

/* R5RS one-character tokens and delimiters (see section 7.1.1, ``Lexical
   structure'').  */
#define CHAR_IS_R5RS_DELIMITER(c)				\
  ((c == ')') || (c == '(') || (c == '\'') || (c == '`')	\
   || (c == '#') || (c == ',') || (c == ';') || (c == '"'))

/* Helper function similar to `scm_read_token ()'.  Read from PORT until a
   whitespace is read.  */
static __inline__ void
read_token (SCM port, char *buf, size_t buf_size, size_t *read)
{
  *read = 0;

  while (buf_size)
    {
      int chr;
      chr = scm_getc (port);
      if (chr == EOF)
	break;
      else if ((CHAR_IS_R5RS_DELIMITER (chr)) || (CHAR_IS_BLANK (chr)))
	/* (!isalnum (chr))  */ /* (CHAR_IS_BLANK (chr)) */
	{
	  scm_ungetc (chr, port);
	  break;
	}

      *(buf++) = (char)chr;
      (*read)++;

      if (*read == buf_size)
	break;
    }
}


/* Readers (mostly) stolen from Guile.  */


/* A simplified version of `scm_flush_ws ()' that makes no assumption about
   the comment syntax being used (in particular, it doesn't assume SCSH block
   comments).  */
static int
_flush_ws (SCM port, const char *eoferr)
{
  register int c;
  while (1)
    switch (c = scm_getc (port))
      {
      case EOF:
      goteof:
	if (eoferr)
	  {
	    scm_i_input_error (eoferr,
			       port,
			       "end of file",
			       SCM_EOL);
	  }
	return c;

      case ';':
      lp:
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    goto goteof;
	  default:
	    goto lp;
	  case SCM_LINE_INCREMENTORS:
	    break;
	  }
	break;

      case SCM_LINE_INCREMENTORS:
      case SCM_SINGLE_SPACES:
      case '\t':
	break;
      default:
	return c;
      }
}

#define scm_flush_ws _flush_ws

SCM
scm_read_sexp (int chr, SCM port, scm_reader_t scm_reader)
#define FUNC_NAME "scm_read_sexp"
{
  /* Note:  We rely here on the ability for SCM_READER to read `.' as a
     symbol (see SCM_SYM_DOT below).  */
  register int c;
  register SCM tmp;
  register SCM tl, tl2 = SCM_EOL;
  SCM ans, ans2 = SCM_EOL;
  /* Need to capture line and column numbers here. */
  int line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;

  c = scm_flush_ws (port, "scm_read_sexp");
  if (')' == c)
    return SCM_EOL;

  scm_ungetc (c, port);
  if (scm_is_eq (scm_sym_dot, (tmp = scm_call_reader (scm_reader, port, 0))))
    {
      ans = scm_call_reader (scm_reader, port, 1);
      if (')' != (c = scm_flush_ws (port, "scm_read_sexp")))
	scm_i_input_error (FUNC_NAME, port, "missing closing paren",
			   SCM_EOL);
      return ans;
    }

  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);
#if 0
  if (SCM_COPY_SOURCE_P)
    ans2 = tl2 = scm_cons (scm_is_pair (tmp)
			   ? tmp /* FIXME: was *copy */
			   : tmp,
			   SCM_EOL);
#endif

  while (')' != (c = scm_flush_ws (port, "scm_read_sexp")))
    {
      SCM new_tail;

      scm_ungetc (c, port);
      if (scm_is_eq (scm_sym_dot,
		     (tmp = scm_call_reader (scm_reader, port, 1))))
	{
	  SCM_SETCDR (tl, tmp = scm_call_reader (scm_reader, port, 0));
#if 0
	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (scm_is_pair (tmp)
				       ? tmp /* FIXME: Was *copy */
				       : tmp,
				       SCM_EOL));
#endif

	  if (')' != (c = scm_flush_ws (port, "scm_read_sexp")))
	    scm_i_input_error (FUNC_NAME, port,
			       "in pair:  missing closing paren", SCM_EOL);
	  goto exit;
	}

      if (tmp == SCM_UNSPECIFIED)
	/* SCM_READER read a character it does not handle.  This may be a
	   closing bracket so let's see.  */
	continue;

      new_tail = scm_cons (tmp, SCM_EOL);
      SCM_SETCDR (tl, new_tail);
      tl = new_tail;

#if 0
      if (SCM_COPY_SOURCE_P)
	{
	  SCM new_tail2 = scm_cons (scm_is_pair (tmp)
				    ? tmp /* FIXME: Was *copy */
				    : tmp, SCM_EOL);
	  SCM_SETCDR (tl2, new_tail2);
	  tl2 = new_tail2;
	}
#endif
    }
exit:
  scm_whash_insert (scm_source_whash,
		    ans,
		    scm_make_srcprops (line,
				       column,
				       SCM_FILENAME (port),
				       SCM_COPY_SOURCE_P
				       ? ans2 /* FIXME: Was *copy = ans2 */
				       : SCM_UNDEFINED,
				       SCM_EOL));

  return ans;
}
#undef FUNC_NAME

#undef scm_flush_ws

SCM
scm_read_string (int chr, SCM port, scm_reader_t scm_reader)
#define FUNC_NAME "scm_read_string"
{
  SCM tok_buf;
  char *str_buf;
  int c;
  unsigned j = 0;

  tok_buf = scm_i_make_string (256, &str_buf);
  while ('"' != (c = scm_getc (port)))
    {
      if (c == EOF)
	str_eof: scm_i_input_error (FUNC_NAME, port,
				    "end of file in string constant",
				    SCM_EOL);

      while (j + 2 >= scm_i_string_length (tok_buf))
	scm_grow_tok_buf (&tok_buf);

      if (c == '\\')
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    goto str_eof;
	  case '"':
	  case '\\':
	    break;
#if SCM_ENABLE_ELISP
	  case '(':
	  case ')':
	    if (SCM_ESCAPED_PARENS_P)
	      break;
	    goto bad_escaped;
#endif
	  case '\n':
	    continue;
	  case '0':
	    c = '\0';
	    break;
	  case 'f':
	    c = '\f';
	    break;
	  case 'n':
	    c = '\n';
	    break;
	  case 'r':
	    c = '\r';
	    break;
	  case 't':
	    c = '\t';
	    break;
	  case 'a':
	    c = '\007';
	    break;
	  case 'v':
	    c = '\v';
	    break;
	  case 'x':
	    {
	      int a, b;
	      a = scm_getc (port);
	      if (a == EOF) goto str_eof;
	      b = scm_getc (port);
	      if (b == EOF) goto str_eof;
	      if      ('0' <= a && a <= '9') a -= '0';
	      else if ('A' <= a && a <= 'F') a = a - 'A' + 10;
	      else if ('a' <= a && a <= 'f') a = a - 'a' + 10;
	      else goto bad_escaped;
	      if      ('0' <= b && b <= '9') b -= '0';
	      else if ('A' <= b && b <= 'F') b = b - 'A' + 10;
	      else if ('a' <= b && b <= 'f') b = b - 'a' + 10;
	      else goto bad_escaped;
	      c = a * 16 + b;
	      break;
	    }
	  default:
	  bad_escaped:
	    scm_i_input_error(FUNC_NAME, port,
			      "illegal character in escape sequence: ~S",
			      scm_list_1 (SCM_MAKE_CHAR (c)));
	  }
      scm_c_string_set_x (tok_buf, j, SCM_MAKE_CHAR (c));
      ++j;
    }
  if (j == 0)
    return scm_nullstr;

  /* Change this to scm_c_substring_read_only when
     SCM_STRING_CHARS has been removed.
  */
  return scm_c_substring_copy (tok_buf, 0, j);
}
#undef FUNC_NAME


SCM
scm_read_symbol (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;
  SCM result;
  char c_id[1024];
  size_t c_id_len = 0;

  result = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));
  c = chr;
  while (c != EOF)
    {
      /* Note: for pratical reasons (read: laziness), we exclude brackets
	 from the list of allowed characters for symbols.  */
      if ((isalnum (c) || (isgraph (c)))
	  && (c != '(') && (c != ')') && (c != '[') && (c != ']'))
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
	      && (isdigit (c_id[1])))
	    {
	      /* Well, yes, this is a number:  call `scm_read_number ()' to
		 the rescue.  XXX:  This is a bit hackish, indeed.  */
	      scm_ungetc (c_id[1], port);
	      return scm_read_number (chr, port, scm_reader);
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


SCM
scm_read_number_and_radix (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;
  char c_num[1024];
  SCM result_str;
  size_t c_num_len = 0;
  unsigned radix = 10;

  result_str = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));

  switch (chr)
    {
    case 'B':
    case 'b':
      radix = 2;
      break;

    case 'o':
    case 'O':
      radix = 8;
      break;

    case 'd':
    case 'D':
      radix = 10;
      break;

    case 'x':
    case 'X':
      radix = 16;
      break;

    case 'i':
    case 'I':
    case 'e':
    case 'E':
      /* FIXME */

    default:
      scm_i_input_error (__FUNCTION__, port,
			 "unknown number radix", SCM_EOL);
    }

  chr = scm_getc (port);
  c = chr;
  while (c != EOF)
    {
      if (((radix > 10) && (isxdigit (c)))
	  || (isdigit (c)))
	c_num[c_num_len++] = (char)c;
      else
	{
	  scm_ungetc (c, port);
	  break;
	}

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

  if (!c_num_len)
    scm_i_input_error(__FUNCTION__, port,
		      "invalid number syntax", SCM_EOL);

  result_str =
    scm_string_append (scm_list_2
		       (result_str,
			scm_from_locale_stringn (c_num, c_num_len)));

  return (scm_string_to_number (result_str, SCM_I_MAKINUM (radix)));
}

SCM
scm_read_number (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;
  SCM result, result_str;
  char c_num[1024];
  size_t c_num_len = 0;
  unsigned saw_point = 0, saw_plus_or_minus = 0, last_char_is_i = 0;
  unsigned return_symbol = 0;
  int sign = 1;

  if ((chr == '+') || (chr == '-'))
    {
      sign = (chr == '-') ? -1 : +1;
      c = chr = scm_getc (port);
    }

  result_str = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));
  c = chr;

  while (c != EOF)
    {
      if ((CHAR_IS_BLANK (c)) || (c == EOF)
	  || (CHAR_IS_R5RS_DELIMITER (c)))
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
      else if ((c == '+') || (c == '-'))
	{
	  if (saw_plus_or_minus)
	    /* Already seen a `+' or `-' before, so this can't be a
	       complex.  */
	    return_symbol = 1;
	  else
	    saw_plus_or_minus = 1;
	}
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

  if (saw_plus_or_minus)
    {
      if (last_char_is_i)
	/* Oh, this is a complex number!  */
	return_symbol = 0;
      else
	return_symbol = 1;
    }

  result_str =
    scm_string_append (scm_list_2
		       (result_str,
			scm_from_locale_stringn (c_num, c_num_len)));

  if (scm_c_string_length (result_str) == 0)
    scm_i_input_error(__FUNCTION__, port,
		      "invalid number syntax", SCM_EOL);

  if (return_symbol)
    /* The token wasn't actually a number so we'll return a symbol, just like
       Guile's default reader does (e.g. it reads `123.123.123' as a
       symbol).  */
    return (scm_string_to_symbol (result_str));

  result = scm_string_to_number (result_str, SCM_I_MAKINUM (10));
  if (sign < 0)
    result = scm_difference (SCM_I_MAKINUM (0), result);

  return result;
}

SCM
scm_read_quote (int chr, SCM port, scm_reader_t scm_reader)
{
  SCM p;

  switch (chr)
    {
    case '`':
      p = scm_sym_quasiquote;
      break;

    case '\'':
      p = scm_sym_quote;
      break;

    case ',':
      {
	int c;

	c = scm_getc (port);
	if ('@' == c)
	  p = scm_sym_uq_splicing;
	else
	  {
	    scm_ungetc (c, port);
	    p = scm_sym_unquote;
	  }
	break;
      }

    default:
      fprintf (stderr, "%s: unhandled quote character (%i)\n",
	       __FUNCTION__, chr);
      abort ();
    }

  p = scm_cons2 (p, scm_call_reader (scm_reader, port, 0), SCM_EOL);
  if (SCM_RECORD_POSITIONS_P)
    scm_whash_insert (scm_source_whash,
		      p,
		      scm_make_srcprops (SCM_LINUM (port),
					 SCM_COL (port) - 1,
					 SCM_FILENAME (port),
					 SCM_COPY_SOURCE_P
					 ? (/**copy = */
					    scm_cons2 (SCM_CAR (p),
						       SCM_CAR (SCM_CDR (p)),
						       SCM_EOL))
					 : SCM_UNDEFINED,
					 SCM_EOL));
  return p;
}

SCM
scm_read_semicolon_comment (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;

  for (c = scm_getc (port);
       (c != EOF) && (c != '\n');
       c = scm_getc (port));

  return SCM_UNSPECIFIED;
}


/* The hash syntax and its support functions.  */

/* Consume an SCSH-style block comment.  Assume that we've already read the
   initial `#!', and eat characters until we get an
   exclamation-point/sharp-sign sequence.  */
static void
skip_scsh_block_comment (SCM port)
{
  int bang_seen = 0;

  for (;;)
    {
      int c = scm_getc (port);

      if (c == EOF)
	scm_i_input_error ("skip_block_comment", port,
			   "unterminated `#! ... !#' comment", SCM_EOL);

      if (c == '!')
	bang_seen = 1;
      else if (c == '#' && bang_seen)
	return;
      else
	bang_seen = 0;
    }
}



#if 0 /* XXX This function was originally used for `read-hash-extend', but do
	 we care any longer?  */

/* Recover the read-hash procedure corresponding to char c.  */
static inline SCM
scm_get_hash_procedure (int c)
{
  /* FIXME:  SCM_READ_HASH_PROCEDURES is not public!  */
  return SCM_BOOL_F;
  SCM rest = *scm_read_hash_procedures;

  while (1)
    {
      if (scm_is_null (rest))
	return SCM_BOOL_F;
  
      if (SCM_CHAR (SCM_CAAR (rest)) == c)
	return SCM_CDAR (rest);
     
      rest = SCM_CDR (rest);
    }
}
#endif

#if 0  /* XXX This function is being replaced by `scm_sharp_reader'.  */
SCM
scm_read_sharp (int chr, SCM port, scm_reader_t scm_reader)
#define FUNC_NAME "scm_read_sharp"
{
  int c = scm_getc (port);

  {
    /* Check for user-defined hash procedure first, to allow
       overriding of builtin hash read syntaxes.  */
    SCM sharp = scm_get_hash_procedure (c);
    if (scm_is_true (sharp))
      {
	int line = SCM_LINUM (port);
	int column = SCM_COL (port) - 2;
	SCM got;

	got = scm_call_2 (sharp, SCM_MAKE_CHAR (c), port);
	if (scm_is_eq (got, SCM_UNSPECIFIED))
	  goto handle_sharp;
	if (SCM_RECORD_POSITIONS_P)
	  return /* *copy = */ recsexpr (got, line, column,
					 SCM_FILENAME (port));
	else
	  return got;
      }
  }
 handle_sharp:
  switch (c)
    {
      /* Vector, arrays, both uniform and not are handled by this
	 one function.  It also disambiguates between '#f' and
	 '#f32' and '#f64'.
      */
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case 'u': case 's': case 'f':
    case '@':
    case '(':
#if SCM_ENABLE_DEPRECATED
      /* See below for 'i' and 'e'. */
    case 'a':
    case 'c':
    case 'y':
    case 'h':
    case 'l':
#endif
      return scm_i_read_array (port, c);

    case 't':
    case 'T':
      return SCM_BOOL_T;

    case 'F':
      /* See above for lower case 'f'. */
      return SCM_BOOL_F;


    case 'i':
    case 'e':
#if SCM_ENABLE_DEPRECATED
      {
	/* When next char is '(', it really is an old-style
	   uniform array. */
	int next_c = scm_getc (port);
	if (next_c != EOF)
	  scm_ungetc (next_c, port);
	if (next_c == '(')
	  return scm_i_read_array (port, c);
	/* Fall through. */
      }
#endif  
    case 'b':
    case 'B':
    case 'o':
    case 'O':
    case 'd':
    case 'D':
    case 'x':
    case 'X':
    case 'I':
    case 'E':
      scm_ungetc (c, port);
      c = '#';
      /* The call below should yield a call to `scm_read_number ()' or some
	 such.  */
      return scm_call_reader (scm_reader, port);

    case '!':
      /* Only handle `#! ... !#' block comments if no user extension was
	 defined for `!' using `read-hash-extend'.  */
      skip_scsh_block_comment (port);
      return SCM_UNSPECIFIED;

#if 0 /* FIXME:  Adapt the code below.  */
    case '*':
      j = scm_read_token (c, tok_buf, port, 0);
      p = scm_istr2bve (scm_c_substring_shared (*tok_buf, 1, j));
      if (scm_is_true (p))
	return p;
      else
	goto unkshrp;

    case '{':
      j = scm_read_token (c, tok_buf, port, 1);
      return scm_string_to_symbol (scm_c_substring_copy (*tok_buf, 0, j));

    case '\\':
      c = scm_getc (port);
      j = scm_read_token (c, tok_buf, port, 0);
      if (j == 1)
	return SCM_MAKE_CHAR (c);
      if (c >= '0' && c < '8')
	{
	  /* Dirk:FIXME::  This type of character syntax is not R5RS
	   * compliant.  Further, it should be verified that the constant
	   * does only consist of octal digits.  Finally, it should be
	   * checked whether the resulting fixnum is in the range of
	   * characters.  */
	  p = scm_i_mem2number (scm_i_string_chars (*tok_buf), j, 8);
	  if (SCM_I_INUMP (p))
	    return SCM_MAKE_CHAR (SCM_I_INUM (p));
	}
      for (c = 0; c < scm_n_charnames; c++)
	if (scm_charnames[c]
	    && (scm_i_casei_streq (scm_charnames[c],
				   scm_i_string_chars (*tok_buf), j)))
	  return SCM_MAKE_CHAR (scm_charnums[c]);
      scm_i_input_error (FUNC_NAME, port, "unknown character name ~a",
			 scm_list_1 (scm_c_substring (*tok_buf, 0, j)));

#endif

      /* #:SYMBOL is a syntax for keywords supported in all contexts.  */
    case ':':
      return scm_symbol_to_keyword (scm_call_reader (scm_reader, port));

    default:
    callshrp:
      {
	SCM sharp = scm_get_hash_procedure (c);

	if (scm_is_true (sharp))
	  {
	    int line = SCM_LINUM (port);
	    int column = SCM_COL (port) - 2;
	    SCM got;

	    got = scm_call_2 (sharp, SCM_MAKE_CHAR (c), port);
	    if (scm_is_eq (got, SCM_UNSPECIFIED))
	      goto unkshrp;
	    if (SCM_RECORD_POSITIONS_P)
	      return /* *copy = */ recsexpr (got, line, column,
					     SCM_FILENAME (port));
	    else
	      return got;
	  }
      }
    unkshrp:
    scm_i_input_error (FUNC_NAME, port, "Unknown # object: ~S",
		       scm_list_1 (SCM_MAKE_CHAR (c)));
    }

  abort ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


/* Sharp readers, i.e. readers called after a `#' sign has been read.  */

SCM
scm_read_boolean (int chr, SCM port, scm_reader_t scm_reader)
{
  switch (chr)
    {
    case 't':
    case 'T':
      return SCM_BOOL_T;

    case 'f':
    case 'F':
      return SCM_BOOL_F;
    }

  return SCM_UNSPECIFIED;
}

SCM
scm_read_character (int chr, SCM port, scm_reader_t scm_reader)
{
  unsigned c;
  char charname[100];
  size_t charname_len;

  read_token (port, charname, sizeof (charname), &charname_len);

  if (charname_len == 1)
    return SCM_MAKE_CHAR (charname[0]);

  if (*charname >= '0' && *charname < '8')
    {
      /* Dirk:FIXME::  This type of character syntax is not R5RS
       * compliant.  Further, it should be verified that the constant
       * does only consist of octal digits.  Finally, it should be
       * checked whether the resulting fixnum is in the range of
       * characters.  */
      SCM p = scm_i_mem2number (charname, charname_len, 8);
      if (SCM_I_INUMP (p))
	return SCM_MAKE_CHAR (SCM_I_INUM (p));
    }

  for (c = 0; c < scm_n_charnames; c++)
    if (scm_charnames[c]
	&& (!strncasecmp (scm_charnames[c], charname, charname_len)))
      return SCM_MAKE_CHAR (scm_charnums[c]);

  scm_i_input_error (__FUNCTION__, port, "unknown character name ~a",
		     scm_list_1 (scm_from_locale_stringn (charname,
							  charname_len)));

  return SCM_UNSPECIFIED;
}

SCM
scm_read_keyword (int chr, SCM port, scm_reader_t scm_reader)
{
  /* Initially, I was tempted to call SCM_READER below, thinking that it was
     likely to result in a call to `scm_read_symbol ()'.  However,
     `scm_read_keyword ()' may typically be used either in the top-level
     reader or in the sharp reader.  In the latter case, calling SCM_READER
     just wouldn't work since the sharp reader doesn't handle symbols.  */
  return (scm_symbol_to_keyword (scm_read_symbol (scm_getc (port), port,
						  scm_reader)));
}

SCM
scm_read_srfi4_vector (int chr, SCM port, scm_reader_t scm_reader)
{
  return scm_i_read_array (port, chr);
}

SCM
scm_read_block_comment (int chr, SCM port, scm_reader_t scm_reader)
{
  skip_scsh_block_comment (port);
  return SCM_UNSPECIFIED;
}

SCM
scm_read_extended_symbol (int chr, SCM port, scm_reader_t scm_reader)
{
  /* Guile's extended symbol read syntax looks like this:

       #{This is all a symbol name}#

     So here, CHR is expected to be `{'.  */
  SCM result;
  int saw_brace = 0, finished = 0;
  size_t len = 0;
  char buf[1024];

  result = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));

  while ((chr = scm_getc (port)) != EOF)
    {
      if (saw_brace)
	{
	  if (chr == '#')
	    {
	      finished = 1;
	      break;
	    }
	  else
	    {
	      saw_brace = 0;
	      buf[len++] = '}';
	      buf[len++] = chr;
	    }
	}
      else if (chr == '}')
	saw_brace = 1;
      else
	buf[len++] = chr;

      if (len >= sizeof (buf) - 2)
	{
	  scm_string_append (scm_list_2 (result,
					 scm_from_locale_stringn (buf, len)));
	  len = 0;
	}

      if (finished)
	break;
    }

  if (len)
    result = scm_string_append (scm_list_2
				(result,
				 scm_from_locale_stringn (buf, len)));

  return (scm_string_to_symbol (result));
}


/* Skribe's read syntax.  So-called sk-exps.  */
SCM
scm_read_skribe_exp (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;
  char c_literal[1024];
  size_t c_literal_len = 0;
  SCM result = SCM_EOL;

  for (c = scm_getc (port);
       (c != EOF) && (c != ']');
       c = scm_getc (port))
    {
      switch (c)
	{
	case ',':
	  c = scm_getc (port);
	  if (c == '(')
	    {
	      SCM subexp;
	      result = scm_cons (scm_from_locale_stringn (c_literal,
							  c_literal_len),
				 result);
	      c_literal_len = 0;
	      scm_ungetc (c, port);
	      subexp = scm_cons2 (scm_sym_unquote,
				  scm_call_reader (scm_reader, port, 0),
				  SCM_EOL);
	      result = scm_cons (subexp, result);
	    }
	  else
	    {
	      c_literal[c_literal_len++] = ',';
	      if (c != EOF)
		c_literal[c_literal_len++] = (char)c;
	    }
	  break;

	default:
	  c_literal[c_literal_len++] = (char)c;
	}
    }

  if (c_literal_len)
    result = scm_cons (scm_from_locale_stringn (c_literal,
						c_literal_len),
		       result);

  result = scm_reverse_x (result, SCM_EOL);
  result = scm_cons2 (scm_sym_quasiquote, result, SCM_EOL);

  return result;
}


/* Directory of standard token readers.  */

#include <string.h>
/* #include "token-reader-lookup.c" */

const scm_token_reader_spec_t *
scm_token_reader_lookup (const char *name)
{
#if 0
  const struct scm_token_reader_entry *entry;

  entry = _scm_token_reader_lookup (name, strlen (name));
#endif

  const scm_token_reader_spec_t **group, *spec;
  const scm_token_reader_spec_t *specs[] =
    { scm_reader_standard_specs, scm_sharp_reader_standard_specs,
      scm_reader_misc_specs, NULL };

  for (group = specs; *group != NULL; group++)
    {
      for (spec = *group; spec->name != NULL; spec++)
	{
	  if (!strcmp (spec->name, name))
	    return (spec);
	}
    }

  return NULL;
}


/* Sample Scheme reader.  */

const scm_token_reader_spec_t scm_sharp_reader_standard_specs[] =
  {
    SCM_DEFTOKEN_SINGLE ('\\', "character",      scm_read_character, 0),
    SCM_DEFTOKEN_SET ("suf",   "srfi-4",         scm_read_srfi4_vector, 0),
    SCM_DEFTOKEN_SET ("ftTF",  "boolean",        scm_read_boolean, 0),
    SCM_DEFTOKEN_SINGLE (':',  "keyword",        scm_read_keyword, 0),
    SCM_DEFTOKEN_SET ("bBoOdDxXiIeE", "number+radix", scm_read_number_and_radix, 0),
    SCM_DEFTOKEN_SINGLE ('{',  "extended-symbol",scm_read_extended_symbol, 0),

    /* For block comments, we set the `escape' field to 1 so that the sharp
       reader will not loop when the block comment TR returns
       SCM_UNSPECIFIED.  This way, the sharp reader will return
       SCM_UNSPECIFIED to the top-level reader, which in turn will loop.  */
    SCM_DEFTOKEN_SINGLE ('!',  "block-comment",  scm_read_block_comment, 1),
    SCM_END_TOKENS
  };

/* A reader that must be compiled at initialization-time.  */
scm_reader_t scm_standard_sharp_reader = NULL;

/* A default, Scheme-like, reader specification.  */
scm_token_reader_spec_t scm_reader_standard_specs[] =
  {
    /* Whitespaces are defined using a NULL reader:  characters in this range
       are just ignored by the reader.  XXX:  The set itself cannot contain
       the null character.  */
    SCM_DEFTOKEN_RANGE ('\1', ' ', "whitespace", NULL, 0),

    SCM_DEFTOKEN_SINGLE ('(', "sexp",   scm_read_sexp, 0),
    SCM_DEFTOKEN_SINGLE ('"', "string", scm_read_string, 0),

    /* Both numbers and symbols can start with `+' or `-'.  */
    SCM_DEFTOKEN_RANGE ('0', '9', "number", scm_read_number, 0),

    /* Let's define symbols as two ranges plus one set of triggering
       characters.  Note that the sexp reader relies on the ability to read
       `.' as a symbol.  */
    SCM_DEFTOKEN_RANGE ('a', 'z', "symbol-lower-case", scm_read_symbol, 0),
    SCM_DEFTOKEN_RANGE ('A', 'Z', "symbol-upper-case", scm_read_symbol, 0),
    SCM_DEFTOKEN_SET (".+-/*%@_<>!=?$",
		      "symbol-misc-chars",  scm_read_symbol, 0),

    SCM_DEFTOKEN_SET ("'`,", "quote-quasiquote-unquote", scm_read_quote, 0),

    /* Although `:kw'-style keywords are not allowed by default in Guile (one
       must use `read-set!'), we'll make it the default here.  */
    SCM_DEFTOKEN_SINGLE (':', "colon-keyword", scm_read_keyword, 0),

    /* This one is defined at reader's compile-time.  */
    SCM_DEFTOKEN_SINGLE ('#', "sharp",                   NULL, 0),

    SCM_DEFTOKEN_SINGLE (';', "semicolon-comment",
			 scm_read_semicolon_comment, 0),

    SCM_END_TOKENS
  };

/* This is where we put non-standard token readers so that they can easily be
   looked up with `scm_token_reader_lookup ()'.  */
const scm_token_reader_spec_t scm_reader_misc_specs[] =
  {
    /* Skribe/Skribilo literal sequences.  In R6RS, `[' would rather be
       associated to `scm_read_sexp ()'.  */
    SCM_DEFTOKEN_SINGLE ('[', "skribe-exp", scm_read_skribe_exp, 0),

    SCM_END_TOKENS
  };



/* The standard reader (which depends on the standard keyword reader),
   compiled at initialization time.  */
scm_reader_t scm_standard_reader = NULL;

/* The standard fault handler proc.  */
SCM scm_reader_standard_fault_handler_proc = SCM_BOOL_F;


static char standard_reader_code[8000];
static char standard_sharp_reader_code[4000];

static SCM
scm_reader_standard_fault_handler (SCM chr, SCM port, SCM reader)
{
  scm_i_input_error ("%reader-standard-fault-handler",
		     port, "unhandled character: ~S", scm_list_1 (chr));
  return SCM_UNSPECIFIED;
}

void
scm_load_standard_reader (void)
{
  /* XXX  Ultimately, we might want to simply mmap a file containing the
     pre-compiled readers.  */

  size_t code_size = 0;

  if (scm_reader_standard_fault_handler_proc == SCM_BOOL_F)
    {
      scm_reader_standard_fault_handler_proc =
	scm_c_make_gsubr ("%reader-standard-fault-handler", 3, 0, 0,
			  scm_reader_standard_fault_handler);
      scm_permanent_object (scm_reader_standard_fault_handler_proc);
    }

  if (!scm_standard_sharp_reader)
    {
      scm_standard_sharp_reader =
	scm_c_make_reader (standard_sharp_reader_code,
			   sizeof (standard_sharp_reader_code),
			   scm_sharp_reader_standard_specs,
			   scm_reader_standard_fault_handler_proc, 0,
			   &code_size);
    }

  if (!scm_standard_reader)
    {
      /* Replace the sharp reader.  */
      scm_token_reader_spec_t *tr;
      for (tr = (scm_token_reader_spec_t *)scm_reader_standard_specs;
	   tr->name != 0;
	   tr++)
	{
	  if ((tr->token.type == SCM_TOKEN_SINGLE)
	      && (tr->token.value.single == '#'))
	    {
	      tr->reader.type = SCM_TOKEN_READER_READER;
	      tr->reader.value.reader = scm_standard_sharp_reader;
	      break;
	    }
	}

      /* We should not have reached the end of list.  */
      assert (tr->name);

      scm_standard_reader =
	scm_c_make_reader (standard_reader_code,
			   sizeof (standard_reader_code),
			   scm_reader_standard_specs,
			   scm_reader_standard_fault_handler_proc, 0,
			   &code_size);
    }
}
