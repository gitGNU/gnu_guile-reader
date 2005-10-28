/* A Scheme reader compiler for Guile.

   Copyright (C) 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>

   Part of the code here (a few `scm_token_reader_t' functions below) is
   based on Guile code released under the GNU LGPL (file `read.c') which
   contains the following copyright line:

   Copyright (C) 1995,1996,1997,1999,2000,2001,2003, 2004 Free Software
   Foundation, Inc.


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


/* A library of stardard token readers that can be assembled to create a
   reader equivalent to that of Guile.  */

#include <libguile.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <assert.h>

#include "reader.h"
#include "token-readers.h"

#include "config.h"



/* `isblank' is only in C99.  */
#define CHAR_IS_BLANK(_chr)					\
  (((_chr) == ' ') || ((_chr) == '\t') || ((_chr) == '\n'))

/* R5RS one-character delimiters (see section 7.1.1, ``Lexical
   structure'').  */
#define CHAR_IS_R5RS_DELIMITER(c)				\
  (CHAR_IS_BLANK (c)						\
   || (c == ')') || (c == '(') || (c == ';') || (c == '"'))

#define CHAR_IS_R6RS_DELIMITER(c)				\
  (CHAR_IS_R5RS_DELIMITER (c) || (c == '[') || (c == ']'))


/* Helper function similar to `scm_read_token ()'.  Read from PORT until a
   whitespace is read.  */
static inline void
read_token (SCM port, char *buf, size_t buf_size, size_t *read)
{
  *read = 0;

  while (buf_size)
    {
      int chr;
      chr = scm_getc (port);
      if (chr == EOF)
	break;
      else if (CHAR_IS_R5RS_DELIMITER (chr))
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
scm_read_sexp (int chr, SCM port, scm_reader_t reader,
	       scm_reader_t top_level_reader)
#define FUNC_NAME "scm_read_sexp"
{
  /* Note:  We rely here on the ability for SCM_READER to read `.' as a
     symbol (see SCM_SYM_DOT below).  */
  register int c;
  register SCM tmp;
  register SCM tl, tl2 = SCM_EOL;
  SCM ans, ans2 = SCM_EOL;
  int terminating_char = 0;

  switch (chr)
    {
      /* We allow for several ways to write S-exps, notably `[' which is
	 going to be supported by R6RS.  */
    case '(':
      terminating_char = ')';
      break;

    case '[':
      terminating_char = ']';
      break;

    case '{':
      terminating_char = '}';
      break;

    default:
      scm_ungetc (chr, port);
      scm_i_input_error (FUNC_NAME, port, "invalid list starting character",
			 scm_list_1 (SCM_MAKE_CHAR (chr)));
      return SCM_BOOL_F;
    }

  c = scm_flush_ws (port, "scm_read_sexp");
  if (terminating_char == c)
    return SCM_EOL;

  scm_ungetc (c, port);
  if (scm_is_eq (scm_sym_dot,
		 (tmp = scm_call_reader (top_level_reader, port, 0,
					 top_level_reader))))
    {
      ans = scm_call_reader (top_level_reader, port, 1, top_level_reader);
      if (terminating_char != (c = scm_flush_ws (port, "scm_read_sexp")))
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

  while (terminating_char != (c = scm_flush_ws (port, "scm_read_sexp")))
    {
      SCM new_tail;

      scm_ungetc (c, port);
      if (scm_is_eq (scm_sym_dot,
		     (tmp = scm_call_reader (top_level_reader, port, 1,
					     top_level_reader))))
	{
	  SCM_SETCDR (tl, tmp = scm_call_reader (top_level_reader, port, 0,
						 top_level_reader));
#if 0
	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (scm_is_pair (tmp)
				       ? tmp /* FIXME: Was *copy */
				       : tmp,
				       SCM_EOL));
#endif

	  if (terminating_char != (c = scm_flush_ws (port, "scm_read_sexp")))
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

  return ans;
}
#undef FUNC_NAME

#undef scm_flush_ws

SCM
scm_read_string (int chr, SCM port, scm_reader_t scm_reader,
		 scm_reader_t top_level_reader)
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


/* Produce several flavors of symbol and number token readers: with or
   without case-sensitivity, and with various sets of allowed characters in a
   symbol name.  Note that the ``number TR'' may as well return a symbol, in
   cases like `123.123.123'.

   Both the symbol TR and the number TR rely on the definition of delimiter
   characters which varies, for instance, between R5RS and R6RS.  So we'll
   generate both of them here.  */


#define SYMBOL_TR_NAME scm_read_guile_mixed_case_symbol
#define NUMBER_TR_NAME scm_read_guile_number
#define SYMBOL_TR_TRANSFORM_CHARACTER(_c)  do {} while (0)
#define CHAR_IS_DELIMITER(_c)   (CHAR_IS_R5RS_DELIMITER (_c))

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTER
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME

/* FIXME: Check whether this really corresponds to R5RS.  */

#define SYMBOL_TR_NAME scm_read_r5rs_lower_case_symbol
#define NUMBER_TR_NAME scm_read_r5rs_lower_case_number
#define SYMBOL_TR_TRANSFORM_CHARACTER(_c)	\
  do { (_c) = tolower (_c); } while (0)

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTER
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME

#define SYMBOL_TR_NAME scm_read_r5rs_upper_case_symbol
#define NUMBER_TR_NAME scm_read_r5rs_upper_case_number
#define SYMBOL_TR_TRANSFORM_CHARACTER(_c)	\
  do { (_c) = toupper (_c); } while (0)

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTER
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME
#undef CHAR_IS_DELIMITER

/* FIXME: Check whether this really corresponds to R6RS.  */

#define SYMBOL_TR_NAME scm_read_r6rs_symbol
#define NUMBER_TR_NAME scm_read_r6rs_number
#define SYMBOL_TR_TRANSFORM_CHARACTER(_c)  do {} while (0)
#define CHAR_IS_DELIMITER(_c)   (CHAR_IS_R6RS_DELIMITER (_c))
#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTER
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME
#undef CHAR_IS_DELIMITER

#define SYMBOL_TR_NAME scm_read_brace_free_symbol
#define NUMBER_TR_NAME scm_read_brace_free_number
#define SYMBOL_TR_TRANSFORM_CHARACTER(_c)  do {} while (0)
#define CHAR_IS_DELIMITER(_c)			\
  ((CHAR_IS_R5RS_DELIMITER (_c))		\
   || ((_c) == '}') || ((_c) == '{'))

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTER
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME
#undef SYMBOL_TR_VALID_CHARACTER



SCM
scm_read_number_and_radix (int chr, SCM port, scm_reader_t scm_reader,
			   scm_reader_t top_level_reader)
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
scm_read_quote (int chr, SCM port, scm_reader_t scm_reader,
		scm_reader_t top_level_reader)
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

  p = scm_cons2 (p, scm_call_reader (scm_reader, port, 0, top_level_reader),
		 SCM_EOL);

  return p;
}

SCM
scm_read_semicolon_comment (int chr, SCM port, scm_reader_t scm_reader,
			    scm_reader_t top_level_reader)
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
scm_read_sharp (int chr, SCM port, scm_reader_t scm_reader,
		scm_reader_t top_level_reader)
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
scm_read_boolean (int chr, SCM port, scm_reader_t scm_reader,
		  scm_reader_t top_level_reader)
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
scm_read_character (int chr, SCM port, scm_reader_t reader,
		    scm_reader_t top_level_reader)
{
  unsigned c;
  char charname[100];
  size_t charname_len;

  read_token (port, charname, sizeof (charname), &charname_len);

  if (charname_len == 0)
    {
      chr = scm_getc (port);
      if (chr == EOF)
	scm_i_input_error (__FUNCTION__, port, "unexpected end of file "
			   "while reading character", SCM_EOL);

      /* CHR must be a token delimiter, like a whitespace.  */
      return (SCM_MAKE_CHAR (chr));
    }

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
scm_read_keyword (int chr, SCM port, scm_reader_t reader,
		  scm_reader_t top_level_reader)
{
  /* Initially, I was tempted to call SCM_READER below, thinking that it was
     likely to result in a call to `scm_read_symbol ()'.  However,
     `scm_read_keyword ()' may typically be used either in the top-level
     reader or in the sharp reader.  In the latter case, calling SCM_READER
     just wouldn't work since the sharp reader doesn't handle symbols.  */
  return (scm_symbol_to_keyword
	  (scm_read_guile_mixed_case_symbol (scm_getc (port), port,
					     reader, top_level_reader)));
}

SCM
scm_read_vector (int chr, SCM port, scm_reader_t reader,
		 scm_reader_t top_level_reader)
{
  /* Note: We call `scm_read_sexp ()' rather than READER here in order to
     guarantee that it's going to do what we want.  After all, this is an
     implementation detail of `scm_read_vector ()', not a desirable
     property.  */
  return scm_vector (scm_read_sexp (chr, port, reader,
				    top_level_reader));
}

SCM
scm_read_srfi4_vector (int chr, SCM port, scm_reader_t scm_reader,
		       scm_reader_t top_level_reader)
{
  return scm_i_read_array (port, chr);
}

SCM
scm_read_guile_bit_vector (int chr, SCM port, scm_reader_t scm_reader,
			   scm_reader_t top_level_reader)
{
  /* Read the `#*10101'-style read syntax for bit vectors in Guile.  This is
     terribly inefficient but who cares?  */
  SCM s_bits = SCM_EOL;

  for (chr = scm_getc (port);
       (chr != EOF) && ((chr == '0') || (chr == '1'));
       chr = scm_getc (port))
    {
      s_bits = scm_cons ((chr == '0') ? SCM_BOOL_F : SCM_BOOL_T, s_bits);
    }

  if (chr != EOF)
    scm_ungetc (chr, port);

  return scm_bitvector (scm_reverse_x (s_bits, SCM_EOL));
}

SCM
scm_read_scsh_block_comment (int chr, SCM port, scm_reader_t scm_reader,
			     scm_reader_t top_level_reader)
{
  skip_scsh_block_comment (port);
  return SCM_UNSPECIFIED;
}

SCM
scm_read_extended_symbol (int chr, SCM port, scm_reader_t scm_reader,
			  scm_reader_t top_level_reader)
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
scm_read_skribe_exp (int chr, SCM port, scm_reader_t reader,
		     scm_reader_t top_level_reader)
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
				  scm_call_reader (top_level_reader,
						   port, 0,
						   top_level_reader),
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

extern const struct scm_token_reader_entry *
_scm_token_reader_lookup (const char *, unsigned int);

#include <string.h>
#include "token-reader-lookup.c"

const scm_token_reader_spec_t *
scm_token_reader_lookup (const char *name)
{
  const struct scm_token_reader_entry *entry;

  entry = _scm_token_reader_lookup (name, strlen (name));

  return (entry ? &entry->reader : NULL);
}

/* A NULL-terminated array of token reader names.  */
const char *scm_standard_token_reader_list[] =
  {
#include "token-reader-list.c"
    NULL
  };

SCM_DEFINE (scm_standard_token_reader_names,
	    "standard-token-reader-names", 0, 0, 0,
	    (void),
	    "Return a list of symbols where each symbol is the name "
	    "of a standard token reader that may be obtained using "
	    "@code{standard-token-reader}.")
{
  SCM lst;
  const char **p;

  for (p = scm_standard_token_reader_list, lst = SCM_EOL;
       *p;
       p++)
    {
      lst = scm_cons (scm_from_locale_symbol (*p), lst);
    }

  return lst;
}


/* Initialization.  */

void
scm_initialize_token_reader_library (void)
{
#include "token-readers.c.x"
}
