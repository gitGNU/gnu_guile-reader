/* A dynamic Scheme reader compiler.

   Copyright (C) 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>

   Part of the code in here (a few `scm_token_reader_t' functions below) is
   heavily based on Guile's code (file `read.c') which contain the following
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


#include <libguile.h>
#include <ctype.h>
#include <strings.h>
#include <assert.h>

#include "reader.h"
#include "token-readers.h"



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
      if ((chr == ' ') || (chr == '\t') || (chr == '\n'))
	{
	  scm_ungetc (chr, port);
	  break;
	}
      else if (chr == EOF)
	break;

      *(buf++) = (char)chr;
      (*read)++;

      if (*read == buf_size)
	break;
    }
}


/* Readers (mostly) stolen from Guile.  */

SCM
scm_read_sexp (int chr, SCM port, scm_reader_t scm_reader)
#define FUNC_NAME "scm_read_sexp"
{
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
  if (scm_is_eq (scm_sym_dot, (tmp = scm_reader (port))))
    {
      ans = scm_reader (port);
      if (')' != (c = scm_flush_ws (port, "scm_read_sexp")))
	scm_i_input_error (FUNC_NAME, port, "missing close paren", SCM_EOL);
      return ans;
    }
  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);
  if (SCM_COPY_SOURCE_P)
    ans2 = tl2 = scm_cons (scm_is_pair (tmp)
			   ? tmp /* FIXME: was *copy */
			   : tmp,
			   SCM_EOL);
  while (')' != (c = scm_flush_ws (port, "scm_read_sexp")))
    {
      SCM new_tail;

      scm_ungetc (c, port);
      if (scm_is_eq (scm_sym_dot, (tmp = scm_reader (port))))
	{
	  SCM_SETCDR (tl, tmp = scm_reader (port));
	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (scm_is_pair (tmp)
				       ? tmp /* FIXME: Was *copy */
				       : tmp,
				       SCM_EOL));
	  if (')' != (c = scm_flush_ws (port, "scm_read_sexp")))
	    scm_i_input_error (FUNC_NAME, port,
			       "missing close paren", SCM_EOL);
	  goto exit;
	}

      new_tail = scm_cons (tmp, SCM_EOL);
      SCM_SETCDR (tl, new_tail);
      tl = new_tail;

      if (SCM_COPY_SOURCE_P)
	{
	  SCM new_tail2 = scm_cons (scm_is_pair (tmp)
				    ? tmp /* FIXME: Was *copy */
				    : tmp, SCM_EOL);
	  SCM_SETCDR (tl2, new_tail2);
	  tl2 = new_tail2;
	}
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


#if 0
/* The list of valid Scheme identifiers starting characters.  */
static const char scm_identifiers_charset[] =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-/+*&@%[]";
#endif

SCM
scm_read_symbol (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;
  char c_id[1024];
  size_t c_id_len = 0;

  c = chr;
  while (c != EOF)
    {
      /* Note: for pratical reasons (read: laziness), we excluse brackets
	 from the list of allowed characters for symbols.  */
      if ((isalnum (c) || (isgraph (c)))
	  && (c != '(') && (c != ')') && (c != '[') && (c != ']'))
	c_id[c_id_len++] = (char)c;
      else
	{
	  scm_ungetc (c, port);
	  break;
	}

      if (c_id_len + 1 >= sizeof (c_id))  /* FIXME: shortcoming */
	break;

      c = scm_getc (port);
    }

  return (scm_from_locale_symboln (c_id, c_id_len));
}

SCM
scm_read_number (int chr, SCM port, scm_reader_t scm_reader)
{
  int c;
  char c_num[1024];
  size_t c_num_len = 0;

  c = chr;
  while (c != EOF)
    {
      if (isdigit (c))
	c_num[c_num_len++] = (char)c;
      else
	{
	  scm_ungetc (c, port);
	  break;
	}

      if (c_num_len + 1 >= sizeof (c_num))  /* FIXME: shortcoming */
	break;

      c = scm_getc (port);
    }

  return (scm_i_mem2number (c_num, c_num_len, 10));
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

  p = scm_cons2 (p, scm_reader (port), SCM_EOL);
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
   initial `#!', and eat characters until we get a
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

/* recsexpr is used when recording expressions
 * constructed by read:sharp.
 */
static inline SCM
recsexpr (SCM obj, long line, int column, SCM filename)
{
  if (!scm_is_pair(obj)) {
    return obj;
  } else {
    SCM tmp = obj, copy;
    /* If this sexpr is visible in the read:sharp source, we want to
       keep that information, so only record non-constant cons cells
       which haven't previously been read by the reader. */
    if (scm_is_false (scm_whash_lookup (scm_source_whash, obj)))
      {
	if (SCM_COPY_SOURCE_P)
	  {
	    copy = scm_cons (recsexpr (SCM_CAR (obj), line, column, filename),
			     SCM_UNDEFINED);
	    while ((tmp = SCM_CDR (tmp)) && scm_is_pair (tmp))
	      {
		SCM_SETCDR (copy, scm_cons (recsexpr (SCM_CAR (tmp),
						      line,
						      column,
						      filename),
					    SCM_UNDEFINED));
		copy = SCM_CDR (copy);
	      }
	    SCM_SETCDR (copy, tmp);
	  }
	else
	  {
	    recsexpr (SCM_CAR (obj), line, column, filename);
	    while ((tmp = SCM_CDR (tmp)) && scm_is_pair (tmp))
	      recsexpr (SCM_CAR (tmp), line, column, filename);
	    copy = SCM_UNDEFINED;
	  }
	scm_whash_insert (scm_source_whash,
			  obj,
			  scm_make_srcprops (line,
					     column,
					     filename,
					     copy,
					     SCM_EOL));
      }
    return obj;
  }
}

/* Recover the read-hash procedure corresponding to char c.  */
static inline SCM
scm_get_hash_procedure (int c)
{
  /* FIXME:  SCM_READ_HASH_PROCEDURES is not public!  */
  return SCM_BOOL_F;
#if 0
  SCM rest = *scm_read_hash_procedures;

  while (1)
    {
      if (scm_is_null (rest))
	return SCM_BOOL_F;
  
      if (SCM_CHAR (SCM_CAAR (rest)) == c)
	return SCM_CDAR (rest);
     
      rest = SCM_CDR (rest);
    }
#endif
}

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
      return scm_reader (port);

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
      return scm_symbol_to_keyword (scm_reader (port));

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
  return (scm_symbol_to_keyword (scm_reader (port)));
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
				  scm_reader (port), SCM_EOL);
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


/* Sample Scheme reader.  */

const scm_token_reader_spec_t scm_sharp_reader_standard_specs[] =
  {
    SCM_DEFTOKEN_SINGLE ('\\', "character",      scm_read_character),
    SCM_DEFTOKEN_SET ("suf",   "srfi-4",         scm_read_srfi4_vector),
    SCM_DEFTOKEN_SET ("ftTF",  "boolean",        scm_read_boolean),
    SCM_DEFTOKEN_SINGLE (':',  "keyword",        scm_read_keyword),
    SCM_DEFTOKEN_SINGLE ('!',  "block-comment",  scm_read_block_comment),
    SCM_END_TOKENS
  };

/* A reader that must be compiled at initialization-time.  */
scm_reader_t scm_standard_sharp_reader = NULL;

/* A default, Scheme-like, reader specification.  */
scm_token_reader_spec_t scm_reader_standard_specs[] =
  {
    SCM_DEFTOKEN_SINGLE ('(', "sexp",   scm_read_sexp),
    SCM_DEFTOKEN_SINGLE ('"', "string", scm_read_string),

    /* FIXME: Number can also start with `+' or `-' (?), which conflicts with
       symbol names.  */
    SCM_DEFTOKEN_RANGE ('0', '9', "number", scm_read_number),
    SCM_DEFTOKEN_SET ("bBoOdDxXiIeE", "number+base", scm_read_number),

    /* Let's define symbols as two ranges plus one set of triggering
       characters.  */
    SCM_DEFTOKEN_RANGE ('a', 'z', "symbol-lower-case", scm_read_symbol),
    SCM_DEFTOKEN_RANGE ('A', 'Z', "symbol-upper-case", scm_read_symbol),
    SCM_DEFTOKEN_SET ("+-/*%@_", "symbol-misc-chars",  scm_read_symbol),

    SCM_DEFTOKEN_SET ("'`,", "quote-quasiquote-unquote", scm_read_quote),

    /* This one is defined at reader's compile-time.  */
    SCM_DEFTOKEN_SINGLE ('#', "sharp",                   NULL),

    SCM_DEFTOKEN_SINGLE (';', "semicolon-comment",
			 scm_read_semicolon_comment),

#if 0
    /* Skribe/Skribilo literal sequences.  In R6RS, `[' would rather be
       associated to `scm_read_sexp ()'.  */
    SCM_DEFTOKEN_SINGLE ('[', "skribe-exp", scm_read_skribe_exp),
#endif

    SCM_END_TOKENS
  };

/* The standard reader (which depends on the standard keyword reader),
   compiled at initialization time.  */
scm_reader_t scm_standard_reader = NULL;


static char standard_reader_code[8000];
static char standard_sharp_reader_code[4000];

void
scm_load_standard_reader (void)
{
  /* XXX  Ultimately, we might want to simply mmap a file containing the
     pre-compiled readers.  */

  size_t code_size = 0;

  if (!scm_standard_sharp_reader)
    {
      scm_standard_sharp_reader =
	scm_c_make_reader (standard_sharp_reader_code,
			   sizeof (standard_sharp_reader_code),
			   " \t\n",
			   scm_sharp_reader_standard_specs,
			   0,
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
			   " \t\n",
			   scm_reader_standard_specs,
			   0,
			   &code_size);
    }
}
