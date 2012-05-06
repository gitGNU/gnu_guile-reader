/* A Scheme reader compiler for Guile.

   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2012  Ludovic Courtès <ludo@gnu.org>

   Part of the code here (a few `scm_token_reader_t' functions below) is
   based on Guile code released under the GNU LGPL (file `read.c') which
   contains the following copyright line:

   Copyright (C) 1995, 1996, 1997, 1999, 2000, 2001, 2003, 2004, 2006,
     2007, 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.


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


/* A library of stardard token readers that can be assembled to create a
   reader equivalent to that of Guile.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <assert.h>

#include "reader.h"
#include "token-readers.h"

#include "compat.h"



/* `isblank' is only in C99.  */

#define BLANK_CHARS_ " \t\n\f\r"
#ifdef MSDOS
# define BLANK_CHARS  BLANK_CHARS_ "\032"
#else
# define BLANK_CHARS  BLANK_CHARS_
#endif

#define CHAR_IS_BLANK(c)			\
  (index (BLANK_CHARS, (c)) != NULL)


/* R5RS one-character delimiters (see section 7.1.1, ``Lexical
   structure'').  */
#define R5RS_DELIMITERS  BLANK_CHARS "();\""

#define CHAR_IS_R5RS_DELIMITER(c)		\
  (index (R5RS_DELIMITERS, (c)) != NULL)

#define R6RS_DELIMITERS  R5RS_DELIMITERS "[]"

#define CHAR_IS_R6RS_DELIMITER(c)		\
  (index (R6RS_DELIMITERS, (c)) != NULL)


/* Read from PORT until a delimiter (e.g., a whitespace) is read.  Put the
   result in the pre-allocated buffer BUF.  Return zero if the whole token has
   fewer than BUF_SIZE bytes, non-zero otherwise. READ will be set the number of
   bytes actually read.  */
int
read_token (SCM port, char *buf, size_t buf_size, const char *delimiters,
	    size_t *read)
{
  *read = 0;

  while (*read < buf_size)
    {
      int chr;

      chr = scm_get_byte_or_eof (port);

      if (chr == EOF)
	return 0;
      else if (index (delimiters, chr) != NULL)
	{
	  scm_unget_byte (chr, port);
	  return 0;
	}
      else
	{
	  *buf = (char) chr;
	  buf++, (*read)++;
	}
    }

  return 1;
}

/* Like `read_token', but return either BUFFER, or a GC-allocated buffer
   if the token doesn't fit in BUFFER_SIZE bytes.  */
static char *
read_complete_token (SCM port, char *buffer, size_t buffer_size,
		     const char *delimiters, size_t *read)
{
  int overflow = 0;
  size_t bytes_read, overflow_size = 0;
  char *overflow_buffer = NULL;

  do
    {
      overflow = read_token (port, buffer, buffer_size,
			     delimiters, &bytes_read);
      if (bytes_read == 0)
        break;
      if (overflow || overflow_size != 0)
        {
          if (overflow_size == 0)
            {
              overflow_buffer = scm_gc_malloc_pointerless (bytes_read, "read");
              memcpy (overflow_buffer, buffer, bytes_read);
              overflow_size = bytes_read;
            }
          else
            {
	      char *new_buf =
#ifdef HAVE_SCM_GC_MALLOC_POINTERLESS
		scm_gc_malloc_pointerless (overflow_size + bytes_read, "read");
#else
	        scm_malloc (overflow_size + bytes_read);
#endif

	      memcpy (new_buf, overflow_buffer, overflow_size);
              memcpy (new_buf + overflow_size, buffer, bytes_read);

#ifdef HAVE_SCM_GC_MALLOC_POINTERLESS
	      free (overflow_buffer);
#endif

	      overflow_buffer = new_buf;
              overflow_size += bytes_read;
            }
        }
    }
  while (overflow);

  if (overflow_size)
    *read = overflow_size;
  else
    *read = bytes_read;

  return (overflow_size > 0 ? overflow_buffer : buffer);
}


/* Readers (mostly) stolen from Guile.  */


/* A simplified version of `scm_flush_ws ()' that makes no assumption about
   the comment syntax being used (in particular, it doesn't assume SCSH block
   comments).  */
static int
_flush_ws (SCM port, const char *eoferr)
{
  scm_t_wchar c;

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

  return 0;
}

#define scm_flush_ws _flush_ws

SCM
scm_read_sexp (scm_t_wchar chr, SCM port, scm_reader_t reader,
	       scm_reader_t top_level_reader)
#define FUNC_NAME "scm_read_sexp"
{
  /* Note:  We rely here on the ability for SCM_READER to read `.' as a
     symbol (see SCM_SYM_DOT below).  */
  register int c;
  register SCM tmp;
  register SCM tl, ans = SCM_EOL;
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
      scm_i_input_error (FUNC_NAME, port, "invalid sexp-starting character",
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
	{
	  /* TOP_LEVEL_READER read a character it does not handle.  This may
	     be a closing bracket so let's see.  */
	  c = scm_getc (port);
	  if (c == terminating_char)
	    /* Fine: we're done with this S-expression so we can leave the
	       loop.  */
	    break;
	  else
	    {
	      /* Gosh! C is a unhandled character, so we'll let the calling
		 reader handle it.  */
	      scm_ungetc (c, port);
	      return SCM_UNSPECIFIED;
	    }
	}

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

/* Number of 32-bit codepoints in the buffer used to read strings.  */
#define READER_STRING_BUFFER_SIZE 128

/* Read a hexadecimal number NDIGITS in length.  Put its value into the variable
   C.  If TERMINATOR is non-null, terminate early if the TERMINATOR character is
   found.  */
#define SCM_READ_HEX_ESCAPE(ndigits, terminator)                   \
  do                                                               \
    {                                                              \
      scm_t_wchar a;                                               \
      size_t i = 0;                                                \
      c = 0;                                                       \
      while (i < ndigits)                                          \
        {                                                          \
          a = scm_getc (port);                                     \
          if (a == EOF)                                            \
            goto str_eof;                                          \
          if (terminator                                           \
              && (a == (scm_t_wchar) terminator)                   \
              && (i > 0))                                          \
            break;                                                 \
          if ('0' <= a && a <= '9')                                \
            a -= '0';                                              \
          else if ('A' <= a && a <= 'F')                           \
            a = a - 'A' + 10;                                      \
          else if ('a' <= a && a <= 'f')                           \
            a = a - 'a' + 10;                                      \
          else                                                     \
            {                                                      \
              c = a;                                               \
              goto bad_escaped;                                    \
            }                                                      \
          c = c * 16 + a;                                          \
          i ++;                                                    \
        }                                                          \
    } while (0)

/* FIXME: Add a version that supports R6RS escapes.  */
SCM
scm_read_string (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
		 scm_reader_t top_level_reader)
#define FUNC_NAME "scm_read_string"
{
  /* For strings smaller than C_STR, this function creates only one Scheme
     object (the string returned).  */

  SCM str = SCM_EOL;
  size_t c_str_len = 0;
  scm_t_wchar c, c_str[READER_STRING_BUFFER_SIZE];

  while ('"' != (c = scm_getc (port)))
    {
      if (c == EOF)
        {
        str_eof:
          scm_i_input_error (FUNC_NAME, port,
                             "end of file in string constant", SCM_EOL);
        }

      if (c_str_len + 1 >= READER_STRING_BUFFER_SIZE)
	{
	  str = scm_cons (scm_from_utf32_stringn (c_str, c_str_len), str);
	  c_str_len = 0;
	}

      if (c == '\\')
        {
          switch (c = scm_getc (port))
            {
            case EOF:
              goto str_eof;
            case '"':
            case '\\':
              break;
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
            case 'b':
              c = '\010';
              break;
            case 'x':
	      SCM_READ_HEX_ESCAPE (2, '\0');
              break;
            case 'u':
	      SCM_READ_HEX_ESCAPE (4, '\0');
	      break;
            case 'U':
	      SCM_READ_HEX_ESCAPE (6, '\0');
	      break;

            default:
	    bad_escaped:
              scm_i_input_error (FUNC_NAME, port,
                                 "invalid character in escape sequence: ~S",
                                 scm_list_1 (SCM_MAKE_CHAR (c)));
            }
        }

      c_str[c_str_len++] = c;
    }

  if (scm_is_null (str))
    /* Fast path: we got a string that fits in C_STR.  */
    str = scm_from_utf32_stringn (c_str, c_str_len);
  else
    {
      if (c_str_len > 0)
	str = scm_cons (scm_from_utf32_stringn (c_str, c_str_len), str);

      str = scm_string_concatenate_reverse (str, SCM_UNDEFINED, SCM_UNDEFINED);
    }

  return str;
}
#undef FUNC_NAME


/* Produce several flavors of symbol and number token readers: with or
   without case-sensitivity, and with various sets of allowed characters in a
   symbol name.  Note that the ``number TR'' may as well return a symbol, in
   cases like `123.123.123'.

   Both the symbol TR and the number TR rely on the definition of delimiter
   characters which varies, for instance, between R5RS and R6RS.  So we'll
   generate both of them here.  */

/* Size of the on-stack buffer.  */
#define SYMBOL_BUFFER_SIZE  256


#define SYMBOL_TR_NAME scm_read_guile_mixed_case_symbol
#define NUMBER_TR_NAME scm_read_guile_number
#define SYMBOL_TR_TRANSFORM_CHARACTERS(s)  do {} while (0)
#define DELIMITERS     R5RS_DELIMITERS

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTERS
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME

/* FIXME: Check whether this really corresponds to R5RS.  */

#define SYMBOL_TR_NAME scm_read_r5rs_lower_case_symbol
#define NUMBER_TR_NAME scm_read_r5rs_lower_case_number
#define SYMBOL_TR_TRANSFORM_CHARACTERS(s)		\
  do { s = scm_string_downcase_x (s); } while (0)

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTERS
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME

#define SYMBOL_TR_NAME scm_read_r5rs_upper_case_symbol
#define NUMBER_TR_NAME scm_read_r5rs_upper_case_number
#define SYMBOL_TR_TRANSFORM_CHARACTERS(s)		\
  do { s = scm_string_upcase_x (s); } while (0)

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTERS
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME
#undef DELIMITERS

/* FIXME: Check whether this really corresponds to R6RS.  */

#define SYMBOL_TR_NAME scm_read_r6rs_symbol
#define NUMBER_TR_NAME scm_read_r6rs_number
#define SYMBOL_TR_TRANSFORM_CHARACTERS(s)  do {} while (0)
#define DELIMITERS     R6RS_DELIMITERS
#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTERS
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME
#undef DELIMITERS

#define SYMBOL_TR_NAME scm_read_brace_free_symbol
#define NUMBER_TR_NAME scm_read_brace_free_number
#define SYMBOL_TR_TRANSFORM_CHARACTERS(s)  do {} while (0)
#define DELIMITERS     R5RS_DELIMITERS "{}"

#include "symbol-token-reader.c"

#undef SYMBOL_TR_TRANSFORM_CHARACTERS
#undef NUMBER_TR_NAME
#undef SYMBOL_TR_NAME
#undef DELIMITERS
#undef SYMBOL_TR_VALID_CHARACTER



SCM
scm_read_number_and_radix (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
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
scm_read_quote (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
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
	scm_t_wchar c;

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

SCM_SYMBOL (sym_syntax, "syntax");
SCM_SYMBOL (sym_quasisyntax, "quasisyntax");
SCM_SYMBOL (sym_unsyntax, "unsyntax");
SCM_SYMBOL (sym_unsyntax_splicing, "unsyntax-splicing");

/* Read R6RS-style syntax quotes: #', #`, #,.  Adapted from Guile's `read.c'
   where it appeared in commit 34f3d47d, dated 2009-05-28.  */
SCM
scm_read_r6rs_syntax_quote (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
			    scm_reader_t top_level_reader)
{
  SCM p;

  switch (chr)
    {
    case '`':
      p = sym_quasisyntax;
      break;

    case '\'':
      p = sym_syntax;
      break;

    case ',':
      {
       scm_t_wchar c;

       c = scm_getc (port);
       if ('@' == c)
         p = sym_unsyntax_splicing;
       else
         {
           scm_ungetc (c, port);
           p = sym_unsyntax;
         }
       break;
      }

    default:
      fprintf (stderr, "%s: unhandled syntax character (%i)\n",
              "scm_read_syntax", chr);
      abort ();
    }

  p = scm_cons2 (p, scm_call_reader (top_level_reader, port, 0,
				     top_level_reader),
		 SCM_EOL);

  return p;
}

SCM
scm_read_semicolon_comment (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
			    scm_reader_t top_level_reader)
{
  scm_t_wchar c;

  for (c = scm_getc (port);
       (c != EOF) && (c != '\n');
       c = scm_getc (port));

  return SCM_UNSPECIFIED;
}


/* Sharp readers, i.e. readers called after a `#' sign has been read.  */

SCM
scm_read_boolean (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
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

#ifndef HAVE_SCM_CHARNAMES

static const char *const scm_charnames[] =
{
  "nul","soh","stx","etx","eot","enq","ack","bel",
   "bs", "ht", "newline", "vt", "np", "cr", "so", "si",
  "dle","dc1","dc2","dc3","dc4","nak","syn","etb",
  "can", "em","sub","esc", "fs", "gs", "rs", "us",
  "space", "sp", "nl", "tab", "backspace", "return", "page", "null", "del"
};

#define scm_n_charnames  (sizeof (scm_charnames) / sizeof (char *))

#endif /* !HAVE_SCM_CHARNAMES */

#ifndef HAVE_SCM_CHARNUMS

static const char scm_charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
  \n\t\b\r\f\0\177";

#endif /* !HAVE_SCM_CHARNUMS */


SCM
scm_read_character (scm_t_wchar chr, SCM port, scm_reader_t reader,
		    scm_reader_t top_level_reader)
{
  unsigned c;
  char charname[100];
  size_t charname_len;

  read_token (port, charname, sizeof charname, R5RS_DELIMITERS,
	      &charname_len);

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
      SCM p = scm_c_locale_stringn_to_number (charname, charname_len, 8);
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
scm_read_keyword (scm_t_wchar chr, SCM port, scm_reader_t reader,
		  scm_reader_t top_level_reader)
{
  SCM symbol;

  /* Invoke TOP_LEVEL_READER to read the symbol that comprises the keyword.
     Doing this instead of invoking a specific symbol reader function allows
     `scm_read_keyword ()' to adapt to the delimiters currently valid of
     symbols.

     XXX: This implementation allows sloppy syntaxes like `#:  key' but so
     does Guile's built-in reader.  */
  symbol = scm_call_reader (top_level_reader, port, 0, top_level_reader);
  if (!scm_is_symbol (symbol))
    scm_i_input_error (__FUNCTION__, port,
		       "symbol prefix `~a' not followed by a symbol: ~s",
		       scm_list_2 (SCM_MAKE_CHAR (chr), symbol));

  return (scm_symbol_to_keyword (symbol));
}

SCM
scm_read_vector (scm_t_wchar chr, SCM port, scm_reader_t reader,
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
scm_read_srfi4_vector (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
		       scm_reader_t top_level_reader)
{
  return scm_i_read_array (port, chr);
}

SCM
scm_read_guile_bit_vector (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
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
scm_read_scsh_block_comment (scm_t_wchar chr, SCM port, scm_reader_t reader,
			     scm_reader_t top_level_reader)
{
  int bang_seen = 0;

  for (;;)
    {
      scm_t_wchar c = scm_getc (port);

      if (c == EOF)
	scm_i_input_error (__FUNCTION__, port,
			   "unterminated `#! ... !#' comment", SCM_EOL);

      if (c == '!')
	bang_seen = 1;
      else if (c == '#' && bang_seen)
	break;
      else
	bang_seen = 0;
    }

  return SCM_UNSPECIFIED;
}

SCM
scm_read_srfi30_block_comment (scm_t_wchar chr, SCM port, scm_reader_t reader,
			       scm_reader_t top_level_reader)
{
  /* Unlike SCSH-style block comments, SRFI-30 block comments may be nested.
     So care must be taken.  */
  int nesting_level = 1;
  int opening_seen = 0, closing_seen = 0;

  while (nesting_level > 0)
    {
      scm_t_wchar c = scm_getc (port);

      if (c == EOF)
	scm_i_input_error (__FUNCTION__, port,
			   "unterminated `#| ... |#' comment", SCM_EOL);

      if (opening_seen)
	{
	  if (c == '|')
	    nesting_level++;
	  opening_seen = 0;
	}
      else if (closing_seen)
	{
	  if (c == '#')
	    nesting_level--;
	  closing_seen = 0;
	}
      else if (c == '|')
	closing_seen = 1;
      else if (c == '#')
	opening_seen = 1;
      else
	opening_seen = closing_seen = 0;
    }

  return SCM_UNSPECIFIED;
}

SCM
scm_read_srfi62_sexp_comment (scm_t_wchar chr, SCM port, scm_reader_t reader,
			      scm_reader_t top_level_reader)
{
  /* Skip the S-expression that follows the semi-colon.  */
  scm_call_reader (top_level_reader, port, 0, top_level_reader);

  return SCM_UNSPECIFIED;
}

SCM
scm_read_extended_symbol (scm_t_wchar chr, SCM port, scm_reader_t scm_reader,
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
scm_read_skribe_exp (scm_t_wchar chr, SCM port, scm_reader_t reader,
		     scm_reader_t top_level_reader)
{
  scm_t_wchar c, escaped = 0;
  scm_t_wchar c_literal[1024];
  size_t c_literal_len = 0;
  SCM result = SCM_EOL, s_literal = SCM_EOL;

#define FLUSH_STRING()							\
  do									\
    {									\
      s_literal =							\
	scm_cons (scm_from_utf32_stringn (c_literal, c_literal_len),	\
		  s_literal);						\
      c_literal_len = 0;						\
    }									\
  while (0)

  for (c = scm_getc (port);
       (c != EOF) && ((c != ']') || (escaped));
       c = scm_getc (port))
    {
      if (escaped)
	{
	  c_literal[c_literal_len++] = c;
	  escaped = 0;
	}
      else
	{
	  switch (c)
	    {
	    case ',':
	      c = scm_getc (port);
	      if (c == '(')
		{
		  SCM subexp;

		  FLUSH_STRING ();
		  s_literal = scm_string_concatenate_reverse (s_literal,
							      SCM_UNDEFINED,
							      SCM_UNDEFINED);
		  result = scm_cons (s_literal, result);
		  s_literal = SCM_EOL;

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
		    c_literal[c_literal_len++] = c;
		}
	      break;

	    case '\\':
	      escaped = 1;
	      break;

	    default:
	      c_literal[c_literal_len++] = c;
	    }
	}

      if (c_literal_len + 1 >= sizeof (c_literal))
	/* Flush the contents of C_LITERAL to S_LITERAL, a dynamically grown
	   Scheme string.  */
	FLUSH_STRING ();
    }

  FLUSH_STRING ();
  if (scm_is_pair (s_literal))
    {
      s_literal = scm_string_concatenate_reverse (s_literal, SCM_UNDEFINED,
						  SCM_UNDEFINED);
      result = scm_cons (s_literal, result);
    }

  result = scm_reverse_x (result, SCM_EOL);
  result = scm_cons2 (scm_sym_quasiquote, result, SCM_EOL);

#undef FLUSH_STRING

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
#include "token-readers.x"
}
