/* A Scheme reader compiler for Guile.

   Copyright (C) 2005, 2009, 2012  Ludovic Courtès  <ludo@gnu.org>

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


#ifndef GUILE_TOKEN_READERS_H
#define GUILE_TOKEN_READERS_H

/* A list of specific token readers, most of which were stolen from
   Guile.  */

#include <libguile.h>
#include "reader.h"


/* Main token readers.  */

extern SCM scm_read_sexp (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_string (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_guile_number (scm_t_wchar, SCM, scm_reader_t,
				  scm_reader_t);

extern SCM scm_read_number_and_radix (scm_t_wchar, SCM, scm_reader_t,
				      scm_reader_t);

extern SCM scm_read_quote (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_semicolon_comment (scm_t_wchar, SCM, scm_reader_t,
				       scm_reader_t);

extern SCM scm_read_scsh_block_comment (scm_t_wchar, SCM, scm_reader_t,
					scm_reader_t);

extern SCM scm_read_srfi30_block_comment (scm_t_wchar, SCM, scm_reader_t,
					  scm_reader_t);

extern SCM scm_read_srfi62_sexp_comment (scm_t_wchar, SCM, scm_reader_t,
					 scm_reader_t);

extern SCM scm_read_boolean (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_character (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_keyword (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_vector (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_srfi4_vector (scm_t_wchar, SCM, scm_reader_t,
				  scm_reader_t);

extern SCM scm_read_guile_bit_vector (scm_t_wchar, SCM, scm_reader_t,
				      scm_reader_t);

extern SCM scm_read_skribe_exp (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);


/* Symbol token readers.  */
extern SCM scm_read_guile_mixed_case_symbol (scm_t_wchar, SCM, scm_reader_t,
					     scm_reader_t);

extern SCM scm_read_r5rs_lower_case_symbol (scm_t_wchar, SCM, scm_reader_t,
					    scm_reader_t);

extern SCM scm_read_r5rs_upper_case_symbol (scm_t_wchar, SCM, scm_reader_t,
					    scm_reader_t);

extern SCM scm_read_r5rs_lower_case_number (scm_t_wchar, SCM, scm_reader_t,
					    scm_reader_t);

extern SCM scm_read_r5rs_upper_case_number (scm_t_wchar, SCM, scm_reader_t,
					    scm_reader_t);

extern SCM scm_read_r6rs_symbol (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_r6rs_number (scm_t_wchar, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_r6rs_syntax_quote (scm_t_wchar, SCM, scm_reader_t,
				       scm_reader_t);

extern SCM scm_read_brace_free_number (scm_t_wchar, SCM, scm_reader_t,
				       scm_reader_t);

extern SCM scm_read_brace_free_symbol (scm_t_wchar, SCM, scm_reader_t,
				       scm_reader_t);


/* Sharp token readers, should be called after `#' has been read.  */

extern SCM scm_read_extended_symbol (scm_t_wchar, SCM, scm_reader_t,
				     scm_reader_t);


/* Look for a token reader named NAME in the standard token readers and
   return its specification if found, otherwise return NULL.  */
extern const scm_token_reader_spec_t *
scm_token_reader_lookup (const char *name);

/* A NULL-terminated list of token reader names.  */
extern const char *scm_standard_token_reader_list[];

/* Return a list of symbols each of which is the name of a standard token
   reader.  */
extern SCM scm_standard_token_reader_names (void);

/* Initialize the token reader library and its Scheme bindings.  */
extern void scm_initialize_token_reader_library (void);



/* Static C token readers initializers.

   Note that the name enclose in comments right after the macro name is
   important: it is extracted at compilation-time and then used as the token
   reader identifier by `scm_token_reader_lookup ()'.  */


/* A number of sharp token readers, i.e. token readers whose invocation only
   makes sense after the `#' character.  */
#define SCM_TR_CHARACTER /* character */				\
  SCM_DEFTOKEN_SINGLE ('\\', "character",      scm_read_character, 0,	\
		       "This is a sharp token reader, i.e. it reads "	\
		       "an R5RS character once a @code{#} character "	\
		       "has been read.")
#define SCM_TR_VECTOR /* vector */					\
  SCM_DEFTOKEN_SINGLE ('(',  "vector",         scm_read_vector, 0,	\
		       "This is a sharp token reader, i.e. it reads "	\
		       "an R5RS vector once a @code{#} character has "	\
		       "been read.")
#define SCM_TR_SRFI_4 /* srfi-4 */					 \
  SCM_DEFTOKEN_SET ("suf",   "srfi-4",         scm_read_srfi4_vector, 0, \
		    "This is a sharp token reader, i.e. it reads an "	 \
		    "SRFI-4 homogenous numeric vector once a @code{#} "	 \
		    "character has been read.  This token reader also "	 \
		    "handles the boolean values @code{#f}.")
#define SCM_TR_GUILE_BIT_VECTOR /* guile-bit-vector */			\
  SCM_DEFTOKEN_SINGLE ('*', "guile-bit-vector",				\
		       scm_read_guile_bit_vector, 0,			\
		       "This is a sharp token reader, i.e. it reads a "	\
		       "bit vector following Guile's read syntax for "	\
		       "bit vectors.  See @inforef{Bit Vectors, "	\
		       "Guile's bit vectors, guile}, for details.")
#define SCM_TR_BOOLEAN /* boolean */					\
  SCM_DEFTOKEN_SET ("ftTF",  "boolean",        scm_read_boolean, 0,	\
		    "This is a sharp token reader, i.e. it reads an "	\
		    "R5RS boolean (@code{#f} or @code{#F}, @code{#t} "	\
		    "or @code{#T}) once a @code{#} character has been "	\
		    "read.")
#define SCM_TR_BOOLEAN_SRFI_4 /* boolean-srfi-4 */			\
  SCM_DEFTOKEN_SET ("tTF",  "boolean-srfi-4",    scm_read_boolean, 0,	\
		    "This is a sharp token reader, i.e. it reads an "	\
		    "R5RS boolean (@code{#t}, @code{#T}, @code{#F}, "	\
		    "but @emph{not} @code{#f}) once a @code{#} "	\
		    "character has been read.  Compared to the "	\
		    "@code{boolean} token reader, this one is useful "	\
		    "when SRFI-4 floating-point homogeneous vectors "	\
		    "are to be used at the same time: the SRFI-4 TR "	\
		    "will handle @code{#f} on its own (@pxref{"		\
		    "Overlapping Token Readers}).")
#define SCM_TR_KEYWORD /* keyword */					\
  SCM_DEFTOKEN_SINGLE (':',  "keyword",        scm_read_keyword, 0,	\
		       "This token reader returns a keyword as found "	\
		       "in Guile.  It may be used either after a "	\
		       "@code{#} character (to implement Guile's "	\
		       "default keyword syntax, @code{#:kw}) or "	\
		       "within the top-level reader (to implement "	\
		       "@code{:kw}-style keywords).\n\n"		\
		       "It is worth noting that this token reader "	\
		       "invokes its top-level in order to read the "	\
		       "symbol subsequent to the @code{:} "		\
		       "character.  Therefore, it will adapt to the "	\
		       "symbol delimiters currently in use "		\
		       "(@pxref{Token Delimiters}).")
#define SCM_TR_NUMBER_AND_RADIX /* number+radix */			\
  SCM_DEFTOKEN_SET ("bBoOdDxXiIeE", "number+radix",			\
		    scm_read_number_and_radix, 0,			\
		    "This is a sharp token reader, i.e. it reads a "	\
		    "number using the radix notation, like "		\
		    "@code{#b01} for the binary notation, @code{#x1d} "	\
		    "for the hexadecimal notation, etc., see "		\
		    "@inforef{Number Syntax, Guile's number syntax, "	\
		    "guile}, for details.")
#define SCM_TR_GUILE_EXTENDED_SYMBOL /* guile-extended-symbol */	\
  SCM_DEFTOKEN_SINGLE ('{',  "guile-extended-symbol",			\
		       scm_read_extended_symbol, 0,			\
		       "This is a sharp token reader, i.e. it reads "	\
		       "a symbol using Guile's extended symbol "	\
		       "syntax assuming a @code{#} character was "	\
		       "read.  See @inforef{Symbol Read Syntax, "	\
		       "Guile's extended read syntax for symbols, "	\
		       "guile}, for details.")
#define SCM_TR_SCSH_BLOCK_COMMENT /* scsh-block-comment */		\
  SCM_DEFTOKEN_SINGLE ('!',  "scsh-block-comment",			\
		       scm_read_scsh_block_comment, 1,			\
		       "This is a sharp token reader, i.e. it reads "	\
		       "a SCSH-style block comment (like "		\
		       "@code{#! multi-line comment !#}) and returns "	\
		       "@code{*unspecified*}, assuming a @code{#} "	\
		       "character was read before.  This token reader "	\
		       "has its ``escape'' bit set, meaning that "	\
		       "the reader that calls it will return "		\
		       "@code{*unspecified*} to its parent reader.  "	\
		       "See also @inforef{Block Comments, "		\
		       "block comments, guile}, for details about "	\
		       "SCSH block comments.")
#define SCM_TR_SRFI30_BLOCK_COMMENT /* srfi30-block-comment */		\
  SCM_DEFTOKEN_SINGLE ('|',  "srfi30-block-comment",			\
		       scm_read_srfi30_block_comment, 1,		\
		       "\n@cindex SRFI-30\n"				\
		       "This is a sharp token reader, i.e. it reads "	\
		       "an SRFI-30 block comment (like "		\
		       "@code{#| multi-line comment |#}) and returns "	\
		       "@code{*unspecified*}, assuming a @code{#} "	\
		       "character was read before.  This token reader "	\
		       "has its ``escape'' bit set.  For more details "	\
		       "about SRFI-30, see @uref{http://srfi."		\
		       "schemers.org/srfi-30/srfi-30.html, Nested "	\
		       "Multi-line Comments}.")
#define SCM_TR_SRFI62_SEXP_COMMENT /* srfi62-sexp-comment */		\
  SCM_DEFTOKEN_SINGLE (';', "srfi62-sexp-comment",			\
		       scm_read_srfi62_sexp_comment, 1,			\
		       "\n@cindex SRFI-62\n"				\
		       "This is a sharp token reader, i.e. it reads "	\
		       "an SRFI-62 comment S-expression (as in "	\
		       "@code{(+ 2 #;(comment here) 2)}) and returns "	\
		       "@code{*unspecified*}, assuming a @code{#} "	\
		       "character was read before.  This token reader "	\
		       "has its ``escape'' bit set.  For more details "	\
		       "about SRFI-62, please see @uref{http://srfi."	\
		       "schemers.org/srfi-62/srfi-62.html, "		\
		       "S-expression comments specifications}.")

/* Top-level token readers.  */
#define SCM_TR_WHITESPACE /* whitespace */				\
  SCM_DEFTOKEN_RANGE ('\1', ' ', "whitespace", NULL, 0,			\
		      "This is a void token reader that causes its "	\
		      "calling reader to ignore (i.e. treat as "	\
		      "whitespace) all ASCII characters ranging from "	\
		      "1 to 32.")
#define SCM_TR_SEXP /* sexp */						\
  SCM_DEFTOKEN_SINGLE ('(', "sexp",   scm_read_sexp, 0,			\
		       "Read a regular S-expression enclosed in "	\
		       "parentheses.")
#define SCM_TR_STRING /* string */				\
  SCM_DEFTOKEN_SINGLE ('"', "string", scm_read_string, 0,	\
		       "Read an R5RS string.")
#define SCM_TR_GUILE_NUMBER /* guile-number */				\
  SCM_DEFTOKEN_RANGE ('0', '9', "guile-number",				\
		      scm_read_guile_number, 0,				\
		      "Read a number following Guile's fashion, "	\
		      "that is, as in R5RS (@inforef{Lexical "		\
		      "structure, R5RS' lexical structure, r5rs}, "	\
		      "for syntactic details).  "			\
		      "Because the syntaxes for numbers and symbols "	\
		      "are closely tight in R5RS and Guile, this "	\
		      "token reader may return either a number or a "	\
		      "symbol.  For instance, it will be invoked if "	\
		      "the string @code{123.123.123} is passed to the "	\
		      "reader but this will actually yield a symbol "	\
		      "instead of a number (@pxref{Overlapping Token "	\
		      "Readers}).")
#define SCM_TR_GUILE_SYMBOL_LOWER_CASE /* guile-symbol-lower-case */	\
  SCM_DEFTOKEN_RANGE ('a', 'z', "guile-symbol-lower-case",		\
		      scm_read_guile_mixed_case_symbol, 0,		\
		      "Read a symbol that starts with a lower-case "	\
		      "letter in a case-sensitive fashion.")
#define SCM_TR_GUILE_SYMBOL_UPPER_CASE /* guile-symbol-upper-case */	\
  SCM_DEFTOKEN_RANGE ('A', 'Z', "guile-symbol-upper-case",		\
		      scm_read_guile_mixed_case_symbol, 0,		\
		      "Read a symbol that starts with an upper-case "	\
		      "letter in a case-sensitive fashion.")
#define SCM_TR_GUILE_SYMBOL_MISC_CHARS /* guile-symbol-misc-chars */	 \
  SCM_DEFTOKEN_SET ("[]{}~^:.+-/*%&@_<>!=?$",				 \
		    "guile-symbol-misc-chars",				 \
		    scm_read_guile_mixed_case_symbol, 0,		 \
		    "Read a symbol that starts with a non-alphanumeric " \
		    "character in a case-sensitive fashion.")
#define SCM_TR_QUOTE_QUASIQUOTE_UNQUOTE /* quote-quasiquote-unquote */	  \
  SCM_DEFTOKEN_SET ("'`,", "quote-quasiquote-unquote",			  \
		    scm_read_quote, 0,					  \
		    "Read a quote, quasiquote, or unquote S-expression.")
#define SCM_TR_R6RS_SYNTAX_QUOTE_QUASIQUOTE_UNQUOTE /* r6rs-syntax-quote-quasiquote-unquote */ \
  SCM_DEFTOKEN_SET ("'`,", "r6rs-syntax-quasiquote-unquote",		\
		    scm_read_r6rs_syntax_quote, 0,			\
		    "Read an R6RS-style syntax, quasisyntax, or unsyntax " \
		    "S-expression.")
#define SCM_TR_SEMICOLON_COMMENT /* semicolon-comment */		   \
  SCM_DEFTOKEN_SINGLE (';', "semicolon-comment",			   \
		       scm_read_semicolon_comment, 0,			   \
		       "Read an R5RS semicolon line-comment and return "   \
		       "@code{*unspecified*}.  Consequently, the calling " \
		       "reader will loop and ignore the comment.")


/* Alternative (uncommon) token readers.  */

#define SCM_TR_SKRIBE_EXP /* skribe-exp */				\
  SCM_DEFTOKEN_SINGLE ('[', "skribe-exp", scm_read_skribe_exp, 0,	\
		       "Read a Skribe markup expression.  Skribe's "	\
		       "expressions look like this:\n\n"		\
		       "@smallexample\n"				\
		       "[Hello ,(bold [World])!]\n"			\
		       "=> (\"Hello \" (bold \"World\") \"!\")\n"	\
		       "@end smallexample\n\n"				\
		       "See @uref{http://www.inria.fr@/"		\
		       "/mimosa/fp/Skribe, the Skribe web site} or "	\
		       "@uref{http://www.nongnu.org/skribilo/, the "	\
		       "Skribilo web site} for more details.")
#define SCM_TR_SQUARE_BRACKET_SEXP /* square-bracket-sexp */			\
  SCM_DEFTOKEN_SINGLE ('[', "square-bracket-sexp", scm_read_sexp, 0,		\
		       "Read an S-expression enclosed in square "		\
		       "brackets.  This is already permitted by a "		\
		       "number of Scheme implementations and will soon "	\
		       "be made compulsory by R6RS.")
#define SCM_TR_CURLY_BRACE_SEXP /* curly-brace-sexp */				\
  SCM_DEFTOKEN_SINGLE ('{', "curly-brace-sexp", scm_read_sexp, 0,		\
		       "Read an S-expression enclosed in square "		\
		       "brackets.  This is already permitted by a "		\
		       "number of Scheme implementations and will soon "	\
		       "be made compulsory by R6RS.")

/* Various flavors of symbols.  */
#define SCM_TR_R5RS_LOWER_CASE_SYMBOL_LOWER_CASE /* r5rs-lower-case-symbol-lower-case */ \
  SCM_DEFTOKEN_RANGE ('a', 'z', "r5rs-lower-case-symbol-lower-case",			 \
		      scm_read_r5rs_lower_case_symbol, 0,				 \
		      "Read a symbol that starts with a lower-case "			 \
		      "letter and return a lower-case symbol, "				 \
		      "regardless of the case of the input.")
#define SCM_TR_R5RS_LOWER_CASE_SYMBOL_UPPER_CASE /* r5rs-lower-case-symbol-upper-case */ \
  SCM_DEFTOKEN_RANGE ('A', 'Z', "r5rs-lower-case-symbol-upper-case",			 \
		      scm_read_r5rs_lower_case_symbol, 0,				 \
		      "Read a symbol that starts with an upper-case "			 \
		      "letter and return a lower-case symbol, "				 \
		      "regardless of the case of the input.")
#define SCM_TR_R5RS_LOWER_CASE_SYMBOL_MISC_CHARS /* r5rs-lower-case-symbol-misc-chars */ \
  SCM_DEFTOKEN_SET ("[]{}~^:.+-/*%&@_<>!=?$",						 \
		    "r5rs-lower-case-symbol-misc-chars",				 \
		    scm_read_r5rs_lower_case_symbol, 0,					 \
		    "Read a symbol that starts with a non-"				 \
		    "alphanumeric character and return a "				 \
		    "lower-case symbol, regardless of the "				 \
		    "case of the input.")


#define SCM_TR_R6RS_SYMBOL_LOWER_CASE /* r6rs-symbol-lower-case */	\
  SCM_DEFTOKEN_RANGE ('a', 'z', "r6rs-symbol-lower-case",		\
		      scm_read_r6rs_symbol, 0,				\
		      "Read a symbol that starts with a lower-case "	\
		      "letter and return a symbol.  This token "	\
		      "reader conforms with R6RS in that it is "	\
		      "case-sensitive and recognizes square "		\
		      "brackets as delimiters (@pxref{Token "		\
		      "Delimiters}).")
#define SCM_TR_R6RS_SYMBOL_UPPER_CASE /* r6rs-symbol-upper-case */	\
  SCM_DEFTOKEN_RANGE ('A', 'Z', "r6rs-symbol-upper-case",		\
		      scm_read_r6rs_symbol, 0,				\
		      "Read a symbol that starts with an upper-case "	\
		      "letter and return a symbol.  This token "	\
		      "reader conforms with R6RS in that it is "	\
		      "case-sensitive and recognizes square "		\
		      "brackets as delimiters (@pxref{Token "		\
		      "Delimiters}).")
#define SCM_TR_R6RS_SYMBOL_MISC_CHARS /* r6rs-symbol-misc-chars */	\
  SCM_DEFTOKEN_SET ("{}~^:.+-/*%&@_<>!=?$",				\
		    "r6rs-symbol-misc-chars",				\
		    scm_read_r6rs_symbol, 0,				\
		    "Read a symbol that starts with a non-"		\
		    "alphanumeric character and return a "		\
		    "symbol.  This token "				\
		    "reader conforms with R6RS in that it is "		\
		    "case-sensitive and recognizes square "		\
		    "brackets as delimiters (@pxref{Token "		\
		    "Delimiters}).")
#define SCM_TR_BRACE_FREE_SYMBOL_LOWER_CASE /* brace-free-symbol-lower-case */ \
  SCM_DEFTOKEN_RANGE ('a', 'z', "brace-free-symbol-lower-case",		       \
		      scm_read_brace_free_symbol, 0,			       \
		      "Read a symbol that starts with a lower-case "	       \
		      "letter and return a symbol.  This token "	       \
		      "reader recognizes braces as delimiters, "	       \
		      "unlike R5RS/R6RS.")
#define SCM_TR_BRACE_FREE_SYMBOL_UPPER_CASE /* brace-free-symbol-upper-case */	\
  SCM_DEFTOKEN_RANGE ('A', 'Z', "brace-free-symbol-upper-case",			\
		      scm_read_brace_free_symbol, 0,				\
		      "Read a symbol that starts with an upper-case "		\
		      "letter and return a symbol.  This token "		\
		      "reader recognizes braces as delimiters, "		\
		      "unlike R5RS/R6RS.")
#define SCM_TR_BRACE_FREE_SYMBOL_MISC_CHARS /* brace-free-symbol-misc-chars */ \
  SCM_DEFTOKEN_SET ("[]~^:.+-/*%&@_<>!=?$",				       \
		    "brace-free-symbol-misc-chars",			       \
		    scm_read_brace_free_symbol, 0,			       \
		    "Read a symbol that starts with a non-"		       \
		    "alphanumeric character and return a "		       \
		    "symbol.  This token "				       \
		    "reader recognizes braces as delimiters, "		       \
		    "unlike R5RS/R6RS.")

/* Flavours of numbers.  */

#define SCM_TR_R5RS_LOWER_CASE_NUMBER /* r5rs-lower-case-number */	\
  SCM_DEFTOKEN_RANGE ('0', '9', "r5rs-lower-case-number",		\
		      scm_read_r5rs_lower_case_number, 0,		\
		      "Return a number or a lower-case symbol.")
#define SCM_TR_R5RS_UPPER_CASE_NUMBER /* r5rs-upper-case-number */	\
  SCM_DEFTOKEN_RANGE ('0', '9', "r5rs-upper-case-number",		\
		      scm_read_r5rs_upper_case_number, 0,		\
		      "Return a number or an upper-case symbol.")
#define SCM_TR_R6RS_NUMBER /* r6rs-number */				\
  SCM_DEFTOKEN_RANGE ('0', '9', "r6rs-number",				\
		      scm_read_r6rs_number, 0,				\
		      "Return a number or a symbol.  This token "	\
		      "reader conforms to R6RS, i.e. it considers "	\
		      "square brackets as delimiters.")
#define SCM_TR_BRACE_FREE_NUMBER /* brace-free-number */		\
  SCM_DEFTOKEN_RANGE ('0', '9', "brace-free-number",			\
		      scm_read_brace_free_number, 0,			\
		      "Return a number or a symbol, considering "	\
		      "curly braces as delimiters.")			\


#endif
