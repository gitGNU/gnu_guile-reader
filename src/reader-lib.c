/* A Scheme reader compiler for Guile.

   Copyright (C) 2005, 2007, 2008, 2009, 2012  Ludovic Courtès <ludo@gnu.org>

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


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "reader.h"
#include "token-readers.h"
#include "reader-lib.h"

#include <string.h>
#include <assert.h>



/* The Guile readers.  */

/* A reader that must be compiled at initialization-time.  */
scm_reader_t scm_standard_sharp_reader = NULL;

const scm_token_reader_spec_t scm_sharp_reader_standard_specs[] =
  {
    SCM_TR_CHARACTER,
    SCM_TR_VECTOR,
    SCM_TR_SRFI_4,
    SCM_TR_GUILE_BIT_VECTOR,
    SCM_TR_BOOLEAN_SRFI_4,
    SCM_TR_KEYWORD,
    SCM_TR_NUMBER_AND_RADIX,
    SCM_TR_GUILE_EXTENDED_SYMBOL,
    SCM_TR_SCSH_BLOCK_COMMENT,

    /* The following syntax is supported by default in Guile 1.9 and later
       only, but we always enable it, so that Guile-Reader can read the same
       source regardless of the underlying version of Guile.  */
    SCM_TR_R6RS_SYNTAX_QUOTE_QUASIQUOTE_UNQUOTE,
    SCM_TR_SRFI62_SEXP_COMMENT,

    SCM_END_TOKENS
  };

/* A default, Scheme-like, reader specification.  */
scm_token_reader_spec_t scm_reader_standard_specs[] =
  {
    SCM_TR_WHITESPACE,

    /* This one is defined at reader's compile-time.  Note that reader
       compilers expect to find it here!  See the definition of
       SCM_STANDARD_READER_SHARP_OFFSET.  */
    SCM_DEFTOKEN_SINGLE ('#', "sharp", NULL, 0,
			 "This is Guile's ``sharp reader'', i.e. a "
			 "reader for tokens starting with @code{#}.  "
			 "It is actually defined at reader's compile-"
			 "time."),

    SCM_TR_STRING,
    SCM_TR_SEXP,

    /* Guile 2.0 recognizes square brackets by default, so follow
       its lead.  */
    SCM_TR_SQUARE_BRACKET_SEXP,
    SCM_TR_R6RS_NUMBER,
    SCM_TR_R6RS_SYMBOL_LOWER_CASE,
    SCM_TR_R6RS_SYMBOL_UPPER_CASE,
    SCM_TR_R6RS_SYMBOL_MISC_CHARS,

    SCM_TR_QUOTE_QUASIQUOTE_UNQUOTE,
    SCM_TR_SEMICOLON_COMMENT,

    SCM_END_TOKENS
  };


/* The number of elements in SCM_READER_STANDARD_SPECS.  */
static size_t standard_reader_specs_size = 0;

#ifdef DEBUG
# define SHARP_READER_SIZE      20000
# define TOP_LEVEL_READER_SIZE  25000
#else
# define SHARP_READER_SIZE      4000
# define TOP_LEVEL_READER_SIZE  5000
#endif

SCM_DEFINE (scm_default_reader, "default-reader", 0, 0, 0,
	    (void),
	    "Returns a reader compatible with Guile's built-in "
	    "reader.")
{
  SCM s_reader;

  /* This one may _not_ be freed by Scheme code at GC time.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, scm_standard_reader,
		       NULL, 0);
  return (s_reader);
}

SCM_DEFINE (scm_default_sharp_reader, "default-sharp-reader", 0, 0, 0,
	    (void),
	    "Returns Guile's default reader for the @code{#} character.")
{
  SCM s_reader;

  /* This one may _not_ be freed by Scheme code at GC time.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type,
		       scm_standard_sharp_reader, NULL, 0);
  return (s_reader);
}

SCM_DEFINE (scm_default_reader_token_readers,
	    "default-reader-token-readers", 0, 0, 0,
	    (void),
	    "Return the list of token readers that comprise "
	    "Guile's default reader.")
{
  return scm_from_reader_spec (scm_reader_standard_specs, 1);
}

SCM_DEFINE (scm_default_sharp_reader_token_readers,
	    "default-sharp-reader-token-readers", 0, 0, 0,
	    (void),
	    "Return the list of token readers that comprise "
	    "Guile's default reader for the @code{#} character.")

{
  return scm_from_reader_spec (scm_sharp_reader_standard_specs, 1);
}

SCM_DEFINE (scm_make_guile_reader, "make-guile-reader", 0, 1, 1,
	    (SCM fault_handler, SCM flags),
	    "Make and return a new reader compatible with Guile 2.0's "
	    "@code{read}, with its default settings.  This function calls "
	    "@code{make-reader} with @var{flags}.  Note that the sharp "
	    "reader used by the returned reader is also instantiated using "
	    "@var{flags}.  The value of @var{fault-handler} defaults to "
	    "@code{%reader-standard-fault-handler}.")
#define FUNC_NAME s_scm_make_guile_reader
{
  SCM s_reader, *s_deps;
  scm_reader_t c_reader, c_sharp_reader;
  scm_token_reader_spec_t *c_specs;
  unsigned c_flags;
  size_t code_size;
  void *buffer;

  if (SCM_UNBNDP (fault_handler))
    fault_handler = scm_variable_ref (scm_reader_standard_fault_handler_var);

  if (fault_handler != SCM_BOOL_F)
    SCM_VALIDATE_PROC (1, fault_handler);

  c_flags = scm_to_make_reader_flags (flags);

  /* Build a brand new sharp reader.  Should be enough for both readers.  */
  buffer = scm_malloc (SHARP_READER_SIZE + TOP_LEVEL_READER_SIZE);
  c_sharp_reader = scm_c_make_reader ((char *) buffer + TOP_LEVEL_READER_SIZE,
				      SHARP_READER_SIZE,
				      scm_sharp_reader_standard_specs,
				      fault_handler, c_flags,
				      &code_size);
  assert (c_sharp_reader);
  assert (code_size <= SHARP_READER_SIZE);

  /* Get a local copy of the reader specs and change the sharp token reader. */
  c_specs = alloca ((standard_reader_specs_size + 1) * sizeof (*c_specs));
  memcpy (c_specs, scm_reader_standard_specs,
	  (standard_reader_specs_size + 1) * sizeof (*c_specs));
  assert (c_specs[SCM_STANDARD_READER_SHARP_OFFSET].token.value.single
	  == '#');
  c_specs[SCM_STANDARD_READER_SHARP_OFFSET].reader.type =
    SCM_TOKEN_READER_READER;
  c_specs[SCM_STANDARD_READER_SHARP_OFFSET].reader.value.reader =
    c_sharp_reader;

  /* Build the top-level reader.  */
  c_reader = scm_c_make_reader (buffer, TOP_LEVEL_READER_SIZE, c_specs,
				fault_handler, c_flags,
				&code_size);
  assert (c_reader);
  assert (code_size <= TOP_LEVEL_READER_SIZE);

  if (fault_handler != SCM_BOOL_F)
    {
      /* Prepare a (small) list of GC dependencies.  */
      s_deps = scm_malloc (2 * sizeof (*s_deps));
      s_deps[0] = fault_handler;
      s_deps[1] = SCM_BOOL_F;
    }
  else
    /* No GC dependencies.  */
    s_deps = NULL;

  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, c_reader,
		       s_deps, 1);
  return s_reader;
}
#undef FUNC_NAME



/* The R5RS reader.  */


/* The R6RS reader.  */


/* Initialization.  */


static char standard_reader_code[8000];
static char standard_sharp_reader_code[4000];

/* The standard reader (which depends on the standard keyword reader),
   compiled at initialization time.  */
scm_reader_t scm_standard_reader = NULL;


void
scm_load_standard_reader (void)
{
  /* XXX  Ultimately, we might want to simply mmap a file containing the
     pre-compiled readers.  But in fact, that won't work because we won't be
     able to relocate pointers to non-reader controlled data structures, like
     pointers to C functions or to other readers.  */

  size_t code_size = 0;

  if (!scm_standard_sharp_reader)
    {
      scm_standard_sharp_reader =
	scm_c_make_reader (standard_sharp_reader_code,
			   sizeof (standard_sharp_reader_code),
			   scm_sharp_reader_standard_specs,
			   scm_variable_ref (scm_reader_standard_fault_handler_var),
			   0,
			   &code_size);
    }

  if (!scm_standard_reader)
    {
      scm_token_reader_spec_t *sharp_tr;

      /* Replace the sharp reader.  */
      sharp_tr = &scm_reader_standard_specs[SCM_STANDARD_READER_SHARP_OFFSET];
      sharp_tr->reader.type = SCM_TOKEN_READER_READER;
      sharp_tr->reader.value.reader = scm_standard_sharp_reader;

      scm_standard_reader =
	scm_c_make_reader (standard_reader_code,
			   sizeof (standard_reader_code),
			   scm_reader_standard_specs,
			   scm_variable_ref (scm_reader_standard_fault_handler_var),
			   0,
			   &code_size);
    }
}


void
scm_initialize_reader_library (void)
{
  scm_token_reader_spec_t *tr;

  /* Precompute the size of the SCM_STANDARD_READER_SPECS vector.  */
  for (tr = scm_reader_standard_specs, standard_reader_specs_size = 0;
       tr->token.type != SCM_TOKEN_UNDEF;
       tr++, standard_reader_specs_size++);

#include "reader-lib.x"
}

/* arch-tag: e7438c42-4225-4a7d-9957-a64b2226b4e5
 */
