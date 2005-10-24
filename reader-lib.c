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


#include "reader.h"
#include "token-readers.h"
#include "reader-lib.h"

#include "config.h"



/* The Guile readers.  */

/* A reader that must be compiled at initialization-time.  */
scm_reader_t scm_standard_sharp_reader = NULL;

const scm_token_reader_spec_t scm_sharp_reader_standard_specs[] =
  {
    SCM_TR_CHARACTER,
    SCM_TR_VECTOR,
    SCM_TR_SRFI_4,
    SCM_TR_BOOLEAN,
    SCM_TR_KEYWORD,
    SCM_TR_NUMBER_AND_RADIX,
    SCM_TR_GUILE_EXTENDED_SYMBOL,
    SCM_TR_SCSH_BLOCK_COMMENT,
    SCM_END_TOKENS
  };

/* A default, Scheme-like, reader specification.  */
scm_token_reader_spec_t scm_reader_standard_specs[] =
  {
    SCM_TR_WHITESPACE,

    /* This one is defined at reader's compile-time.  Note that reader
       compilers expect to find it here!  */
    SCM_DEFTOKEN_SINGLE ('#', "sharp", NULL, 0,
			 "This is Guile's ``sharp reader'', i.e. a "
			 "reader for tokens starting with @code{#}.  "
			 "It is actually defined at reader's compile-"
			 "time."),

    SCM_TR_SEXP,
    SCM_TR_STRING,
    SCM_TR_GUILE_NUMBER,
    SCM_TR_GUILE_SYMBOL_LOWER_CASE,
    SCM_TR_GUILE_SYMBOL_UPPER_CASE,
    SCM_TR_GUILE_SYMBOL_MISC_CHARS,
    SCM_TR_QUOTE_QUASIQUOTE_UNQUOTE,
    SCM_TR_SEMICOLON_COMMENT,

    SCM_END_TOKENS
  };


/* The number of elements in SCM_READER_STANDARD_SPECS.  */
static size_t standard_reader_specs_size = 0;

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

SCM_DEFINE (scm_make_guile_reader, "make-guile-reader", 0, 0, 1,
	    (SCM flags),
	    "Make and return a new reader compatible with Guile's built-in "
	    "reader.  This function call @code{make-reader} with "
	    "@var{flags}.  Note that the sharp reader used by the returned "
	    "reader is also instantiated using @var{flags}.")
#define FUNC_NAME s_scm_make_guile_reader
{
#if 0
  SCM reader;
  scm_reader_t c_reader;
  unsigned c_flags;
  void *buffer;

  c_flags = scm_to_make_reader_flags (flags);
  buffer = scm_malloc (5000); /* should be enough */

  c_reader = scm_c_make_reader (buffer, 5000, c_token_readers,
				fault_handler, flags);
#endif
  return SCM_BOOL_F;
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
			   scm_reader_standard_fault_handler_proc, 0,
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
			   scm_reader_standard_fault_handler_proc, 0,
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

#include "reader-lib.c.x"
}

/* arch-tag: e7438c42-4225-4a7d-9957-a64b2226b4e5
 */
