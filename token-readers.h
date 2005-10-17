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


#ifndef __SCM_TOKEN_READERS_H__
#define __SCM_TOKEN_READERS_H__

/* A list of specific token readers, most of which were stolen from
   Guile.  */

#include <libguile.h>
#include "reader.h"


/* Main token readers.  */

extern SCM scm_read_sexp (int, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_string (int, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_guile_number (int, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_quote (int, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_skribe_literal (int, SCM, scm_reader_t, scm_reader_t);


/* Symbol token readers.  */
extern SCM scm_read_guile_mixed_case_symbol (int, SCM, scm_reader_t,
					     scm_reader_t);

extern SCM scm_read_r5rs_lower_case_symbol (int, SCM, scm_reader_t,
					    scm_reader_t);

extern SCM scm_read_r5rs_upper_case_symbol (int, SCM, scm_reader_t,
					    scm_reader_t);

extern SCM scm_read_r6rs_symbol (int, SCM, scm_reader_t, scm_reader_t);

extern SCM scm_read_brace_free_symbol (int, SCM, scm_reader_t, scm_reader_t);


/* Sharp token readers, should be called after `#' has been read.  */

extern SCM scm_read_extended_symbol (int, SCM, scm_reader_t, scm_reader_t);



/* Zero-terminated array of a standard Scheme reader specification.  */
extern scm_token_reader_spec_t scm_reader_standard_specs[];
extern const scm_token_reader_spec_t scm_sharp_reader_standard_specs[];
extern const scm_token_reader_spec_t scm_reader_misc_specs[];

/* Two standard (in Guile terms) readers compiled at initialization time.  */
extern scm_reader_t scm_standard_reader;
extern scm_reader_t scm_standard_sharp_reader;

/* A standard reader fault handler.  */
extern SCM scm_reader_standard_fault_handler_proc;

/* Look for a token reader named NAME in the standard token readers and
   return its specification if found, otherwise return NULL.  */
extern const scm_token_reader_spec_t *
scm_token_reader_lookup (const char *name);

/* Load or compile the standard reader (and its `#' reader) declared above.
   This function is automatically called by the `(reader)' module at
   load-time.  */
extern void scm_load_standard_reader (void);

#endif
