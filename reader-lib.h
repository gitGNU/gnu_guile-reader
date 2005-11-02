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

#ifndef __SCM_READER_LIB_H__
#define __SCM_READER_LIB_H__

#include <libguile.h>
#include "reader.h"
#include "token-readers.h"


extern SCM scm_default_reader (void);
extern SCM scm_default_sharp_reader (void);
extern SCM scm_default_reader_token_readers (void);
extern SCM scm_default_sharp_reader_token_readers (void);
extern SCM scm_make_guile_reader (SCM fault_handler, SCM flags);


/* Zero-terminated array of a standard Scheme reader specification.  */
extern scm_token_reader_spec_t scm_reader_standard_specs[];
extern const scm_token_reader_spec_t scm_sharp_reader_standard_specs[];

/* The offset of the sharp token reader within SCM_READER_STANDARD_SPECS.  */
#define SCM_STANDARD_READER_SHARP_OFFSET 1

/* Two standard (in Guile terms) readers compiled at initialization time.  */
extern scm_reader_t scm_standard_reader;
extern scm_reader_t scm_standard_sharp_reader;



/* Initialize guile-reader's reader library.  */
extern void scm_initialize_reader_library (void);

/* Load or compile the standard reader (and its `#' reader) declared above.
   This function is automatically called by the `(reader)' module at
   load-time.  This function is only meant to be used internally.  */
extern void scm_load_standard_reader (void);

#endif

/* arch-tag: e84bc988-2c9f-40e7-bc1c-d08b9f114d1c
 */
