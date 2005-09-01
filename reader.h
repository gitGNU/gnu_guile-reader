/* A Scheme reader compiler for Guile.

   Copyright (C) 2005  Ludovic Courtès  <ludovic.courtes@laas.fr>

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


#ifndef __SCM_READER_H__
#define __SCM_READER_H__

#include <libguile.h>
#include <lightning.h>

#include "reader-config.h"


#ifdef SCM_READER_USE_LIGHTNING

typedef SCM (* scm_reader_t) (SCM port);

/* Readers should *always* be called using the `scm_call_reader ()' macro
   since this may be done differently in the non-Lightning case.  */
#define scm_call_reader(_reader, _port)  ((_reader) (_port))

#else

typedef struct scm_reader *scm_reader_t;

/* In the non-Lightning case, reader invocation relies on a slower, generic,
   support function.  */
extern SCM scm_call_reader (scm_reader_t reader, SCM port);

#endif


typedef SCM (* scm_token_reader_t) (int chr, SCM port, scm_reader_t reader);

/* The way a token is defined.  */
typedef enum
  {
    SCM_TOKEN_UNDEF = 0,
    SCM_TOKEN_SINGLE,           /* single character */
    SCM_TOKEN_RANGE,            /* range of characters */
    SCM_TOKEN_SET               /* set of characters */
  } scm_token_type_t;

typedef enum
  {
    SCM_TOKEN_READER_UNDEF = 0,
    SCM_TOKEN_READER_C,         /* C function, `scm_token_reader_t' */
    SCM_TOKEN_READER_SCM,       /* Scheme procedure */
    SCM_TOKEN_READER_READER     /* reader is an `scm_reader_t' */
  } scm_token_reader_type_t;

/* Token reader specification, i.e. reader functions associated to a
   character, a range of characters, or a set of characters.  */
typedef struct scm_token_reader_spec
{
  struct
  {
    scm_token_type_t type;
    union
    {
      char single;
      struct
      {
	char low;
	char high;
      } range;
      const char *set;
    } value;
  } token;
  const char *name;

  struct
  {
    scm_token_reader_type_t type;
    union
    {
      scm_token_reader_t       c_reader;
      SCM                      scm_reader;
      scm_reader_t             reader;
    } value;
  } reader;
} scm_token_reader_spec_t;

/* A reader's specifications is just a zero-terminated array of token reader
   specs.  */
typedef scm_token_reader_spec_t *scm_reader_spec_t;


/* Return a pointer to a reader function compliant with the specifications in
   TOKEN_READERS.  If DEBUG is non-zero, debugging code is generated.  If
   CODE_BUFFER, of BUFFER_SIZE bytes, is too small to contain the generated
   code, NULL is returned and CODE_SIZE is set to the size of the generated
   code at this point.  On success, CODE_SIZE is also set to the actual size
   of the generated code.  */
extern scm_reader_t scm_c_make_reader (void *code_buffer,
				       size_t buffer_size,
				       const scm_token_reader_spec_t *specs,
				       int debug,
				       size_t *code_size);

/* Convenience macros for statically specifying C token readers.  */

#define SCM_DEFTOKEN_SINGLE(_chr, _name, _func)				\
  {									\
    { .type = SCM_TOKEN_SINGLE, .value = { .single = (_chr) } },	\
    .name = (_name),							\
    .reader = { .type = SCM_TOKEN_READER_C, .value.c_reader = (_func) }	\
  }

#define SCM_DEFTOKEN_RANGE(_lo, _hi, _name, _func)			\
  {									\
    { .type = SCM_TOKEN_RANGE,						\
      .value = { .range = { .low = (_lo), .high = (_hi) } } },		\
    .name = (_name),							\
    .reader = { .type = SCM_TOKEN_READER_C, .value.c_reader = (_func) }	\
  }

#define SCM_DEFTOKEN_SET(_set, _name, _func)				\
  {									\
    { .type = SCM_TOKEN_SET, .value = { .set = (_set) } },		\
    .name = (_name),							\
   .reader = { .type = SCM_TOKEN_READER_C, .value.c_reader = (_func) }	\
  }

#define SCM_END_TOKENS							   \
  { { .type = SCM_TOKEN_UNDEF },					   \
    .name = NULL,							   \
    .reader = { .type = SCM_TOKEN_READER_UNDEF, .value.c_reader = NULL } }


/* The SMOB type associated to `scm_reader_t', `scm_token_reader_spec_t', and
   `scm_token_reader_t'.  */
extern scm_t_bits scm_reader_type, scm_token_reader_type,
  scm_token_reader_proc_type;


#endif
