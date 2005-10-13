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


#ifndef __SCM_READER_H__
#define __SCM_READER_H__

#include <libguile.h>

#include "reader-config.h"


#ifdef SCM_READER_USE_LIGHTNING

#include <lightning.h>

typedef SCM (* scm_reader_t) (SCM port, int caller_handled);

/* Invoke READER reading from port PORT.  If CALLER_HANDLED is non-zero, then
   read faults (reading an unhandled character) will be handled by the caller
   and READER should therefore not make any attempt to handle them.

   Readers should *always* be called using the `scm_call_reader ()' macro
   since this may be done differently in the non-Lightning case.  */
#define scm_call_reader(_reader, _port, _caller_handled)	\
  ((_reader) ((_port), (_caller_handled)))

#else

typedef struct scm_reader *scm_reader_t;

/* In the non-Lightning case, reader invocation relies on a slower, generic,
   support function.  */
extern SCM scm_call_reader (scm_reader_t reader, SCM port,
			    int caller_handled);

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

  /* If true, then the reader this TR belongs to should return even if
     SCM_UNSPECIFIED is returned.  */
  unsigned escape:1;
} scm_token_reader_spec_t;

/* A reader's specifications is just a zero-terminated array of token reader
   specs.  */
typedef scm_token_reader_spec_t *scm_reader_spec_t;


/* Flags that may be passed to `scm_c_make_reader ()'.  */
#define SCM_READER_FLAG_DEBUG           0x01 /* output debugging info */
#define SCM_READER_FLAG_POSITIONS       0x02 /* record source position */

/* Return a pointer to a reader function compliant with the specifications in
   TOKEN_READERS.  FLAGS should be a logical or of the `SCM_READER_FLAG_'
   macros.  If FAULT_HANDLER_PROC is a procedure, then the generated reader
   will call it whenever a character is read that is not handled (passing it
   the faulty character, the port, and the reader).  If CODE_BUFFER, of
   BUFFER_SIZE bytes, is too small to contain the generated code, NULL is
   returned and CODE_SIZE is set to the size of the generated code at this
   point.  On success, CODE_SIZE is also set to the actual size of the
   generated code.  */
extern scm_reader_t scm_c_make_reader (void *code_buffer,
				       size_t buffer_size,
				       const scm_token_reader_spec_t *specs,
				       SCM fault_handler_proc,
				       unsigned flags,
				       size_t *code_size);

/* Scheme version of `scm_c_make_reader ()'.  TOKEN_READERS should be a list
   of token readers (returned by `make-token-reader' or
   `standard-token-reader' for instance).  The second argument is optional
   and may be a three-argument procedure to call when an unexpected character
   is read.  FLAGS is a rest argument which may contain a list of symbols
   representing reader compilation flags.  */
extern SCM scm_make_reader (SCM token_readers,
			    SCM fault_handler_proc,
			    SCM flags);

/* Convert FLAGS, a list of symbols representing flags for `make-reader', to
   their C representation (which may be passed to `scm_c_make_reader
   ()').  */
extern unsigned scm_to_make_reader_flags (SCM flags);


/* Return a Scheme representation of the token reader specified by TR.  If
   CALLER_OWNED is non-zero, it is assumed that C code will keep control over
   the resources held by TR (notably the memory pointed to by TR).  If
   CALLER_OWNED is zero, then garbage collection of the returned SMOB will
   yield to the resources and memory pointed to by TR being freed.  */
extern SCM scm_from_token_reader (const scm_token_reader_spec_t *tr,
				  int caller_owned);

/* Return a pointer to a newly allocated C representation of token reader TR.
   The returned object may eventually be freed using `free ()', as well as
   the `token.value.set' pointer if `token.type' is SCM_TOKEN_SET.  */
extern scm_token_reader_spec_t *scm_to_token_reader (SCM tr);

/* Create a token reader and return its Scheme representation.  SPEC
   represents its specifications, i.e. when this token reader should be
   triggered.  SPEC may be either a single character, or a pair or characters
   (representing a range), or a list of characters (representing a set).  If
   PROC is not #f, then it is the procedure that will be invoked.  Finally,
   ESCAPE_P is a boolean that determines whether the token reader may
   ``escape'' its parent reader returning SCM_UNSPECIFIED.  */
extern SCM scm_make_token_reader (SCM spec, SCM proc, SCM escape_p);


/* Convenience macros for statically specifying C token readers.  */

#define SCM_DEFTOKEN_SINGLE(_chr, _name, _func, _escape)		\
  {									\
    { .type = SCM_TOKEN_SINGLE, .value = { .single = (_chr) } },	\
    .name = (_name),							\
    .reader = { .type = SCM_TOKEN_READER_C,				\
		.value.c_reader = (_func) },				\
    .escape = _escape							\
  }

#define SCM_DEFTOKEN_RANGE(_lo, _hi, _name, _func, _escape)	\
  {								\
    { .type = SCM_TOKEN_RANGE,					\
      .value = { .range = { .low = (_lo), .high = (_hi) } } },	\
    .name = (_name),						\
    .reader = { .type = SCM_TOKEN_READER_C,			\
		.value.c_reader = (_func) },			\
    .escape = _escape						\
  }

#define SCM_DEFTOKEN_SET(_set, _name, _func, _escape)		\
  {								\
    { .type = SCM_TOKEN_SET, .value = { .set = (_set) } },	\
    .name = (_name),						\
   .reader = { .type = SCM_TOKEN_READER_C,			\
	       .value.c_reader = (_func) },			\
   .escape = _escape						\
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
