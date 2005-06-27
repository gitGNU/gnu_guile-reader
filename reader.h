/* A dynamic Scheme reader compiler.

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


typedef SCM (* scm_reader_t) (SCM port);
typedef SCM (* scm_token_reader_t) (int chr, SCM port, scm_reader_t reader);

typedef enum
  {
    SCM_TOKEN_UNDEF,
    SCM_TOKEN_SINGLE,
    SCM_TOKEN_RANGE,
    SCM_TOKEN_SET,
  } scm_token_type_t;

/* Token reader specification, i.e. reader functions associated to a
   character, a range of characters, or a set of characters.  */
typedef struct
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
  unsigned    is_scheme_proc:1;
  union
  {
    scm_token_reader_t c_reader;
    SCM                scm_reader;
  } reader;
} scm_token_reader_spec_t;

/* A reader's specifications is just a zero-terminated array of token reader
   specs.  */
typedef scm_token_reader_spec_t *scm_reader_spec_t;


/* Return a pointer to a reader function compliant with the specifications in
   WHITESPACES and TOKEN_READERS.  If DEBUG is non-zero, debugging code is
   generated.  If CODE_BUFFER, of BUFFER_SIZE bytes, is too small to contain
   the generated code, NULL is returned and CODE_SIZE is set to the size of
   the generated code at this point.  On success, CODE_SIZE is also set to
   the actual size of the generated code.  */
extern scm_reader_t scm_c_make_reader (jit_insn *code_buffer,
				       size_t buffer_size,
				       const char *whitespaces,
				       const scm_token_reader_spec_t *specs,
				       int debug,
				       size_t *code_size);

/* Convenience macros for statically specifying C token readers.  */

#define SCM_DEFTOKEN_SINGLE(_chr, _name, _func)				\
  {									\
    { .type = SCM_TOKEN_SINGLE, .value = { .single = (_chr) } },	\
    .is_scheme_proc = 0,						\
    .name = (_name),							\
    .reader = { .c_reader = (_func) }					\
  }

#define SCM_DEFTOKEN_RANGE(_lo, _hi, _name, _func)		\
  {								\
    { .type = SCM_TOKEN_RANGE,					\
      .value = { .range = { .low = (_lo), .high = (_hi) } } },	\
    .name = (_name),						\
    .is_scheme_proc = 0,					\
    .reader = { .c_reader = (_func) }				\
  }

#define SCM_DEFTOKEN_SET(_set, _name, _func)			\
  {								\
    { .type = SCM_TOKEN_SET, .value = { .set = (_set) } },	\
    .is_scheme_proc = 0,					\
    .name = (_name),						\
   .reader = { .c_reader = (_func) }				\
  }

#define SCM_END_TOKENS					\
  { { .type = SCM_TOKEN_UNDEF },			\
    .name = NULL, .reader = { .c_reader = NULL } }


/* The SMOB type associated to `scm_reader_t'.  */
extern scm_t_bits scm_reader_type;


#endif
