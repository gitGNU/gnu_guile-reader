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


#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

#include <lightning.h>
#include <libguile.h>

#include "reader.h"
#include "token-readers.h"




/* SMOB helper data structures and macros.  */

/* The structure that bridges SMOB and C objects.  The point of this
   structure is to have knowledge as to whether the SMOB's underlying C
   object is was created from Scheme (and is therefore freeable) or not.  */
typedef struct
{
  void *c_object;   /* the underlying C object */
  int   freeable;   /* whether the underlying C object should be freed when
		       the SMOB is GC'd */
} scm_reader_smob_t;


#define SCM_NEW_READER_SMOB(_smob, _smob_type, _c_obj, _freeable)	\
do									\
{									\
  scm_reader_smob_t *_smobinfo;						\
  _smobinfo = scm_malloc (sizeof (*_smobinfo));				\
  _smobinfo->c_object = (void *)(_c_obj);				\
  _smobinfo->freeable = (_freeable);					\
  SCM_NEWSMOB (_smob, _smob_type, _smobinfo);				\
}									\
while (0)

#define SCM_READER_SMOB_DATA(_data, _smob)			\
do								\
{								\
  scm_reader_smob_t *_smobinfo;					\
  _smobinfo = (scm_reader_smob_t *)SCM_SMOB_DATA (_smob);	\
  (_data) = (scm_reader_t)_smobinfo->c_object;			\
}								\
while(0)

#define SCM_TOKEN_READER_SMOB_DATA(_data, _smob)		\
do								\
{								\
  scm_reader_smob_t *_smobinfo;					\
  _smobinfo = (scm_reader_smob_t *)SCM_SMOB_DATA (_smob);	\
  (_data) = (scm_token_reader_spec_t *)_smobinfo->c_object;	\
}								\
while(0)



/* Fill BUFFER with an ASCII representation of TR.  BUFFER must be at least
   20-byte long.  */
static inline void
token_spec_to_string (const scm_token_reader_spec_t *tr,
		      char *buffer)
{
  switch (tr->token.type)
    {
    case SCM_TOKEN_SINGLE:
      sprintf (buffer, "char 0x%02x", tr->token.value.single);
      break;

    case SCM_TOKEN_RANGE:
      sprintf (buffer, "range 0x%02x-0x%02x",
	       tr->token.value.range.low, tr->token.value.range.high);
      break;

    case SCM_TOKEN_SET:
      {
	char first, last;
	const char *p;

	first = last = *tr->token.value.set;
	for (p = tr->token.value.set; *p; p++)
	  last = *p;

	sprintf (buffer, "set 0x%02x..0x%02x", first, last);
	break;
      }

    default:
      sprintf (buffer, "<invalid 0x%02x>", tr->token.type);
    }
}

#if 1 /* HAVE_LIGHTNING_H */

/* The Lightning-based implementation of `scm_c_make_reader ()'.  */

static void
do_like_printf (const char *fmt, int i)
{
#if 0
  printf ("i=%i\n", i);
  printf ("fmt=\"%s\"\n", fmt);
#endif
  printf (fmt, i);
}

/* Make sure we are not producing more code than can fit in the user's
   buffer.  This is somewhat dangerous.  (XXX)  */
#define CHECK_CODE_SIZE(_max, _start)			\
do							\
{							\
  if (jit_get_ip ().ptr - (_start) + 40 >= (_max))	\
    {							\
      *code_size = jit_get_ip ().ptr - (_start);	\
      assert (*code_size <= (_max));			\
      return NULL;					\
    }							\
} while (0)

/* Generate code the calls FUNC, a printf-like function, where PTR1 points to
   an immediate format string containing either `%s' or `%p', and PTR2 is a
   pointer (immediate value).  REG1 (resp. REG2) is the register where PTR1
   (resp. PTR2) is stored.  */
#define DO_DEBUG_2_PP(_ptr1, _reg1, _ptr2, _reg2, _func)	\
do								\
{								\
  jit_movi_p ((_reg1), (_ptr1));				\
  jit_movi_p ((_reg2), (_ptr2));				\
  jit_prepare (2);						\
  jit_pusharg_p ((_reg2));					\
  jit_pusharg_p ((_reg1));					\
  jit_finish ((_func));						\
} while (0)



static SCM
do_scm_make_char (int chr)
{
#ifdef DEBUG
  printf ("%s: got char %i\n", __FUNCTION__, chr);
#endif
  return SCM_MAKE_CHAR (chr);
}

/* The guts of the system: compiles a reader to native code.  */
scm_reader_t
scm_c_make_reader (void *code_buffer,
		   size_t buffer_size,
		   const char *whitespaces,
		   const scm_token_reader_spec_t *token_readers,
		   int debug,
		   size_t *code_size)
{
  static const char the_string[] = "read char: 0x%02x\n";
  static const char the_other_string[] = "read token 0x%02x!\n";
  static const char start_c_call_string[] = "calling token reader `%s'...\n";
  static const char start_scm_call_string[] =
    "calling Scheme token reader for %s...\n";
  static const char start_reader_call_string[] =
    "calling reader for %s...\n";
  static const char end_call_string[] = "token reader for %s finished\n";

  scm_reader_t result;
  char *start, *end;
  const char *ws;
  jit_insn *jumps[20], *ref, *do_again, *jump_to_end;
  size_t jump_count = 0;
  const scm_token_reader_spec_t *tr;
  SCM in;

  result = (scm_reader_t) (jit_set_ip (code_buffer).iptr);
  start = jit_get_ip ().ptr;

  /* Take one argument (the port) and put it into V0 (preserved accross
     function calls).  */
  jit_prolog (1);
  in = (void *)jit_arg_p ();
  jit_getarg_p (JIT_V0, in);

  do_again = jit_get_label ();

  /* Call `scm_getc ()'.  */
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  jit_finish (scm_getc);

  /* Test whether we got `EOF'.  */
  jump_to_end = jit_beqi_i (jit_forward (), JIT_RET, (int)EOF);

  /* Put the character just read in V1 (preserved accross function
     calls).  */
  jit_movr_i (JIT_V1, JIT_RET);

  CHECK_CODE_SIZE (buffer_size, start);

  if (debug)
    {
      /* Print a debug message.  */
      jit_movi_p (JIT_R0, the_string);
      jit_prepare (2);
      jit_pusharg_i (JIT_V1);
      jit_pusharg_p (JIT_R0);
      jit_finish (do_like_printf);
    }

  /* Generate code that skips whitespaces.  */
  ref = NULL;
  if (whitespaces)
    for (ws = whitespaces; *ws; ws++)
      {
	if (ref)
	  jit_patch (ref);

	ref = jit_bnei_i (jit_forward (), JIT_V1, (int)*ws);

	/* A whitespace was read, jump to `do_again'.  */
	jit_jmpi (do_again);
      }

  if (ref)
    jit_patch (ref);

  for (tr = token_readers;
       tr->token.type != SCM_TOKEN_UNDEF;
       tr++)
    {
      CHECK_CODE_SIZE (buffer_size, start);

      if (jump_count)
	{
	  /* Resolve the previous forward references.  */
	  unsigned j;
	  for (j = 0; j < jump_count; j++)
	    jit_patch (jumps[j]);
	  jump_count = 0;
	}

      switch (tr->token.type)
	{
	case SCM_TOKEN_SINGLE:
	  /* Compare TR's single-char token against V1.  */
	  jumps[jump_count++] =
	    jit_bnei_i (jit_forward (), JIT_V1,
			(int)tr->token.value.single);
	  break;

	case SCM_TOKEN_RANGE:
	  /* Test whether V1 is within TR's range.  */
	  {
	    char low, high;

	    if (tr->token.value.range.low
		< tr->token.value.range.high)
	      low = tr->token.value.range.low,
		high = tr->token.value.range.high;
	    else
	      low = tr->token.value.range.high,
		high = tr->token.value.range.low;

	    jumps[jump_count++] =
	      jit_blti_i (jit_forward (), JIT_V1, (int)low);
	    jumps[jump_count++] =
	      jit_bgti_i (jit_forward (), JIT_V1, (int)high);
	  }
	  break;

	case SCM_TOKEN_SET:
	  {
	    /* Test whether V1 is part of TR's set.  */
	    const char *tok;
	    jit_insn *go_next_jumps[256];
	    size_t go_next_jump_count = 0, j;

	    if (debug)
	      printf ("%s: token set contains %u chars\n",
		      __FUNCTION__, strlen (tr->token.value.set));

	    ref = NULL;
	    for (tok = tr->token.value.set;
		 *tok;
		 tok++)
	      {
		CHECK_CODE_SIZE (buffer_size, start);

		if (ref)
		  jit_patch (ref);

		ref = jit_bnei_i (jit_forward (), JIT_V1, (int)*tok);

		/* Add a jump instruction to where processing takes
		   place.  */
		go_next_jumps[go_next_jump_count++] =
		  jit_jmpi (jit_forward ());
	      }

	    if (ref)
	      jumps[jump_count++] = ref;

	    for (j = 0; j < go_next_jump_count; j++)
	      jit_patch (go_next_jumps[j]);

	    break;
	  }

	default:
	  fprintf (stderr, "%s: unknown token type: %i\n",
		   __FUNCTION__, (int)tr->token.type);
	  abort ();
	}

      /* The code that gets executed when the character returned by `scm_getc
	 ()' is equal to that of TOKEN_READER.  */
      CHECK_CODE_SIZE (buffer_size, start);
      if (debug)
	{
	  jit_movi_p (JIT_R0, the_other_string);
	  jit_prepare (2);
	  jit_pusharg_i (JIT_V1);
	  jit_pusharg_p (JIT_R0);
	  jit_finish (do_like_printf);
	  CHECK_CODE_SIZE (buffer_size, start);
	}

      switch (tr->reader.type)
	{
	case SCM_TOKEN_READER_C:
	  if (tr->reader.value.c_reader)
	    {
	      /* Call the C reader function associated with this
		 character.  */
	      const char *name = (tr->name ? tr->name : "<nameless>");
	      void *func = (void *)tr->reader.value.c_reader;

	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  DO_DEBUG_2_PP (start_c_call_string, JIT_R0,
				 name, JIT_R1, do_like_printf);
		}

	      CHECK_CODE_SIZE (buffer_size, start);
	      jit_movi_p (JIT_R0, start);
	      jit_prepare (3);
	      jit_pusharg_p (JIT_R0); /* reader */
	      jit_pusharg_p (JIT_V0); /* port */
	      jit_pusharg_i (JIT_V1); /* character */
	      jit_finish (func);

	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  jit_movr_p (JIT_V2, JIT_RET);
		  DO_DEBUG_2_PP (end_call_string, JIT_R0,
				 name, JIT_R1, do_like_printf);
		  jit_movr_p (JIT_RET, JIT_V2);
		}

	      /* When the reader returns SCM_UNSPECIFIED, then start again.
		 This is what, for instance, comment readers do.  */
	      CHECK_CODE_SIZE (buffer_size, start);
	      jit_beqi_p (do_again, JIT_RET, (void *)SCM_UNSPECIFIED);
	    }
	  else
	    jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);

	  break;

	case SCM_TOKEN_READER_SCM:
	  /* Call the Scheme proc associated with this character.  */
	  if (scm_procedure_p (tr->reader.value.scm_reader) == SCM_BOOL_T)
	    {
	      char spec_str[60];
	      SCM s_reader;

	      SCM_NEWSMOB (s_reader, scm_reader_type, start);

	      CHECK_CODE_SIZE (buffer_size, start);
	      token_spec_to_string (tr, spec_str);
	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  DO_DEBUG_2_PP (start_scm_call_string, JIT_R0,
				 spec_str, JIT_R1, do_like_printf);
		}

	      CHECK_CODE_SIZE (buffer_size, start);

	      /* Convert the character read to a Scheme char.  */
	      jit_prepare (1);
	      jit_pusharg_i (JIT_V1);
	      jit_finish (do_scm_make_char);
	      jit_movr_p (JIT_R2, JIT_RET);

	      /* Actually call the Scheme procedure.  */
	      CHECK_CODE_SIZE (buffer_size, start);
	      jit_movi_p (JIT_R0, (void *)s_reader);
	      jit_movi_p (JIT_R1, (void *)tr->reader.value.scm_reader);
	      jit_prepare (4);
	      CHECK_CODE_SIZE (buffer_size, start);
	      jit_pusharg_p (JIT_R0); /* reader */
	      jit_pusharg_p (JIT_V0); /* port */
	      jit_pusharg_p (JIT_R2); /* character */
	      jit_pusharg_p (JIT_R1); /* procedure */
	      jit_finish (scm_call_3);

	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  jit_movr_p (JIT_V2, JIT_RET);
		  DO_DEBUG_2_PP (end_call_string, JIT_R0,
				 spec_str, JIT_R1, do_like_printf);
		  jit_movr_p (JIT_RET, JIT_V2);
		}
	    }
	  else
	    jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);

	  break;

	case SCM_TOKEN_READER_READER:
	  {
	    /* A simple C function call.  */
	    char spec_str[60];

	    token_spec_to_string (tr, spec_str);
	    if (debug)
	      {
		CHECK_CODE_SIZE (buffer_size, start);
		DO_DEBUG_2_PP (start_reader_call_string, JIT_R0,
			       spec_str, JIT_R1, do_like_printf);
	      }

	    CHECK_CODE_SIZE (buffer_size, start);
	    jit_prepare (1);
	    jit_pusharg_p (JIT_V0); /* port */
	    jit_finish (tr->reader.value.reader);

	    if (debug)
	      {
		CHECK_CODE_SIZE (buffer_size, start);
		jit_movr_p (JIT_V2, JIT_RET);
		DO_DEBUG_2_PP (end_call_string, JIT_R0,
			       spec_str, JIT_R1, do_like_printf);
		jit_movr_p (JIT_RET, JIT_V2);
	      }
	  }
	  break;

	default:
	  return NULL;
	}

      /* The reader's return value is already in JIT_RET, so we just have to
	 return.  */
      jit_ret ();
    }

  if (jump_count)
    {
      /* Resolve the previous forward references.  */
      unsigned j;
      for (j = 0; j < jump_count; j++)
	jit_patch (jumps[j]);
      jump_count = 0;
    }

  /* When this point is reached, then no handler was specified for the
     character we just read.  So we return it to PORT.  */
  CHECK_CODE_SIZE (buffer_size, start);
  jit_prepare (2);
  jit_pusharg_p (JIT_V0);
  jit_pusharg_i (JIT_V1);
  jit_finish (scm_ungetc);

  jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
  jit_ret ();

  /* This is where we get when `scm_getc ()' returned EOF.  */
  jit_patch (jump_to_end);
  jit_movi_p (JIT_RET, (void *)SCM_EOF_VAL);
  jit_ret ();

  end = jit_get_ip ().ptr;
  jit_flush_code (start, end);

  *code_size = end - start;
  printf ("generated %u bytes of code\n", end - start);

  return result;
}


#else /* HAVE_LIGHTNING_H */

/* The slow Lightning-free implementation of `scm_c_make_reader ()' and
   `scm_call_reader ()'.  */

struct scm_reader
{
  char *whitespaces;
  scm_token_reader_spec_t *token_readers;
};

scm_reader_t
scm_c_make_reader (void *code_buffer,
		   size_t buffer_size,
		   const char *whitespaces,
		   const scm_token_reader_spec_t *token_readers,
		   int debug,
		   size_t *code_size)
{
  char *ws;
  struct scm_reader *result;
  scm_token_reader_spec_t *tr_copy;
  const scm_token_reader_spec_t *tr;
  unsigned char *buffer = code_buffer;

  *code_size = sizeof (*result) + strlen (whitespaces) + 1;
  if (buffer_size < sizeof (*result) + strlen (whitespaces) + 1)
    return NULL;

  result = (struct scm_reader *)buffer;
  ws = buffer + sizeof (*result);
  tr_copy = (scm_token_reader_spec_t *)(ws + strlen (whitespaces) + 1);

  result->whitespaces = ws;
  result->token_readers = tr_copy;

  memcpy (ws, whitespaces, strlen (whitespaces));

  for (tr = token_readers;
       tr->token.type != SCM_TOKEN_UNDEF;
       tr++, tr_copy++, *code_size += sizeof (*tr))
    {
      if (*code_size + sizeof (*tr) > buffer_size)
	return NULL;

      memcpy (tr_copy, tr, sizeof (*tr));
    }

  /* Copy the terminating zero.  */
  if (*code_size + sizeof (*tr) > buffer_size)
    return NULL;

  memcpy (tr_copy, tr, sizeof (*tr));
  *code_size += sizeof (*tr);

  return result;
}

static inline int
tr_handles_char (const scm_token_reader_spec_t *tr, char c)
{
  switch (tr->token.type)
    {
    case SCM_TOKEN_SINGLE:
      return (c == tr->token.value.single);

    case SCM_TOKEN_RANGE:
      return ((c >= tr->token.value.range.low)
	      && (c <= tr->token.value.range.high));

    case SCM_TOKEN_SET:
      return (index (tr->token.value.set, c) ? 1 : 0);

    default:
      return 0;
    }

  return 0;
}

static SCM
tr_invoke (const scm_token_reader_spec_t *tr, char c, SCM port,
	   scm_reader_t reader)
{
  switch (tr->reader.type)
    {
    case SCM_TOKEN_READER_C:
      return tr->reader.value.c_reader (c, port, reader);

    case SCM_TOKEN_READER_SCM:
      {
	SCM s_reader;
	SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, 0);
	return scm_call_3 (tr->reader.value.scm_reader,
			   SCM_MAKE_CHAR (c), port, s_reader);
      }

    case SCM_TOKEN_READER_READER:
      return scm_call_reader (tr->reader.value.reader, port);

    default:
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}

SCM
scm_call_reader (scm_reader_t reader, SCM port)
{
  int c = 0;
  scm_token_reader_spec_t *tr;
  SCM result = SCM_UNSPECIFIED;

 doit:
  while ((c = scm_getc (port)) != EOF)
    {
      if (index (reader->whitespaces, c))
	continue;

      for (tr = reader->token_readers;
	   tr->token.type != SCM_TOKEN_UNDEF;
	   tr++)
	{
	  if (tr_handles_char (tr, c))
	    {
	      result = tr_invoke (tr, c, port, reader);
	      if (result == SCM_UNSPECIFIED)
		goto doit;
	      else
		return result;
	    }
	}

      /* Unhandled character.  */
      return SCM_UNSPECIFIED;
    }

  return SCM_EOF_VAL;
}


#endif /* HAVE_LIGHTNING_H */



/* Wrapping/unwrapping methods.  */


/* Read token specifier SPEC and update TR accordingly.  On error (i.e. if
   SPEC is not a valid specification), return non-zero.  This has to be in
   sync with `token-reader-spec'.  */
static int
read_token_spec (SCM spec, scm_token_reader_spec_t *tr)
{
  if (SCM_CHARP (spec))
    {
      tr->token.type = SCM_TOKEN_SINGLE;
      tr->token.value.single = SCM_CHAR (spec);
    }
  else if (scm_list_p (spec) == SCM_BOOL_T)
    {
      SCM s_set;
      s_set = scm_string (spec);
      tr->token.type = SCM_TOKEN_SET;
      tr->token.value.set = scm_to_locale_string (s_set);
    }
  else if (scm_is_pair (spec))
    {
      tr->token.type = SCM_TOKEN_RANGE;
      tr->token.value.range.low = SCM_CHAR (SCM_CAR (spec));
      tr->token.value.range.high = SCM_CHAR (SCM_CDR (spec));
    }
  else
    return 1;

  return 0;
}


SCM
scm_from_reader (scm_reader_t reader)
{
  SCM s_reader;

  /* Return a "non-freeable" reader SMOB, i.e. leave control over memory
     allocated for READER to the C code.  (XXX) */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, 0);
  return (s_reader);
}

SCM
scm_from_token_reader (const scm_token_reader_spec_t *token_reader)
{
  SCM s_token_reader;
  scm_token_reader_spec_t *copy;

  copy = scm_malloc (sizeof (*copy));
  *copy = *token_reader;

  /* Return a "freeable" SMOB.  */
  SCM_NEW_READER_SMOB (s_token_reader, scm_token_reader_type, copy, 1);
  return (s_token_reader);
}

scm_reader_t
scm_to_reader (SCM reader)
{
  if (SCM_SMOB_PREDICATE (scm_reader_type, reader))
    {
      /* This a reader implemented in C.  */
      scm_reader_t c_reader;
      SCM_READER_SMOB_DATA (c_reader, reader);
      return (c_reader);
    }
  else if (scm_procedure_p (reader) == SCM_BOOL_T)
    {
      /* READER is _not_ implemented in C:  it's a Scheme procedure.  So
	 we'll generate a native reader function (using Lightning) that will
	 just call `scm_call_1 (READER, port)'.  */
      abort ();  /* FIXME */
    }

  /* Type error (FIXME) */
  return NULL;
}


#include "token-readers.h"

#if 0
SCM_DEFINE (dynr_do_stuff, "do-stuff", 1, 0, 0,
	    (SCM port),
	    "Do stuff.")
{
  static jit_insn code_buffer[4096];
  static scm_reader_t reader = NULL;
  size_t size;

  if (!reader)
    reader = scm_c_make_reader (code_buffer, sizeof (code_buffer),
				" \n\t", scm_reader_standard_specs,
				1, &size);

  return (reader (port));
}
#endif

SCM_DEFINE (scm_make_reader, "make-reader", 2, 1, 0,
	    (SCM whitespaces, SCM token_readers, SCM debug_p),
	    "Create a reader.")
#define FUNC_NAME "make-reader"
{
  SCM s_reader;
  scm_reader_t reader;
  size_t code_size = 1024;
  jit_insn *code_buffer;
  size_t actual_size;
  unsigned token_reader_count, i;
  char *c_whitespaces;
  scm_token_reader_spec_t *c_specs;

  SCM_VALIDATE_STRING (1, whitespaces);
  SCM_VALIDATE_LIST (2, token_readers);

  /* Convert the list TOKEN_READERS to a C array in C_SPECS.  */
  token_reader_count = scm_to_uint (scm_length (token_readers));
  c_specs = alloca (token_reader_count * sizeof (*c_specs));
  for (i = 0;
       i < token_reader_count;
       i++, token_readers = SCM_CDR (token_readers))
    {
      scm_token_reader_spec_t *tr_spec;
      SCM tr = SCM_CAR (token_readers);
      scm_assert_smob_type (scm_token_reader_type, tr);

      SCM_TOKEN_READER_SMOB_DATA (tr_spec, tr);
      c_specs[i] = *tr_spec;
    }

  /* Make it a zero-terminated array.  */
  c_specs[i].token.type = SCM_TOKEN_UNDEF;
  c_specs[i].name = NULL;
  c_specs[i].reader.type = SCM_TOKEN_READER_UNDEF;

  /* Go ahead with the reader compilation process.  */
  code_buffer = malloc (code_size);
  c_whitespaces = scm_to_locale_string (whitespaces);

  do
    {
      if (!code_buffer)
	{
	  free (c_whitespaces);
	  return SCM_BOOL_F;
	}

      reader = scm_c_make_reader (code_buffer, code_size, c_whitespaces,
				  c_specs,
				  (debug_p == SCM_UNDEFINED) ? 0
				  : ((debug_p == SCM_BOOL_F) ? 0 : 1),
				  &actual_size);
      if (!reader)
	{
	  printf ("%s: reader too small (%u vs. %u)\n",
		  __FUNCTION__, code_size, actual_size);
	  code_size <<= 1;
	  code_buffer = realloc (code_buffer, code_size);
	}
    }
  while (!reader);

  free (c_whitespaces);
  code_buffer = realloc (code_buffer, actual_size);

  /* Return a "freeable" SMOB, i.e. whose memory is owned and managed by
     Scheme code.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, 1);
  return (s_reader);
}
#undef FUNC_NAME

SCM_DEFINE (scm_default_reader, "default-reader", 0, 0, 0,
	    (void),
	    "Returns Guile's default reader.")
{
  SCM s_reader;

  /* This one may _not_ be freed by Scheme code at GC time.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, scm_standard_reader, 0);
  return (s_reader);
}

SCM_DEFINE (scm_make_token_reader, "make-token-reader", 2, 0, 0,
	    (SCM spec, SCM proc),
	    "Use procedure (or reader) @var{proc} as a token reader for "
	    "the characters defined by @var{spec}.")
#define FUNC_NAME "make-token-reader"
{
  SCM s_token_reader;
  scm_token_reader_spec_t *c_spec;

  SCM_VALIDATE_PROC (2, proc);

  c_spec = scm_malloc (sizeof (scm_token_reader_spec_t));

  if (read_token_spec (spec, c_spec))
    {
      free (c_spec);
      scm_error (scm_from_locale_symbol ("reader-error"),
		 "make-token-reader", "invalid token specifier: ~A",
		 scm_list_1 (spec), SCM_EOL);
    }

  if (SCM_SMOB_PREDICATE (scm_reader_type, proc))
    {
      /* Reader SMOBs are considered as procedures (because they are
	 "applicable") but for efficiency and consistency reasons, we
	 special-case them here.  */
      c_spec->reader.type = SCM_TOKEN_READER_READER;
      SCM_READER_SMOB_DATA (c_spec->reader.value.reader, proc);
      c_spec->name = NULL;
    }
  else if (scm_procedure_p (proc) == SCM_BOOL_T)
    {
      c_spec->reader.type = SCM_TOKEN_READER_SCM;
      c_spec->reader.value.scm_reader = proc;
      c_spec->name = NULL;
    }
  else
    {
      free (c_spec);
      scm_error (scm_from_locale_symbol ("reader-error"),
		 "make-token-reader", "invalid token reader procedure: ~A",
		 scm_list_1 (proc), SCM_EOL);
    }

  /* Return a "freeable" SMOB.  */
  SCM_NEW_READER_SMOB (s_token_reader, scm_token_reader_type,
		       c_spec, 1);
  return (s_token_reader);
}
#undef FUNC_NAME

SCM_DEFINE (scm_standard_token_reader, "standard-token-reader", 1, 0, 0,
	    (SCM name),
	    "Lookup standard token reader named @var{name} (a symbol) and "
	    "return it.  Return @code{#f} if not found.")
#define FUNC_NAME "standard-token-reader"
{
  SCM s_token_reader;
  const scm_token_reader_spec_t *spec;

  SCM_VALIDATE_SYMBOL (1, name);

  spec = scm_token_reader_lookup (scm_i_symbol_chars (name));
  if (!spec)
    return SCM_BOOL_F;

  /* Return a "non-freeable" SMOB.  */
  SCM_NEW_READER_SMOB (s_token_reader, scm_token_reader_type, spec, 0);
  return (s_token_reader);
}
#undef FUNC_NAME

SCM_DEFINE (scm_token_reader_proc, "token-reader-procedure", 1, 0, 0,
	    (SCM tr),
	    "Return the procedure attached to token reader @var{tr}.")
#define FUNC_NAME "token-reader-procedure"
{
  scm_token_reader_spec_t *c_tr;

  scm_assert_smob_type (scm_token_reader_type, tr);
  SCM_TOKEN_READER_SMOB_DATA (c_tr, tr);

  switch (c_tr->reader.type)
    {
    case SCM_TOKEN_READER_SCM:
      return (c_tr->reader.value.scm_reader);

    case SCM_TOKEN_READER_READER:
      {
	SCM s_reader;
	SCM_NEW_READER_SMOB (s_reader, scm_reader_type,
			     c_tr->reader.value.reader, 0);
	return (s_reader);
      }

    case SCM_TOKEN_READER_C:
      {
	char *name = NULL;
	if (c_tr->name)
	  {
	    name = alloca (strlen (c_tr->name) + 20);
	    strcpy (name, "%token-reader:");
	    strcat (name, c_tr->name);
	  }
	SCM_RETURN_NEWSMOB (scm_token_reader_proc_type,
			    c_tr->reader.value.c_reader);
      }

    default:
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_token_reader_spec, "token-reader-specification", 1, 0, 0,
	    (SCM tr),
	    "Return the specification, of token reader @var{tr}.")
#define FUNC_NAME "token-reader-specification"
{
  /* Note:  This has to be in sync with `read_token_spec ()'.  */
  scm_token_reader_spec_t *c_tr;

  scm_assert_smob_type (scm_token_reader_type, tr);
  SCM_TOKEN_READER_SMOB_DATA (c_tr, tr);

  switch (c_tr->token.type)
    {
    case SCM_TOKEN_SINGLE:
      return (SCM_MAKE_CHAR (c_tr->token.value.single));

    case SCM_TOKEN_RANGE:
      {
	SCM lo, hi;
	lo = SCM_MAKE_CHAR (c_tr->token.value.range.low);
	hi = SCM_MAKE_CHAR (c_tr->token.value.range.high);
	return (scm_cons (lo, hi));
      }

    case SCM_TOKEN_SET:
      return (scm_string_to_list
	      (scm_from_locale_string (c_tr->token.value.set)));

    default:
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* SMOB types.  */
scm_t_bits scm_reader_type, scm_token_reader_type,
  scm_token_reader_proc_type;

static SCM
reader_mark (SCM reader)
{
  return SCM_BOOL_F;
}

static size_t
reader_free (SCM reader)
{
  scm_reader_smob_t *smobinfo;

  smobinfo = (scm_reader_smob_t *)SCM_SMOB_DATA (reader);
  if (smobinfo->freeable)
    {
      scm_reader_t c_reader;

      c_reader = (scm_reader_t)smobinfo->c_object;
      free (c_reader);
    }

  smobinfo->freeable = 0;
  smobinfo->c_object = NULL;
  free (smobinfo);

  return 0;
}

static SCM
reader_apply (SCM reader, SCM port)
{
  scm_reader_t c_reader;

  if (port == SCM_UNDEFINED)
    port = scm_current_input_port ();

  SCM_READER_SMOB_DATA (c_reader, reader);

  return (scm_call_reader (c_reader, port));
}

static SCM
token_reader_mark (SCM tr)
{
  scm_token_reader_spec_t *c_spec;

  SCM_TOKEN_READER_SMOB_DATA (c_spec, tr);
  if (c_spec->reader.type == SCM_TOKEN_READER_SCM)
    return (c_spec->reader.value.scm_reader);

  return SCM_BOOL_F;
}

static size_t
token_reader_free (SCM tr)
{
  scm_reader_smob_t *smobinfo;

  smobinfo = (scm_reader_smob_t *)SCM_SMOB_DATA (tr);
  if (smobinfo->freeable)
    {
      scm_token_reader_spec_t *c_spec;

      c_spec = (scm_token_reader_spec_t *)smobinfo->c_object;
      if (c_spec->token.type == SCM_TOKEN_SET)
	/* XXX: We're assuming we can safely do this when it's freeable.
	   This is the actually the case when created from Scheme.  */
	free ((void *)c_spec->token.value.set);
      free (c_spec);
    }

  smobinfo->freeable = 0;
  smobinfo->c_object = NULL;
  free (smobinfo);

  return 0;
}

static SCM
token_reader_proc_mark (SCM tr_proc)
{
  return SCM_BOOL_F;
}

static size_t
token_reader_proc_free (SCM tr_proc)
{
  return 0;
}

static SCM
token_reader_proc_apply (SCM tr_proc, SCM chr, SCM port, SCM reader)
#define FUNC_NAME "%token-reader-proc-apply"
{
  int c_chr;
  scm_reader_t c_reader;
  scm_token_reader_t c_tr_proc;

  SCM_VALIDATE_CHAR (1, chr);
  SCM_VALIDATE_PORT (2, port);
  /* FIXME:  We should be able to accept arbitrary Scheme procs as well.  */
  scm_assert_smob_type (scm_reader_type, reader);

  c_tr_proc = (scm_token_reader_t)SCM_SMOB_DATA (tr_proc);
  SCM_READER_SMOB_DATA (c_reader, reader);
  c_chr = SCM_CHAR (chr);

  return (c_tr_proc (c_chr, port, c_reader));
}
#undef FUNC_NAME



/* Initialization routine.  */
void
scm_reader_init_bindings (void)
{
  scm_reader_type = scm_make_smob_type ("reader", 0);
  scm_set_smob_mark (scm_reader_type, reader_mark);
  scm_set_smob_free (scm_reader_type, reader_free);
  scm_set_smob_apply (scm_reader_type, reader_apply, 0, 1, 0);

  scm_token_reader_type = scm_make_smob_type ("token-reader", 0);
  scm_set_smob_mark (scm_token_reader_type, token_reader_mark);
  scm_set_smob_free (scm_token_reader_type, token_reader_free);

  scm_token_reader_proc_type = scm_make_smob_type ("token-reader-proc", 0);
  scm_set_smob_mark (scm_token_reader_proc_type, token_reader_proc_mark);
  scm_set_smob_free (scm_token_reader_proc_type, token_reader_proc_free);
  scm_set_smob_apply (scm_token_reader_proc_type, token_reader_proc_apply,
		      3, 0, 0);

#include "reader.c.x"

  /* Compile/load the standard reader.  */
  scm_load_standard_reader ();
}
