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

static SCM
do_scm_make_char (int chr)
{
  printf ("%s: got char %i\n", __FUNCTION__, chr);
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
  jit_insn *jumps[10], *ref, *do_again, *jump_to_end;
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
		DO_DEBUG_2_PP (start_c_call_string, JIT_R0,
			       name, JIT_R1, do_like_printf);

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

	      /* When the reader returns SCM_UNSPECIFIED, the start again.
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

	      token_spec_to_string (tr, spec_str);
	      if (debug)
		DO_DEBUG_2_PP (start_scm_call_string, JIT_R0,
			       spec_str, JIT_R1, do_like_printf);

	      CHECK_CODE_SIZE (buffer_size, start);

	      /* Convert the character read to a Scheme char.  */
	      jit_prepare (1);
	      jit_pusharg_i (JIT_V1);
	      jit_finish (do_scm_make_char);
	      jit_movr_p (JIT_R2, JIT_RET);

	      /* Actually call the Scheme procedure.  */
	      jit_movi_p (JIT_R0, (void *)s_reader);
	      jit_movi_p (JIT_R1, (void *)tr->reader.value.scm_reader);
	      jit_prepare (4);
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
	      DO_DEBUG_2_PP (start_reader_call_string, JIT_R0,
			     spec_str, JIT_R1, do_like_printf);

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

  jit_patch (jump_to_end);

  jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
  jit_ret ();

  end = jit_get_ip ().ptr;
  jit_flush_code (start, end);

  *code_size = end - start;
  printf ("generated %u bytes of code\n", end - start);

  return result;
}


/* Wrapping/unwrapping methods.  */

const scm_token_reader_spec_t *
scm_token_reader_lookup (const scm_reader_spec_t spec,
			 const char *name)
{
  const scm_token_reader_spec_t *tr;

  for (tr = spec; tr->name; tr++)
    {
      if (!strcmp (name, tr->name))
	return (tr);
    }

  return NULL;
}

/* Read token specifier SPEC and update TR accordingly.  On error (i.e. if
   SPEC is not a valid specification), return non-zero.  */
static int
read_token_spec (SCM spec, scm_token_reader_spec_t *tr)
{
  if (SCM_CHARP (spec))
    {
      tr->token.type = SCM_TOKEN_SINGLE;
      tr->token.value.single = SCM_CHAR (spec);
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

/* Read token reader specifier SPEC and update TR accordingly.  On error
   (i.e. if SPEC is not a valid specification), return non-zero.  */
static int
read_token_reader_spec (SCM spec, scm_token_reader_spec_t *tr)
{
  if (scm_procedure_p (spec) == SCM_BOOL_T)
    {
      tr->reader.type = SCM_TOKEN_READER_SCM;
      tr->reader.value.scm_reader = spec;
    }
  else if (scm_is_symbol (spec))
    {
      const scm_token_reader_spec_t *ref_specs = scm_reader_standard_specs;
      const scm_token_reader_spec_t *ref;

    lookup:
      ref = scm_token_reader_lookup (ref_specs,
				     scm_i_symbol_chars (spec));
      if ((!ref) && (ref_specs == scm_reader_standard_specs))
	{
	  ref_specs = scm_sharp_reader_standard_specs;
	  goto lookup;
	}

      if (!ref)
	{
	  printf ("%s: %s: unknown token reader\n", __FUNCTION__,
		  scm_i_symbol_chars (spec));
	  return 1;
	}

      tr->reader.type = ref->reader.type;
      switch (tr->reader.type)
	{
	case SCM_TOKEN_READER_C:
	  tr->reader.value.c_reader = ref->reader.value.c_reader;
	  break;
	case SCM_TOKEN_READER_SCM:
	  tr->reader.value.scm_reader = ref->reader.value.scm_reader;
	  break;
	case SCM_TOKEN_READER_READER:
	  tr->reader.value.reader = ref->reader.value.reader;
	  break;
	default:
	  printf ("%s: %i: unknown token reader type\n", __FUNCTION__,
		  (int)tr->reader.type);
	  return 1;
	}
    }
  else if (SCM_SMOB_PREDICATE (scm_reader_type, spec))
    {
      tr->reader.type = SCM_TOKEN_READER_READER;
      tr->reader.value.reader = (scm_reader_t)SCM_SMOB_DATA (spec);
    }
  else
    return 1;

  return 0;
}


scm_reader_spec_t
scm_to_reader_spec (SCM lst)
#define FUNC_NAME "scm_to_reader_spec"
{
  SCM car;
  scm_token_reader_spec_t *specs = NULL;
  size_t specs_size = 0, spec_items = 0;

#define APPEND_TO_SPECS(_trs)							\
  do										\
    {										\
      if (spec_items + 1 > specs_size)						\
	{									\
	  specs_size = specs_size ? specs_size << 1 : 20;			\
	  specs = realloc (specs,						\
			   specs_size * sizeof (scm_token_reader_spec_t));	\
	  if (!specs)								\
	    goto finish;							\
	}									\
      specs[spec_items++] = (_trs);						\
    }										\
  while (0)

#define TERMINATE_SPECS()						\
  do									\
    {									\
      scm_token_reader_spec_t _trs;					\
      _trs.name = NULL;							\
      _trs.token.type = SCM_TOKEN_UNDEF;				\
      APPEND_TO_SPECS (_trs);						\
      specs = realloc (specs,						\
		       spec_items * sizeof (scm_token_reader_spec_t));	\
    }									\
  while (0)

#define CLEAR_SPECS()				\
  do						\
    {						\
      free (specs);				\
      specs = NULL, specs_size = 0;		\
    }						\
  while (0)


  SCM_VALIDATE_LIST (0, lst);

  for (car = SCM_CAR (lst);
       lst != SCM_EOL;
       lst = SCM_CDR (lst))
    {
      car = SCM_CAR (lst);

      if (scm_is_symbol (car))
	{
	  const scm_token_reader_spec_t *trs;
	  const scm_token_reader_spec_t *ref_specs = scm_reader_standard_specs;

	lookup:
	  trs = scm_token_reader_lookup (scm_reader_standard_specs,
					 scm_i_symbol_chars (car));
	  if ((!trs) && (ref_specs == scm_reader_standard_specs))
	    {
	      /* Try again.  */
	      ref_specs = scm_sharp_reader_standard_specs;
	      goto lookup;
	    }

	  if (!trs)
	    {
	      CLEAR_SPECS ();
	      goto finish;
	    }

	  APPEND_TO_SPECS (*trs);
	}
      else if (scm_is_pair (car))
	{
	  scm_token_reader_spec_t trs;
	  SCM caar = SCM_CAR (car);
	  SCM cdar = SCM_CADR (car);

	  scm_write_line (caar, SCM_UNDEFINED);
	  scm_write_line (cdar, SCM_UNDEFINED);

	  trs.name = "%user-scheme-proc";

	  /* Read the token specifier.  */
	  if (read_token_spec (caar, &trs))
	    {
	      printf ("%s: invalid token specifier\n", __FUNCTION__);
	      CLEAR_SPECS ();
	      goto finish;
	    }

	  /* Read the token reader specifier.  */
	  if (read_token_reader_spec (cdar, &trs))
	    {
	      printf ("%s: invalid token reader specifier\n", __FUNCTION__);
	      CLEAR_SPECS ();
	      goto finish;
	    }

	  APPEND_TO_SPECS (trs);
	}
      else
	{
	  printf ("%s: unknown token specifier object\n", __FUNCTION__);
	  CLEAR_SPECS ();
	  goto finish;
	}
    }

 finish:
  if (specs)
    TERMINATE_SPECS ();

  return specs;
}

#undef APPEND_TO_SPECS
#undef TERMINATE_SPECS
#undef CLEAR_SPECS


#include "token-readers.h"

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

SCM_DEFINE (scm_make_reader, "make-reader", 2, 1, 0,
	    (SCM whitespaces, SCM specs, SCM debug_p),
	    "Create a reader.")
{
  scm_reader_t reader;
  size_t code_size = 1024;
  jit_insn *code_buffer;
  size_t actual_size;
  char *c_whitespaces;
  scm_reader_spec_t c_specs;

  SCM_VALIDATE_STRING (1, whitespaces);

  c_specs = scm_to_reader_spec (specs);
  if (!c_specs)
    return SCM_BOOL_F;

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

  SCM_RETURN_NEWSMOB (scm_reader_type, reader);
}


SCM_DEFINE (scm_default_reader, "default-reader", 0, 0, 0,
	    (void),
	    "Returns Guile's default reader.")
{
  SCM_RETURN_NEWSMOB (scm_reader_type, scm_standard_reader);
}



scm_t_bits scm_reader_type;

static SCM
reader_mark (SCM reader)
{
  return SCM_BOOL_F;
}

static size_t
reader_free (SCM reader)
{
  scm_reader_t c_reader;

  c_reader = (scm_reader_t)SCM_SMOB_DATA (reader);
  free (c_reader);

  return 0;
}

static SCM
reader_apply (SCM reader, SCM port)
{
  scm_reader_t c_reader;

  if (port == SCM_UNDEFINED)
    port = scm_current_input_port ();

  c_reader = (scm_reader_t)SCM_SMOB_DATA (reader);

  return (c_reader (port));
}


void
dynr_init_bindings (void)
{
  scm_reader_type = scm_make_smob_type ("reader", 0);
  scm_set_smob_mark (scm_reader_type, reader_mark);
  scm_set_smob_free (scm_reader_type, reader_free);
  scm_set_smob_apply (scm_reader_type, reader_apply, 0, 1, 0);

#include "reader.c.x"

  /* Compile/load the standard reader.  */
  scm_load_standard_reader ();
}
