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

#include <lightning.h>
#include <libguile.h>

#include "reader.h"




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
   buffer.  */
#define CHECK_CODE_SIZE(_max, _start)			\
do							\
{							\
  if (jit_get_ip ().ptr - (_start) + 10 >= (_max))	\
    {							\
      *code_size = jit_get_ip ().ptr - (_start);	\
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


/* The guts of the system: compiles a reader to native code.  */
scm_reader_t
scm_c_make_reader (jit_insn *code_buffer,
		   size_t buffer_size,
		   const char *whitespaces,
		   const scm_token_reader_spec_t *token_readers,
		   int debug,
		   size_t *code_size)
{
  static const char the_string[] = "read char: 0x%02x\n";
  static const char the_other_string[] = "read token 0x%02x!\n";
  static const char start_call_string[] = "calling token reader `%s'...\n";
  static const char end_call_string[] = "token reader `%s' finished\n";

  scm_reader_t result;
  char *start, *end;
  const char *ws;
  jit_insn *jumps[10], *ref, *do_again;
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
      if (debug)
	{
	  jit_movi_p (JIT_R0, the_other_string);
	  jit_prepare (2);
	  jit_pusharg_i (JIT_V1);
	  jit_pusharg_p (JIT_R0);
	  jit_finish (do_like_printf);
	}

      if (tr->reader.c_reader)
	{
	  /* Call the C reader function associated with this character.  */
	  const char *name = (tr->name ? tr->name : "<nameless>");
	  void *func = (void *)tr->reader.c_reader;

	  if (debug)
	    DO_DEBUG_2_PP (start_call_string, JIT_R0,
			   name, JIT_R1, do_like_printf);

	  jit_movi_p (JIT_R0, start);
	  jit_prepare (3);
	  jit_pusharg_p (JIT_R0);
	  jit_pusharg_p (JIT_V0);
	  jit_pusharg_i (JIT_V1);
	  jit_finish (func);

	  if (debug)
	    {
	      jit_movr_p (JIT_V2, JIT_RET);
	      DO_DEBUG_2_PP (end_call_string, JIT_R0,
			     name, JIT_R1, do_like_printf);
	      jit_movr_p (JIT_RET, JIT_V2);
	    }

	  /* When the reader returns SCM_UNSPECIFIED, the start again.  This
	     is what, for instance, comment readers do.  */
	  jit_beqi_p (do_again, JIT_RET, (void *)SCM_UNSPECIFIED);
	}
      else
	jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);

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

  jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
  jit_ret ();

  end = jit_get_ip ().ptr;
  jit_flush_code (start, end);

  *code_size = end - start;
  printf ("generated %u bytes of code\n", end - start);

  return result;
}



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

SCM_DEFINE (dynr_make_reader, "make-reader", 0, 1, 0,
	    (SCM debug_p),
	    "Create a reader.")
{
  jit_insn *code_buffer = malloc (4096);
  size_t size;

  if (!code_buffer)
    return SCM_BOOL_F;

  return (scm_c_make_gsubr ("%dynamic-reader", 1, 0, 0,
			    scm_c_make_reader (code_buffer, 4096, " \n\t",
					       scm_reader_standard_specs,
					       (debug_p == SCM_BOOL_F) ? 0 : 1,
					       &size)));
}


void
dynr_init_bindings (void)
{
#include "reader.c.x"
}
