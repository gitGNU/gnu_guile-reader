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


#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include <libguile.h>

#include "reader.h"
#include "token-readers.h"

#ifdef SCM_READER_USE_LIGHTNING
# include <lightning.h>
#endif


/* Debugging support.  */
static void debug (const char *, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 1, 2)))
#endif
     ;

#ifdef DEBUG
#include <stdio.h>

static __inline__ void
debug (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
}

#else

static __inline__ void
debug (const char *fmt, ...)
{
  /* Nothing.  */
}

#endif /* DEBUG */



/* SMOB helper data structures and macros.  */

/* The structure that bridges SMOB and C objects.  The point of this
   structure is to have knowledge as to whether the SMOB's underlying C
   object is was created from Scheme (and is therefore freeable) or not.  */
typedef struct
{
  void *c_object;   /* the underlying C object */
  int   freeable;   /* whether the underlying C object should be freed when
		       the SMOB is GC'd */
  SCM  *deps;       /* #f-terminated array of objects the current SMOB
		       depends on and that should be marked by the GC */
} scm_reader_smob_t;


#define SCM_NEW_READER_SMOB(_smob, _smobtype, _c_obj, _deps, _freeable)	\
do									\
{									\
  scm_reader_smob_t *_smobinfo;						\
  _smobinfo = scm_malloc (sizeof (*_smobinfo));				\
  _smobinfo->c_object = (void *)(_c_obj);				\
  _smobinfo->freeable = (_freeable);					\
  _smobinfo->deps = (_deps);						\
  SCM_NEWSMOB (_smob, _smobtype, _smobinfo);				\
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

#ifdef SCM_READER_USE_LIGHTNING

/* The Lightning-based implementation of `scm_c_make_reader ()'.  */


/* Run-time helper function.  */

static void
do_like_printf (const char *fmt, int i)
{
#if 0
  printf ("i=%i\n", i);
  printf ("fmt=\"%s\"\n", fmt);
#endif
  printf (fmt, i);
}


static SCM
do_scm_make_char (int chr)
{
  debug ("%s: got char %i\n", __FUNCTION__, chr);

  return SCM_MAKE_CHAR (chr);
}

static SCM
do_scm_make_reader_smob (scm_reader_t reader)
{
  register SCM s_reader;

  /* We return a non-freeable reader SMOB with no `deps'.  Hopefully this
     will be a short-lived SMOB.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, NULL, 0);

  debug ("d-s-m-r-s: new reader @ %p [SCM %p]\n", reader, s_reader);

  return s_reader;
}

static void
do_scm_set_source_position (SCM obj, SCM line, SCM column,
			    SCM filename)
{
  size_t len;
  char *c_filename;

  debug ("%s: o=%p l=%p c=%p f=%p\n",
	  __FUNCTION__, obj, line, column, filename);

  assert (scm_is_string (filename));
  assert (scm_is_number (column));
  assert (scm_is_number (line));

  len = scm_c_string_length (filename);
  c_filename = alloca (len + 1);
  scm_to_locale_stringbuf (filename, c_filename, len);
  c_filename[len] = 0;

  debug ("%s (obj=%p[%s], line=%u, col=%u, file=\"%s\")\n",
	 __FUNCTION__, (void *)obj,
	 SCM_IMP (obj) ? "imm" : "non-imm",
	 scm_to_uint (line), scm_to_uint (column),
	 c_filename);

  if ((SCM_NIMP (obj)) && (scm_is_pair (obj)))
    {
      /* FIXME:  Why does `scm_set_source_property_x ()' expect the thing to
	 be a pair?  */

      /* OBJ is a non-immediate Scheme value so we can set its source
	 properties.  */
      scm_set_source_property_x (obj, scm_sym_line, line);
      scm_set_source_property_x (obj, scm_sym_column, column);
      scm_set_source_property_x (obj, scm_sym_filename, filename);
    }
}


/* Code generation functions.  */

/* Register allocation invariants
   ------------------------------

   Registers V0, V1, and V2 are preserved accross function calls.  We have
   only three of them so care must be taken to use them carefully.  The
   following invariants were chosen:

   - V0 contains at any time the PORT argument;

   - V1 contains at any time the character just returned by `scm_getc ()',
   i.e. a C integer;

   - V2 contains the frame pointer, i.e. a pointer to the base of all stack
     variables.


   Variables allocated on the stack
   --------------------------------

   The generated code stores a few variables on the stack, starting at the
   frame pointer and going downwards.  The `JIT_STACK_' macros give the
   offset of those variables.  */

#define JIT_STACK_CALLER_HANDLED      0
#define JIT_STACK_POSITION_FILENAME   4
#define JIT_STACK_POSITION_LINE       8
#define JIT_STACK_POSITION_COLUMN    12

/* Total size needed to store local variables.  */
#define JIT_STACK_LOCAL_VARIABLES_SIZE 16

/* FIXME: Using Lightning 1.2 on x86, register V2 is _not_ preserved accross
   function calls as it should be.  The two macros below are meant to work
   around this.  */

#if (defined __i386__)

# define workaround_pre_call()   do { jit_pushr_p (JIT_V2); } while (0)
# define workaround_post_call()  do { jit_popr_p (JIT_V2); } while (0)

#else

# define workaround_pre_call()   do {} while (0)
# define workaround_post_call()  do {} while (0)

#endif


/* Generate code the calls FUNC, a printf-like function, where PTR1 points to
   an immediate format string containing either `%s' or `%p', and PTR2 is a
   pointer (immediate value).  REG1 (resp. REG2) is the register where PTR1
   (resp. PTR2) is stored.  */
#define DO_DEBUG_2_PP(_ptr1, _reg1, _ptr2, _reg2, _func)	\
do								\
{								\
  workaround_pre_call ();					\
  jit_movi_p ((_reg1), (_ptr1));				\
  jit_movi_p ((_reg2), (_ptr2));				\
  jit_prepare (2);						\
  jit_pusharg_p ((_reg2));					\
  jit_pusharg_p ((_reg1));					\
  jit_finish ((_func));						\
  workaround_post_call ();					\
} while (0)

/* In the current code generation function, check whether we are not getting
   too close to MAX bytes, given the fact that code generation started at
   START (a pointer-to-char).  Return ERROR if this is the case.  */
#define CHECK_CODE_SIZE(_max, _start, _error)		\
do							\
{							\
  if (jit_get_ip ().ptr - (_start) + 40 >= (_max))	\
    return (_error);					\
							\
} while (0)


/* Generate code that will fetch and store the current position information
   of PORT.  The relevant information is made available on the stack.  The
   usual register allocation invariants are assumed.  */
static __inline__ int
generate_position_store (jit_state *lightning_state,
			 char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  debug ("%s (@%p)\n", __FUNCTION__, jit_get_ip ().ptr);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  workaround_pre_call ();
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);  /* port */
  (void)jit_finish (scm_port_line);
  CHECK_CODE_SIZE (buffer_size, start, -1);
  workaround_post_call ();
  jit_stxi_p (-JIT_STACK_POSITION_LINE, JIT_V2, JIT_RET);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  workaround_pre_call ();
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (scm_port_column);
  CHECK_CODE_SIZE (buffer_size, start, -1);
  workaround_post_call ();
  jit_stxi_p (-JIT_STACK_POSITION_COLUMN, JIT_V2, JIT_RET);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  workaround_pre_call ();
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (scm_port_filename);
  workaround_post_call ();
  jit_stxi_p (-JIT_STACK_POSITION_FILENAME, JIT_V2, JIT_RET);

  /* XXX: We shouldn't need to call `scm_gc_protect ()' here since
     PORT is unlikely to be GC'd anyway.  */

  CHECK_CODE_SIZE (buffer_size, start, -1);

  return 0;
}
#undef _jit

/* Generate code that will set the source properties of the expression just
   read and available in register JIT_RET.  The relevant information is
   assumed to be stored on the stack.  The usual register allocation
   invariants are assumed.  */
static __inline__ int
generate_position_set (jit_state *lightning_state,
		       char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  debug ("%s\n", __FUNCTION__);

  /* Put the expression just read in V1 which is no longer used at
     this point (it used to contain the character read).  */
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_pushr_i (JIT_V1);  /* character as an `int' */
  jit_retval_p (JIT_V1); /* Scheme expression just read */

  /* Pop back line, column and filename.  */
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_ldxi_p (JIT_R0, JIT_V2, -JIT_STACK_POSITION_LINE);
  jit_ldxi_p (JIT_R1, JIT_V2, -JIT_STACK_POSITION_COLUMN);
  jit_ldxi_p (JIT_R2, JIT_V2, -JIT_STACK_POSITION_FILENAME);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  workaround_pre_call ();
  jit_prepare (4);
  jit_pusharg_p (JIT_R2);
  jit_pusharg_p (JIT_R1);
  jit_pusharg_p (JIT_R0);
  jit_pusharg_p (JIT_V1); /* expr */
  jit_finish (do_scm_set_source_position);
  workaround_post_call ();

  /* Put the expression read back in JIT_RET.  */
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_movr_p (JIT_RET, JIT_V1);
  jit_popr_i (JIT_V1);

  return 0;
}
#undef _jit

/* Generate a prologue that reserves enough space on the stack to store the
   reader's local variables and store the original value of the stack pointer
   (which we'll refer to as the ``frame pointer'') in V2.  */
static __inline__ int
generate_reader_prologue (jit_state *lightning_state,
			  int debug,
			  char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  static const char *msg_prologue = "reader prologue: SP is %p\n";

  if (debug)
    {
      jit_pushr_p (JIT_R0);
      jit_pushr_p (JIT_R1);

      (void)jit_movi_p (JIT_R0, msg_prologue);
      jit_addi_p (JIT_R1, JIT_SP, 2 * sizeof (void *));

      jit_prepare (2);
      jit_pusharg_p (JIT_R1);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);
      CHECK_CODE_SIZE (buffer_size, start, -1);

      jit_popr_p (JIT_R1);
      jit_popr_p (JIT_R0);
    }

  jit_movr_p (JIT_V2, JIT_SP);
  jit_subi_p (JIT_SP, JIT_SP, JIT_STACK_LOCAL_VARIABLES_SIZE + 16);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  return 0;
}
#undef _jit

/* Generate code that restores the stack pointer from the frame pointer
   and returns.  The usual register invariants are assumed.  */
static __inline__ int
generate_reader_epilogue (jit_state *lightning_state,
			  int debug,
			  char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  static const char *msg_epilogue = "reader epilogue: restoring SP %p\n";

  if (debug)
    {
      jit_pushr_p (JIT_RET);

      (void)jit_movi_p (JIT_R0, msg_epilogue);
      workaround_pre_call ();
      jit_prepare (2);
      jit_pusharg_p (JIT_V2);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);
      workaround_post_call ();
      CHECK_CODE_SIZE (buffer_size, start, -1);

      jit_popr_p (JIT_RET);
    }

  jit_movr_p (JIT_SP, JIT_V2);
  jit_ret ();

  CHECK_CODE_SIZE (buffer_size, start, -1);
  return 0;
}
#undef _jit

/* Generate code that handles an unexpected character (the character, a C
   integer, is expected to be in V1 at this point).  I.e., if the
   CALLER_HANDLED argument passed to the function being generated (this
   argument is expected to be on the stack) is true, then do nothing.
   Otherwise, call FAULT_HANDLER.  */
static __inline__ int
generate_unexpected_character_handling (jit_state *lightning_state,
					SCM fault_handler, int debug,
					char *start, size_t buffer_size)
{
  if (scm_procedure_p (fault_handler) == SCM_BOOL_T)
    {
      /* Check whether the CALLER_HANDLED argument is true, in which case
	 we'll simply return the faulty character to PORT and return.  */
      jit_insn *ref;

      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_ldxi_i (JIT_R0, JIT_V2, -JIT_STACK_CALLER_HANDLED);
      ref = jit_beqi_i (jit_forward (), JIT_R0, 0);
      workaround_pre_call ();
      jit_prepare (2);
      jit_pusharg_p (JIT_V0); /* port */
      jit_pusharg_i (JIT_V1); /* character */
      jit_finish (scm_ungetc);
      workaround_post_call ();

      jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
      generate_reader_epilogue (&_jit, debug, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start, -1);

      jit_patch (ref);

      /* Else, since CALLER_HANDLED is false, call the user-defined fault
	 handler.  */
      CHECK_CODE_SIZE (buffer_size, start, -1);

      jit_pushr_p (JIT_V2); /* save the frame pointer */

      jit_movi_p (JIT_R1, start);
      jit_prepare (1);
      jit_pusharg_p (JIT_R1);
      jit_finish (do_scm_make_reader_smob);
      jit_retval_p (JIT_V2);

      workaround_pre_call ();
      jit_prepare (1);
      jit_pusharg_i (JIT_V1);
      jit_finish (do_scm_make_char);
      jit_retval_p (JIT_R1);
      workaround_post_call ();

      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_movi_p (JIT_R0, (void *)fault_handler);
      jit_prepare (4);
      jit_pusharg_p (JIT_V2);  /* reader */
      jit_pusharg_p (JIT_V0);  /* port */
      jit_pusharg_p (JIT_R1);  /* character */
      jit_pusharg_p (JIT_R0);  /* procedure */
      jit_finish (scm_call_3);

      jit_popr_p (JIT_V2); /* restore the frame pointer */
    }
  else
    {
      /* The user did not define any method to handle this situation so
	 return the faulty character to PORT.  */
      CHECK_CODE_SIZE (buffer_size, start, -1);
      workaround_pre_call ();
      jit_prepare (2);
      jit_pusharg_p (JIT_V0); /* port */
      jit_pusharg_i (JIT_V1); /* character */
      jit_finish (scm_ungetc);
      workaround_post_call ();

      jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
    }

  CHECK_CODE_SIZE (buffer_size, start, -1);

  return 0;
}


/* The top-level code-generating function.  */

#undef CHECK_CODE_SIZE

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


/* The guts of the system: compiles a reader to native code.  */
scm_reader_t
scm_c_make_reader (void *code_buffer,
		   size_t buffer_size,
		   const scm_token_reader_spec_t *token_readers,
		   SCM fault_handler,
		   int record_positions,
		   int debug,
		   size_t *code_size)
{
  static const char msg_getc[] = "got char: 0x%02x\n";
  static const char msg_found_token[] = "read token 0x%02x!\n";
  static const char msg_start_c_call[] = "calling token reader `%s'...\n";
  static const char msg_start_scm_call[] =
    "calling Scheme token reader `%s'...\n";
  static const char msg_start_reader_call[] =
    "calling reader `%s'...\n";
  static const char msg_end_of_call[] = "token reader `%s' finished\n";

  scm_reader_t result;
  char *start, *end;
  jit_insn *jumps[20], *ref, *do_again, *jump_to_end;
  size_t jump_count = 0;
  const scm_token_reader_spec_t *tr;
  SCM arg_port;
  int arg_caller_handled;

  result = (scm_reader_t) (jit_set_ip (code_buffer).iptr);
  start = jit_get_ip ().ptr;

  /* Take two arguments (the port and an `int') and put them into V0 and V1
     (preserved accross function calls).  */
  jit_prolog (2);
  arg_port = (SCM)jit_arg_p ();
  jit_getarg_p (JIT_V0, arg_port);
  arg_caller_handled = jit_arg_i ();
  jit_getarg_i (JIT_V1, arg_caller_handled);

  /* The PORT argument is optional.  If not passed (i.e. equals to
     SCM_UNDEFINED), default to the current input port.  */
  ref = jit_bnei_p (jit_forward (), JIT_V0, (void *)SCM_UNDEFINED);
  jit_finish (scm_current_input_port);
  jit_retval_p (JIT_V0);
  jit_patch (ref);

  /* FIXME:  We should check the type of PORT here.  */

  /* Assuming the stack grows downwards, reserve some space for local
     variables and store the ``frame pointer'' (i.e. the beginning of the
     local variables array) in V2.  */
  generate_reader_prologue (&_jit, debug, start, buffer_size);
  CHECK_CODE_SIZE (buffer_size, start);

  /* Store the CALLER_HANDLED argument (currently in V1) on the stack.  */
  jit_stxi_p (-JIT_STACK_CALLER_HANDLED, JIT_V2, JIT_V1);

  do_again = jit_get_label ();

  /* Call `scm_getc ()'.  */
  workaround_pre_call ();
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (scm_getc);
  workaround_post_call ();

  /* Put the character just read (an `int') in V1 (preserved accross function
     calls).  */
  jit_retval_i (JIT_V1);

  /* Test whether we got `EOF'.  */
  jump_to_end = jit_beqi_i (jit_forward (), JIT_V1, (int)EOF);

  CHECK_CODE_SIZE (buffer_size, start);

  if (debug)
    {
      /* Print a debug message.  */
      (void)jit_movi_p (JIT_R0, msg_getc);
      workaround_pre_call ();
      jit_prepare (2);
      jit_pusharg_i (JIT_V1);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);
      workaround_post_call ();
    }

  if (record_positions)
    {
      /* Before invoking a token reader, keep track of the current position
	 of PORT.  */
      generate_position_store (&_jit, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start);
    }

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
	      /* Mark the last `bnei' for future patching, so that it will
		 jump to the next token reader considered.  */
	      jumps[jump_count++] = ref;

	    for (j = 0; j < go_next_jump_count; j++)
	      jit_patch (go_next_jumps[j]);

	    break;
	  }

	default:
	  scm_misc_error (__FUNCTION__, "unknown token type: ~A",
			  scm_list_1 (SCM_I_MAKINUM ((int)tr->token.type)));
	}

      /* The code that gets executed when the character returned by `scm_getc
	 ()' is equal to that of TOKEN_READER.  */
      CHECK_CODE_SIZE (buffer_size, start);
      if (debug)
	{
	  (void)jit_movi_p (JIT_R0, msg_found_token);

	  workaround_pre_call ();
	  jit_prepare (2);
	  jit_pusharg_i (JIT_V1);
	  jit_pusharg_p (JIT_R0);
	  (void)jit_finish (do_like_printf);
	  workaround_post_call ();

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
		  DO_DEBUG_2_PP (msg_start_c_call, JIT_R0,
				 name, JIT_R1, do_like_printf);
		}

	      CHECK_CODE_SIZE (buffer_size, start);
	      (void)jit_movi_p (JIT_R0, start);

	      workaround_pre_call ();
	      jit_prepare (3);
	      jit_pusharg_p (JIT_R0); /* reader */
	      jit_pusharg_p (JIT_V0); /* port */
	      jit_pusharg_i (JIT_V1); /* character */
	      (void)jit_finish (func);
	      workaround_post_call ();

	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  jit_pushr_i (JIT_V1);
		  jit_movr_p (JIT_V1, JIT_RET);
		  DO_DEBUG_2_PP (msg_end_of_call, JIT_R0,
				 name, JIT_R1, do_like_printf);
		  jit_movr_p (JIT_RET, JIT_V1);
		  jit_popr_i (JIT_V1);
		}
	    }
	  else
	    /* The NULL reader:  simply ignore the character that was just
	       read and jump to DO_AGAIN.  This is useful for whitespaces.  */
	    jit_jmpi (do_again);

	  break;

	case SCM_TOKEN_READER_SCM:
	  /* Call the Scheme proc associated with this character.  */
	  if (scm_procedure_p (tr->reader.value.scm_reader) == SCM_BOOL_T)
	    {
	      char spec_str[100];

	      CHECK_CODE_SIZE (buffer_size, start);
	      token_spec_to_string (tr, spec_str);
	      if (debug)
		{
		  /* FIXME:  SPEC_STR must be computed at run-time!  */
		  CHECK_CODE_SIZE (buffer_size, start);
		  DO_DEBUG_2_PP (msg_start_scm_call, JIT_R0,
				 "spec_str", JIT_R1, do_like_printf);
		}

	      CHECK_CODE_SIZE (buffer_size, start);

	      /* Save the current value of V1 (the char as an `int').  */
	      jit_pushr_i (JIT_V1);

	      /* Convert the character read to a Scheme char.  */
	      workaround_pre_call ();
	      jit_prepare (1);
	      jit_pusharg_i (JIT_V1);
	      jit_finish (do_scm_make_char);
	      workaround_post_call ();
	      jit_retval_p (JIT_V1);

	      /* Same for the reader.  */
	      jit_movi_p (JIT_R0, start);
	      workaround_pre_call ();
	      jit_prepare (1);
	      jit_pusharg_i (JIT_R0);
	      jit_finish (do_scm_make_reader_smob);
	      workaround_post_call ();
	      jit_retval_p (JIT_R0);

	      /* Actually call the Scheme procedure.  */
	      CHECK_CODE_SIZE (buffer_size, start);
	      jit_movi_p (JIT_R1, (void *)tr->reader.value.scm_reader);
	      workaround_pre_call ();
	      jit_prepare (4);
	      CHECK_CODE_SIZE (buffer_size, start);
	      jit_pusharg_p (JIT_R0); /* reader */
	      jit_pusharg_p (JIT_V0); /* port */
	      jit_pusharg_p (JIT_V1); /* character (Scheme) */
	      jit_pusharg_p (JIT_R1); /* procedure */
	      jit_finish (scm_call_3);
	      workaround_post_call ();

	      /* Restore the C character.  */
	      jit_popr_i (JIT_V1);

	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  jit_pushr_i (JIT_V1);
		  jit_retval_p (JIT_V1);
		  DO_DEBUG_2_PP (msg_end_of_call, JIT_R0,
				 "spec_str", JIT_R1, do_like_printf);
		  jit_movr_p (JIT_RET, JIT_V1);
		  jit_popr_i (JIT_V1);
		}
	    }
	  else
	    jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);

	  break;

	case SCM_TOKEN_READER_READER:
	  if (tr->reader.value.reader)
	    {
	      /* A simple C function call.  */
	      char spec_str[100];

	      token_spec_to_string (tr, spec_str);
	      if (debug)
		{
		  /* FIXME:  SPEC_STR must be computed at run-time!  */
		  CHECK_CODE_SIZE (buffer_size, start);
		  DO_DEBUG_2_PP (msg_start_reader_call, JIT_R0,
				 "spec_str", JIT_R1, do_like_printf);
		}

	      CHECK_CODE_SIZE (buffer_size, start);
	      workaround_pre_call ();
	      jit_prepare (1);
	      jit_pusharg_p (JIT_V0); /* port */
	      jit_finish (tr->reader.value.reader);
	      workaround_post_call ();

	      if (debug)
		{
		  CHECK_CODE_SIZE (buffer_size, start);
		  jit_pushr_i (JIT_V1);
		  jit_retval_p (JIT_V1);
		  DO_DEBUG_2_PP (msg_end_of_call, JIT_R0,
				 "spec_str", JIT_R1, do_like_printf);
		  jit_movr_p (JIT_RET, JIT_V1);
		  jit_popr_i (JIT_V1);
		}
	    }
	  else
	    /* The NULL reader:  simply ignore the character that was just
	       read and jump to DO_AGAIN.  This is useful for whitespaces.  */
	    jit_jmpi (do_again);

	  break;

	default:
	  scm_misc_error (__FUNCTION__, "unknown token reader type: ~A",
			  scm_list_1 (SCM_I_MAKINUM ((int)tr->reader.type)));
	}

      CHECK_CODE_SIZE (buffer_size, start);

      if (!tr->escape)
	/* When the reader returns SCM_UNSPECIFIED, then start again.
	   This is what, for instance, comment readers do in the
	   top-level reader.  */
	jit_beqi_p (do_again, JIT_RET, (void *)SCM_UNSPECIFIED);

      if (record_positions)
	{
	  generate_position_set (&_jit, start, buffer_size);
	  CHECK_CODE_SIZE (buffer_size, start);
	}

      /* The reader's return value is already in JIT_RET, so we just have to
	 return.  */
      generate_reader_epilogue (&_jit, debug, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start);
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
     character we just read.  */
  generate_unexpected_character_handling (&_jit, fault_handler, debug,
					  start, buffer_size);
  CHECK_CODE_SIZE (buffer_size, start);

  generate_reader_epilogue (&_jit, debug, start, buffer_size);
  CHECK_CODE_SIZE (buffer_size, start);

  /* This is where we get when `scm_getc ()' returned EOF.  */
  jit_patch (jump_to_end);
  jit_movi_p (JIT_RET, (void *)SCM_EOF_VAL);
  generate_reader_epilogue (&_jit, debug, start, buffer_size);

  end = jit_get_ip ().ptr;
  jit_flush_code (start, end);

  *code_size = end - start;
  if (debug)
    printf ("generated %u bytes of code\n", end - start);

  assert (*code_size <= buffer_size);

  return result;
}


#else /* SCM_READER_USE_LIGHTNING */

/* The slow Lightning-free implementation of `scm_c_make_reader ()' and
   `scm_call_reader ()'.  */
#warning "Compiling the slow, Lightning-free, implementation!"

struct scm_reader
{
  scm_token_reader_spec_t *token_readers;
  SCM fault_handler_proc;
  int debug;
};

scm_reader_t
scm_c_make_reader (void *code_buffer,
		   size_t buffer_size,
		   const scm_token_reader_spec_t *token_readers,
		   SCM fault_handler_proc,
		   int record_positions, -- FIXME: Unimplemented
		   int debug,
		   size_t *code_size)
{
  struct scm_reader *result;
  scm_token_reader_spec_t *tr_copy;
  const scm_token_reader_spec_t *tr;
  unsigned char *buffer = code_buffer;

  *code_size = sizeof (*result);
  if (buffer_size < sizeof (*result))
    return NULL;

  result = (struct scm_reader *)buffer;
  result->fault_handler_proc = fault_handler_proc;
  result->debug = debug;
  tr_copy = (scm_token_reader_spec_t *)(buffer + sizeof (*result));

  result->token_readers = tr_copy;

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
      if (tr->reader.value.c_reader)
	return tr->reader.value.c_reader (c, port, reader);
      else
	return SCM_UNSPECIFIED;

    case SCM_TOKEN_READER_SCM:
      {
	SCM s_reader;
	SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader,
			     NULL, 0);
	return scm_call_3 (tr->reader.value.scm_reader,
			   SCM_MAKE_CHAR (c), port, s_reader);
      }

    case SCM_TOKEN_READER_READER:
      if (tr->reader.value.reader)
	return scm_call_reader (tr->reader.value.reader, port, 0);
      else
	return SCM_UNSPECIFIED;

    default:
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}

SCM
scm_call_reader (scm_reader_t reader, SCM port, int caller_handled)
#define FUNC_NAME "%call-reader"
{
  int c = 0;
  scm_token_reader_spec_t *tr;
  SCM result = SCM_UNSPECIFIED;

  if (port == SCM_UNDEFINED)
    port = scm_current_input_port ();
  else
    SCM_VALIDATE_PORT (2, port);

 doit:
  while ((c = scm_getc (port)) != EOF)
    {
      for (tr = reader->token_readers;
	   tr->token.type != SCM_TOKEN_UNDEF;
	   tr++)
	{
	  if (tr_handles_char (tr, c))
	    {
	      result = tr_invoke (tr, c, port, reader);
	      if ((result == SCM_UNSPECIFIED) && (!tr->escape))
		goto doit;
	      else
		return result;
	    }
	}

      /* Unhandled character.  */
      if (caller_handled)
	{
	  /* Caller will take care of C.  */
	  scm_ungetc (c, port);
	  return SCM_UNSPECIFIED;
	}
      else
	{
	  if (scm_procedure_p (reader->fault_handler_proc) == SCM_BOOL_T)
	    {
	      /* Call the user-defined fault-handler.  */
	      SCM s_reader;

	      SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader,
				   NULL, 0);
	      return (scm_call_3 (reader->fault_handler_proc,
				  SCM_MAKE_CHAR (c), port, s_reader));
	    }
	  else
	    {
	      /* No handler defined.  */
	      scm_ungetc (c, port);
	      return SCM_UNSPECIFIED;
	    }
	}
    }

  return SCM_EOF_VAL;
}
#undef FUNC_NAME


#endif /* SCM_READER_USE_LIGHTNING */



/* Wrapping/unwrapping methods.  */


/* Read token specifier SPEC and update TR accordingly.  On error (i.e. if
   SPEC is not a valid specification), return non-zero.  This has to be in
   sync with `token-reader-specification'.  */
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
      /* The set itself cannot contain #\nul.  */
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


#if 0
SCM
scm_from_reader (scm_reader_t reader)
{
  SCM s_reader;

  /* Return a "non-freeable" reader SMOB, i.e. leave control over memory
     allocated for READER to the C code.  (XXX) */
  /* FIXME:  This is actually unfeasible since we don't know the `deps' of
     the compiled reader.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, NULL, 0);
  return (s_reader);
}
#endif

SCM
scm_from_token_reader (const scm_token_reader_spec_t *token_reader,
		       int caller_owned)
{
  SCM *s_deps;
  SCM s_token_reader;
  scm_token_reader_spec_t *copy;

  if (caller_owned)
    {
      /* Operate on a copy.  */
      copy = scm_malloc (sizeof (*copy));
      *copy = *token_reader;
    }
  else
    /* Operate directly on TOKEN_READER.  */
    copy = (scm_token_reader_spec_t *)token_reader;

  s_deps = scm_malloc (2 * sizeof (*s_deps));

  if (token_reader->reader.type == SCM_TOKEN_READER_SCM)
    s_deps[0] = token_reader->reader.value.scm_reader;
  else
    s_deps[0] = SCM_BOOL_F;

  s_deps[1] = SCM_BOOL_F;

  /* Return a "freeable" SMOB.  */
  SCM_NEW_READER_SMOB (s_token_reader, scm_token_reader_type, copy,
		       s_deps, 1);

  return (s_token_reader);
}

scm_token_reader_spec_t *
scm_to_token_reader (SCM tr)
#define FUNC_NAME "scm_to_token_reader"
{
  scm_token_reader_spec_t *c_tr, *c_copy;

  scm_assert_smob_type (scm_token_reader_type, tr);

  c_copy = scm_malloc (sizeof (*c_copy));
  SCM_TOKEN_READER_SMOB_DATA (c_tr, tr);

  *c_copy = *c_tr;
  if (c_copy->token.type == SCM_TOKEN_SET)
    {
      /* Create a copy of the set.  */
      char *set;

      set = scm_malloc (strlen (c_tr->token.value.set) + 1);
      strcpy (set, c_tr->token.value.set);
      c_copy->token.value.set = set;
    }

  return c_copy;
}
#undef FUNC_NAME

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


SCM_DEFINE (scm_make_reader, "make-reader", 1, 3, 0,
	    (SCM token_readers, SCM fault_handler_proc,
	     SCM record_pos_p, SCM debug_p),
	    "Create a reader.")
#define FUNC_NAME "make-reader"
{
  SCM s_reader, *s_deps;
  scm_reader_t reader;
  size_t code_size = 1024;
  void *code_buffer;
  size_t actual_size;
  unsigned token_reader_count, i;
  scm_token_reader_spec_t *c_specs;

  SCM_VALIDATE_LIST (1, token_readers);
  if (fault_handler_proc == SCM_UNDEFINED)
    fault_handler_proc = scm_reader_standard_fault_handler_proc;
  else if (fault_handler_proc != SCM_BOOL_F)
    SCM_VALIDATE_PROC (2, fault_handler_proc);

  /* Convert the list TOKEN_READERS to a C array in C_SPECS.  */
  token_reader_count = scm_to_uint (scm_length (token_readers));
  c_specs = alloca ((token_reader_count + 1) * sizeof (*c_specs));
  s_deps = scm_malloc ((token_reader_count + 2) * sizeof (SCM));
  for (i = 0;
       i < token_reader_count;
       i++, token_readers = SCM_CDR (token_readers))
    {
      scm_token_reader_spec_t *tr_spec;
      SCM tr = SCM_CAR (token_readers);
      scm_assert_smob_type (scm_token_reader_type, tr);

      SCM_TOKEN_READER_SMOB_DATA (tr_spec, tr);
      c_specs[i] = *tr_spec;

      /* Keep a copy of the TR SMOBs that the reader being built depends
	 on so that they can get marked appropriately during GC.  */
      s_deps[i] = tr;
    }

  /* Make it a zero-terminated array.  */
  c_specs[i].token.type = SCM_TOKEN_UNDEF;
  c_specs[i].name = NULL;
  c_specs[i].reader.type = SCM_TOKEN_READER_UNDEF;

  if (fault_handler_proc != SCM_BOOL_F)
    /* Add the fault handler proc as part of the SMOB's dependencies.  */
    s_deps[i++] = fault_handler_proc;

  /* Terminate the SMOB array with `#f'.  */
  s_deps[i] = SCM_BOOL_F;

  /* Go ahead with the reader compilation process.  */
  code_buffer = scm_malloc (code_size);

  do
    {
      reader = scm_c_make_reader (code_buffer, code_size,
				  c_specs, fault_handler_proc,
				  (record_pos_p == SCM_UNDEFINED) ? 0
				  : ((record_pos_p == SCM_BOOL_F) ? 0 : 1),
				  (debug_p == SCM_UNDEFINED) ? 0
				  : ((debug_p == SCM_BOOL_F) ? 0 : 1),
				  &actual_size);
      if (!reader)
	{
	  debug ("%s: reader too small (%u vs. %u)\n",
		 __FUNCTION__, code_size, actual_size);
	  code_size <<= 1;
	  code_buffer = scm_realloc (code_buffer, code_size);
	}
    }
  while (!reader);

/*   code_buffer = scm_realloc (code_buffer, actual_size); */

  /* Return a "freeable" SMOB, i.e. whose memory is owned and managed by
     Scheme code.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, s_deps, 1);

  debug ("new reader @ %p [SCM %p]\n", reader, s_reader);
  return (s_reader);
}
#undef FUNC_NAME

SCM_DEFINE (scm_default_reader, "default-reader", 0, 0, 0,
	    (void),
	    "Returns Guile's default reader.")
{
  SCM s_reader;

  /* This one may _not_ be freed by Scheme code at GC time.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, scm_standard_reader,
		       NULL, 0);
  return (s_reader);
}

SCM_DEFINE (scm_make_token_reader, "make-token-reader", 2, 1, 0,
	    (SCM spec, SCM proc, SCM escape_p),
	    "Use procedure (or reader) @var{proc} as a token reader for "
	    "the characters defined by @var{spec}.  If @var{escape_p} is true, "
	    "then the reader this token reader belongs to should return "
	    "even if its result is undefined.")
#define FUNC_NAME "make-token-reader"
{
  SCM *s_deps;
  SCM s_token_reader;
  scm_token_reader_spec_t *c_spec;

  if (proc != SCM_BOOL_F)
    SCM_VALIDATE_PROC (2, proc);

  if (escape_p == SCM_UNDEFINED)
    escape_p = SCM_BOOL_F;
  else
    SCM_VALIDATE_BOOL (3, escape_p);

  c_spec = scm_malloc (sizeof (scm_token_reader_spec_t));
  s_deps = scm_malloc (2 * sizeof (*s_deps));

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
    }
  else if (SCM_SMOB_PREDICATE (scm_token_reader_proc_type, proc))
    {
      /* Same as above.  */
      c_spec->reader.type = SCM_TOKEN_READER_C;
      c_spec->reader.value.c_reader =
	(scm_token_reader_t)SCM_SMOB_DATA (proc);
    }
  else if (scm_procedure_p (proc) == SCM_BOOL_T)
    {
      c_spec->reader.type = SCM_TOKEN_READER_SCM;
      c_spec->reader.value.scm_reader = proc;
    }
  else if (proc == SCM_BOOL_F)
    {
      /* A ``fake'' reader that does nothing.  Useful for whitespaces.  */
      c_spec->reader.type = SCM_TOKEN_READER_C;
      c_spec->reader.value.c_reader = NULL;
    }
  else
    {
      free (c_spec);
      scm_error (scm_from_locale_symbol ("reader-error"),
		 "make-token-reader", "invalid token reader procedure: ~A",
		 scm_list_1 (proc), SCM_EOL);
    }

  c_spec->escape = (escape_p == SCM_BOOL_T) ? 1 : 0;
  c_spec->name = NULL;

  /* Keep track of the SMOBs this token reader depends on.  */
  if (proc != SCM_BOOL_F)
    s_deps[0] = proc;
  else
    s_deps[0] = proc;

  s_deps[1] = SCM_BOOL_F;

  /* Return a "freeable" SMOB.  */
  SCM_NEW_READER_SMOB (s_token_reader, scm_token_reader_type,
		       c_spec, s_deps, 1);
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
  SCM_NEW_READER_SMOB (s_token_reader, scm_token_reader_type, spec,
		       NULL, 0);
  return (s_token_reader);
}
#undef FUNC_NAME

SCM_DEFINE (scm_token_reader_proc, "token-reader-procedure", 1, 0, 0,
	    (SCM tr),
	    "Return the procedure attached to token reader @var{tr}.  When "
	    "@code{#f} is returned, the @var{tr} is a ``fake'' reader that "
	    "does nothing.  This is typically useful for whitespaces.")
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
      if (c_tr->reader.value.reader)
	{
	  SCM s_reader;
	  SCM_NEW_READER_SMOB (s_reader, scm_reader_type,
			       c_tr->reader.value.reader,
			       NULL, /* FIXME:  We are not able to determine
					the `deps' of this reader at this
					point.  However, they are hopefully
					handled by another SMOB pointing to
					the same reader.  */
			       0);
	  debug ("t-r-p: new reader @ %p [SCM %p]\n",
		 c_tr->reader.value.reader, s_reader);
	  return (s_reader);
	}
      else
	return SCM_BOOL_F;

    case SCM_TOKEN_READER_C:
      if (c_tr->reader.value.c_reader)
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
      else
	return SCM_BOOL_F;

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

SCM_DEFINE (scm_token_reader_escape_p, "token-reader-escape?", 1, 0, 0,
	    (SCM tr),
	    "Return @code{#t} if token reader @var{tr} requires the readers "
	    "that use it to return even if its return value is unspecified.")
#define FUNC_NAME "token-reader-escape?"
{
  scm_token_reader_spec_t *c_tr;

  scm_assert_smob_type (scm_token_reader_type, tr);
  SCM_TOKEN_READER_SMOB_DATA (c_tr, tr);

  return (c_tr->escape ? SCM_BOOL_T : SCM_BOOL_F);
}
#undef FUNC_NAME



/* SMOB types.  */
scm_t_bits scm_reader_type, scm_token_reader_type,
  scm_token_reader_proc_type;


static SCM
generic_reader_smob_mark (SCM reader)
{
  scm_reader_smob_t *smobinfo;

  smobinfo = (scm_reader_smob_t *)SCM_SMOB_DATA (reader);
  if (smobinfo->deps)
    {
      SCM *deps, prev_dep;
      for (deps = smobinfo->deps, prev_dep = SCM_BOOL_F;
	   *deps != SCM_BOOL_F;
	   prev_dep = *deps, deps++)
	{
	  if ((prev_dep != SCM_BOOL_F) && (prev_dep != reader))
	    scm_gc_mark (prev_dep);
	}

      return ((prev_dep != reader) ? prev_dep : SCM_BOOL_F);
    }

  return SCM_BOOL_F;
}

static size_t
generic_reader_smob_free (SCM reader_smob)
{
  scm_reader_smob_t *smobinfo;

  smobinfo = (scm_reader_smob_t *)SCM_SMOB_DATA (reader_smob);
  assert (smobinfo);
  if (smobinfo->freeable)
    {
      void *c_object = smobinfo->c_object;
      unsigned smob_type = SCM_SMOBNUM (reader_smob);

      assert (c_object);
      debug ("freeing %s %p [SCM %p]\n",
	     SCM_SMOBNAME (smob_type), c_object, reader_smob);
      free (c_object);
    }

  smobinfo->freeable = 0;
  smobinfo->c_object = NULL;

  if (smobinfo->deps)
    free (smobinfo->deps);
  smobinfo->deps = NULL;

  free (smobinfo);

  return 0;
}

static SCM
reader_apply (SCM reader, SCM port, SCM caller_handled)
{
  scm_reader_t c_reader;

  SCM_READER_SMOB_DATA (c_reader, reader);
  debug ("%s: applying reader %p\n", __FUNCTION__, c_reader);

  /* Type checking and optional argument definition are checked in either
     `scm_call_reader ()' or the compiled reader's code.  */

  return (scm_call_reader (c_reader, port,
			   (caller_handled == SCM_BOOL_T) ? 1 :0));
}

static SCM
token_reader_proc_mark (SCM tr_proc)
{
  /* Nothing to mark.  */
  return SCM_BOOL_F;
}

static size_t
token_reader_proc_free (SCM tr_proc)
{
  /* Nothing to be freed.  */
#if 0
  debug ("freeing TR proc %p [SCM %p]\n",
	 (void *)SCM_SMOB_DATA (tr_proc), (void *)tr_proc);
#endif
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
  scm_set_smob_mark (scm_reader_type, generic_reader_smob_mark);
  scm_set_smob_free (scm_reader_type, generic_reader_smob_free);
  scm_set_smob_apply (scm_reader_type, reader_apply, 0, 2, 0);

  scm_token_reader_type = scm_make_smob_type ("token-reader", 0);
  scm_set_smob_mark (scm_token_reader_type, generic_reader_smob_mark);
  scm_set_smob_free (scm_token_reader_type, generic_reader_smob_free);

  scm_token_reader_proc_type = scm_make_smob_type ("token-reader-proc", 0);
  scm_set_smob_mark (scm_token_reader_proc_type, token_reader_proc_mark);
  scm_set_smob_free (scm_token_reader_proc_type, token_reader_proc_free);
  scm_set_smob_apply (scm_token_reader_proc_type, token_reader_proc_apply,
		      3, 0, 0);

#include "reader.c.x"

  /* Compile/load the standard reader.  */
  scm_load_standard_reader ();
}
