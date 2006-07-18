/* A Scheme reader compiler for Guile.

   Copyright (C) 2005, 2006  Ludovic Courtès  <ludovic.courtes@laas.fr>

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

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include <libguile.h>

#include "reader.h"
#include "token-readers.h"
#include "reader-lib.h"

#include "config.h" /* this defines `inline' among other things */

#ifdef SCM_READER_USE_LIGHTNING
# include <lightning.h>

/* If defined, an inlined implementation of `scm_getc ()' will be used.  */
# define SCM_READER_INLINE_GETC

#endif


/* Debugging support.  */
static void debug (const char *, ...)
#ifdef __GNUC__
     __attribute__ ((format (printf, 1, 2)))
#endif
     ;

#ifdef DEBUG
#include <stdio.h>

static inline void
debug (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
}

#else

static inline void
debug (const char *fmt, ...)
{
  /* Nothing.  */
}

#endif /* DEBUG */




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

static void
do_scm_set_source_position (SCM obj, long line, int column,
			    SCM filename)
{
  debug ("%s: o=%p l=%li c=%i f=%p\n",
	  __FUNCTION__, (void *)obj, line, column, (void *)filename);

  if (filename == SCM_BOOL_F)
    /* Maybe the input port was not a file port.  */
    return;

  assert (scm_is_string (filename));
  assert ((line >= 0L) && (column >= 0));

#ifdef DEBUG
  {
    size_t len;
    char *c_filename;

    len = scm_c_string_length (filename);
    c_filename = alloca (len + 1);
    scm_to_locale_stringbuf (filename, c_filename, len);
    c_filename[len] = 0;

    debug ("%s (obj=%p[%s], line=%u, col=%u, file=\"%s\")\n",
	   __FUNCTION__, (void *)obj,
	   SCM_IMP (obj) ? "imm" : "non-imm",
	   scm_to_uint (line), scm_to_uint (column),
	   c_filename);
  }
#endif

  if ((SCM_NIMP (obj)) && (scm_is_pair (obj)))
    {
      /* OBJ is a non-immediate Scheme value so we can set its source
	 properties.  */
      scm_set_source_property_x (obj, scm_sym_line, scm_from_long (line));
      scm_set_source_property_x (obj, scm_sym_column, scm_from_int (column));
      scm_set_source_property_x (obj, scm_sym_filename, filename);
    }
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

  if (!reader)
    return SCM_BOOL_F;

  /* We return a non-freeable reader SMOB with no `deps'.  Hopefully this
     will be a short-lived SMOB.  */
  SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader, NULL, 0);

  debug ("d-s-m-r-s: new reader @ %p [SCM %p]\n", reader, s_reader);

  return s_reader;
}



/* Code generation functions.  */

/* Register allocation invariants
   ------------------------------

   Registers V0, V1, and V2 are preserved accross function calls.  We have
   only three of them so care must be taken to use them carefully.  The
   following invariants were chosen:

   - V0 contains at any time the PORT argument;

   - V1 contains at any time the character just returned by `scm_getc ()',
     i.e., a C integer;

   - V2 contains the frame pointer, i.e. a pointer to the base of all stack
     variables.


   Variables allocated on the stack
   --------------------------------

   The generated code stores a few variables on the stack, starting at the
   frame pointer and going downwards.  The `JIT_STACK_' macros give the
   offset of those variables.  */

#define JIT_WORD_SIZE                 sizeof (void *)

#define JIT_STACK_CALLER_HANDLED      JIT_WORD_SIZE
#define JIT_STACK_TOP_LEVEL_READER    (JIT_WORD_SIZE * 2)
#define JIT_STACK_POSITION_FILENAME   (JIT_WORD_SIZE * 3)
#define JIT_STACK_POSITION_LINE       (JIT_WORD_SIZE * 4)
#define JIT_STACK_POSITION_COLUMN     (JIT_WORD_SIZE * 5)

/* Total size needed to store local variables.  */
#define JIT_STACK_LOCAL_VARIABLES_SIZE (JIT_WORD_SIZE * 8)

/* Fetch in RESULT_REG the pointer to the `scm_t_port' object corresponding
   to the `SCM' object in PORT_REG (normally, V0).  This is equivalent to
   `SCM_PTAB_ENTRY ()'.  */
#define SCM_JIT_PTAB_ENTRY(_result_reg, _port_reg)	\
  jit_ldxi_p ((_result_reg), (_port_reg), sizeof (SCM))


/* Aid in register-level debugging.  */

#ifdef DEBUG

# define debug_pre_call()						\
do									\
{									\
  generate_debug_registers (&_jit, 1, __LINE__, start, buffer_size);	\
} while (0)

# define debug_post_call()						\
do									\
{									\
  generate_debug_registers (&_jit, 0, __LINE__, start, buffer_size);	\
} while (0)

#else

# define debug_pre_call()   do {} while (0)
# define debug_post_call()  do {} while (0)

#endif


/* Generate code the calls FUNC, a printf-like function, where PTR1 points to
   an immediate format string containing either `%s' or `%p', and PTR2 is a
   pointer (immediate value).  REG1 (resp. REG2) is the register where PTR1
   (resp. PTR2) is stored.  */
#define DO_DEBUG_2_PP(_ptr1, _reg1, _ptr2, _reg2, _func)	\
do								\
{								\
  debug_pre_call ();					\
  jit_movi_p ((_reg1), (_ptr1));				\
  jit_movi_p ((_reg2), (_ptr2));				\
  jit_prepare (2);						\
  jit_pusharg_p ((_reg2));					\
  jit_pusharg_p ((_reg1));					\
  jit_finish ((_func));						\
  debug_post_call ();					\
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


/* Abort.  */
static void
guile_reader_abort (const void *v0, int v1, const void *v2)
{
  fprintf (stderr, "compiled reader aborting (v0=%p, v1=%i, v2=%p)\n",
	   v0, v1, v2);
  abort ();
}

static void
do_debug_regs (unsigned line, int entering, void *sp, void *v2, void *v1)
{
  static int level = 0;
  char indent[1024];
  unsigned i;

  if (!entering)
    level--;
  assert (level >= 0);

  for (i = 0; i < level; i++)
    indent[i] = '+';
  indent[level] = '\0';

  if (entering)
    level++;

  if (level < 0)
    level = 0;

  printf ("%sregisters:%u: SP=%p V2=%p V1=%p\n", indent, line, sp, v2, v1);
}

static inline int
generate_debug_registers (jit_state *lightning_state,
			  int enter, unsigned line,
			  char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_pushr_p (JIT_RET);
  jit_pushr_p (JIT_R0);
  jit_pushr_p (JIT_R1);
  jit_pushr_p (JIT_R2);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_movi_ui (JIT_R0, line);
  jit_movi_i (JIT_R1, enter);
  jit_prepare (5);
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_pusharg_p (JIT_V1);
  jit_pusharg_p (JIT_V2);
  jit_pusharg_p (JIT_SP);
  jit_pusharg_i (JIT_R1);
  jit_pusharg_i (JIT_R0);
  (void)jit_finish (do_debug_regs);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_popr_p (JIT_R2);
  jit_popr_p (JIT_R1);
  jit_popr_p (JIT_R0);
  jit_popr_p (JIT_RET);

  CHECK_CODE_SIZE (buffer_size, start, -1);
  return 0;
}
#undef _jit

/* Generate code that will fetch and store the current position information
   of PORT.  The relevant information is made available on the stack.  Expect
   the port-as-`SCM' in V0 and the frame pointer in V2.  */
static inline int
generate_position_store (jit_state *lightning_state,
			 char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  debug ("%s (@%p)\n", __FUNCTION__, jit_get_ip ().ptr);

  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Load the C port structure into R0.  */
  SCM_JIT_PTAB_ENTRY (JIT_R0, JIT_V0);

  /* Get the line (a `long') and column number (an `int') and save them onto
     the stack.  */
  jit_ldxi_l (JIT_R1, JIT_R0, offsetof (scm_t_port, line_number));
  jit_ldxi_i (JIT_R2, JIT_R0, offsetof (scm_t_port, column_number));
  jit_stxi_l (-JIT_STACK_POSITION_LINE, JIT_V2, JIT_R1);
  jit_stxi_i (-JIT_STACK_POSITION_COLUMN, JIT_V2, JIT_R2);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Same for the file name (an `SCM' object).  */
  jit_ldxi_p (JIT_R1, JIT_R0, offsetof (scm_t_port, file_name));
  jit_stxi_p (-JIT_STACK_POSITION_FILENAME, JIT_V2, JIT_R1);

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
static inline int
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
  debug_pre_call ();
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_prepare (4);
  jit_pusharg_p (JIT_R2);
  jit_pusharg_p (JIT_R1);
  jit_pusharg_p (JIT_R0);
  jit_pusharg_p (JIT_V1); /* expr */
  (void)jit_finish (do_scm_set_source_position);
  debug_post_call ();

  /* Put the expression read back in JIT_RET.  */
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_movr_p (JIT_RET, JIT_V1);
  jit_popr_i (JIT_V1);

  return 0;
}
#undef _jit

/* Generate code that converts the ASCII character in V1 to upper case.  */
static inline int
generate_to_upper (jit_state *lightning_state,
		   char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  jit_insn *lt_test, *gt_test;

  CHECK_CODE_SIZE (buffer_size, start, -1);
  lt_test = jit_blti_i (jit_forward (), JIT_V1, (int)'a');
  gt_test = jit_bgti_i (jit_forward (), JIT_V1, (int)'z');
  jit_movr_i (JIT_R0, JIT_V1);
  jit_subi_i (JIT_V1, JIT_R0, (int)('a' - 'A'));
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_patch (lt_test);
  jit_patch (gt_test);

  return 0;
}
#undef _jit

/* Generate code that converts the ASCII character in V1 to lower case.  */
static inline int
generate_to_lower (jit_state *lightning_state,
		   char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  jit_insn *lt_test, *gt_test;

  CHECK_CODE_SIZE (buffer_size, start, -1);
  lt_test = jit_blti_i (jit_forward (), JIT_V1, (int)'A');
  gt_test = jit_bgti_i (jit_forward (), JIT_V1, (int)'Z');
  jit_movr_i (JIT_R0, JIT_V1);
  jit_addi_i (JIT_V1, JIT_R0, (int)('a' - 'A'));
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_patch (lt_test);
  jit_patch (gt_test);

  return 0;
}
#undef _jit

#ifdef SCM_READER_INLINE_GETC
/* Functions producing an inline version of `scm_getc ()'.  */

/* Update a port's column and line numbers based on the character just read
   from it.  Assumes a pointer to the C port structure is in R0 and the last
   character read from it is in V1.  Clobbers R1 and R2.  */
static inline int
generate_getc_update_port_position (jit_state *lightning_state,
				    char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  /* This code mimics the `switch' statement that appears at the end of the C
     version of `scm_getc ()'.  */

  /* XXX: We're assuming that the `line_number' field is a `long' while the
     `column_number' is an `int'.  */

  jit_insn *test,
    *alarm_jump, *bs_jump, *nl_jump, *reset_jump, *tab_jump;

  /* Alarm: don't change line and column numbers.  */
  alarm_jump = jit_beqi_i (jit_forward (), JIT_V1, (int)'\a');

  /* Backspace: decrease column number if greater than zero.  */
  test = jit_bnei_i (jit_forward (), JIT_V1, (int)'\b');
  jit_ldxi_i (JIT_R1, JIT_R0, offsetof (scm_t_port, column_number));
  CHECK_CODE_SIZE (buffer_size, start, -1);
  {
    jit_insn *lt_zero;

    lt_zero = jit_blei_i (jit_forward (), JIT_R1, 0);
    jit_subi_i (JIT_R2, JIT_R1, 1);
    jit_stxi_i (offsetof (scm_t_port, column_number), JIT_R0, JIT_R2);
    jit_patch (lt_zero);
  }
  bs_jump = jit_jmpi (jit_forward ());
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* New line: increase line number and reset column number.  */
  jit_patch (test);
  test = jit_bnei_i (jit_forward (), JIT_V1, (int)'\n');
  jit_ldxi_l (JIT_R1, JIT_R0, offsetof (scm_t_port, line_number));
  jit_addi_l (JIT_R2, JIT_R1, 1);
  jit_stxi_l (offsetof (scm_t_port, line_number), JIT_R0, JIT_R2);
  jit_movi_i (JIT_R2, 0);
  jit_stxi_i (offsetof (scm_t_port, column_number), JIT_R0, JIT_R2);
  nl_jump = jit_jmpi (jit_forward ());
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Reset: reset the column number.  */
  jit_patch (test);
  test = jit_bnei_i (jit_forward (), JIT_V1, (int)'\r');
  jit_movi_i (JIT_R2, 0);
  jit_stxi_i (offsetof (scm_t_port, column_number), JIT_R0, JIT_R2);
  reset_jump = jit_jmpi (jit_forward ());
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Tab: set column number to (C + 8 - (C % 8)).  */
  jit_patch (test);
  test = jit_bnei_i (jit_forward (), JIT_V1, (int)'\t');
  jit_ldxi_i (JIT_R1, JIT_R0, offsetof (scm_t_port, column_number));
  jit_addi_i (JIT_R2, JIT_R1, 8);      /* R2 = R1+8 */
  jit_andi_i (JIT_R1, JIT_R2, 7);      /* R1 = R2%8 */
  jit_subr_i (JIT_R1, JIT_R2, JIT_R1); /* R1 = R2-R1 */
  jit_stxi_i (offsetof (scm_t_port, column_number), JIT_R0, JIT_R1);
  tab_jump = jit_jmpi (jit_forward ());
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Default: increase column number.  */
  jit_patch (test);
  jit_ldxi_i (JIT_R1, JIT_R0, offsetof (scm_t_port, column_number));
  jit_addi_i (JIT_R2, JIT_R1, 1);
  jit_stxi_i (offsetof (scm_t_port, column_number), JIT_R0, JIT_R2);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* End of `switch' statement.  */
  jit_patch (alarm_jump);
  jit_patch (bs_jump);
  jit_patch (nl_jump);
  jit_patch (reset_jump);
  jit_patch (tab_jump);

  return 0;
}
#undef _jit


/* Generate an implementation of `scm_getc ()' inline.  The code expects the
   `SCM' port object in V0 and return its result in V1.  It potentially
   clobbers all the `R' registers.  Note: This is an exact re-implementation
   of the C version that's in `ports.c' as of Guile 1.8.0 (and most older
   versions certainly).  */
static inline int
generate_getc (jit_state *lightning_state,
	       int debug,
	       char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  jit_insn *rw_check, *position_check, *eof_check,
    *random_access_check, *jump_to_end;

  /* Throughout this piece of code, we try to keep the pointer to the C port
     structure in R0.  */

  CHECK_CODE_SIZE (buffer_size, start, -1);
  SCM_JIT_PTAB_ENTRY (JIT_R0, JIT_V0);

  /* Check whether read-write.  */
  jit_ldxi_p (JIT_R1, JIT_R0, offsetof (scm_t_port, rw_active));
  rw_check = jit_bnei_p (jit_forward (), JIT_R1, SCM_PORT_WRITE);

  /* Invoke `scm_flush ()', passing it the port as SCM, and then reload the C
     port into R0.  */
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (scm_flush);
  SCM_JIT_PTAB_ENTRY (JIT_R0, JIT_V0);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_patch (rw_check);

  /* Check whether `port->rw_random' is true.  If this is the case, set
     `port->rw_active' to `SCM_PORT_READ'.  */
  jit_ldxi_i (JIT_R1, JIT_R0, offsetof (scm_t_port, rw_random));
  random_access_check = jit_beqi_i (jit_forward (), JIT_R1, 0);
  jit_movi_i (JIT_R1, SCM_PORT_READ);
  jit_stxi_i (offsetof (scm_t_port, rw_active), JIT_R0, JIT_R1);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_patch (random_access_check);

  /* Check whether `read_pos' is passed `read_end'.  */
  jit_ldxi_p (JIT_R1, JIT_R0, offsetof (scm_t_port, read_pos));
  jit_ldxi_p (JIT_R2, JIT_R0, offsetof (scm_t_port, read_end));
  position_check = jit_bltr_p (jit_forward (), JIT_R1, JIT_R2);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Since `port->read_pos >= port->read_end', we must fill it again.  */
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (scm_fill_input);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Check whether `scm_fill_input ()' return `EOF'.  If so, return `EOF'.  */
  jit_retval_i (JIT_R1);
  SCM_JIT_PTAB_ENTRY (JIT_R0, JIT_V0);
  eof_check = jit_bnei_i (jit_forward (), JIT_R1, EOF);
  jit_movi_i (JIT_V1, EOF);
  jump_to_end = jit_jmpi (jit_forward ());
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_patch (position_check);
  jit_patch (eof_check);

  /* Load the character at `port->read_pos' in V1 and increment
     `port->read_pos'.  */
  jit_ldxi_p (JIT_R1, JIT_R0, offsetof (scm_t_port, read_pos));
  jit_ldxi_c (JIT_V1, JIT_R1, 0); /* the character */
  jit_addi_p (JIT_R2, JIT_R1, 1);
  jit_stxi_p (offsetof (scm_t_port, read_pos), JIT_R0, JIT_R2);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  if (generate_getc_update_port_position (lightning_state,
					  start, buffer_size))
    return -1;

  jit_patch (jump_to_end);


  if (debug)
    {
      static const char msg_read[] = "getc returned %i\n";

      (void)jit_movi_p (JIT_R2, msg_read);
      jit_prepare (2);
      jit_pusharg_i (JIT_V1);
      jit_pusharg_p (JIT_R2);
      (void)jit_finish (do_like_printf);

      CHECK_CODE_SIZE (buffer_size, start, -1);
    }

  return 0;
}
#undef _jit
#endif

/* Reserve at least HOW_MUCH bytes starting after CODE_BUFFER.  Produce an
   unconditional `jump' instruction right at CODE_BUFFER.  The exact location
   of the HOW_MUCH bytes available is returned in RESULT.  */
static inline int
generate_space_reservation (jit_state *lightning_state, size_t how_much,
			    char *start, size_t buffer_size,
			    void **result)
#define _jit (* lightning_state)
{
  jit_insn *jump;

  jump = jit_jmpi (jit_forward ());

  CHECK_CODE_SIZE (buffer_size, start, -1);
  *result = (void *)jit_get_label ();

  /* Fill the buffer with `nop's, thereby preserving alignment constraints. */
  while (jit_get_ip ().ptr - (char *)*result < how_much)
    {
      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_nop ();
    }

  debug ("%s: %u bytes requested, %u bytes allocated\n", __FUNCTION__,
	 how_much, jit_get_ip ().ptr - (char *)*result);

  jit_patch (jump);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  return 0;
}
#undef _jit

/* Generate a prologue that reserves enough space on the stack to store the
   reader's local variables and store the original value of the stack pointer
   (which we'll refer to as the ``frame pointer'') in V2.  */
static inline int
generate_reader_prologue (jit_state *lightning_state,
			  int debug,
			  char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  static const char *msg_prologue = "reader prologue: SP is %p\n";
  jit_insn *ref;

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

  /* Set up the stack, saving space for local variables.  */
  jit_movr_p (JIT_V2, JIT_SP);
  jit_subi_p (JIT_SP, JIT_V2, JIT_STACK_LOCAL_VARIABLES_SIZE+16);

  CHECK_CODE_SIZE (buffer_size, start, -1);

  /* Store the CALLER_HANDLED argument (currently in V1) and the
     TOP_LEVEL_READER argument (in R2) on the stack.  */
  jit_stxi_i (-JIT_STACK_CALLER_HANDLED, JIT_V2, JIT_V1);
  {
    /* If TOP_LEVEL_READER is NULL, then we'll advertise ourself as the
       top-level reader.  */
    ref = jit_bnei_p (jit_forward (), JIT_R2, NULL);
    jit_movi_p (JIT_R2, start);
    jit_patch (ref);
    jit_stxi_p (-JIT_STACK_TOP_LEVEL_READER, JIT_V2, JIT_R2);
    CHECK_CODE_SIZE (buffer_size, start, -1);
  }

  /* FIXME:  We should check the type of PORT here.  */

  /* The PORT argument is optional.  If not passed (i.e. equals to
     SCM_UNDEFINED), default to the current input port.  */
  ref = jit_bnei_p (jit_forward (), JIT_V0, (void *)SCM_UNDEFINED);
  (void)jit_finish (scm_current_input_port);
  jit_retval_p (JIT_V0);
  jit_patch (ref);

  return 0;
}
#undef _jit

/* Generate code that restores the stack pointer from the frame pointer
   and returns.  The usual register invariants are assumed.  */
static inline int
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
      debug_pre_call ();
      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_prepare (2);
      jit_pusharg_p (JIT_V2);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);
      debug_post_call ();
      CHECK_CODE_SIZE (buffer_size, start, -1);

      jit_popr_p (JIT_RET);
    }

  jit_movr_p (JIT_SP, JIT_V2);
  jit_ret ();

  CHECK_CODE_SIZE (buffer_size, start, -1);
  return 0;
}
#undef _jit

/* Generate code that aborts.  */
static inline int
generate_abortion (jit_state *lightning_state,
		   char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  CHECK_CODE_SIZE (buffer_size, start, -1);

  jit_prepare (3);
  jit_pusharg_p (JIT_V2);
  jit_pusharg_i (JIT_V1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (guile_reader_abort);

  CHECK_CODE_SIZE (buffer_size, start, -1);

  return 0;
}
#undef _jit

/* Generate code that jumps to the location associated with the character
   just read (in V1) according to JUMP_TABLE.  JUMP_TABLE is assumed to
   contain only valid positions into code.  */
static inline int
generate_character_dispatch (jit_state *lightning_state,
			     jit_insn **jump_table,
			     int debug,
			     char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  jit_insn *test;

  CHECK_CODE_SIZE (buffer_size, start, -1);
  if (debug)
    {
      static const char msg_dispatching[] = "dispatching for character `%i'\n";

      jit_movi_p (JIT_R0, msg_dispatching);

      jit_prepare (2);
      jit_pusharg_i (JIT_V1);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);

      CHECK_CODE_SIZE (buffer_size, start, -1);
    }

  /* Make sure the character just read is in the range [0;255].  */

  test = jit_bgei_i (jit_forward (), JIT_V1, 0);
  if (generate_abortion (&_jit, start, buffer_size))
    return -1;

  jit_patch (test);
  test = jit_blei_i (jit_forward (), JIT_V1, 255);
  if (generate_abortion (&_jit, start, buffer_size))
    return -1;

  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_patch (test);

  /* Load the pointer at JUMP_TABLE + (V1 * sizeof (void *)).  */
#if SIZEOF_VOID_P == 4
  jit_lshi_i (JIT_R1, JIT_V1, 2);
#elif SIZEOF_VOID_P == 8
  jit_lshi_i (JIT_R1, JIT_V1, 3);
#else
# error "unsupported pointer size"
#endif
  jit_ldxi_p (JIT_R0, JIT_R1, jump_table);

  if (debug)
    {
      static const char msg_jumping[] = "preparing to jump at %p\n";

      jit_pushr_p (JIT_R0);

      jit_movi_p (JIT_R1, msg_jumping);
      jit_prepare (2);
      jit_pusharg_p (JIT_R0);
      jit_pusharg_p (JIT_R1);
      (void)jit_finish (do_like_printf);

      jit_popr_p (JIT_R0);
      CHECK_CODE_SIZE (buffer_size, start, -1);
    }

  /* Jump to the handling code.  */
  jit_jmpr (JIT_R0);

  CHECK_CODE_SIZE (buffer_size, start, -1);

  return 0;
}
#undef _jit

/* Generate an invocation of token reader TR, assuming all the usual register
   invariants.  A jump to DO_AGAIN is generated when a NULL reader is
   encountered or when an escaping reader returns `SCM_UNSPECIFIED'.  If
   DEBUG is true (non-zero), additional debugging code is generated.  If
   POSITIONS is true, then position-recording code is generated.  */
static inline int
generate_token_reader_invocation (jit_state *lightning_state,
				  const scm_token_reader_spec_t *tr,
				  jit_insn *do_again,
				  int debug, int positions,
				  char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  static const char msg_start_c_call[] = "calling token reader `%s'...\n";
  static const char msg_start_scm_call[] =
    "calling Scheme token reader `%s'...\n";
  static const char msg_start_reader_call[] =
    "calling reader `%s'...\n";
  static const char msg_end_of_call[] = "token reader `%s' finished\n";

  switch (tr->reader.type)
    {
    case SCM_TOKEN_READER_C:
      if (tr->reader.value.c_reader)
	{
	  /* Call the C reader function associated with this
	     character.  */
	  static const char *nameless = "<nameless>";
	  const char *name = (tr->name ? tr->name : nameless);
	  void *func = (void *)tr->reader.value.c_reader;

	  if (debug)
	    {
	      CHECK_CODE_SIZE (buffer_size, start, -1);
	      DO_DEBUG_2_PP (msg_start_c_call, JIT_R0,
			     name, JIT_R1, do_like_printf);
	    }

	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  (void)jit_movi_p (JIT_R0, start);
	  jit_ldxi_i (JIT_R1, JIT_V2, -JIT_STACK_TOP_LEVEL_READER);

	  debug_pre_call ();
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_prepare (4);
	  jit_pusharg_p (JIT_R1); /* top-level reader */
	  jit_pusharg_p (JIT_R0); /* reader */
	  jit_pusharg_p (JIT_V0); /* port */
	  jit_pusharg_i (JIT_V1); /* character */
	  (void)jit_finish (func);
	  debug_post_call ();

	  if (debug)
	    {
	      CHECK_CODE_SIZE (buffer_size, start, -1);
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
	(void)jit_jmpi (do_again);

      break;

    case SCM_TOKEN_READER_SCM:
      /* Call the Scheme proc associated with this character.  */
      if (scm_procedure_p (tr->reader.value.scm_reader) == SCM_BOOL_T)
	{
	  char spec_str[100];

	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  token_spec_to_string (tr, spec_str);
	  if (debug)
	    {
	      /* FIXME:  SPEC_STR must be computed at run-time!  */
	      CHECK_CODE_SIZE (buffer_size, start, -1);
	      DO_DEBUG_2_PP (msg_start_scm_call, JIT_R0,
			     "spec_str", JIT_R1, do_like_printf);
	    }

	  CHECK_CODE_SIZE (buffer_size, start, -1);

	  /* Save the current value of V1 (the char as an `int').  */
	  jit_pushr_i (JIT_V1);

	  /* Convert the character read to a Scheme char.  */
	  debug_pre_call ();
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_prepare (1);
	  jit_pusharg_i (JIT_V1);
	  (void)jit_finish (do_scm_make_char);
	  debug_post_call ();
	  jit_retval_p (JIT_V1);

	  /* Same for the reader.  */
	  (void)jit_movi_p (JIT_R0, start);
	  debug_pre_call ();
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_prepare (1);
	  jit_pusharg_i (JIT_R0);
	  jit_finish (do_scm_make_reader_smob);
	  debug_post_call ();
	  jit_retval_p (JIT_R0);

	  /* Same for the top-level reader.  */
	  jit_pushr_p (JIT_R0);
	  jit_ldxi_i (JIT_R2, JIT_V2, -JIT_STACK_TOP_LEVEL_READER);
	  debug_pre_call ();
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_prepare (1);
	  jit_pusharg_i (JIT_R2);
	  jit_finish (do_scm_make_reader_smob);
	  debug_post_call ();
	  jit_retval_p (JIT_R2);
	  jit_popr_p (JIT_R0);

	  /* Actually call the Scheme procedure.  */
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_movi_p (JIT_R1, (void *)tr->reader.value.scm_reader);
	  debug_pre_call ();
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_prepare (5);
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_pusharg_p (JIT_R2); /* top-level reader */
	  jit_pusharg_p (JIT_R0); /* reader */
	  jit_pusharg_p (JIT_V0); /* port */
	  jit_pusharg_p (JIT_V1); /* character (Scheme) */
	  jit_pusharg_p (JIT_R1); /* procedure */
	  jit_finish (scm_call_4);
	  debug_post_call ();

	  /* Restore the C character.  */
	  jit_popr_i (JIT_V1);

	  if (debug)
	    {
	      CHECK_CODE_SIZE (buffer_size, start, -1);
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
	      CHECK_CODE_SIZE (buffer_size, start, -1);
	      DO_DEBUG_2_PP (msg_start_reader_call, JIT_R0,
			     "spec_str", JIT_R1, do_like_printf);
	    }

	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_ldxi_i (JIT_R2, JIT_V2, -JIT_STACK_TOP_LEVEL_READER);
	  jit_movi_i (JIT_R0, 0); /* let the callee handle its things */
	  debug_pre_call ();
	  CHECK_CODE_SIZE (buffer_size, start, -1);
	  jit_prepare (3);
	  jit_pusharg_p (JIT_R2); /* top-level reader */
	  jit_pusharg_i (JIT_R0); /* caller_handled */
	  jit_pusharg_p (JIT_V0); /* port */
	  jit_finish (tr->reader.value.reader);
	  debug_post_call ();

	  if (debug)
	    {
	      CHECK_CODE_SIZE (buffer_size, start, -1);
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

  CHECK_CODE_SIZE (buffer_size, start, -1);

  if (!tr->escape)
    /* When the reader returns SCM_UNSPECIFIED, then start again.
       This is what, for instance, comment readers do in the
       top-level reader.  */
    jit_beqi_p (do_again, JIT_RET, (void *)SCM_UNSPECIFIED);

  if (positions)
    {
      generate_position_set (&_jit, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start, -1);
    }

  return 0;
}
#undef _jit

/* Generate code that handles an unexpected character (the character, a C
   integer, is expected to be in V1 at this point).  I.e., if the
   CALLER_HANDLED argument passed to the function being generated (this
   argument is expected to be on the stack) is true, then do nothing.
   Otherwise, call FAULT_HANDLER.  */
static inline int
generate_unexpected_character_handling (jit_state *lightning_state,
					SCM fault_handler, int debug,
					char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  jit_insn *ref;

  /* Check whether the CALLER_HANDLED argument is true, in which case
     we'll simply return the faulty character to PORT and return.  */
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_ldxi_i (JIT_R0, JIT_V2, -JIT_STACK_CALLER_HANDLED);
  ref = jit_beqi_i (jit_forward (), JIT_R0, 0);
  debug_pre_call ();
  CHECK_CODE_SIZE (buffer_size, start, -1);
  jit_prepare (2);
  jit_pusharg_p (JIT_V0); /* port */
  jit_pusharg_i (JIT_V1); /* character */
  (void)jit_finish (scm_ungetc);
  debug_post_call ();
  CHECK_CODE_SIZE (buffer_size, start, -1);

  (void)jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
  generate_reader_epilogue (&_jit, debug, start, buffer_size);
  CHECK_CODE_SIZE (buffer_size, start, -1);

  if (scm_procedure_p (fault_handler) == SCM_BOOL_T)
    {
      jit_patch (ref);

      /* Else, since CALLER_HANDLED is false, call the user-defined fault
	 handler.  */
      CHECK_CODE_SIZE (buffer_size, start, -1);

      jit_pushr_p (JIT_V2); /* save the frame pointer */

      (void)jit_movi_p (JIT_R1, start);
      jit_prepare (1);
      jit_pusharg_p (JIT_R1);
      (void)jit_finish (do_scm_make_reader_smob);
      jit_retval_p (JIT_V2);
      CHECK_CODE_SIZE (buffer_size, start, -1);

      debug_pre_call ();
      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_prepare (1);
      jit_pusharg_i (JIT_V1);
      (void)jit_finish (do_scm_make_char);
      jit_retval_p (JIT_R1);
      debug_post_call ();

      CHECK_CODE_SIZE (buffer_size, start, -1);
      (void)jit_movi_p (JIT_R0, (void *)fault_handler);
      debug_pre_call ();
      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_prepare (4);
      jit_pusharg_p (JIT_V2);  /* reader */
      jit_pusharg_p (JIT_V0);  /* port */
      jit_pusharg_p (JIT_R1);  /* character */
      jit_pusharg_p (JIT_R0);  /* procedure */
      (void)jit_finish (scm_call_3);
      debug_post_call ();

      jit_popr_p (JIT_V2); /* restore the frame pointer */
    }
  else
    {
      jit_patch (ref);

      /* CALLER_HANDLED is false and the user did not define any method to
	 handle this situation so return the faulty character to PORT.  */
      CHECK_CODE_SIZE (buffer_size, start, -1);
      debug_pre_call ();
      CHECK_CODE_SIZE (buffer_size, start, -1);
      jit_prepare (2);
      jit_pusharg_p (JIT_V0); /* port */
      jit_pusharg_i (JIT_V1); /* character */
      (void)jit_finish (scm_ungetc);
      debug_post_call ();

      (void)jit_movi_p (JIT_RET, (void *)SCM_UNSPECIFIED);
    }

  CHECK_CODE_SIZE (buffer_size, start, -1);

  return 0;
}
#undef _jit

/* For each character CHR handled by TR, set JUMP_TABLE[CHR] to
   HANDLER_CODE. */
static void
populate_jump_table_for_tr (const scm_token_reader_spec_t *tr,
			    jit_insn **jump_table,
			    jit_insn *handler_code)
{
  switch (tr->token.type)
    {
    case SCM_TOKEN_SINGLE:
      jump_table[(int)tr->token.value.single] = handler_code;
      break;

    case SCM_TOKEN_RANGE:
      {
	int c;
	for (c = (int)tr->token.value.range.low;
	     c <= (int)tr->token.value.range.high;
	     c++)
	  jump_table[c] = handler_code;
      }
      break;

    case SCM_TOKEN_SET:
      {
	const char *p;
	for (p = tr->token.value.set; *p; p++)
	  jump_table[(int)*p] = handler_code;
      }
      break;

    default:
      abort ();
    }
}

/* Generate character-handling code, filling in JUMP_TABLE.  This code will
   be used by the character-dispatch code when referring to JUMP_TABLE.
   LOOP_START should be a pointer to the first instruction of the reader
   loop.  The usual register invariants are assumed.  */
static int
generate_character_handling_code (jit_state *lightning_state,
				  const scm_token_reader_spec_t *token_readers,
				  jit_insn **jump_table,
				  jit_insn *loop_start,
				  unsigned flags,
				  char *start, size_t buffer_size)
#define _jit (* lightning_state)
{
  const scm_token_reader_spec_t *tr;

  /* Populate the jump table: for each TR, generate an invocation followed by
     a `ret'.  If the TR is a NULL token reader, then simply generate a jump
     to LOOP_START.  */
  for (tr = token_readers;
       tr->token.type != SCM_TOKEN_UNDEF;
       tr++)
    {
      jit_insn *handler_code;

      CHECK_CODE_SIZE (buffer_size, start, -1);

      if (tr->reader.value.c_reader == NULL)
	handler_code = loop_start;
      else
	{
	  handler_code = jit_get_label ();
	  if (generate_token_reader_invocation (&_jit, tr, loop_start,
						flags & SCM_READER_FLAG_DEBUG,
						flags
						& SCM_READER_FLAG_POSITIONS,
						start, buffer_size))
	    return -1;

	  /* The reader's return value is already in JIT_RET, so we just have
	     to return.  */
	  if (generate_reader_epilogue (&_jit, flags & SCM_READER_FLAG_DEBUG,
					start, buffer_size))
	    return -1;

	  CHECK_CODE_SIZE (buffer_size, start, -1);
	}

      populate_jump_table_for_tr (tr, jump_table, handler_code);
    }

  return 0;
}
#undef _jit



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
		   unsigned flags,
		   size_t *code_size)
{
  static const char msg_getc[] = "got char: 0x%02x\n";

  scm_reader_t result;
  char *start, *end;
  jit_insn *do_again, *jump_to_end;
  int arg_port, arg_caller_handled, arg_top_level_reader;
  jit_insn **jump_table;

  result = (scm_reader_t) (jit_set_ip (code_buffer).iptr);
  start = jit_get_ip ().ptr;

  /* Take three arguments (the port, an `int', and the top-level reader) and
     put them into V0 and V1 (preserved accross function calls) and R2 (not
     preserved---but we put it on the stack soon after).  */
  jit_prolog (3);
  arg_port = jit_arg_p ();
  jit_getarg_p (JIT_V0, arg_port);
  arg_caller_handled = jit_arg_i ();
  jit_getarg_i (JIT_V1, arg_caller_handled);
  arg_top_level_reader = jit_arg_p ();
  jit_getarg_p (JIT_R2, arg_top_level_reader);


  /* Assuming the stack grows downwards, reserve some space for local
     variables and store the ``frame pointer'' (i.e. the beginning of the
     local variables array) in V2.  */
  generate_reader_prologue (&_jit, flags & SCM_READER_FLAG_DEBUG,
			    start, buffer_size);
  CHECK_CODE_SIZE (buffer_size, start);

  /* Allocate room for a character jump table.  */
  if (generate_space_reservation (&_jit, 256 * sizeof (*jump_table),
				  start, buffer_size,
				  (void **)&jump_table))
    return NULL;

  /* Clear the jump table.  */
  memset (jump_table, 0, 256 * sizeof (*jump_table));

  /* Beginning of the reader loop.  */
  CHECK_CODE_SIZE (buffer_size, start);
  do_again = jit_get_label ();

  if (flags & SCM_READER_FLAG_POSITIONS)
    {
      /* Before invoking `scm_getc ()', keep track of the current position of
	 PORT.  */
      generate_position_store (&_jit, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start);
    }

#ifndef SCM_READER_INLINE_GETC
  /* Call `scm_getc ()'.  */
  debug_pre_call ();
  CHECK_CODE_SIZE (buffer_size, start);
  jit_prepare (1);
  jit_pusharg_p (JIT_V0);
  (void)jit_finish (scm_getc);
  debug_post_call ();

  /* Put the character just read (an `int') in V1 (preserved accross function
     calls).  */
  jit_retval_i (JIT_V1);
#else
  /* Use an inlined version of `scm_getc ()'.  The character just read is put
     in V1.  */
  if (generate_getc (&_jit, flags & SCM_READER_FLAG_DEBUG,
		     start, buffer_size))
    return NULL;
#endif

  /* Test whether we got `EOF'.  */
  jump_to_end = jit_beqi_i (jit_forward (), JIT_V1, (int)EOF);

  CHECK_CODE_SIZE (buffer_size, start);

  if (flags & SCM_READER_FLAG_UPPER_CASE)
    {
      generate_to_upper (&_jit, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start);
    }
  else if (flags & SCM_READER_FLAG_LOWER_CASE)
    {
      generate_to_lower (&_jit, start, buffer_size);
      CHECK_CODE_SIZE (buffer_size, start);
    }

  if (flags & SCM_READER_FLAG_DEBUG)
    {
      /* Print a debug message.  */
      (void)jit_movi_p (JIT_R0, msg_getc);
      debug_pre_call ();
      CHECK_CODE_SIZE (buffer_size, start);
      jit_prepare (2);
      jit_pusharg_i (JIT_V1);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);
      debug_post_call ();
    }

  /* Lookup the character just read into the jump table and jump there.  */
  if (generate_character_dispatch (&_jit, jump_table,
				   flags & SCM_READER_FLAG_DEBUG,
				   start, buffer_size))
    return NULL;

  /* This is where we get when `scm_getc ()' returned EOF.  */
  jit_patch (jump_to_end);
  jit_movi_p (JIT_RET, (void *)SCM_EOF_VAL);
  generate_reader_epilogue (&_jit, flags & SCM_READER_FLAG_DEBUG,
			    start, buffer_size);

  /* Generate the character-handling code that will be used by the
     character-dispatching code.  */
  if (generate_character_handling_code (&_jit, token_readers,
					jump_table, do_again, flags,
					start, buffer_size))
    return NULL;


  {
    /* Fill out the remaining of the jump table with jumps to
       unexpected-character-handling code.  */
    size_t chr;
    jit_insn *unexpected_handling;

    unexpected_handling = jit_get_label ();
    if (generate_unexpected_character_handling (&_jit, fault_handler,
						flags & SCM_READER_FLAG_DEBUG,
						start, buffer_size))
      return NULL;

    if (generate_reader_epilogue (&_jit, flags & SCM_READER_FLAG_DEBUG,
				  start, buffer_size))
      return NULL;

    for (chr = 0; chr < 256; chr++)
      {
	if (jump_table[chr] == NULL)
	  jump_table[chr] = unexpected_handling;
      }
  }


  end = jit_get_ip ().ptr;
  jit_flush_code (start, end);

  *code_size = end - start;
  if (flags & SCM_READER_FLAG_DEBUG)
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
  unsigned flags;

  scm_token_reader_spec_t *eightbit_char_to_tr_map[256];
};

scm_reader_t
scm_c_make_reader (void *code_buffer,
		   size_t buffer_size,
		   const scm_token_reader_spec_t *token_readers,
		   SCM fault_handler_proc,
		   unsigned flags,
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
  result->flags = flags;
  memset (result->eightbit_char_to_tr_map, 0,
	  sizeof (result->eightbit_char_to_tr_map));

  tr_copy = (scm_token_reader_spec_t *)(buffer + sizeof (*result));

  result->token_readers = tr_copy;

  for (tr = token_readers;
       tr->token.type != SCM_TOKEN_UNDEF;
       tr++, tr_copy++, *code_size += sizeof (*tr))
    {
      if (*code_size + sizeof (*tr) > buffer_size)
	return NULL;

      memcpy (tr_copy, tr, sizeof (*tr));

      /* Fill out the character-to-token-reader map (for faster lookup).  */
      switch (tr->token.type)
	{
	case SCM_TOKEN_SINGLE:
	  result->eightbit_char_to_tr_map[(int)tr->token.value.single]
	    = tr_copy;
	  break;

	case SCM_TOKEN_RANGE:
	  {
	    char c;
	    for (c = tr->token.value.range.low;
		 c <= tr->token.value.range.high;
		 c++)
	      result->eightbit_char_to_tr_map[(int)c] = tr_copy;
	  }
	  break;

	case SCM_TOKEN_SET:
	  {
	    const char *p;
	    for (p = tr->token.value.set; *p; p++)
	      result->eightbit_char_to_tr_map[(int)*p] = tr_copy;
	  }
	  break;

	default:
	  abort ();
	}
    }

  /* Copy the terminating zero.  */
  if (*code_size + sizeof (*tr) > buffer_size)
    return NULL;

  memcpy (tr_copy, tr, sizeof (*tr));
  *code_size += sizeof (*tr);

  return result;
}

static SCM
tr_invoke (const scm_token_reader_spec_t *tr, char c, SCM port,
	   scm_reader_t reader, scm_reader_t top_level_reader)
{
  switch (tr->reader.type)
    {
    case SCM_TOKEN_READER_C:
      if (tr->reader.value.c_reader)
	return tr->reader.value.c_reader (c, port, reader,
					  top_level_reader);
      else
	return SCM_UNSPECIFIED;

    case SCM_TOKEN_READER_SCM:
      {
	SCM s_reader, s_top_level_reader;

	SCM_NEW_READER_SMOB (s_reader, scm_reader_type, reader,
			     NULL, 0);
	SCM_NEW_READER_SMOB (s_top_level_reader, scm_reader_type,
			     top_level_reader, NULL, 0);

	return scm_call_4 (tr->reader.value.scm_reader,
			   SCM_MAKE_CHAR (c), port, s_reader,
			   s_top_level_reader);
      }

    case SCM_TOKEN_READER_READER:
      if (tr->reader.value.reader)
	return scm_call_reader (tr->reader.value.reader, port, 0,
				top_level_reader);
      else
	return SCM_UNSPECIFIED;

    default:
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}

SCM
scm_call_reader (scm_reader_t reader, SCM port, int caller_handled,
		 scm_reader_t top_level_reader)
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
  while (1)
    {
      int column = 0;
      long line = 0L;
      SCM filename = SCM_BOOL_F;

      if (reader->flags & SCM_READER_FLAG_POSITIONS)
	{
	  column = SCM_COL (port);
	  line = SCM_LINUM (port);
	  filename = SCM_FILENAME (port);
	}

      c = scm_getc (port);
      if (c == EOF)
	break;

      if (reader->flags & SCM_READER_FLAG_LOWER_CASE)
	c = tolower (c);
      else if (reader->flags & SCM_READER_FLAG_UPPER_CASE)
	c = toupper (c);

      if ((c >= 0) && (c < 256))
	tr = reader->eightbit_char_to_tr_map[c];
      else
	{
	  /* Currently, we don't support anything other than 8-bit
	     characters, so this code is useless.  */
	  for (tr = reader->token_readers;
	       tr->token.type != SCM_TOKEN_UNDEF;
	       tr++)
	    {
	      if (tr_handles_char (tr, c))
		break;
	    }

	  if (tr->token.type == SCM_TOKEN_UNDEF)
	    tr = NULL;
	}

      if (tr)
	{
	  assert (tr_handles_char (tr, c));
	  result = tr_invoke (tr, c, port, reader,
			      top_level_reader);
	  if ((result == SCM_UNSPECIFIED) && (!tr->escape))
	    goto doit;
	  else
	    {
	      if (reader->flags & SCM_READER_FLAG_POSITIONS)
		do_scm_set_source_position (result, line,
					    column, filename);

	      return result;
	    }
	}
      else
	{
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

SCM
scm_from_reader_spec (const scm_token_reader_spec_t *spec,
		      int caller_owned)
{
  SCM s_result = SCM_EOL;
  const scm_token_reader_spec_t *tr;

  for (tr = spec;
       tr->token.type != SCM_TOKEN_UNDEF;
       tr++)
    {
      SCM s_token_reader;

      s_token_reader = scm_from_token_reader (tr, caller_owned);
      s_result = scm_cons (s_token_reader, s_result);
    }

  return scm_reverse_x (s_result, SCM_EOL);
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

struct scm_reader_flag_entry;

const struct scm_reader_flag_entry *
_scm_to_make_reader_flag (const char *, unsigned int);

/* Include the automatically-generated perfect hash function.  */
#include "make-reader-flags.c"


unsigned
scm_to_make_reader_flags (SCM flags)
#define FUNC_NAME "scm_to_make_reader_flags"
{
  unsigned c_flags = 0;
  unsigned argnum = 1;

  SCM_VALIDATE_LIST (1, flags);

  for (; flags != SCM_EOL; flags = SCM_CDR (flags), argnum++)
    {
      SCM flag_name = SCM_CAR (flags);
      size_t flag_name_len;
      char *c_flag_name;
      const struct scm_reader_flag_entry *c_flag_entry;

      if (!scm_is_symbol (flag_name))
	scm_wrong_type_arg (FUNC_NAME, argnum, flag_name);

      flag_name = scm_symbol_to_string (flag_name);
      flag_name_len = scm_c_string_length (flag_name);
      c_flag_name = alloca (flag_name_len + 1);
      scm_to_locale_stringbuf (flag_name, c_flag_name, flag_name_len);
      c_flag_name[flag_name_len] = '\0';

      c_flag_entry = _scm_to_make_reader_flag (c_flag_name, flag_name_len);
      if ((c_flag_entry) && (c_flag_entry->flag))
	c_flags |= c_flag_entry->flag;
      else
	scm_misc_error (FUNC_NAME, "unknown `make-reader' flag: ~A",
			scm_list_1 (flag_name));
    }

  return c_flags;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_reader, "make-reader", 1, 1, 1,
	    (SCM token_readers, SCM fault_handler_proc, SCM flags),
	    "Create a reader.")
#define FUNC_NAME "make-reader"
{
  SCM s_reader, *s_deps;
  scm_reader_t reader;
  size_t code_size = 1024;
  void *code_buffer;
  size_t actual_size;
  unsigned token_reader_count, i, reader_flags;
  scm_token_reader_spec_t *c_specs;

  SCM_VALIDATE_LIST (1, token_readers);
  if ((fault_handler_proc == SCM_UNDEFINED)
      || (fault_handler_proc == SCM_BOOL_F))
    fault_handler_proc = scm_reader_standard_fault_handler_proc;
  else
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

  /* Add the fault handler proc as part of the SMOB's dependencies.  */
  s_deps[i++] = fault_handler_proc;

  /* Terminate the SMOB array with `#f'.  */
  s_deps[i] = SCM_BOOL_F;

  /* Go ahead with the reader compilation process.  */
  reader_flags = scm_to_make_reader_flags (flags);
  code_buffer = scm_malloc (code_size);

  do
    {
      reader = scm_c_make_reader (code_buffer, code_size,
				  c_specs, fault_handler_proc,
				  reader_flags,
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
	    "return it.  If @var{name} is does not name a standard token "
	    "reader, then an error is raised.")
#define FUNC_NAME "standard-token-reader"
{
  SCM s_token_reader;
  const scm_token_reader_spec_t *spec;

  SCM_VALIDATE_SYMBOL (1, name);

  spec = scm_token_reader_lookup (scm_i_symbol_chars (name));
  if (!spec)
    {
      scm_misc_error (FUNC_NAME, "not a standard token reader: ~A",
		      scm_list_1 (name));
      return SCM_BOOL_F;
    }

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

SCM_DEFINE (scm_token_reader_documentation,
	    "token-reader-documentation", 1, 0, 0,
	    (SCM tr),
	    "Return a string containing the Texinfo documentation of "
	    "token reader @var{tr} or @code{#f} if @code{tr} is "
	    "undocumented.")
#define FUNC_NAME s_scm_token_reader_documentation
{
  scm_token_reader_spec_t *c_tr;

  scm_assert_smob_type (scm_token_reader_type, tr);
  SCM_TOKEN_READER_SMOB_DATA (c_tr, tr);

  return (c_tr->documentation
	  ? scm_from_locale_string (c_tr->documentation)
	  : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_token_reader_handles_char_p, "token-reader-handles-char?",
	    2, 0, 0,
	    (SCM tr, SCM chr),
	    "Return true if @var{tr} handles character @var{chr}.")
#define FUNC_NAME s_scm_token_reader_handles_char_p
{
  char c_chr;
  scm_token_reader_spec_t *c_tr;

  scm_assert_smob_type (scm_token_reader_type, tr);
  SCM_VALIDATE_CHAR (2, chr);

  SCM_TOKEN_READER_SMOB_DATA (c_tr, tr);
  c_chr = (char)SCM_CHAR (chr);

  return scm_from_bool (tr_handles_char (c_tr, c_chr));
}
#undef FUNC_NAME

SCM_DEFINE (scm_guile_reader_uses_lightning,
	    "%guile-reader-uses-lightning?", 0, 0, 0,
	    (void),
	    "Return @code{#t} is guile-reader was compiled with "
	    "GNU Lightning support, @code{#f} otherwise.")
#define FUNC_NAME s_scm_guile_reader_uses_lightning
{
#ifdef SCM_READER_USE_LIGHTNING
  return SCM_BOOL_T;
#else
  return SCM_BOOL_F;
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_guile_reader_version_major,
	    "%guile-reader-version-major", 0, 0, 0,
	    (void),
	    "Return the version's major number for the version of "
	    "guile-reader in use.")
#define FUNC_NAME s_scm_guile_reader_version_major
{
  return SCM_I_MAKINUM (SCM_READER_VERSION_MAJOR);
}
#undef FUNC_NAME

SCM_DEFINE (scm_guile_reader_version_minor,
	    "%guile-reader-version-minor", 0, 0, 0,
	    (void),
	    "Return the version's minor number for the version of "
	    "guile-reader in use.")
#define FUNC_NAME s_scm_guile_reader_version_minor
{
  return SCM_I_MAKINUM (SCM_READER_VERSION_MINOR);
}
#undef FUNC_NAME


#if (defined SCM_READER_INLINE_GETC) && (defined DEBUG)
SCM_DEFINE (test_getc, "test-getc", 1, 0, 0,
	    (SCM port),
	    "Test our JIT implementation of `scm_getc ()'.")
#define FUNC_NAME s_test_getc
{
  static unsigned char buffer[2048];
  static int (* my_getc) (SCM) = NULL;
  scm_t_port *c_port;

  if (!my_getc)
    {
      static const char *msg_dbg = "test-getc: got char %i\n";

#define _jit lightning_state
      jit_state lightning_state;
      char *start, *end;
      int arg_port;

      my_getc = (int (*) (SCM))(jit_set_ip (buffer).iptr);
      start = (char *)my_getc;

      jit_prolog (1);
      arg_port = jit_arg_p ();
      jit_getarg_p (JIT_V0, arg_port);

      generate_getc (&lightning_state, start, sizeof (buffer));
      jit_movi_p (JIT_R0, msg_dbg);

      jit_prepare (2);
      jit_pusharg_p (JIT_V1);
      jit_pusharg_p (JIT_R0);
      (void)jit_finish (do_like_printf);

      (void)jit_movr_p (JIT_RET, JIT_V1);
      jit_ret ();

      end = jit_get_ip ().ptr;
      jit_flush_code (start, end);

      if (end - start > sizeof (buffer))
	abort ();
#undef _jit
    }

  c_port = SCM_PTAB_ENTRY (port);
  printf ("port info for %p: %s %s %s\n",
	  port,
	  (c_port->rw_active == SCM_PORT_WRITE)
	  ? "write"
	  : ((c_port->rw_active == SCM_PORT_READ)
	     ? "read"
	     : "active?"),
	  (c_port->rw_random ? "random" : "not-random"),
	  ((c_port->read_pos >= c_port->read_end)
	   ? "passed-the-end" : "before-end"));

  return scm_from_int (my_getc (port));
}
#undef FUNC_NAME
#endif



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
reader_apply (SCM reader, SCM port, SCM caller_handled,
	      SCM top_level_reader)
{
  scm_reader_t c_reader, c_top_level_reader;

  SCM_READER_SMOB_DATA (c_reader, reader);
  debug ("%s: applying reader %p\n", __FUNCTION__, c_reader);

  if (top_level_reader != SCM_UNDEFINED)
    {
      scm_assert_smob_type (scm_reader_type, top_level_reader);
      SCM_READER_SMOB_DATA (c_top_level_reader, top_level_reader);
    }
  else
    c_top_level_reader = c_reader;

  /* Type checking and optional argument definition are checked in either
     `scm_call_reader ()' or the compiled reader's code.  */

  return (scm_call_reader (c_reader, port,
			   (caller_handled == SCM_BOOL_T) ? 1 :0,
			   c_top_level_reader));
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
  /* XXX: Guile only supports up to 3 compulsory arguments for SMOB `apply'.
     So we have to forget about the TOP_LEVEL_READER argument: let's set it
     to READER, which should be good enough.  */

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

  return (c_tr_proc (c_chr, port, c_reader, c_reader /* top-level */));
}
#undef FUNC_NAME



/* The standard fault handler proc.  */
SCM scm_reader_standard_fault_handler_proc = SCM_BOOL_F;

static SCM
scm_reader_standard_fault_handler (SCM chr, SCM port, SCM reader)
{
  scm_i_input_error ("%reader-standard-fault-handler",
		     port, "unhandled character: ~S", scm_list_1 (chr));
  return SCM_UNSPECIFIED;
}



/* Initialization routine.  */
void
scm_reader_init_bindings (void)
{
  scm_reader_type = scm_make_smob_type ("reader", 0);
  scm_set_smob_mark (scm_reader_type, generic_reader_smob_mark);
  scm_set_smob_free (scm_reader_type, generic_reader_smob_free);
  scm_set_smob_apply (scm_reader_type, reader_apply, 0, 3, 0);

  scm_token_reader_type = scm_make_smob_type ("token-reader", 0);
  scm_set_smob_mark (scm_token_reader_type, generic_reader_smob_mark);
  scm_set_smob_free (scm_token_reader_type, generic_reader_smob_free);

  scm_token_reader_proc_type = scm_make_smob_type ("token-reader-proc", 0);
  scm_set_smob_mark (scm_token_reader_proc_type, token_reader_proc_mark);
  scm_set_smob_free (scm_token_reader_proc_type, token_reader_proc_free);
  scm_set_smob_apply (scm_token_reader_proc_type, token_reader_proc_apply,
		      3, 0, 0); /* XXX unfortunately, we are limited to 3
				   compulsory arguments...  */

  scm_reader_standard_fault_handler_proc =
    scm_permanent_object (scm_c_define_gsubr
			  ("%reader-standard-fault-handler", 3, 0, 0,
			   scm_reader_standard_fault_handler));

#include "reader.c.x"

  /* Initialize the other subsystems.  */
  scm_initialize_token_reader_library ();
  scm_initialize_reader_library ();

  /* Compile the standard reader.  */
  scm_load_standard_reader ();
}
