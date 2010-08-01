/******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************

 comp.c

 Compiling functions

 *****************************************************************************/

/************************************************************************
 *                                                                      *
 * fpl.library - A shared library interpreting script langauge.         *
 * Copyright (C) 1992-1995 FrexxWare                                    *
 * Author: Daniel Stenberg                                              *
 *                                                                      *
 * This program is free software; you may redistribute for non          *
 * commercial purposes only. Commercial programs must have a written    *
 * permission from the author to use FPL. FPL is *NOT* public domain!   *
 * Any provided source code is only for reference and for assurance     *
 * that users should be able to compile FPL on any operating system     *
 * he/she wants to use it in!                                           *
 *                                                                      *
 * You may not change, resource, patch files or in any way reverse      *
 * engineer anything in the FPL package.                                *
 *                                                                      *
 * This program is distributed in the hope that it will be useful,      *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 *
 *                                                                      *
 * Daniel Stenberg                                                      *
 * Ankdammsgatan 36, 4tr                                                *
 * S-171 43 Solna                                                       *
 * Sweden                                                               *
 *                                                                      *
 * FidoNet 2:201/328    email:dast@sth.frontec.se                       *
 *                                                                      *
 ************************************************************************/

#ifdef AMIGA
#include <exec/types.h>
#include <proto/exec.h>
#include <libraries/dos.h>
#include <proto/dos.h>

#include <exec/libraries.h>
#include <dos.h>

#elif defined(UNIX)
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#endif

#include <stdio.h>
#include <string.h>
#include "script.h"

struct Pass1Data current_data;

struct Pass1Data first_data;
struct Pass1Data *pass1_list;

static int put_count=0;

#ifdef DEBUG_OUTPUT

static void Describe(struct Data *scr, Pass1 code, long count)
{
  static char *s[]={
/*
    "{{{{{{{{{",
    "}}}}}}}}}",
    "<main start>",
    "<main end>",
    "case",
    "default",
    "return",
    "exit",
    "switch",
    "if",
    "while",
    "else",
    "break",
    "continue",
    "do",
    "for",
    "resize",
    "<declaration>",
    "<parameter list>",
    "<assign>",
    "<local symbol access>",
    "<global symbol access>",
    "<export symbol access>",
    "[[[[[[[[[",
    "]]]]]]]]]",
    "<post++>",
    "<post-->",
    "<++pre>",
    "<--pre>",
    "<contents of>",
    "<numercial constant>",
    "<string constant>",
    "(((((((((",
    ")))))))))",
    "<~>",
    "<!>",
    "<negate>",

    "<,>",
    "<!=>",
    "<'>'>",
    "<'>>'>",
    "<'>='>",
    "<'<'>",
    "<'<<'>",
    "<'<='>",
    "<^>",
    "<%>",
    "</>",
    "<*>",
    "<:>",
    "<?>",
    "<->",
    "<+>",
    "<|>",
    "<&>",
    "<||>",
    "<&&>",
    "==",

    "<function call>",
    "<internal function call>",
    "<do-while condition>",
    "<function define>",
    "<no more declaration>",
    "<variable reference '&'>",
    "<start of expr>",
    "<end of expr>",
    "<string append>",

    "<type of argument>",

    "<# of variables>",
    "<end of function>",
*/
  "COMP_OPEN_BRACE",
  "COMP_CLOSE_BRACE",
  "COMP_MAIN_START",
  "COMP_MAIN_END",
  "COMP_CASE",
  "COMP_DEFAULT",
  "COMP_RETURN",
  "COMP_EXIT",
  "COMP_SWITCH",
  "COMP_IF",
  "COMP_WHILE",
  "COMP_ELSE",
  "COMP_BREAK",
  "COMP_CONTINUE",
  "COMP_DO",
  "COMP_FOR",
  "COMP_RESIZE",
  "COMP_DECLARE",
  "COMP_PARAM_LIST", /* string follows */
  "COMP_ASSIGN", /* assign type follows */
  "COMP_REF_LOCAL_SYMBOL", /* refers to a local symbol", number follows! */
  "COMP_REF_GLOBAL_SYMBOL", /* refers to a global symbol", number follows! */
  "COMP_REF_EXPORT_SYMBOL", /* refers to a export symbol", hash and name
                             follows */
  "COMP_OPEN_BRACKET",  /* [ */
  "COMP_CLOSE_BRACKET", /* ] */

  "COMP_POSTINC",
  "COMP_POSTDEC",
  "COMP_PREINC",
  "COMP_PREDEC",
  "COMP_CONTENTSOF",
  "COMP_NUM_CONSTANT",
  "COMP_STRING_CONSTANT",
  "COMP_OPEN_PAREN",
  "COMP_CLOSE_PAREN",
  "COMP_ONCECOMPLEMENT",
  "COMP_NOTOPERATOR",
  "COMP_NEGATE",

  "COMP_COMMA",
  "COMP_NOTEQUAL",
  "COMP_GREATER",
  "COMP_SHIFTRIGHT",
  "COMP_GREATEQ",
  "COMP_LESS",
  "COMP_SHIFTLEFT",
  "COMP_LESSEQ",
  "COMP_XOR",
  "COMP_REMAIN",
  "COMP_DIVISION",
  "COMP_MULTIPLY",
  "COMP_CONDOPEND",
  "COMP_CONDOPSTART",
  "COMP_MINUS",
  "COMP_PLUS",
  "COMP_BINARYOR",
  "COMP_BINARYAND",
  "COMP_LOGICOR",
  "COMP_LOGICAND",
  "COMP_EQUAL",

  "COMP_CALL_FUNCTION",
  "COMP_CALL_INTERNAL_FUNCTION",

  "COMP_DO_CONDITION",     /* the condition followin in a do-while expression */
  "COMP_FUNCTION_DECLARE", /* function define coming up! */
  "COMP_END_OF_DECLARE",   /* end of allowed declaration phase */
  "COMP_VARIABLE_REFERENCE", /* a standard '&' in front of a variable name! */
  "COMP_START_OF_EXPR", /* start of expression */
  "COMP_END_OF_EXPR", /* end of expression */
  "COMP_APPEND_STRING", /* '+' in a string expression */

  "COMP_TYPE_OF_ARGUMENTS",

  "COMP_AMOUNT_VARIABLES", /* the amount of variables declared within the
							just passed scope */
  "COMP_END_OF_FUNCTION",
                                                        
  "COMP_LAST"     /* Last", does NOTHING */

  };
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS1) {
    if(code-1>sizeof(s)/sizeof(char *))
      puts("UNKNOWN");
    else
      printf("%s (%d)(no %d)\n", s[code-1], code, count);
  }
}

static void DescribeNum(struct Data *scr, long code)
{
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS1)
    printf("  num: [%08x]\n", code);
}

static void DescribeStr(struct Data *scr, char *str, long len)
{
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS1) {
    printf("  string: [%.*s]\n", len, str);
  }
}
#endif

ReturnCode PutOpen(struct Data *scr, char *file)
{
  memset(&current_data, 0, sizeof(struct Pass1Data));
  memset(&first_data, 0, sizeof(struct Pass1Data));
  current_data.code=COMP_NOTHING;
  pass1_list=&first_data;
  put_count=0;
  return FPL_OK;
}

ReturnCode PushPut(struct Data *scr)
{
  GETMEMA(pass1_list->next, sizeof(struct Pass1Data));
  memcpy(pass1_list->next, &current_data, sizeof(struct Pass1Data));;
  pass1_list=pass1_list->next;
  pass1_list->next=NULL;
  current_data.flags=0;
  return FPL_OK;
}

void PutClose(struct Data *scr)
{
  if (current_data.code!=COMP_NOTHING)
    PushPut(scr);
}

void Pass1Free(struct Data *scr)  /* Free all memory from pass1 */
{
  struct Pass1Data *count=first_data.next;
  struct Pass1Data *prev;
  while (count->next) {
    prev=count;
    count=count->next;
    FREE(prev);
  }
  first_data.next=NULL;
}


ReturnCode Put(struct Data *scr, Pass1 code)
{
  short temp=code;
  ReturnCode ret;
  put_count++;

  if (current_data.code!=COMP_NOTHING) {
    CALL(PushPut(scr));
  }
  current_data.counter=put_count;
  current_data.code=code;
  current_data.row= scr->text-scr->program;
  current_data.line=scr->virprg;
  current_data.next=NULL;
  current_data.flags=0;
#ifdef DEBUG_OUTPUT
  Describe(scr, code, put_count);
#endif
  return FPL_OK;
}

ReturnCode PutArg(struct Data *scr, Pass1 code, long arg)
{
  ReturnCode ret;
  if(code != COMP_NOTHING)
    CALL(Put(scr, code));
  if (current_data.flags&P1D_FLAG_NUM0) {
    current_data.num[1]=arg;
    current_data.flags|=P1D_FLAG_NUM1;
  } else {
    current_data.num[0]=arg;
    current_data.flags|=P1D_FLAG_NUM0;
  }
#ifdef DEBUG_OUTPUT
  DescribeNum(scr, arg);
#endif
  return FPL_OK;
}

ReturnCode PutString(struct Data *scr, Pass1 code, char *arg, long len)
{
  ReturnCode ret;
  if(!arg)
    arg="";
  if (len<0)
    len=strlen(arg);
  if(code != COMP_NOTHING) {
    CALL(Put(scr, code));
  }
  STRDUP(current_data.string, arg);
  current_data.strlen=len;
  current_data.flags|=P1D_FLAG_STRING;
#ifdef DEBUG_OUTPUT
  DescribeStr(scr, arg, len);
#endif
  return FPL_OK;
}

