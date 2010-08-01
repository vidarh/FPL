/************************************************************************
 *			 FREXX PROGRAMMING LANGUAGE			      *
 ************************************************************************

 Pass2.c

 Compiling functions

 ************************************************************************/

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
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include "script.h"

#include "optimize.h"
#include "pass2.h"


extern struct Pass1Data first_data;

struct Pass2Data pass2_first_data;
struct Pass2Data pass2_current_data;

struct Pass2Data main_declare_link_first, function_declare_link_first, function_contain_link_first;
struct Pass2Data *main_declare_link=&main_declare_link_first;
struct Pass2Data *function_declare_link=&function_declare_link_first;
struct Pass2Data *function_contain_link=&function_contain_link_first;
struct Pass2Data *function_start;

static ReturnCode HandleReference(struct Data *scr, struct Pass1Data **p1datapek, struct Pass2Data **pass2_list);
static ReturnCode HandleFunctionCall(struct Data *scr, struct Pass1Data **p1datapek, struct Pass2Data **pass2_list);
static ReturnCode ExamineLevel(struct Data *scr, struct Pass1Data **p1datapek, struct Pass2Data **pass2_list);

long level=0;
long mainlevel=0;
long labelcount=0;
long total_number_of_variables=0;
long total_number_of_functions=0;
long max_level=0;
long main_label_number;

#ifdef DEBUG_OUTPUT
static void Describe(struct Data *scr, struct Pass2Data *data)
{

  char *s[]={
  "NOTHING",
  "DECLARE",

  "REF_LOCAL_SYMBOL",
  "REF_GLOBAL_SYMBOL",
  "REF_EXPORT_SYMBOL",

  "POSTINC",
  "POSTDEC",
  "PREINC",
  "PREDEC",
  "CONTENTSOF",
  "NUM_CONSTANT",
  "STRING_CONSTANT",
  "OPEN_PAREN",
  "CLOSE_PAREN",
  "ONCECOMPLEMENT",
  "NOTOPERATOR",
  "NEGATE",

  "COMMA",
  "NOTEQUAL",
  "GREATER",
  "SHIFTRIGHT",
  "GREATEQ",
  "LESS",
  "SHIFTLEFT",
  "LESSEQ",
  "XOR",
  "REMAIN",
  "DIVISION",
  "MULTIPLY",
  "CONDOPEND",
  "CONDOPSTART",
  "MINUS",
  "PLUS",
  "BINARYOR",
  "BINARYAND",
  "LOGICOR",
  "LOGICAND",
  "EQUAL",
  "STRING_APPEND",

  "END_OF_EXPR",

  "ASSIGN_LOCAL_SYMBOL",  /* [var num] [assign type] */
  "ASSIGN_GLOBAL_SYMBOL",  /* [var num] [assign type] */
  "ASSIGN_EXPORT_SYMBOL", /* [hash num] [assign type] [name] */

  "LABEL_GOTO",	/* [number] */

  "IFNOT_BRANCH",	/* if branch */
  "IF_BRANCH",	/* if branch */

  "CALL_LOCAL_FUNCTION",
  "CALL_EXPORT_FUNCTION",
  "CALL_INTERNAL_FUNCTION",

  "SWITCH",
  "CASE",	/* [label number] */

  "BREAK_EXPR", /* Followed by an expression and several GOTO_BREAK and END_OF_EXPR */
  "RETURN",

  "RESIZE",
  "OPEN_BRACKET",
  "CLOSE_BRACKET",

  "ASSIGN_ARGUMENT", /* [var number], [arg number] */

  "EXPORT_FUNCTION", /* [type], [number], [offset] */

  "RESET_VARIABLE", /* [number] */
  "OPEN_BRACE",
  "CLOSE_BRACE",

  "EXIT", /* expression */
  "VARIABLE_REFERENCE",

  "TYPE_OF_ARGUMENTS", /* [string] */


  "JUMP_OVER_ELSE",	/* jump over olse */

  "MAIN_START",
  "MAIN_END",

  "IFNOT_BRANCH_BREAK",/* if not branch to break */
  "IFNOT_BRANCH_ELSE",/* if not branch to else */
  "LOCAL_FUNCTION", /* [type], [number], [offset] */
  "LABEL",
  "LABEL_BEGIN",	/* [number] */
  "LABEL_CONTINUE",	/* [level] */
  "LABEL_BREAK",	/* [level] */
  "GOTO_BEGIN",	/* [number] */
  "GOTO_BREAK",	/* [level] */
  "LABEL_ELSE",		/* else [number]*/
  "END_OF_ELSE",	/* else [number]*/

  "FUNCTION_START", /* [function/var number] */
  "CONTINUE", /* level */
  "AMOUNT_VARIABLES",
  "END_OF_FUNCTION",

  "LINE_NUMBER",

  };
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS2) {
    if(data->code>sizeof(s)/sizeof(char *))
      printf("UNKNOWN\n");
    else {
      printf("%1d %2d %-20s", data->final_size, data->level, s[data->code]);
      if (data->flags&P2D_FLAG_NUM0)
        printf(" [%08x]", data->num[0]);
      if (data->flags&P2D_FLAG_NUM1)
        printf(" [%08x]", data->num[1]);
      if (data->flags&P2D_FLAG_NUM2)
        printf(" [%08x]", data->num[2]);
      if (data->flags&P2D_FLAG_STRING) {
        printf(" [%.*s] (%d)", data->strlen[0], data->string[0], data->strlen[0]);
      }
      if (data->flags&P2D_FLAG_STRING2) {
        printf(" [%.*s] (%d)", data->strlen[1], data->string[1], data->strlen[1]);
      }
      printf("\n");
    }
  }
}
#endif

ReturnCode Pass2PutOpen(struct Data *scr, char *file)
{
#ifdef DEBUG_OUTPUT
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS2)
    printf("\n-------------------------------\nPASS2 START\n-------------------------------\n");
#endif
  return FPL_OK;
}

ReturnCode Pass2PushPut(struct Data *scr, struct Pass2Data **link)
{
  ReturnCode ret;
  struct Pass2Data *new;
  if (pass2_current_data.code!=PASS2_NOTHING) {
    long size=sizeof(struct Pass2Data);
    if (!pass2_current_data.flags)
      size=offsetof(struct Pass2Data, num);  /* OBS: The structure is allocated shorter now */
    GETMEM(new, size);
    memcpy(new, &pass2_current_data, size);
    if (*link) {
      new->next=(*link)->next;
      (*link)->next=new;
    } else
      (*link)=new;
#ifdef DEBUG_OUTPUT
    Describe(scr, &pass2_current_data);
#endif
    pass2_current_data.flags=0;
    pass2_current_data.code=PASS2_NOTHING;
    *link=new;
  }
  return FPL_OK;
}

ReturnCode Pass2Link(struct Data *scr, struct Pass2Data **link, struct Pass2Data *new)
{
  ReturnCode ret;
  struct Pass2Data *last=NULL;
  if (*link)
    last=(*link)->next;
  if (new) {
    if (*link) {
      (*link)->next=new;
      while (new->next)
        new=new->next;
      new->next=last;
    }
    *link=new;
  }
  return FPL_OK;
}

  void Pass2PutClose(struct Data *scr)
{
  struct Pass2Data *count;
  count=&pass2_first_data;
#ifdef DEBUG_OUTPUT
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS2) {
    printf("\nRESULT\n");
    while (count=count->next) {
      Describe(scr, count);
    }
  }
#endif
}


  ReturnCode Pass2Put(struct Data *scr, Pass2 code, struct Pass1Data *pass1_reference, struct Pass2Data **pass2_list, char size)
{
  ReturnCode ret;

  if (pass1_reference) {
    pass2_current_data.line=pass1_reference->line;
    pass2_current_data.row=pass1_reference->row;
  }
  pass2_current_data.code=code;
  pass2_current_data.level=level;
  pass2_current_data.final_size=size;
  if (pass2_current_data.code!=PASS2_NOTHING) {
    CALL(Pass2PushPut(scr, pass2_list));
  }
  pass2_current_data.code=PASS2_NOTHING;
  pass2_current_data.level=0;
  pass2_current_data.line=0;
  pass2_current_data.row=0;
  pass2_current_data.next=NULL;
  return FPL_OK;
}

  ReturnCode Pass2PutArg(struct Data *scr, long arg)
{
  ReturnCode ret;
  if (!(pass2_current_data.flags&P2D_FLAG_NUM0)) {
    pass2_current_data.num[0]=arg;
    pass2_current_data.flags|=P2D_FLAG_NUM0;
  } else if (!(pass2_current_data.flags&P2D_FLAG_NUM1)) {
    pass2_current_data.num[1]=arg;
    pass2_current_data.flags|=P2D_FLAG_NUM1;
  } else if (!(pass2_current_data.flags&P2D_FLAG_NUM2)) {
    pass2_current_data.num[2]=arg;
    pass2_current_data.flags|=P2D_FLAG_NUM2;
  }
  return FPL_OK;
}

  ReturnCode Pass2PutString(struct Data *scr, char *arg, long len)
{
  ReturnCode ret;
  short num=0;
  if (pass2_current_data.flags&P2D_FLAG_STRING) {
    num=1;
    pass2_current_data.flags|=P2D_FLAG_STRING2;
  }
  pass2_current_data.string[num]=arg;
  pass2_current_data.strlen[num]=len;
  pass2_current_data.flags|=P2D_FLAG_STRING;
  return FPL_OK;
}

BOOL GetOper(void *user, struct Oper *oper, char index)
{
  return TRUE;
}
void PutOper(void *user, struct Oper *oper, char step)
{
}

  static ReturnCode ParseExpression(struct Data *scr, struct Pass1Data **p1datapek,
                                    struct Pass2Data **pass2_list)
{
  ReturnCode ret;
  struct Pass1Data *p1data=*p1datapek;
  char stop=FALSE;
  char loop;
  long logic_or_label=labelcount++, logic_and_label=0;
  if (p1data->code==COMP_END_OF_EXPR) {
    stop=TRUE;
    p1data=p1data->next;				/* eat me */
  }
  while (!stop) {
    do {
      loop=FALSE;
      switch(p1data->code) {
      case COMP_REF_LOCAL_SYMBOL: /* refers to a local symbol, number follows! */
      case COMP_REF_GLOBAL_SYMBOL: /* refers to a local symbol, number follows! */
      case COMP_REF_EXPORT_SYMBOL: /* refers to a export symbol, hash and name */
        CALL(HandleReference(scr, &p1data, pass2_list));
        break;
      case COMP_CALL_FUNCTION:
      case COMP_CALL_INTERNAL_FUNCTION:
        CALL(HandleFunctionCall(scr, &p1data, pass2_list));
        break;
      case COMP_OPEN_BRACE:	/* Only for assigns of arrays */
        p1data=p1data->next;				/* eat open brace */
        CALL(Pass2Put(scr, PASS2_OPEN_BRACE, p1data, pass2_list, 1));
        CALL(ParseExpression(scr, &p1data, pass2_list));
        p1data=p1data->next;				/* eat close brace */
        CALL(Pass2Put(scr, PASS2_CLOSE_BRACE, p1data, pass2_list, 1));
        break;
      case COMP_START_OF_EXPR:	/* Only for assigns of arrays */
        p1data=p1data->next;				/* eat start of expr */
        CALL(ParseExpression(scr, &p1data, pass2_list));
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));
        break;
      case COMP_OPEN_PAREN:
        CALL(Pass2Put(scr, PASS2_OPEN_PAREN, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat open paren */
        CALL(ParseExpression(scr, &p1data, pass2_list));
/*        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1)); 960306 */
        break;
      case COMP_NUM_CONSTANT:
        CALL(Pass2PutArg(scr, p1data->num[0]));
        CALL(Pass2Put(scr, PASS2_NUM_CONSTANT, p1data, pass2_list, 3));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_STRING_CONSTANT:
        CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
        CALL(Pass2Put(scr, PASS2_STRING_CONSTANT, p1data, pass2_list, 3));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_CLOSE_BRACKET: /* ] */
        stop=TRUE;
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_VARIABLE_REFERENCE:
        CALL(Pass2Put(scr, PASS2_VARIABLE_REFERENCE, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      case COMP_PREINC:
        CALL(Pass2Put(scr, PASS2_PREINC, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      case COMP_PREDEC:
        CALL(Pass2Put(scr, PASS2_PREDEC, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      case COMP_CONTENTSOF:
        CALL(Pass2Put(scr, PASS2_CONTENTSOF, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      case COMP_ONCECOMPLEMENT:
        CALL(Pass2Put(scr, PASS2_ONCECOMPLEMENT, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      case COMP_NOTOPERATOR:
        CALL(Pass2Put(scr, PASS2_NOTOPERATOR, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      case COMP_NEGATE:
        CALL(Pass2Put(scr, PASS2_NEGATE, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        loop=TRUE;
        break;
      }
    } while(loop);
    if (!stop) {
      switch(p1data->code) {
      case COMP_END_OF_EXPR:
        stop=TRUE;
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_COMMA:
        CALL(Pass2Put(scr, PASS2_COMMA, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
  
      case COMP_NOTEQUAL:
        CALL(Pass2Put(scr, PASS2_NOTEQUAL, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_GREATER:
        CALL(Pass2Put(scr, PASS2_GREATER, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_SHIFTRIGHT:
        CALL(Pass2Put(scr, PASS2_SHIFTRIGHT, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_GREATEQ:
        CALL(Pass2Put(scr, PASS2_GREATEQ, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_LESS:
        CALL(Pass2Put(scr, PASS2_LESS, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_SHIFTLEFT:
        CALL(Pass2Put(scr, PASS2_SHIFTLEFT, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_LESSEQ:
        CALL(Pass2Put(scr, PASS2_LESSEQ, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_XOR:
        CALL(Pass2Put(scr, PASS2_XOR, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_REMAIN:
        CALL(Pass2Put(scr, PASS2_REMAIN, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_DIVISION:
        CALL(Pass2Put(scr, PASS2_DIVISION, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_MULTIPLY:
        CALL(Pass2Put(scr, PASS2_MULTIPLY, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_MINUS:
        CALL(Pass2Put(scr, PASS2_MINUS, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_PLUS:
        CALL(Pass2Put(scr, PASS2_PLUS, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_BINARYOR:
        CALL(Pass2Put(scr, PASS2_BINARYOR, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_BINARYAND:
        CALL(Pass2Put(scr, PASS2_BINARYAND, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_EQUAL:
        CALL(Pass2Put(scr, PASS2_EQUAL, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_LOGICOR:
        CALL(Pass2PutArg(scr, logic_or_label));
        CALL(Pass2Put(scr, PASS2_LOGICOR, p1data, pass2_list, 3));
        if (logic_and_label) {
          CALL(Pass2PutArg(scr, logic_and_label));
          CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
          logic_and_label=0;
        }
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_LOGICAND:
        if (!logic_and_label) {
          logic_and_label=labelcount;
          labelcount++;
        }
        CALL(Pass2PutArg(scr, logic_and_label));
        CALL(Pass2Put(scr, PASS2_LOGICAND, p1data, pass2_list, 3));
        p1data=p1data->next;				/* eat me */
        break;
  
      case COMP_APPEND_STRING:
        CALL(Pass2Put(scr, PASS2_STRING_APPEND, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
  
      case COMP_CONDOPSTART:
        {
          long label=labelcount;
          long label2;
          labelcount++;
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2Put(scr, PASS2_CONDOPSTART, p1data, pass2_list, 3));
          p1data=p1data->next;				/* eat me */
          p1data=p1data->next;				/* eat start of expr */
          CALL(ParseExpression(scr, &p1data, pass2_list));
          label2=labelcount;
          labelcount++;
          CALL(Pass2PutArg(scr, label2));
          CALL(Pass2Put(scr, PASS2_LABEL_GOTO, p1data, pass2_list, 3));
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
          p1data=p1data->next;				/* eat condopend */
          p1data=p1data->next;				/* eat start of expr */
          CALL(ParseExpression(scr, &p1data, pass2_list));
          CALL(Pass2PutArg(scr, label2));
          CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
        }
        break;

      case COMP_CLOSE_PAREN:
        CALL(Pass2Put(scr, PASS2_CLOSE_PAREN, p1data, pass2_list, 1));
        stop=TRUE;
        p1data=p1data->next;				/* eat me */
        break;

      case COMP_CLOSE_BRACKET: /* ] */
        stop=TRUE;
        p1data=p1data->next;				/* eat me */
        break;

      default:
#ifdef DEBUG_OUTPUT
        if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS2)
          printf("UNKNOWN EXPRESSION CODE (%d)(no %d)\n", p1data->code, p1data->counter);
#endif
        stop=TRUE;
        break;
      }
    }
  }
  if (logic_and_label) {
    CALL(Pass2PutArg(scr, logic_and_label));
    CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
  }
  if (logic_or_label) {
    CALL(Pass2PutArg(scr, logic_or_label));
    CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
  }
  *p1datapek=p1data;
  return FPL_OK;
}

static ReturnCode HandleFunctionCall(struct Data *scr, struct Pass1Data **p1datapek,
                                     struct Pass2Data **pass2_list)
{
  ReturnCode ret;
  struct Pass1Data *p1data=*p1datapek;
  struct Pass2Data *p2_stack;

  switch (p1data->code) {
  case COMP_CALL_INTERNAL_FUNCTION:
    CALL(Pass2PutArg(scr, p1data->num[0]));
    CALL(Pass2Put(scr, PASS2_CALL_INTERNAL_FUNCTION, p1data, pass2_list, 3));
    break;
  case COMP_CALL_FUNCTION:
    CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
    CALL(Pass2PutArg(scr, p1data->num[0]));
    CALL(Pass2Put(scr, PASS2_CALL_EXPORT_FUNCTION, p1data, pass2_list, 3));
    break;
  }
  p2_stack=*pass2_list;

  p1data=p1data->next;					/* eat call */
  p1data=p1data->next;					/* eat open paren */
  CALL(ParseExpression(scr, &p1data, pass2_list));

  if (p1data->code==COMP_TYPE_OF_ARGUMENTS) {
    CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
    CALL(Pass2Put(scr, PASS2_TYPE_OF_ARGUMENTS, p1data, &p2_stack, 3));
    p1data=p1data->next;  /* eat type of argument */
  }

  *p1datapek=p1data;
  return FPL_OK;
}

static ReturnCode HandleReference(struct Data *scr, struct Pass1Data **p1datapek,
                                  struct Pass2Data **pass2_list)
{
  ReturnCode ret;
  struct Pass1Data *p1data=*p1datapek;
  struct Pass2Data bracket_link_first;
  struct Pass2Data *bracket_link=&bracket_link_first;
  struct Pass1Data *datain=p1data;
  char stop;

  bracket_link_first.next=NULL;
  p1data=p1data->next;			/* eat reference */

  do {
    stop=TRUE;
    switch(p1data->code) {
    case COMP_ASSIGN:
      {
        CALL(Pass2PutArg(scr, datain->num[0]));	/* number */
        CALL(Pass2PutArg(scr, p1data->num[0]));	/* assign type */
        if (datain->code==COMP_REF_LOCAL_SYMBOL) {
          CALL(Pass2Put(scr, PASS2_ASSIGN_LOCAL_SYMBOL, datain, pass2_list, 5));
        } else {
          if (datain->code==COMP_REF_GLOBAL_SYMBOL) {
            CALL(Pass2Put(scr, PASS2_ASSIGN_GLOBAL_SYMBOL, datain, pass2_list, 5));
          } else {
            CALL(Pass2PutString(scr, datain->string, datain->strlen));
            CALL(Pass2Put(scr, PASS2_ASSIGN_EXPORT_SYMBOL, datain, pass2_list, 5));
          }
        }
        if (bracket_link_first.next) {
          CALL(Pass2Link(scr, pass2_list, bracket_link_first.next));
          bracket_link_first.next=NULL;
        }
        p1data=p1data->next;			/* eat assign */
        if (p1data->code==COMP_START_OF_EXPR)
          p1data=p1data->next;			/* eat begin of expression */
  
        CALL(ParseExpression(scr, &p1data, pass2_list));
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));
      }
      break;
    case COMP_OPEN_BRACKET:
      do {
        p1data=p1data->next; 				/* eat bracket */
        CALL(Pass2Put(scr, PASS2_OPEN_BRACKET, p1data, &bracket_link, 1));
        CALL(ParseExpression(scr, &p1data, &bracket_link));
        CALL(Pass2Put(scr, PASS2_CLOSE_BRACKET, p1data, &bracket_link, 1));
      } while (p1data->code==COMP_OPEN_BRACKET);
      stop=FALSE;
      break;
    default:
      CALL(Pass2PutArg(scr, datain->num[0]));	/* put hash or local number */
      if (datain->code==COMP_REF_LOCAL_SYMBOL) {
        CALL(Pass2Put(scr, PASS2_REF_LOCAL_SYMBOL, datain, pass2_list, 3));
      } else {
        if (datain->code==COMP_REF_GLOBAL_SYMBOL) {
          CALL(Pass2Put(scr, PASS2_REF_GLOBAL_SYMBOL, datain, pass2_list, 3));
        } else {
          CALL(Pass2PutString(scr, p1data->string, datain->strlen));
          CALL(Pass2Put(scr, PASS2_REF_EXPORT_SYMBOL, datain, pass2_list, 3));
        }
      }
      if (bracket_link_first.next) {
        CALL(Pass2Link(scr, pass2_list, bracket_link_first.next));
        bracket_link_first.next=NULL;
      }
      switch(p1data->code) {
      case COMP_POSTINC:
        CALL(Pass2Put(scr, PASS2_POSTINC, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      case COMP_POSTDEC:
        CALL(Pass2Put(scr, PASS2_POSTDEC, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat me */
        break;
      }
      break;
    }
  } while (!stop);
  *p1datapek=p1data;
  return FPL_OK;
}



static ReturnCode ExamineLevel(struct Data *scr, struct Pass1Data **p1datapek,
                               struct Pass2Data **pass2_list)
{
  char stop=FALSE;
  ReturnCode ret;
  struct Pass1Data *p1data=*p1datapek;
  struct Pass2Data case_link_first, default_link_first;
  struct Pass2Data *case_link=&case_link_first;
  struct Pass2Data *default_link=&default_link_first;
  struct Pass2Data *initial_link=*pass2_list;

  default_link_first.next=NULL;
  case_link_first.next=NULL;

  if (level>max_level)
    max_level=level;

  do {
    switch(p1data->code) {
    case COMP_DECLARE:
      {
        struct Pass2Data *declare_link=&function_declare_link_first;
        struct Pass1Data *ingoingdata=p1data;
        long var_number;
        if (mainlevel==0)
          declare_link=&main_declare_link_first;
        CALL(Pass2PutArg(scr, p1data->num[0]));
        CALL(Pass2PutArg(scr, p1data->num[1]));
        var_number=p1data->num[1];
        if (!(p1data->num[0]&FPL_EXPORT_SYMBOL)) {
          if (total_number_of_variables<var_number)
             total_number_of_variables=var_number;
	}
        CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
        while (declare_link->next && declare_link->next->num[0]<p1data->num[0])
          declare_link=declare_link->next;
        CALL(Pass2Put(scr, PASS2_DECLARE, p1data, &declare_link, 0));
        switch(p1data->next->code) {
        case COMP_ASSIGN:
          CALL(Pass2PutArg(scr, p1data->num[1]));
          CALL(Pass2PutArg(scr, p1data->next->num[0])); /* assign type */
          if (p1data->num[0]&FPL_EXPORT_SYMBOL) {
            CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
            CALL(Pass2Put(scr, PASS2_ASSIGN_EXPORT_SYMBOL, p1data, pass2_list, 5));
          } else {
            if (p1data->num[0]&FPL_GLOBAL_SYMBOL) {
              CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
              CALL(Pass2Put(scr, PASS2_ASSIGN_GLOBAL_SYMBOL, p1data, pass2_list, 5));
            } else {
              CALL(Pass2Put(scr, PASS2_ASSIGN_LOCAL_SYMBOL, p1data, pass2_list, 5));
            }
          }
          p1data=p1data->next;				/* eat declare */
          p1data=p1data->next; 				/* eat assign */
          p1data=p1data->next; 				/* eat begin of expression */
          CALL(ParseExpression(scr, &p1data, pass2_list));
          CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));
          break;
        case COMP_OPEN_BRACKET:  /* [ */
          CALL(Pass2PutArg(scr, p1data->num[1]));
          if (p1data->num[0]&FPL_EXPORT_SYMBOL) {
            CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
            CALL(Pass2Put(scr, PASS2_REF_EXPORT_SYMBOL, p1data, pass2_list, 3));
          } else {
            if (p1data->num[0]&FPL_GLOBAL_SYMBOL) {
              CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
              CALL(Pass2Put(scr, PASS2_REF_GLOBAL_SYMBOL, p1data, pass2_list, 3));
            } else {
              CALL(Pass2Put(scr, PASS2_REF_LOCAL_SYMBOL, p1data, pass2_list, 3));
            }
          }
          p1data=p1data->next;				/* eat declare */
          CALL(Pass2Put(scr, PASS2_RESIZE, p1data, pass2_list, 1));
          do {
            p1data=p1data->next; 				/* eat bracket */
            CALL(Pass2Put(scr, PASS2_OPEN_BRACKET, p1data, pass2_list, 1));
            CALL(ParseExpression(scr, &p1data, pass2_list));
            CALL(Pass2Put(scr, PASS2_CLOSE_BRACKET, p1data, pass2_list, 1));
          } while (p1data->code==COMP_OPEN_BRACKET);
          if (p1data->code==COMP_ASSIGN) {
            CALL(Pass2PutArg(scr, ingoingdata->num[1]));
            CALL(Pass2PutArg(scr, p1data->num[0])); /* assign type */
            if (ingoingdata->num[0]&FPL_EXPORT_SYMBOL) {
              CALL(Pass2PutString(scr, ingoingdata->string, ingoingdata->strlen));
              CALL(Pass2Put(scr, PASS2_ASSIGN_EXPORT_SYMBOL, ingoingdata, pass2_list, 5));
            } else {
              if (ingoingdata->num[0]&FPL_GLOBAL_SYMBOL) {
                CALL(Pass2Put(scr, PASS2_ASSIGN_GLOBAL_SYMBOL, ingoingdata, pass2_list, 5));
              } else {
                CALL(Pass2Put(scr, PASS2_ASSIGN_LOCAL_SYMBOL, ingoingdata, pass2_list, 5));
              }
            }
            p1data=p1data->next; 				/* eat assign */
            CALL(ParseExpression(scr, &p1data, pass2_list));
            CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));
          }
          break;
        default:
          {
            long type=p1data->num[0];
            p1data=p1data->next;				/* eat declare */
            if (!(type&(FPL_EXPORT_SYMBOL|FPL_GLOBAL_SYMBOL))) {
              CALL(Pass2PutArg(scr, var_number));
              CALL(Pass2Put(scr, PASS2_RESET_VARIABLE, p1data, pass2_list, 3));
            }
          }
        }
      }
      break;
    case COMP_END_OF_DECLARE:   /* end of allowed declaration phase */
      p1data=p1data->next;	/* eat END OF DECLARE */
      break;
    case COMP_FUNCTION_DECLARE:
      {
        struct Pass2Data *declare_link=&main_declare_link_first;
        long argument_count=0;

        while (declare_link->next && declare_link->next->num[0]<p1data->num[0])
          declare_link=declare_link->next;
        if (p1data->num[0]&FPL_EXPORT_SYMBOL) {
          long label=labelcount;
          labelcount++;
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2Put(scr, PASS2_LABEL, p1data, &function_contain_link, 0));

          CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
          CALL(Pass2PutArg(scr, p1data->num[0]));
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2PutArg(scr, p1data->num[1]));
          p1data=p1data->next;	/* eat function declare */
          CALL(Pass2PutString(scr, p1data->string, p1data->strlen)); /* put param list */
          CALL(Pass2Put(scr, PASS2_EXPORT_FUNCTION, p1data, &declare_link, 9));
          p1data=p1data->next;	/* eat parameter list */
        } else {
          if (total_number_of_functions<p1data->num[1])
            total_number_of_functions=p1data->num[1];

          CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
          CALL(Pass2PutArg(scr, p1data->num[1]));
          CALL(Pass2Put(scr, PASS2_FUNCTION_START, p1data, &function_contain_link, 0));
/*
          CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
          CALL(Pass2PutArg(scr, p1data->num[0]));
          CALL(Pass2PutArg(scr, p1data->num[1]));
          CALL(Pass2PutArg(scr, (long)*pass2_list));
          CALL(Pass2Put(scr, PASS2_LOCAL_FUNCTION, p1data, &declare_link, 0));
*/
          p1data=p1data->next;	/* eat function declare */
          p1data=p1data->next;	/* eat parameter list */
        }
        function_start=function_contain_link;

        p1data=p1data->next;	/* eat open paren */
        while (p1data->code!=COMP_CLOSE_PAREN) {
          declare_link=&function_declare_link_first;
          CALL(Pass2PutArg(scr, p1data->num[0]));
          CALL(Pass2PutArg(scr, p1data->num[1]));
          if (total_number_of_variables<p1data->num[1])
            total_number_of_variables=p1data->num[1];
          CALL(Pass2PutString(scr, p1data->string, p1data->strlen));
          while (declare_link->next && declare_link->next->num[0]<p1data->num[0])
            declare_link=declare_link->next;
          CALL(Pass2Put(scr, PASS2_DECLARE, p1data, &declare_link, 0));

          CALL(Pass2PutArg(scr, p1data->num[1]));
          CALL(Pass2PutArg(scr, argument_count++));
          CALL(Pass2Put(scr, PASS2_ASSIGN_ARGUMENT, p1data, &function_contain_link, 5));
          p1data=p1data->next; /* eat declaration */
        }
        p1data=p1data->next; /* eat close paren */
        if (p1data->code==COMP_OPEN_BRACE) {
          p1data=p1data->next;	/* eat me */
          mainlevel++;
          CALL(ExamineLevel(scr, &p1data, &function_contain_link));			/* examine level */
          mainlevel--;
        }
      }
      break;
    case COMP_MAIN_START:
      main_label_number=labelcount;
      labelcount++;
      mainlevel++;
      CALL(Pass2PutArg(scr, main_label_number));
      CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
      function_start=*pass2_list;
        /* Falling down */
    case COMP_OPEN_BRACE:
      p1data=p1data->next;	/* eat me */
      CALL(ExamineLevel(scr, &p1data, pass2_list));			/* examine level */
      break;
    case COMP_MAIN_END:
      CALL(Pass2Put(scr, PASS2_EXIT, p1data, pass2_list, 1));
      CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));
#if 0
      if (function_declare_link_first.next) {
        CALL(Pass2Link(scr, &function_start, function_declare_link_first.next));
      }
      function_declare_link=&function_declare_link_first;
      function_declare_link_first.next=NULL;
#endif
      {
        struct Pass2Data *declare_link=&main_declare_link_first;
        while (declare_link->next)
          declare_link=declare_link->next;
        CALL(Pass2PutArg(scr, main_label_number));
        CALL(Pass2Put(scr, PASS2_MAIN_START, p1data, &declare_link, 3));
#if 0
        if (main_declare_link_first.next) {
          struct Pass2Data *temp=&pass2_first_data;
          CALL(Pass2Link(scr, &temp, main_declare_link_first.next));
          main_declare_link_first.next=NULL;
        }
#endif
        CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
#if 0
        if (function_contain_link_first.next) {
          CALL(Pass2Link(scr, pass2_list, function_contain_link_first.next));
          function_contain_link_first.next=NULL;
        }
#endif
        CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
        CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
        CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
      }
      p1data=p1data->next;	/* eat me */
      break;
    case COMP_END_OF_FUNCTION:
      CALL(Pass2Put(scr, PASS2_RETURN, p1data, &function_contain_link, 1));
      CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, &function_contain_link, 1));
      CALL(Pass2Put(scr, PASS2_END_OF_FUNCTION, p1data, &function_contain_link, 0));
      p1data=p1data->next;	/* eat me */
      if (function_declare_link_first.next) {
        CALL(Pass2Link(scr, &function_start, function_declare_link_first.next));
      }
      function_declare_link=&function_declare_link_first;
      function_declare_link_first.next=NULL;
      break;
      
    case COMP_CLOSE_BRACE:
      stop=TRUE;
      p1data=p1data->next;	/* eat me */
      break;
    case COMP_AMOUNT_VARIABLES:
      CALL(Pass2PutArg(scr, p1data->num[0]));
      CALL(Pass2Put(scr, PASS2_AMOUNT_VARIABLES, p1data, pass2_list, 0));
      p1data=p1data->next;	/* eat me */
      break;
    case COMP_IF:
      {
        long label=labelcount;
        labelcount+=2;
        p1data=p1data->next; /* Eat and forget IF */
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_IFNOT_BRANCH_ELSE, p1data, pass2_list, 3));

        p1data=p1data->next;				/* Eat begin of expression */
        CALL(ParseExpression(scr, &p1data, pass2_list));/* repeat command */
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with IFNOT */
        p1data=p1data->next;				/* Eat open brace */
        CALL(ExamineLevel(scr, &p1data, pass2_list));	/* repeat command */
        if (p1data->code==COMP_ELSE) {
          CALL(Pass2PutArg(scr, label+1));
          CALL(Pass2Put(scr, PASS2_JUMP_OVER_ELSE, p1data, pass2_list, 3));/* Store jump over else */
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2Put(scr, PASS2_LABEL_ELSE, p1data, pass2_list, 0));/* Store else label */
          p1data=p1data->next;				/* eat else */
          p1data=p1data->next;				/* eat open brace */
          CALL(ExamineLevel(scr, &p1data, pass2_list));	/* else */
          CALL(Pass2PutArg(scr, label+1));
          CALL(Pass2Put(scr, PASS2_END_OF_ELSE, p1data, pass2_list, 0));/* Store else label */
        } else {
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2Put(scr, PASS2_LABEL_ELSE, p1data, pass2_list, 0));/* Store else label */
        }
      }
      break;
    case COMP_FOR:
      {
        long label=labelcount;
        struct Pass2Data *stack1, *stack2;
        labelcount++;
        p1data=p1data->next;				/* Eat and forget FOR */
        p1data=p1data->next;				/* eat begin of expression */
        CALL(ParseExpression(scr, &p1data, pass2_list));/* initial expression */
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_LABEL_BEGIN, p1data, pass2_list, 0));	/* Store label */
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_IFNOT_BRANCH_BREAK, p1data, pass2_list, 3));
        p1data=p1data->next;				/* eat begin of expression */
        CALL(ParseExpression(scr, &p1data, pass2_list));/* condition */
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with IFNOT */

        stack1=*pass2_list;
        p1data=p1data->next;				/* eat begin of expression */
        CALL(ParseExpression(scr, &p1data, pass2_list));/* repeat command */
        stack2=*pass2_list;
        *pass2_list=stack1;
        level++;
        p1data=p1data->next;				/* eat open brace */
        CALL(ExamineLevel(scr, &p1data, pass2_list));	/* repeat command */
        level--;
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_CONTINUE, p1data, pass2_list, 0));/* A continue goes here */
        if (stack1!=stack2)
          *pass2_list=stack2;

        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_GOTO_BEGIN, p1data, pass2_list, 3));	/* Repeat for */
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_BREAK, p1data, pass2_list, 0));	/* A break goes here */
      }
      break;
    case COMP_CONTINUE:
      p1data=p1data->next;				/* Eat and forget CONTINUE */
      CALL(Pass2PutArg(scr, level));
      CALL(Pass2Put(scr, PASS2_CONTINUE, p1data, pass2_list, 3));
      break;
    case COMP_WHILE:
      {
        long label, label2, label_else;
        struct Pass1Data *stack1;
        label=labelcount;
        labelcount++;
        label2=labelcount;
        labelcount++;
        label_else=labelcount;
        labelcount++;
        p1data=p1data->next;				/* Eat and forget WHILE */
        p1data=p1data->next;				/* eat begin of expression */
        stack1=p1data;
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_LABEL_BEGIN, p1data, pass2_list, 0));	/* Store label */
        CALL(Pass2PutArg(scr, label_else));
        CALL(Pass2Put(scr, PASS2_IFNOT_BRANCH_ELSE, p1data, pass2_list, 3));
        CALL(ParseExpression(scr, &p1data, pass2_list));/* condition */
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with IFNOT */
        CALL(Pass2PutArg(scr, label2));
        CALL(Pass2Put(scr, PASS2_LABEL_BEGIN, p1data, pass2_list, 0));	/* Store label */

        level++;
        p1data=p1data->next;				/* eat open brace */
        CALL(ExamineLevel(scr, &p1data, pass2_list));	/* repeat command */
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_CONTINUE, p1data, pass2_list, 0));/* A continue goes here */
        level--;

        if (p1data->code==COMP_ELSE) {
          CALL(Pass2PutArg(scr, level));
          CALL(Pass2Put(scr, PASS2_IFNOT_BRANCH_BREAK, stack1, pass2_list, 3));
          CALL(ParseExpression(scr, &stack1, pass2_list));/* condition */
          CALL(Pass2Put(scr, PASS2_END_OF_EXPR, stack1, pass2_list, 1));	/* together with IFNOT */
          CALL(Pass2PutArg(scr, label2));
          CALL(Pass2Put(scr, PASS2_GOTO_BEGIN, stack1, pass2_list, 3));
          p1data=p1data->next;				/* eat else */
          CALL(Pass2PutArg(scr, label_else));
          CALL(Pass2Put(scr, PASS2_LABEL_ELSE, p1data, pass2_list, 0));/* Store else label */
          p1data=p1data->next;				/* eat open brace */
          CALL(ExamineLevel(scr, &p1data, pass2_list));	/* else */
        } else {
          CALL(Pass2PutArg(scr, label));
          CALL(Pass2Put(scr, PASS2_GOTO_BEGIN, p1data, pass2_list, 3));	/* Repeat for */
          CALL(Pass2PutArg(scr, label_else));
          CALL(Pass2Put(scr, PASS2_LABEL_ELSE, p1data, pass2_list, 0));/* Store else label */
        }
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_BREAK, p1data, pass2_list, 0));	/* A break goes here */
      }
      break;
    case COMP_SWITCH:
      {
        p1data=p1data->next;				/* Eat and forget SWITCH */
        CALL(Pass2Put(scr, PASS2_SWITCH, p1data, pass2_list, 1));
        p1data=p1data->next;				/* eat begin of expression */
        CALL(ParseExpression(scr, &p1data, pass2_list));/* initial expression */
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with SWITCH */
        level++;
        p1data=p1data->next;				/* eat open brace */
        CALL(ExamineLevel(scr, &p1data, pass2_list));	/* repeat command */
        level--;
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_BREAK, p1data, pass2_list, 0));	/* A break goes here */
      }
      break;
    case COMP_CASE:
      {
        long label=labelcount;
        labelcount++;
        p1data=p1data->next;				/* Eat and forget CASE */
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_CASE, p1data, &case_link, 3));
        p1data=p1data->next;				/* eat begin of expression */
        CALL(ParseExpression(scr, &p1data, &case_link));/* initial expression */
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, &case_link, 1));	/* together with CASE */
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
      }
      break;
    case COMP_DEFAULT:
      {
        long label=labelcount;
        labelcount++;
        p1data=p1data->next;				/* Eat and forget DEFAULT */
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_LABEL_GOTO, p1data, &default_link, 3));
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_LABEL, p1data, pass2_list, 0));
      }
      break;
    case COMP_EXIT:
      CALL(Pass2Put(scr, PASS2_EXIT, p1data, pass2_list, 1));
      p1data=p1data->next;				/* Eat and forget EXIT */
      p1data=p1data->next;				/* eat begin of expression */
      CALL(ParseExpression(scr, &p1data, pass2_list));/* initial expression */
      CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with EXIT */
      break;
    case COMP_BREAK:
      {
        long count=p1data->num[0];
        p1data=p1data->next;				/* Eat and forget BREAK */
        if (p1data->next->code!=COMP_END_OF_EXPR) {
          CALL(Pass2Put(scr, PASS2_BREAK_EXPR, p1data, pass2_list, 1));
          p1data=p1data->next;				/* eat begin of expression */
          CALL(ParseExpression(scr, &p1data, pass2_list));/* initial expression */
          CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with BREAK */
          {
            long downcount=level;
            while (--count>=0) {
              downcount--;
              CALL(Pass2PutArg(scr, downcount));
              CALL(Pass2Put(scr, PASS2_GOTO_BREAK, p1data, pass2_list, 3));
            }
          }
          CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with BREAK */
        } else {
          p1data=p1data->next;				/* eat begin of expression */
          p1data=p1data->next;				/* eat end of expression */
          CALL(Pass2PutArg(scr, level-1));
          CALL(Pass2Put(scr, PASS2_GOTO_BREAK, p1data, pass2_list, 3));
        }
      }
      break;
    case COMP_DO:
      {
        long label=labelcount;
        labelcount++;
        p1data=p1data->next;				/* Eat and forget DO */
        p1data=p1data->next;				/* eat begin of expression */
        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_LABEL_BEGIN, p1data, pass2_list, 0));	/* Store label */
        level++;
        CALL(ExamineLevel(scr, &p1data, pass2_list));	/* repeat command */
        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_CONTINUE, p1data, pass2_list, 0));/* A continue goes here */
        level--;

        CALL(Pass2PutArg(scr, label));
        CALL(Pass2Put(scr, PASS2_IF_BRANCH, p1data, pass2_list, 3));
        p1data=p1data->next;				/* eat while */
        p1data=p1data->next;				/* eat begin of expression */
        CALL(ParseExpression(scr, &p1data, pass2_list));/* repeat command */
        CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with IF_BRANCH */

        CALL(Pass2PutArg(scr, level));
        CALL(Pass2Put(scr, PASS2_LABEL_BREAK, p1data, pass2_list, 0));	/* A break goes here */
      }
      break;
    case COMP_RETURN:
      p1data=p1data->next;				/* Eat and forget RETURN */
      CALL(Pass2Put(scr, PASS2_RETURN, p1data, pass2_list, 1));	/* A break goes here */
      p1data=p1data->next;				/* eat begin of expression */
      CALL(ParseExpression(scr, &p1data, pass2_list));/* repeat command */
      CALL(Pass2Put(scr, PASS2_END_OF_EXPR, p1data, pass2_list, 1));	/* together with RETURN */
      break;
    case COMP_RESIZE:
      p1data=p1data->next;				/* Eat and forget RESIZE */
      CALL(HandleReference(scr, &p1data, pass2_list));
      CALL(Pass2Put(scr, PASS2_RESIZE, p1data, pass2_list, 1));
      do {
        p1data=p1data->next; 				/* eat start of expr */
        CALL(Pass2Put(scr, PASS2_OPEN_BRACKET, p1data, pass2_list, 1));
        CALL(ParseExpression(scr, &p1data, pass2_list));
        CALL(Pass2Put(scr, PASS2_CLOSE_BRACKET, p1data, pass2_list, 1));
      } while (p1data->code==COMP_START_OF_EXPR);
      break;
    default:
      {
        register struct Pass1Data *oldp1=p1data;
        CALL(ParseExpression(scr, &p1data, pass2_list));/* parse expression */
        if (p1data==oldp1) {
          printf("UNKNOWN CODE (%d)(no %d)\n", p1data->code, p1data->counter);
          exit(0);
        }
      }
      break;
    }
  } while (p1data && !stop);
#if 0
  if (!p1data && !level) {
    struct Pass2Data *declare_link=&main_declare_link_first;
    while (declare_link->next)
      declare_link=declare_link->next;
    CALL(Pass2PutArg(scr, main_label_number));
    CALL(Pass2Put(scr, PASS2_MAIN_START, p1data, &declare_link, 3));
    if (main_declare_link_first.next) {
      struct Pass2Data *temp=&pass2_first_data;
      CALL(Pass2Link(scr, &temp, main_declare_link_first.next));
      main_declare_link_first.next=NULL;
    }
    CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
    if (function_contain_link_first.next) {
      CALL(Pass2Link(scr, pass2_list, function_contain_link_first.next));
      function_contain_link_first.next=NULL;
    }
    CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
    CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
    CALL(Pass2Put(scr, PASS2_MAIN_END, p1data, pass2_list, 0));
  }
#endif
  if (case_link_first.next || default_link_first.next) {
    CALL(Pass2PutArg(scr, level));
    CALL(Pass2Put(scr, PASS2_LABEL_BREAK, p1data, pass2_list, 0));
    if (default_link_first.next) {
      CALL(Pass2Link(scr, &case_link, default_link_first.next));
      default_link_first.next=NULL;
    } else {
      CALL(Pass2PutArg(scr, level));
      CALL(Pass2Put(scr, PASS2_GOTO_BREAK, p1data, &case_link, 3));
    }
    if (case_link_first.next) {
      CALL(Pass2Link(scr, &initial_link, case_link_first.next));
      case_link_first.next=NULL;
    }
  }
  if (!p1data) {
    if (main_declare_link_first.next) {
      struct Pass2Data *temp=&pass2_first_data;
      CALL(Pass2Link(scr, &temp, main_declare_link_first.next));
      main_declare_link_first.next=NULL;
    }
    if (function_declare_link_first.next) {
      CALL(Pass2Link(scr, &function_start, function_declare_link_first.next));
    }
    function_declare_link=&function_declare_link_first;
    function_declare_link_first.next=NULL;
    if (function_contain_link_first.next) {
      CALL(Pass2Link(scr, pass2_list, function_contain_link_first.next));
      function_contain_link_first.next=NULL;
    }
  }
  *p1datapek=p1data;
  return FPL_OK;
}

ReturnCode Pass2Start(struct Data *scr)
{
  ReturnCode ret;
  struct Pass1Data *p1data=first_data.next;
  struct Pass2Data *pass2_list=&pass2_first_data;

  memset(&pass2_first_data, 0, sizeof(struct Pass2Data));
  memset(&pass2_current_data, 0, sizeof(struct Pass2Data));
  
  memset(&main_declare_link_first,0, sizeof(struct Pass2Data));
  memset(&function_declare_link_first, 0, sizeof(struct Pass2Data));
  memset(&function_contain_link_first, 0, sizeof(struct Pass2Data));
  main_declare_link=&main_declare_link_first;
  function_declare_link=&function_declare_link_first;
  function_contain_link=&function_contain_link_first;
  function_start=NULL;
  
  mainlevel=0;
  level=0;
  labelcount=0;
  total_number_of_variables=0;
  total_number_of_functions=0;
  max_level=0;
  
  CALL(ExamineLevel(scr, &p1data, &pass2_list));
  CALL(Pass2PushPut(scr, &pass2_list));		/* flush list */
  return FPL_OK;
}

