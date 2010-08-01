/******************************************************************************
 *                        FREXX PROGRAMMING LANGUAGE                          *
 ******************************************************************************

 error.c

 The error output function

 *****************************************************************************/

/************************************************************************
 *                                                                      *
 * fpl.library - A shared library interpreting script langauge.         *
 * Copyright (C) 1992-1994 FrexxWare                                    *
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
#include <dos.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#include "script.h"

static void OutInfo(char *file,
                    long line, /* can be set to 0 == unknown */
                    long bytepos, /* can be set to 0 == unknown */
                    char *sourceline, /* can be set to NULL == unknown */
                    ErrorCode error,
                    void **arg) /* arguments */
{
  int a;
  struct Error info[]= {
    ERR( CERROR_CMPPOS,	CERROR_INFORM ),
    ERR( CERROR_MISSING_SEMICOLON,	CERROR_WARNING ),
    ERR( CERROR_MISSING_BRACKET,	CERROR_WARNING ),
    ERR( CERROR_MISSING_PARENTHESIS,	CERROR_WARNING ),
    ERR( CERROR_MISSING_SINGLEQUOTE,	CERROR_WARNING ),
    ERR( CERROR_CANT_REACH_STATEMENT,	CERROR_WARNING ),
    ERR( CERROR_MISSING_BRACE,		CERROR_WARNING ),
    ERR( CERROR_MISSING_COLON,		CERROR_WARNING ),
    ERR( CERROR_IDENTIFIER_USED,	CERROR_ERROR ),
    ERR( CERROR_IDENTIFIER_NOT_FOUND,	CERROR_ERROR ),
    ERR( CERROR_READONLY_VIOLATE,	CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_NUMERICAL,	CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_STRING,		CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_REFERENCE,	CERROR_ERROR ),
    ERR( CERROR_MISSING_OPERAND,	CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_CONDOPER,	CERROR_ERROR ),
    ERR( CERROR_INCOMPLETE_STATEMENT,	CERROR_ERROR ),
    ERR( CERROR_TOO_LARGE_ARRAY,	CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_DECLARATION,	CERROR_ERROR ),
    ERR( CERROR_MISSING_WHILE,		CERROR_ERROR ),
    ERR( CERROR_NOT_YET_SUPPORTED,	CERROR_ERROR ),
    ERR( CERROR_ELSE,			CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_BRACE,		CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_XXX,		CERROR_ERROR ),
    ERR( CERROR_ALREADY_DEFINED,	CERROR_ERROR ),
    ERR( CERROR_MISSING_ARGUMENT,	CERROR_ERROR ),
    ERR( CERROR_EXPECTED_PAREN,		CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_PARAMETER,	CERROR_ERROR ),
    ERR( CERROR_ILLEGAL_ARRAY,		CERROR_ERROR ),
    ERR( CERROR_FILE_NOT_FOUND,		CERROR_ERROR ),
  };
  for(a=0; a<sizeof(info)/sizeof(info[0]);a++) {
    if(error == info[a].error) {
      char *text = sourceline;
      char *str;

      printf("====================\n");
      if(text) {
	char t;
	do {
	  t = *++text;
	  putchar(t);
	} while(*text && t!='\n');
	if(!t || !*text)
	  putchar('\n');
	if(bytepos>=1)
	  printf("%*c\n", bytepos, '^');
      }
      printf("%s ", file);
      if(line>0)
	printf("line %d ", line);
      switch(info[a].type) {
      case CERROR_ERROR:
	str = "Error";
	break;
      case CERROR_WARNING:
	str = "Warning";
	break;
      case CERROR_INFORM:
	str = "Message";
	break;
      default:
	str = "...";
	break;
      }
      printf("%s %d: ", str, info[a].error);
      vprintf(info[a].text, (va_list)arg);
      putchar('\n');
      return;
    }
  }
  printf("Undefined error occurred!\n");
}

void CompilerInfo(struct Data *scr, ErrorCode error, ...)
{
  va_list arg;
  char *text =scr->text;
  char *source;
  long column=0;
  long bytes=0;

  va_start(arg, error);

  if(text = scr->text) {
    while(text>=scr->program && '\n' != *text) {
      text--;
    }
    bytes  = scr->text-text;
  }
  source=text;
  while(bytes--) {
    if('\t' == *text++)
      column += 8-column%8;
    else
      column++;
  }
  scr->newerror=1; /* used */

  OutInfo(scr->virfile,
	  scr->virprg,
	  column,
	  source,
	  error,
	  (void **)arg);
}

