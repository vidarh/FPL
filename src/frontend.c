/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 frontend.c

 All frontend functions.

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

#if defined(AMIGA)
#include <exec/types.h>
#include <proto/exec.h>
#elif defined(UNIX)
#include <sys/types.h>
#endif

#include "script.h"
#include "debug.h"
#include <stdio.h>

/***************************************************************************
 *
 * fplGetErrorMsg()
 *
 * Returns a char pointer to an error message to the error given as argument.
 *
 ******/

PREFIX uchar *
fplGetErrorMsg(AREG(0) struct Data *scr,
               DREG(0) long error,
               AREG(1) uchar *buffer)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplGetErrorMsg");
#endif
  return GetErrorMsg(scr, error, buffer);
}

uchar * REGARGS
GetErrorMsg(struct Data *scr,
            long error,
            uchar *buffer)
{
  /* All FPL error messages. */
  static const uchar *errors[]={
    "", /* not used */
    "Division by zero",
    "Illegal anchor",
    "Illegal array:",   /* */
    "Illegal assign",
    "Illegal break",
    "Illegal condition operator",
    "Illegal continue",
    "Illegal declaration",
    "Illegal parameter",
    "Illegal pre operation",
    "Illegal prototype",
    "Illegal resize",
    "Illegal statement",
    "Illegal variable type",
    "Internal",
    "Function not found:", /* */
    "Missing apostrophe",
    "Missing argument",
    "Missing brace",
    "Missing bracket",
    "Missing operand",
    "Missing parentheses",
    "Missing semicolon",
    "Incomplete statement",
    "File",
    "Out of memory",
    "Parameter out of range",
    "Out of stack space",
    "Program stopped",
    "Read only violation:", /* */
    "Syntax",
    "Unbalanced comment",
    "Unexpected end of program",
    "Unmatched brace",
    "Identifier not found:",  /* */
    "Identifier already used:", /* */

    "Missing colon",   /* from version 7 */
    "Missing 'while'", /* from version 7 */

    /* These were added to version 11: */

    "Illegal 'case'",
    "Illegal 'default'",
    "Unexpected integer statement",
    "Unexpected string statement",
    "Illegal string index:",
    "Illegal reference",
    "Too many parameters",
    
    "Unknown" /* MUST be the last member */
    };

  uchar len;
  if(!scr || !buffer)
    return(NULL);
  strcpy(buffer, errors[((error>FPL_EXIT_OK) && (error<FPLERR_UNKNOWN_ERROR))?
			(error-2):(FPLERR_UNKNOWN_ERROR-2)]);
  len=strlen(buffer);
  if(buffer[len-1]==':') {
    /* Add " <buffer> ", but avoid sprintf() */
    strcat(buffer, " \"");
    strcat(buffer, scr->buf);
    strcat(buffer, "\"");
  }
  strcat(buffer, " error!");
  return(buffer);
}

#ifdef AMIGA
long PREFIX fplOpenLib(AREG(0) struct Data *scr,
                       AREG(1) uchar *name,   /* funclib name */
                       DREG(0) long version, /* version number required */
                       DREG(1) long flags)
{
  long retval;
  ReturnCode ret;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplOpenLib");
#endif
  CALL(OpenLib(scr, name, version, &retval, (uchar)flags));
  if(retval)
    return -retval;
  return FPL_OK;
}

long PREFIX fplCloseLib(AREG(0) struct Data *scr,
                        AREG(1) uchar *name, /* funclib name */
                        DREG(0) long force) /* boolean force close! */
{
  long retval;
  ReturnCode ret;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplCloseLib");
#endif
  CALL(CloseLib(scr, name, FPLLIB_FORCE, &retval));
  if(retval)
    return -retval;
  return FPL_OK;
}
#endif
