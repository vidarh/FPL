/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 statement.c

 Support routines to the Expression() function.

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
#include <stdlib.h>

#include <proto/dos.h>
#include <exec/execbase.h>
#include <dos.h>

#elif defined(UNIX)
#include <sys/types.h>
#endif

#include "script.h"
#include <stdio.h>
#include <stddef.h>
#include <limits.h>

/* Lower-case digits.  */
const uchar lower_digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";

/* Upper-case digits.  */
const uchar upper_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

static ReturnCode REGARGS Ltostr(struct Data *scr, struct fplStr **,
			         long, long);
static long REGARGS my_memicmp(uchar *, uchar *, long);

/************************************************************************
 *
 * ReturnChar()
 *
 * Returns the ASCII code of the character scr->text points to.
 *
 * Supports 100% ANSI C escape sequences.
 */

ReturnCode REGARGS
ReturnChar(struct Data *scr,
           long *num,
           uchar string) /* is this is within quotes */
{
  ReturnCode ret=FPL_OK;
  long cont=TRUE, steps;
  *num=256;
  while(cont) {
    cont=FALSE;
    if(*scr->text==CHAR_BACKSLASH) {
      steps=2;
      switch(scr->text[1]) {
      case CHAR_B:
	*num=CHAR_BACKSPACE;
	break;
      case CHAR_T:
	*num=CHAR_TAB;
	break;
      case CHAR_N:
	*num=CHAR_NEWLINE;
	break;
      case CHAR_F:
	*num=CHAR_FORMFEED;
	break;
      case CHAR_BACKSLASH:
	*num=CHAR_BACKSLASH;
	break;
      case CHAR_QUOTATION_MARK:
	*num=CHAR_QUOTATION_MARK;
	break;
      case CHAR_APOSTROPHE:
	*num=CHAR_APOSTROPHE;
	break;
      case CHAR_A:
	*num=CHAR_ALERT;
	/*   ^^^^ causes warnings ('warning: \a is ANSI C "alert" character')
	 * on some compilers. Ignore and look happy!
	 */
	break;
      case CHAR_R:
	*num=CHAR_CARRIAGE_RETURN;
	break;
      case CHAR_V:
	*num=CHAR_VERTICAL_TAB;
	break;
      case CHAR_QUESTION:
	*num=CHAR_QUESTION;
	break;
      case CHAR_X:
	steps=*num=0;
	for(scr->text+=2; steps++<2 && isxdigit(*scr->text); scr->text++)
	  *num=*num*16+ (isdigit(*scr->text)?
                         *scr->text-CHAR_ZERO:
                         UPPER(*scr->text)-CHAR_UPPER_A+10);
        if(!steps)
	  return(FPLERR_SYNTAX_ERROR); /* no number followed \x sequence */
	steps=0;
	break;
      case CHAR_ZERO:
      case CHAR_ONE:
      case CHAR_TWO:
      case CHAR_THREE:
      case CHAR_FOUR:
      case CHAR_FIVE:
      case CHAR_SIX:
      case CHAR_SEVEN:
	*num=steps=0;
	for(scr->text++;steps++<3 && isodigit(*scr->text);)
	  *num=*num*8+ *scr->text++ - CHAR_ZERO;
	steps=0;
	break;
      case CHAR_NEWLINE:
	/* After a line continuation backslash, a newline is required!
	   This is made to apply to the ANSI C escape sequence standard.
	   (added 930113-1305 / DaSt) */
	cont=TRUE;
	scr->virprg++;
	break;
      case CHAR_ASCII_ZERO:
        /* Added 950603 to make the fplConvertString() work better if the
           string to convert ends with a backslash! */
        cont=TRUE;
        CALL(Newline(scr));
        steps=0;
        break;
      default:
	/* Any character not identified as a escape sequence character
	   will simply ignore the backslah character!
	   (added 930113-1307 / DaSt) */
	*num=scr->text[1];
	break;
      }
      scr->text+=steps;
    } else if(!string && *scr->text== CHAR_NEWLINE) {
      /* This won't occur if the script is preprocessed! */
      cont=TRUE;
      scr->text++;
    } else if(*scr->text== CHAR_ASCII_ZERO) {
      /* This won't occur if the script is preprocessed! */
      cont=TRUE;
      CALL(Newline(scr));
    } else {
      *num=*scr->text;
      scr->text++;
    }
  }
  return(ret);
}

/**********************************************************************
 *
 * ReturnCode NewMember(struct Expr **);
 *
 * This function adds a new member in the linked list which keeps
 * track on every operand and it's opertor in the expression.
 *
 *******/

ReturnCode REGARGS
NewMember(struct Data *scr,
          struct Expr **expr)
{
  GETMEM((*expr)->next, sizeof(struct Expr));

  (*expr)=(*expr)->next;
  (*expr)->val.val=0;
  (*expr)->unary=NULL;
  (*expr)->operator=OP_NOTHING;
  (*expr)->flags=FPL_OPERAND;
  (*expr)->next=NULL;
  return(FPL_OK);
}

static ReturnCode REGARGS
Ltostr(struct Data *scr,
       struct fplStr **string,
       long base,
       long num)
{
  /*
   * Convert the integer to string with `any base'-convertions.
   */
  extern const uchar upper_digits[];
  static const uchar *digits = upper_digits;
  long is_neg=num<0;
  long len=0;
  uchar buffer[34+sizeof(struct fplStr)];
  uchar *bufpoint;  /* the accurate position in the buffer */

  if(base>(long)strlen(digits)) {
    return FPLERR_OUT_OF_REACH;
  }
  num=ABS(num);
	
  buffer[33+sizeof(struct fplStr)]=CHAR_ASCII_ZERO; /* zero byte termination */
  bufpoint=&buffer[33+sizeof(struct fplStr)]; /* start digit output position */
	
  if(num) {
    while(num>0) {
      *--bufpoint= digits[num % base];
      num /= base;
      len++;
    }
    if(is_neg) {
      *--bufpoint='-';
      len++;
    }
  } else {
    *--bufpoint=CHAR_ZERO;
    len++;
  }

  GETMEM(*string, len+sizeof(struct fplStr));
  strcpy((*string)->string, bufpoint);
  (*string)->len=len;
  (*string)->alloc=len;
  return(FPL_OK);
}

/**********************************************************************
 *
 * Strtol()
 *
 * String to long integer. Code copied and changed from the GNU libc
 * source code package.
 *
 ****/

long REGARGS
Strtol(uchar *nptr,
       long base,
       uchar **end)
{
  uchar negative;
  unsigned long cutoff;
  unsigned long cutlim;
  long i;
  uchar *s;
  uchar c;
  uchar *save;
  long overflow;

  if (base < 0 || base == 1 || base > 36)
    base = 10;

  s = nptr;

  /* Skip white space.  */
  while(isspace(*s))
      s++;

  if (*s == CHAR_ASCII_ZERO)
    return (0);

  /* Check for a sign.  */
  else if (*s == CHAR_MINUS) {
    negative = 1;
    ++s;
  } else if (*s == CHAR_PLUS) {
    negative = 0;
    ++s;
  } else
    negative = 0;

  if ((base == 16 && s[0] == CHAR_ZERO && UPPER(s[1]) == CHAR_UPPER_X) ||
      (base == 2 && s[0] == CHAR_ZERO && UPPER(s[1]) == CHAR_UPPER_B) )
    s += 2;

  /* If BASE is zero, figure it out ourselves.  */
  if (base == 0)
    if (*s == '0') {
      switch(UPPER(s[1])) {
      case CHAR_UPPER_X:
	s += 2;
	base = 16;
	break;
      case CHAR_UPPER_B:
	s += 2;
	base = 2;
	break;
      default:
	base = 8;
	break;
      }
    } else
      base = 10;

  /* Save the pointer so we can check later if anything happened.  */
  save = s;

  cutoff = ULONG_MAX / (unsigned long int) base;
  cutlim = ULONG_MAX % (unsigned long int) base;

  overflow = 0;
  i = 0;
  for (c = *s; c; c = *++s) {
    if (isdigit(c))
      c -= '0';
    else if (isalpha(c))
      c = UPPER(c) - CHAR_UPPER_A + 10;
    else
      break;
    if (c >= base)
      break;
    /* Check for overflow.  */
    if (i > (long)cutoff || (i == (long)cutoff && c > (long)cutlim))
      overflow = 1;
    else {
      i *= (unsigned long int) base;
      i += c;
    }
  }

  *end=s; /* this is the end position of the number */

  /* Check if anything actually happened.  */
  if (s == save)
    return (0);

  /* Check for a value that is within the range of
     `unsigned long int', but outside the range of `long int'.  */
  if (i > (negative ?
	   - (long int) LONG_MIN :
	   (long int) LONG_MAX))
    overflow = 1;

  if (overflow)
    return negative ? LONG_MIN : LONG_MAX;

  /* Return the result of the appropriate sign.  */
  return (negative ? - i : i);
}

/*****************************************************************************
 *
 * my_memicmp()
 *
 * This makes a case insensitive memcmp() with the very same parameters.
 *
 *********/

static long REGARGS my_memicmp(uchar *s1, uchar *s2, long len)
{
  long pos=0;
  long result;
  while(pos < len) {
    result = tolower(s1[pos]) - tolower(s2[pos]);
    if(result)
      return result;
    pos++;
  }
  return 0;
}


ReturnCode REGARGS
  StringExpr(struct Expr *val,		/* original string -> new */
	     struct Data *scr)		/* standard */
{
  ReturnCode ret;

  CALL(Eat(scr)); /* scan white spaces */

  while(CHAR_PLUS == scr->text[0]) {
    CALL(Put(scr, COMP_APPEND_STRING));
    scr->text++; /* skip the '+' */
      
    if(!(val->flags&FPL_NOFREE) && val->val.str) {
      FREE(val->val.str); /* free the old one, new one coming up! */
      memset(val, 0, sizeof(struct Expr));
    }

    CALL(Expression(val, scr, CON_STRING, NULL));
  }
  return FPL_OK;
}
#if !defined(AMIGA) && !defined(WIN32)
#if defined(SUNOS)
#define stricmp strcasecmp
#else
/*****************************************************************************
 *
 * stricmp()
 *
 * This makes a case insensitive strcmp().
 *
 *********/

long stricmp(uchar *s1, uchar *s2)
{
  long pos=0;
  long result=0;
  while(s1[pos]) {
    result = tolower(s1[pos]) - tolower(s2[pos]);
    if(result)
      return result;
    pos++;
  }
  return 0;
}
#endif
#endif
