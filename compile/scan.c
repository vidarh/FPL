/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 scan.c

 Various program scanning/reading routines.

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
#include <dos.h>

#elif defined(UNIX)
#include <sys/types.h>
#include <stdio.h>
#endif

#include "script.h"

static ReturnCode INLINE Eatcomment(struct Data *);

/************************************************************************
 *
 * int GetEnd(struct Data *, uchar, uchar, uchar)
 *
 * Makes the current position to be the one right after the character
 * you wanna search for.
 *
 * Returns error code.
 *
 *****/

ReturnCode REGARGS
GetEnd(struct Data *scr, /* giant script structure */
       uchar leta,	 /* what character you do wanna find */
       uchar motsats,	 /* the opposite character do the one above */
       uchar outside)	 /* TRUE/FALSE if outside an opposite version */
{
  ReturnCode ret;
  uchar find=1-outside;
  long junk; /* only for the ReturnChar() function */
  long prg=scr->prg;
  uchar *text=scr->text;
  uchar check;
  while(scr->prg<= 1) {
    check=*scr->text;
    if(check==leta) {
      scr->text++;
      if(!--find)
	return(FPL_OK);
    } else if(check==motsats) {
      find++;
      scr->text++;
    } else if(check==CHAR_QUOTATION_MARK) {
      scr->text++;
      if(GetEnd(scr, CHAR_QUOTATION_MARK, (uchar)255, FALSE))
	return(FPLERR_SYNTAX_ERROR); /* missing quotation mark */
    } else if(check==CHAR_APOSTROPHE && leta!=CHAR_QUOTATION_MARK) {
      scr->text++;
      CALL(ReturnChar(scr, &junk, FALSE));
      if(CHAR_APOSTROPHE!=*scr->text++)
	return(FPLERR_MISSING_APOSTROPHE);
    } else if(check==CHAR_ASCII_ZERO) {
      CALL(Newline(scr));
    } else if(leta==CHAR_QUOTATION_MARK && check == CHAR_BACKSLASH) {
      CALL(ReturnChar(scr, &junk, TRUE));
    } else {
      if(check==CHAR_NEWLINE)
	scr->virprg++;
      scr->text++;
      if(leta!=CHAR_QUOTATION_MARK && Eat(scr))
        /* we only call Eat() if this is *not* a string passing! */
	break;
    }
  }
  scr->text=text;
  scr->prg=prg;
  return(FPLERR_MISSING_PARENTHESES);
}

/**********************************************************************
 *
 * Getword()
 *
 * Store next word in a buffer. Returns error code!
 *
 *******/

ReturnCode REGARGS
Getword(struct Data *scr)
{
  ReturnCode ret;
  uchar len=0;
  uchar *buffer = scr->buf;
  if(ret=Eat(scr))
    ;
  else if(!isident(*scr->text))
    ret=FPLERR_SYNTAX_ERROR; /* non-alpha char found where alpha is supposed */
  else
    do {
      if(len<IDENTIFIER_LEN) {
	/*
	 * With the length check above, we can use identifiers with
	 * _any_ length. There are only IDENTIFIER_LEN number of
	 * significant characters!
	 *
	 */
	len++;
	*buffer++=*scr->text++;
      }
    } while(isidentnum(*scr->text));
  *buffer=0;
  return(ret);
}

/**********************************************************************
 *
 * int Eatcomment(struct Data *);
 *
 * Jumps to the end of the comment we're standing on.
 *
 *******/

static ReturnCode INLINE Eatcomment(struct Data *scr)
{
  ReturnCode ret;
  long nums=0;
  scr->text+=2;
  while(scr->prg<=1) {
    switch(scr->text[0]) {
    case CHAR_MULTIPLY:
      if(scr->text[1]==CHAR_DIVIDE) {
	scr->text+=2;
        if(nums--)
          break;
	return(FPL_OK);
      } else
	scr->text++;
      break;
    case CHAR_ASCII_ZERO:
      CALL(Newline(scr));
      break;
    case CHAR_NEWLINE:
      scr->text++;
      scr->virprg++; /* stepped down another virutal line! */
      break;
    case CHAR_DIVIDE:
      if(scr->flags&FPLDATA_NESTED_COMMENTS && scr->text[1]==CHAR_MULTIPLY) {
        nums++;
        scr->text+=2;
        break;
      }
    default:
      scr->text++;
      break;
    }
  }
  return(FPLERR_UNBALANCED_COMMENT);
}

ReturnCode REGARGS
HashScan(struct Data *scr)
{
  ReturnCode ret;
  uchar *pnt;
  if(CHAR_HASH == scr->text[0]) {
    /* This is the first 'real' character after a newline! That means
       this could be a valid #line-instruction! */
    int num=0;
    scr->text++; /* pass the hash */
    if(!Getword(scr) || (num = Strtol(scr->text, 10, &scr->text))) {
      /* there was a word or a number following the hash sign! */
      if(num || !strcmp(scr->buf, "line")) {
        /* 'line' instruction! */
        if(num)
          scr->virprg=num-1;
        else
          scr->virprg=Strtol(scr->text, 10, &scr->text)-1; /* get number */
        while(isspace(*scr->text) && '\n' != *scr->text)
          scr->text++; /* get ONLY whitespace, nothing else */
        if(*scr->text==CHAR_QUOTATION_MARK) {
          /* we have a new virtual file name! */
          FREE(scr->virfile); /* remove previous */
          GETMEM(pnt, MAX_FILENAME_LENGTH);
          scr->virfile = pnt;
          ++scr->text;
          do {
             *pnt++ = *scr->text;
          } while (*++scr->text != CHAR_QUOTATION_MARK);
          ++scr->text;
          *pnt=0; /* zero terminate */
          Eat(scr); /* get whitespace */
        }
      }
      else if(!strcmp(scr->buf, "pragma")) {
        /* 'pragma' instruction! */
        Getword(scr); /* get the following word */
        /* no supported pragma was found, eat the rest of the line as
           usual in this case */
        while (*scr->text!=CHAR_NEWLINE)
          ++scr->text;
      }
      else {
        INFO(scr, CERROR_ILLEGAL_XXX, "#-operator");
        return FPLERR_SYNTAX_ERROR;
      }
    }
  }
  return FPL_OK;
}
/**********************************************************************
 *
 * int Eat(struct Data *);
 *
 * This eats all whitespaces, new lines and comments
 *
 * Returns error code.
 *
 *******/

ReturnCode REGARGS
Eat(struct Data *scr)
{
  ReturnCode ret;
  uchar new=0;
  while(1) {
    switch(*scr->text) {
    case CHAR_NEWLINE:
      scr->text++;
      scr->virprg++; /* stepped down another virutal line! */
      new=1;
      break;
    case CHAR_ASCII_ZERO:
      CALL(Newline(scr));
      /* This really confuses our virtual line counter! */
      new=1;
      break;
    case CHAR_HASH:
      if(new) {
        CALL(HashScan(scr));
        break;
      }
      else
       return FPL_OK;
    case CHAR_DIVIDE:
      if(scr->text[1]==CHAR_MULTIPLY) {
	CALL(Eatcomment(scr));
      } else if(scr->text[1]==CHAR_DIVIDE)
	while (*++scr->text && *scr->text!=CHAR_NEWLINE);
      else
	return(FPL_OK);
      new=0;
      break;
    default:
      if(!isspace(*scr->text))
	return(FPL_OK);
      scr->text++;
      new=0;
      break;
    }
  }
}

/*********************************************************************
 *
 * Newline()
 *
 *****/

ReturnCode REGARGS
Newline(struct Data *scr)
{
  return FPLERR_UNEXPECTED_END;
}
