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

#ifndef __AROS__
#include <dos.h>
#endif

#elif defined(UNIX) || defined(WIN32)
#include <sys/types.h>
#include <stdio.h>
#endif

#include "script.h"
#include "debug.h"

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
  while(scr->prg<=scr->prog->lines) {
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
  while(scr->prg<=scr->prog->lines) {
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
	/* This is the first 'real' character after a newline! That means
	   this could be a valid #line-instruction! */
	scr->text++; /* pass the hash */
	if(!Getword(scr)) {
          /* there was a word following the hash sign! */

	  if(!strcmp(scr->buf, "line")) {
            /* 'line' instruction! */
            scr->virprg=Strtol(scr->text, 10, &scr->text); /* get number */
            Eat(scr); /* get whitespace */
            if(*scr->text==CHAR_QUOTATION_MARK) {
              /* we have a new virtual file name! */
              scr->virfile=scr->text++; /* just point to this text! */
              if(GetEnd(scr, CHAR_QUOTATION_MARK, 255, FALSE))
                return FPLERR_SYNTAX_ERROR;
              Eat(scr);
            }
            break;
          }
	  if(!strcmp(scr->buf, "pragma")) {
            /* 'pragma' instruction! */
            Getword(scr); /* get the following word */
            if(!strcmp(scr->buf, "nocache")) {
              /*
               * The pragma 'nocache' marks this file to get flushed
               * from memory as fast as not in use
               */
              scr->prog->flags |= PR_FLUSH_NOT_IN_USE;
              break;
            }
            if(!strcmp(scr->buf, "cache")) {
              /*
               * The pragma 'cache' marks this file to *not* get flushed
               * from memory when not in use
               */
              scr->prog->flags &= ~PR_FLUSH_NOT_IN_USE;
              break;
            }
            if(!strcmp(scr->buf, "reread")) {
              /*
               * The pragma 'reread' marks this file to get reread into
               * memory when accessed and the actual file is changed on disk
               */
              scr->prog->flags |= PR_REREAD_CHANGES;
              break;
            }
            if(!strcmp(scr->buf, "noreread")) {
              /*
               * The pragma 'noreread' marks this file to *not* get reread
               * into memory when accessed and the actual file is changed
               * on disk
               */
              scr->prog->flags &= ~PR_REREAD_CHANGES;
              break;
            }
            /* no supported pragma was found, eat the rest of the line as
               usual in this case */
          }
	  while (*++scr->text!=CHAR_NEWLINE);
        }
#ifdef UNIX
	else if('!' == scr->text[0]) {
	  /* #! - styled line, skip the rest of it! */
	  while (*++scr->text!=CHAR_NEWLINE);
	}
#endif
      } else
	return(FPL_OK);
      break;
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
 * This routine gets called everytime the interpreter finds an ASCII
 * zero in the program. This is made like this for future version which
 * will be able to specify programs in several ways. (Not only the
 * array and continues memory alternatives!)
 *
 *****/

ReturnCode REGARGS
Newline(struct Data *scr)
{
  if(scr->prg<scr->prog->lines) {
    scr->text=(&scr->prog->program)[scr->prg++];
    return(FPL_OK);
  } else
    return(FPLERR_UNEXPECTED_END);
}


ReturnCode REGARGS ScanForNext(struct Data *scr,
                               Operator op) /* previous operator */
{
  long val=0;
  long junk;
  ReturnCode ret;
  while( !( ret = Eat(scr) ) ) {
    if(ispunct(scr->text[0])) {
      switch(scr->text[0]) {
      case CHAR_OPEN_PAREN:
	CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));
	continue;
      case CHAR_OPEN_BRACKET:
	if(GetEnd(scr, CHAR_CLOSE_BRACKET, CHAR_OPEN_BRACKET, TRUE))
          return FPLERR_MISSING_BRACKET;
	continue;
      case CHAR_SEMICOLON:
      case CHAR_CLOSE_PAREN:
      case CHAR_CLOSE_BRACKET:

      case CHAR_OPEN_BRACE:  /* these two are very wrong if they appear! */
      case CHAR_CLOSE_BRACE:

	/* done, return to base */
	return FPL_OK;
	break;
      case CHAR_QUOTATION_MARK:
	if(GetEnd(scr, CHAR_QUOTATION_MARK, 255, FALSE))
          return FPLERR_SYNTAX_ERROR;
	break;
      case CHAR_AND:
	break;
      case CHAR_OR:
	if(CHAR_OR == scr->text[1]) {
	  if(OP_LOGAND == op) {
	    /* we have to stop here! */
	    return FPL_OK;
	  }
	}
	break;
      case CHAR_APOSTROPHE:
	/*
	 * Eat this properly!!!
	 */
	scr->text++;
	CALL(ReturnChar(scr, &junk, FALSE));
	if(CHAR_APOSTROPHE!=*scr->text++)
	  return(FPLERR_MISSING_APOSTROPHE);
	continue;
      case CHAR_QUESTION:
	if(OP_LOGAND == op ||
	   OP_LOGOR == op)
	  return FPL_OK;
	if(OP_COND1 == op)
	  val++;	/* remember there was an extra '?' !! */
	break;
      case CHAR_COLON:
	if(OP_COND1 == op) {
	  if(!val--)	/* don't forget if there were any extra '?' !! */
	    return FPL_OK;
	}
	break;
      case CHAR_COMMA:
	return FPL_OK;
      }
      scr->text++;
    }
    else {
      while(!ispunct(*scr->text))
	scr->text++;
    }
  }
}
