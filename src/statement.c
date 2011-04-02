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
#ifndef __AROS__
#include <proto/exec.h>
#endif
#include <stdlib.h>

#include <proto/dos.h>
#include <exec/execbase.h>
#ifndef __AROS__
#include <dos.h>
#include "/funclib/funclib.h"
#else
#include "../funclib/funclib.h"
#endif

#else
#include <sys/types.h>
#ifdef SUNOS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#endif

#include "script.h"
#include "debug.h"
#include <stdio.h>
#include <stddef.h>
#include <limits.h>

static ReturnCode INLINE SendMessage(struct Data *, struct fplMsg *);
static ReturnCode REGARGS Ltostr(struct Data *scr, struct fplStr **,
			         long, long);
static ReturnCode REGARGS GetSymbols(struct Data *, long, long,
                                     struct fplSymbol **);

/**********************************************************************
 *
 * ReturnCode CmpAssign()
 *
 * Performs a compound assign to the value the third argument points to.
 * The assign performed is the one with the operator specified in the fourth
 * argument eg, x, +, /, &, %, | etc, etc...
 *
 ***************/

ReturnCode REGARGS
CmpAssign(struct Data *scr,
          long val,	/* right operand */
	  long *value,	/* return value pointer */
	  long flags,	/* variable flags */
	  uchar operation)
{
  ReturnCode ret;
  switch(operation) { /* check the type of the assign */
  case CHAR_PLUS:
    *value+=val;
    break;
  case CHAR_MINUS:
    *value-=val;
    break;
  case CHAR_MULTIPLY:
    *value*=val;
    break;
  case CHAR_DIVIDE:
    *value/=val;
    break;
  case CHAR_AND:
    *value&=val;
    break;
  case CHAR_OR:
    *value|=val;
    break;
  case CHAR_REMAIN:
    *value%=val;
    break;
  case CHAR_XOR:
    *value^=val;
    break;
  case CHAR_LESS_THAN:
    *value<<=val;
    break;
  case CHAR_GREATER_THAN:
    *value>>=val;
    break;
  case CHAR_ASSIGN:
    *value=val;
    break;
  default:
    CALL(Warn(scr, FPLERR_ILLEGAL_ASSIGN)); /* >warning< */
    *value=val; /* perform a straight assign! */
    break;
  }
  if(flags&FPL_VARIABLE_LESS32) {
    /* if using less than 32 bit */
    if(flags&FPL_CHAR_VARIABLE)
      *value=(long)((signed char)*value);
    else
      *value=(long)((signed short)*value);
  }
  return(FPL_OK);
}


/**********************************************************************
 *
 * StrAssign();
 *
 * Assign a string variable.
 *
 ********/

ReturnCode REGARGS
StrAssign(struct fplStr *app,
          struct Data *scr,
	  struct fplStr **string,
	  uchar append) /* TRUE or FALSE if append */
{
  ReturnCode ret;

  if(!append) { /* if not append */
    /* Exchange this string with the old one in the variable! */
    if(*string) {
      /*
       * There is a string! Free any type!
       */
      FREE_KIND(*string);
    }
    if(!app) {
      GETMEM(app, sizeof(struct fplStr));
      memset(app, 0, sizeof(struct fplStr)); /* clean it! */
    }
    *string = app;

  } else { /* append string */
    if(! (app?app->len:0))
      /* we don't append zero length strings! */
      return FPL_OK;

    CALL(AppendStringToString(scr, string, app->string, app->len));

  }
  return(FPL_OK);
}

/************************************************************************
 *
 * AppendStringToString()
 *
 * Append a generic string to a fplStr.
 *
 */

ReturnCode REGARGS
AppendStringToString(struct Data *scr,  /* common data struct */
		     struct fplStr **string, /* variable to append to */
		     uchar *append, /* string to append */
		     long applen)  /* length of append string */
{
    long length;
    long alloc;
    long ln;
    struct fplStr *pek;
    void *dest;
    uchar type = *string?TypeMem(*string):MALLOC_DYNAMIC;

    length=*string?(*string)->len:0;
    alloc=*string?(*string)->alloc:0;

    ln = applen + length; /* total length */
    if (ln>=alloc) { /* do we have that much memory allocated? */
      /*
       * Allocate new memory for string.
       */

      GETMEM(pek, sizeof(struct fplStr)+ln+ADDSTRING_INC);
      if(MALLOC_STATIC == type)
        SwapMem(scr, pek, MALLOC_STATIC);

      if(*string) {
        memcpy(pek, (*string), length+sizeof(struct fplStr));
        FREE_KIND(*string);
      } else
	pek->len=0;
#ifdef DEBUG
      CheckMem(scr, pek);
#endif
      (*string)=pek;			  /* the new pointer */
      (*string)->alloc=ln+ADDSTRING_INC;  /* new allocated size */
    }

    dest=(void *)&(*string)->string[length];

    /* no string function... only mem-versions! */
    memcpy(dest, (void *)append, applen);

    (*string)->len += applen;
 
    (*string)->string[(*string)->len]=CHAR_ASCII_ZERO; /* zero terminate */

    return FPL_OK;
}


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


/**********************************************************************
 *
 * ReturnCode Warn();
 *
 * This routines calls the interface function to ask for permission to
 * continue the execution, even though error(s) has/have been found in
 * the interpreted program.
 *
 ******/

ReturnCode REGARGS
Warn(struct Data *scr,
     ReturnCode rtrn)
{
  struct fplArgument *pass;
  struct fplMsg *msg;
  ReturnCode ret;

  GETMEM(pass, sizeof(struct fplArgument));
  pass->ID=FPL_WARNING;
  pass->key=scr;
  pass->argc=1;
  pass->argv=(void **)&rtrn; /* first ->argv member holds the error/warning number! */

  ret=InterfaceCall(scr, pass, scr->function);

  FREE(pass);
  GetMessage(scr, FPLMSG_CONFIRM, &msg);
  if(msg) {
    if(msg->message[0]) {
      rtrn=ret;
      scr->prog->warnings++;
    }
    DeleteMessage(scr, msg);
  }
  return(rtrn);
}


#if !defined(AMIGA) || !defined(SHARED)  /* if not using SAS/C on Amiga */

#ifdef VARARG_FUNCTIONS
long fplSendTags(void *anchor, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, anchor); /* get parameter list */
#endif
  ret = fplSend(anchor, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else
long PREFIX fplSendTags(void *anchor, unsigned long tags, ...)
{
  return(fplSend(anchor, &tags));
}
#endif

#endif

/**********************************************************************
 *
 * fplSend()
 *
 * Send a message to FPL.
 *
 ******/

ReturnCode PREFIX fplSend(AREG(0) struct Data *scr,
			  AREG(1) unsigned long *tags)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplSend");
#endif
  return Send(scr, tags);
}

ReturnCode REGARGS Send(struct Data *scr,
			unsigned long *tags)
{
  struct fplMsg msg;
  long len=-1;
  struct Program *prog;
  uchar *data=NULL;
  ReturnCode ret;
  struct fplSymbol *symbol;
  struct fplStr *string;
  long mixed;
  static long *resultcode=NULL;
  uchar fplallocstring=FALSE;
  if(!scr)
    return(FPLERR_ILLEGAL_ANCHOR);

  memset(&msg, 0, sizeof(struct fplMsg));

  while(tags && *tags) {
    switch(*tags++) {

    case FPLSEND_STRING:
      /* FPLSEND_PROGRAMFILE is the same tag */
      data=(void *)*tags;
      msg.type=FPLMSG_RETURN;
      msg.flags = FPLMSG_FLG_STRING;
      break;

    case FPLSEND_STRLEN:
      len=(long)*tags;
      break;

    case FPLSEND_DONTCOPY_STRING: /* the string sent is fplAllocString()'ed */
      fplallocstring=(uchar)*tags;
      break;

    case FPLSEND_INT:
      msg.message[0]=(void *)*tags;
      msg.type=FPLMSG_RETURN;
      msg.flags = FPLMSG_FLG_INT;
      break;

    case FPLSEND_PROGRAM:
      msg.message[0]=(void *)*tags;
      msg.type=FPLMSG_PROGRAM;
      break;

    case FPLSEND_CONFIRM:
      msg.type=FPLMSG_CONFIRM;
      msg.message[0]=(void *)*tags;
      break;

    case FPLSEND_GETINTERVAL:
      *(long *)*tags=(long)scr->interfunc;
      break;

    case FPLSEND_GETFUNCTION:
      *(long *)*tags=(long)scr->function;
      break;

    case FPLSEND_GETLINE:
      *(long *)*tags=scr->prg;
      break;

    case FPLSEND_GETVIRFILE:
      *(uchar **)*tags=scr->virfile;
      break;

    case FPLSEND_GETVIRLINE:
      *(long *)*tags=scr->virprg;
      break;

    case FPLSEND_GETNEWLINE_HOOK:		/* OBSOLETE!!!! */
      break;

    case FPLSEND_GETRESULT:
      *(long *)*tags=scr->data;
      break;

    case FPLSEND_GETRETURNCODE:
      *(long *)*tags=scr->FPLret;
      break;

    case FPLSEND_GETRETURNINT: /* new from V10 */
      *(long **)*tags=scr->returnint;
      break;

    case FPLSEND_GETUSERDATA:
      *(long *)*tags=(long)scr->userdata;
      break;

    case FPLSEND_GETCOLUMN:
      if(scr->prog && scr->prog->running)
	*(long *)*tags=(scr->text-(&scr->prog->program)[scr->prg-1]+1);
      else if(scr->prog)
	/* we cannot count on this programs presence */
	*(long *)*tags=scr->prog->column;
      else
	*(long *)*tags=0; /* we don't know! */
      break;

    case FPLSEND_GETPROGNAME:
      if(scr->prog && scr->prog->name)
	*(uchar **)*tags=scr->prog->name;
      else /* we have no program information */
	*(uchar **)*tags=FPLTEXT_UNKNOWN_PROGRAM;
      break;

    case FPLSEND_GETPROG:
      if(scr->prog && scr->prog->program)
	*(uchar **)*tags=scr->prog->program;
      else /* we have no program information */
	*(uchar **)*tags=NULL;
      break;

    case FPLSEND_FLUSHCACHE:
      if(*tags)
	FlushFree(scr);
      break;

    case FPLSEND_FLUSHFILE:
      if(*tags) {
	prog=scr->programs;
	while(prog) {
	  if(prog->name && !strcmp(prog->name, (uchar *)*tags))
	    break;
	  prog=prog->next;
	}
	if(!prog)
	  return(FPLERR_INTERNAL_ERROR);
      } else
	prog=scr->programs;
      while(prog) {
	if(!(prog->running)) {
	  /* if the program isn't running right now! */
	  len=prog->flags;
	  prog->flags&=~PR_CACHEFILE; /* switch off the cache bit now */
	  CALL(LeaveProgram(scr, prog));
	  prog->flags=len; /* restore flag bits! */
	}
	if(*tags)
	  /* only the specified */
	  break;
	prog=prog->next;
      }
      break;

    case FPLSEND_FREEFILE:
      if(!(*tags))
        break;
      prog=scr->programs;
      while(prog) {
	if(prog->name && !strcmp(prog->name, (uchar *)*tags))
	  break;
	prog=prog->next;
      }
      if(!prog || prog->running || prog->openings)
	/* if not found or if the found one is currently in use! */
	return(FPLERR_ILLEGAL_PARAMETER);
      {
        for(mixed=0; mixed<scr->hash_size; mixed++) {
          register struct Identifier *nident;
          register struct Identifier *ident = scr->hash[mixed];
          while(ident) {
            nident=ident->next;
            if(!strcmp(ident->file, (uchar *)*tags))
              DelIdentifier(scr, NULL, ident);
            ident=nident;
          }
        }
      }
      DelProgram(scr, prog);
      break;

    case FPLSEND_STEP:
      if(*tags>0) {
	while((*tags)--) {
	  if(!*scr->text)
	    CALL(Newline(scr));
	  scr->text++;
	}
      } else if((signed int)(*tags)<0) {
	while((*tags)++) {
	  if( (scr->text-(&scr->prog->program)[scr->prg-1])>=0)
	    scr->text--;
	  else
	    if(scr->prg>1)
	      scr->text=(&scr->prog->program)[--scr->prg-1];
	    else
	      return(FPLERR_UNEXPECTED_END);
	}
      }
      break;
    case FPLSEND_GETSYMBOL_FUNCTIONS:
      CALL(GetSymbols(scr, FPL_EXTERNAL_FUNCTION|FPL_INSIDE_FUNCTION,
		      FPL_EXPORT_SYMBOL,
		      (struct fplSymbol **)*tags));
      break;
    case FPLSEND_GETSYMBOL_MYFUNCTIONS:
      CALL(GetSymbols(scr, FPL_EXTERNAL_FUNCTION, FPL_FUNCTION,
		      (struct fplSymbol **)*tags));
      break;
    case FPLSEND_GETSYMBOL_FPLFUNCTIONS:
      CALL(GetSymbols(scr, FPL_EXPORT_SYMBOL, FPL_INSIDE_FUNCTION,
		      (struct fplSymbol **)*tags));
      break;
    case FPLSEND_GETSYMBOL_VARIABLES:
      CALL(GetSymbols(scr, FPL_EXPORT_SYMBOL, FPL_VARIABLE,
		      (struct fplSymbol **)*tags));
      break;
    case FPLSEND_GETSYMBOL_ALLVARIABLES:
      CALL(GetSymbols(scr, ~0, FPL_VARIABLE, (struct fplSymbol **)*tags));
      break;

    case FPLSEND_GETSYMBOL_ALLFUNCTIONS:
      CALL(GetSymbols(scr, ~0, FPL_FUNCTION, (struct fplSymbol **)*tags));
      break;

    case FPLSEND_GETSYMBOL_CACHEDFILES:
      prog=scr->programs;
      mixed=0;
      while(prog) {
	if(prog->flags&PR_CACHEFILE)
	  mixed++;
	prog=prog->next;
      }

      GETMEM(symbol, sizeof(struct fplSymbol));
      symbol->num=mixed;
      GETMEM(symbol->array, mixed*sizeof(uchar *));

      mixed=0;
      prog=scr->programs;
      while(prog) {
	if(prog->flags&PR_CACHEFILE)
	  symbol->array[mixed++]=prog->name;
	prog=prog->next;
      }
      *(struct fplSymbol **)*tags=symbol;

#ifdef DEBUG
      CheckMem(scr, symbol);
      CheckMem(scr, symbol->array);
#endif

      break;

      /* ----------------  new from V10: --------------------- */

    case FPLSEND_RESULT:
      resultcode = (long *)(*tags); /* long to store result in! */
      break;

    case FPLSEND_IS_FILE_CACHED:
      if(resultcode) {
        prog=scr->programs;
        while(prog) {
          if(prog->name && !strcmp(prog->name, (uchar *)(*tags))) {
            *resultcode = TRUE;
            break;
          }
          prog=prog->next;
        }
        *resultcode = FALSE;
      }
      break;
    
      /* --------------------------------------------------- */

    case FPLSEND_GETSYMBOL_FREE:
#ifdef DEBUG
      CheckMem(scr, (void *)(*tags));
      CheckMem(scr, ((struct fplSymbol *)*tags)->array);
#endif
      FREE(((struct fplSymbol *)*tags)->array);
      FREE(*tags);
      break;

#if defined(AMIGA) && defined(SHARED)
    case FPLSEND_GETSTACKSIZE:
      *(long *)*tags=scr->stack_size;
      break;
    case FPLSEND_GETSTACKUSED:
      *(long *)*tags=GetStackUsed(scr);
      break;
#endif

    case FPLSEND_SETPROGNAME:
      if(scr->prog) {
	if(scr->prog->name)
	  FREEA(scr->prog->name);
	STRDUPA(scr->prog->name, *tags);
      }
      break;

    case FPLSEND_SETFILENAMEGET:
      if(scr->prog) {
	if(*tags)
	  scr->prog->flags|=PR_FILENAMEFLUSH;
	else
	  scr->prog->flags&=~PR_FILENAMEFLUSH;
      }
      break;
    case FPLSEND_GETVERSION:
      *(long *)(*tags) = FPL_VERSION; /* as defined in FPL.h */
      break;
    case FPLSEND_GETREVISION:
      *(long *)(*tags) = FPL_REVISION; /* as defined in FPL.h */
      break;
    }
    tags++;
  }
  if(!msg.type)
    /*
     * There is no message to send. Everything we had to do is done!
     */
    return(FPL_OK);

  if(msg.type==FPLMSG_RETURN && (msg.flags == FPLMSG_FLG_STRING)) {
    if(len<0)
      if(data)
	len=strlen(data);
    if(!len || !data)
      /* this really is a zero length string! */
      msg.message[0]=NULL;
    else {
      if(!fplallocstring) {
        /* we have to duplicate the data */
        GETMEM(msg.message[0], len+sizeof(struct fplStr));
        string=msg.message[0];
        string->len=len;
        string->alloc=len;
        memcpy(string->string, data, len); /* copy string! */
        string->string[string->len]=CHAR_ASCII_ZERO; /* zero terminate */
      } else {
        /* the data was sent as fplAllocString() data! */
        string= (struct fplStr *)(data - offsetof(struct fplStr, string));
        string->len=len;
        string->string[string->len]=CHAR_ASCII_ZERO; /* zero terminate */
        SwapMem(scr, string, MALLOC_DYNAMIC); /* convert */
        msg.message[0]=string;
      }
    }
  }
  CALL(SendMessage(scr, &msg));
  return(ret);
}


/*********************************************************************
 *
 * fplConvertString()
 *
 * Returns the number of characters converted from the FPL format
 * string to the binary sting stored in a buffer.
 *
 * The output string always get zero terminated!
 *
 *****/

long PREFIX
fplConvertString(AREG(0) struct Data *scr,
                 AREG(1) uchar *string,
                 AREG(2) uchar *buffer)
{
  long prg=scr->prg;
  uchar *text=scr->text;
  long line;
  uchar *base;
  long a;
  long number=0;

#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplConvertString");
#endif

  if(!scr->prog) {
    /*
     * There is no program at the moment!
     * create a pseudo program for now!
     */
    scr->prog=(struct Program *)MALLOC(sizeof(struct Program));
    if(!scr->prog)
      return(0); /* no characters in output! */
    scr->prog->flags|=PR_TEMPORARY;
  }
  
  base=scr->prog->name;
  line=scr->prog->lines;
/*
  if(*string==CHAR_QUOTATION_MARK)
    string++;
*/

  scr->prg=1;
  scr->text=string;
  scr->prog->lines=1;
  scr->prog->name=NULL; /* we have no file ID yet! */

  while(/* *scr->text!=CHAR_QUOTATION_MARK && */
	!ReturnChar(scr, &a, TRUE)) { /* returns non-zero when an ascii zero is
				         found! */
    *buffer++=(uchar)a;
    number++;
  }
  
  *buffer=CHAR_ASCII_ZERO;

  scr->prg=prg;
  scr->text=text;
  scr->prog->lines=line;
  scr->prog->name=base;
  
  if(scr->prog->flags&PR_TEMPORARY) {
    FREE(scr->prog);
    scr->prog=NULL;
  }

  return(number);
}

/**********************************************************************
 *
 * GetSymbols();
 *
 * Allocates a structure and data, which is a list of name pointers
 * that match the flag parameter.
 *
 *******/

static ReturnCode REGARGS
GetSymbols(struct Data *scr,
           long flag1,
           long flag2,
           struct fplSymbol **get)
{
  long i;
  long num;
  struct Identifier *ident;
  struct fplSymbol *symbol;

  for(i=num=0; i<scr->hash_size; i++) {
    ident=scr->hash[i];
    while(ident) {
      if(ident->flags&flag1 && ident->flags&flag2)
	num++;
      ident=ident->next;
    }
  }

  GETMEM(symbol, sizeof(struct fplSymbol));
  symbol->num=num;

  GETMEM(symbol->array, sizeof(uchar *)*symbol->num);

  for(i=num=0; i<scr->hash_size; i++) {
    ident=scr->hash[i];
    while(ident) {
      if(ident->flags&flag1 && ident->flags&flag2)
	symbol->array[num++]=ident->name;
      ident=ident->next;
    }
  }
  *get=symbol;

#ifdef DEBUG
  CheckMem(scr, symbol->array);
#endif
  return(FPL_OK);
}


/**********************************************************************
 *
 * SendMessage();
 *
 * Add a member to the message queue. Allocate a new struct and copy the
 * data of from second parameter message pointer.
 *
 ******/

static ReturnCode INLINE
SendMessage(struct Data *scr,
            struct fplMsg *msg)
{
  struct fplMsg *NewMsg, *ptr;

  GETMEM(NewMsg, sizeof(struct fplMsg));

  *NewMsg=*msg; /* copy all data from source */

  /* Queue the message: */
  if(ptr=scr->msg)
    ptr->prev=NewMsg; /* this message becomes the previous for this */

  scr->msg=NewMsg;
  NewMsg->next=ptr;
  NewMsg->prev=NULL; /* no previous, this is first! */

  return(FPL_OK);
}

/**********************************************************************
 *
 * DeleteMessage();
 *
 * Deletes specified or current message (NULL).
 *
 *****/

ReturnCode REGARGS
DeleteMessage(struct Data *scr,
              struct fplMsg *msg)
{
  struct fplMsg *ptr=scr->msg;
  if(msg) 
    ptr=msg;
  if(ptr) {
    if(ptr->next)
      ptr->next->prev=ptr->prev; /* redirect next message's prev pointer */
    else if(!ptr->prev) /* is this the only message? */
      scr->msg=NULL;
    if(ptr->prev)
      ptr->prev->next=ptr->next; /* redirect previous message's next pointer */
    FREE(ptr);  /* free message */
  }
  return(FPL_OK);
}

/**********************************************************************
 *
 * GetMessage()
 *
 * Returns the first message of the requested type in the pointer
 * in the third argument!
 *
 ****/

ReturnCode REGARGS
GetMessage(struct Data *scr,
           uchar type,
           struct fplMsg **get)
{
  struct fplMsg *msg=scr->msg;
  while(*get=msg) {
    if(msg->type==type)
      break;
    msg=msg->next;
  }
  return(FPL_OK);
}

/**********************************************************************
 *
 * GetProgram();
 *
 * Whenever we want to access a program in the program list, we do it
 * using this function. This enables heavy program swapping capabilities.
 * Programs that are not being used can be flushed from memory and brought
 * back whenever we need it!
 *
 ******/

ReturnCode REGARGS
GetProgram(struct Data *scr,
           struct Program *prog)
{
  struct fplArgument *arg;
  ReturnCode ret;
  struct fplMsg *msg;
  struct fplStr *string;
  if(!prog->program) {
    /*
     * The program is not currently in memory. Get it!
     */
    
    if(prog->flags&PR_FILENAMEFLUSH) {
      /*
       * We know that the program is simply to load from the file the program
       * name specifies.
       */
      CALL(ReadFile(scr, prog->name, prog));
    } else {
      /*
       * We must ask user for information!
       */
      
      GETMEM(arg, sizeof(struct fplArgument));
      arg->ID=FPL_FILE_REQUEST;
      arg->key=(void *)scr;
      arg->argv=(void **)&prog->name;
      arg->argc=1;
      ret=InterfaceCall(scr, arg, scr->function);
      FREE(arg);
      if(ret)
        return ret;
      
      GetMessage(scr, FPLMSG_PROGRAM, &msg);
      if(!msg) {
	GetMessage(scr, FPLMSG_RETURN, &msg);
	if(!msg || ((msg->flags&FPLMSG_FLG_BITS) != FPLMSG_FLG_STRING))
	  /*
	   * No kind of proper answer could be found!
	   * Dead end failure!
	   */	  
	  return FPLERR_INTERNAL_ERROR;
	
	string=(struct fplStr *)msg->message[0];
	ret = ReadFile(scr, string->string, prog);
	FREE(msg->message[0]); /* we don't need this anymore! */
	if(ret)
	  return ret;
      } else {
	/*
	 * User supplied us with a memory pointer to the program again!
	 */
	prog->program= (uchar *)msg->message[0];
	prog->flags|=PR_USERSUPPLIED;
      }
      DeleteMessage(scr, msg);
    }
  } /* else
       we already have it loaded! */
  prog->running++;
  return(FPL_OK);
}


/**********************************************************************
 *
 * LeaveProgram();
 *
 * If we leave one program, call this. If any flush is to be done, this
 * will perform that!
 *
 ******/

ReturnCode REGARGS
LeaveProgram(struct Data *scr,
             struct Program *prog)
{
  struct fplArgument *arg;
  ReturnCode ret;
  struct fplMsg *msg;
  prog->running--;
  if(prog->program && !prog->running && prog->flags&PR_FLUSH_NOT_IN_USE) {
    /*
     * The program is there and no one is using it!
     * flush it if we want to!
     */

    if(prog->flags&PR_USERSUPPLIED) {
      /*
       * This program is supplied by the external program. We cannot
       * free the memory, only tell our father that freeing is OK...
       */
      GETMEM(arg, sizeof(struct fplArgument));
      arg->ID=FPL_FLUSH_FILE;
      arg->key=(void *)scr;
      arg->argv=(void **)&prog->name;
      arg->argc=1;
      CALL(InterfaceCall(scr, arg, scr->function));
      FREE(arg);
      GetMessage(scr, FPLMSG_CONFIRM, &msg);
      /*
       * We require a {FPLSEND_CONFIRM, TRUE} message from the user before we
       * flush the user supplied function! Simply ignore implementing any
       * answer to this message if we never want to flush user supplied
       * functions.
       */
      if(msg && msg->message[0])
	/* If we got a "OK" message! */
	prog->program=NULL;
      if(msg)
	DeleteMessage(scr, msg);
    } else {
      /*
       * The memory occupied by this program is our business.
       * Swap the memory first to be sure we know the kind of it!
       */
      SwapMem(scr, prog->program, MALLOC_DYNAMIC);
      FREE(prog->program);
      prog->program=NULL; /* to visualize the clearing of this program! */
    }
  }
  return(FPL_OK);
}

static long INLINE
Exists(struct Data *scr,
       uchar *name,
       long check)
{
  ReturnCode ret;
  struct Identifier *ident;
  GetIdentifier(scr, name, &ident);
  if(check && ident) {
    switch(tolower(check)) {
    case EXISTS_FUNCTION:
      ret = ident->flags&FPL_FUNCTION;
      break;
    case EXISTS_INTEGER:
      ret = ident->flags&FPL_INT_VARIABLE;
      break;
    case EXISTS_STRING:
      ret = ident->flags&FPL_STRING_VARIABLE;
      break;
    case EXISTS_VARIABLE:
      ret = ident->flags&FPL_VARIABLE;
      break;
    }
  }
  else
    ret = ident?TRUE:FALSE;
  return ret;
}

#if 0
/*
 * These are some fixes to make this work on compiled programs too.
 * It currently does not.
 */

ReturnCode static INLINE Interpret(struct Data *scr,
                                   struct fplArgument *arg,
                                   struct Expr *val)
{
  unsigned long inttags[]={FPLSEND_INT, 0, FPLSEND_DONE};
  ReturnCode ret;
  struct Program *prog;
  struct Program *oldprog=scr->prog;
  long prg;
  long virprg;
  char *text;
  char *virfile;
  
  GETMEM(prog, sizeof(struct Program));
  memcpy(prog, scr->prog, sizeof(struct Program));

  prog->lines=1;
  prog->flags &= ~(PR_COMPILED|PR_SELECTED_FPC);
  STRDUP(prog->name, "<interpret>");
  
  prog->next = scr->programs;
  scr->prog = scr->programs = prog;

  prg=scr->prg;
  text=scr->text;    
  virprg=scr->virprg;
  virfile=scr->virfile;
  
  scr->virprg=1;
  scr->virfile=NULL;
  scr->prg=1;
  scr->text=(uchar *)arg->argv[0];

  scr->interpret=NULL; /* nothing recursive here, no no! */

  CALL(LeaveProgram(scr, oldprog));
  CALL(GetProgram(scr, prog));
  
  GETMEM(val, sizeof(struct Expr));
  ret = Script(scr, val, SCR_NORMAL, NULL);
  if( ret ) {
    if(scr->prog != prog) {
      LeaveProgram(scr, scr->prog); /* leave the failed program! */
      GetProgram(scr, prog); /* fetch the previous program again! */
    }
    return ret;
  }
  inttags[1]=val->val.val;
  FREE(val);

  CALL(LeaveProgram(scr, scr->prog));
  scr->prog=oldprog;
  CALL(GetProgram(scr, scr->prog));
  prog->program = NULL; /* we don't actually have a program ;) */
  DelProgram(scr, prog); /* remove it again please */

  scr->prg=prg;
  scr->text=text;

  scr->virprg=virprg;
  scr->virfile=virfile;
  CALL(Send(arg->key, inttags));
}
#endif

/**********************************************************************
 *
 * int functions(struct fplArgument *);
 *
 * This function handles the internal functions. *EXACTLY* the same way
 * external processes handles their functions!!! :-)
 *
 *****/

ReturnCode REGARGS
functions(struct fplArgument *arg)
{
  struct Expr *val;
  unsigned long inttags[]={FPLSEND_INT, 0, FPLSEND_DONE};
  unsigned long strtags[]={FPLSEND_STRING, 0, FPLSEND_STRLEN, 0, FPLSEND_DONE};
  long base;
  ReturnCode ret;
  struct Data *scr=(struct Data *)arg->key;
  struct fplStr *string;
  long prg;
  long line;
  long virprg;
  uchar *virfile;
  uchar *text;

  long len;		/* length of the string */
  register long col;	/* the column parameter */
  switch(arg->ID) {
    
  case FNC_ABS:
    inttags[1]= ABS((long)arg->argv[0]);
    CALL(Send(arg->key, inttags));
    break;

  case FNC_ITOC:
    prg=(long)arg->argv[0]&255;
    text=(uchar *)&line; /* we just need 2 bytes to play with in peace! */
    text[1]='\0';
    text[0]=(uchar)prg;
    strtags[1]=(long)text;
    strtags[3]=1;
    CALL(Send(scr, strtags));
    break;
    
  case FNC_JOINSTR:
    string=NULL;
    for(prg=0; prg<arg->argc; prg++) {
      CALL(StrAssign((struct fplStr *) ((uchar *)arg->argv[prg]-
					offsetof(struct fplStr, string)),
		     scr, &string, TRUE));
    }
    if(string) {
      strtags[1]=(unsigned long)string->string;
      strtags[3]=string->len;
      CALL(Send(scr, strtags));
      FREE(string);
    }
    break;

  case FNC_ITOA:
  case FNC_LTOSTR:
    base=(arg->argc<2?10:(long)arg->argv[1]);
    CALL(Ltostr(scr, &string, base, (long)arg->argv[0]));
    strtags[1]=(unsigned long)string->string;
    strtags[3]=string->len;
    CALL(Send(scr, strtags));
    FREE(string);
    break;
    
  case FNC_ATOI:
  case FNC_STRTOL:
    base=(arg->argc<2?10:(long)arg->argv[1]);
    inttags[1]= Strtol((uchar *)arg->argv[0], base, &text);
    CALL(Send(scr, inttags));
    break;
    
  case FNC_EVAL:
    prg=scr->prg;
    text=scr->text;
    line=scr->prog->lines;
    virprg=scr->virprg;
    virfile=scr->virfile;

    scr->virprg=1;
    scr->virfile=NULL;
    scr->text=(uchar *)arg->argv[0];
    scr->prg=scr->prog->lines=1;
 
    GETMEM(val, sizeof(struct Expr));
    CALL(Expression(val, scr, CON_GROUNDLVL|CON_END|CON_NUM, NULL));
    inttags[1]=val->val.val;
    FREE(val);

    scr->prg=prg;
    scr->text=text;
    scr->prog->lines=line;
    scr->virprg=virprg;
    scr->virfile=virfile;
    
    CALL(Send(scr, inttags));
    break;
    
  case FNC_INTERPRET:
    prg=scr->prg;
    text=scr->text;
    line=scr->prog->lines;
/*    file=scr->prog->name; */
    virprg=scr->virprg;
    virfile=scr->virfile;
    scr->virprg=1;
    scr->virfile=NULL;
    scr->interpret=NULL; /* nothing recursive here, no no! */
    scr->prg=1;
    scr->text=(uchar *)arg->argv[0];
    scr->prog->lines=1;
/*    scr->prog->name=NULL; */ /* we have no file name! */

    GETMEM(val, sizeof(struct Expr));
    ret=Script(scr, val, SCR_NORMAL, NULL);
    inttags[1]=val->val.val;
    FREE(val);

    if(ret) {
      /*
       * Check if the error occurred somewhere in the real program
       * or if it was within the argument. If within argument, we
       * set back the previous program pointer, otherwise not.
       */
      for(base=0;base<line;base++)
	if(scr->text>(&scr->prog->program)[base] &&
	   scr->text<((&scr->prog->program)[base]+
		      strlen((&scr->prog->program)[base])))
	  break;
      if(base==line) {
	scr->prg=prg;
	scr->text=text;
	scr->prog->lines=line;
/*	scr->prog->name=file; */
      }
      return(ret);
    }
    scr->prg=prg;
    scr->text=text;
    scr->prog->lines=line;
/*    scr->prog->name=file; */
    scr->virprg=virprg;
    scr->virfile=virfile;
    CALL(Send(arg->key, inttags));
    break;
#if 0
  case FNC_INTERPRET:
    CALL( Interpret(scr, arg, val) );
    break;
#endif
  case FNC_RENAME:
    {
      struct Identifier *ident;
      GetIdentifier(scr, (uchar *)arg->argv[0], &ident);
      if(!ident || ident->flags&FPL_KEYWORD) {
        /* not found or found keyword, fail! */
        inttags[1] = FPLERR_IDENTIFIER_NOT_FOUND;
      }
      else {
        if(((char *)arg->argv[1])[0])
          inttags[1] = RenameIdentifier(scr, ident, arg->argv[1]);
        else
          inttags[1] = DelIdentifier(scr, NULL, ident);
      }
      CALL(Send(arg->key, inttags));
    }    
  break;

  case FNC_STRCMP:
  case FNC_STRICMP:
    /*
     * strcmp() with strings that can include a zero byte must use
     * memcmp(), but that also takes a third length argument which
     * must never be larger than the smallest of the two compared
     * strings!
     */
    line = MIN(FPL_STRING_LENGTH(arg, 0), FPL_STRING_LENGTH(arg, 1)); /* len */

    if(FNC_STRCMP == arg->ID)
      base = memcmp(arg->argv[0], arg->argv[1], line);
    else
      base = my_memicmp(arg->argv[0], arg->argv[1], line);

    if(!base && FPL_STRING_LENGTH(arg, 0) != FPL_STRING_LENGTH(arg, 1)) {
      /* similar strings after 'line' characters */

      /*
       * The strings are of different length.
       */

      base = ((uchar *)arg->argv[0])[line] -
	(FPL_STRING_LENGTH(arg, 1)>line?
	 ((uchar *)arg->argv[1])[line] : 0 );
      
      if(!base) {
	/* only possible since FPL strings can hold zeroes! */
	base = 256; /* not possible in regular C */
      }
    }
    inttags[1]=base;
    CALL(Send(scr, inttags));
    break;
    
  case FNC_SUBSTR:
    len=FPL_STRING_LENGTH(arg, 0);
    col=(long)arg->argv[1];
    if(col>len || col<0) {
      ;				/* we can't get any string! */
    } else {
      len-=col;			/* Maximum length we can get */
      strtags[3]=((long)arg->argv[2]>len?len:(long)arg->argv[2]); /* strlen */
      strtags[1]=(long) arg->argv[0]+col; /* return string from here */
      CALL(Send(scr, strtags));
    }
    break;
    
  case FNC_STRLEN:
    inttags[1]=FPL_STRING_LENGTH(arg, 0);
    CALL(Send(scr, inttags));
    break;

  case FNC_STRNCMP:
  case FNC_STRNICMP:
    /*
     * strncmp() with strings that can include a zero byte must use
     * memcmp(), that also takes a third length argument which
     * must never be larger than the smallest of the two compared
     * strings or the number specified!
     */
    if(FNC_STRNCMP == arg->ID) {
      inttags[1]=
	memcmp(arg->argv[0], arg->argv[1],
	       MIN3((long)arg->argv[2],
		    FPL_STRING_LENGTH(arg, 0), FPL_STRING_LENGTH(arg, 1)));
    }
    else {
      inttags[1]=
	my_memicmp(arg->argv[0], arg->argv[1],
		MIN3((long)arg->argv[2],
		     FPL_STRING_LENGTH(arg, 0), FPL_STRING_LENGTH(arg, 1)));
    }
    CALL(Send(scr, inttags));
    break;
    
  case FNC_STRSTR:
  case FNC_STRISTR:
    /*
     * strstr() should compare two memory regions, like a memmem()!
     * Code an own!
     */
    base = FPL_STRLEN(arg->argv[0]);
    line = FPL_STRLEN(arg->argv[1]);
    text = (uchar *)arg->argv[0];

    /*
     * Addition from FPL version 9:
     * starting search column in third parameter!
     */
    if(arg->argc>2) {
      if((int)arg->argv[2] < base) {
	text+=(int)arg->argv[2];
	base-=(int)arg->argv[2];
      }
      else {
	/* tried to start searching outside the string! */

	line = 1; /* to make a not-found return code */
	base = 0;
      }
    }
    
    if(line && base) {
      if(FNC_STRSTR == arg->ID) {
	/* Case sensitive */
	while(base-->=line) {
	  if(!memcmp(text, (uchar *)arg->argv[1], line)) {
	    line=0;
	    break;
	  }
	  text++;
	}
      }
      else {
	/* Case insensitive */
	while(base-->=line) {
	  if(!my_memicmp(text, (uchar *)arg->argv[1], line)) {
	    line=0;
	    break;
	  }
	  text++;
	}
      }
    }
    inttags[1]=line?-1:text-(uchar *)arg->argv[0];

    /* OLD ONE:
    text=(uchar *)strstr((uchar *)arg->argv[0], (uchar *)arg->argv[1]);
    inttags[1]=text?text-(uchar *)arg->argv[0]:-1;
    */
    CALL(Send(scr, inttags));
    break;

  case FNC_SPRINTF:
    {
        static unsigned long tags[]={
            FPLSEND_STRING, 0,
            FPLSEND_STRLEN, 0,
            FPLSEND_DONTCOPY_STRING, TRUE,
            FPLSEND_DONE
        };
        string = NULL;
        CALL(Sprintf(scr, &string, arg->argv[0], arg->argv, arg->format,
		     arg->argc));
        if(string) {
          tags[1] = (long)string->string;
          tags[3] = string->len;
          CALL(Send(scr, tags));
        }
    }
    break;

  case FNC_SSCANF:
    {
      inttags[1] =
	  Sscanf(scr,
		 (uchar *)arg->argv[0],
		 (uchar *)arg->argv[1],
		 arg->argc,
		 arg->argv,
		 arg->format);
      CALL(Send(scr, inttags));
    }
    break;
  case FNC_EXISTS:
    inttags[1] =  Exists(scr,
			 (uchar *)arg->argv[0],
			 arg->argc>1?(long)arg->argv[1]:0);
    CALL( Send(scr, inttags) );
    break;

#if defined(AMIGA) && defined(SHARED)
  case FNC_OPENLIB:
    CALL(OpenLib(scr,
                 (uchar *)arg->argv[0], /* name */
                 (long)arg->argv[1],   /* version */
                 (long *)&inttags[1],  /* funclib result */
                 0));                  /* normal 'soft' open */
    CALL(Send(scr, inttags));
    break;

  case FNC_CLOSELIB:
    CALL(CloseLib(scr,
                  (uchar *)arg->argv[0],  /* name */
                  0,                     /* 'soft' close */
                  (long *)&inttags[1])); /* funclib result */
    CALL(Send(scr, inttags));
    break;

  case FNC_DEBUG:
    if(arg->argc) {
      if(!(int)arg->argv[0]) {
        scr->flags&=~FPLDATA_DEBUG_MODE; /* switch off debug mode */
#ifdef DEBUGMAIL
        DebugMail(scr, MAIL_EXIT, 500, NULL);
#endif
      }
      else {
        scr->flags|=FPLDATA_DEBUG_MODE;  /* switch on debug mode */
#ifdef DEBUGMAIL
        DebugMail(scr, MAIL_START, 500, NULL);
#endif
      }
    }
    else {
       inttags[1]=scr->flags&FPLDATA_DEBUG_MODE?1:0; /* return status */
       CALL(Send(scr, inttags));
    }
    break;
#endif
  }
  return(FPL_OK);
}

#if defined(AMIGA) && defined(SHARED)
ReturnCode REGARGS
OpenLib(struct Data *scr,
        uchar *lib,        /* funclib name */
        long version,     /* funclib version */
        long *retvalue,   /* funclib return code */
        uchar flags)
{
   struct MyLibrary *library;
   struct Library *DOSBase;
   BPTR seglist;
   uchar *command;
   uchar *cmd;
   struct FuncList *namelist=scr->funclibs;
   uchar *name;
   ReturnCode ret;
   struct fplStr *string;

   struct ExecBase *SysBase = *(struct ExecBase **)4;

   library = (struct MyLibrary *)getreg(REG_A6);
   DOSBase = library->ml_DosBase;

   GETMEM(command, FPLLIB_MAXSPACE);

   while(namelist) {
     if(!strcmp(namelist->name, lib)) {
       namelist->opens++;
       return FPL_OK; /* this funclib is already opened */
     }
     namelist = namelist->next;
   }

   cmd = command;
   strcpy(command, FPLLIB_SOURCE);
   strcpy(command+strlen(FPLLIB_SOURCE), lib);
   seglist = LoadSeg(command); /* load the command! */
   if(seglist) {
     strcpy(command, FPLLIB_OPENCMD);
     command += strlen(FPLLIB_OPENCMD);

     CALL(Ltostr(scr, &string, 10, (long)scr));
     strcpy(command, string->string);
     command[string->len]= ' '; /* pad with a single space */
     command+=string->len+1;
     FREE(string);

     CALL(Ltostr(scr, &string, 10, version));
     strcpy(command, string->string);
     command[string->len]= '\n';   /* add newline */
     command[string->len+1]= '\0'; /* zero terminate */
     FREE(string);

     if(SysBase->SoftVer<36) {
       /* V33 solution! */
       uchar *segment = BADDR(seglist);
       int (*func)();
#pragma msg 147 ignore
       func = segment + 4; /* generates warning */
#pragma msg 147 warning

       putreg(REG_A0, (long)cmd);
       putreg(REG_D0, strlen(cmd));
#pragma msg 154 ignore
       *retvalue = (func)(); /* generates warning */
#pragma msg 154 warning
     } else /* version 36 or up! */
       *retvalue = RunCommand(seglist, 4000, cmd, strlen(cmd));

     UnLoadSeg( seglist );
   } else {
     /* we failed loading the command! */
     *retvalue = FUNCLIB_LOAD;
   }

   FREE(cmd);

   if(!*retvalue) {
      GETMEMA(namelist, sizeof(struct FuncList));
      STRDUPA(name, lib);
      namelist->name = name;
      namelist->opens = 1;
      namelist->flags = flags;
      namelist->next = scr->funclibs;
      scr->funclibs = namelist;
   }
   return FPL_OK;
}

ReturnCode REGARGS
CloseLib(struct Data *scr,
         uchar *lib,        /* funclib name or NULL for all */
         long flags,       /* options */
         long *retvalue)   /* funclib return code */
{
   struct MyLibrary *library;
   struct Library *DOSBase;
   struct FuncList *namelist=scr->funclibs;
   struct FuncList *prevlist=NULL;
   struct FuncList *next;
   uchar *command;
   uchar *cmd;
   ReturnCode ret;
   struct fplStr *string;
   BPTR seglist;

   struct ExecBase *SysBase = *(struct ExecBase **)4;

   library = (struct MyLibrary *)getreg(REG_A6);
   DOSBase = library->ml_DosBase;

   GETMEM(command, FPLLIB_MAXSPACE);
   cmd = command;

   while(namelist) {
     if(namelist->flags&FPLLIB_KEEP && namelist->opens==1) {
       /* This funclib is prevented from being 'soft' closed! */
       namelist->opens++;
     }
     if((!lib || !strcmp(namelist->name, lib)) &&
        (!--namelist->opens || flags&FPLLIB_FORCE) ) {
       /* the funclib _is_ opened! */

       strcpy(command, FPLLIB_SOURCE);
       strcpy(command+strlen(FPLLIB_SOURCE), lib);
       seglist = LoadSeg(command); /* load the command! */
       if(seglist) {
         strcpy(command, FPLLIB_CLOSECMD);
         command += strlen(FPLLIB_CLOSECMD);
    
         CALL(Ltostr(scr, &string, 10, (long)scr));
         strcpy(command, string->string);
         command[string->len]= '\n';   /* add newline */
         command[string->len+1]= '\0'; /* zero terminate */
         FREE(string);
    
         if(SysBase->SoftVer<36) {
           /* V33 solution! */
           uchar *segment = BADDR(seglist);
           int (*func)();
#pragma msg 147 ignore
           func = segment + 4; /* generates warning */
#pragma msg 147 warning
    
           putreg(REG_A0, (long)cmd);
           putreg(REG_D0, strlen(cmd));
#pragma msg 154 ignore
           *retvalue = (func)(); /* generates warning */
#pragma msg 154 warning
         } else /* version 36 or up! */
           *retvalue = RunCommand(seglist, 4000, cmd, strlen(cmd));
    
         UnLoadSeg( seglist );
       } else {
         /* we failed loading the command! */
         *retvalue = FUNCLIB_LOAD;
       }
    
    
       if(!*retvalue) {
         next = namelist->next;
         if(prevlist) /* was there a previous funclib in the list? */
           prevlist->next=next; /* point it to the next in the list */
         else
           scr->funclibs = next; /* point the origin to the next */
         FREEA(namelist->name); /* free name space */
         FREEA(namelist);       /* free struct */
         namelist = next;
         continue;
       }
     }
     prevlist = namelist;
     namelist = namelist->next;
   }

   FREE(cmd);

   return FPL_OK;
}

#endif

/**********************************************************************
 *
 * fplLtostr()
 *
 * Frontend to the FPL Ltostr() function to be used by anyone! The returned
 * string *must* be freed using the fplFreeString() function!
 *
 ****/

PREFIX uchar *
fplLtostr(AREG(0) struct Data *scr,
          DREG(0) long base,
          DREG(1) long num)
{
  ReturnCode ret;
  struct fplStr *string;
  uchar *retstring=NULL;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, (void *)"fplLtostr");
#endif
  ret = Ltostr(scr, &string, base, num);
  if(FPL_OK == ret) {
    SwapMem(scr, string, MALLOC_STATIC); /* turn allocation to static type */
    retstring = string->string; /* return string pointer */
  }
  return retstring;
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
  ReturnCode ret;
  static const uchar *digits = upper_digits;
  long is_neg=num<0;
  long len=0;
  uchar buffer[34+sizeof(struct fplStr)];
  uchar *bufpoint;  /* the accurate position in the buffer */

  if(base>(long)strlen(digits)) {
    CALL(Warn(scr, FPLERR_OUT_OF_REACH));
    num=strlen(digits); /* reset to maximum */
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
 * fplStrtol()
 *
 * Frontend to the FPL Strtol() function to be used by anyone!
 *
 ****/

long PREFIX
fplStrtol(AREG(0) uchar *string,
          DREG(0) long base,
          AREG(1) uchar **end)
{
  uchar *dummypoint;
  if(!end)
    end = &dummypoint;
  /*
   * THIS CAN'T CURRENTLY CALL DEBUGMAIL SINCE NO struct Data IS USED!!!
   */
  return Strtol(string, base, end);
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
    if ((unsigned long)i > cutoff ||
		((unsigned long)i == cutoff && c > cutlim))
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
	   - (long ) LONG_MIN :
	   (long ) LONG_MAX))
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

long REGARGS my_memicmp(uchar *s1, uchar *s2, long len)
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
  struct fplStr *whole;

  CALL(Eat(scr)); /* scan white spaces */

  if(CHAR_PLUS == scr->text[0]) {
    
    GETMEM(whole, sizeof(struct fplStr));
    memset(whole, 0, sizeof(struct fplStr));
    
    /* put string in new string variable */
    CALL(StrAssign(val->val.str, scr, &whole, 1));
    
    do {
      scr->text++; /* skip the '+' */
      
      CALL(Expression(val, scr, CON_STRING, NULL));
      
      /* append string to that new variable */
      CALL(StrAssign(val->val.str, scr, &whole, 1));
      
      if(!(val->flags&FPL_NOFREE) && val->val.str)
	FREE(val->val.str);
    } while(CHAR_PLUS == scr->text[0]);
      
    val->val.str = whole; /* get the string info! */
    val->flags&=~FPL_NOFREE; /* free this, yes! */
  }
  return FPL_OK;
}
