/******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************

 script.c

 The main routine of the language. Handles all keywords, {'s and }'s.

 *****************************************************************************/

/************************************************************************
 *                                                                      *
 * fpl.library - A shared library interpreting script langauge.         *
 * Copyright (C) 1992-1997 FrexxWare                                    *
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
 * S-171 67 Solna                                                       *
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

#elif defined(UNIX) || defined(WIN32)
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#endif

#include <stdio.h>
#include <string.h>
#include "script.h"

#ifdef DEBUG
long mem=0;
long maxmem=0;
#endif

static ReturnCode INLINE Declare(struct Expr *, struct Data *,
				 struct Identifier *, long);
static ReturnCode REGARGS Loop(struct Data *, short, uchar *);
static ReturnCode INLINE Resize(struct Data *, struct Expr *, short);
static ReturnCode INLINE Switch(struct Data *, struct Expr *, short);
/*
 * Global character flags:
 */

const uchar type[257] = { /* Character type codes */
   _C, /* -1 == regular ANSI C eof character */
   _C,    _C,	  _C,	 _C,    _C,    _C,    _C,    _C, /* 00		*/
   _C,    _S,	  _S,	 _C,    _C,    _S,    _C,    _C, /* 08		*/
   _C,    _C,	  _C,	 _C,    _C,    _C,    _C,    _C, /* 10		*/
   _C,    _C,	  _C,	 _C,    _C,    _C,    _C,    _C, /* 18		*/
   _S,    _P,     _P,	 _P,    _P,    _P,    _P,    _P, /* 20	!"#$%&' */
   _P,    _P,     _P,    _P,    _P,    _P,    _P,    _P, /* 28 ()*+,-./ */
 _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, /* 30 01234567 */
 _N|_X, _N|_X,    _P,    _P,    _P,    _P,    _P,    _P, /* 38 89:;<=>? */
   _P, _U|_X,  _U|_X, _U|_X, _U|_X, _U|_X, _U|_X,    _U, /* 40 @ABCDEFG */
   _U,    _U,	  _U,	 _U,    _U,    _U,    _U,    _U, /* 48 HIJKLMNO */
   _U,    _U,	  _U,	 _U,    _U,    _U,    _U,    _U, /* 50 PQRSTUVW */
   _U,    _U,	  _U,	 _P,    _P,    _P,    _P, _P|_W, /* 58 XYZ[\]^_ */
   _P, _L|_X,  _L|_X, _L|_X, _L|_X, _L|_X, _L|_X,    _L, /* 60 `abcdefg */
   _L,    _L,	  _L,	 _L,    _L,    _L,    _L,    _L, /* 68 hijklmno */
   _L,    _L,	  _L,	 _L,    _L,    _L,    _L,    _L, /* 70 pqrstuvw */
   _L,    _L,	  _L,	 _P,    _P,    _P,    _P,   000, /* 78 xyz{|}~	*/
  000,   000,	 000,	000,   000,   000,   000,   000, /* 80 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* 88 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* 90 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* 98 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* A0 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* A8 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* B0 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* B8 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* C0 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* C8 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* D0 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* D8 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* E0 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* E8 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* F0 	        */
  000,   000,	 000,	000,   000,   000,   000,   000, /* F8 	        */
};

/**************************************************************************
 *
 * ReadFile()
 *
 *   Reads the specified file into memory, stores the pointer to the memory
 * area in the pointer `program' points to, and the size of the memory area
 * in the integer `size' points to. I decided to use a different way on Amiga
 * to increase performance a lot.
 *
 *   This function first checks the size of the file it's about to fetch
 * and then reads the entire file at once in one continuos memory area.
 *
 *   Returns the proper return code. If anything goes wrong, there won't be
 * *ANY* program to look at (the pointer will be NULL, but the size will most
 * probably still be correct which means a non-zero value). If this function
 * fails it takes care of freeing the program memory by itself. You only have
 * to free that memory if this functions reports success.
 *
 ********/

ReturnCode REGARGS
ReadFile(void *fpl,
         uchar *filename)
{
  ReturnCode ret=FPL_OK;
#if 1
  struct Data *scr=(struct Data *)fpl;
#ifdef AMIGA  /* Amiga version. */
  struct FileInfoBlock fileinfo;
  struct FileLock *lock=NULL;
  struct FileHandle *fileread;

#elif defined(UNIX) || defined(WIN32)
  FILE *stream;
#endif
#ifdef AMIGA

  if(filename && filename[0])
    /* Lock on file */
    lock=(struct FileLock *)Lock((UBYTE *)filename, ACCESS_READ);
  if (lock) {
    if (Examine((BPTR)lock, &fileinfo) && fileinfo.fib_Size) {
      /*
       * Only do this if the file was there, and it was larger than zero
       * bytes!
       */
      scr->size = fileinfo.fib_Size+1; /* Add one for a terminating zero! */
    } else
      ret=FPLERR_OPEN_ERROR;	/* something went wrong */
    UnLock((BPTR)lock);	/* release the lock of the file */
  } else
    ret=FPLERR_OPEN_ERROR;		/* we couldn't lock on the file */
#elif defined(UNIX) || defined(WIN32)
  if (!(stream = fopen(filename, "r")))
    ret=FPLERR_OPEN_ERROR;
  else {
    if(fseek(stream, 0, 2)) {
      fclose(stream);
      ret=FPLERR_OPEN_ERROR;
    } else {
      scr->size=ftell(stream)+1;
      fseek(stream, 0, 0);
    }
  }
#endif

  if(ret)
    return(ret);

  /* Open file for reading. */
#ifdef AMIGA
  /* We could use OpenFromLock() here, but it's a V36+ function! */
  fileread=(struct FileHandle *)Open((UBYTE *)filename, MODE_OLDFILE);
#endif
  scr->program=(uchar *)MALLOC(scr->size); /* Allocate memory for program. */
  if(!scr->program) /* if we didn't get the requested memory: */
    ret=FPLERR_OUT_OF_MEMORY;
#ifdef AMIGA
  else if(Read((BPTR)fileread, scr->program, (LONG)scr->size)<0) /* get entire file */
#elif defined(UNIX) || defined(WIN32)
  else if(!fread(scr->program, 1, scr->size, stream))
#endif
    /* if we couldn't Read() the file: */
    ret=FPLERR_OPEN_ERROR;
  else
    scr->program[scr->size-1]='\0'; /* add the terminating zero byte. */
#ifdef AMIGA
  Close((BPTR)fileread); /* close file */
#elif defined(UNIX) || defined(WIN32)
  fclose(stream); /* close the stream */
#endif
  /* only if error and we could allocate the proper memory */
  if(ret && scr->program) {
    FREE(scr->program); /* free the, for the program allocated, memory */
  }
#else
  scr->mainbuf[0]=0; /* zero terminate the buffer */
  scr->fileread = fopen(filename, "r");
  if(!scr->fileread)
    ret=FPLERR_OPEN_ERROR;;
#endif
  return ret; /* get back to parent */
}

/***************************************************************************
 *
 * fplExecuteFile()
 *
 * Executes the specified file as an FPL program.
 *
 ******/

ReturnCode REGARGS fplExecuteFile(struct Data *scr,
				  uchar *filename)
{
  ReturnCode end, ret;
  struct Expr *val;

  scr->varlevel =0; /* start at locale level 0 */

  STRDUP(scr->virfile, filename); /* allocate this */
  scr->filename = filename; /* original and startup name */

  end = ReadFile(scr, filename); /* get file */
  if(end>FPL_EXIT_OK) {
    scr->virfile = filename;
    scr->text=NULL;
    scr->virprg = 0;
    INFO(scr, CERROR_FILE_NOT_FOUND, filename);
    return FPLERR_OPEN_ERROR;
  }

  scr->prg=1;
  scr->text=scr->program;

  /* fprintf(stderr, "Exp:%s", scr->text); */

  scr->ret=FPL_OK;                /* return code reset */
  scr->virprg=1;
  scr->level=0;                   /* level counter */
  scr->strret=FALSE;              /* we don't want no string back! */
  scr->locals=NULL;               /* local symbol list */
  scr->globals=NULL;              /* global symbol list */
  scr->FPLret=0;                  /* initialize return code value */

  val= MALLOC(sizeof(struct Expr));
  if(val) {
    CALL(Eat(scr));
    CALL(HashScan(scr));
    end=Script(scr, val,
               SCR_FUNCTION| /* return on return() */
               SCR_FILE|     /* this level may end with '\0' */
               SCR_GLOBAL);  /* global symbol declarations enabled */
    FREE(val);
  } else
    end=FPLERR_OUT_OF_MEMORY;

  if(end>FPL_EXIT_OK) {
    if(!scr->newerror) {
      char buffer[FPL_ERRORMSG_LENGTH];
      /* We'll fix the error string! */
      GetErrorMsg(scr, end, buffer);
  
      printf("*** BEEP! Report this occurence to the authors!!\n"
             "OLD STYLE ERROR MESSAGE:\n"
             "%s!\n"
             "Line %d in file %s!\n",
             buffer, scr->virprg, scr->virfile);
    }
  }
  else {
    if(scr->cmdline[LINE_VERBOSE]) {
      fprintf(stderr,
              " === Symbols used: ===\n"
              "Max concurrent amount: %d\n"
              "Total amount in file: %d\n",
              scr->maxvariables, scr->totalvariables);
    }
  }

  /*
   * Go through the ENTIRE locals list and delete all. Otherwise they will
   * ruin the symbol table.
   */

  while(scr->locals)
    DelLocalVar(scr, &scr->locals);

 /*
  * We must delete the global symbol lists
  * properly and not just free the memory. Otherwise we might free memory
  * used in the middle of the list we intend to save for next run!
  */
  while(scr->globals)
    /* There is some global symbols to delete! */
    DelLocalVar(scr, &scr->globals);

  /*
   * Check if this program was stored in memory earlier (in
   * another run). If not ...
   */
  FREE(scr->program);
  FREE(scr->virfile);

  return(end==FPL_EXIT_OK?FPL_OK:end);
}

/**************************************************************************
 *
 * int Script(struct Data *);
 *
 * Interprets an FPL program, very recursive. Returns progress in an integer,
 * and the FPL program result code in the int scr->ret.
 * USE AS FEW VARIABLES AS POSSIBLE to spare stack usage!
 *
 **********/

ReturnCode ASM
Script(AREG(2) struct Data *scr,  /* big FPL structure */
       AREG(3) struct Expr *val,  /* result structure  */
       DREG(2) short control)     /* control byte */
{
  /* declaration allowed? */
  uchar declare=control&(SCR_BRACE|SCR_FUNCTION)?1:0;
  ReturnCode ret; /* return value variable */
  uchar brace=0; /* general TRUE/FALSE variable */
  uchar *text; /* position storage variable */
  long prg;   /* position storage variable */
  long levels=scr->level; /* previous level spectra */
  struct Identifier *ident; /* used when checking keywords */
  uchar done=FALSE; /* TRUE when MAIN_END has been output */

  uchar aborted=FALSE;  /* set true if the statement is returned,  broken
			   or continued */
  uchar mainstart=FALSE;
			   
#define ABORT_RETURN (1<<0)
#define ABORT_BREAK  (1<<1)
#define ABORT_CONT   (1<<2)

  if(control&(SCR_BRACE|SCR_FUNCTION)) {
    /*
     * New symbol declaration level!
     */
    scr->varlevel++;
    CALL(AddLevel(scr));
  }

  if(control&SCR_FUNCTION)
    scr->level=0; /* number of levels to look for variables */
  else if(control&SCR_BRACE)
    scr->level++;

  do {
    if(ret=Eat(scr)) {
      if(control&SCR_FILE && ret==FPLERR_UNEXPECTED_END)
	/* It's OK! */
	ret=FPL_OK;
      break;
    }

    /*
     * Time to parse the statement!
     */

    text=scr->text;                /* store current position */
    prg=scr->prg;
    if(!Getword(scr)) {    /* get next word */
      GetIdentifier(scr, scr->buf, &ident);
    }
    else {
      prg=-1;    /* we have not read a word! */
      ident=NULL;
    }

    if(declare && (!ident || !(ident->flags&FPL_KEYWORD_DECLARE))) {
      declare=FALSE;
      if(control&SCR_GLOBAL) {
        /* still on ground level and declaration allowed */
        CALL(Put(scr, COMP_MAIN_START));
        mainstart=TRUE;
      }
      else {
        CALL(Put(scr, COMP_END_OF_DECLARE));
      }
    }

    switch(*text) {
    case CHAR_OPEN_BRACE:		/* open brace */
      if(aborted) {
	INFO(scr, CERROR_CANT_REACH_STATEMENT);
	aborted=FALSE; /* no more messages */
      }
      scr->text++;
      CALL(Put(scr, COMP_OPEN_BRACE));
      CALL(Script(scr, val, SCR_NORMAL|SCR_BRACE));
      break;

    case CHAR_CLOSE_BRACE:
      if(control&(SCR_BRACE|SCR_SWITCH)) {
        scr->text++;
        if(control&SCR_DO) {
          CALL(Loop(scr, control, &brace));
        }
	if(control&SCR_SWITCH) {
	  scr->text--; /* back on brace! */
        }
	else {
	  /*
	   * Only do this if not switch(), since switch() doesn't
	   * force a new variable level when it calls this function!
	   */
	  CleanUp(scr, control, levels);
	}
	if (!(control&SCR_DO)) {
          CALL(Put(scr, COMP_CLOSE_BRACE));
        }
        return(FPL_OK);  /* return to calling function */
      }
      INFO(scr, CERROR_ILLEGAL_BRACE, CHAR_CLOSE_BRACE);
      return FPLERR_SYNTAX_ERROR;

    case CHAR_SEMICOLON:
      if(aborted) {
	INFO(scr, CERROR_CANT_REACH_STATEMENT);
	aborted=FALSE; /* no more messages */
      }
      scr->text++;
      break;

    default:
      if(control&SCR_SWITCH) {
	if(ident &&
           (ident->data.external.ID == CMD_CASE ||
	   ident->data.external.ID == CMD_DEFAULT))
	  aborted = FALSE; /* new session at each 'label' */
      }
      if(aborted) {
	if(!(control&SCR_GLOBAL)) {
	  INFO(scr, CERROR_CANT_REACH_STATEMENT);
	  aborted=FALSE; /* no more messages */
	}
	else {
	  CALL(Put(scr, COMP_MAIN_END)); /* end of main */
	  declare = TRUE; /* allow declarations again */
	  aborted = FALSE; /* switch off abort-mode! */
	  done = TRUE; /* yes, we have written MAIN_END */
	}
      }
      if(ident && ident->flags&FPL_KEYWORD) {
	if(ident->flags&FPL_KEYWORD_DECLARE) {
	  if(!declare) {
	    INFO(scr, CERROR_ILLEGAL_DECLARATION);
	    return FPLERR_ILLEGAL_DECLARE;
	  }
	  if(ident->data.external.ID == CMD_TYPEDEF) {
            /*
             * Add as a declarator in the list.
             */
	    CALL(Getword(scr));
	    CALL(GetIdentifier(scr, scr->buf, &ident));
	    if(!ret &&
	       (ident->data.external.ID==CMD_INT ||
		ident->data.external.ID==CMD_STRING)) {
	      CALL(Getword(scr));
	      text=(void *)ident;
	      GETMEM(ident, sizeof(struct Identifier));
	      *ident=*(struct Identifier *)text; /* copy entire structure! */
	      STRDUP(ident->name, scr->buf);
	      ident->flags&=~FPL_INTERNAL_FUNCTION; /* no longer any internal
						       declarator symbol! */
	      CALL(AddVar(scr, ident, &scr->locals, FALSE));
	    }
	    else {
	      INFO(scr, CERROR_IDENTIFIER_NOT_FOUND, scr->buf);
	      return FPLERR_IDENTIFIER_NOT_FOUND;
	    }
	    if(*scr->text!=CHAR_SEMICOLON) {
	      INFO(scr, CERROR_MISSING_SEMICOLON);
            }
            else 
              ++scr->text;
	  }
	  else {
	    CALL(Declare(val, scr, ident, control&SCR_GLOBAL?CON_DECLGLOB:0));
	  }
	}
	else {
	  switch(ident->data.external.ID) {
          case CMD_ELSE:
            INFO(scr, CERROR_ELSE);
            return FPLERR_SYNTAX_ERROR;
          case CMD_SWITCH:
            CALL(Put(scr, COMP_SWITCH));
            CALL(Switch(scr, val, control));
            break;

          case CMD_CASE:    /* 'case' */
            if(!control&SCR_SWITCH) {
              INFO(scr, CERROR_ILLEGAL_XXX, ident->name);
              return FPLERR_ILLEGAL_CASE; /* 'case' not within switch! */
            }
            /*
             * This word can only be found if (control&SCR_SWITCH), and then
             * we must just skip the "case XX:" text and continue.
             */
            CALL(Eat(scr));
            CALL(Put(scr, COMP_CASE));
	    CALL(Put(scr, COMP_START_OF_EXPR));
	    CALL(Expression(val, scr,
			    CON_NORMAL|
			    (scr->switchtype==SWITCH_STR?CON_STRING:
			     CON_NUM),
			    NULL));
	    CALL(Put(scr, COMP_END_OF_EXPR));
	    if(*scr->text!=CHAR_COLON) {
	      INFO(scr, CERROR_MISSING_COLON);
	    }
	    else
	      ++scr->text;
            if(val->flags&FPL_STRING &&
	       !(val->flags&FPL_NOFREE) &&
	       val->val.str)
	      /* If there was a string return, it should be freed and the
		 string really held a string! */
              FREE(val->val.str);
            break;

          case CMD_DEFAULT: /* 'default' */
            if(!control&SCR_SWITCH) {
              INFO(scr, CERROR_ILLEGAL_XXX, ident->name);
              return FPLERR_ILLEGAL_DEFAULT; /* 'default' not within switch! */
            }
            /*
             * This word can only be found if (control&SCR_SWITCH), and then
             * we must just skip the "default:" text and continue.
             */
            CALL(Put(scr, COMP_DEFAULT));
	    CALL(Eat(scr));
            if(scr->text[0]!=CHAR_COLON) {
	      INFO(scr, CERROR_MISSING_COLON);
            } else
              ++scr->text;
            break;

	  case CMD_RETURN:
	  case CMD_EXIT:
	    Eat(scr);
	    if(ident->data.external.ID==CMD_RETURN) {
              CALL(Put(scr, COMP_RETURN));
            }
            else {
              CALL(Put(scr, COMP_EXIT));
            }

            CALL(Put(scr, COMP_START_OF_EXPR));
	    if(*scr->text!=CHAR_SEMICOLON) { /* no return */
	      brace=*scr->text==CHAR_OPEN_PAREN; /* not required! */
	      scr->text+=brace;

	      /*
	       * If return()ing from a function when scr->strret is TRUE,
	       * return a string.
	       */
	      if((scr->strret && ident->data.external.ID==CMD_RETURN) ||
                 (scr->string_return && ident->data.external.ID==CMD_EXIT)) {
		CALL(Expression(val, scr, CON_NORMAL, NULL));
		if(!(val->flags&FPL_STRING)) {
		  /* that wasn't a string! */
                  INFO(scr, CERROR_ILLEGAL_STRING);
		  return FPLERR_UNEXPECTED_INT_STATEMENT;
		} else {
		  /* It was a string! */
		  if(val->flags&FPL_NOFREE) {
		    /*
		     * We're only refering to another string! We can't
		     * allow that since that string might be a local
		     * variable, and all such are about to be deleted now!
		     */
		    struct fplStr *string;
                    if(val->val.str) {
                      /* did we really get a pointer? */
		      GETMEM(string, val->val.str->len+sizeof(struct fplStr));
		      memcpy(string,
			     val->val.str,
			     val->val.str->len+sizeof(struct fplStr));
		      string->alloc=val->val.str->len;
                    }
                    else {
                      GETMEM(string, sizeof(struct fplStr));
                      string->len = string->alloc = 0;
                    }
		    val->val.str=string;
		    val->flags&=~FPL_NOFREE;
		  }
		}

	      } else {
		CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
                scr->returnint = &scr->FPLret; /* point to result */
	      }
	      if(brace) {
                if(*scr->text!=CHAR_CLOSE_PAREN) {
                  INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
                }
                else
                  ++scr->text;
              }
	    } else {
	      val->val.val=0;
	      val->flags=0;
	    }
            CALL(Put(scr, COMP_END_OF_EXPR));
	    scr->FPLret=val->val.val;	/* set return code! */
            CALL(Eat(scr));
	    if(*scr->text!=CHAR_SEMICOLON) {
               INFO(scr, CERROR_MISSING_SEMICOLON);
            }
            else 
              ++scr->text;
	    aborted |= ABORT_RETURN;
	    break;
            
	  case CMD_IF:		/* if() */
	  case CMD_WHILE:	/* while() */
	    Eat(scr);

	    /*********************

	      PARSE CONDITION

	      *******************/

            if(ident->data.external.ID==CMD_IF) {
              CALL(Put(scr, COMP_IF));
            }
            else {
              CALL(Put(scr, COMP_WHILE));
            }
	    if(*scr->text!=CHAR_OPEN_PAREN)
              INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_OPEN_PAREN);
            else
              ++scr->text;

            CALL(Put(scr, COMP_START_OF_EXPR));
	    CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
            CALL(Put(scr, COMP_END_OF_EXPR));
	    if(*scr->text!=CHAR_CLOSE_PAREN)
              INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
            else
              ++scr->text;

	    /********************

	      PARSE STATMENT

	    ******************/

	    Eat(scr);
	    scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);

	    if(CMD_WHILE == ident->data.external.ID) {
	      scr->breaks++; /* yet another break level */
	      scr->conts++; /* yet another continue level */
	    }
            CALL(Put(scr, COMP_OPEN_BRACE));
	    CALL(Script(scr, val,
			(short)((brace?SCR_BRACE:0)|
			(ident->data.external.ID==CMD_WHILE?SCR_WHILE:SCR_IF) )));
            if(!brace) {
              CALL(Put(scr, COMP_CLOSE_BRACE));
            }
	    if(CMD_WHILE == ident->data.external.ID) {
	      scr->breaks--; /* step down a break level */
	      scr->conts--; /* step down a continue level */
	    }

            Eat(scr); /* we must eat space before storing the position,
                         otherwise we might eat newlines several times! */
            
	    text=scr->text;
	    prg=scr->prg;

	    Getword(scr);

	    if(!strcmp(KEYWORD_ELSE, scr->buf)) {
              CALL(Put(scr, COMP_ELSE));
              CALL(Eat(scr));
	      scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
              CALL(Put(scr, COMP_OPEN_BRACE));
              CALL(Script(scr, val, (short)(brace?SCR_BRACE:0)));
              if(!brace) {
                CALL(Put(scr, COMP_CLOSE_BRACE));
              }
	    }
            else {
              scr->text=text;
              scr->prg=prg;
            }
	    break;
	  case CMD_BREAK:
	    if(!scr->breaks) {
              INFO(scr, CERROR_ILLEGAL_XXX, ident->name);
	      return FPLERR_SYNTAX_ERROR;
            }
	    val->val.val=1;	/* default is break 1 */
	    val->flags=0;	/* reset flags */
	    CALL(Eat(scr));
            CALL(PutArg(scr, COMP_BREAK,
			scr->breaks)); /* maximum levels break allowed */
	    /*
	     * Check if break out of several statements.
	     */
            CALL(Put(scr, COMP_START_OF_EXPR));
	    if(*scr->text!=CHAR_SEMICOLON) {
	      /* Get the result of the expression. */
	      brace=(*scr->text==CHAR_OPEN_PAREN);
	      scr->text+=brace;
	      CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
	      if(brace) {
		if(*scr->text!=CHAR_CLOSE_PAREN) {
		  INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
                }
                else
                  ++scr->text;
              }
	    }
            CALL(Put(scr, COMP_END_OF_EXPR));
	    if(*scr->text!=CHAR_SEMICOLON) {
	      INFO(scr, CERROR_MISSING_SEMICOLON);
            }
            else
              ++scr->text;

	    val->flags|=FPL_BREAK;
	    aborted |= ABORT_BREAK; /* we're broken! */
            break;
            
	  case CMD_CONTINUE:
	    if(!scr->conts) {
              INFO(scr, CERROR_ILLEGAL_XXX, ident->name);
	      return FPLERR_ILLEGAL_CONTINUE;
            }
	    if(*scr->text!=CHAR_SEMICOLON) {
	      INFO(scr, CERROR_MISSING_SEMICOLON);
            }
            else
              ++scr->text;
            CALL(Put(scr, COMP_CONTINUE));
            val->flags=FPL_CONTINUE;
	    aborted |= ABORT_CONT;
            break;
	  case CMD_DO:
	    CALL(Eat(scr));
	    scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
	    ++scr->breaks; /* increase break level */
	    ++scr->conts; /* increase continue level */
            CALL(Put(scr, COMP_DO));
            CALL(Put(scr, COMP_OPEN_BRACE));
	    CALL(Script(scr, val, (short)(SCR_DO|(brace?SCR_BRACE:0)) ));
	    --scr->breaks; /* increase break level */
	    --scr->conts; /* increase continue level */
	    break;
	  case CMD_FOR:
	    CALL(Eat(scr));
	    if(*scr->text!=CHAR_OPEN_PAREN) 
              INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_OPEN_PAREN);
            else
              ++scr->text;

            CALL(Put(scr, COMP_FOR));

	    CALL(Eat(scr));

            CALL(Put(scr, COMP_START_OF_EXPR));
	    if(CHAR_SEMICOLON != *scr->text) {
	      CALL(Expression(val, scr, CON_GROUNDLVL| CON_ACTION, NULL));
	      if(*scr->text!=CHAR_SEMICOLON) 
		INFO(scr, CERROR_MISSING_SEMICOLON);
	      else
		++scr->text;
	    }
	    else
	      ++scr->text;
            CALL(Put(scr, COMP_END_OF_EXPR));

            CALL(Put(scr, COMP_START_OF_EXPR));
	    if(CHAR_SEMICOLON != *scr->text) {
	      CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
	      if(*scr->text!=CHAR_SEMICOLON) 
		INFO(scr, CERROR_MISSING_SEMICOLON);
	      else
		++scr->text;
	    }
	    else {
	      CALL(PutArg(scr, COMP_NUM_CONSTANT, 1)); /* store a constant 1 */
	      ++scr->text;
	    }
            CALL(Put(scr, COMP_END_OF_EXPR));

	    CALL(Eat(scr));
            CALL(Put(scr, COMP_START_OF_EXPR));
	    if(CHAR_CLOSE_PAREN != *scr->text) {
	      CALL(Expression(val, scr, CON_GROUNDLVL|CON_ACTION, NULL));
	      if(*scr->text!=CHAR_CLOSE_PAREN) 
		INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
	      else
		++scr->text;
	    }
	    else
	      ++scr->text;
            CALL(Put(scr, COMP_END_OF_EXPR));

	    CALL(Eat(scr));
	    scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
	    ++scr->breaks; /* increase break level */
	    ++scr->conts; /* increase continue level */
            CALL(Put(scr, COMP_OPEN_BRACE));
	    CALL(Script(scr, val, (short)((brace?SCR_BRACE:0)|SCR_FOR )));
            if(!brace) {
              CALL(Put(scr, COMP_CLOSE_BRACE));
            }
	    --scr->breaks; /* decrease break level */
	    --scr->conts; /* decrease continue level */
	    break;
	  case CMD_RESIZE:
            CALL(Put(scr, COMP_RESIZE));
	    CALL(Resize(scr, val, control));
	    break;
	  } /* switch(keyword) */
        } /* if it wasn't a declaring keyword */
      }
      else {
	CALL(Expression(val, scr, CON_ACTION|(prg>=0?CON_IDENT:0), ident));
	/*
	 * It returned a string, flush it!
	 */
	if(val->flags&FPL_STRING && !(val->flags&FPL_NOFREE) && val->val.str) {
	  /* If there was a string return, it should be freed and the
	     string really held a string! */
	  FREE(val->val.str);
	}
	/*
	 * Check for semicolon!
	 */
	if(*scr->text!=CHAR_SEMICOLON) {
	  INFO(scr, CERROR_MISSING_SEMICOLON);
	} else
	  scr->text++;
      }
    } /* switch (*scr->text) */

    if(!(control&(SCR_BRACE|SCR_FUNCTION|SCR_SWITCH))) {
      if(control&SCR_DO) {
        CALL(Loop(scr, control, &brace));
      }
      break;
    }
  } while(1); /* loop! */
  if(control&SCR_GLOBAL) {
    if(!mainstart) {
      /* declaration still allowed, we never found any "real" code */
      CALL(Put(scr, COMP_MAIN_START));
    }
    if(!done) {
      CALL(Put(scr, COMP_MAIN_END)); /* end of main */
    }
  }
  CleanUp(scr, control, levels);
  return(ret);
}

static ReturnCode INLINE
Switch(struct Data *scr,
       struct Expr *val,
       short control)
{
  ReturnCode ret;
  uchar *text;
  long prg;
  struct Identifier *ident;
  CALL(Eat(scr)); /* eat whitespace */

  /* Check the open parenthesis */
  if(scr->text[0]!=CHAR_OPEN_PAREN)
    INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_OPEN_PAREN);
  else
    ++scr->text;

  CALL(Put(scr, COMP_START_OF_EXPR));
  CALL(Expression(val, scr, CON_NORMAL, NULL));
  CALL(Put(scr, COMP_END_OF_EXPR));

  if(val->flags&FPL_STRING)
    /* there was a string statement! */
    scr->switchtype = SWITCH_STR;
  else
    /* there was an integer expression */
    scr->switchtype = SWITCH_NUM;

  /* Check the close parenthesis */
  if(scr->text[0]!=CHAR_CLOSE_PAREN)
    INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
  else
    ++scr->text;

  CALL(Eat(scr)); /* eat whitespace */

  /* Check the open brace */
  if(scr->text[0]!=CHAR_OPEN_BRACE)
    INFO(scr, CERROR_MISSING_BRACE, CHAR_OPEN_BRACE);
  else
    ++scr->text;

  CALL(Put(scr, COMP_OPEN_BRACE));

  scr->breaks++; /* allow another level of break */
  do {
    CALL(Eat(scr));
    text =scr->text;
    prg = scr->prg;
    CALL(Getword(scr));    /* get next word */
    CALL(GetIdentifier(scr, scr->buf, &ident));
    if(ident->data.external.ID != CMD_CASE &&
       (ident->data.external.ID != CMD_DEFAULT)) {
      INFO(scr, CERROR_ILLEGAL_XXX, "switch");
      return FPLERR_SYNTAX_ERROR;
    }
    scr->text = text;
    scr->prg = prg;

    CALL(Script(scr, val, SCR_SWITCH));
  } while(*scr->text != CHAR_CLOSE_BRACE);
  scr->breaks--; /* remove a level of break */
  ++scr->text;
  return FPL_OK;
}

static ReturnCode INLINE
Declare(struct Expr *val,
	struct Data *scr,
	struct Identifier *ident,
	long start)			/* start flags */
{
  ReturnCode ret;
  long flags=start;
  do {
    switch(ident->data.external.ID) {
    case CMD_EXPORT:
      flags|=CON_DECLEXP;
      break;
    case CMD_STRING:
      flags|=CON_DECLSTR;
      break;
    case CMD_INT:
      flags|=CON_DECLINT;
      if(ident->flags&FPL_SHORT_VARIABLE)
	flags|=CON_DECL16;
      else if(ident->flags&FPL_CHAR_VARIABLE)
	flags|=CON_DECL8;
      break;
    case CMD_VOID:
      flags|=CON_DECLVOID;
      break;
    case CMD_AUTO:
    case CMD_REGISTER:
      /* flags&=~(CON_DECLEXP|CON_DECLGLOB); */
      break;
    case CMD_CONST:
      flags|=CON_DECLCONST;
      break;
    case CMD_STATIC:
      flags|=CON_DECLSTATIC;
      break;
    }
    CALL(Getword(scr));
    ret=GetIdentifier(scr, scr->buf, &ident);
  } while(!ret && ident->flags&FPL_KEYWORD_DECLARE);

  if(!(flags&CON_DECLARE))
    flags|=CON_DECLINT; /* integer declaration is default! */

  CALL(Expression(val, scr, CON_GROUNDLVL|flags|CON_IDENT, ident));
  if(*scr->text!=CHAR_SEMICOLON) {
    if(val->flags&FPL_DEFUNCTION) {
      if(*scr->text!=CHAR_CLOSE_BRACE) {
	INFO(scr, CERROR_MISSING_BRACE, CHAR_CLOSE_BRACE);
        return FPLERR_MISSING_BRACE;
      }
    }
    else {
      INFO(scr, CERROR_MISSING_SEMICOLON);
      return FPLERR_MISSING_SEMICOLON;
    }
  }
  scr->text++;
  return(FPL_OK);
}



/**********************************************************************
 *
 * Resize()
 *
 * This function resizes a variable array to the new given size.
 *
 *****/

static ReturnCode INLINE Resize(struct Data *scr, struct Expr *val, short control)
{
  uchar num=0; /* number of dimensions */
  long *dims; /* dimension array */
  struct fplVariable *var;
  struct Identifier *ident;
  ReturnCode ret;
  CALL(Getword(scr));
  GetIdentifier(scr, scr->buf, &ident);
  if(ident) {
    var=&ident->data.variable;

    if(!(ident->flags&FPL_VARIABLE) || !var->num) {
      return FPLERR_ILLEGAL_RESIZE;
    }
  }
  if(ident && !(ident->flags&FPL_EXPORT_SYMBOL)) {
    /* local, number-referenced identifier! */
    CALL(PutArg(scr,  ident->flags&FPL_GLOBAL_SYMBOL?
                      COMP_REF_GLOBAL_SYMBOL:
                      COMP_REF_LOCAL_SYMBOL, ident->number));
  }
  else {
    /* exported, name-referenced identifier! */
    CALL(PutArg(scr, COMP_REF_EXPORT_SYMBOL, Gethash(scr->buf) ));
    CALL(PutString(scr, COMP_NOTHING, scr->buf, -1));
  }

  Eat(scr);
  GETMEM(dims, MAX_DIMS*sizeof(long));

  do {
    if(*scr->text!=CHAR_OPEN_BRACKET) {
      INFO(scr, CERROR_MISSING_BRACKET, CHAR_OPEN_BRACKET);
    }
    else
      ++scr->text; /* pass the open bracket */

    /* eval the expression: */
    CALL(Put(scr, COMP_START_OF_EXPR));
    CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
    CALL(Put(scr, COMP_END_OF_EXPR));

    if(*scr->text!=CHAR_CLOSE_BRACKET)
      /* no close bracket means warning */
      INFO(scr, CERROR_MISSING_BRACKET, CHAR_CLOSE_BRACKET);
    else
      ++scr->text;

    dims[num++] = val->val.val; /* Add another dimension */
    
    if( num==MAX_DIMS ) {
      /* if we try to declare too many dimensions... */
      /*
       * Set back original variable name!
       */
      if(ident)
        strcpy(scr->buf, ident->name);
      ret = FPLERR_ILLEGAL_ARRAY;
      INFO(scr, CERROR_ILLEGAL_ARRAY);
      break;
    }
    /*
     * Go on as long there are brackets,
     */
  } while(*scr->text==CHAR_OPEN_BRACKET);

  FREE(dims);
  return ret;
}

/**********************************************************************
 *
 * CleanUp()
 *
 * Deletes/frees all local variable information.
 *
 *******/

void REGARGS
CleanUp(struct Data *scr,
        long control,
        long levels)
{
  if(control&(SCR_BRACE|SCR_FUNCTION)) {
    int deleted = DelLocalVar(scr, &scr->locals);

    if(scr->maxvariables < scr->currvariables+deleted)
      scr->maxvariables += scr->currvariables+deleted;

    PutArg(scr, COMP_AMOUNT_VARIABLES, deleted); /* ignores return code! */

    scr->varlevel--;
    scr->level=levels; /* new variable amplitude */
  }
}


/**********************************************************************
 *
 * Loop()
 *
 * This function is called at the end of a block, however the block was
 * started (brace or not brace).
 *
 *******/

static ReturnCode REGARGS
Loop(struct Data *scr,
     short control,
     uchar *cont) /* store TRUE or FALSE if loop or not */
{
  ReturnCode ret = FPL_OK;
  if(control&SCR_DO) {
    struct Expr *val;

    GETMEM(val, sizeof(struct Expr));

    /* This a do while end. */
    do {
      CALL(Put(scr, COMP_CLOSE_BRACE));
      if(ret=Getword(scr))
        break;
      if(strcmp(scr->buf, "while")) {
	INFO(scr, CERROR_MISSING_WHILE);
        ret=FPLERR_MISSING_WHILE; /* missing 'while' after do-while statement */
        break;
      }
      CALL(Put(scr, COMP_DO_CONDITION));
      if(ret=Eat(scr))
        break;
      if(*scr->text!=CHAR_OPEN_PAREN) {
	INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_OPEN_PAREN);
      }
      else
	++scr->text;
      CALL(Put(scr, COMP_START_OF_EXPR));
      if(ret=Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL))
        break;
      if(*scr->text!=CHAR_CLOSE_PAREN) {
	INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
      }
      else
	++scr->text;
      CALL(Put(scr, COMP_END_OF_EXPR));
    } while(0); /* only used to break out from! */

    FREE(val);

  }
  return(ret);
}

