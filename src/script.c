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
 * S-171 43 Solna                                                       *
 * Sweden                                                               *
 *                                                                      *
 * FidoNet 2:201/328    email:Daniel.Stenberg@sth.frontec.se            *
 *                                                                      *
 ************************************************************************/

#ifdef AMIGA
#include <exec/types.h>
#include <proto/exec.h>
#include <libraries/dos.h>
#include <proto/dos.h>

#include <exec/libraries.h>
#include <dos.h>

#else
#include <sys/types.h>
#include <sys/stat.h>
#ifdef SUNOS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#endif

#include <stdlib.h> /* for the system() call i.e */
#include <stddef.h> /* for offsetof() */
#include <stdio.h>
#include <string.h>
#include "script.h"
#include "debug.h"
#include "compile.h"

#ifdef DEBUG
long mem=0;
long maxmem=0;
#endif

static ReturnCode INLINE AddProgram(struct Data *, struct Program **,
				    uchar *, long, uchar *);
static uchar REGARGS CheckIt(struct Data *, struct Expr *, short, ReturnCode *);
static ReturnCode INLINE Declare(struct Expr *, struct Data *,
				 struct Identifier *, long);
static ReturnCode Go(struct Data *, struct Expr *val);
static ReturnCode REGARGS Loop(struct Data *, struct Condition *, short, uchar *);
static ReturnCode INLINE Resize(struct Data *, struct Expr *, short);
static ReturnCode REGARGS SkipStatement(struct Data *);
static ReturnCode REGARGS StoreGlobals(struct Data *, uchar);
static ReturnCode REGARGS Run(struct Data *, uchar *, uchar *, long, unsigned long *);
static ReturnCode INLINE Switch(struct Data *, struct Expr *, short,
                                struct Condition *);
static REGARGS void StoreBeginning(struct Data *, char *, long);
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

unsigned long inttags[]={FPLSEND_INT, 0, FPLSEND_DONE};
unsigned long strtags[]={FPLSEND_STRING, 0,
                         FPLSEND_STRLEN, 0,
                         FPLSEND_DONTCOPY_STRING, TRUE,
                         FPLSEND_DONE};

#ifndef AMIGA /* if not using SAS/C on Amiga */

/******************************************************/
/* Parameter list frontends of the library functions: */
/******************************************************/

#ifdef VARARG_FUNCTIONS
long fplExecuteFileTags(void *anchor, uchar *program, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, program); /* get parameter list */
#endif
  ret = fplExecuteFile(anchor, program, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else /* VARARG_FUNCTIONS */
long PREFIX fplExecuteFileTags(void *anchor, uchar *program, unsigned long tags, ...)
{
  return(fplExecuteFile(anchor, program, (unsigned long *)&tags));
}
#endif

#endif

/***************************************************************************
 *
 * fplExecuteFile()
 *
 * Executes the specified file as an FPL program.
 *
 ******/

ReturnCode PREFIX fplExecuteFile(AREG(0) struct Data *scr,
				 AREG(1) uchar *filename,
				 AREG(2) unsigned long *tags)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplExecuteFile");
#endif
  return(Run(scr, filename, NULL, 1, tags));
}


#ifndef AMIGA /* if not using SAS/C on Amiga */

#ifdef VARARG_FUNCTIONS
long fplExecuteScriptTags(void *anchor, uchar **program, long lines, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, lines); /* get parameter list */
#endif
  ret = fplExecuteScript(anchor, program, lines, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else /* VARARG_FUNCTIONS */

long PREFIX fplExecuteScriptTags(void *anchor, uchar **program, long lines,
                                 unsigned long tags, ...)
{
  return(fplExecuteScript(anchor, program, lines, (unsigned long *)&tags));
}
#endif

#endif

/**********************************************************************
 *
 * fplExecuteScript()
 *
 * Frontend to Run().
 *
 * The error code is returned to daddy...
 *
 ******/

ReturnCode PREFIX fplExecuteScript(AREG(0) struct Data *scr, /* nice struct */
				   AREG(1) uchar **program, /* program array */
				   DREG(1) long lines, 	/* number of lines */
				   AREG(2) unsigned long *tags)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplExecuteScript");
#endif
  return(Run(scr, NULL, *program, lines, tags));
}


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
         uchar *filename,
         struct Program *prog)
{
  struct Data *scr=(struct Data *)fpl;
#ifdef AMIGA  /* Amiga version. */
  struct FileInfoBlock fileinfo;
  struct FileLock *lock=NULL;
  struct FileHandle *fileread;

  struct MyLibrary *lib = (struct MyLibrary *)getreg(REG_A6);
  struct Library *DOSBase = lib->ml_DosBase;
#else
  FILE *stream;
  struct stat statstr;
#endif
  ReturnCode ret=FPL_OK;
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
      prog->size = fileinfo.fib_Size+1; /* Add one for a terminating zero! */
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
      prog->size=ftell(stream)+1;
      fseek(stream, 0, 0);
    }
  }
#endif

#ifdef AMIGA
  prog->date = GETFILEDATE(fileinfo);
#else
  if(!stat(filename, &statstr)) {
    prog->date = statstr.st_mtime;
  } else
    ret=FPLERR_OPEN_ERROR;
#endif

  if(ret)
    return(ret);

  /* Open file for reading. */
#ifdef AMIGA
  /* We could use OpenFromLock() here, but it's a V36+ function! */
  fileread=(struct FileHandle *)Open((UBYTE *)filename, MODE_OLDFILE);
#elif defined(UNIX) || defined(WIN32)
  /* file is already opened! */
#endif
  prog->program=(uchar *)MALLOC(prog->size); /* Allocate memory for program. */
  if(!prog->program) /* if we didn't get the requested memory: */
    ret=FPLERR_OUT_OF_MEMORY;
#ifdef AMIGA
  else if(Read((BPTR)fileread, prog->program, (LONG)prog->size)<0) /* get entire file */
#elif defined(UNIX) || defined(WIN32)
  else if(!fread(prog->program, 1, prog->size, stream))
#endif
    /* if we couldn't Read() the file: */
    ret=FPLERR_OPEN_ERROR;
  else
    (prog->program)[prog->size-1]='\0'; /* add the terminating zero byte. */
#ifdef AMIGA
  Close((BPTR)fileread); /* close file */
#elif defined(UNIX) || defined(WIN32)
  fclose(stream); /* close the stream */
#endif
  /* only if error and we could allocate the proper memory */
  if(ret && prog->program) {
    FREE(prog->program); /* free the, for the program allocated, memory */
  }
  return(ret); /* get back to parent */
}

/**********************************************************************
 *
 * AddProgram();
 *
 * Adds a program to FPL's internal lists of program files.
 *
 ****/

static ReturnCode INLINE AddProgram(struct Data *scr,
				    struct Program **get,
				    uchar *program,
				    long lines,
				    uchar *name)
{
  struct Program *next, *prog=NULL;
  ReturnCode ret;
  long date=-1;
  if(name && name[0]) {
    /*
     * Name was given. Search through the internals to see if
     * we have this file cached already!
     */
    prog=scr->programs;
    while(prog) {
      if(prog->name && !strcmp(prog->name, name))
	break;
      prog=prog->next;
    }
  }
  if(prog) {

    /*
     * The program already exists.
     */
    if( (prog->flags & PR_REREAD_CHANGES) &&
        (prog->flags & PR_NAME_IS_FILENAME) &&
        !(prog->flags&PR_USERSUPPLIED) ) {

      timeoffile(date, name); /* big macro */

      /* Compare dates of internal program and actual file */
      if(date != prog->date) {
        /*
         * The dates are different, flush all info that has to do with the
         * file, and re-read it into memory!
         */
        unsigned long tags[]={FPLSEND_FREEFILE, 0, FPLSEND_DONE};
        tags[1] = (unsigned long)name;
        CALL(Send(scr, tags));
        prog=NULL; /* force a insertion of this file again! */
      }
    }

    /*
     * The very same good old program. If the FPLTAG_PREVENT_RUNNING_SAME
     * was used, then abort here and now!
     */
    if(prog && scr->flags&FPLDATA_PREVENT_RUNNING_SAME) {
      *get = NULL;
      return FPL_OK;
    }

/*

  These following actions don't have to be done!

    CALL(LeaveProgram(scr, scr->prog));
    CALL(GetProgram(scr, prog));
*/
  }

  if(!prog) {
    GETMEMA(prog, sizeof(struct Program));
    memset(prog, 0, sizeof(struct Program));
#ifdef DEBUG
    CheckMem(scr, prog);
#endif
    next=scr->programs;
    prog->next=next;
    prog->program=program;
    prog->lines=lines;
    prog->startprg=1;
    prog->virprg=1;
    prog->flags = (scr->flags&FPLDATA_REREAD_CHANGES?
                    PR_REREAD_CHANGES:0)|
                  (scr->flags&FPLDATA_FLUSH_NOT_IN_USE?
                    PR_FLUSH_NOT_IN_USE:0)|
		  (scr->flags&FPLDATA_KIDNAP_CACHED?
		    PR_KIDNAP_CACHED:0);
    if(program) {
      SetupCompiled(prog);
    }
    if(name) {
      STRDUPA(prog->name, name);
    }
    scr->programs=prog;
  }

  scr->prog=prog;
  *get=prog;
  return(FPL_OK);
}

/**********************************************************************
 *
 * DelProgram()
 *
 * Deletes a specifed program from memory. If NULL is specified where
 * the program struct is supposed, all programs are removed! (Amiga
 * version *have* to do that to UnLock() all files that might be locked
 * when using the FPLTAG_LOCKUSED!
 *
 *******/

ReturnCode REGARGS
DelProgram(struct Data *scr,
           struct Program *del)
{
  struct Program *prog=scr->programs, *prev=NULL;
  while(prog) { /* it must not be running! */
    if((!del || prog==del) && !prog->running) {
      if(prev)
	prev->next=prog->next;
      else
	scr->programs=prog->next;
      if(del && (scr->prog==del))
	scr->prog=scr->prog->next;
      prev=prog->next;
      if(prog->name)
	FREEA(prog->name);
      if(!(prog->flags&PR_USERSUPPLIED) && prog->program) {
        SwapMem(scr, prog->program, MALLOC_DYNAMIC);
        FREE(prog->program);
      }
      FREEA(prog);
      if(del)
        break;
      
      prog=prev;
      prev=NULL;

    }
    else {
      prev=prog;
      prog=prog->next;
    }
  }
  return(FPL_OK);
}

/**********************************************************************
 *
 * This program checks and autoruns .FPC files newer than the .FPL
 * that is about to run.
 */
 
ReturnCode FileCheck(struct Data *scr,
                     char **filename,
                     struct Program *prog)
{
#define SPACE_FOR_FPLC 0 /* strlen(COMPILE_COMMAND) */
                         /* bytes needed to add "fplc " */
  ReturnCode ret=FPL_OK;
  uchar *filebuffer;
  uchar *file;
  uchar ext;
  long timethis;
  long timethat;
  long len=strlen(*filename); /* length of file name */
  if(len >= 4) {
    if(!my_memicmp(UNCOMPILED_EXTENSION,
                   &(*filename)[len-strlen(UNCOMPILED_EXTENSION)],
                   strlen(UNCOMPILED_EXTENSION)+1)) {
      ext=1; /* it is a non-compiled extension */
    }
    else
      ext = 0;
    if(ext) {
      GETMEM(filebuffer, len+1+SPACE_FOR_FPLC);
      file = &filebuffer[SPACE_FOR_FPLC];

      timeoffile(timethis, prog->name); /* big macro */
      
      memcpy(file, *filename, len-strlen(COMPILED_EXTENSION));
      memcpy(&file[len-strlen(COMPILED_EXTENSION)],
             COMPILED_EXTENSION,
             strlen(COMPILED_EXTENSION)+1);

      timeoffile(timethat, file); /* big macro */

      if(timethis > timethat) {
        /* uncompiled is the newest, go with that! */
        FREE(filebuffer);
        return FPL_OK;
      }
      
      /* run the compiled file */
      prog->flags |= PR_SELECTED_FPC;
      *filename = filebuffer;
    }
  }
  return ret;
}

/**********************************************************************
 *
 * Run()
 *
 *****/

static ReturnCode REGARGS
Run(struct Data *scr,
    uchar *filename,
    uchar *program,
    long lines,
    unsigned long *tags)
{
  ReturnCode end;
  struct Expr *val;
  unsigned long *tag=tags;
  uchar storeglobals;	/* DEFAULT: fplInit() value! */
  struct Program *thisprog, *prog;
  struct Store *store;
  struct Local *glob;
  long currcol;
  long *globpointer=NULL;

  /* Store the 'soft' debugging information! */
  long prev_mode = scr->flags & (FPLDATA_DEBUG_MODE|FPLDATA_ISOLATE);

#ifdef DEBUG
  long memory=mem;
#endif

  if(!scr)
    /* misbehaviour */
    return(FPLERR_ILLEGAL_ANCHOR);

  if(scr->runs) {
    /* this is a nested call! */
    GETMEM(store, offsetof(struct Data, store_end)-
                  offsetof(struct Data, store_from));

    currcol=scr->text-(&scr->prog->program)[scr->prg-1];

    LeaveProgram(scr, scr->prog);
    memcpy(store, &scr->store_from, offsetof(struct Data, store_end)-
                                    offsetof(struct Data, store_from));
  } else {
    scr->msg = NULL;  /* We start with an empty message queue! */
    scr->varlevel =0; /* start at locale level 0 */
  }
  end = AddProgram(scr, &prog, program, lines, filename);

  if(NULL == prog && FPL_OK == end) {
    /*
     * This execution was simply prevented due to circumstances!
     */
  }
  else if(end <= FPL_EXIT_OK) {

    if(!prog->program && filename) {
      /*
       * It didn't already exist.
       */
      if(scr->flags & (FPLDATA_AUTORUN|FPLDATA_AUTOCOMPILE))
        FileCheck(scr, &filename, prog);
      end = ReadFile(scr, filename, prog); /* get file */
      prog->flags|=PR_NAME_IS_FILENAME;
      SetupCompiled(prog);
      if(prog->flags & PR_SELECTED_FPC) {
        FREE(filename);
      }
    }
    else if(!filename)
      prog->flags=PR_USERSUPPLIED;

    if(end <= FPL_EXIT_OK) {

      end=GetProgram(scr, prog); /* lock it for our use! */

      if(end <= FPL_EXIT_OK) {

        thisprog=scr->prog;
        if(scr->flags&FPLDATA_CACHEALLFILES) {
          thisprog->flags|=PR_CACHEFILE;
          if(scr->flags&FPLDATA_CACHEEXPORTS)
            thisprog->flags|=PR_CACHEEXPORTS;
        } else
          thisprog->flags&=~PR_CACHEFILE;

        thisprog->openings++;

        scr->prg=thisprog->startprg;     /* starting line number */
        scr->text=(&thisprog->program)[thisprog->startprg-1]+
          thisprog->startcol; /* execute point */


	/* fprintf(stderr, "Exp:%s", scr->text); */

        scr->ret=FPL_OK;		/* return code reset */
        scr->virprg=thisprog->virprg;	/* starting at right virtual line */
        scr->virfile=thisprog->virfile;	/* starting at right virtual file */
        scr->level=0;			/* level counter */
        scr->strret=FALSE;		/* we don't want no string back! */
        scr->interpret=NULL;		/* no interpret tag as default */
        scr->locals=NULL;		/* local symbol list */
        scr->globals=NULL;		/* global symbol list */
        scr->FPLret=0;			/* initialize return code value */
        scr->string_return=NULL;	/* no string returns allowed */
        scr->msg = NULL;                /* no pending messages */

        while(tag && *tag) {
          switch(*tag++) {
          case FPLTAG_ISOLATE:
	    scr->flags = BitToggle(scr->flags, FPLDATA_ISOLATE, *tags);
	    break;

          case FPLTAG_DEBUG:
	    scr->flags = BitToggle(scr->flags, FPLDATA_DEBUG_MODE, *tags);
	    break;

          case FPLTAG_REREAD_CHANGES:
	    thisprog->flags = BitToggle(thisprog->flags,
					PR_REREAD_CHANGES, *tags);
            break;

          case FPLTAG_FLUSH_NOT_IN_USE:
	    thisprog->flags = BitToggle(thisprog->flags,
					PR_FLUSH_NOT_IN_USE, *tags);
            break;

          case FPLTAG_KIDNAP_CACHED:
	    thisprog->flags = BitToggle(thisprog->flags,
					PR_KIDNAP_CACHED, *tags);
            break;

          case FPLTAG_STRING_RETURN:
            scr->string_return = (uchar **)*tag;
            scr->strret=TRUE; /* enable return string */
            break;

          case FPLTAG_INTERPRET:
            scr->interpret=(uchar *)*tag;
            break;

          case FPLTAG_STARTPOINT:
            scr->text=(uchar *)*tag;
            break;
          case FPLTAG_STARTLINE:
            scr->prg=(long)*tag;
            break;
          case FPLTAG_USERDATA:
            scr->userdata=(void *)*tag;
            break;
          case FPLTAG_CACHEFILE:
            if(*tag) {
              thisprog->flags|=PR_CACHEFILE;
              if(*tag=FPLCACHE_EXPORTS)
                thisprog->flags|=PR_CACHEEXPORTS;
            } else
              thisprog->flags&=~PR_CACHEFILE;
            break;
          case FPLTAG_PROGNAME:
	    if(*tag) {
              prog=scr->programs;
              while(prog) {
                if(prog->name && !strcmp(prog->name, (uchar *)*tag))
                  break;
                prog=prog->next;
              }
              if(!prog) {
                /*
                 * The program was not found, then set/rename the
                 * current program to this name!
                 */
                if(thisprog->name) {
                  FREEA(thisprog->name);
                }
                STRDUPA(thisprog->name, *tag);
              } else {
                /*
                 * We found another progam with that name. Execute that
                 * instead of this!
                 */
                DelProgram(scr, thisprog);
                thisprog=prog;
              }
	    }
            break;
          case FPLTAG_FILENAMEGET:
	    thisprog->flags = BitToggle(thisprog->flags,
					PR_FILENAMEFLUSH, *tags);
            break;
          case FPLTAG_ISCACHED:
            globpointer = (long *)*tag;
            break;
          }
          tag++;
        }

        if(!thisprog->name) {
          /* If no name has been given, do not store any global symbols from it! */
          STRDUPA(thisprog->name, FPLTEXT_UNKNOWN_PROGRAM);
          storeglobals=FALSE;
          thisprog->flags&=~(PR_CACHEFILE|PR_CACHEEXPORTS);
        } else
          storeglobals = thisprog->flags&(PR_CACHEFILE|PR_CACHEEXPORTS);

        scr->virfile=thisprog->name; /* starting with this file */
        val= MALLOC(sizeof(struct Expr));
        if(val) {
          memset(val, 0, sizeof(struct Expr)); /* initial clear */
          end=Go(scr, val);
          if(end<=FPL_EXIT_OK &&
             scr->string_return) {
            /*
             * No error and
             * we accept string returns and...
             */
            if((val->flags&(FPL_STRING|FPL_RETURN)) ==
               (FPL_STRING|FPL_RETURN) &&
               val->val.str) {
              /*
               * ...there was a final "return" or "exit" keyword.
               * and we have a returned string to deal with.
               */
  
              /* assign the pointer */
              *scr->string_return = val->val.str->string;
  
              /* make it a "static" allocation */
              SwapMem(scr, val->val.str, MALLOC_STATIC);
            }
            else {
              /*
               * If not, reset the pointer to NULL!
               */
              *scr->string_return = NULL;
            }
          }
          FREE(val);
        } else
          end=FPLERR_OUT_OF_MEMORY;

        if(end>FPL_EXIT_OK) {
          struct fplArgument pass={
            NULL, FPL_GENERAL_ERROR, NULL, 0};
          void *array[1];
          pass.key=(void *)scr;
          array[0] = (void *)end;
          pass.argv= array;

          thisprog = scr->prog;
          
          if(thisprog->flags&PR_COMPILED)
            scr->buf[0]=0; /* no damned identifier */
            
          if(scr->error) {
	    /* We'll fix the error string! */
	    GetErrorMsg(scr, end, scr->error);
	  }

          /* new argv assigning for OS/2 compliance! */
          InterfaceCallNoStack(scr, &pass, scr->function);
        }

        thisprog->column=scr->text-(&thisprog->program)[scr->prg-1]+1;
        scr->virfile=NULL; /* most likely to not point to anything decent
                              anyway! */

        /*
         * Go through the ENTIRE locals list and delete all. Otherwise they will
         * ruin the symbol table.
         */

        while(scr->locals)
          DelLocalVar(scr, &scr->locals);

        thisprog->openings--;
        LeaveProgram(scr, thisprog); /* failure is a victory anyway! */

        /*
         * If the option to cache only programs exporting symbols is turned on,
         * then we must check if any of the globals are exported before caching!
         */

        if(end<=FPL_EXIT_OK && (storeglobals & PR_CACHEEXPORTS)) {
          glob = scr->globals;

          while(glob) {
            /* Traverse all global symbols */

            if(glob->ident->flags&FPL_EXPORT_SYMBOL)
              /* if we found an exported symbol, get out of loop */
              break;

            glob=glob->next; /* goto next global */
          }

          if(!glob)
            /* no exported symbols were found! */
            storeglobals = FALSE; /* do not cache this file! */
        }

        if(end<=FPL_EXIT_OK && storeglobals && thisprog->flags&PR_CACHEFILE) {
         /* no error, store the globals and cache the file */

          if(!(thisprog->flags&PR_GLOBALSTORED)) {

            if(scr->globals) {
	      long total_size;
	      long line=1;
	      uchar *newprogram;
              
              if(!(thisprog->flags&PR_USERSUPPLIED))
                /*
                 * The memory is allocated by FPL itself!
                 */
                SwapMem(scr, thisprog->program, MALLOC_STATIC);
              else {
                /*
                 * The memory is allocated by the user!
                 */
                if(thisprog->flags&PR_KIDNAP_CACHED) {
                  /*
                   * We have been instructed to "take over" all host
                   * allocations that we intend to keep as cached files!
                   */

                  /* start with counting the total size of the program: */
                  for(line = total_size = 0; line<thisprog->lines; line++)
                    total_size += strlen( (&thisprog->program)[line] );

                  /* get enough memory to duplicate it! */
                  newprogram = MALLOCA(total_size + 1 ); /* add for zero */
                  if(newprogram) {
                    /*
                     * We got requested amount of memory to copy the entire
                     * user supplied program!
                     */
                    for(line = total_size = 0; line<thisprog->lines; line++) {
                      strcpy(newprogram+total_size,
                             (&thisprog->program)[line]);
                      total_size += strlen( (&thisprog->program)[line] );
                    }
                    thisprog->program = newprogram;
                    newprogram[total_size] = CHAR_ASCII_ZERO;
                    
                    thisprog->lines = 1; /* this is now in one single line! */

                    /* switch off the now incorrect bit: */
                    thisprog->flags &= ~PR_USERSUPPLIED;
                  }
                  else {
                    /* We couldn't allocate a copy of the program, fail */
                    line=0;
                    end = FPLERR_OUT_OF_MEMORY; /* fail with proper return
                                                   code! */
                  }
                }
              }
	      if(line) {
                /* Store all global symbols!!! */
                StoreGlobals(scr, MALLOC_STATIC); /* ignore return code */

	        /* set the flag saying we did so! */
                thisprog->flags|=PR_GLOBALSTORED;
	      }
            } else
              DelProgram(scr, thisprog); /* this also removes the Lock() */
          }
        } else {
          /*
           * We must delete the global symbol lists
           * properly and not just free the memory. Otherwise we might free memory
           * used in the middle of the list we intend to save for next run!
           */
          if(!thisprog->openings) {
            /* If not in use */
            if(scr->globals)
            /* There is some global symbols to delete! */
            DelLocalVar(scr, &scr->globals);

            /*
             * Check if this program was stored in memory earlier (in
             * another run). If not ...
             */
            if(!(thisprog->flags&PR_GLOBALSTORED)) {
              /*
               * ...delete this program from memory!
               */
              DelProgram(scr, thisprog); /* this also removes the Lock() */
            }
          }
        }

        if(globpointer)
          *globpointer=(long)scr->globals;

        scr->runs--;
      } /* else
          We didn't get the program, out of memory or stupid interface
          function reply!
         */
    } else
      DelProgram(scr, prog); /* we couldn't load it! */
  }

  /*
   * Reset the debug mode status we had when we entered this function!
   */
  scr->flags = BitToggle(scr->flags, FPLDATA_DEBUG_MODE,
                         prev_mode&FPLDATA_DEBUG_MODE);
  /*
   * Reset the isolate status we had when we entered this function!
   */
  scr->flags = BitToggle(scr->flags, FPLDATA_ISOLATE,
                         prev_mode&FPLDATA_ISOLATE);

  if(scr->runs) {
    /* still running! */

    memcpy(&scr->store_from, store, offsetof(struct Data, store_end)-
                                    offsetof(struct Data, store_from));
    GetProgram(scr, scr->prog);
    FREE(store);

    /* reset execute point: */
    scr->text=(&scr->prog->program)[scr->prg-1]+ currcol;
  }
  else {
    FREEALL(); /* frees all ALLOC_DYNAMIC */
  }

  return(end==FPL_EXIT_OK?FPL_OK:end);
}

/**********************************************************************
 *
 * Go();
 *
 * This is an own function to make the stack usage in this particular
 * function very small. Then we don't have to copy more than 10-20 bytes
 * of the old stack when swapping to the new in the amiga version of the
 * library!
 *
 ******/

static ReturnCode Go(struct Data *scr, struct Expr *val)
{
#if defined(AMIGA) && defined(SHARED)
  /* The function call below is an assembler routine that allocates a new
     stack to use in the library! */
#define FIRSTFUNC InitStack
#else
  /* Not Amiga or not shared! */
#define FIRSTFUNC Script
#endif

  scr->runs++;
  return FIRSTFUNC(scr, val,
                   SCR_BRACE|    /* to make it loop and enable declarations */
                   SCR_FUNCTION| /* return on return() */
                   SCR_FILE|     /* this level may end with '\0' */
                   SCR_GLOBAL,   /* global symbol declarations enabled */
                   NULL);
}


static ReturnCode REGARGS
StoreGlobals(struct Data *scr,
             uchar type)
{
  struct Local *local, *prev=NULL;
  struct Identifier *ident;
  struct fplVariable *var;

  if(scr->prog->running>1)
    /*
     * It's enough if we commit this only on the ground level exit!
     */
    return(FPL_OK);

  local=scr->globals;
  while(local) {
    ident=local->ident;
    if(ident->flags&FPL_VARIABLE) {
      SwapMem(scr, local, type);		/* preserve the chain! */
      SwapMem(scr, ident, type);		/* structure */

      if(!(ident->flags&FPL_COMPILER_ADDED))
        SwapMem(scr, ident->name, type);	/* name */

      var=&ident->data.variable;

      SwapMem(scr, var->var.val32, type); /* variable area */

      if(!var->num && ident->flags&FPL_STRING_VARIABLE && var->var.str[0])
	/* no array but assigned string variable */
	SwapMem(scr, var->var.str[0], type);	/* string */
      else if(var->num) {
	/* array */
	SwapMem(scr, var->dims, type); /* dim info */
	if(ident->flags&FPL_STRING_VARIABLE) {
	  int i;
	  for(i=0; i<var->size; i++) {
	    /* Take one pointer at a time */
	    if(var->var.str[i])
	      /* if the value is non-zero, it contains the allocated length
		 of the corresponding char pointer in the ->array->vars
		 array! */
	      SwapMem(scr, var->var.str[i], type);
          }
	  SwapMem(scr, var->var.str, type);
	}
      }
    } else if(ident->flags&FPL_FUNCTION) {
      SwapMem(scr, local, type);		/* preserve the chain! */
      SwapMem(scr, ident, type);		/* structure */
      if(!(ident->flags&FPL_COMPILER_ADDED)) {
        SwapMem(scr, ident->name, type);		/* name */
        SwapMem(scr, ident->data.inside.format, type);	/* parameter string */
      }
    }
    prev=local;
    local=local->next;
  }
  if(prev) {
    prev->next=scr->usersym; /* link in front of our previous list! */
    scr->usersym=scr->globals;
  }
  scr->globals=NULL;
  return(FPL_OK);
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
       DREG(2) short control,     /* control byte */
       AREG(1) struct Condition *con)
{
  uchar declare=control&SCR_BRACE?1:0; /* declaration allowed? */
  ReturnCode ret;		   /* return value variable */
  struct Condition *con2;      /* recursive check information! */
  uchar brace=0; /* general TRUE/FALSE variable */
  uchar *text; /* position storage variable */
  long prg;   /* position storage variable */
  long levels=scr->level; /* previous level spectra */
  struct Identifier *ident; /* used when checking keywords */
  long virprg=scr->virprg;
  uchar *virfile=scr->virfile;
  uchar done=FALSE; /* TRUE when exiting */
  struct fplArgument *pass;

#if defined(AMIGA) && defined(SHARED)
  if(ret=CheckStack(scr, scr->stack_limit, scr->stack_margin)) {
    if(ret==1)
      return(FPLERR_OUT_OF_MEMORY);
    else
      return(FPLERR_OUT_OF_STACK);
  }
#endif

  if(control&(SCR_BRACE|SCR_FUNCTION)) {
    /*
     * New symbol declaration level!
     */
    scr->varlevel++;
    CALL(AddLevel(scr));

    if((control&(SCR_BRACE|SCR_FUNCTION|SCR_FILE|SCR_GLOBAL)) ==
               (SCR_BRACE|SCR_FUNCTION|SCR_FILE|SCR_GLOBAL)) {
      /* first line in the script/file */
      Eat(scr);
      if(!strncmp("#!", scr->text, 2)) {
        /* unix-style shell type line found, skip it */
        while(*++scr->text && (CHAR_NEWLINE!=scr->text[0]));
      }
    }

  }

  if(control&SCR_FUNCTION)
    scr->level=0; /* number of levels to look for variables */
  else if(control&SCR_BRACE)
    scr->level++;

  if(scr->flags&FPLDATA_DEBUG_MODE) {
    /*
     * If debug mode is on already here, it means that our previous level
     * had it and we must make sure that they will even when we return.
     * (Without that bit, CleanUp() will switch off debug mode!)
     */
    control|=SCR_DEBUG;
  }

  if(scr->prog->flags&PR_COMPILED) {
    /*
     * Halleluja! This is a compiled mega-mighty-cool FPL program designed
     * for maximum interpreting speed.
     */
     
    /* THE FOLLOWING LINE IS TEMPORARY ADDED HERE: */
    scr->globalinfo = &scr->prog->globalinfo;
     
    brace = FALSE; /* set to false from start, set it back after usage!	*/
    
    while(!done) {
      Pass2 code; /* compiled instruction */

      if(scr->interfunc) {
        /* call the interval function */
        if(scr->data=InterfaceCall(scr, scr->userdata, scr->interfunc))
  	  return FPLERR_PROGRAM_STOPPED;
      }
      code = GETSHORT;

      P_SHORT; /* pass the instruction */

      switch(code) {
        /* These codes are defined in pass2.h */
        case PASS2_LINE_NUMBER:
          scr->virprg = GETLONG;
          P_LONG;
          break;
          
        case PASS2_BREAK_EXPR:
          CALL(CmpBreak(scr, val));
          break;
          
        case PASS2_SWITCH:
          CALL(CmpSwitch(scr, val));
          break;
          
        case PASS2_END_OF_EXPR:
          break;

        case PASS2_DECLARE:
          CALL(CmpDeclare(scr));
          break;

        case PASS2_EXPORT_FUNCTION:
          CALL(CmpExport(scr));
          break;

        case PASS2_ASSIGN_ARGUMENT: /* [var number] [argument number] */
          CALL(AssignArg(scr));
          break;
          
        case PASS2_LABEL_GOTO: /* OFFSET to set the program pointer to */
          scr->text = &scr->prog->program[ scr->prog->index + GETLONG ];
          break;

        case PASS2_IFNOT_BRANCH:       /* OFFSET follows */
          brace = TRUE;
          /* falls through! */
        case PASS2_IF_BRANCH:  /* OFFSET follows */
          prg = GETLONG;
          P_LONG; /* pass offset */
          CALL(CmpExpr(val, scr, CON_GROUNDLVL|CON_NUM)); /* get result */
          
          if(brace ^ (val->val.val?TRUE:FALSE))
            scr->text = &scr->prog->program[ scr->prog->index + prg ];
          else
            P_SHORT; /* pass end of expr */
          brace = FALSE; /* it has served its purpose, set back to FALSE */
          break;
	  
        case PASS2_MAIN_START: /* OFFSET the main program starts at */
          scr->prog->startcol=
            scr->prog->index + GETLONG;
          P_LONG; /* pass the argument */
          scr->prog->foundstart=TRUE;
          break;
          
        case PASS2_RETURN:
        case PASS2_EXIT:
          if(scr->strret) {
            /*
             * This function is supposed to return a string. Get it.
             */
            CALL(CmpExpr(val, scr, CON_STRING));

            if(val->flags&FPL_NOFREE) {
              /*
               * We're only refering to another string! We can't
               * allow that since that string might be a local
               * variable, and all such are about to be deleted now!
               */
              register struct fplStr *string;
              if(val->val.str) {
                /* did we really get a pointer? */
                GETMEM(string, val->val.str->len+sizeof(struct fplStr));
                memcpy(string,
                val->val.str,
                val->val.str->len+sizeof(struct fplStr));
                string->alloc=val->val.str->len;
                strtags[1]=(long)string->string;
                strtags[3]=string->len;
                CALL(Send(scr, strtags));
              }
              else {
                strtags[1] = strtags[3] = 0;
                CALL(Send(scr, strtags));
              }
            }
            else {
              if(val->val.str) {
                strtags[1]=(long)val->val.str->string;
                strtags[3]=val->val.str->len;
              }
              else
                strtags[1] = strtags[3] = 0;
              CALL(Send(scr, strtags));
            }
          }
          else {
            CALL(CmpExpr(val, scr, CON_GROUNDLVL|CON_NUM));
	    scr->FPLret=val->val.val;	/* set return code! */
            scr->returnint = &scr->FPLret; /* point to result */
            inttags[1]=val->val.val;
            CALL(Send(scr, inttags));
	  }
          done = TRUE;
          if(PASS2_EXIT == code)
            ret = FPL_EXIT_OK; /* exit from this file */
          break;

        case PASS2_RESET_VARIABLE:
          CALL(CmpReset(scr, GETLONG)); /* reset the local variable to
                                           "scratch position" */
          P_LONG;
          break;
          
        default:
          scr->text-=sizeof(short); /* back on the instruction */
          CALL(CmpExpr(val, scr, CON_NORETURN|CON_GROUNDLVL));
          break;
      }
    }
    if(scr->localinfo.listsize) {
      FREE(scr->localinfo.list);
      scr->localinfo.listentries = scr->localinfo.listsize =0;
    }
  }
  else {
  
    while(!done) {
      if(ret=Eat(scr)) {
        if(control&SCR_FILE && ret==FPLERR_UNEXPECTED_END)
          /* It's OK! */
          ret=FPL_OK;
        break;
      }
  
      /* call the interval function */
      if(scr->interfunc) {
        if(scr->data=InterfaceCall(scr, scr->userdata, scr->interfunc))
          return FPLERR_PROGRAM_STOPPED;
      }
  
  #ifdef DEBUGMAIL
      DebugMail(scr, MAIL_EXECUTE, 500, NULL);
  #endif
  
      switch(*scr->text) {
      case CHAR_OPEN_BRACE:               /* open brace */
        scr->text++;
        CALL(Script(scr, val,
                    SCR_NORMAL|SCR_BRACE,
                    con));
        if(CheckIt(scr, val, control, &ret)) {
          CleanUp(scr, control, levels);
          return(ret);
        }
        break;
  
      case CHAR_CLOSE_BRACE:
        if(control&SCR_LOOP) {
          if(control&SCR_BRACE) {
            DelLocalVar(scr, &scr->locals); /* delete all local declarations */
            scr->varlevel--;                /* previous variable level */
            scr->level--;                   /* previous level spectra */
          }
          CALL(Loop(scr, con, control, &brace));
          if(brace) {
            /* Yes! We should loop! */
            if(control&SCR_BRACE) {
              /* bring back the proper values */
              scr->varlevel++;
              scr->level++;
              AddLevel(scr); /* restart this level! */
              declare=TRUE;
            }
            scr->virprg=virprg;
            scr->virfile=virfile;
            continue;
          }
          val->flags=0;
        } else {
          scr->text++;
          val->flags=FPL_BRACE;
          CleanUp(scr, control, levels);
        }
        scr->returnint = NULL; /* no result integer! */
        return(FPL_OK);  /* return to calling function */
  
      case CHAR_SEMICOLON:
        scr->text++;
        break;
  
      default:
        /*
         * Time to parse the statement!
         */
  
        text=scr->text;                /* store current position */
        prg=scr->prg;
        if(!Getword(scr))    /* get next word */
          GetIdentifier(scr, scr->buf, &ident);
        else {
          prg=-1;    /* we have not read a word! */
          ident=NULL;
        }
        if(ident && control&SCR_GLOBAL && declare) {
          /* still on ground level and declaration allowed */
          if(!(ident->flags&FPL_KEYWORD_DECLARE)) {
            if(!scr->prog->foundstart) {
              /*
               * Only do this if this point isn't already known!
               * We move the pointer for the execution start position to
               * this position.
               */
              StoreBeginning(scr, text, prg);
            }
            /*
             * This is the end of the declaration phase. Now, let's
             * check for that FPLTAG_INTERPRET tag to see if we should
             * have a little fun or simply continue!
             */
            if(scr->interpret) {
              done = TRUE;
              continue;
            }
          }
        }
        if(ident && ident->flags&FPL_KEYWORD) {
          if(ident->flags&FPL_KEYWORD_DECLARE) {
            if(!declare)
              return FPLERR_ILLEGAL_DECLARE;
            CALL(Declare(val, scr, ident, control&SCR_GLOBAL?CON_DECLGLOB:0));
  
          } else {
            switch(ident->data.external.ID) {
            case CMD_SWITCH:
              scr->breaks++; /* allow another level of break */
              CALL(Switch(scr, val, control, con));
              if(CheckIt(scr, val, control, &ret)) {
                CleanUp(scr, control, levels);
                return(ret);
              }
              break;
  
            case CMD_CASE:    /* 'case' */
              if(!control&SCR_SWITCH)
                return FPLERR_ILLEGAL_CASE; /* 'case' not within switch! */
              /*
               * This word can only be found if (control&SCR_SWITCH), and then
               * we must just skip the "case XX:" text and continue.
               */
              CALL(Eat(scr));
              if(scr->text[0]==CHAR_OPEN_PAREN) {
                /*
                 * If this is an open parenthesis, we must search for the
                 * opposite parenthesis to enable conditional statements
                 * using the '?' and ':' operators.
                 */
                CALL(GetEnd(scr, CHAR_CLOSE_PAREN,
                            CHAR_OPEN_PAREN, FALSE)); /* find close paren! */
              }
              if(GetEnd(scr, CHAR_COLON, 255, FALSE)) /* find colon! */
                return FPLERR_MISSING_COLON;
              if(val->flags&FPL_STRING && !(val->flags&FPL_NOFREE) && val->val.str)
              /* If there was a string return, it should be freed and the
                 string really held a string! */
                FREE(val->val.str);
              break;
  
            case CMD_DEFAULT: /* 'default' */
              if(!control&SCR_SWITCH)
                return FPLERR_ILLEGAL_DEFAULT; /* 'default' not within switch! */
              /*
               * This word can only be found if (control&SCR_SWITCH), and then
               * we must just skip the "default:" text and continue.
               */
              if(scr->text[0]!=CHAR_COLON) {
                if(GetEnd(scr, CHAR_COLON, 255, FALSE))
                  return FPLERR_MISSING_COLON;
              } else
                scr->text++;
              break;
  
            case CMD_TYPEDEF:
              CALL(Getword(scr));
              CALL(GetIdentifier(scr, scr->buf, &ident));
              if(!ret &&
                 (ident->data.external.ID==CMD_INT ||
                  ident->data.external.ID==CMD_STRING)) {
                CALL(Getword(scr));
                text=(void *)ident;
                GETMEM(ident, sizeof(struct Identifier));
                *ident=*(struct Identifier *)text; /* copy entire structure! */
                GETMEM(ident->name, strlen(scr->buf)+1);
                strcpy(ident->name, scr->buf);
                ident->flags&=~FPL_INTERNAL_FUNCTION; /* no longer any internal
                                                         declarator symbol! */
                CALL(AddVar(scr, ident, &scr->locals));
              } else
                return FPLERR_IDENTIFIER_NOT_FOUND;
              break;
            case CMD_RETURN:
            case CMD_EXIT:
              Eat(scr);
              scr->breaks=0; /* reset number of allowed breaks */
              scr->returnint = NULL; /* point to result */
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
                      strtags[1]=(long)string->string;
                      strtags[3]=string->len;
                      CALL(Send(scr, strtags));
  
                      val->val.str=string;
                      val->flags&=~FPL_NOFREE;
                    }
                    else {
                      strtags[1]=(long)val->val.str->string;
                      strtags[3]=val->val.str->len;
                      CALL(Send(scr, strtags));
                    }
                  }
  
                } else {
                  CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
                  scr->returnint = &scr->FPLret; /* point to result */
                  inttags[1]=val->val.val;
                  CALL(Send(scr, inttags));
                }
                if(brace)
                  if(*scr->text!=CHAR_CLOSE_PAREN)
                    return FPLERR_MISSING_PARENTHESES;
                    /* continue */
                  else
                    scr->text++;
              } else {
                val->val.val=0;
                val->flags=0;
              }
              scr->FPLret=val->val.val;   /* set return code! */
              if(ident->data.external.ID==CMD_RETURN) {
                ret=FPL_OK;
              } else
                ret=FPL_EXIT_OK; /* This will make us return through it all! */
  
              val->flags|=FPL_RETURN; /* inform calling function */
  
              CleanUp(scr, control, levels);
              return(ret);
            case CMD_IF:          /* if() */
            case CMD_WHILE:       /* while() */
              Eat(scr);
  
              /*********************
  
                PARSE CONDITION
  
                *******************/
  
  
              if(*scr->text!=CHAR_OPEN_PAREN)
                return FPLERR_MISSING_PARENTHESES;
                /* please, go on! */
              else
                scr->text++;
  
              GETMEM(con2, sizeof(struct Condition));
  
              /* save check position! */
              con2->check=scr->text;
              con2->checkl=scr->prg;
  
              CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
              if(*scr->text!=CHAR_CLOSE_PAREN) {
                return FPLERR_MISSING_PARENTHESES;
                /* continue */
              } else
                scr->text++;
  
              if(val->val.val) {
                /********************
  
                  PARSE STATMENT
  
                  ******************/
  
                Eat(scr);
                scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
                con2->bracetext=scr->text;
                con2->braceprg=scr->prg;
  
                if(CMD_WHILE == ident->data.external.ID)
                  scr->breaks++; /* yet another break level */
                CALL(Script(scr, val,
                            (short)((brace?SCR_BRACE:0)|
                            (ident->data.external.ID==CMD_WHILE?SCR_WHILE:SCR_IF)),
                            con2));
                if(CheckIt(scr, val, control, &ret)) {
                  FREE(con2);
                  CleanUp(scr, control, levels);
                  return(ret);
                }
                brace=TRUE;
              } else {
                /********************
  
                  SKIP STATEMENT
  
                  ******************/
  
                CALL(SkipStatement(scr));
                brace=FALSE;
              }
  
              Eat(scr); /* we must eat space before storing the position,
                           otherwise we might eat newlines several times! */
              
              text=scr->text;
              prg=scr->prg;
  
              Getword(scr);
  
              if(!strcmp(KEYWORD_ELSE, scr->buf) && brace) {
                /********************
  
                  SKIP STATEMENT
  
                  ******************/
  
                CALL(SkipStatement(scr));
              } else if(!strcmp(KEYWORD_ELSE, scr->buf) && !brace) {
                /********************
  
                  PARSE STATMENT
  
                  ******************/
  
                Eat(scr);
                scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
                con2->bracetext=scr->text;
                con2->braceprg=scr->prg;
                CALL(Script(scr, val, (short)(brace?SCR_BRACE:0), con2));
                if(CheckIt(scr, val, control, &ret)) {
                  FREE(con2);
                  CleanUp(scr, control, levels);
                  return(ret);
                }
              } else {
                scr->text=text;
                scr->prg=prg;
              }
              FREE(con2);
              break;
            case CMD_BREAK:
              val->val.val=1;     /* default is break 1 */
              val->flags=0;       /* reset flags */
              CALL(Eat(scr));
              /*
               * Check if break out of several statements.
               */
              if(*scr->text!=CHAR_SEMICOLON) {
                /* Get the result of the expression. */
                brace=*scr->text==CHAR_OPEN_PAREN;
                scr->text+=brace;
                CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
                if(brace)
                  if(*scr->text!=CHAR_CLOSE_PAREN) {
                    return FPLERR_MISSING_PARENTHESES;
                  } else
                    scr->text++;
                else if(val->val.val<=0) {
                  return FPLERR_ILLEGAL_BREAK;
                }
              }
              /*
               * Check that the requested number of break levels is possible
               * to break out from!
               */
              if(scr->breaks < val->val.val)
                return FPLERR_ILLEGAL_BREAK;
  
              /*
               * Go to end of statement!!! If this was started without
               * SCR_BRACE set, we're already at the end of the statement!
               */
              if(control&SCR_BRACE) {
                if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, FALSE))
                  return FPLERR_MISSING_BRACE;
  #ifdef DEBUG_BREAKS
                fprintf(stderr, "First: levels %d line %d, brace? %d bl: %d\n",
                        val->val.val, scr->virprg, control&SCR_BRACE?1:0,
                        scr->breaks);
  #endif
              }
              if(control&SCR_DO)
                /* if it was inside a do statement, pass the ending `while' */
                CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));
              val->flags|=FPL_BREAK;
              if(control&(SCR_LOOP)) {
                scr->breaks--; /* decrease break level counter */
                if(!--val->val.val)
                  val->flags&=~FPL_BREAK; /* only this break! */
              }
              CleanUp(scr, control, levels);
              return(FPL_OK);
            case CMD_CONTINUE:
              if(*scr->text!=CHAR_SEMICOLON) {
                return FPLERR_MISSING_SEMICOLON;
              } else
                scr->text++;
              if(! scr->breaks)
                return FPLERR_ILLEGAL_CONTINUE;
              if(control&SCR_LOOP) {
  
                if(control&SCR_BRACE) {
                  DelLocalVar(scr, &scr->locals); /* delete all locals */
                  scr->varlevel--;                /* previous variable level */
                  scr->level--;                   /* previous level spectra */
                }
  
                /* loop! */
                CALL(Loop(scr, con, control, &brace));
                if(!brace) {
                  /*
                   * The result of the condition check was FALSE. Move to the end
                   * of the block and continue execution there!
                   */
  
                  if(control&SCR_BRACE) {
                    /* braces */
                    if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, FALSE))
                      return FPLERR_MISSING_BRACE;
                  }
                  val->flags=0;
                } else {
                  if(control&SCR_BRACE) {
                    /* bring back the proper values */
                    scr->varlevel++;
                    scr->level++;
                    AddLevel(scr); /* restart this level! */
                    declare=TRUE;
                  }
                  scr->virprg=virprg;
                  scr->virfile=virfile;
                  continue;
                }
              } else {
                /* it's no looping statement! */
  
                if(control&SCR_BRACE) {
                  /* braces */
                  if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, FALSE))
                    return FPLERR_MISSING_BRACE;
                }
                val->flags=FPL_CONTINUE;
                CleanUp(scr, control, levels);
              }
              return(FPL_OK);
            case CMD_DO:
              CALL(Eat(scr));
              GETMEM(con2, sizeof(struct Condition));
              scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
              con2->bracetext=scr->text;
              con2->braceprg=scr->prg;
              con2->check=NULL;
              scr->breaks++; /* increase break level */
              CALL(Script(scr, val, (short)(SCR_DO|(brace?SCR_BRACE:0)), con2));
              FREE(con2);
              if(CheckIt(scr, val, control, &ret)) {
                CleanUp(scr, control, levels);
                return(ret);
              }
              break;
            case CMD_FOR:
              Eat(scr);
              scr->text++;
              CALL(Expression(val, scr, CON_GROUNDLVL|CON_SEMICOLON, NULL));
  
              if(*scr->text!=CHAR_SEMICOLON) {
                return FPLERR_MISSING_SEMICOLON;
              } else
                scr->text++;
              GETMEM(con2, sizeof(struct Condition));
  
              con2->check=scr->text;
              con2->checkl=scr->prg;
              CALL(Expression(val, scr, CON_GROUNDLVL|CON_SEMICOLON|CON_NUM, NULL));
  
              if(*scr->text!=CHAR_SEMICOLON) {
                return FPLERR_MISSING_SEMICOLON;
              } else
                scr->text++;
              con2->postexpr=scr->text;
              con2->postexprl=scr->prg;
              {
                /*
                 * Pass the last expression:
                 */
                CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, FALSE));
              }
              if(!val->val.val) {
                /* We shouldn't enter the loop! Go to end of block:*/
                CALL(SkipStatement(scr));
                FREE(con2);
              } else {
                CALL(Eat(scr));
                scr->text+=(brace=*scr->text==CHAR_OPEN_BRACE);
                con2->bracetext=scr->text;
                con2->braceprg=scr->prg;
                scr->breaks++; /* increase break level */
                CALL(Script(scr, val, (short)((brace?SCR_BRACE:0)|SCR_FOR), con2));
                FREE(con2);
                if(CheckIt(scr, val, control, &ret)) {
                  CleanUp(scr, control, levels);
                  return(ret);
                }
              }
              break;
            case CMD_RESIZE:
              CALL(Resize(scr, val, control));
              break;
            } /* switch(keyword) */
          } /* if it wasn't a declaring keyword */
        } else {
          declare=FALSE;
          CALL(Expression(val, scr, CON_ACTION|(prg>=0?CON_IDENT:0), ident));
          /*
           * It it returned a string, flush it!
           */
          if(val->flags&FPL_STRING && !(val->flags&FPL_NOFREE) && val->val.str) {
            /* If there was a string return, it should be freed and the
               string really held a string! */
            FREE(val->val.str);
          }
          /*
           * Check for semicolon!
           */
          if(*scr->text!=CHAR_SEMICOLON)
            return FPLERR_MISSING_SEMICOLON;
          else
            scr->text++;
        }
      } /* switch (*scr->text) */
  
      if(!(control&(SCR_BRACE|SCR_SWITCH))) {
        if(control&SCR_LOOP) {
          CALL(Loop(scr, con, control, &brace));
          if(brace) {
            /* Yes! We should loop! */
            if(control&SCR_BRACE) {
              /* bring back the proper values */
              scr->varlevel++;
              scr->level++;
              AddLevel(scr); /* restart this level! */
              declare=TRUE;
            }
            scr->virprg=virprg;
            scr->virfile=virfile;
            continue;
          }
          val->flags=0;
          ret=FPL_OK;
          break; /* return to calling function */
        } else
          break;
      }
    } /* loop! */
  
    if(!ret &&
       control&SCR_FILE &&
       !scr->prog->foundstart &&
       !done) {
      /*
       * We did get here by hitting end of program.
       * Let's set the start-of-main position right here to
       * make another run work fine on this file too!
       */
      StoreBeginning(scr, scr->text, scr->prg);
    }

  } /* for the if(compiled)-else */

  /*
   * Check for that FPLTAG_INTERPRET tag!
   */
  if(ret<=FPL_EXIT_OK && scr->interpret) {
    /* an alternative main program is specified */
    GETMEM(pass, sizeof(struct fplArgument));
    pass->ID=FNC_INTERPRET;
    text = scr->interpret;
    pass->argv=(void **)&text;
    pass->key=scr;
    scr->interpret=NULL; /* disable recursion! */
    CALL(functions(pass));

    CleanUp(scr, control, levels);

    /* we're done for this time, exit! */
    ret = FPL_EXIT_OK;
  }

  CleanUp(scr, control, levels);
  return(ret);
}

static REGARGS void
StoreBeginning(struct Data *scr, char *text, long prg)
{
  scr->prog->startcol=text-(&scr->prog->program)[prg-1];
  scr->prog->startprg=prg;
  scr->prog->virprg=scr->virprg;
  scr->prog->virfile=scr->virfile;
  scr->prog->foundstart=TRUE;

  /* fprintf(stderr, "Setexp:%s", text); */
}

static ReturnCode INLINE
Switch(struct Data *scr,
       struct Expr *val,
       short control,
       struct Condition *con)
{
  ReturnCode ret;
  struct fplStr *string;
  long value;
  uchar strtype=FALSE;
  uchar breakout=FALSE;

  /* temporary storage variables */
  uchar *ttext;
  long tprg;
  uchar *tvirfile;
  long tvirprg;

  uchar end=FALSE; /* we have not found the end position */

  long bprg;
  uchar *btext;
  long bvirprg;
  uchar *bvirfile;

  long dprg=-1;
  uchar *dtext;
  long dvirprg;
  uchar *dvirfile;

  CALL(Eat(scr)); /* eat whitespace */

  /* Check the open parenthesis */
  if(scr->text[0]!=CHAR_OPEN_PAREN) {
    return FPLERR_MISSING_PARENTHESES;
  } else
    scr->text++;

  /* Get expression, string or int, static or dynamic! */
  CALL(Expression(val, scr, CON_NORMAL, NULL));

  if(val->flags&FPL_STRING) {
    /* there was a string statement! */
    string = val->val.str;
    if(string)
      strtype=2;
    else
      strtype= 1;

  } else {
    /* there was an integer expression */
    value = val->val.val;
  }

  /* Check the close parenthesis */
  if(scr->text[0]!=CHAR_CLOSE_PAREN) {
    return FPLERR_MISSING_PARENTHESES;
  } else
    scr->text++;

  CALL(Eat(scr)); /* eat whitespace */

  /* Check the open brace */
  if(scr->text[0]!=CHAR_OPEN_BRACE) {
    return FPLERR_MISSING_BRACE;
  } else
    scr->text++;

  while(!(ret=Eat(scr))) {
    tprg = scr->prg;
    ttext = scr->text;
    tvirprg = scr->virprg;
    tvirfile = scr->virfile;
    if(!Getword(scr)) {
      if(!strcmp("case", scr->buf)) {
        /* This is a valid case-line coming up! */

        /* Get expression, string or int! */
        CALL(Expression(val, scr, strtype?CON_STRING:CON_NUM, NULL));
        if(strtype) {
          /*
           * String comparison:
           */
          value = val->val.str?val->val.str->len:0;

          if(value == (string?string->len:0)) {

            if(value) {
              if(!memcmp(val->val.str->string, string->string, value)) {
                /* match! */
                breakout=TRUE;
              }
            } else
              breakout=TRUE;
          }
          if(!val->flags&FPL_NOFREE)
            FREE(val->val.str);
          if(breakout)
            break;
          else
            scr->text++; /* pass the ';' */
        } else {
          /*
           * Integer comparison:
           */
          if(val->val.val == value) {
            breakout = TRUE;
            break;
          } else
            scr->text++; /* pass the ';' */
        }
      } else if(!strcmp("default", scr->buf)) {
        /*
         * Store the default position to make it possible to return to if
         * necessary!
         */

	if(dprg>=0)
	  return FPLERR_ILLEGAL_DEFAULT; /* dual 'default' specified! */

        dprg = scr->prg;
        dtext = scr->text++; /* pass the colon after the assign */
        dvirprg = scr->virprg;
        dvirfile = scr->virfile;

      } else {
        /*
         * Pass the statement!
         */

        /* First, restore the previuos position so that we can skip
           if, while, do and such things without problems! */
        scr->prg=tprg;
        scr->text=ttext;
        scr->virprg=tvirprg;
        scr->virfile=tvirfile;

        CALL(SkipStatement(scr));
      }
    } else {
      /* we didn't get any word */
      if(scr->text[0]==CHAR_CLOSE_BRACE) {
        /*
         * We hit the end without finding our 'case'! Return to the
         * 'default', if any! Store the position to be able to quickly
         * jump down to it again after the possible case-statement.
         */

        scr->text++; /* pass the closing brace */
        if(dprg<0)
          /* we didn't find any 'default' */
          break;
        bprg = scr->prg;
        btext = scr->text;
        bvirprg = scr->virprg;
        bvirfile = scr->virfile;

        end=TRUE; /* we have found the end! */

        scr->prg=dprg;
        scr->text=dtext;
        scr->virprg=dvirprg;
        scr->virfile=dvirfile;
        breakout = TRUE;
        break;

      } else {
        /*
         * Pass the statement!
         */
        CALL(SkipStatement(scr));
      }
    }
  }
  if(breakout) {
    /* we did break out on any of the 'case' or 'default' label lines,
       pass the colon!
     */
    /* CALL(Eat(scr));  eating whitespace shouldn't be necessary here */

    /* Check the colon */
    if(scr->text[0]!=CHAR_COLON) {
      return FPLERR_MISSING_COLON;
    } else
      scr->text++;

    /*
     * run this statement all the way until break or '}'!
     */

    CALL(Script(scr, val, SCR_SWITCH, con));

    if(!(val->flags&FPL_BRACE)) {
      /* we didn't run into the closing brace! */

      if(val->flags&FPL_BREAK) {
        /*
         * We got here after hitting a 'break' !!
         */
        scr->breaks--; /* decrease break level counter */
        if(!--val->val.val)
          val->flags&=~FPL_BREAK; /* only this break and no more ! */
      }

      /*
       * Go to the end of the switch()-statement.
       */
      if(!end) {
        /* we'll have to search for it! */
        if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, FALSE))
          return FPLERR_MISSING_BRACE;
      } else {
        scr->prg=bprg;
        scr->text=btext;
        scr->virprg=bvirprg;
        scr->virfile=bvirfile;
      }
    }

  }
  return ret;
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
      if(!(scr->flags&FPLDATA_ISOLATE))
        /* don't do this while running in isolate mode! */
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
  if(*scr->text!=CHAR_SEMICOLON &&
     (!(val->flags&FPL_DEFUNCTION) || *scr->text!=CHAR_CLOSE_BRACE)) {
    return FPLERR_MISSING_SEMICOLON;
  } else
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
  CALL(GetIdentifier(scr, scr->buf, &ident));
  var=&ident->data.variable;

  if(!(ident->flags&FPL_VARIABLE) || !var->num) {
    return FPLERR_ILLEGAL_RESIZE;
  }

  Eat(scr);
  GETMEM(dims, MAX_DIMS*sizeof(long));

  do {
    if(*scr->text!=CHAR_OPEN_BRACKET) {
      return FPLERR_MISSING_BRACKET;
    } else
      scr->text++; /* pass the open bracket */
    /* eval the expression: */
    CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
    if(*scr->text++!=CHAR_CLOSE_BRACKET)
      /* no close bracket means error */
      return(FPLERR_MISSING_BRACKET); /* missing bracket */
    else if(val->val.val<(control&CON_DECLARE?1:0)) {
      /* illegal result of the expression */
      /*
       * Set back original variable name!
       */
      strcpy(scr->buf, ident->name);
      return(FPLERR_ILLEGAL_ARRAY);
    }
    dims[num++]=val->val.val; /* Add another dimension */
    if(num==MAX_DIMS) {
      /* if we try to declare too many dimensions... */
      /*
       * Set back original variable name!
       */
      strcpy(scr->buf, ident->name);
      return FPLERR_ILLEGAL_ARRAY;
    }
    /*
     * Go on as long there are brackets,
     */
  } while(*scr->text==CHAR_OPEN_BRACKET);

  CALL(ArrayResize(scr, num, dims, ident));

  FREE(dims);
  return(FPL_OK);
}


ReturnCode REGARGS
ArrayResize(struct Data *scr,
            long num,   /* number of new dimensions */
            long *dims, /* array of new dim sizes */
            struct Identifier *ident) /* _valid_ variable to resize */
{
  long size;
  long i;
  long min;
  void *tempvars;
  struct fplVariable *var;
  uchar dynamic=FALSE;
  var=&ident->data.variable;
  
  size=dims[0]; /* array size */
  for(i=1; i<num; i++)
    size*=dims[i];

  min=MIN(size, var->size); /* number of variables to copy! */

  if(MALLOC_DYNAMIC == TypeMem(ident)) {
    dynamic = TRUE;
    GETMEM(tempvars, size * sizeof(void *)); /* data adjust! */
  }
  else {
    GETMEMA(tempvars, size * sizeof(void *)); /* data adjust! */
  }
  memcpy(tempvars, var->var.str, min * sizeof(void *));
  if(size>var->size)
    /*
     * If we create a few more than before, empty that data!
     */
    memset((uchar *)tempvars+var->size*sizeof(void *), 0,
	   (size-var->size)*sizeof(void *));

  if(ident->flags&FPL_STRING_VARIABLE)
    for(i=min; i<var->size; i++) {
      if(var->var.str[i]) {
	FREE_KIND(var->var.str[i]);
      }
    }

  FREE_KIND(var->var.val);
  var->var.val= tempvars;

  var->size= size;
  var->num = num;
  if(var->dims)
    FREE_KIND(var->dims);
  if(dynamic) {
    GETMEM(var->dims, num * sizeof(long));
  }
  else {
    GETMEMA(var->dims, num * sizeof(long));
  }
  memcpy(var->dims, dims, num * sizeof(long));

  return FPL_OK;
}

/**********************************************************************
 *
 * char CheckIt()
 *
 * Returns wether we should return from this Script().
 *
 *****/

static uchar REGARGS
CheckIt(struct Data *scr, /* major script structure */
        struct Expr *val, /* result structure */
        short control,    /* control defines */
        ReturnCode *ret)  /* return code pointer */
{
  if(val->flags&FPL_BREAK) {
    /*
     * A `break' was hit inside that Script() invoke.
     */
    if(control&SCR_BRACE) {
      /*
       * If we're inside braces, search for the close brace!
       */
      if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, FALSE)) {
	*ret = FPLERR_ILLEGAL_BREAK;
	return((uchar)*ret);
      }
    }
#ifdef DEBUG_BREAKS
    fprintf(stderr, "EOS: levels %d line %d, brace? %d bl: %d\n",
	    val->val.val, scr->virprg, control&SCR_BRACE?1:0,
	    scr->breaks);
#endif

    if(control&(SCR_LOOP)) {
      scr->breaks--; /* decrease break level counter */
      if(control&SCR_DO) {
        /*
         * We're inside a do-statement! We must pass the ending "while"
         * before returning! We do it the easy way: look for the closing
         * parenthesis!
         */
	if(*ret=GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE))
	  return((uchar)*ret);
        else if(*ret = Eat(scr))
	  return((uchar)*ret);
        else if(scr->text[0] != CHAR_SEMICOLON) {
          return FPLERR_MISSING_SEMICOLON;
        } else
          scr->text++; /* pass the semicolon */
      }
      if(--val->val.val<1)
	val->flags&=~FPL_BREAK; /* clear the break bit! */
      return(TRUE);
    } else if(!(control&SCR_FUNCTION))
      return(TRUE);
    else if(val->val.val<2) {
      val->flags&=~FPL_BREAK; /* clear the break bit! */
      return(FALSE); /* no more break! */
    }
    *ret=FPLERR_ILLEGAL_BREAK;
    return(TRUE);
  } else if(val->flags&FPL_RETURN)
    /* The FPL function did end in a return() */
    return(TRUE);
  else if(val->flags&FPL_CONTINUE) {
    if(control&SCR_LOOP) {
      if(control&SCR_BRACE) {
	/* If we're inside braces, search for the close brace */
	if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, FALSE)) {
          *ret = FPLERR_MISSING_BRACE;
	  return((uchar)*ret);
        }
	scr->text--; /* move one step back to stand on the close brace */
	return(FALSE);
      }
    } else
      /* this is not a looping block, break out of it! */
      return(TRUE);
  }
  return(FALSE);
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
    DelLocalVar(scr, &scr->locals);
    scr->varlevel--;
    scr->level=levels; /* new variable amplitude */
  }

  if(!(control&SCR_DEBUG)) {
    /* previous version did not run in debug mode, switch it off! */
    scr->flags&=~FPLDATA_DEBUG_MODE;
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
     struct Condition *con,
     short control,
     uchar *cont) /* store TRUE or FALSE if loop or not */
{
  ReturnCode ret = FPL_OK;
  uchar *temptext=scr->text; /* store current position */
  long temprg=scr->prg;
  struct Expr *val;

  GETMEM(val, sizeof(struct Expr));

  /*
   * First check if the block just parsed begun with a while() or for()
   * or perhaps a do in which we know the statment position!
   */

  if((control&SCR_WHILE ||
      control&SCR_FOR ||
      (control&SCR_DO && con->check))) {
    if(control&SCR_FOR) {	 /* check if the pre keyword was for() */
      scr->text=con->postexpr;/* perform the post expression */
      scr->prg=con->postexprl;
      CALL(Expression(val, scr, CON_GROUNDLVL|CON_PAREN, NULL));
    }
    /*
     * Do the condition check. The only statement if it was a while() or
     * do while or the second statement if it was a for().
     *
     * If it was a for() as pre statement, the statement could contain
     * nothing but a semicolon and then equals TRUE.
     */
    scr->text=con->check;
    scr->prg=con->checkl;
    CALL(Expression(val, scr, CON_GROUNDLVL|
		    (control&SCR_FOR?CON_SEMICOLON:0)|CON_NUM, NULL));

    if(val->val.val) { /* the result of the condition was true */
      scr->text=con->bracetext; /* return to the open brace */
      scr->prg=con->braceprg;
      *cont=TRUE;
      FREE(val);
      return(FPL_OK);
    }
  }

  if(control&SCR_DO) {
    /* This a do while end. */

    if(!con->check) {
      /*
       * We *DON'T* know the condition position. We have to scan forward
       * to get it!
       */
      if(*scr->text==CHAR_CLOSE_BRACE)
	/* pass the close brace */
	scr->text++;
      if(ret=Getword(scr))
	;
      else if(strcmp(scr->buf, "while"))
	ret=FPLERR_MISSING_WHILE; /* missing 'while' after do-while statement */
      else if(ret=Eat(scr))
	;
      else if(*scr->text++!=CHAR_OPEN_PAREN)
	ret=FPLERR_MISSING_PARENTHESES; /* >warning< */
      else {
	con->check=scr->text;
	con->checkl=scr->prg;
	if(ret=Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL))
	  ;
	else if(*scr->text++!=CHAR_CLOSE_PAREN)
	  ret=FPLERR_MISSING_PARENTHESES; /* >warning< */
      }
      if(ret)
	return(ret);
    }
    if(!val->val.val) {
      /*
       * If we had the check point up there and the condition equaled
       * FALSE. Now we have to pass the the while keyword following the
       * close brace.
       */
      scr->text=temptext;
      scr->prg=temprg;

      if(*scr->text==CHAR_CLOSE_BRACE)
	/* pass the close brace */
	scr->text++;

      if(Getword(scr) || strcmp("while", scr->buf))
	ret=FPLERR_MISSING_WHILE; /* missing 'while' after do-while statement */
      else if(GetEnd(scr, CHAR_SEMICOLON, (uchar)255, FALSE))
	ret = FPLERR_MISSING_SEMICOLON;
      if(ret)
	return(ret);
    } else {
      /* go to the open brace */
      scr->text=con->bracetext;
      scr->prg=con->braceprg;
      *cont=TRUE;
      FREE(val);
      return(FPL_OK);
    }
  }

  FREE(val);

  /*
   * The condition check has failed!
   */

  *cont=FALSE;

  if(!(control&SCR_DO)) {
    /* it's not a do-while loop */

    scr->text=temptext;
    scr->prg=temprg;

    Eat(scr);

    if(control&SCR_BRACE && *scr->text==CHAR_CLOSE_BRACE)
      /* pass the close brace */
      scr->text++;
  }

  return(ret);
}

/**********************************************************************
 *
 * ReturnCode SkipStatement();
 *
 *  This function should pass one statement. Statements starting with
 * "for", "do", "while" or "if" really can be meesy and in such cases
 * this function recurse extensively!!!
 *
 ******/

static ReturnCode REGARGS
SkipStatement(struct Data *scr)
{
  ReturnCode ret;
  struct Identifier *ident;
  CALL(Eat(scr));

  if(*scr->text==CHAR_SEMICOLON)
    scr->text++;
  else if(*scr->text==CHAR_OPEN_BRACE) {
    if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, TRUE))
      return FPLERR_MISSING_BRACE;
  } else {
    /*
     * Much more trouble this way:
     */

    uchar *t;
    long p;

    ret = Getword(scr);
    if(!ret) {
      GetIdentifier(scr, scr->buf, &ident);
      switch(ident?ident->data.external.ID:0) {
      case CMD_IF:
      case CMD_WHILE:
        Eat(scr);
        CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));
        CALL(SkipStatement(scr));
	CALL(Eat(scr));
        t=scr->text;
        p=scr->prg;

        if(!Getword(scr) && !strcmp(KEYWORD_ELSE, scr->buf)) {
          CALL(SkipStatement(scr));
        } else {
          /*
           * Restore pointers.
           */
          scr->text=t;
          scr->prg=p;
        }
        break;
      case CMD_FOR:
      case CMD_SWITCH:
        Eat(scr);
        /* Now we must stand on an open parenthesis */
        CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));
        CALL(SkipStatement(scr));
        break;
      case CMD_DO:
        Eat(scr);
        CALL(SkipStatement(scr));

        /*
         * The next semicolon must be the one after the
         * following `while' keyword!
         */
        if(GetEnd(scr, CHAR_SEMICOLON, 255, FALSE))
          return FPLERR_MISSING_SEMICOLON;
        break;
      default:
        ret=TRUE;
      }
    }
    if(ret) {
      /*
       * This statement ends at the next semicolon
       */
      if(GetEnd(scr, CHAR_SEMICOLON, 255, FALSE))
        return FPLERR_MISSING_SEMICOLON;
    }
  }
  return(FPL_OK);
}

#if defined(UNIX) || defined(WIN32)
long InterfaceCall(struct Data *scr,
		   void *arg,
		   long (*func)(void *))
{
  return func(arg);
}
#endif
