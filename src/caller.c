/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 caller.c

 For FPL debugging...
 Only part of the executable file version of FPL.

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
 * FidoNet 2:201/328    email:dast@sth.frontec.se                       *
 *                                                                      *
 ************************************************************************/


#include "FPL.h"
#include "reference.h"		/* should be <FPL/reference.h> */

#include "config.h"      /* system dependent setups */

#ifdef AMIGA
#include <exec/types.h>
#ifndef __AROS__
#include <proto/exec.h>
#endif

int CXBRK(void) { return(0); }  /* Disable Lattice/SAS CTRL/C handling */
int chkabort(void) { return(0); }  /* really */
#ifdef SHARED
#include <exec/libraries.h>
#include <libraries/dos.h>

#include "/include/pragmas/FPL_pragmas.h"
#include "/include/clib/FPL_protos.h"
struct Library *FPLBase = NULL;
#endif

#define REG(x) register __ ## x

#else
#include <sys/types.h>

#define REG(x)
#define TRUE  1
#define FALSE 0
#include "../include/clib/FPL_protos.h"

#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if defined(AMIGA) && defined(SHARED)
#define CALLER __saveds
#define ASM __asm
#else
#define CALLER
#define ASM
#endif

#include "script.h"

long ASM func(REG(a0) struct fplArgument *);
long ASM inter(REG(a0) void *);
void CALLER ASM MyFree(REG(a1) void *, REG(d0) long);
void CALLER ASM *MyAlloc(REG(d0) long);
long ASM newline(REG(a0) void *);

int memory_counter=0;
int maxmemory_counter=0;
int mallocs=0;

int newlines=0;

enum myfunctions {
  FN_GETINT,
  FN_GETSTRING,
  FN_OUTPUT,
  FN_EXECUTE,
  FN_PRINTF,
  FN_TEST,
  FN_OPENL,
  FN_CLOSEL,

  VR_TEST /* first variable ever! */

  };



/**********************************************************************
 *
 * int main(int, char **)
 *
 * This function is not included in the run time library version.
 *
 ******/

void *key;

int main(int argc, char **argv)
{
  long n, end=0;
  long count=0;
  int pre_mallocs;
  long pre_malloc;
  struct fplSymbol *symbols;
  long i;
  long Version_I,Revision_I;

#if defined(SHARED) && defined(AMIGA)
  if(!(FPLBase=OpenLibrary(FPLNAME, 5))) {
    printf("Error opening %s!\n", FPLNAME);
    return(-1);
  }
#endif

  if(argc<2) {
    printf("Usage: SFPL <FPL program file name>\n");
#if defined(AMIGA) && defined(SHARED)
    CloseLibrary((struct Library *)FPLBase);
#endif
    return 0;
  }

  key=fplInitTags(func,
#if 0
		  FPLTAG_INTERVAL, (unsigned long)inter, /* interval func */
		  FPLTAG_USERDATA, (unsigned long)&count, /* user data */
		  FPLTAG_INTERNAL_DEALLOC, (unsigned long)MyFree,
		  FPLTAG_INTERNAL_ALLOC, (unsigned long)MyAlloc,
		  FPLTAG_MINSTACK, 7000,
#endif
		  FPLTAG_CACHEALLFILES, FPLCACHE_EXPORTS,
                  FPLTAG_REREAD_CHANGES, TRUE,
                  FPLTAG_AUTORUN, TRUE,
#if 0
                  FPLTAG_IDENTITY, argv[0], /* identify us! */
		  FPLTAG_DEBUG, TRUE,  /* run in debug mode! */
/*                  FPLTAG_AUTOCOMPILE, TRUE, */
#endif
		  FPLTAG_DONE);

  fplSendTags( key, FPLSEND_GETVERSION, &Version_I,
                    FPLSEND_GETREVISION, &Revision_I, FPLSEND_DONE);
  printf("Using FPL version %d.%d.\n",Version_I,Revision_I);
  
  fplAddFunction(key, "getint",    FN_GETINT,    'I', "s", NULL);
  fplAddFunction(key, "getstring", FN_GETSTRING, 'S', "s", NULL);
  fplAddFunction(key, "output",	   FN_OUTPUT,    'S', "O", NULL);
  fplAddFunction(key, "runfile",   FN_EXECUTE,   'I', "S", NULL);
  fplAddFunction(key, "printf",    FN_PRINTF,    'I', "So>", NULL);
  fplAddFunction(key, "array",     FN_TEST,      'I', "R", NULL);  

  fplAddVariable(key, "_TEST_",    VR_TEST,      'S', NULL, NULL);

  pre_mallocs=mallocs;
  pre_malloc=memory_counter;

#if 1
  if(!end && argc>1) {
    int i;
    for(i=1; i<argc; i++) {
      char *string=NULL;
      end=fplExecuteFileTags(key, argv[i],
                             /* FPLTAG_ISOLATE, TRUE, */
                             FPLTAG_STRING_RETURN, &string,
                             FPLTAG_DONE);
      if(string) {
        printf("The '%s' program returned '%s'\n", argv[i], string);
        fplFreeString(key, string);
      }
    }
  }
#else
  {
    char *program1 =
      "int export amiga()\n{ Request(\"hej\"); }";
    int a;
    for(a=0; a<100; a++) {
      fplExecuteScriptTags(key, &program1, 1,
                           FPLTAG_KIDNAP_CACHED, TRUE,
                           FPLTAG_CACHEFILE, FPLCACHE_EXPORTS,
                           FPLTAG_PROGNAME, "program1",
                           FPLTAG_DONE);
      fplSendTags(key, FPLSEND_FREEFILE, (long)"program1",
                  FPLTAG_DONE);
    }
  }
#endif
  if(end)
    printf("exit code: %d\n", end);
#if 0  
  fplSendTags(key, FPLSEND_GETSYMBOL_FUNCTIONS, &symbols, FPLSEND_DONE);
  printf("\n---------------------\nAll exported functions:\n");
  for(i=0; i<symbols->num; i++)
    printf("%s ", symbols->array[i]);
  fplSendTags(key, FPLSEND_GETSYMBOL_FREE, symbols, FPLSEND_DONE);

  fplSendTags(key, FPLSEND_GETSYMBOL_VARIABLES, &symbols, FPLSEND_DONE);
  printf("\n---------------------\nAll exported variables:\n");
  for(i=0; i<symbols->num; i++)
    printf("%s ", symbols->array[i]);
  fplSendTags(key, FPLSEND_GETSYMBOL_FREE, symbols, FPLSEND_DONE);

  fplSendTags(key, FPLSEND_GETSYMBOL_CACHEDFILES, &symbols, FPLSEND_DONE);
  printf("\n---------------------\nAll cached files:\n");
  for(i=0; i<symbols->num; i++)
    printf("%s ", symbols->array[i]);
  fplSendTags(key, FPLSEND_GETSYMBOL_FREE, symbols, FPLSEND_DONE);
#endif

  fplSendTags(key,
	      FPLSEND_GETRETURNCODE, &n,
	      FPLSEND_FLUSHFILE, 0,
	      FPLSEND_FLUSHCACHE, 1,
	      FPLSEND_DONE);

  fplFree(key); /* free all shit FPL uses internally */

#if defined(AMIGA) && defined(SHARED)
  CloseLibrary((struct Library *)FPLBase);
#endif

#ifdef DEBUG_INFO  
  printf("\n-----------------------------------------\n");
  printf("Return code   :  %d\n", n);
  printf("Interval func :  %d\n", count);
  printf("Newlines      :  %d\n", newlines);
  printf("Pre mallocs   :  %d\n", pre_mallocs);
  printf("Pre memory use:  %d\n", pre_malloc);
  printf("Malloc        :  %d\n", mallocs-pre_mallocs);
  printf("memory use    :  %d\n", maxmemory_counter-pre_malloc);
  printf("-----------------------------------------\n");
#endif

  if(memory_counter)
    printf(">ALARM!!< Not freed mem :  %d bytes!\n", memory_counter);

  return end;
}



#define TEST_STRING "I_returned_this_from_the_interface_function!"

long ASM func(REG(a0) struct fplArgument *arg)
{
  int ret;
  long col;
  char *name;
  char *string;
  void *anchor=arg->key;
  char systemline[80];
  switch(arg->ID) {
  case VR_TEST:
    fplSendTags(anchor, FPLSEND_STRING, "internal variable!", FPLSEND_DONE);
    break;
  case FN_TEST:
    {
      long new[2]={0,-1};
      long *integer;
      fplReferenceTags(anchor, arg->argv[0],
                       FPLREF_NAME, &name,
		       FPLREF_TYPE, &col,
		       FPLREF_GET_STRING, &string,
		       FPLREF_GET_INTEGER, &integer,
		       FPLREF_DONE);
      printf("Received a reference to the %s %svariable called '%s'\n",
             col&FPLREF_TYPE_STRING?"string":"integer",
             col&FPLREF_TYPE_ARRAY?"array ":"",
             name);

      if(col&FPLREF_TYPE_ARRAY) {
	struct fplRef ref;
	fplReferenceTags(anchor, arg->argv[0],
			 FPLREF_ARRAY_INFO, &ref,
			 FPLREF_DONE);
	if(1 == ref.Dimensions) {
	  long dims[]={0, -1};
	  for(col=0; col<ref.ArraySize[0]; col++) {
	    dims[0]=col;
	    fplReferenceTags(anchor, arg->argv[0],
			     FPLREF_ARRAY_ITEM, &dims[0],
			     FPLREF_GET_STRING, &name,
			     FPLREF_DONE);
	    if(name[0])
	      printf("#%d: %s\n", col, name);
	  }
	  new[0] = 5;
	  fplReferenceTags(anchor, arg->argv[0],
			   FPLREF_ARRAY_ITEM, &dims[0],
			   FPLREF_SET_MY_STRING, "externally set",
			   FPLREF_DONE);
          ref.ArraySize = new;
          new[0] = 20;
	  fplReferenceTags(anchor, arg->argv[0],
                           FPLREF_ARRAY_RESIZE, &ref,
			   FPLREF_DONE);
	}
      }
      else if(col&FPLREF_TYPE_STRING) {
        printf("It holds the string '%s'\n", string);
	if(string=(char *)fplAllocString(anchor, strlen(TEST_STRING))) {
          strcpy(string, TEST_STRING);
	  fplReferenceTags(anchor, arg->argv[0],
	                   FPLREF_SET_STRING, string,
		           FPLREF_END);
	  fplReferenceTags(anchor, arg->argv[0],
	                   FPLREF_SET_MY_STRING, TEST_STRING,
		           FPLREF_END);
	}
      } else if(col&FPLREF_TYPE_INTEGER) {
        printf("It holds the number %d\n", *integer);      
      }
    }
    break;

  case FN_PRINTF:
    vprintf(arg->argv[0], (char *)&arg->argv[1]);
    break;
  case FN_OUTPUT: /* output */
    if(arg->format[0]==FPL_STRARG)  /* we got a string! */
      string="%s";
    else
      string="%d";
    printf(string, arg->argv[0]);
    fplSendTags(arg->key,
                FPLSEND_STRING, "returned",
		FPLSEND_DONE);
    break;
    
  case FN_GETSTRING:
    if(string=(char *)fplAlloc(anchor, 64)) {
      if(arg->argc)
	printf("%s", arg->argv[0]);
      fgets(string, 64, stdin);
      ret=fplSendTags(arg->key,
		      FPLSEND_STRING, string,
		      FPLSEND_STRLEN, strlen(string)-1,
		      FPLSEND_DONE);
      fplDealloc(anchor, string);
    } else
      ret=FPLERR_OUT_OF_MEMORY;
    return(ret);

  case FN_EXECUTE:
    ret=fplExecuteFile(anchor, arg->argv[0], NULL);
    return(ret);
    break;

  case FPL_GENERAL_ERROR:
    {
      char buffer[FPL_ERRORMSG_LENGTH];
      fplSendTags(anchor,
		  FPLSEND_GETVIRLINE, &col,
		  FPLSEND_GETVIRFILE, &name,
		  FPLSEND_DONE);
      if(*name=='\"') {
	ret=0;
	name++;
	while(name[ret] && name[ret]!='\"')
	  ret++;
	string=(char *)fplAlloca(anchor, ret+1);
	memcpy(string, name, ret);
	string[ret]='\0';
      } else {
	string=name;
	ret=0;
      }
      printf("\n>>> %s\n",
	     fplGetErrorMsg(arg->key, (long)arg->argv[0], buffer));
      printf(">>> Line %d in file \"%s\". <<<\n", col, string);
      if(ret)
	fplDealloca(anchor, string);
    }
    break;

  case FPL_UNKNOWN_FUNCTION:
    col=22; /* only to breakpoint */
    break;

  case FN_GETINT:
    if(arg->argc)
      printf("%s", (char *)arg->argv[0]);
    scanf("%d", &ret);
    ret=fplSendTags(anchor, FPLSEND_INT, ret, FPLSEND_DONE);
    return(ret);
  }
  return FPL_OK;
}

long ASM inter(REG(a0) void *count)
{
  static line=-1;
#if 0
  static char *name;
  long current_line;
  char *curr_name;

  fplSendTags(key, FPLSEND_GETVIRLINE, &current_line, FPLSEND_DONE);
  fplSendTags(key, FPLSEND_GETVIRFILE, &curr_name, FPLSEND_DONE);
  if(line!=current_line || name != curr_name) {
    line=current_line;
    name=curr_name;
/*    fprintf(stderr, " < %d %s > ", line, name?name:"unknwon"); */
  }
#endif

  (*(int *)count)++; /* just to count the number of times this routine has been
			called. */
  return(0);
}

void CALLER ASM MyFree(REG(a1) void *pntr, REG(d0) long size)
{
  memory_counter-=size;
  memset(pntr, 0xaa, size); /* mess up this area before free! */
  free(pntr);
}

void CALLER ASM *MyAlloc(REG(d0) long size)
{
  void *mem;
  mallocs++;
  if((memory_counter+=size)>maxmemory_counter)
    maxmemory_counter=memory_counter;
  mem=malloc(size);
  if(mem)
    memset(mem, 0xaa, size); /* mess up this area before free! */
  return (mem);
}
