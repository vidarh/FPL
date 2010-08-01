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

int CXBRK(void) { return(0); }  /* Disable Lattice/SAS CTRL/C handling */
int chkabort(void) { return(0); }  /* really */

#include <exec/libraries.h>
#include <libraries/dos.h>

#include <pragmas/FPL_pragmas.h>
#include <clib/FPL_protos.h>
#include <libraries/FPL.h>
struct Library *FPLBase = NULL;

#define REG(x) register __ ## x

#elif defined(UNIX) /* #ifdef AMIGA */

#error can't be compiled!

#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#define CALLER __saveds
#define ASM __asm

long ASM func(REG(a0) struct fplArgument *);


enum myfunctions {
  FN_OUTPUT,
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
  long end=0;

#if defined(AMIGA)
  if(!(FPLBase=OpenLibrary(FPLNAME, 7))) {
    printf("Error opening %s!\n", FPLNAME);
    return(-1);
  }
  printf("--> %s\n", FPLBase->lib_IdString);
#endif

  if(argc<2) {
    printf("Usage: test <FPL program file name>\n");
#if defined(AMIGA)
    CloseLibrary((struct Library *)FPLBase);
#endif
    return 0;
  }

  key=fplInitTags(func,
                  FPLTAG_STACK, 20000,
                  FPLTAG_MINSTACK, 4000,
                  FPLTAG_END);

  fplAddFunction(key, "output",	   FN_OUTPUT,    'I', "O", NULL);

  end=fplExecuteFile(key, argv[1], NULL);
  
  fplFree(key); /* free all */

#if defined(AMIGA)
  CloseLibrary((struct Library *)FPLBase);
#endif
  return end;
}

long ASM func(REG(a0) struct fplArgument *arg)
{
  int ret;
  long col;
  char *name;
  char *string;
  void *anchor=arg->key;
  switch(arg->ID) {
  case FN_OUTPUT: /* output */
    if(arg->format[0]==FPL_STRARG)  /* we got a string! */
      string="%s";
    else
      string="%d";
#if defined(AMIGA)
    printf(string, arg->argv[0]);
#elif defined(UNIX)
    fprintf(stderr, string, arg->argv[0]);
#endif
    fplSendTags(anchor, FPLSEND_INT, 1, FPLSEND_DONE);
#if 0
    if(count++>10)
      return(FPLERR_PROGRAM_STOPPED);
#endif
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
  }
  return(0);
}
