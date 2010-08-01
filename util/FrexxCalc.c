;/* Execute me to compile with Lattice 5.10 or SAS/C 6.0
if exists SC:
   sc DEFINE=AMIGA STRINGMERGE NOSTKCHK LINK MODIFIED DATA=NEAR IGNORE=73 OPTIMIZE FrexxCalc.c
else if exists LC:
   lc -DAMIGA -b1 -Lcd -v -M -j73 -O FrexxCalc.c
else
   echo "Didn't find SAS/C version 5 or 6. Compile by yourself!"
endif
quit

*---------------------------------------------------------------------------*
UNIX:
Compile string "cc -O -DUNIX -o FrexxCalc FrexxCalc.c -lfpl -L<fpllib dir>"
*---------------------------------------------------------------------------*

*****************************************************************************
* FrexxCalc.c - Calculates a C-style expression.                            *
*                                                                           *
* Copyright (C) 1992, 1993 by FrexxWare.                                    *
* Author: Daniel Stenberg                                                   *
*                                                                           *
* This example is provided "as-is" and is subject to change; no warranties  *
* are made.  All use is at your own risk.  No liability or responsibility   *
* is assumed.                                                               *
*                                                                           *
****************************************************************************/

#ifdef AMIGA
#include <exec/types.h>
#include <exec/libraries.h>
#include <libraries/dos.h>

#include <proto/exec.h>
#include <proto/FPL.h>
#include <stdlib.h>

#include <libraries/FPL.h>
struct Library *FPLBase = NULL; /* library base */

#define PREFIX __asm
#define REG(x) register __ ## x
#elif UNIX /* AMIGA */
#include <sys/types.h>
#include "../src/FPL.h"
#define PREFIX /* ignored */
#define REG(x)
#endif

#include <stdio.h>
#include <string.h>

long PREFIX functions(REG(a0) struct fplArgument *);

#define OUTPUT_FUNCTION 2

#define FPLNAME "fpl.library"

#ifdef AMIGA
static char *version="\0$VER: FrexxCalc 5.1 (15.4.94)"; /* version */
     /*
      * From version 5.1, FrexxCalc can be compiled to be pure.
      */
#endif

int main(int argc, char **argv)
{
  int n, end, len;	/* some all purpose variables */
  void *anchor;		/* FPL use pointer */
  char *program;

  /*
   * As you can see in the FPL program, we're taking advantage of the
   * internal function eval() which returns the result of the expression
   * held by a string. (Ex: if the string variable Expr contains the string
   * "2+2", eval(Expr) will return 4.)
   *
   * We create a string to hold the program:
   * "Output(eval( command_line_arguments ));" which is interpreted by
   * FPL.
   */

  /*
   * First we check the number of arguments, and if no expression
   * is given, output a usage text.
   */

  if(argc < 2) {	/* if there was no expression given */
    printf("Usage: FrexxCalc [expression]\n\n"
	   "* ALL C operators are supported. E.g:\n"
	   "  -, /, *, %, +, ==, <=, >=, >, <, !, ~, (), &&, ||, ^, &, >>, <<, |, ? and :.\n\n"
	   "* Functions supported are:\n"
	   "  abs, atoi, eval, strcmp, substr, strlen, strncmp, strstr, strtol, ltostr\n\n"
	   "* Hexadecimal numbers are written with a 0x prefix, octal numbers with\n"
	   "  a 0 prefix and binary numbers with a 0b prefix.\n\n"
	   "* Strings (inclosed in quotation marks) and characters (inclosed in\n"
	   "  apostropes) supports ALL C language  escape sequences\n\n"
	   "* Expression example:\n"
	   "  abs(3*(strcmp(\"hi\",\"bye\")-\'a\')*0xcf-0b10010*030)<<2\n\n"
	   "* Read FPL.guide for closer documentation about the expressions.\n\n"
	   "Written by Daniel Stenberg 940415, Copyright (C) 1992-1994 by FrexxWare.\n");
    return(0);
  }

#ifdef AMIGA
  /*
   * Open fpl.library on amiga.
   */

  if(!(FPLBase=OpenLibrary(FPLNAME, 5))) { /* open FPL.library version 5 */
    printf("Error opening %s!\n", FPLNAME);
    return(-1);
  }
#endif

  for(len=0, n=1; n<argc; n++) /* count length of all arguments */
    len+=strlen(argv[n]);

  program=(char *)malloc(len+40); /* allocate space to build the
				     program in */
  /*
   * Initialize the FPL session!
   */

  anchor=fplInit(functions,	/* function handler */
		 NULL);		/* tag list */

  if(anchor) {
    /*
     * Add the function we want FPL to accept!
     */
    fplAddFunction(anchor,
		   "Output",	/* this function will output the result */
		   OUTPUT_FUNCTION,/* ID */
		   FPL_INTARG,	/* return an integer */
		   "I",		/* optional parameter as argument! */
		   NULL);	/* no additional data */
    
    /*
     * Get all the members in the argv[] array and concatenate them all into
     * one single string.
     */
    program[0]=0;
    
    strcat(program, "Output(eval(\""); /* FPL program start! */
    for(n=1; n<argc; n++)
      strcat(program, argv[n]);
    strcat(program, "\"));");

    /*
     * Initialize the fplData structure.
     */
    
    end=fplExecuteScript(anchor, &program, 1, NULL);/* execute the program */
    free(program); /* free the program */
    fplFree(anchor);
  }
#ifdef AMIGA
  /* If amiga: close the library */
  CloseLibrary((struct Library *)FPLBase);
#endif
  return(0);
}

/*
 * The function below is the one that is going to be called each time
 * our previously defined function is discovered in the FPL program.
 * We check the ID member of the fplArgument structure we get access to,
 * to check which of the functions that was invoked.
 * We could also check the name member of the structure since that will
 * point to the function name, but integer comparisons are much faster!
 * The ID will be set to FPL_GENERAL_ERROR if any error was discovered
 * in the FPL program!
 */

long PREFIX functions(REG(a0) struct fplArgument *arg)
{
  char buffer[FPL_ERRORMSG_LENGTH];
  switch(arg->ID) {
  case OUTPUT_FUNCTION:	/* if the Output() function was invoked */
    if(arg->format[0] == FPL_INTARG)
      printf("%d\n", (int)arg->argv[0]);
    else
      printf("%s\n", (char *)arg->argv[0]);
    /* returns nothing! */
    break;
  case FPL_GENERAL_ERROR:
    printf("%s\n", fplGetErrorMsg(arg->key, (long)arg->argv[0], buffer));
    break;
  }
  return(0);
}
