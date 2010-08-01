/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 caller.c

 The initial parts of the FPL compiler system.

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


#include "script.h"

#ifdef AMIGA
#include <exec/types.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <dos/dosasl.h> /* for the MatchXXX() business */

int CXBRK(void) { return(0); }  /* Disable Lattice/SAS CTRL/C handling */
int chkabort(void) { return(0); }  /* really */

#define REG(x)

long __oslibversion = 37; /* we require AmigaOS 2.04 */
long __stack = 8000; /* start with 8K stack */
long __STKNEED = 1000; /* smallest required stack */

#elif defined(UNIX) /* #ifdef AMIGA */
#include <sys/types.h>

#if defined(__GCC_NEW_VARARGS__) /* && defined(__GCC__) */
/*
 * If you want this file to be compiled using the va_* macros instead of the
 * dirty "address of parameter version", define as below!
 * (Without this define, SunOS 4.1.x versions will likely crash!)
 */

#define VARARG_FUNCTIONS /* should be defined before the includes below */
#endif


#define REG(x)
#define TRUE  1
#define FALSE 0

#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#include "pass2.h"
#include "pass3.h"

#define CALLER
#define ASM

#ifdef AMIGA
static char *version ="$VER:fplc " VERSION " " __AMIGADATE__;
#endif

#undef malloc
#undef free

long FindOption(char *search,
                struct CommandLine *cmd)
{
  long count;
  for(count=0; cmd[count].name; count++) {
    if(!STRICMP(cmd[count].name, search)) {
      return count;
    }
  }
  return -1;
}

long ParseOptions(long argc,
                  char **argv,
                  struct CommandLine *cmd,
                  long *result)
{
  long i=1; /* start at argv[1] */
  long rest_of_line_count=0;
  char do_rest=FALSE;
  long count;
  long found;
  long def=-1; /* default */
  for(count=0; cmd[count].name; count++) {
    if (cmd[count].flags==COMLINE_DEFAULT)
      def=count;
    if ((cmd[count].flags==COMLINE_REST ||
         cmd[count].flags==COMLINE_NO_KEY ||
         cmd[count].flags==COMLINE_DEFAULT) &&
        !result[cmd[count].number]) {
      result[cmd[count].number] = (long)malloc(sizeof(long)*(argc+2));
      if (!result[cmd[count].number])
        return -1;
      memset((char *)result[cmd[count].number], 0, sizeof(long)*(argc+2));
    }
  }
  while (i<argc) {
    found=FindOption(argv[i], cmd);
    if (found==-1)
      found=def;

    if (found>=0) {
      switch(cmd[found].flags) {
      case COMLINE_REST:
        do_rest=TRUE;
        /* falling down */
      case COMLINE_NO_KEY:
        if(i>=argc-1)
          return -1; /* missing parameter */
        i++;
        /* falling down */
      case COMLINE_DEFAULT:
        do {
          ((long *)result[cmd[found].number])[rest_of_line_count]=(long)argv[i];
          rest_of_line_count++;
        } while (do_rest && ++i<argc);
        break;

      case COMLINE_STRING:
        if(i>=argc-1)
          return -1; /* missing parameter */
        result[cmd[found].number] = (long)argv[++i];
        break;
      case COMLINE_INTEGER:
        if(i>=argc-1)
          return -1; /* missing parameter */
        result[cmd[found].number] = (long)strtol(argv[++i], NULL, 0);
        break;
      case COMLINE_SWITCH:
        result[cmd[found].number] = (long)TRUE;
        break;
      case COMLINE_BITFIELD:
        result[cmd[found].number] |= 1; /* Indicating bitfield keyword is used */
        if(i<argc-1) {
           /* parameter not missing */
          long bitword; 
          bitword=FindOption(argv[i+1], cmd[found].bitflags);
          if (bitword>=0) {
            result[cmd[found].number] |= cmd[found].bitflags[bitword].flags;
            i++;
          }
        }
        break;
      }
    } else
      return -1; /* this is not a known option */
    i++;
  }
  return rest_of_line_count;
}

void ParseFree(struct CommandLine *cmd,
               long *result)
{
  long count;
  for(count=0; cmd[count].name; count++) {
    if ((cmd[count].flags==COMLINE_REST ||
         cmd[count].flags==COMLINE_NO_KEY ||
         cmd[count].flags==COMLINE_DEFAULT) &&
         result[cmd[count].number])
      free((char *)result[cmd[count].number]);
  }
}

void Help(struct CommandLine *cmd)
{
  int count;
  char *t1;
  char *t2;

  puts("\n\nFPLc [options] pattern file(s)...\n");
  for(count=0; cmd[count].name; count++) {
    if (cmd[count].description) {
      t1 = cmd[count].name;
      switch (cmd[count].flags) {
      case COMLINE_DEFAULT:   /* for all unknown keywords */
        t2 = "(default)";
        break;
      case COMLINE_NO_KEY:    /* next argument is no keyword */
        t2 = "(ignore next)";
        break;
      case COMLINE_REST:      /* the rest of the line belongs to this! */
        t2 = "(rest of line)";
        break;
      case COMLINE_STRING:    /* a string follows this */
        t2 = "<string>";
        break;
      case COMLINE_INTEGER:   /* an integer follows this */
        t2 = "<integer>";
        break;
      case COMLINE_SWITCH:    /* this is a switch only */
        t2 = "(switch)";
        break;
      case COMLINE_BITFIELD:  /* perhaps followed by a bitfield keyword */
        t2 = "<switch>";
        break;
      }
      printf("%-13s %-13s - %s\n", t1, t2, cmd[count].description);
      if (cmd[count].flags==COMLINE_BITFIELD) {
        int loop;
        for(loop=0; cmd[count].bitflags[loop].name; loop++) {
          printf("  %-6s", cmd[count].bitflags[loop].name);
          if (cmd[count].bitflags[loop].description)
            printf(" - %s", cmd[count].bitflags[loop].description);
          printf("\n");
        }
      }
    }
  }

/*
  printf("Usage: FPLc [options] <file(s)>\n"
         "Options:\n"
         "--------\n"
         "OUTPUT <name> - Output name or directory\n"
         "FILES         - The rest of the command line are file names\n"
         "FILE          - The next argument is a file name\n"
         "NOVERSION     - Don't display compiler version information\n"
         "COMMENTNEST   - Allow nested comments in source files\n"
         "CPP           - Alter the name of the preprocessor program\n"
         "NOCPP         - Run the compiler without preprocessor\n"
         "PPOPTS        - Set preprocessor options\n"
         "VERBOSE       - Produce verbose report on compiled code\n"
         "DEBUG         - Debug output\n"
        );
*/
  exit(1);
}

static struct CommandLine debug_flags[]={
  {"LINE",        DEBUG_LINE,  0, NULL, "Include line numbers in compilation result"},
  {NULL,          0,           0, NULL, NULL}
};

static struct CommandLine verbose_flags[]={
  {"PASS1",       VERBOSE_PASS1,  0, NULL, "Output from Pass1"},
  {"PASS2",       VERBOSE_PASS2,  0, NULL, "Output from Pass2"},
  {"PASS3",       VERBOSE_PASS3,  0, NULL, "Output from Pass3"},
  {"FINAL",       VERBOSE_FINAL,  0, NULL, "Output of final"},
  {"SYMBOL",      VERBOSE_SYMBOL, 0, NULL, "Output use of external symbols"},
  {"FULL",        -1,             0, NULL,            "Full output"},
  {NULL,          0,              0, NULL, NULL}
};

static struct CommandLine cmd[]={
  {"OUTPUT",      COMLINE_STRING,   LINE_OUTPUT, NULL,		"Output name or directory"},
  {"FILES",       COMLINE_REST,     LINE_FILES, NULL, 		"The rest of the command line are file names"},
  {"FILE",        COMLINE_NO_KEY,   LINE_FILES, NULL, 		"The next argument is a file name"},
  {"",            COMLINE_DEFAULT,  LINE_FILES, NULL, NULL},
  {"NOVERSION",   COMLINE_SWITCH,   LINE_NOVERSION, NULL, 	"Don't display compiler version information"},
  {"COMMENTNEST", COMLINE_SWITCH,   LINE_COMMENTNEST, NULL, 	"Allow nested comments in source files"},
  {"DEBUG",       COMLINE_BITFIELD, LINE_DEBUG, debug_flags, 	"Include debug info in the output"},
  {"PPOPTS",      COMLINE_STRING,   LINE_PPOPTS, NULL, 		"Set preprocessor options"},
  {"CPP",         COMLINE_STRING,   LINE_CPP, NULL, 		"Alter preprocessor program"},
  {"NOCPP",       COMLINE_SWITCH,   LINE_NOCPP, NULL, 		"Compile without preprocessor"},
  {"VERBOSE",     COMLINE_BITFIELD, LINE_VERBOSE, verbose_flags,"Produce verbose report on compiled code" },
  {NULL,          0,                0, NULL, NULL}
};


long RunCpp(char *readname, char *outname, long *results)
{
  char cppbuffer[256];

#define BUILTIN_CPP "cpp"

  tmpnam(outname); /* get a temporary file name to use */
  strcat(outname, ".fplcmade");
  sprintf(cppbuffer,
          "%s %s \"%s\" \"%s\"",
          results [ LINE_CPP ]?(uchar *)results [ LINE_CPP ]:BUILTIN_CPP,
          results [ LINE_COMMENTNEST ]?"-j ":"",
          readname,
          outname);
  return system(cppbuffer);
}

/**********************************************************************
 *
 * int main(int, char **)
 *
 ******/

long main(long argc, char **argv)
{
  long end=FPL_OK;
  struct Data *scr;
  long i=-1;
  long results[LINE_NUMOFOPTS];
  char tempfile[ 256 ];
  char *infile;
#ifdef AMIGA
  struct AnchorPath anchor;
  char path [ 256 ];
#endif
  char **files;

  memset(results, 0, sizeof(results));

  if(argc>=2) 
    i=ParseOptions(argc, argv, cmd, results);

  if(!results[LINE_NOVERSION]) {
    fprintf(stderr,
           "FPLc and FPL library are (C) 1992-1997 by FrexxWare. "
           "All rights reserved.\n"
           "Compiler v%s makes code for FPL v%d.%d!\n",
           VERSION, REQUIRE_FPL/1000, REQUIRE_FPL%1000);
  }
  if(i<0 || i>=argc)
    Help(cmd);
  i=0;
  files=((char **)(results[LINE_FILES]));
  for(; files[i] && FPL_OK == end; i++) {
#ifdef AMIGA
    memset(&anchor, 0, sizeof(struct AnchorPath));
    if(MatchFirst(files[i], &anchor)) {
      fprintf(stderr, "Can't find %s\n", files[i]);
      continue;
    }
    do {
      if(anchor.ap_Info.fib_DirEntryType>=0)
        continue;
      infile = anchor.ap_Info.fib_FileName;
      /*
       * We build the right and complete filename.
       */
      NameFromLock(anchor.ap_Current->an_Lock, path, 256);
      AddPart(path, infile, 256);
      infile = path;
#else
    infile = files[i];
#endif
    scr=fplInit(results);
  
    if(!scr) {
      printf("Couldn't init compiler!\n");
      return 20;
    }
    else {
      if(!results[LINE_NOVERSION]) {
        fprintf(stderr,
               "FPL-Compiling %s\n", infile);
      }
    }
    if(!results[LINE_NOCPP])
      end=RunCpp(infile, tempfile, results);
    else
      strcpy(tempfile, infile);
    if(!end) {
      end=fplExecuteFile(scr, tempfile);
      PutClose(scr);
      if(!results[LINE_NOCPP])
        unlink( tempfile );
      if(end<=FPL_EXIT_OK) {
        /*
         * Only do this if the 1st pass was OK!
         */
        Pass2PutOpen(scr, infile);
        Pass2Start(scr);
        Pass2PutClose(scr);
        Pass1Free(scr);

        Pass3Start(scr, results[LINE_OUTPUT]?
                        (char *)results[LINE_OUTPUT]:infile, infile);
      }
    }
    fplFree(scr); /* free all shit FPL uses internally */
#ifdef AMIGA
  /* do the wildcard loop */
  } while(!MatchNext(&anchor));
  MatchEnd(&anchor);
#endif
  } /* do the loop */

  ParseFree(cmd, results); /* Free command line parsing */

  return end;
}
