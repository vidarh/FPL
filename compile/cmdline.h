/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 cmdline.h
 
 Script structures and defines for the command line options!

 *****************************************************************************/

/************************************************************************
 *                                                                      *
 * fpl.library - A shared library interpreting script langauge.         *
 * Copyright (C) 1992-1995 FrexxWare                                    *
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
#define STRICMP stricmp
#elif defined(WIN32)
#define STRICMP stricmp
#else
#define STRICMP strcasecmp /* at least in SunOS */
#endif

 /*
  * Array indexes:
  */
typedef enum {
  LINE_OUTPUT,           /* output file/dir name */
  LINE_FILES,            /* the rest of the command line is treated as file
                           names! */
  LINE_NOVERSION,        /* Don't display extensive version information */
  LINE_COMMENTNEST,      /* allow nested comments */
  LINE_PPOPTS,           /* set 'cpp' options */
  LINE_CPP,              /* 'cpp' program name */
  LINE_NOCPP,            /* don't cpp it at all! */
  LINE_VERBOSE,          /* create verbose compile report */
  
  LINE_DEBUG,    /* Debug flags */

  LINE_NUMOFOPTS /* always the last enum! */
} CommandLine;

struct CommandLine {
  char *name;
  long flags;
  CommandLine number;
  struct CommandLine *bitflags;
  char *description;
};
/* CommandLine flags: */
#define COMLINE_DEFAULT  0 /* for all unknown keywords */
#define COMLINE_NO_KEY   1 /* next argument is no keyword */
#define COMLINE_REST     2 /* the rest of the line belongs to this! */
#define COMLINE_STRING   3 /* a string follows this */
#define COMLINE_INTEGER  4 /* an integer follows this */
#define COMLINE_SWITCH   5 /* this is a switch only */
#define COMLINE_BITFIELD 6 /* perhaps followed by a bitfield keyword */

#define VERBOSE_DEFAULT (1<<0)
#define VERBOSE_PASS1   (1<<1)
#define VERBOSE_PASS2   (1<<2)
#define VERBOSE_PASS3   (1<<3)
#define VERBOSE_FINAL   (1<<4)
#define VERBOSE_SYMBOL  (1<<5)

#define DEBUG_DEFAULT   (1<<0)
#define DEBUG_LINE      (1<<1)

