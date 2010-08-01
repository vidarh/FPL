#ifndef FPL_H
#define FPL_H
/*
**   $Filename: libraries/FPL.h $
**   $Release: 14.11 $
**   $Date: 1997/04/01 $
**
**   (C) Copyright 1992, 1993 by FrexxWare
**       All Rights Reserved
*/

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

#ifdef DEFAULT_CHAR_IS_SIGNED
/*
 * We introduce two new variable declarators to make all 'char' variables
 * clearly stated either signed or unsigned, to remain untied by stupid
 * compilers [different] defaults!
 *
 * (Still not properly used due to problems, beats me why!)
 */

typedef signed char   schar; /* 'schar' for -127 to 128 */
typedef unsigned char uchar; /* 'uchar' for 0 to 255 */
#elif defined(WIN32)
typedef unsigned char uchar;
#else
/*
 * If we use some compiler switch or default that makes 'char' unsigned,
 * we can live with simply 'char' instead of 'unsigned char'.
 */
typedef char uchar;
#endif

/*
 *  OBSOLETE RETURN CODES! USE THE NEW 'FPLERR_*' ONES!!!
 */

#ifndef OUTDATE_OLD

enum {
  FPL_COULDNT_OPEN_DOS = 2,
  FPL_DIVISION_BY_ZERO,
  FPL_ILLEGAL_ANCHOR,
  FPL_ILLEGAL_ARRAY,
  FPL_ILLEGAL_ASSIGN,
  FPL_ILLEGAL_BREAK,
  FPL_ILLEGAL_CONDOP,
  FPL_ILLEGAL_CONTINUE,
  FPL_ILLEGAL_DECLARE,
  FPL_ILLEGAL_PARAMETER,
  FPL_ILLEGAL_PREOPERATION,
  FPL_ILLEGAL_PROTOTYPE,
  FPL_ILLEGAL_RESIZE,
  FPL_ILLEGAL_STATEMENT,
  FPL_ILLEGAL_VARIABLE,
  FPL_INTERNAL_ERROR,
  FPL_INSIDE_NOT_FOUND,
  FPL_MISSING_APOSTROPHE,
  FPL_MISSING_ARGUMENT,
  FPL_MISSING_BRACE,
  FPL_MISSING_BRACKET,
  FPL_MISSING_OPERAND,
  FPL_MISSING_PARENTHESES,
  FPL_MISSING_SEMICOLON,
  FPL_NO_ACTION,
  FPL_OPEN_ERROR,
  FPL_OUT_OF_MEMORY,
  FPL_OUT_OF_REACH,
  FPL_OUT_OF_STACK,
  FPL_PROGRAM_STOPPED,
  FPL_READONLY_VIOLATE,
  FPL_SYNTAX_ERROR,
  FPL_UNBALANCED_COMMENT,
  FPL_UNEXPECTED_END,
  FPL_UNMATCHED_BRACE,
  FPL_IDENTIFIER_NOT_FOUND,
  FPL_IDENTIFIER_USED,

  FPL_UNKNOWN_ERROR /* this or higher is unknown error codes! */

};

#endif

/***** ALL BY FPL SUPPLIED, IMPLEMENTED AND SUPPORTED RETURN CODES: ******/
typedef enum {
  FPL_OK,
  FPL_EXIT_OK,		 /* no error, the program stated simply exit() */
  FPLERR_COULDNT_OPEN_DOS, /* NOT USED */
  FPLERR_DIVISION_BY_ZERO,
  FPLERR_ILLEGAL_ANCHOR,
  FPLERR_ILLEGAL_ARRAY,
  FPLERR_ILLEGAL_ASSIGN,
  FPLERR_ILLEGAL_BREAK,
  FPLERR_ILLEGAL_CONDOP,
  FPLERR_ILLEGAL_CONTINUE,
  FPLERR_ILLEGAL_DECLARE,
  FPLERR_ILLEGAL_PARAMETER,
  FPLERR_ILLEGAL_PREOPERATION,
  FPLERR_ILLEGAL_PROTOTYPE,
  FPLERR_ILLEGAL_RESIZE,
  FPLERR_ILLEGAL_STATEMENT,
  FPLERR_ILLEGAL_VARIABLE,
  FPLERR_INTERNAL_ERROR,
  FPLERR_INSIDE_NOT_FOUND,
  FPLERR_MISSING_APOSTROPHE,
  FPLERR_MISSING_ARGUMENT,
  FPLERR_MISSING_BRACE,
  FPLERR_MISSING_BRACKET,
  FPLERR_MISSING_OPERAND,
  FPLERR_MISSING_PARENTHESES,
  FPLERR_MISSING_SEMICOLON,
  FPLERR_NO_ACTION,
  FPLERR_OPEN_ERROR,
  FPLERR_OUT_OF_MEMORY,
  FPLERR_OUT_OF_REACH,
  FPLERR_OUT_OF_STACK,
  FPLERR_PROGRAM_STOPPED,
  FPLERR_READONLY_VIOLATE,
  FPLERR_SYNTAX_ERROR,
  FPLERR_UNBALANCED_COMMENT,
  FPLERR_UNEXPECTED_END,
  FPLERR_UNMATCHED_BRACE,
  FPLERR_IDENTIFIER_NOT_FOUND,
  FPLERR_IDENTIFIER_USED,

  FPLERR_MISSING_COLON, /* new from version 7 */
  FPLERR_MISSING_WHILE, /* new from version 7 */

  /* NEW ONES FROM V11: */

  FPLERR_ILLEGAL_CASE,
  FPLERR_ILLEGAL_DEFAULT,
  FPLERR_UNEXPECTED_INT_STATEMENT,
  FPLERR_UNEXPECTED_STRING_STATEMENT,
  FPLERR_STRING_INDEX,
  FPLERR_ILLEGAL_REFERENCE,
  FPLERR_TOO_MANY_PARAMETERS,

  FPLERR_UNKNOWN_ERROR /* this or higher is unknown error codes! */

  } ReturnCode;

/*********************************************************************
 *
 * Parameter and return type defines:
 *
 */

#define FPL_STRVARARG     'C' /* as in 'C'haracter variable */
#define FPL_INTVARARG     'N' /* as in 'N'numeric variable */
#define FPL_STRVARARG_OPT (FPL_STRVARARG^32)
#define FPL_INTVARARG_OPT (FPL_INTVARARG^32)
#define FPL_OPTVARARG     'R' /* as in 'R'eference to C or N */
#define FPL_STRARRAYVARARG 'B' /* string array reference */
#define FPL_INTARRAYVARARG 'D' /* integer array reference */
#define FPL_VOIDARG       'V' /* as in 'V'oid, no return values */

#define FPL_OPTARG        'A' /* like in 'A'll accepted, which then can
                                 be any one of CNSI */

#define FPL_STRARG        'S' /* as in 'S'tring */
#define FPL_INTARG        'I' /* as in 'I'nteger */
#define FPL_OPTEXPRARG	  'O' /* as in 'O'ptionally S or I */
#define FPL_STRARG_OPT    (FPL_STRARG^32)
#define FPL_INTARG_OPT    (FPL_INTARG^32)
#define FPL_OPTARG_OPT    (FPL_OPTARG^32)
#define FPL_ARGLIST       '>'

/*********************************************************************
 * fplInit() and fplReset() tags:
 ********************************************************************/

#define FPLTAG_END            0
#define FPLTAG_DONE           0
#define FPLSEND_DONE	      0
#define FPLSEND_END	      0
#define FPLREF_DONE	      0
#define FPLREF_END	      0
/* End of tag list defines! */

#define FPLTAG_INTERVAL       1 /* data is a function pointer */
/* Define the interval function pointer! */

#define FPLTAG_ZERO_TERMINATE 2 /* obsolete tag from version 5*/

#define FPLTAG_STACK          3 /* data is size in bytes */
/* Important only for the Amiga library version! Default startup size of the
   library stack. */

#define FPLTAG_USERDATA       4 /* data is free to use to anything! */
/* Userdata able to read anywhere with GetUserdata(): */

#define FPLTAG_FUNCDATA       FPLTAG_USERDATA /* data is free to use */
/* Specifies private data to a specific function. */

#define FPLTAG_FUNCTION       5 /* data is a function pointer */
/* fplAddFunction() tag only. function handler routine for this function. */

#define FPLTAG_MAXSTACK	      6 /* OBSOLETE from version 9.5 */

#define FPLTAG_STACKLIMIT     7 /* data is size in bytes */
/* (Amiga) Absolute maximum memory area used as stack by one single FPL
   program. */

/* removed obsolete tag `FPLTAG_FREE' ! */

#define FPLTAG_INTERNAL_ALLOC 9 /* data is function pointer */
/* To a "void *(*)(long)" (on Amiga, the parameter will be sent in d0
   and *not* on the stack)! */
#define FPLTAG_INTERNAL_DEALLOC 10 /* data is function pointer */
/* To a "void (*)(void *, long);" (on Amiga, the parameters will be sent in
   the registers a1 and d0 and *NOT* on the stack as all other functions do. */

#define FPLTAG_INTERPRET 11 /* data is a char pointer */
/* To a fully FPL syntax statement to be interpreted instead of the
   actual main function of the program that is about to get started. */

#define FPLTAG_STARTPOINT 12 /* data is a char pointer */
/* To the alternative start position of this program. */
#define FPLTAG_STARTLINE  13 /* data is integer */
/* The line of the upper start point if not 1. */

#define FPLTAG_STOREGLOBALS 14 /* data is boolean */
/* This enables/disables the FPL global symbol storage ability. */

#define FPLTAG_CACHEALLFILES 15 /* data is one of the defines below */
/* Should FPL cache all files exporting functions? */

#define FPLTAG_CACHEFILE 16 /* data is one of the defines below */
/* Should FPL cache this file. Default is FPLTAG_CACHEALLFILES or false. */

#define FPLCACHE_NONE    0 /* never cache */
#define FPLCACHE_ALWAYS  1 /* always cache */
#define FPLCACHE_EXPORTS 2 /* cache if symbols were exported */

#define FPLTAG_FILEID 17 /* data is a unique file identification 32-bit */
/* This fileID is used by FPL. Associate this program with this fileID!
   If you ever make a FPLSEND_FLUSHCACHEDFILES and this file is removed from
   memory, FPL will use this when requesting the file from you!
   If this file doesn't declare any `export' functions, if you never flushes
   this file or if you don't allow saved export variables, this tag can be
   ignored (FPL will then create a temporary fileID to use). */

#define FPLTAG_PROGNAME 18 /* data is char pointer to zero terminated name */
/* When using fplExecuteScript(), FPL does not have a name for the program.
   If you want anything but <unknown program> in a possible error report,
   you should use this. fplEXecuteFile() automatically sets this tag, but
   if you don't want the program to be named as the file name, use this tag
   then too! */

#define FPLTAG_FILEGLOBALS 19 /* data is pointer to long */
#define FPLTAG_ISCACHED FPLTAG_FILEGLOBALS
/* Supply this tag with a pointer to a `long' and receive a zero (0) if no
   global symbols was declared, or a non-zero (_not_ the number of symbols)
   value if any global symbols were declared.

   If this leaves a non-zero value, it means that FPL has stored symbols
   associated with this program's ID. If you do not want them, use the
   {FPLSEND_FREEFILE, filename} tag to clean up. If you want to be able
   to start the same program using the old global symbol values (and not
   confusing the interpreter), use the same file name on next start.
   */

#define FPLTAG_FILENAMEGET 20 /* data is boolean */
/* This tag tells FPL that the program name is ok to use as file name to load
   the program from after a flush. fplExecuteFile() uses this tag as default.
   */

#define FPLTAG_MINSTACK 21 /* OBSOLETE from version 9.5 */

#define FPLTAG_LOCKUSED 22 /* OBSOLETE from version 13 */

/**** NEW FROM VERSION 5: ****/

#define FPLTAG_HASH_TABLE_SIZE 23 /* data is a long */
/* This sets the hash table size. USE ONLY IN fplInit()!!! */

#define FPLTAG_NEWLINE_HOOK 24 /* data is a standard long (*)(void *); */
/* Called after every newline character read. The argument is sent in register
   A0 in the Amiga version. The argument is so far only the fplinit() return
   code. This might be subject to change to next release!! If you intend to
   use this, use caution! */

/**** NEW FROM VERSION 5.3: ****/
#define FPLTAG_ALLFUNCTIONS 25 /* data is boolean */
/* Enables the FPL_UNKNOWN_FUNCTION interface message. Whenever FPL finds a
   function not recognized, it will still parse all arguments and call the
   interface function. */

/**** NEW FROM VERSION 6: ****/

#define FPLTAG_STRING_RETURN 27 /* data is pointer to a char pointer */
/* enables the top level FPL program to return a string to the calling
   program in the pointer the data supplies a pointer to. NULL disables
   the ability. The string should be freed using the brand new function
   fplFreeString() */

/**** NEW FROM VERSION >6: ****/
#define FPLTAG_NESTED_COMMENTS 28 /* data is boolean */
/* This fplInit() tag makes FPL allow nested comments. Default is off. */

/**** NEW FROM VERSION 8: ****/

#define FPLTAG_REREAD_CHANGES 29 /* data is boolean */
/* Alter 'reread' status of files */

#define FPLTAG_FLUSH_NOT_IN_USE 30 /* data is boolean */
/* Alter 'flush' status of files */

/**** NEW FROM VERSION 9: ****/

#define FPLTAG_IDENTITY 31 /* data is pointer to a string */
/* Set host process identification string */

#define FPLTAG_DEBUG 32 /* data is boolean */
/* Make this execution use debug mode from the beginning! */

#define FPLTAG_KIDNAP_CACHED 33 /* data is boolean */
/* Only useful when calling fplExecuteScript():
   If this program get cached, then FPL will duplicate it to keep a fair
   copy of it, making no troubles for the calling program to always
   free the executing program after executions! */

#define FPLTAG_ERROR_BUFFER 34 /* data is char pointer */
/* Set this pointer to point to a buffer with the minimum size of
   FPL_ERRORMSG_LENGTH bytes. If any FPL error occures, the buffer will
   hold the FPL error message. fplGetErrorMsg() will not be necessary
   if this is used! */

/**** NEW FROM VERSION 10: ****/

#define FPLTAG_PREVENT_RUNNING_SAME 35 /* data is boolean */
/* Executution of a cached program (already in memory) will abort immediately
   with an OK return code! FPLTAG_REREAD_CHANGES is prioritized and will
   override this tag.*/

#define FPLTAG_ISOLATE 36 /* data is boolean */
/* This tag is valid only for the fplExecuteXXX() functions and it makes the
   program run in 'protected' mode, isolated from the other FPL programs
   that might have been run eariler. Isolated programs cannot access exported
   symbols, nor can they export any themselves. */

/**** NEW FROM VERSION 14: ****/
#define FPLTAG_AUTOCOMPILE 37 /* data is boolean */
/* This tag enforces FPL to "autocompile" all .FPL that is run and that
   is newer than the corresponding .FPC. If the .FPC is newer, it will
   simply run that instead, see FPLTAG_AUTORUN */
   
#define FPLTAG_AUTORUN 38 /* data is boolean */
/* This tag makes FPL check if there is a .FPC file that is newer when
   a .FPL file is run. If the .FPC file is newer, that file is run. */

#define _FPL_DUMMY	100	  /* ignore this */

/**********************************************************************
 * Here follows the tags for the fplSend() function. New for V3.
 *********************************************************************/

#define FPLSEND_STRING	(_FPL_DUMMY +1)
/* Use this when returning a string from a user function. See FPLSEND_STRLEN */

#define FPLSEND_STRLEN  (_FPL_DUMMY +2)
/* Specifies the length of the returned string. If this is -1, a strlen()
   will be performed by FPL to find out the real length! */

#define FPLSEND_INT	(_FPL_DUMMY +3)
/* You return an integer from the function. */

#define FPLSEND_GETRESULT (_FPL_DUMMY +4)
/* You specify a pointer to a `long' to hold the result of the last interval
   function call. */

#define FPLSEND_GETLINE (_FPL_DUMMY +5)
/* You specify a pointer to a `long' to hold the number of the current line the
   interpreter is working on. */

#define FPLSEND_GETRETURNCODE (_FPL_DUMMY +6)
/* You specify a pointer to a `long'  to hold the value received by the last
   return() or exit() call in the FPL program. */

#define FPLSEND_GETUSERDATA (_FPL_DUMMY +7)
/* You specify a pointer to a `long'  to hold the userdata specified in the
   `FPLTAG_USERDATA' tag's data field in the fplInit() call. */

#define FPLSEND_GETCOLUMN (_FPL_DUMMY +8)
/* You specify a pointer to a `long'  to hold the number of the current column
   the interpreter is working on. */

#define FPLSEND_FLUSHCACHE (_FPL_DUMMY +9)
/* specify TRUE/FALSE wheather you want FPL to flush (empty) the internal
   memory cache/queue. */

#define FPLSEND_FREEFILE (_FPL_DUMMY +10) /* data is a program name pointer */
/* FPL frees all functions associated with the given program! */

#define FPLSEND_PROGRAMFILE FPLSEND_STRING
/* Used when returning a program's file name to FPL. */

#define FPLSEND_PROGRAM (_FPL_DUMMY +11) /* data is pointer to an array */
/* When using functions in different source files, this is one way to give
   FPL information about where to find a certain function! */

#define FPLSEND_GETPROGNAME (_FPL_DUMMY +12)
/* Get pointer to the last interpretated FPL program name. If any error has
   occurred, this will be the failing program name! */

#define FPLSEND_CONFIRM (_FPL_DUMMY +13)
/* Used to confirm the query from FPL. Currently, taht can only be
   FPL_FLUSH_FILE. */

#define FPLSEND_FLUSHFILE (_FPL_DUMMY +14)
/* Flush the file with the specified fileID. If no ID is specified, all unused
   files will be flushed. */

#define FPLSEND_STEP (_FPL_DUMMY +15)
/* Moves the current interpret position. Negative number is backwards and
   positive forwards. */

#define FPLSEND_GETSTACKSIZE (_FPL_DUMMY + 16) /* data is pointer to long */
/* (Amiga) Receive the current stack size! */

#define FPLSEND_GETSTACKUSED (_FPL_DUMMY + 17) /* data is pointer to long */
/* (Amiga) Receive the current amount of stack used (this isn't an exact
   figure) */

#define FPLSEND_SETPROGNAME (_FPL_DUMMY + 18) /* data is char pointer */
/* This tag forces a new name to the current (executing) program.
   See also FPLTAG_PROGNAME */

#define FPLSEND_SETFILENAMEGET (_FPL_DUMMY + 19) /* data is boolean */
/* This tag sets or clears the same FPLTAG_FILENAMEGET option */

/**** NEW FOR VERSION 4: ****/

#define FPLSEND_GETSYMBOL_FUNCTIONS	(_FPL_DUMMY + 20)
#define FPLSEND_GETSYMBOL_MYFUNCTIONS	(_FPL_DUMMY + 21)
#define FPLSEND_GETSYMBOL_FPLFUNCTIONS	(_FPL_DUMMY + 22)
#define FPLSEND_GETSYMBOL_VARIABLES	(_FPL_DUMMY + 23)
#define FPLSEND_GETSYMBOL_CACHEDFILES	(_FPL_DUMMY + 24)
#define FPLSEND_GETSYMBOL_FREE		(_FPL_DUMMY + 25)
/* See FPL.guide for information and docs */

/**** NEW FOR VERSION 5: ****/

#define FPLSEND_GETPROG (_FPL_DUMMY + 26)
/* Supply a pointer to a char pointer, and you'll get a pointer to the current
   program. */

#define FPLSEND_GETINTERVAL (_FPL_DUMMY + 27)
/* Supply a pointer to a function pointer, and you'll receive a pointer to the
   specified interval function. */

#define FPLSEND_GETNEWLINE_HOOK (_FPL_DUMMY + 28)		/* OBSOLETE!!! */

#define FPLSEND_GETFUNCTION (_FPL_DUMMY + 29)
/* Supply a pointer to a function pointer, and you'll receive a pointer to the
   specified interface function. */

#define FPLSEND_GETSYMBOL_ALLVARIABLES (_FPL_DUMMY + 30)
#define FPLSEND_GETSYMBOL_ALLFUNCTIONS (_FPL_DUMMY + 31)
/* See FPL.guide for information */

/**** NEW FOR VERSION 5.3: ****/
#define FPLSEND_GETVIRLINE (_FPL_DUMMY + 32)
/* return the virtual line number in the long you send a pointer to!
   NOTE: The virtual line number is most often the "real" line number of your
   program. FPL counts every appearance of \x0a in the program and that makes
   the VIRTUAL line number since that has nothing to do with how FPL really
   counts the lines!
   Future versions will be able to change this line number run-time, using the
   #line - instruction!
   */

#define FPLSEND_GETVIRFILE (_FPL_DUMMY + 33)
/* return the virtual filename in the char pointer you send a pointer to!
   NOTE: The virtual file name is most often the name of the current
   executed program. 
   Future versions will be able to change this line number run-time, using the
   #line - instruction!
   */

/**** NEW FOR VERSION 6: ****/
#define FPLSEND_DONTCOPY_STRING (_FPL_DUMMY + 34)
/* The string we send to FPL is allocated with fplAllocString(). */


/**** NEW FOR VERSION 10: ****/
#define FPLSEND_RESULT (_FPL_DUMMY + 35)
/* Supply a pointer to a long that will get the result code of a following
   query-tag, This pointer is static within FPL and is only then needed to
   be set whenever it's changed! NULL is valid, but no result will be
   available then!
   */

#define FPLSEND_IS_FILE_CACHED (_FPL_DUMMY + 36)
/* Supply a char pointer of a program name. This tag will make FPL report if
   the specified program is cashed - left in memory. FPL will store the result
   (TRUE or FALSE) in the long pointed to by the FPLSEND_RESULT tag.
   */

#define FPLSEND_GETRETURNINT (_FPL_DUMMY + 37)
/* Like FPLSEND_GETRETURNCODE, but this returns a pointer to the actual
   number (long). If no value was returned, this will return NULL. */

/**** NEW FOR VERSION 12: ****/

#define FPLSEND_GETVERSION (_FPL_DUMMY + 38)
/* Supply a pointer to a long to get the FPL library version */

#define FPLSEND_GETREVISION (_FPL_DUMMY + 39)
/* Supply a pointer to a long to get the FPL library revision */

 /***********************************************************************
  *
  * Funclib defines (new from version 7) [Amiga only]
  *
  *****/

#define FPLLIB_NONE  0 /* nothing at all! */
#define FPLLIB_FORCE (1<<0)
#define FPLLIB_KEEP  (1<<1)

struct fplArgument {
  /*
   * Interface function argument structure...
   */

  uchar *name;		/* Name */
  long ID;              /* ID */
  void **argv;          /* Pointer array of all arguments in the same order as
			   they were read. Integers are simply stored in the
			   pointer (read docs for info about this). */
  long argc;            /* Number of members in the array above */
  void *key;		/* The return code from your initial fplInit() call! */
  uchar *format;	/* Pointer to the actual format string of this
			   function. Functions using '>' in their format
			   string can get quite a long string here... */
  void *funcdata;	/* The same data as passed in the FPLTAG_FUNCDATA tag
			   of your AddFunction() call. Your possibility to
			   pass function specific data through the library. */
  /* NEW FOR FPL 5.10 */
  uchar ret;		/* Expected return type. Especially useful when
			   using functions with optional return types. */
  /* NEW FOR V10 */
  void *variable;       /* Holds the [previous] variable contents of an
                           external variable. If ->argc is set, the ->argv
                           will hold the optionally new contents of this
                           variable */
};

struct fplSymbol {
  /*
   * Using the functions FPLSEND_GETSYMBOL_XXXXXXX will result in a pointer
   * to a structure like this!
   *
   * Free that pointer by FPLSEND_GETSYMBOL_FREE.
   */
  long num;
  uchar **array;
};

/****************************/
/***** STRING LENGTH: *******/
/****************************/
/* Since the no-zero-terminated-strings theories was introduced, we can't
   use a simple strlen() to get the string length. This macro will do the
   job for you. (Extensively typecasted to apply to ANSI standard.)
   Supply the name of the structure pointer you received in the interface
   function, and the number of the string. Returns the length of that
   string. */
#define FPL_STRING_LENGTH(arg, n)\
  ((long)*(long *)((uchar *)(arg)->argv[n]-sizeof(long)))

/* Supply a string pointer received in the interface function. Returns
   the length of the FPL-string! */
#define FPL_STRLEN(arg)\
  ((long)*(long *)((uchar *)(arg)-sizeof(long)))

/****************************/
/***** FPLARGUMENT.ID: ******/
/****************************/
#define FPL_GENERAL_ERROR -1
/* There was an error in the interpreting.
   arg->argv[0] contains the error number.
   */

#define FPL_FILE_REQUEST  -2
/* FPL wants a file from you that was previously flushed! The arg->argv[0]
   contains the fileID of the program. Use FPLSEND_PROGRAM or
   FPLSEND_PROGFILE to respond. */

#define FPL_FLUSH_FILE -3
/* FPL allows you to remove the allocation associated with a certain program.
   The arg->argv[0] contains the fileID. Respond with FPLSEND_CONFIRM and the
   data field TRUE if you really do want to flush that program. */

#define FPL_WARNING -4
/* FPL warning. ->argv[0] holds the error number. Return a fplSend() with
   {FPLSEND_CONFIRM, TRUE} if you want FPL to try to continue anyway! */

#if DEVELOP
#define FPL_HIJACK_READ -5
/* The program accessed a variable that previously has been hijacked!
   This is currently not implemented, but are tested in the laboratories! */
#endif

#define FPL_UNKNOWN_FUNCTION -6 /* New from version 5.3 */
/* The program has interpreted an unknown function identifier. The name
   is found in the ->name member, and the parameters are read as usual via
   the ->format member from the ->argc and ->argv members. ->funcdata are
   reserved for future use, do not depend upon it to hold anything
   particular. This is activated with FPLTAG_ALLFUNCTIONS. */

#define FPL_COMPILE -7 /* development ID */


/****************************/
/****** Limitations: ********/
/****************************/
/* Max length of a resulting error message: */
#define FPL_ERRORMSG_LENGTH 100

#define FPLNAME "fpl.library"

#define FPL_VERSION  14
#define FPL_REVISION 12
#endif
