/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 Script.h
 
 Script structures and defines!

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

#define OUTDATE_OLD /* to outdate the old error enums! */

#include <string.h>
#include <stdlib.h> /* for malloc() and free() */
#include "FPL.h"
#include "cmdline.h"
#include "debugmem.h" /* re-define certain memory functions */

/**********************************************************************
 *
 * Global defines:
 *
 *********************************************************************/

#ifndef  TRUE
#define  TRUE	1
#endif
#ifndef  FALSE
#define  FALSE	0
#endif

#define FPLTEXT_UNKNOWN_PROGRAM "<unknown program>"
/* When requesting the name of a program, and no program is available or no
   name has been given. This string will be returned! */

#define MAX_DIMS	     40  /* Maximum number of array dimensions */
#define IDENTIFIER_LEN       64
/* maximum number of characters in a function-, variable- or label name
   ANSI C Standard X3J11 states that there should be at least "31
   significant initial characters in an internal identifier or a macro name" */
#define ADDSTRING_DEFAULT     16  /* default length of a string expression */
#define ADDSTRING_INC         63  /* string expression length increase step */
#define MAX_ARGUMENTS         63  /* number of parsed function arguments
				     before realloc is done */

#define FPL_HASH_SIZE 67
/* Default hash table size. This should not be dividable with 2, 3, 4 or 5 */

#define FPL_MIN_HASH 10
/* The smallest acceptable hash table size */

#define BUF_SIZE (IDENTIFIER_LEN+3) /* "global" FPL buffer size */

#define MAX_FILENAME_LENGTH 256

#if defined(AMIGA) && defined(SHARED)
#define FPL_MIN_STACK 8000  /* smallest required stack */
#define FPL_MAX_STACK 20000 /* maximum stack left after a run */
#define FPL_MAX_LIMIT 40000 /* default maximum stack use possible */

#define FPLSTACK_MINIMUM 1000 /* Stack margin. When the stack space is below,
				 this, than realloc to a bigger one! */
#endif

/*
 * Flags to use with the exists() function:
 */
#define EXISTS_FUNCTION 'f'
#define EXISTS_VARIABLE 'v'
#define EXISTS_STRING   's'
#define EXISTS_INTEGER  'i'

/**********************************************************************
 *
 * Different character defines:
 *
 **********************************************************************/

#define _U (1<<0)  /* upper case */
#define _L (1<<1)  /* lower case */
#define _W (1<<2)  /* also included as a valid identifier character */
#define _N (1<<3)  /* numerical digit 0-9 */
#define _S (1<<4)  /* white space */
#define _C (1<<5)  /* control character */
#define _P (1<<6)  /* punctation characters */
#define _X (1<<7)  /* hexadecimal digit */

extern const uchar type[257];

#define CHAR_OPEN_BRACE    '{'
#define CHAR_CLOSE_BRACE   '}'
#define CHAR_OPEN_PAREN    '('
#define CHAR_CLOSE_PAREN   ')'
#define CHAR_OPEN_BRACKET  '['
#define CHAR_CLOSE_BRACKET ']'
#define CHAR_COMMA         ','
#define CHAR_SEMICOLON     ';'
#define CHAR_PLUS          '+'
#define CHAR_MINUS         '-'
#define CHAR_ONCE_COMPLEMENT '~'
#define CHAR_NOT_OPERATOR  '!'
#define CHAR_MULTIPLY      '*'
#define CHAR_DIVIDE        '/'
#define CHAR_AND           '&'
#define CHAR_OR            '|'
#define CHAR_XOR           '^'
#define CHAR_REMAIN        '%'
#define CHAR_QUESTION      '?'
#define CHAR_COLON         ':'
#define CHAR_ASSIGN        '='
#define CHAR_LESS_THAN     '<'
#define CHAR_GREATER_THAN  '>'
#define CHAR_SPACE         ' '
#define CHAR_DOLLAR	   '$'
#define CHAR_HASH	   '#'
#define CHAR_ZERO          '0'
#define CHAR_ONE           '1'
#define CHAR_TWO           '2'
#define CHAR_THREE         '3'
#define CHAR_FOUR          '4'
#define CHAR_FIVE          '5'
#define CHAR_SIX           '6'
#define CHAR_SEVEN         '7'
#define CHAR_EIGHT         '8'
#define CHAR_NINE          '9'
#define CHAR_UPPER_A	   'A'
#define CHAR_A             'a'
#define CHAR_UPPER_B       'B'
#define CHAR_B             'b'
#define CHAR_UPPER_C       'C'
#define CHAR_C             'c'
#define CHAR_D             'd'
#define CHAR_F             'f'
#define CHAR_UPPER_I       'I'
#define CHAR_I             'i'
#define CHAR_UPPER_N       'N'
#define CHAR_N             'n'
#define CHAR_O             'o'
#define CHAR_R             'r'
#define CHAR_UPPER_S       'S'
#define CHAR_S             's'
#define CHAR_T             't'
#define CHAR_V             'v'
#define CHAR_UPPER_X	   'X'
#define CHAR_X             'x'
#define CHAR_APOSTROPHE    '\''
#define CHAR_NEWLINE       '\n'
#define CHAR_VERTICAL_TAB  '\v'
#define CHAR_CARRIAGE_RETURN '\r'
#define CHAR_ALERT         '\a'
#define CHAR_QUOTATION_MARK '\"'
#define CHAR_BACKSLASH     '\\'
#define CHAR_FORMFEED      '\f'
#define CHAR_BACKSPACE     '\b'
#define CHAR_TAB           '\t'
#define CHAR_ASCII_ZERO    '\0'

#define CASE_BIT  ('a'-'A')

/**********************************************************************
 *
 * A bunch of useful enums:
 *
 **********************************************************************/

typedef enum {		  /* all FPL operators */
  OP_NOTHING, OP_PLUS, OP_MINUS, OP_DIVISION, OP_MULTIPLY,  OP_SHIFTL,
  OP_SHIFTR, OP_REMAIN, OP_BINAND, OP_BINOR, OP_BINXOR, OP_LOGAND,
  OP_LOGOR, OP_COMPL, OP_COND1, OP_COND2, OP_EQUAL, OP_LESSEQ, OP_GRETEQ,
  OP_LESS, OP_GRET, OP_NOTEQ, OP_NOT,

  OP_PREINC, /* pre increment */
  OP_PREDEC  /* pre decrement */
} Operator;

typedef enum { /* the internal functions and keywords */
  CMD_AUTO=-200,
  CMD_BREAK,
  CMD_CASE,
  CMD_CONST,
  CMD_CONTINUE,
  CMD_DEFAULT,
  CMD_DO,
  CMD_DOUBLE,
  CMD_ELSE, /* here just to force error if used misplaced */
  CMD_ENUM,
  CMD_EXIT,
  CMD_EXPORT,
  CMD_FLOAT,
  CMD_FOR,
  CMD_IF,
  CMD_INT,
  CMD_REGISTER,
  CMD_RESIZE,
  CMD_RETURN,
  CMD_SIGNED,
  CMD_STATIC,
  CMD_STRING,
  CMD_STRUCT,
  CMD_SWITCH,
  CMD_TYPEDEF,
  CMD_UNION,
  CMD_UNSIGNED,
  CMD_VOID,
  CMD_VOLATILE,
  CMD_WHILE,

  FNC_ABS=-100,
  FNC_ATOI,
  FNC_DEBUG,
  FNC_EVAL,
  FNC_EXISTS,
  FNC_INTERPRET,
  FNC_ITOA,
  FNC_ITOC,
  FNC_JOINSTR,
  FNC_LTOSTR,
  FNC_RENAME,
  FNC_SPRINTF,
  FNC_SSCANF,
  FNC_STRCMP,
  FNC_STRICMP,
  FNC_STRISTR,
  FNC_STRLEN,
  FNC_STRNCMP,
  FNC_STRNICMP,
  FNC_STRSTR,
  FNC_STRTOL,
  FNC_SUBSTR,
  FNC_OPENLIB, /* amiga only */
  FNC_CLOSELIB, /* amiga only */

  LAST_INTERNAL /* must be the last of these ones! */
  } Funcs;

#define KEYWORD_ELSE "else" /* the "else" keyword define !! */

/**********************************************************************
 *
 * Script() control bits:
 *
 *********************************************************************/

#define SCR_NORMAL  0   /* Nothing! */
#define SCR_IF      (1<<0)
#define SCR_WHILE   (1<<1)
#define SCR_DO      (1<<2)
#define SCR_FOR     (1<<3)
#define SCR_LOOP    (SCR_WHILE|SCR_DO|SCR_FOR)
#define SCR_FUNCTION (1<<4)
#define SCR_BRACE   (1<<5) /* Declaration is allowed! This started with a brace -
			     should end with a brace, return(), break or exit()
			   */
#define SCR_RETURN_STRING (1<<6)/* This function is declared to return a string */
#define SCR_GLOBAL (1<<7)
#define SCR_SWITCH (1<<8)
#define SCR_FILE   (1<<9)  /* this is the file level, on this level the program
                              can end with a '\0' char! */
#define SCR_DEBUG  (1<<10) /* this level runs in debug mode! */

/***********************************************************************
 *
 * Expression() control bits:
 *
 **********************************************************************/

#define CON_NORMAL     0      /* normal statement */
#define CON_DECLINT    (1<<0) /* int declaration statement */
#define CON_DECLSTR    (1<<1) /* string declaration statement */
#define CON_GROUNDLVL  (1<<2) /* this statement starts at the ground level */
#define CON_SEMICOLON  (1<<3) /* forces Statement() to return positive on ";"
				 statement. Designed to support "for(;;)". */
#define CON_PAREN      (1<<4) /* support for the last expression of the for(;;)
				 ones */
#define CON_ACTION     (1<<5) /* This flag forces Statement() to report errror
				 if no "action" was made in the statement just
				 parsed. */
#define CON_END        (1<<6) /* Tell statement() there can be no
				 "UNEXPECTED_END".*/
#define CON_NUM        (1<<7) /* Only accept numerical statements! */
#define CON_STRING     (1<<8) /* Hint about this being a string statement! */
#define CON_DECLVOID   (1<<9) /* Declaration of a `void' function! */
#define CON_DECLEXP    (1<<10) /* Declaration of an `export' symbol */
#define CON_DECLGLOB   (1<<11) /* Declaration of a global symbol! */
#define CON_IDENT      (1<<12) /* The local parameter points to an already
				  parsed "struct Identifier" */
#define CON_DECL8      (1<<13) /* Declaration of an eight bit variable */
#define CON_DECL16     (1<<14) /* Declaration of an sixteen bit variable */
#define CON_DECLUNSIGN (1<<15) /* Unsigned declaration */
#define CON_DECLCONST  (1<<16) /* Constant declaration (read only) */
#define CON_DECLSTATIC (1<<17) /* Static declaration */

#define CON_LEVELOK    (1<<18) /* Disables same "proper level" controls */

#define CON_LESSTHAN32 (CON_DECL8|CON_DECL16)
#define CON_DECLARE (CON_DECLINT|CON_DECLSTR|CON_DECLVOID) /* declaration */

/***********************************************************************
 *
 * A bunch of useful macros:
 *
 **********************************************************************/

#define isalpha(c)  ((type+1)[c] & (_U|_L))
#define isupper(c)  ((type+1)[c] & _U)
#define islower(c)  ((type+1)[c] & _L)
#define isdigit(c)  ((type+1)[c] & _N)
#define isxdigit(c) ((type+1)[c] & _X)
#define isalnum(c)  ((type+1)[c] & (_U|_L|_N))
#define isspace(c)  ((type+1)[c] & _S)
#define ispunct(c)  ((type+1)[c] & _P)
#define isprint(c)  ((type+1)[c] & (_U|_L|_N|_P))
#define iscntrl(c)  ((type+1)[c] & _C)
#define isascii(c)  (!((c) & ~127))
#define isgraph(c)  ((type+1)[c] & (_U|_L|_N|_P))
#define toascii(c)  ((c) & 127)
#define toupper(c)  ((type+1)[c] & _L? (c)-CASE_BIT: c)
#define tolower(c)  ((type+1)[c] & _U? (c)+CASE_BIT: c)

#define isodigit(c) ((c) >= CHAR_ZERO && (c) <= CHAR_SEVEN)

#define isident(c)  ((type+1)[c] & (_U|_L|_W))
#define isidentnum(c)  ((type+1)[c] & (_U|_L|_W|_N))

  /* CALL - macro performing the instruction inside parentheses and receiving
     the return code in `ret'. If `ret' happens to become non-zero, a
     "return(ret);" will be performed! */
#define CALL(func) if(ret=(func)) return(ret)

  /* GETMEM - macro allocating memory and returning FPLERR_OUT_OF_MEMORY if it
     fails! */

#define GETMEM(var,size) if(!(var=(void *)MALLOC(size))) \
  return(FPLERR_OUT_OF_MEMORY);

  /* GETMEMA - macro allocating static memory and returning FPLERR_OUT_OF_MEMORY
     if it fails! */

#define GETMEMA(x,y) GETMEM(x,y)

  /* STRDUP - macro instead of the common strdup() ! */

#define STRDUP(var, pointer) \
  GETMEM(var, strlen((uchar *)(pointer))+1);\
  strcpy(var, (pointer));

  /* STRDUPA - macro instead of the common strdup() for STATIC allocs ! */

#define STRDUPA(x,y) STRDUP(x,y)

  /* UPPER - returns uppercase version any a-z character */
#define UPPER(x) ((x)&~CASE_BIT)

  /* ABS - returns the absolute value of the argument */
#define ABS(x) ((x)>0?x:-x)

  /* MIN - returns the minimum value of the two input arguments */
#define MIN(x,y) ((x)<(y)?(x):(y))

  /* MIN3 - returns the minimum value of the three input arguments */
#define MIN3(x,y,z) MIN( MIN((x),(y)) ,(z))

  /* Here follows the define for strdup() of a string stored in a
     (struct fplStr *) */
#define STRFPLDUP(dest,source)\
   do {\
     GETMEM(dest, sizeof(struct fplStr)+source->len); /* get string space */\
     memcpy(dest->string, source->string, source->len); /* copy string */\
     dest->len=dest->alloc=source->len; /* set alloc and length */\
     dest->string[dest->len]='\0'; /* zero terminate! */\
   } while(0)


#define ASSIGN_OPERATOR ( \
			 (scr->text[0]==CHAR_ASSIGN &&		\
			  scr->text[1]!=CHAR_ASSIGN) ||		\
			 ((scr->text[0]==CHAR_PLUS ||		\
			  scr->text[0]==CHAR_MINUS ||		\
			  scr->text[0]==CHAR_MULTIPLY ||	\
			  scr->text[0]==CHAR_DIVIDE ||		\
			  scr->text[0]==CHAR_AND ||		\
			  scr->text[0]==CHAR_OR ||		\
			  scr->text[0]==CHAR_REMAIN ||		\
			  scr->text[0]==CHAR_XOR) &&		\
			 scr->text[1]==CHAR_ASSIGN) ||		\
			 !strncmp("<<=", scr->text, 3) ||	\
			 !strncmp(">>=", scr->text, 3)		\
			)

/***********************************************************************
 *
 * Defines:
 *
 **********************************************************************/

#define MALLOC(x) malloc(x)
#define FREE(x) free((void *)(x))
#define FREEALL  DBG_FreeAll  /* this is from the debugmem bunch */

/* compatibility mode: */
#define FREEA FREE
#define MALLOCA MALLOC
#define FREEALLA FREEALL

/* old version:
   #define GETSTRLEN(str) ((long)*(long *)(str))
   */
#define GETSTRLEN(str) (((struct fplStr *)(((uchar *)str)-offsetof(struct fplStr, string)))->len)

#if defined(AMIGA)
  /*
   * We have to make all external referenced functions to receive the
   * parameters in certain registers and restore the A4 register.
   */

#define PREFIX
#define AREG(x)
#define DREG(x)
#define REGARGS __regargs
#define ASM

 /***************************************
  *
  * funclib specific defines:
  *
  **************************************/

#else
  /*
   * No need for any of those!
   */
#define PREFIX
#define REGARGS
#define AREG(x)
#define DREG(x)
#define ASM

typedef unsigned char BOOL;
#endif

#if defined(AMIGA) /* the amiga library defines... */
#define INLINE __inline
#else
#define INLINE
#endif

/**********************************************************************
 *
 * Create some structures and define their flags:
 *
 *********************************************************************/

struct Unary {
  Operator unary;
  struct Unary *next;
};

struct InsideFunction {
  /*
   * Used for `inside' functions.
   */

  uchar ret;
  uchar *format; 
  
  long col; /* column number of the inside function position. */
  long prg; /* line number of the function */
  uchar *file; /* name of file where this function resides */
  long virprg; /* virtual line number */
  uchar *virfile; /* virtual file name */
};

struct ExternalFunction {
  /*
   * Used for all other functions and keywords.
   */
  uchar ret; /* 'I' - returns an integer
	       'S' - returns a string
	       */
  uchar *format; /* Parameter format. Zero terminated. Unlimited length.
		   'I' - integer
		   'S' - string
		   'C' - string variable structure
		   'N' - integer variable structure
		   '>' - variable number of the previous type.

		   NULL pointer - no argument at all.
		   lower case - optional (must only be to the right of
		   the required)
		     
		   Ex: "ISsc"
		   means that the function requires two parameters:
		   one integer and one string. It has two optional
		   parameters: one string and one string variable. */
  long ID; /* Identifier ID. This information is sent in the
	      fplArgument structure.
	      <0 is reserved for FPL internals. */
};

struct fplStr {
  /*
   * FPL 'string' structure!
   */
  long alloc;     /* Allocated length of following string. That goes for the
		     string *only*! The structure's size have to be added if
		     the entire alloc is wanted! Notice that the first (or
		     last) byte in the string belongs to the structure and
		     not the 'string'!!! */
  long len;	  /* length of following string */
  uchar string[1]; /* memory included in the string! */
};


struct fplVariable {
  long num;    /* Number of dimensions */
};

struct Identifier {
  /* This structure is used to store all identifiers in when they are "hashed
     in". Notice that *ALL* data in this structure is pointing and referring
     to the very same data as was sent to it, which means that you must keep
     that data intact while using FPL. */

  uchar *name; /* Indentifier. Must be absolutely unique and not more than
		 MAX_COMMAND_LEN characters long. */

  long number; /* identifier number */
  long linenum; /* virtual line number of the variable declaration/definition */
  
  union {
    struct ExternalFunction external;
    struct InsideFunction inside;
    struct fplVariable variable;
  } data;
  
  unsigned long flags; /* See below! */

  uchar *file;	/* file name of the file in which we find this identifier */
  
  struct Identifier *func; /* It exists only under this function. Pointer might
			      be NULL if in no function! */

  long level; /* In which level this exists.
		 Variables exist in all levels below (with a higher number)
		 where it was declared. Two declarations using the same
		 name in the same level is not allowed!
		 LONG_MAX if global! */
  
  unsigned long hash; /* Hash value. To get the proper hash table entry for
			 this, use [hash%HASH_TABLE_SIZE] */
  
  /* Bidirectional links to keep a hash value sorted order among
     functions using the same hash table entry: */
  struct Identifier *prev;
  struct Identifier *next;
};

/****** Identifier.flags defines:  ******/

/* Data type */

#define FPL_STRING_VARIABLE   (1<<0)  /* string variable */
#define FPL_INT_VARIABLE      (1<<1)  /* integer variable */
#define FPL_REFERENCE         (1<<2)  /* identifier reference */
#define FPL_INTERNAL_FUNCTION (1<<3)  /* internal FPL function */
#define FPL_EXTERNAL_FUNCTION (1<<4)  /* user supplied external function */
#define FPL_INSIDE_FUNCTION   (1<<5)  /* inside function in any program */
#define FPL_KEYWORD	      (1<<6)  /* this is a keyword identifier! */
#define FPL_KEYWORD_DECLARE   (1<<7)  /* declaring keyword */
#define FPL_INSIDE_NOTFOUND   (1<<8)  /* This inside function has not been
					 discovered yet. The position the
					 data points to is the search start
					 position. */
#define FPL_IGNORE            (1<<9) /* Read this and then drop it! */
/* Data status */

#define FPL_READONLY          (1<<10) /* const variable! */
#define FPL_EXTERNAL_VARIABLE (1<<11) /* external variable! new from V10 */
#define FPL_EXPORT_SYMBOL     (1<<12) /* cross program accessible */
#define FPL_GLOBAL_SYMBOL     (1<<13) /* global accessible in one file */
#define FPL_SHORT_VARIABLE    (1<<14) /* short (16-bit) variable */
#define FPL_CHAR_VARIABLE     (1<<15) /* char (8-bit) variable */
#define FPL_UNSIGNED_VARIABLE (1<<16) /* unsigned variable */
#define FPL_STATIC_VARIABLE   (1<<17) /* static variable! */

#define FPL_DEALLOC_NAME_ANYWAY (1<<18) /* In normal cases, this name should
                                           not get freed, but this is not a
                                           normal case... ;-P */

#define FPL_STATUS (FPL_READONLY|FPL_HIJACKED_VARIABLE|FPL_EXPORT_SYMBOL\
		    FPL_GLOBAL_SYMBOL|FPL_SHORT_VARIABLE|FPL_CHAR_VARIABLE\
		    FPL_UNSIGNED_VARIABLE|FPL_STATIC_VARIABLE|FPL_REFEREMCE)

/*
 * These two lower flags should be combined with the "Data status" flags
 * when declaring variables!
 */

#define FPLDECL_STRING	      (1<<31) /* Keyword declaring string */
#define FPLDEC_INT	      (1<<30) /* Keyword declaring int */


#define FPL_VARIABLE_LESS32 (FPL_SHORT_VARIABLE|FPL_CHAR_VARIABLE)
#define FPL_VARIABLE (FPL_STRING_VARIABLE|FPL_INT_VARIABLE)
#define FPL_FUNCTION (FPL_INTERNAL_FUNCTION|FPL_EXTERNAL_FUNCTION |\
		      FPL_INSIDE_FUNCTION)

/***** Identifier.ID defines: ******/

struct Position {
  /*
   * This struct should be used from now on when storing and using the
   * interpret position!
   */
  uchar *text;       /* Current interpret position */
  long prg;         /* Current line number */
  uchar *virfile;    /* virtual file name pointer */
  long virprg;	    /* virtual line number */
};


#define FPL_INSIDE_FUNCTION_ID -3;

struct Condition {
  uchar *bracetext;  /* pointer to the character to the right of the open
		       brace */
  long braceprg;    /* line number of the above data */
  uchar *check;      /* pointer to the expression. Used by while() and for() */
  long checkl;      /* the line number of the expression */
  uchar *postexpr;   /* USED BY "for" : pointer to statement3 */
  long postexprl;   /* USED BY "for" : statement3's line number */
};

struct Expr {  /* the numerical expression linked list */
  union {
    long val;		/* numerical return */
    struct fplStr *str; /* string return */
  } val;
  Operator operator;  /* see the operator enums! */
  struct Unary *unary; /* unary/primary operators linked list! */
  short flags;	      /* see below */
  struct Expr *next; 
};

/**** struct Expr.flags defines: ****/
#define FPL_STRING     (1<<0) /* Expr structure is a string. */
#define FPL_NOFREE     (1<<1) /* A returned string should not be freed */
#define FPL_OPERAND    (1<<2) /* Next part in the expression is a operand */
#define FPL_ACTION     (1<<3) /* The expression includes any variable change(s) */
#define FPL_BREAK      (1<<4) /* The val member specifies number of levels
				 left to break from! */
#define FPL_RETURN     (1<<5) /* There is a return call received, return to the
				 last function caller. */
#define FPL_CONTINUE   (1<<6) /* Continue is flagged! */

#define FPL_DEFUNCTION (1<<7) /* The Expression() just called declared AND
				 defined a function! */
#define FPL_BRACE      (1<<8) /* This invoke returned due to a closing brace! */

struct Local {
  /*
   * This structure will create a linked list of all local variables declared
   * in this level. When leaving this level, *ALL* variables with the names
   * that the ->ident member points to must be deleted, using DelIdentifier().
   */
  struct Identifier *ident;
		/* This pointer points to the Identifier structure,
		   that means this should *NOT* be freed individually
		   but only the entire structure and the Identifier structure
		   (and members) at the same time! */
  struct Local *next; /* Next member in this chain */
};

/*
 * All fplFunction ID's below zero are reserved for FPL internal use.
 * We use the funcdata member to set some flags:
 */
#define FPL_HASH_INSIDE   1
#define FPL_HASH_INTERNAL 2

struct Data {
  /*
   * Allocated at fplInit() and freed at fplFree().
   */
  
  uchar *text;      /* Current interpret position */
  long prg;         /* Current line number */
  uchar *virfile;   /* virtual file name pointer */
  uchar *filename;  /* actual file name pointer */
  long virprg;	    /* virtual line number */

  long size;        /* file size */
  uchar *program;   /* file total content! */
  
  uchar strret;	    /* The Script() now executing should return a
		       string! (TRUE/FALSE) */
  long level;	    /* Nesting level */
  long varlevel;    /* current variable level */

  struct Local *globals; /* Pointer to list holding all global symbols
			    currently declared in this program. They might be
			    removed when this program quits if the user has
			    set that flag or if we miss certain information */

  struct Local *locals; /* Linked list of local variables! If any error
			   code is returned, there might be local variables
			   left to free in any precious local level! Use this
			   list to delete 'em all!
			   
			   We add *ALL* levels to one list, separated with a
			   NULL name. Deleting only the latest level, deletes
			   to the nearest NULL name!
			   */

  long ret;		/* Return value of the FPL block */
  uchar *buf;		/* Global buffer pointer (Why use more than one buffer
			   at a time? It's only a waste of valuable stack!) */
  void *userdata;       /* Global Userdata. Free to use. */
  unsigned long flags;  /* Flags. See defines below! */
  long FPLret;	        /* FPL return code = the result of the
			   "exit(RETURN_CODE);" call! */
  long *returnint;	/* pointer to the above if an integer actually was
                           returned */
  struct Identifier **hash;  /* Must be set to NULL before doing anything
				major... like using fplAddFunction() or
				calling fplExecute[File]() The NULL-setting is
				done by fplInit(). */

  struct Local *usersym; /* Pointer to list holding all global symbols
			    declared in another FPL program run. These symbols
			    are legal global symbols */
  
  struct Identifier *func; /* pointer to the current interpreted function
			      or NULL */
  uchar **string_return;  /* whether this program should allow strings to be
			    returned to the host program (set with the
			    FPLTAG_STRING_RETURN tag to 'fplExecuteXXX()'). */
  uchar *identifier;	/* host program identifier. This should point to
  			   a unique string to enable separation between
  			   different processes use of FPL library functions! */

  long breaks; /* number of levels possible to break! Each nested for, do,
		  while and switch increases, each end of the same decreases
		  as well as break, return and exit */

  long conts; /* number of possible continue levels */

#define ADDBUFFER_SIZE 32 /* buffered size; before appending string */

  uchar addchar_buffer[ADDBUFFER_SIZE];
  long addchar_len;

  uchar switchtype; /* in a switch Script() call, this controls what kind of
		       'case' -expressions we should allow! */
#define SWITCH_NUM 1
#define SWITCH_STR 2

  long currvariables; /* current amount of variables declared in the
			 function */
  long maxvariables; /* maximum number of variables declared at the same time
			within a single function */
  long totalvariables; /* total number of variables declared in a single
			  function */
                          
  long *cmdline; /* pointer to array with command line info
                    see cmdline.h */
  char newerror; /* set when a new error has been reported. If Script()
                    fails without it being set, we know we have struck
                    one of those old-style error messages! */
};

/***** Data.flags: *****/

#define FPLDATA_ALLFUNCTIONS (1<<0)
/* Accept all functions, even if not found! */

#define FPLDATA_CACHEFILE (1<<1)
/* This file should be cached among the other global data. */

#define FPLDATA_CACHEALLFILES (1<<2)
/* This makes FPL store all files in memory that it has to remember */

#define FPLDATA_CACHEEXPORTS (1<<3)
/* This makes FPL store all files in memory that exports symbols */

#define FPLDATA_NESTED_COMMENTS (1<<6)
/* Allow nested comments */

#define FPLDATA_REREAD_CHANGES (1<<7)
/* Mark all files by default to get reread into memory when its accessed
   and the actual file is changed on disk, the symbols
   for that file will be removed and the file re-read into memory! */

#define FPLDATA_FLUSH_NOT_IN_USE (1<<8)
/* Mark all files by default to be flushed from memory when not used! */

#define FPLDATA_DEBUG_MODE (1<<9) /* currently running in debug mode! */
#define FPLDATA_DEBUG_GLOBAL (1<<10) /* always debug mode! */

#define FPLDATA_DEBUG (FPLDATA_DEBUG_MODE|FPLDATA_DEBUG_GLOBAL)

#define FPLDATA_KIDNAP_CACHED (1<<11) /* kidnap mode default */

#define FPLDATA_PREVENT_RUNNING_SAME (1<<12) /* never execute a file that is
						cached and hasn't been changed
						since last execution */

/**********************************************************************
 *                                                                    *
 * All functions used from external functions.                        *
 *                                                                    *
 **********************************************************************/

ReturnCode REGARGS fplExecuteFile(struct Data *, uchar *);
void * REGARGS fplInit(long *);
void REGARGS fplFree(struct Data *);

/**********************************************************************
 * All functions used globally in the library.                        *
 **********************************************************************/
ReturnCode ASM Script(AREG(2) struct Data *,
                      AREG(3) struct Expr *,
                      DREG(2) short);
ReturnCode  REGARGS Expression(struct Expr *, struct Data *, long, struct Identifier *);
ReturnCode  REGARGS Eat(struct Data *);
ReturnCode  REGARGS Getword(struct Data *);

uchar * REGARGS GetErrorMsg(struct Data *, long, uchar *);

ReturnCode  REGARGS ReadFile(void *, uchar *);
ReturnCode  REGARGS Newline(struct Data *);
long  REGARGS Strtol(uchar *, long, uchar **);

ReturnCode  REGARGS NewMember(struct Data *, struct Expr **);
ReturnCode  REGARGS AddVar(struct Data *, struct Identifier *, struct Local **,
                           uchar);
ReturnCode  REGARGS ReturnChar(struct Data *, long *, uchar);
ReturnCode  REGARGS GetEnd(struct Data *, uchar, uchar, uchar);

ReturnCode REGARGS AppendStringToString(struct Data *, struct fplStr **,
					uchar *, long);
ReturnCode REGARGS Send(struct Data *, unsigned long *);

unsigned long Gethash(uchar *);
ReturnCode  REGARGS GetIdentifier(struct Data *, uchar *, struct Identifier **);
ReturnCode  REGARGS DelIdentifier(struct Data *, uchar *, struct Identifier *);
void ASM *DefaultAlloc(DREG(0) long, AREG(0) void *);
void ASM DefaultDealloc(AREG(1) void *, DREG(0) long, AREG(0) void *);
long REGARGS DelLocalVar(struct Data *, struct Local **);
ReturnCode  REGARGS AddLevel(struct Data *);
long REGARGS ArrayNum(long, long, long *, long *);
ReturnCode REGARGS ArrayResize(struct Data *, long, long *,
                               struct Identifier *);

void  REGARGS CleanUp(struct Data *, long, long);
long REGARGS BitToggle(long, long, long);

ReturnCode REGARGS HashScan(struct Data *);

ReturnCode REGARGS StringExpr(struct Expr *, struct Data *);

ReturnCode REGARGS AddCharBuffer(struct Data *, struct fplStr **, long);
#define ADD_RESET -1 /* reset buffer */
#define ADD_FLUSH -2 /* flush buffer */

#include "comp.h"
#include "error.h" /* compiler error messages */
