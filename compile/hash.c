/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 hash.c

 Functions for FPL hash tables and sorting!

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
#include <proto/exec.h>
#include <dos.h>
#else
#include <stdio.h>
#endif
#include "script.h"
#include <limits.h>

#ifdef DEBUG
#include <stdio.h>
#endif

static long IdentNumber=0; /* identifier number to increase at every declaration */

static ReturnCode REGARGS AddIdentifier(struct Data *, struct Identifier *);
static ReturnCode INLINE InitHash(struct Data *);
static void * INLINE Init(struct Data *scr, long*);

/**********************************************************************
 *
 * int AddVar();
 *
 * Frontend to the AddIdentifier function.
 *
 * This routine adds a member to the linked list of local variable names.
 * That list exists to enable easy and fast removal of local variables
 * when leaving a block within which local variables has been declared!
 *
 * Make sure that the name member data is static as long we need this list
 * cause this routine doesn't copy that name, simply points to it!
 *
 *****/


ReturnCode REGARGS
AddVar(struct Data *scr, /* pointer to struct Data */
       struct Identifier *ident,/* identifier struct pointer */
       struct Local **local,
       uchar output)
{
  ReturnCode ret;
  struct Local *temp;
  if(ret=AddIdentifier(scr, ident))
    INFO(scr, CERROR_IDENTIFIER_USED, ident->name);
  else {
    GETMEM(temp, sizeof(struct Local));  
    temp->next=*local;
    temp->ident=ident;
    *local=temp;

    scr->currvariables++; /* increase number of current symbols */
    scr->totalvariables++; /* increase total number of symbols */
    ident->number= ++IdentNumber;
    if(output) {
      CALL(PutArg(scr, COMP_DECLARE, ident->flags));
      if(!(ident->flags&FPL_EXPORT_SYMBOL)) {
        CALL(PutArg(scr, COMP_NOTHING, ident->number));
      }
      else {
        CALL(PutArg(scr, COMP_NOTHING, ident->hash));
      }
      CALL(PutString(scr, COMP_NOTHING, ident->name, -1));
    }
  }
  return(ret);
}

/**********************************************************************
 *
 * AddLevel();
 *
 * This function adds a NULL-name in the local symbol list to visualize
 * the beginning of a new variable level!
 *
 *******/

ReturnCode REGARGS
AddLevel(struct Data *scr)
{
  struct Local *temp;
  GETMEM(temp, sizeof(struct Local));  
  temp->next=scr->locals;
  temp->ident=NULL;
  scr->locals=temp;
  return(FPL_OK);
}


/**********************************************************************
 *
 * int DelLocalVar()
 *
 * This routine deletes all members to the linked list of local variable
 * names. Call this routine every time you leave a local level. Deletes
 * all variables and the following NULL name!
 *
 *****/

long REGARGS
DelLocalVar(struct Data *scr,
            struct Local **local)
{
  /* This removes only all listed symbols! */
  long deleted=0;
  struct Identifier *ident;
  while(*local) {
    struct Local *temp=(*local)->next;
    ident=(*local)->ident;
    FREE(*local);
    *local=temp;
    if(ident) {
      deleted++;
      scr->currvariables--; /* count down the current amount */
      DelIdentifier(scr, NULL, ident); /* delete it for real */
    }
    else
      break;
  }
  return deleted;
}


/**********************************************************************
 *
 * int AddIdentifier()
 *
 * This function adds the function to the hash table according to all
 * parameters.
 *
 * If the hash member of the Data structure is NULL, the hash table
 * will be inited. No not init the hash list if you don't have to cause
 * that sure is a peep hole in the performance...
 *
 *******/

static ReturnCode REGARGS
AddIdentifier(struct Data *scr,
              struct Identifier *ident)
{
  unsigned long hash;       /* hash number of the identifier */
  struct Identifier **add;  /* where to store the pointer to this identifier */
  struct Identifier *prev=NULL; /* pointer to previous hash structure */
  struct Identifier *next;  /* pointer to next hash structure */
  hash=Gethash(ident->name);
  
  add=(struct Identifier **)&scr->hash[hash % FPL_HASH_SIZE];
  while(*add) {
    if((*add)->hash==hash) {
      /* they were identical */
      if(ident->flags&FPL_FUNCTION &&
	 !strcmp((*add)->name, ident->name) &&
/*
  PREV STOOPID WAY:
	 (!ident->file || !strcmp(ident->file, (*add)->file))) { */

	 (ident->flags&FPL_EXPORT_SYMBOL || !strcmp(ident->file, (*add)->file))) {
	/* if it's already there, fail!!! */
	return FPLERR_IDENTIFIER_USED;
      } else
	/* add it here! */
	break; 
    } else if((*add)->hash>hash) {
      /* continue search for a place to insert */
      /* 'add' now points to the pointer */
      prev=(*add);
      add=(struct Identifier **)&((*add)->next);
    } else {
      /* insert it here! */
      prev=(*add)->prev;
      break;
    }
  }

  next=(*add);
  *add=ident;
  (*add)->hash=hash;
  (*add)->prev=prev;
  (*add)->next=next;
  if(next)
    next->prev=ident;
  return(FPL_OK);
}

/**********************************************************************
 *
 * int GetIdentifier();
 *
 * Sets the pointer to the Identifier structure to which the name
 * fits, in the third argument.
 *
 *****/

#ifdef DEBUG
int hashed=0;
int max_hashed=0;
#endif

ReturnCode REGARGS
GetIdentifier(struct Data *scr,
              uchar *name,
	      struct Identifier **ident)
{
  struct Identifier *get;
  unsigned long hash=Gethash(name);
  get=scr->hash[hash%FPL_HASH_SIZE];
#ifdef DEBUG
  hashed=0;
#endif

  while(get) {
    if(

       (get->hash==hash) && 
       /* identical hash value! */

       !strcmp(get->name, name) &&
       /* identical name! */

       (get->flags&(FPL_GLOBAL_SYMBOL|FPL_EXPORT_SYMBOL) ||
        (get->func==scr->func && get->level<=scr->varlevel))
       /* If not global, declared under the *same* function, in this or
	  a lower level! */

       ) {

      /* this is it! */
      *ident=get;
#ifdef DEBUG
      if(hashed>max_hashed)
	max_hashed=hashed;
#endif
      return(FPL_OK);
    } else if(get->hash<hash)
      /* we've been searching through all possible alternatives! */
      break;
#ifdef DEBUG
    hashed++;
#endif
    get=get->next;
  }    
  *ident=NULL;
  return(FPLERR_IDENTIFIER_NOT_FOUND);
}


/**********************************************************************
 *
 * int InitHash()
 *
 * Initialize the hash table. Simple and quick!
 *
 *****/

struct ShitData {
  uchar *name;
  long ID;
  uchar ret;
  uchar *format;
};

struct MoreShitData {
  uchar *name;
  long ID;
  long flags;
};

static ReturnCode INLINE InitHash(struct Data *scr)
{
  ReturnCode ret;
  static struct ShitData internal_functions[]={
    {"abs",		FNC_ABS,	'I', "I"},
    {"atoi",		FNC_ATOI,	'I', "S"},
    {"closelib",	FNC_CLOSELIB,	'I', "S"},  /* amiga only */
    {"debug",		FNC_DEBUG,	'I', "i"},
    {"eval",		FNC_EVAL,	'I', "S"},
    {"exists",          FNC_EXISTS,     'I', "Si"},
    {"interpret",	FNC_INTERPRET,	'I', "S"},
    {"itoa",		FNC_ITOA,	'S', "I"},
    {"itoc",		FNC_ITOC,	'S', "I"},
    {"joinstr",		FNC_JOINSTR,	'S', "s>"},
    {"ltostr",		FNC_LTOSTR,	'S', "Ii"},
    {"openlib",		FNC_OPENLIB,	'I', "SI"}, /* amiga only */
    {"rename",		FNC_RENAME,     'I', "SS"},
    {"sprintf",		FNC_SPRINTF,	'S', "Sa>"},
    {"sscanf",          FNC_SSCANF,     'I', "SSa>"},
    {"strcmp",		FNC_STRCMP,	'I', "SS"},
    {"stricmp",		FNC_STRICMP,	'I', "SS"},
    {"strlen",		FNC_STRLEN,	'I', "S"},
    {"strncmp",		FNC_STRNCMP,	'I', "SSI"},
    {"strnicmp",	FNC_STRNICMP,	'I', "SSI"},
    {"strstr",		FNC_STRSTR,	'I', "SSi"},
    {"stristr",		FNC_STRISTR,	'I', "SSi"},
    {"strtol",		FNC_STRTOL,	'I', "Si"},
    {"substr",		FNC_SUBSTR,	'S', "SII"},
  };

  /* FPL keywords. "else" is not included (treated special). Is is
     defines as KEYWORD_ELSE */

  static struct MoreShitData keywords[]={
    {"auto",	CMD_AUTO,	FPL_KEYWORD_DECLARE},
    {"break",	CMD_BREAK,	0},
    {"case",	CMD_CASE,	0},
    {"char",	CMD_INT,	FPL_KEYWORD_DECLARE|FPL_CHAR_VARIABLE},
    {"const",	CMD_CONST,	FPL_KEYWORD_DECLARE},
    {"continue", CMD_CONTINUE,	0},
    {"default",	CMD_DEFAULT,	0},
    {"do",	CMD_DO,		0},
    {"double",	CMD_DOUBLE,	FPL_IGNORE},
    {"else",	CMD_ELSE,	0},
    {"enum",	CMD_ENUM,	FPL_IGNORE},
    {"exit",	CMD_EXIT,	0},
    {"export",	CMD_EXPORT,	FPL_KEYWORD_DECLARE},
    {"float",   CMD_FLOAT,	FPL_IGNORE},
    {"for",	CMD_FOR,	0},
    {"if",	CMD_IF,		0},
    {"int",	CMD_INT,	FPL_KEYWORD_DECLARE},
    {"long",	CMD_INT,	FPL_KEYWORD_DECLARE},
    {"register",CMD_REGISTER,	FPL_KEYWORD_DECLARE},
    {"resize",	CMD_RESIZE,	0},
    {"return",	CMD_RETURN,	0},
    {"short",	CMD_INT,	FPL_KEYWORD_DECLARE|FPL_SHORT_VARIABLE},
    {"signed",	CMD_SIGNED,	FPL_KEYWORD_DECLARE|FPL_IGNORE},
    {"static",  CMD_STATIC,	FPL_KEYWORD_DECLARE},
    {"string",	CMD_STRING,	FPL_KEYWORD_DECLARE},
    {"struct",  CMD_STRUCT,	FPL_IGNORE},
    {"switch",	CMD_SWITCH,	0},
    {"typedef",	CMD_TYPEDEF,	FPL_KEYWORD_DECLARE},
    {"union",	CMD_UNION,	FPL_IGNORE},
    {"unsigned",CMD_UNSIGNED,	FPL_KEYWORD_DECLARE|FPL_IGNORE},
    {"void",	CMD_VOID,	FPL_KEYWORD_DECLARE},
    {"volatile",CMD_VOLATILE,	FPL_KEYWORD_DECLARE|FPL_IGNORE},
    {"while",	CMD_WHILE,	0},
  };
  long i;
  struct Identifier *ident;
  GETMEMA(scr->hash, sizeof(struct Identifier *)* FPL_HASH_SIZE);

  memset((void *)scr->hash, 0, sizeof(struct Identifier *)*FPL_HASH_SIZE);
  /*
   * The hash table initialization gives us a brilliant chance to bring up
   * the execution speed even more by inserting the few internal functions
   * into this same table. The functions will then act *EXACTLY* the same
   * and we can shorten the code and much easier write internal functions
   * that return strings...
   */

  for(i=0; i<sizeof(internal_functions)/sizeof(struct ShitData);i++) {
    GETMEMA(ident, sizeof(struct Identifier));
    ident->name=internal_functions[i].name;
    ident->data.external.ID=internal_functions[i].ID;
    ident->data.external.ret=internal_functions[i].ret;
    ident->data.external.format=internal_functions[i].format;
    ident->flags=FPL_INTERNAL_FUNCTION|FPL_EXPORT_SYMBOL;
    ident->level=0;
    ident->func=NULL; /* all functions */
    ident->file= "<FPL>"; /* internal */
    ident->number = 0;
    ret=AddIdentifier(scr, ident);
    if(ret)
      break;
  }
  for(i=0; i<sizeof(keywords)/sizeof(struct MoreShitData);i++) {
    GETMEMA(ident, sizeof(struct Identifier));
    ident->name=keywords[i].name;
    ident->data.external.ID=keywords[i].ID;  /* dirty enum work! */
    ident->flags=FPL_EXPORT_SYMBOL|FPL_KEYWORD|keywords[i].flags;
    ident->level=0;
    ident->func=NULL;  /* all functions */
    ident->file= "<FPL>";  /* internal */
    ident->number = 0;
    ret=AddIdentifier(scr, ident);
    if(ret)
      break;
  }
  return(ret);
}

/**********************************************************************
 *
 * int Gethash();
 *
 * Return the hash number for the name received as argument.
 *
 *****/

unsigned long Gethash(uchar *name)
{
  unsigned long hash=0;
  while(*name)
    hash=(hash<<1)+*name+++(hash&(1<<31)?-2000000000:0);
  return(hash);
}

/**********************************************************************
 *
 * void Free();
 *
 * This function frees the resources used by this FPL session.
 *
 ***********/

void REGARGS fplFree(struct Data *scr)
{
  struct Data onstack;
  onstack=*scr; /* copy the entire struct */
  scr=&onstack; /* use the `stack-struct' */
  FREEALL();
}

/**********************************************************************
 *
 * int DelIdentifier()
 *
 * Delete an identifier from the hash table. Specify 'name' or 'ident'.
 *
 ******/

ReturnCode REGARGS
DelIdentifier(struct Data *scr,
              uchar *name, /* only needed if 'ident' is NULL! */
	      struct Identifier *ident)
{
  ReturnCode ret=FPL_OK;
  if(!ident) {
    /* Get the structure pointer */
    CALL(GetIdentifier(scr, name, &ident));
  }
  
  /* Link the previous member in the list to the next member */
  if(ident->prev)
    /* If there is a previous member */
    ident->prev->next=ident->next;
  else
    /* if this was the first in the list */
    scr->hash[ident->hash%FPL_HASH_SIZE]=ident->next;

  if(ident->next)
    ident->next->prev=ident->prev;

  if(ident->flags&FPL_INSIDE_FUNCTION &&
     ident->data.inside.format) {
    FREE(ident->data.inside.format);
  }
  if((ident->flags&FPL_EXTERNAL_FUNCTION)||
     (ident->flags&(FPL_INTERNAL_FUNCTION|FPL_KEYWORD)))
    /* internal or external function/keyword */
    ;
  else  {
    FREE(ident->name);
  }
  FREE(ident);
  return(ret);
}


/**********************************************************************
 *
 * fplInit();
 *
 * Initialize a lot of FPL internal structures and references. Returns
 * NULL if anything went wrong!
 *
 *******/

void * REGARGS
fplInit(long *cmdline)
{
  struct Data point;
  struct Data *scr;
  void *init;
  scr=&point;

  init=Init(&point, cmdline);
  if(init) {
    if(PutOpen(scr, NULL))
      init=NULL;
  }
  
  if(!init)
    FREEALLA();
  
  return(init);
}

static void * INLINE
Init(struct Data *scr,	/* stack oriented */
     long *cmdline) /* command line info */
{
  ReturnCode ret;
  uchar *buffer;
  struct Data *ptr;

  memset(scr, 0, sizeof(struct Data)); /* NULLs everything! */
  
  /*
   * Set some flags according to command line parameters:
   */
  if(cmdline[LINE_COMMENTNEST] ) {
    scr->flags |= FPLDATA_NESTED_COMMENTS;
  }

  buffer=(uchar *)MALLOCA(BUF_SIZE);
  if(!buffer)
    /* fail! */
    return(NULL);

  scr->buf=buffer;
  scr->cmdline = cmdline;

  if(ret=InitHash(scr))
    return(NULL);

  ptr=(struct Data *)MALLOCA(sizeof(struct Data));
  if(ptr)
    *ptr=*scr; /* copy the entire structure! */

  IdentNumber=0; /* identifier number to increase at every declaration */
  return((void *)ptr);
}


long REGARGS
BitToggle(long original, /* Original 32 bits */
	  long bit,      /* alternate bit pattern */
	  long OnOff)    /* Or/And boolean, TRUE==OR, FALSE==AND */
{
  if(OnOff)
    return ( original | bit );
  else
    return ( original & ~bit );
}
