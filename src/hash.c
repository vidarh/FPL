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
#ifndef __AROS__
#include <proto/exec.h>
#include <dos.h>
#endif
#else
#include <stdio.h>
#ifdef SUNOS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#endif
#include "script.h"
#include "debug.h"
#include <limits.h>

#ifdef DEBUG
#include <stdio.h>
#endif

static ReturnCode INLINE InitHash(struct Data *);
static unsigned long INLINE Gethash(uchar *);
static void * Init(struct Data *, long ASM (*)(AREG(0) void *), unsigned long *);
static ReturnCode REGARGS SetTags(struct Data *, unsigned long *);
static ReturnCode INLINE Hijack(struct Data *, struct Identifier *);
static ReturnCode REGARGS AddIdentifier(struct Data *, struct Identifier *);


ReturnCode REGARGS
    RenameIdentifier(struct Data *scr,
		     struct Identifier *ident, /* existing identifier */
		     uchar *newname)           /* new name */
{
    ReturnCode ret;
    struct Identifier *nident;
    ret = GetIdentifier(scr, newname, &nident);
    if(ret != FPLERR_IDENTIFIER_NOT_FOUND) {
      return FPLERR_IDENTIFIER_USED; /* !!! */
    }

    if(ident->flags&FPL_KEYWORD ||
       !(ident->flags&(FPL_EXTERNAL_FUNCTION|FPL_EXPORT_SYMBOL))) {
      /*
       * Nonononono... we have to draw the limit somewhere!!!
       */
      return FPLERR_IDENTIFIER_USED;
    }
    /*
     * Let's take the old one out of its chains!
     */
    if(ident->prev) {
      /* If there is a previous member */
      ident->prev->next=ident->next;
    }
    else {
      /* if this was the first in the list */
      scr->hash[ident->hash%scr->hash_size]=ident->next;
    }
    if(ident->next) {
      ident->next->prev=ident->prev;
    }
    if(ident->flags&(FPL_INTERNAL_FUNCTION|FPL_EXTERNAL_FUNCTION)) {
      if(ident->flags&FPL_DEALLOC_NAME_ANYWAY) {
        /* Oh well, this has already been renamed once! */
        FREE_KIND(ident->name);
      }
      else
        ident->flags |= FPL_DEALLOC_NAME_ANYWAY;
        /* this have to be known since this kind of functions regularly uses
           names in the user-area which never get freed by us! */
    }
    else {
      FREE_KIND(ident->name);
    }
    STRDUPA(ident->name, newname);
    CALL( AddIdentifier(scr, ident));
    return ret;
}

#ifndef AMIGA /* if not using SAS/C on Amiga */

#ifdef VARARG_FUNCTIONS
long fplAddFunctionTags(void *anchor, uchar *name, long ID, uchar rtrn,
                        uchar *format, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, format); /* get parameter list */
#endif
  ret = fplAddFunction(anchor, name, ID, rtrn, format, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else
long PREFIX fplAddFunctionTags(void *anchor, uchar *name, long ID, uchar rtrn,
                               uchar *format, unsigned long tags, ...)
{
  return(fplAddFunction(anchor, name, ID, rtrn, format, &tags));
}
#endif

#endif

/**********************************************************************
 *
 * int fplAddFunction();
 *
 * User frontend to AddIdentifier().
 *
 *****/

ReturnCode PREFIX
  fplAddFunction(AREG(0) struct Data *scr,      /* pointer to struct Data */
		 AREG(1) uchar *name,     /* name of function */
		 DREG(0) long ID,	 /* function ID */
		 DREG(1) uchar rtrn,      /* return type */
		 AREG(2) uchar *format,   /* format string */
		 AREG(3) unsigned long *tags) /* taglist pointer */
{
  ReturnCode ret;
  struct Identifier *ident;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplAddFunction");
#endif
  if(!scr)
    return(FPLERR_ILLEGAL_ANCHOR);

  ident=MALLOCA(sizeof(struct Identifier));
  if(!ident)
    return(FPLERR_OUT_OF_MEMORY);

  memset(&ident->data.external, 0, sizeof(struct ExternalFunction));
  while(tags && *tags) {
    switch(*tags++) {
    case FPLTAG_USERDATA:
      ident->data.external.data=(void *)*tags;
      break;
    case FPLTAG_FUNCTION:
      ident->data.external.func=(long (*)(void *))*tags;
      break;
    }
    tags++; /* next! */
  }

  ident->name = name;
  ident->data.external.ID = ID;
  ident->data.external.ret = rtrn;
  ident->data.external.format = format;
  ident->flags = FPL_EXTERNAL_FUNCTION|FPL_GLOBAL_SYMBOL|FPL_EXPORT_SYMBOL;
  ident->file = "<user>"; /* User added! */
  ident->func = NULL; /* everywhere! */
  ident->level = 0;

  CALL(AddIdentifier(scr, ident));
  return(FPL_OK);
}

/**********************************************************************
 *
 * int fplDelFunction();
 *
 * User frontend to DelIdentifier().
 *
 ******/

ReturnCode PREFIX fplDelFunction(AREG(0) struct Data *scr,
				 AREG(1) uchar *name)
{
  ReturnCode ret;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplDelFunction");
#endif
  if(!scr)
    return(FPLERR_ILLEGAL_ANCHOR);
  CALL(DelIdentifier(scr, name, NULL));
  return(FPL_OK);
}


/**********************************************************************
 *
 * int AddToList();
 *
 * Add the ident pointer the Local list.
 *
 *********************************************************************/
ReturnCode REGARGS
AddToList(struct Data *scr, /* pointer to struct Data */
          struct Identifier *ident,/* identifier struct pointer */
          struct Local **local)
{
  struct Local *temp;
  GETMEM(temp, sizeof(struct Local));  
  temp->next=*local;
  temp->ident=ident;
  *local=temp;
  return FPL_OK;
}

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
       struct Local **local)
{
  ReturnCode ret;
  if(ret=AddIdentifier(scr, ident))
    ;
  else {
    ret = AddToList(scr, ident, local);
  }
  return ret;
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

ReturnCode REGARGS
DelLocalVar(struct Data *scr,
            struct Local **local)
{
  /* This removes only all listed symbols! */
  struct Identifier *ident;
  while(*local) {
    struct Local *temp=(*local)->next;
    ident=(*local)->ident;
    FREE(*local);
    *local=temp;
    if(ident)
      DelIdentifier(scr, NULL, ident); /* delete it for real */
    else
      break;
  }
  return(FPL_OK);
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

ReturnCode REGARGS static
AddIdentifier(struct Data *scr,
              struct Identifier *ident)
{
  unsigned long hash;       /* hash number of the identifier */
  struct Identifier **add;  /* where to store the pointer to this identifier */
  struct Identifier *prev=NULL; /* pointer to previous hash structure */
  struct Identifier *next;  /* pointer to next hash structure */
  hash=Gethash(ident->name);
  
  add=(struct Identifier **)&scr->hash[hash % scr->hash_size];
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
  ReturnCode ret;
  struct Identifier *get;
  unsigned long hash=Gethash(name);
  get=scr->hash[hash%scr->hash_size];
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
        (get->func==scr->func && get->level<=scr->varlevel)) &&
       /* If not global, declared under the *same* function, in this or
	  a lower level! */

       (get->flags&FPL_EXPORT_SYMBOL || !strcmp(get->file, scr->prog->name))
       /* If not cross-file, the same file! */

       ) {

      /* this is it! */
      *ident=get;
#ifdef DEBUG
      if(hashed>max_hashed)
	max_hashed=hashed;
#endif
      if(get->flags&FPL_EXTERNAL_VARIABLE) {
        CALL(Hijack(scr, get));
      }
      else if(scr->flags&FPLDATA_ISOLATE && get->flags&FPL_EXPORT_SYMBOL) {
        /*
         * Isolated and exported symbol... really?
         */
        if((get->flags&(FPL_FUNCTION|FPL_KEYWORD) == FPL_INSIDE_FUNCTION) ||
           get->flags&FPL_VARIABLE) {
          /*
           * Nope, this symbol shouldn't get returned!
           */
          break;
        }
      }
#ifdef DEBUGMAIL
      DebugMail(scr, MAIL_IDENTIFIER_ACCESS, 0, get);
#endif
      return FPL_OK;
    } else if(get->hash<hash)
      /* we've been searching through all possible alternatives! */
      break;
#ifdef DEBUG
    hashed++;
#endif
    get=get->next;
  }
  *ident=NULL;
  return FPLERR_IDENTIFIER_NOT_FOUND;
}


#ifndef AMIGA /* if not using SAS/C on Amiga */

#ifdef VARARG_FUNCTIONS
long fplAddVariableTags(void *anchor, uchar *name, long ID, uchar type,
                        void *defvalue, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, defvalue); /* get parameter list */
#endif
  ret = fplAddVariable(anchor, name, ID, type, defvalue, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else
long PREFIX fplAddVariableTags(void *anchor, uchar *name, long ID, uchar type,
                               void *defvalue, unsigned long tags, ...)
{
  return fplAddVariable(anchor, name, ID, type, defvalue, &tags);
}
#endif

#endif

/**********************************************************************
 *
 * int fplAddVariable();
 *
 * User frontend to AddIdentifier(). New in version 10!
 *
 *****/

ReturnCode PREFIX
  fplAddVariable(AREG(0) struct Data *scr, /* pointer to struct Data */
		 AREG(1) uchar *name,       /* name of variable */
		 DREG(0) long ID,	   /* variable ID */
		 DREG(1) uchar type,        /* variable type */
                 AREG(2) void *defvalue,   /* default value */
		 AREG(3) unsigned long *tags) /* taglist pointer */
{
  ReturnCode ret;
  struct Identifier *ident;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplAddVariable");
#endif
  if(!scr)
    return(FPLERR_ILLEGAL_ANCHOR);

  GETMEMA(ident, sizeof(struct Identifier));

  memset(&ident->data.variable, 0, sizeof(struct fplVariable));

  ident->name = name;
  ident->data.variable.ID = ID;
  ident->flags = FPL_EXTERNAL_VARIABLE|FPL_GLOBAL_SYMBOL|FPL_EXPORT_SYMBOL|
    (type == FPL_STRARG? FPL_STRING_VARIABLE:FPL_INT_VARIABLE)|
      FPL_READONLY;
  if(type == FPL_INTARG) {
    GETMEM(ident->data.variable.var.val32, sizeof(long));
    ident->data.variable.var.val32[0] = (long) defvalue;
  }
  else
    if(defvalue) {
      register long len = (long)strlen((uchar *)defvalue);
      if(len) {
        GETMEM(ident->data.variable.var.str, sizeof(struct fplStr *));
        GETMEM(ident->data.variable.var.str[0], len + sizeof(struct fplStr));
        ident->data.variable.var.str[0]->len = len;
        ident->data.variable.var.str[0]->alloc = len;
        memcpy(ident->data.variable.var.str[0]->string, defvalue,
               len);
        ident->data.variable.var.str[0]->string[len] = '\0'; /* Z-terminate */
      }
    }
  ident->file = "<user>"; /* Used added! */
  ident->func = NULL; /* everywhere! */
  ident->level = 0;

  CALL(AddIdentifier(scr, ident));
  return(FPL_OK);
}

/*************************************************************************
 *
 * Hijack();
 *
 * This function gets called whenever an external variable has been selected
 * by GetIdentifier().
 *
 ******/

static ReturnCode INLINE Hijack(struct Data *scr, struct Identifier *ident)
{
  struct fplArgument pass;
  struct fplMsg *msg;
  ReturnCode ret;

  memset(&pass, 0, sizeof(struct fplArgument));
  pass.argc=0;
  pass.name=ident->name;
  pass.ID=ident->data.variable.ID;
  pass.key=scr;

  pass.variable = (void *)
    (ident->flags&FPL_STRING_VARIABLE?
     (ident->data.variable.var.str &&ident->data.variable.var.str[0]?
      ident->data.variable.var.str[0]->string:""):
     (void *)ident->data.variable.var.val32[0]);

  CALL(InterfaceCall(scr, &pass, scr->function));
  if(ident->flags&FPL_INT_VARIABLE) {
    /*
     * Integer variable!
     */
    CALL(GetMessage(scr, FPLMSG_RETURN, &msg));
    if(msg && msg->flags&FPLMSG_FLG_INT) {
      ident->data.variable.var.val32[0]=(long)msg->message[0];
      CALL(DeleteMessage(scr, msg));
    }
  } else {
    /*
     * String variable!
     */
    CALL(GetMessage(scr, FPLMSG_RETURN, &msg));
    if(msg && msg->flags&FPLMSG_FLG_STRING) {
      if(!ident->data.variable.var.str) {
	GETMEMA(ident->data.variable.var.str, sizeof(struct fplStr *));
      }
      else if(ident->data.variable.var.str[0]) {
	FREEA(ident->data.variable.var.str[0]);
      }
      ident->data.variable.var.str[0]=(struct fplStr *)msg->message[0];
      /*
       * Make the string to be static always!
       */
      SwapMem(scr, ident->data.variable.var.str[0], MALLOC_STATIC);
      DeleteMessage(scr, msg);
    }
  }
  return(FPL_OK);
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
    {"typedef",	CMD_TYPEDEF,	0},
    {"union",	CMD_UNION,	FPL_IGNORE},
    {"unsigned",CMD_UNSIGNED,	FPL_KEYWORD_DECLARE|FPL_IGNORE},
    {"void",	CMD_VOID,	FPL_KEYWORD_DECLARE},
    {"volatile",CMD_VOLATILE,	FPL_KEYWORD_DECLARE|FPL_IGNORE},
    {"while",	CMD_WHILE,	0},
  };
  long i;
  struct Identifier *ident;
  GETMEMA(scr->hash, sizeof(struct Identifier *)* scr->hash_size);

  memset((void *)scr->hash, 0, sizeof(struct Identifier *)*scr->hash_size);
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

static unsigned long INLINE Gethash(uchar *name)
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

void PREFIX fplFree(AREG(0) struct Data *scr)
{
  struct Data onstack;
#ifdef AMIGA
  long retval;
#endif
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplFree");
#endif
  onstack=*scr; /* copy the entire struct */
  scr=&onstack; /* use the `stack-struct' */
  DelProgram(scr, NULL); /* remove all programs from memory, some might be
			    Lock()'ed! */
#if defined(AMIGA) && defined(SHARED) /* only amiga supports funclibs! */
  CloseLib(scr, NULL, TRUE, &retval); /* force close of all funclibs */
#endif
  FREEALL();
  FREEALLA();
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
  long i;
  struct fplVariable *var;

  if(!ident) {
    /* Get the structure pointer */
    CALL(GetIdentifier(scr, name, &ident));
  }

  if( (ident->flags&FPL_EXPORT_SYMBOL) ||
     !(ident->flags & FPL_COMPILER_ADDED)) {
    /*
     * Symbols added by compiled programs are not added to the hash
     * list (unless they are exported)!
     */
    
    /* Link the previous member in the list to the next member */
    if(ident->prev)
      /* If there is a previous member */
      ident->prev->next=ident->next;
    else
      /* if this was the first in the list */
      scr->hash[ident->hash%scr->hash_size]=ident->next;
  
    if(ident->next)
      ident->next->prev=ident->prev;
  }

  /*
   * If it is any kind of funtion, all the data the pointers points to
   * should (in the specs) be static and should therefore *NOT* be
   * freed here!
   *
   * Notice that even internal functions are possible to remove here...
   */

  if(ident->flags & FPL_VARIABLE) {
    /*
     * It's a variable identifier. Free some members:
     */
    
    var=&ident->data.variable;
    
    if(ident->flags & FPL_REFERENCE) {
      /* only keep a reference pointer! */
    }
    else {
      if(ident->flags & FPL_STRING_VARIABLE) {
	/* it's a string array! */
	for(i=0; i<var->size; i++)
	  if(var->var.str[i]) {
	    FREE_KIND(var->var.str[i]);
	  }
      }
      if(var->num)
	FREE_KIND(var->dims);
      FREE_KIND(var->var.val);
    }
  } else if(ident->flags&FPL_INSIDE_FUNCTION &&
            ident->data.inside.format) {
    FREE_KIND(ident->data.inside.format);
  }
  if((ident->flags&FPL_EXTERNAL_FUNCTION)||
     (ident->flags&(FPL_INTERNAL_FUNCTION|FPL_KEYWORD))) {
    /* internal or external function/keyword */
    if(ident->flags&FPL_DEALLOC_NAME_ANYWAY)
      /* this name has been renamed into this! */
      FREE_KIND(ident->name);
    FREEA(ident);
  } else  {
    if(!(ident->flags&FPL_COMPILER_ADDED)) {
      FREE_KIND(ident->name);
    }
    FREE_KIND(ident);
  }
  return(ret);
}


#if !defined(AMIGA) || !defined(SHARED)  /* if not using SAS/C on Amiga */

#ifdef VARARG_FUNCTIONS
void *fplInitTags(long (*func)(void *), ...)
{
  va_list tags;
  void *ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, func); /* get parameter list */
#endif
  ret = fplInit(func, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else
PREFIX void *fplInitTags(long (*func)(void *), unsigned long tags, ...)
{
  return(fplInit(func, (unsigned long *)&tags));
}
#endif

#endif

/**********************************************************************
 *
 * fplInit();
 *
 * Initialize a lot of FPL internal structures and references. Returns
 * NULL if anything went wrong!
 *
 *******/

void * ASM fplInit(AREG(0) long (*function) (void *),
		   /* function handler pointer */
		   AREG(1) unsigned long *tags) /* taglist */
{
  struct Data point;
  struct Data *scr;
  void *init;
  scr=&point;

#if defined(__AMIGA) && !defined(__AROS__)
  /* Store all register before loading index register */
  StoreRegisters(scr);
  geta4();
#endif

  init=Init(&point, function, tags);
  if(!init)
    FREEALLA();
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplInit");
#endif
  return(init);
}

static void * Init(struct Data *scr,	/* stack oriented */
			  long ASM (*function) (AREG(0) void *), 
			  unsigned long *tags) /* taglist */
{
  ReturnCode ret;
  uchar *buffer;
  struct Data *ptr;
#ifdef AMIGA
  long registers[11];

  memcpy(registers, scr->registers, sizeof(long)*11);
#endif
  /* Set default that just might get changed in SetTags(); */

  memset(scr, 0, sizeof(struct Data)); /* NULLs everything! */

  scr->Alloc=DefaultAlloc;
  scr->Dealloc=DefaultDealloc;;
  scr->hash_size=FPL_HASH_SIZE;
  scr->runs=0;
  InitFree(scr); /* init memory caches */

#ifdef AMIGA

  memcpy(scr->registers, registers, sizeof(long)*11);

  scr->stack_size=FPL_MIN_STACK;
  scr->stack_max=FPL_MAX_STACK;
  scr->stack_limit=FPL_MAX_LIMIT;
  scr->stack_margin=FPLSTACK_MINIMUM;
#endif

  SetTags(scr, tags); /* read tags and set proper members */

  buffer=(uchar *)MALLOCA(BUF_SIZE);
  if(!buffer)
    /* fail! */
    return(NULL);

#ifdef AMIGA
#pragma msg 225 ignore	/* ignore the 225 warnings that occur on the following
			   assign! */
#endif
  scr->function=(long ASM (*)(AREG(0) void *))function;

#ifdef AMIGA
#pragma msg 225 warning	/* enable the 225 warnings again! */
#endif

  scr->buf=buffer;

#if defined(AMIGA) && defined(SHARED)
  scr->stack_base=MALLOCA(scr->stack_size);
  if(!scr->stack_base)
    return(NULL);
  scr->intern_stack = (long)scr->stack_base + scr->stack_size;
  Forbid();
  scr->task = FindTask(NULL);  /* get pointer to our task! */
  Permit();
#endif

  if(ret=InitHash(scr))
    return(NULL);

  ptr=(struct Data *)MALLOCA(sizeof(struct Data));
  if(ptr)
    *ptr=*scr; /* copy the entire structure! */

  return((void *)ptr);
}

#ifdef VARARG_FUNCTIONS
long fplResetTags(void *anchor, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, anchor); /* get parameter list */
#endif
  ret = fplReset(anchor, (unsigned long *)tags);
  va_end(tags);
  return ret;
}
#else
long PREFIX fplResetTags(void *anchor, unsigned long tags, ...)
{
  return(fplReset(anchor, &tags));
}
#endif


/**********************************************************************
 *
 * fplReset();
 *
 * This function is used to change or add tags to FPL. All tags
 * available for fplFree() is legal. Not changed tags will remain
 * as they were before this call!
 *
 * I had to insert this function since I found out that I wanted to
 * alter the userdata in my application using FPL, and that was hard
 * doing so (nice) without this change.
 * 
 * Library front end to SetTags();
 *
 *****/

ReturnCode PREFIX fplReset(AREG(0) struct Data *scr,
		 	   AREG(1) unsigned long *tags)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplReset");
#endif
  if(!scr)
    return FPLERR_ILLEGAL_ANCHOR;
  return SetTags(scr, tags);
}


/**********************************************************************
 *
 * SetTags();
 *
 * Read the taglist supplied in the second parameter, and set all data
 * according to those.
 *
 *****/

static ReturnCode REGARGS
SetTags(struct Data *scr,
        unsigned long *tags)
{
  if(!scr)
    return(FPLERR_ILLEGAL_ANCHOR);

  while(tags && *tags) {
    switch(*tags++) {
#ifdef AMIGA
#pragma msg 225 ignore	/* ignore the 225 warnings that occur on the following
			   four assigns! */
#endif
    case FPLTAG_INTERVAL:
      scr->interfunc=(long ASM (*)(AREG(0) void *))*tags;
      break;

    case FPLTAG_INTERNAL_ALLOC:
      scr->Alloc=(void * ASM (*)(DREG(0) long,
								 AREG(0) void *))*tags;
      break;
    case FPLTAG_INTERNAL_DEALLOC:
      scr->Dealloc=(void ASM (*)(AREG(1) void *,
								 DREG(0) long,
								 AREG(0) void *))*tags;
      break;
#ifdef AMIGA
#pragma msg 225 warning /* enable the 225 warning again for correct program
			   checking! */
#endif

    case FPLTAG_REREAD_CHANGES:
      scr->flags = BitToggle(scr->flags, FPLDATA_REREAD_CHANGES, *tags);
      break;

    case FPLTAG_FLUSH_NOT_IN_USE:
      scr->flags = BitToggle(scr->flags, FPLDATA_FLUSH_NOT_IN_USE, *tags);
      break;

    case FPLTAG_KIDNAP_CACHED:
      scr->flags = BitToggle(scr->flags, FPLDATA_KIDNAP_CACHED, *tags);
      break;

    case FPLTAG_PREVENT_RUNNING_SAME:
      scr->flags = BitToggle(scr->flags, FPLDATA_PREVENT_RUNNING_SAME, *tags);
      break;

    case FPLTAG_AUTOCOMPILE:
      scr->flags = BitToggle(scr->flags, FPLDATA_AUTOCOMPILE, *tags);
      break;

    case FPLTAG_AUTORUN:
      scr->flags = BitToggle(scr->flags, FPLDATA_AUTORUN, *tags);
      break;
      
    case FPLTAG_HASH_TABLE_SIZE:
      if(*tags>FPL_MIN_HASH)
	scr->hash_size=*tags;
      break;

    case FPLTAG_USERDATA:
      scr->userdata=(void *)*tags;
      break;

    case FPLTAG_ALLFUNCTIONS:
      scr->flags = BitToggle(scr->flags, FPLDATA_ALLFUNCTIONS, *tags);
      break;

    case FPLTAG_NESTED_COMMENTS:
      scr->flags = BitToggle(scr->flags, FPLDATA_NESTED_COMMENTS, *tags);
      break;

    case FPLTAG_CACHEALLFILES:
      if(*tags) {
	scr->flags|=FPLDATA_CACHEALLFILES;
	if(*tags == FPLCACHE_EXPORTS)
	  scr->flags|=FPLDATA_CACHEEXPORTS;
      } else
	scr->flags &= ~(FPLDATA_CACHEALLFILES|FPLDATA_CACHEEXPORTS);
      break;

#ifdef AMIGA
    case FPLTAG_STACK:
      /* Only change stack if the required size is large enough! */
      if(*tags>FPL_MIN_STACK)
	scr->stack_size=(long)*tags;
      break;

    case FPLTAG_MAXSTACK:
      /* Only change this if the required size is large enough! */
      if(*tags>FPL_MIN_STACK)
	scr->stack_max=(long)*tags;
      break;

    case FPLTAG_STACKLIMIT:
      /* Only change this if the required size is large enough! */
      if(*tags>FPL_MIN_STACK)
	scr->stack_limit=(long)*tags;      
      break;

    case FPLTAG_MINSTACK:
      /* Only change this if the required size is larger than default! */
      if(*tags>FPLSTACK_MINIMUM)
	scr->stack_margin=*tags;
      break;
#endif

    case FPLTAG_IDENTITY:
      /* new from version 9: Host process identifier! */
      scr->identifier = (uchar *)(*tags);
      break;
      
    case FPLTAG_DEBUG:
      scr->flags = BitToggle(scr->flags, FPLDATA_DEBUG_GLOBAL, *tags);
      break;
      
    case FPLTAG_ERROR_BUFFER:
      scr->error = (uchar *)(*tags);
      break;

    }
    tags++; /* next! */
  }
  return(FPL_OK);
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
