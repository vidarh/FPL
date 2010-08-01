/******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************
 
 Reference.c
 
 Routines for interpreting variable references from the interface function.
 
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

#include <stddef.h>
#include "script.h"

#if defined(UNIX)
#include <stdio.h>
#ifdef SUNOS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#endif

#include "reference.h"

struct fplStr EmptyString={0,0,0};

#ifndef AMIGA /* if not using SAS/C on Amiga */

#ifdef VARARG_FUNCTIONS

/*
 * The following stub functions to the actual library functions was done
 * to make this run under SunOS 4.1.x using gcc. Thanks to Bernd Noll
 * (b_noll@sun.rhrk.un) for this contribution!
 */

long fplReferenceTags(void *anchor, void *refID, ...)
{
  va_list tags;
  long ret;
#ifdef SUNOS
  va_start(tags); /* get parameter list */
#else
  va_start(tags, refID); /* get parameter list */
#endif
  ret = fplReference(anchor, refID, (unsigned long *)tags);
  va_end(tags);
  return ret;
}

#else /* VARARG_FUNCTIONS */

long PREFIX fplReferenceTags(void *anchor, void *refID, unsigned long tags, ...)
{
  return(fplReference(anchor, refID, &tags));
}

#endif

#endif

/**********************************************************************
 *
 * fplReference()
 *
 * Handles all referencing to FPL internal symbols from the interface
 * function.
 *
 ******/

ReturnCode PREFIX
fplReference(AREG(0) struct Data *scr,
             AREG(1) struct Identifier *ident,
             AREG(2) unsigned long *tags)
{
  ReturnCode ret = FPL_OK;
  long item=0;
  long a=0;
  long *dims;
  long my_strlen=-1;
  struct fplRef *ref;
  while(tags && *tags) {
    switch(*tags++) {
    case FPLREF_ARRAY_RESIZE:		/* version 11 magic! */
      if(ident->flags&FPL_VARIABLE && ident->data.variable.num) {
        /*
         * It is a variable and it is at least a one dimensional array!
         */
        ref = (struct fplRef *)(*tags);
        CALL( ArrayResize(scr,
                          ref->Dimensions,
                          ref->ArraySize,
                          ident));
      }
      break;
      
    case FPLREF_ARRAY_ITEM:
      if(ident->flags&FPL_VARIABLE) {
	dims = (long *)(*tags);
	while(dims[a]>0)
	  a++;
	item = ArrayNum(a, ident->data.variable.num,
			dims, ident->data.variable.dims);
	if(item<0)
	  item=0; /* just ignore stupid values! */
        a=0; /* reset 'a' again */
      }
      break;
    case FPLREF_ARRAY_INFO:
      /*
       * Fill out the structure given to us by the caller!
       */
      if(ident->flags&FPL_VARIABLE && ident->data.variable.num) {
	ref = (struct fplRef *)(*tags);
	ref->Dimensions = ident->data.variable.num;  /* number of dims */
	ref->ArraySize  = ident->data.variable.dims; /* actual dims */
      }
      break;
    case FPLREF_NAME:
      /*
       * Receive name in supplied char pointer!
       */
      *(uchar **)(*tags) = ident->name;
      break;
    case FPLREF_TYPE:
      /*
       * Receive flags in supplied long!
       */
      *(long *)(*tags) =
        (ident->flags&FPL_STRING_VARIABLE?FPLREF_TYPE_STRING:0)|
	  (ident->flags&FPL_INT_VARIABLE?FPLREF_TYPE_INTEGER:0)|
	    (ident->flags&FPL_VARIABLE?
	     (ident->data.variable.num?FPLREF_TYPE_ARRAY:0):0 )|
	       (ident->flags&FPL_FUNCTION?FPLREF_TYPE_FUNCTION:0);
      break;

    case FPLREF_GET_STRING:
      if(ident->flags&FPL_STRING_VARIABLE &&
	 item<ident->data.variable.size) {
        if(ident->data.variable.var.str[item]) {
          /*
           * This is a string!
           */
          *(uchar **)(*tags) = ident->data.variable.var.str[item]->string;
        }
        else {
          /*
           * Can't return NULL for no string, return pointer to ""!
           */
          *(uchar **)(*tags) = EmptyString.string;
        }
      }
      else {
	*(uchar **)(*tags) = NULL;
      }
      break;

    case FPLREF_SET_MY_STRLEN:
      my_strlen=(long)(*tags);
      break;
    case FPLREF_SET_MY_STRING:
      a=1;
    case FPLREF_SET_STRING:
      if(ident->flags&FPL_STRING_VARIABLE &&
	 item<ident->data.variable.size &&
         *tags) {
	/*
	 * Get the allocation type.
	 */
	uchar type=MALLOC_DYNAMIC; /* default memory state */
	struct fplStr *str;
	if(ident->data.variable.var.str[item]) {
	  type = TypeMem(ident->data.variable.var.str[item]);

	  if(MALLOC_STATIC == type ) {
	    /*
	     * The previous allocation was static!
	     */
	    FREEA( ident->data.variable.var.str[item] );
	  }
          else {
            FREE( ident->data.variable.var.str[item] );
          }
	}
	if(1 == a) {
	  /*
	   * This is a FPLREF_SET_MY_STRING call. That means we got a regular
	   * C style char pointer to a zero terminated string here, allocate
	   * a regular FPL string and copy the string into that one
	   */
	  register long len = 0>my_strlen?strlen((uchar *)*tags):my_strlen;
          GETMEM(str, len + sizeof(struct fplStr));
          str->alloc= len;
          str->len= len; /* set default to entire string */

          /* copy the data into the string */
          memcpy(str->string, (uchar *)(*tags), len);
	}
	else {
          /*
           * We received a pointer to the ->string member of a fplStr struct.
           * Make 'str' point to the struct itself!
           */
	  str = (struct fplStr *)
	    (((uchar *)(*tags)) - offsetof(struct fplStr, string));
        }

	ident->data.variable.var.str[item] = str;

        str->string[str->len] = '\0'; /* force zero termination! */
	
	if(MALLOC_STATIC == type || ident->flags&FPL_EXPORT_SYMBOL) {
	  /*
	   * The previous memory was static, which probably means that
	   * this is an exported or global variable which required
	   * static existance. Swap it back to that state!
	   */
	  SwapMem(scr,
                  ident->data.variable.var.str[item],
                  MALLOC_STATIC);
	}
        else {
          /*
           * Make sure this allocation is dynamic!
           */
	  SwapMem(scr,
                  ident->data.variable.var.str[item],
                  MALLOC_DYNAMIC);
        }
      }
      else {
	*(uchar **)(*tags) = NULL;
      }
      break;

    case FPLREF_GET_INTEGER:
      if(ident->flags&FPL_INT_VARIABLE &&
	 item<ident->data.variable.size) {
	/* we supply a pointer to it, then we can return NULL if there was
	   something that looked fishy! */
        *(long **)(*tags) = &ident->data.variable.var.val32[item];
      }
      else {
	*(long **)(*tags) = NULL;
      }
      break;

    case FPLREF_SET_INTEGER:
      if(ident->flags&FPL_INT_VARIABLE &&
         item<ident->data.variable.size) {
	/* set integer variable item */
        ident->data.variable.var.val32[item] = (*tags);
      }
      break;
    }
    tags++;
  }
  return ret;
}

