/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 memory.h

 Structures and defines for memory functions.

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

struct MemInfo {
  struct MemInfo *prev;
  struct MemInfo *next;
  long size;		/* the most significant bit in this number will be
			   set if this is a MALLOC_STATIC type malloc! */
#ifdef DEBUG
#ifdef DEBUGPARAMETERS2
  long line;
  uchar *source;
#endif /* debugparameters */

#if PRE_COOKIE>0
  uchar dummy[PRE_COOKIE];
#endif /* pre cookie */
#endif
};

struct FreeBlock {
  struct MemInfo mem;
  struct FreeBlock *next;
};
