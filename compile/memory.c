/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 memory.c

 Memory functions handling all allocating and freeing.

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

#ifdef UNIX
#include <sys/types.h>
#elif defined(AMIGA)
#include <exec/types.h>
#include <exec/memory.h>
#include <proto/exec.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "script.h"
#include "memory.h"
#include "debug.h"

static void  REGARGS LinkMemory(struct Data *, struct MemInfo *, uchar);
static void  REGARGS UnLinkMemory(struct Data *, struct MemInfo *, uchar);

#ifdef DEBUG
extern long mem;
extern long maxmem;
long malloc_count=0;
long Mmalloc_count=0;
long free_mem=0;
long max_free_mem=0;
#endif

/**********************************************************************
 *
 * Malloc();
 *
 * Allocate memory for real.
 *
 ******/

void ASM *Malloc(AREG(0) struct Data *scr, DREG(0) long size, DREG(1) uchar type
#ifdef DEBUGPARAMETERS1
DEBUGPARAMETERS1
#endif
)
{
  uchar *alloc;

#ifdef DEBUG
  struct MemInfo *meminfo;

  size+=MEMORY_COOKIE; /* add extra bytes after the block! */
#endif

  alloc=(uchar *)scr->Alloc(size+sizeof(struct MemInfo), scr->userdata);

  if(!alloc) {
    /* If the first alloc failed, flush caches and retry! */
    FlushFree(scr);
    alloc=(uchar *)scr->Alloc(size+sizeof(struct MemInfo),
                             scr->userdata);
    if(!alloc)
      return(NULL);
  }

  ((struct MemInfo *)alloc)->size=size | (type * ALLOCBIT);
  /* size = size | (type * 1<<31) */

  LinkMemory(scr, (struct MemInfo *)alloc, type);

#ifdef DEBUGPARAMETERS2
  meminfo=(struct MemInfo *)alloc;

  meminfo->line=line;
  meminfo->source=source; /* the two debugparameters */
#endif

#ifdef DEBUG
#if PRE_COOKIE>0
  /* Fill the pre_cookie with junk too! */
  memset((void *)meminfo->dummy, 0xbb,
	 sizeof(struct MemInfo)-offsetof(struct MemInfo, dummy));
#endif

#if MEMORY_COOKIE>0
  memset((uchar *)meminfo + sizeof(struct MemInfo) + size-MEMORY_COOKIE,
	 0xbb, MEMORY_COOKIE);
#endif

  Mmalloc_count++;
  malloc_count++;
  mem+=size+sizeof(struct MemInfo);
  if(mem>maxmem)
    maxmem=mem;
#endif
{static long total=0; total+=size; printf("\rMalloc %d", total)}

  return (void *) ((uchar *)alloc+sizeof(struct MemInfo));
}

/**********************************************************************
 *
 * MallocCycle();
 *
 * Get memory to a MALLOC_DYNAMIC alloc. The memory can be taken from the
 * memory cache if any is available.
 *
 ******/

void  *MallocCycle(struct Data *scr, long size
#ifdef DEBUGPARAMETERS2
DEBUGPARAMETERS2
#endif
)
{
#if MEMORY_QUEUE>0
  if(size<256) {
    register struct FreeBlock *pnt;
    size>>=4;
    if(pnt=scr->blox[size]) {
      scr->blox[size]=pnt->next;
      scr->blockcount[size]--;
#ifdef DEBUGPARAMETERS2
      pnt->mem.line=line;
      pnt->mem.source=source; /* the two debugparameters */
#endif
#ifdef DEBUG
      Mmalloc_count++;
      free_mem-=(((struct MemInfo *)pnt)->size&SIZEBITS)+
	sizeof(struct MemInfo);
      CheckMem(scr, (uchar *)pnt+sizeof(struct MemInfo));
#endif
      return (void *)((uchar *)pnt+sizeof(struct MemInfo));
    } else
      size=(size<<4)+15;
  }
#endif

#ifdef DEBUGPARAMETERS2
  return Malloc(scr, size, MALLOC_DYNAMIC, source, line);
#else
  return Malloc(scr, size, MALLOC_DYNAMIC);
#endif
}

/**********************************************************************
 *
 * DefaultAlloc();
 *
 * THis function allocates memory from the system. Replaceable with the
 * tag FPLTAG_INTERNAL_ALLOC.
 *
 ******/

void ASM *DefaultAlloc(DREG(0) long size, AREG(0) void *userdata)
{
#ifdef AMIGA
  return (void *)AllocMem(size, MEMF_ANY);
#elif defined(UNIX)
  return (void *)malloc(size);
#endif
}

/**********************************************************************
 *
 * DefaultDealloc();
 *
 * This functions does nothing but returns allocated memory to the system.
 * It can be replaced by a user function specified with the tag
 * FPLTAG_INTERNAL_DEALLOC.
 *
 ******/
 
void ASM DefaultDealloc(AREG(1) void *point,
                        DREG(0) long size,
                        AREG(0) void *userdata)
{
#ifdef AMIGA  
  FreeMem(point, size);
#elif UNIX
  free(point);
#endif
}


/**********************************************************************
 *
 * LinkMemory();
 *
 * Adds the specifed memory area to the certain memory type list.
 *
 *****/

static void REGARGS
LinkMemory(struct Data *scr,
           struct MemInfo *point,
           uchar type)
{
  point->prev=scr->MallocKey[type];	 /* previous */
  point->next=NULL;		 /* next */
  if(scr->MallocKey[type])
    point->prev->next=point;
  scr->MallocKey[type] = (void *)point;
}


/**********************************************************************
 *
 * UnLinkMemory();
 *
 * Deletes the specifed memory area from the certain memory type list.
 *
 *****/

static void REGARGS
UnLinkMemory(struct Data *scr,
             struct MemInfo *point,
             uchar type)
{
  if(scr->MallocKey[type]==point) {
    /* if this is the last Malloc, set `last' to `prev' */
    scr->MallocKey[type]=point->prev;
    if(scr->MallocKey[type])
      scr->MallocKey[type]->next=NULL;
  } else {
    /* point the previous' `next' to our `next', and our next `previous'
       to our `previous'. Unlink us from the chain */
    if(point->prev)
      /* only if we aren't the _first_ Malloc() ! */
      point->prev->next=point->next;
    if(point->next)
      /* only if there is a next! */
      point->next->prev=point->prev;
  }
}


/**********************************************************************
 *
 * FreeCycle();
 *
 * Free the MALLOC_DYNAMIC allocated memory to the memory cache, or if
 * that is full, free for real.
 *
 * In this routine we do not have to bitwise and SIZEBITS since the ALLOCBIT
 * is never set when this is called!
 *
 ******/

void  REGARGS FreeCycle(struct Data *scr, void *ptr)
{
  struct MemInfo *point=(struct MemInfo *)((uchar *)ptr-sizeof(struct MemInfo));
  /* `point' points to the MemInfo structure */
#if defined(DEBUG) && defined(UNIX)
  if(point->size&ALLOCBIT) {
    fprintf(stderr, "*WARNING* Illegal free() of %d bytes!\n",
	    point->size&SIZEBITS);
  }
#endif

#ifdef DEBUG
  CheckMem(scr, ptr);
#endif

#if MEMORY_QUEUE>0
  if(point->size<256+MEMORY_COOKIE) {
    register long size=point->size-MEMORY_COOKIE>>4;
    if(scr->blockcount[size]<MEMORY_QUEUE_SIZE) {
#ifdef DEBUG
      memset(ptr, 0xaa, point->size-MEMORY_COOKIE);

      free_mem+=point->size+sizeof(struct MemInfo);
      if(free_mem>max_free_mem)
	max_free_mem=free_mem;
#endif
      ((struct FreeBlock *)point)->next=scr->blox[size];
      scr->blox[size]=(struct FreeBlock *)point;
      scr->blockcount[size]++;
      return;
    }
  }
#endif
  Free(scr, ptr, MALLOC_DYNAMIC);
}

/**********************************************************************
 *
 * Free();
 *
 * Free a allocated memory area of a certain type. The freed memory
 * area gets freed `for real'.
 *
 *****/

void ASM Free(AREG(0) struct Data *scr, AREG(1) void *ptr, DREG(0) uchar type)
{
  struct MemInfo *point=(struct MemInfo *)((uchar *)ptr-sizeof(struct MemInfo));
  /* `point' points to the MemInfo structure: */
#ifdef DEBUG
  CheckMem(scr, ptr);
#endif
  UnLinkMemory(scr, point, type);
#ifdef DEBUG
  mem-=(point->size&SIZEBITS)+sizeof(struct MemInfo);
#endif
  scr->Dealloc(point,
               (point->size&SIZEBITS)+sizeof(struct MemInfo),
               scr->userdata);
}

#ifdef DEBUG
/**********************************************************************
 *
 * CheckMem();
 *
 * Check a special malloc to see that it hasn't overwritten it's boundaries.
 *
 ******/

ReturnCode REGARGS CheckMem(struct Data *scr, void *ptr)
{
  long i;
  long b, c;
  uchar *data;
  struct MemInfo *point;
  /* `point' points to the MemInfo structure: */
  point=(struct MemInfo *)((uchar *)ptr-sizeof(struct MemInfo));

#if MEMORY_COOKIE>0
  for(i=b=0;i<MEMORY_COOKIE; i++) {
    data=(uchar *)point+ sizeof(struct MemInfo)+i+(point->size&SIZEBITS)-MEMORY_COOKIE;
    if(*data!= 0xbb)
      b++;
  }

  if(b) {
#ifdef AMIGA
    /* ERROR */
    point=NULL;
#else
    fprintf(stderr, "Memory violation: malloc(%d) was abused %d bytes after block!\n",
	    (point->size&SIZEBITS), b);
#endif
  }
#endif

#if PRE_COOKIE>0
  for(c=i=0;i<sizeof(struct MemInfo)-offsetof(struct MemInfo, dummy); i++)
    if(point->dummy[i]!= 0xbb)
      c++;

  if(c) {
#ifdef AMIGA
    /* ERROR */
    point=NULL;
#else
    fprintf(stderr, "Memory violation: malloc(%d) was abused %d bytes before block!\n",
	    (point->size&SIZEBITS), c);
#endif
  }
#endif

  return(b+c?FPLERR_OUT_OF_MEMORY:FPL_OK);
}
#endif

/**********************************************************************
 *
 * FreeAll();
 *
 * Free all memory allocated with specified type!
 *
 ******/

void REGARGS
FreeAll(struct Data *scr,
        uchar type)
{
  struct MemInfo *point;
  void *prev;
#ifdef DEBUG
  long size;
#endif
  if(!scr->MallocKey[type])
    return;
  do {
    point=scr->MallocKey[type];
    /* `point' points to the MemInfo structure! */

    prev=(void *)point->prev;

#ifdef DEBUG
    mem-=(point->size&SIZEBITS)+sizeof(struct MemInfo);
    size=(point->size&SIZEBITS);

    if(type == MALLOC_DYNAMIC)
      free_mem=0; /* no free mem left in memory queuing */
#endif
    scr->Dealloc(scr->MallocKey[type],
                 (point->size&SIZEBITS)+sizeof(struct MemInfo),
                 scr->userdata);
  } while(scr->MallocKey[type]=prev);
  if(type==MALLOC_DYNAMIC)
    InitFree(scr); /* init the memory cache tables to prevent accidents with
		      FPLSEND_FLUSHCACHE */
}

/**********************************************************************
 *
 * InitFree()
 *
 * Initialize the memory cache/queue.
 *
 ******/

void REGARGS InitFree(struct Data *scr)
{
  memset(scr->blox, 0, sizeof(struct FreeBlock *)*BLOCK_ENTRIES);
  memset(scr->blockcount, 0, sizeof(long)*BLOCK_ENTRIES);
}

/**********************************************************************
 *
 * FlushFree();
 *
 * Flush the memory chache.
 *
 *******/

void REGARGS FlushFree(struct Data *scr)
{
  register long i;
  for(i=0; i<BLOCK_ENTRIES;i++) {
    register struct FreeBlock *pnt;
    while(pnt=scr->blox[i]) {
      scr->blox[i]=pnt->next; /* delete block from chain */
      scr->blockcount[i]--;   /* count down counter */
      /* free block for real */
      Free(scr, (uchar *)pnt+sizeof(struct MemInfo), MALLOC_DYNAMIC);
    }
  }
}

/**********************************************************************
 *
 * SwapMem();
 *
 * If type is MALLOC_STATIC or MALLOC_DYNAMIC, this function will secure that
 * the memory area will be of that kind!
 *
 * THE AREA MUST BE ALLOCATED WHEN THIS CONVERTION HAPPENS!!! IT MUST NOT
 * BE IN THE FREE MEMORY CACHE/QUEUE LIST.
 *
 * This function is mainly used when storing global data. Since all
 * MALLOC_DYNAMIC data gets freed at the end of a FPL program run, we must
 * convert the mallocs to be able to keep them for next execution!
 *
 *****/

void REGARGS SwapMem(struct Data *scr, void *ptr, uchar type)
{
  struct MemInfo *point;
  if(ptr) {
    /* points to something! */

    point=(struct MemInfo *)((uchar *)ptr-sizeof(struct MemInfo));
    /* `point' points to the MemInfo structure: */
    if(point->size&ALLOCBIT && type==MALLOC_DYNAMIC) {
      /* This is a MALLOC_STATIC alloc */
      UnLinkMemory(scr, point, MALLOC_STATIC); /* take away from static list */
      point->size&=SIZEBITS;
      LinkMemory(scr, point, MALLOC_DYNAMIC);  /* insert in dynamic list */
    } else if(!(point->size&ALLOCBIT) && type==MALLOC_STATIC) {
      /* This is a MALLOC_DYNAMIC alloc */
      UnLinkMemory(scr, point, MALLOC_DYNAMIC); /* take away from dynamic list */
      point->size|=ALLOCBIT;
      LinkMemory(scr, point, MALLOC_STATIC);    /* insert in static list */
    }
    /* If no if() statement was reached, the memory area type was corect! */
  }
}

/**********************************************************************
 *
 * TypeMem()
 *
 * Returns the type of the specified memory pointer.
 * Returns MALLOC_STATIC or MALLOC_DYNAMIC.
 *
 ******************************************/

uchar REGARGS TypeMem(void *ptr)
{
  struct MemInfo *point=(struct MemInfo *)((uchar *)ptr-sizeof(struct MemInfo));
  /* `point' points to the MemInfo structure: */
  if(point->size&ALLOCBIT)
    return MALLOC_STATIC;
  return MALLOC_DYNAMIC;
}

/**********************************************************************
 *
 * fplAlloc()
 *
 * fplAlloc() allocates memory and assign it to the fpl internal list of
 * used memory. If fplFree() is called, this memory area will be free'd if
 * not earlier. Free this memory using fplDealloc().
 *
 ******/

void PREFIX *fplAlloc(AREG(0) struct Data *scr, DREG(0) long size)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplAlloc");
#endif
  if(!scr)
    return(NULL);
  return MALLOCA(size);
}

/**********************************************************************
 *
 * fplDealloc()
 *
 * This function frees memory previously allocated with fplAlloc();
 *
 *****/

void PREFIX fplDealloc(AREG(0) struct Data *scr, AREG(1) void *ptr)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplDealloc");
#endif
  if(!scr)
    return;
  FREE_KIND(ptr);
}

/**********************************************************************
 *
 * fplAlloca()
 *
 * fplAlloca() allocates memory and assign it to the fpl internal list of
 * used memory. The memory will only exist while executing the current file or
 * until next program has been executed. If fplFree() is called, this memory
 * area will also be free'd. Free this memory using fplDealloca(). This function
 * will use the FPL internal memory caching system.
 *
 ******/

void PREFIX *fplAlloca(AREG(0) struct Data *scr, DREG(0) long size)
{
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplAlloca");
#endif
  if(!scr)
    return(NULL);
  return MALLOC(size);
}

/**********************************************************************
 *
 * fplDealloca()
 *
 * This function frees memory previously allocated with fplAlloca();
 *
 *****/

void PREFIX fplDealloca(AREG(0) struct Data *scr, AREG(1) void *ptr)
{
  fplDealloc(scr, ptr);
}

/***************************************************************
 *
 * fplAllocString()
 *
 * Returns a pointer to a memory block of the requested size.
 * The memory block is initialized as a string and can be used
 * when returning strings to FPL with fplSend(), without having
 * to allocate memory, make FPL have to allocate memory and copy
 * all your data and then you free your memory again.
 *  Now all you have to do is allocate a string at once and then
 * tell FPL in fplSend() using the FPLSEND_DONTCOPY_STRING. The memory
 * block will then be used as it is by FPL.
 *
 * If you want to free the memory, you do that with fplFreeString().
 * You must not free memory sent to FPL!
 *
 ********/

void PREFIX *fplAllocString(AREG(0) struct Data *scr, DREG(0) long size)
{
  uchar *ptr;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplAllocString");
#endif
  /*
   * Allocate the memory DYNAMIC and convert it to staic later so that the
   * memory block gets aligned properly and then later can get converted to
   * DYNAMIC kind again without any risking anything!
   */
  ptr = MALLOC(size + sizeof(struct fplStr)); 

  if(!ptr)
    return NULL;
  
  /*
   * Convert this to static, most strings allocated like this are likely to
   * be used as "static" memory areas!
   */
  SwapMem(scr, ptr, MALLOC_STATIC);

  ((struct fplStr *)ptr)->alloc=size;
  ((struct fplStr *)ptr)->len=size; /* set default to entire string */

  ptr+=offsetof(struct fplStr, string); /* make 'ptr' point to the string
					   entry */
  return ptr; /* return string must be freed with fplFreeString() */
}

void PREFIX fplFreeString(AREG(0) struct Data *scr, AREG(1) void *ptr)
{
  uchar *point=ptr;
#ifdef DEBUGMAIL
  DebugMail(scr, MAIL_FUNCTION, 500, "fplFreeString");
#endif
  if(!point)
    return;

  point-=offsetof(struct fplStr, string); /* go to beginning of allocation */

  FREEA(point); /* free the string! */
}

/**********************************************************************
 *
 * FreeKind()
 *
 * Frees the memory area, of both dynamic and static kinds!
 *
 ****************/

void REGARGS FreeKind(struct Data *scr, void *memory)
{
  if(MALLOC_DYNAMIC == TypeMem(memory)) {
    FREE(memory);
  }
  else {
    FREEA(memory);
  }
}
