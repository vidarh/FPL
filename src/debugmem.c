#ifdef RCS
static char rcsid[] = "$Id: debugmem.c,v 1.2 1996/02/29 14:10:50 dast Exp $";
#endif
/******************************************************************************
 *                        FRONTEC RAILWAY SYSTEMS AB
 * ----------------------------------------------------------------------------
 *
 * Project: FR-3000 CS
 * $Source: /home/pop/proj/rail/usr/dast/dancer/dancer/RCS/debugmem.c,v $
 * $Revision: 1.2 $
 * $Date: 1996/02/29 14:10:50 $
 * $Author: dast $
 * $State: Exp $
 * $Locker:  $
 *
 * ----------------------------------------------------------------------------
 * $Log: debugmem.c,v $
 * Revision 1.2  1996/02/29 14:10:50  dast
 * small change
 *
 * Revision 1.1  1996/01/18 12:32:11  dast
 * Initial revision
 *
 * Revision 3.7  1994/11/30  08:58:22  dast
 * Fixed a bug in the header
 *
 *****************************************************************************/

 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef AMIGA
#include <exec/types.h>
#include <exec/memory.h>
#include <proto/exec.h>
#endif

#include "debugmem.h"

/*
#undef malloc
#undef free
*/

static int bytes_alloc_I=0;
static struct DebugMem *MallocKey =NULL;
static char Verbose=0;

struct DebugMem {
	struct DebugMem *prev;
	struct DebugMem *next;
	int size_I;
#ifdef SOURCE_INFO
	char *source_PC;
	int line_I;
#endif
};

void UnLinkMemory( struct DebugMem * );
void LinkMemory ( struct DebugMem * );

/*************************************************************************
*
* Function   : DBG_realloc
* Purpose    : Debug version of the system function realloc()
* Parameters : memory pointer, new size, source file, line number
* Returns    : new allocation or NULL if failed
*
****************************************************************************/

void * DBG_realloc( OldBuf_P, size_I, source_PC, line_I )
	 void *		OldBuf_P;
	 int	 	size_I;	
	 char *		source_PC;
	 int		line_I;
{
	struct DebugMem *debug_PS;
	void	*NewBuf_P;

	if ( OldBuf_P == NULL )
		/* *NOT* ANSI return!!! */
		return DBG_malloc( size_I, source_PC, line_I );

	debug_PS = (struct DebugMem *)
		((char *) OldBuf_P - sizeof( struct DebugMem ) - PRE_COOKIE_SIZE);

#if 0
    /* this code prevents reallocing to less size */
	if ( size_I <= debug_PS->size_I )
		return OldBuf_P;
#endif

    if ( (NewBuf_P = DBG_malloc( size_I, source_PC, line_I )) == NULL )
		return NULL;

	memcpy( NewBuf_P, OldBuf_P, debug_PS->size_I );
	DBG_free( OldBuf_P, source_PC, line_I );
	return NewBuf_P;
}

/***************************************************************************
*
* Function   : DBG_Verbose
* Purpose    : Switch on verbose mode for debugging
* Parameters : TRUE/FALSE
* Returns    : none
*
*****************************************************************************/

void DBG_Verbose (char verbose_C)
{
	Verbose = verbose_C;
}

/*************************************************************************
*
* Function   : DBG_calloc
* Purpose    : Add debug level to the system function calloc()
* Parameters : number of itmes, itemsize, source file, line
* Returns    : allocated [cleared] memory or NULL if failed
*
**********************************************************************/

void * DBG_calloc ( items_I, itemsize_I, source_PC, line_I )
	 int	 	items_I;
	 int	 	itemsize_I;
	 char *		source_PC;
	 int		line_I;
{
  int size_I = items_I * itemsize_I;
  void *mem = DBG_malloc(size_I, source_PC, line_I);
  if(mem)
    memset(mem, 0, size_I); /* clear it */
  return mem;
}

/*************************************************************************
*
* Function   : DBG_malloc
* Purpose    : Add debug level to the system function malloc()
* Parameters : size, source file, line
* Returns    : allocated memory or NULL if failed
*
**********************************************************************/

void * DBG_malloc ( size_I, source_PC, line_I )
	 int	 	size_I;	
	 char *		source_PC;
	 int		line_I;
{
	struct DebugMem *debug_PS;
	void *p;

	/* Get some extra memory to make room for an extra
	   integer storage to store the malloc()'ed size! */
#ifdef AMIGA
	debug_PS = (struct DebugMem *) _MALLOC( size_I +
						  sizeof( struct DebugMem ) +
						  PRE_COOKIE_SIZE +
						  POST_COOKIE_SIZE, MEMF_ANY );
#else
	debug_PS = (struct DebugMem *) _MALLOC( size_I +
						  sizeof( struct DebugMem ) +
						  PRE_COOKIE_SIZE +
						  POST_COOKIE_SIZE );
#endif

	if ( !debug_PS )
		return NULL;
	
	if(Verbose) {
		MALLOCED(size_I, source_PC, line_I);
	}
	/* The malloc() did succeed */
	debug_PS->size_I = size_I ; /* remember size of malloc() */
	bytes_alloc_I += size_I ; /* count the malloc()s */

	LinkMemory( debug_PS );
	
#ifdef SOURCE_INFO
	debug_PS->source_PC = source_PC;
	debug_PS->line_I    = line_I;
#endif
	
#if PRE_COOKIE_SIZE > 0
	memset((char*)debug_PS + sizeof( struct DebugMem ), PRE_COOKIE_FILL_BYTE,
		   PRE_COOKIE_SIZE);
#endif
	
#if POST_COOKIE_SIZE > 0
	memset((char*)debug_PS + sizeof( struct DebugMem ) + size_I + PRE_COOKIE_SIZE,
		   POST_COOKIE_FILL_BYTE, POST_COOKIE_SIZE);
#endif
	

	p = (char*)debug_PS + sizeof( struct DebugMem ) + PRE_COOKIE_SIZE;
	
	return p;
}

/********************************************************************
 *
 * Function   : DBG_free
 * Purpose    : Add debug level to the system function free()
 * Parameters : memory pointer, source file, source line
 * Returns    : none
 *
 ***********************************************************************/

void DBG_free ( mem_P,
#ifdef FREE_WITH_SIZE
                size_I,
#endif
                source_PC, line_I )
	 void *		mem_P;
#ifdef FREE_WITH_SIZE
         int		size_I;
#endif
	 char *		source_PC;
	 int		line_I;
{
	struct DebugMem *debug_PS;

	if( !mem_P) {
#ifdef LOG
		Logf("DEBUG", "Freeing NULL pointer at %s line %d\n",
		     source_PC, line_I);
#endif
		return;
	}
	debug_PS = (struct DebugMem *)
		((char *) mem_P - sizeof( struct DebugMem ) - PRE_COOKIE_SIZE);

	DBG__CheckMem(mem_P, source_PC, line_I);

	bytes_alloc_I -= debug_PS->size_I;

	if(Verbose) {
		FREED(debug_PS->size_I, source_PC, line_I);
	}
	UnLinkMemory( debug_PS );
#ifdef FREE_WITH_SIZE
	FREE_WITH_SIZE( debug_PS, size_I );
#else
	_FREE( debug_PS );
#endif

}


/***************************************************************************
 *
 * Function   : DBG_UsedMem
 * Purpose    : Return number of allocated bytes.
 * Parameters : none
 * Returns    : Number of bytes
 *
 ***************************************************************************/

int DBG_UsedMem( )
{
	return bytes_alloc_I;
}

/*************************************************************************
 *
 * Function   : DBG__CheckMem
 * Purpose    : Check the cookies around the memory allocation for overwritten
 *              memory areas!
 * Parameters : memory pointer, source file, source line
 *
 ***************************************************************************/

long DBG__CheckMem ( mem_P, source_PC, line_I )
	 void *		mem_P;
	 char *		source_PC;
	 int		line_I;
{
	struct DebugMem *debug_PS;
	int		a, b, c;

	debug_PS = (struct DebugMem *)
		((char *) mem_P - sizeof( struct DebugMem ) - PRE_COOKIE_SIZE);

#if PRE_COOKIE_SIZE > 0
	for(a=b=0; a<PRE_COOKIE_SIZE; a++)
		if( *((unsigned char *)mem_P - PRE_COOKIE_SIZE + a ) != PRE_COOKIE_FILL_BYTE )
			b++;
	if ( b ) {
		PRE_COOKIE_ACTION(b, source_PC, line_I
						  , debug_PS->source_PC, debug_PS->line_I
						  );
	}
#endif

#if POST_COOKIE_SIZE > 0
	for(a=c=0; a<POST_COOKIE_SIZE; a++)
		if(*((unsigned char *)mem_P + debug_PS->size_I + a) != POST_COOKIE_FILL_BYTE)
			c++;
	if ( c ) {
		POST_COOKIE_ACTION(c, source_PC, line_I, debug_PS->source_PC, debug_PS->line_I );
	}
#endif

	if(Verbose) {
		CHECKMEMED(debug_PS->size_I, source_PC, line_I);
	}

	return b + c;
}

/***************************************************************************
 *
 * Function   : DBG_Strdup
 * Purpose    : Replaces the system strdup() function for debugging.
 * Parameters : string, source file, source line
 * Returns    : Allocated string or NULL if failed
 *
 **************************************************************************/

char *DBG_Strdup(char *string_PC, char *source_PC, int line_I)
{
	int len   = strlen( string_PC );
	char *ptr = DBG_malloc( len+1, source_PC, line_I );
	if(ptr)
		strcpy(ptr, string_PC);
	return ptr;
}

/*************************************************************************
 *
 * Function   : DBG_MemList
 * Purpose    : Display all allocations on stdout!
 * Parameters : none
 * Returns    : none
 *
 *************************************************************************/

void DBG_MemList()
{
	struct 	DebugMem *point = MallocKey;
	
	printf("------> Total %d bytes <------\n",
		   DBG_UsedMem());
	
	while(point) {
		printf("source: %s line: %d size: %d\n",
			   point->source_PC,
			   point->line_I,
			   point->size_I);
		point = point->prev;
	} 

	printf("------> End of table <------\n");
}

/**********************************************************************
 *
 * Function   : LinkMemory
 * Purpose    : Link a memory pointer to the linked list of memory.
 * Parameters : (struct DebugMem *) to the memory to add
 * Returns    : none
 *
 ************************************************************************/

void LinkMemory(point)
	 struct DebugMem *point;
{
	point->prev=MallocKey;	 /* previous */
	point->next=NULL;		 /* next */
	if(MallocKey)
		point->prev->next=point;
	MallocKey = (void *)point;
}

/**********************************************************************
 *
 * Function   : UnLinkMemory
 * Purpose    : Remove a memory area from the linked list.
 * Parameters : (struct DebugMem *) to the area to remove
 * Returns    : none
 *
 ************************************************************************/

void UnLinkMemory(point)
	 struct DebugMem *point;
{
	if(MallocKey==point) {
		/* if this is the last Malloc, set `last' to `prev' */
		MallocKey=point->prev;
		if(MallocKey)
			MallocKey->next=NULL;
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

/***********************************************************************
 *
 * Function   : FreeAll
 * Purpose    : Free all memory areas in the list
 * Parameters : none
 * Returns    : none
 *
 *************************************************************************/

void FreeAll()
{
	struct DebugMem *point;
	struct DebugMem *prev;

	if(!MallocKey)
		return;
		
	do {
		point = MallocKey;
		
		prev = point->prev;
		
		free(point);
	} while(MallocKey = prev);

}

