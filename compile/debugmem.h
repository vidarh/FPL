/******************************************************************************
 *                        FRONTEC RAILWAY SYSTEMS AB
 * ----------------------------------------------------------------------------
 *
 * Project: FR-3000 CS
 * $Source: Lazer:Codingx/C/src/FPL/compile/RCS/debugmem.h,v $
 * $Revision: 1.1 $
 * $Date: 1996/01/24 19:56:40 $
 * $Author: Daniel $
 * $State: Exp $
 * $Locker:  $
 *
 * ----------------------------------------------------------------------------
 * $Log: debugmem.h,v $
 * Revision 1.1  1996/01/24  19:56:40  Daniel
 * Initial revision
 *
 * Revision 1.1  1996/01/18 12:32:11  dast
 * Initial revision
 *
 * Revision 3.6  1994/11/30  08:59:00  dast
 * Fixed the header to work
 *
 *****************************************************************************/

#ifndef DEBUGMEM_H
#define DEBUGMEM_H

/*
#define MEGANINJACHECKER
 * Define this to force checking of *ALL* memory allocations on *ALL* mallocs
 * and frees!!! This will of course be a major speed-hog... not working.
 */

/* Prototype the memory functions: */
void 	DBG_free	( void *, char *, int );
void *	DBG_malloc	( int, char *, int );
void *	DBG_realloc	( void *, int, char *, int );
void *	DBG_calloc	( int, int, char *, int );
void 	DBG_MemList	( void );
int 	DBG_UsedMem	( void );
long 	DBG__CheckMem	( void *, char *, int );
long	DBG_MemListCheck(char *file, long line);
char *  DBG_Strdup      (char *, char *, int);
void    DBG_Verbose     ( char );
void    DBG_FreeAll     ( void );


/* This function checks a memory block allocated using
   the DBG_malloc() for overwritten cookies.
   Returns TRUE if any error was found.
   NOTE: any failure in this check will invoke the same
   actions as if a regular DBG_free() failed. */
#define DBG_CheckMem(mem) DBG__CheckMem(mem, __FILE__, __LINE__)

/* replace malloc() with the new routine */
#define malloc(x) DBG_malloc(x, __FILE__, __LINE__)

/* replace calloc() with the new routine */
#define calloc(x, y) DBG_calloc(x, y, __FILE__, __LINE__)

/* replace realloc() with the new routine */
#define realloc(p, x) DBG_realloc(p, x, __FILE__, __LINE__)

/* replace free() with the new routine */
#define free(x) DBG_free(x, __FILE__, __LINE__)

/* replace strdup() with the new routine */
#define strdup(x) DBG_Strdup(x, __FILE__, __LINE__)

/* fix a nice function entry for the *ALL* memory check routine */
#define allmemcheck DBG_MemListCheck(__FILE__, __LINE__)

/* store source code information on malloc */
#define SOURCE_INFO

/* number of bytes to allocate before every block */
#define PRE_COOKIE_SIZE 64

/* number of bytes to allocate after every block */
#define POST_COOKIE_SIZE 64

/* byte to fill pre cookie with */
#define PRE_COOKIE_FILL_BYTE 0xbb

/* byte to fill post cookie with */
#define POST_COOKIE_FILL_BYTE 0xdd

/* Actions to perform when any error has been discovered in the
   cookies. The parameters to these macros are:
   
   Number_I   - Number of overwritten bytes
   FSource_PC - Source file of the free()
   FLine_I    - Line number of the free()
   MSource_PC - Source file of the malloc()
   MLine_I    - Line number of the malloc()
   */

extern char MT_LogString_AC[];

#define PRE_COOKIE_ACTION(Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I) \
	printf("ERROR! PRE COOKIE %d bytes (free: %s/%d malloc: %s/%d)!\n",\
		   Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I)

#define POST_COOKIE_ACTION(Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I)\
	printf("ERROR! POST COOKIE %d bytes (free: %s/%d malloc: %s/%d)!\n",\
		   Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I)

#define MALLOCED(size,source,lyne)\
	printf(">>> allocated %d bytes in %s line %d\n", size, source, lyne)

#define FREED(size,source,lyne)\
	printf("<<< freed %d bytes in %s line %d\n", size, source, lyne)

#define CHECKMEMED(size,source,lyne)\
	printf("### checked %d bytes in %s line %d\n", size, source, lyne)
        
#define NULLFREEACTION(source_PC,line_I)\
        printf("ALARM! Freeing NULL pointer at %s line %d\n", source_PC, line_I)
#endif
