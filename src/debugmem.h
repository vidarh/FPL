/******************************************************************************
 *                        FRONTEC RAILWAY SYSTEMS AB
 * ----------------------------------------------------------------------------
 *
 * Project: FR-3000 CS
 * $Source: /home/pop/proj/rail/usr/dast/dancer/dancer/RCS/debugmem.h,v $
 * $Revision: 1.2 $
 * $Date: 1996/02/29 14:11:17 $
 * $Author: dast $
 * $State: Exp $
 * $Locker:  $
 *
 * ----------------------------------------------------------------------------
 * $Log: debugmem.h,v $
 * Revision 1.2  1996/02/29 14:11:17  dast
 * fixed some action defines
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

#ifndef AMIGA
#define _FREE (free) /* function to use when freeing memory for real */
#define _MALLOC (malloc) /* function to use when allocing memory for real */
/* #define FREE_WITH_SIZE (FreeMem) */
/* define if such a function is in use!
   Overrides the 'FREE' define */
#define LOG printf /* printf style function to call */
#else
#define _FREE (FreeMem)
#define _MALLOC (AllocMem)
#define FREE_WITH_SIZE
#endif
                                  
/* Prototype the memory functions: */
#ifdef FREE_WITH_SIZE
void 	DBG_free		( void *, int, char *, int );
#else
void 	DBG_free		( void *, char *, int );
#endif
void *	DBG_malloc		( int, char *, int );
void *	DBG_realloc		( void *, int, char *, int );
void *	DBG_calloc		( int, int, char *, int );
void 	DBG_MemList		( );
int 	DBG_UsedMem	    ( );
long 	DBG__CheckMem	( void *, char *, int );
char *  DBG_Strdup      (char *, char *, int);
int     DBG_NewFunc     ( );
void    DBG_Verbose     ( char );

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

#ifdef FREE_WITH_SIZE
/* replace FreeMem() with the new routine */
#define FreeMem(x, y) DBG_free(x, y, __FILE__, __LINE__)
#define AllocMem(x, y) DBG_malloc(x, __FILE__, __LINE__)
#endif

/* replace strdup() with the new routine */
#define strdup(x) DBG_Strdup(x, __FILE__, __LINE__)

/* store source code information on malloc */
#define SOURCE_INFO

/* number of bytes to allocate before every block */
#define PRE_COOKIE_SIZE 8

/* number of bytes to allocate after every block */
#define POST_COOKIE_SIZE 8

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

#ifdef LOG
#define PRE_COOKIE_ACTION(Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I) \
	LOG("PRE COOKIE %d bytes (free: %s/%d malloc: %s/%d)!",\
		   Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I);

#define POST_COOKIE_ACTION(Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I)\
	LOG("POST COOKIE %d bytes (free: %s/%d malloc: %s/%d)!",\
		   Number_I, FSource_PC, FLine_I, MSource_PC, MLine_I);

#define MALLOCED(size,source,lyne)\
	LOG(">>> allocated %d bytes in %s line %d\n", size, source, lyne);

#define FREED(size,source,lyne)\
	LOG("<<< freed %d bytes in %s line %d\n", size, source, lyne);

#define CHECKMEMED(size,source,lyne)\
	LOG("### checked %d bytes in %s line %d\n", size, source, lyne);
#endif /* LOG */

#endif
