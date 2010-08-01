/******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************
 
 liballoc.h
 
 Stack allocation routine prototypes.
 
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

#include <exec/libraries.h>

struct MyLibrary {
        struct             Library ml_Lib;
        ULONG              ml_SegList;
        ULONG              ml_Flags;
        APTR               ml_ExecBase; /* pointer to exec base  */
#ifndef ORIGINAL
        struct Library *   ml_DosBase;  /* pointer to dos base */
#endif
};

/* From script.h */
struct Condition;
struct Expr;
struct Data;

/*****************************************************
 *
 * InitStack(...);
 *
 * Initialize a new stack and call Script().
 * Input : a (struct Data *) should be the first input.
 * Returns: nothing, but there's a new stack pointer!
 ******/
long InitStack(register __a2 struct Data *,
			   register __a3 struct Expr *,
			   register __d2 short,
			   register __a1 struct Condition *);

/*****************************************************
 *
 * CheckStack(char **stackbase);
 *
 * Check whether to expand stack.
 * Input : struct Data *, stack limit, stack margin (minimum stack free)
 * Returns: NULL if ok, 1=out of mem, 2=Stack full.
 ******/
long __asm CheckStack(register __a3 struct Data *,
		      register __d2 long,
		      register __d3 long);

/******************************************************
 *
 * GetStackUsed(struct Data *);
 *
 * Returns the current stack size used in D0.
 *
 *******/

long __asm GetStackUsed(register __a0 struct Data *);

/******************************************************
 *
 * InterfaceCall();
 *
 * Calls a user function - with the original registers set!
 *
 ******/

long __asm InterfaceCall(register __a1 struct Data *,
		         register __a0 void *, /* function argument */
			 register __a2 long __asm (*)(register __a0 void *));

/******************************************************
 *
 * InterfaceCallNoStack();
 *
 * Calls a user function - with the original registers set!
 * Without doing anything to the stack.
 *
 ******/

long __asm InterfaceCallNoStack(register __a1 struct Data *,
		                register __a0 void *, /* function argument */
			        register __a2 long __asm (*)(register __a0 void *));

/******************************************************
 *
 * StoreRegisters();
 *
 * Store all registers as they look now to be read and
 * set each time the interface function is called!
 *
 *******/

void __asm StoreRegisters(register __a0 struct Data *);

/******************************************************
 *
 * The lock and unlock routines.
 *
 * Specify memory address and bit number of the
 * semaphore to use!
 *
 *******/

void __asm Locker(register __a0 long *, register __d0 char);

void __asm Unlocker(register __a0 long *, register __d0 char);
