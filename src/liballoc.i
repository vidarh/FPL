 ******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************
 
 * liballoc.i
 
 * the top of the "Data struct" for assembler use
 
 ******************************************************************************/

 ************************************************************************
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
 ************************************************************************


 STRUCTURE DATA,0
    APTR	DATA_STACKBASE	; our new stack base
    ULONG	DATA_STACKSIZE	; stack size
    ULONG	DATA_STACKMAX	; Maximum stack left after a FPL function call
    ULONG	DATA_STACKLIMIT	; absolute maximum stack usage allowed
    ULONG	DATA_MINSTACK	; minimum stack required at "interface" calls

    STRUCT	DATA_REGISTERSTORAGE,11*4	; room to store registers
    APTR	DATA_TASK	; pointer to our task structure
    APTR	DATA_OLDTOP	; old stack top
    APTR	DATA_OLDBOT	; old stack bottom
    ULONG	DATA_EXT_STACK	; external stack
    ULONG	DATA_INT_STACK	; internal stack
