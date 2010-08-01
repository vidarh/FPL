/************************************************************************
 *	           FREXX PROGRAMMING LANGUAGE    		        *
 ************************************************************************

 Compile.h
 
 Compile structures and defines!

 ************************************************************************/

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

/************************************************************************
LOOK OF A COMPILED PROGRAM

index   contents comment
-----   -------- -------
0       .FC      three letter string telling this is compiled
3       <byte>   8-bits flag byte, currently telling if big or little endian
4       <hunk>   each hunk goes like:
                 <4-letter name> <32-bit length> <contents>
                 "CODE" is the name of the compiled code hunk
************************************************************************/

#ifdef AMIGA
#include "/compile/pass2.h"
#else
#include "../compile/pass2.h"
#endif

#define COMPILE_COMMAND "fplc " /* and then filename, to compile! */

#define COMPILED_EXTENSION ".FPC" /* file extension for compiled programs */
#define UNCOMPILED_EXTENSION ".FPL" /* file extension for non-compiled
                                       programs */

#define COMPILED_HEADER ".FC" /* first three bytes of a compiled program */
#define COMPILED_HEADER_LEN 4 /* including the flag byte on index 3 */

#define COMPILED_HUNKNAME_LEN  4  /* each hunk name is 4 bytes long */
#define COMPILED_HUNKLENGTH_LEN  4 /* each hunk length is 4 bytes long */
#define COMPILED_HUNK_CODE "CODE" /* name of the code hunk */

ReturnCode REGARGS CmpDeclare(struct Data *scr);

