/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 optimize.h

 Definitions for the expression optimizer.
 
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

/*********************************************************
** Optimization types
**
**************************/
typedef enum
{
   OPT_IMPOSSIBLE = 0,
   OPT_TWO_CONSTANTS,		/* Two constants can be combined */
   OPT_AND_IS_FALSE,		/* The left operand of '&&' is FALSE */
   OPT_OR_IS_TRUE,		/* The left operand of '||' is TRUE */
   OPT_MULTIPLY_WITH_1,		/* A multiplication with 1 */
   OPT_DIVISION_WITH_1		/* A division with 1 */
} OptType;

/*********************************************************
** Return codes from OptimizeTree()
**
************************/
typedef enum
{
   OPT_OK = 0,
   OPT_DIVISION_BY_ZERO
} OptErr;
   
/*********************************************************
** This is the operand/operator information structure
**
************************/
struct Oper
{
   Pass1 type;		/* The data type (COMP_NOTHING if an operand */
   long num;		/* The constant numerical value if an operand */
   void *data;		/* Reference to the actual data structure */
   BOOL constant;	/* TRUE if the operand is a constant */
};

/*********************************************************
** This is the tree node structure
**
************************/
struct PNode
{
   struct PNode *left;	/* Link to the left operand */
   struct PNode *right;	/* Link to the right operand */
   struct Oper data;	/* Contains the data structure (see above) */
};

/*********************************************************
** The exported functions
**
************************/
OptErr OptimizeExpression(void *user);

/*********************************************************
** The imported functions
**
************************/
BOOL GetOper(void *user, struct Oper *oper, char index);
void PutOper(void *user, struct Oper *oper, char step);
