/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 optimize.c
 
 Description:

 Let's assume the following expression: 2 + a + 5 % 2 * 3

 First, a tree is built as follows:
					     +
					    / \ 
					   +   *
					  /|  / \
					 2 a %   3
					    / \
					   5   2

High priority operations are placed low down in the tree, the lowest
priority operation is on top.

Now the optimization can begin. It is performed in two steps. The first
step is called Reduce(). It calculates all branches with constant
expressions. After Reduce() on the above tree, it will look like this:

					     +
					    / \ 
					   +   3
					  / \
					 2   a

Now it is time for the second step. It is called MergeConstants(). It will
try to move constants together to create branches that Reduce() can
remove. It starts from the top and moves constants down by swapping them
with the underlying expressions. After MergeConstants() the tree will look
like this:

					     +
					    / \ 
					   +   a
					  / \
					 2   3

Now we start the process over again with Reduce(). After Reduce() the tree
is even smaller:

					     +
					    / \ 
					   5   a

Now we are finished. The final expression will be 5 + a.

The optimizations that are performed are:

- If two operands are constants, they are combined.
- If a left operand is 0 and the operator is '&&' then the entire
  expression is set to 0.
- If a left operand is 1 and the operator is '||' then the entire
  expression is set to 1.
- Multiplications and divisions with 1 are eliminated.

There are other optimizations that can be done. When MergeConstants sees
that it is about to swap two constants, it will combine them if they have
the same operator.


		   Interface
		--------------

The caller must provide two functions, GetOper() and PutOper().

GetOper() is called by ParseExpression() to get the expression to
parse, one operand/operator at a time. This function must return FALSE
when the expression is empty (after the last operand/operator).
The parameter 'step' is either 0 or 1 depending on if we want to step
to the next item in the expression.

PutOper() is called by Optimizetree() to put back the optimized
expression, one operand/operator at a time. The last Oper structure
returned has the type COMP_LAST. The parameter 'index' is increased
for every call.

To run the optimizer, call OptimizeExpression(). When it returns, it has
already put back the expression with PutOper(). Just check the return
code and voila!

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

#include <stdio.h>
#include <stdlib.h>

#ifdef AMIGA
#include <exec/types.h>
#else
#include <sys/types.h>
#endif

#include "script.h"
#include "comp.h"
#include "optimize.h"

struct PNode *BuildTree(void *user, struct PNode *node);

BOOL changed = FALSE;
OptErr OptError;

/****************************************************************
** GetPrio()	- Get the priority of a type
**
**************/
char GetPrio(Pass1 type)
{
   switch(type)
   {
      case COMP_DIVISION:
      case COMP_MULTIPLY:
      case COMP_REMAIN:
	 return(0);

      case COMP_PLUS:
	 return(1);

      case COMP_SHIFTLEFT:
      case COMP_SHIFTRIGHT:
	 return(2);

      case COMP_LESS:
      case COMP_LESSEQ:
      case COMP_GREATER:
      case COMP_GREATEQ:
	 return(3);

      case COMP_EQUAL:
      case COMP_NOTEQUAL:
	 return(4);

      case COMP_BINARYAND:
	 return(5);

      case COMP_XOR:
	 return(6);
	 
      case COMP_BINARYOR:
	 return(7);
	 
      case COMP_LOGICAND:
	 return(8);
	 
      case COMP_LOGICOR:
	 return(9);

      default:
	 return(100);	/* No priority (for COMP_NOTHING) */
   }
}

/*********************************************************************
** BuildDown()	- Builds a subtree of a higher priority than the
**		  current. The parameter becomes the left node in the
**		  new subtree.
**
****************/
struct PNode *BuildDown(void *user, struct PNode *left)
{
   struct PNode *right, *node;
   struct Oper oper;

   /* Create the new operator node */
   node = malloc(sizeof(struct PNode));

   /* Get the next operator from the expression */
   GetOper(user, &node->data, 1);

   /* Create the new right node */
   right = malloc(sizeof(struct PNode));

   /* Get the next operand from the expression */
   GetOper(user, &right->data, 1);

   right->left = NULL;
   right->right = NULL;

   /* Link the left and right nodes to the operator */
   node->left = left;
   node->right = right;

   /* Build further down if the next operator is of a higher priority */
   if(GetOper(user, &oper, 0))
   {
      if (GetPrio(oper.type) < GetPrio(node->data.type))
      {
	 node->right = BuildDown(user, right);
      }
   }
   
   /* Continue to build on this priority level */
   return (BuildTree(user, node));
}

/*****************************************************************
** BuildTree()	- Builds a tree from a node. It terminates when
**		  the list ends or when the next operator is of
**		  a lower priority than the starting priority.
**
************************/
struct PNode *BuildTree(void *user, struct PNode *node)
{
   struct PNode *left, *right;
   struct Oper oper;
   char prio = GetPrio(node->data.type); /* The starting priority */

   /* Do while there are more operators/operands to fetch */
   while (GetOper(user, &oper, 0))
   {
      /* If it is an operator, and its priority is higher than the
         current one, then we go down in the tree, otherwise we
	 break the loop and return, i.e go up a level */
      if (node->data.type != COMP_NOTHING)
      {
	 if (GetPrio(oper.type) < GetPrio(node->data.type))
	 {
	    node->right = BuildDown(user, node->right);
	 }
	 else if(GetPrio(oper.type) > prio)
	 {
	    break;
	 }
      }

      /* Return if we have reached the end of the expression */
      if(!GetOper(user, &oper, 0))
      {
	 break;
      }

      /* The current node is an operand and it will be the left node
         of the next operator */
      left = node;

      /* Get the next operator */
      node = malloc(sizeof(struct PNode));
      GetOper(user, &node->data, 1);

      /* Get the next operand */
      right = malloc(sizeof(struct PNode));
      GetOper(user, &right->data, 1);
      
      right->left = NULL;
      right->right = NULL;

      node->left = left;
      node->right = right;
   }

   return (node);
}

/*******************************************************************
** ParseExpression()	- The main call to build the expression tree
**
******************/
struct PNode *ParseExpression(void *user)
{
   struct PNode *left, *top;

   /* Create the first operand node */
   left = malloc(sizeof(struct PNode));
   GetOper(user, &left->data, 1);
   left->left = NULL;
   left->right = NULL;

   /* Start building the tree recursively */
   top = BuildTree(user, left);
   return top;
}

/***************************************************************
** OutputTree()	- Debugging function to output the complete
**		  expression tree.
**
******************/
void OutputTree(struct PNode *tree)
{
   if (tree->data.type == COMP_NOTHING)
   {
      if(tree->data.constant)
      {
	 printf("%d ", tree->data.num);
      }
      else
      {
	 printf("%c ", tree->data.num);
      }
      return;
   }

   OutputTree(tree->left);

   switch(tree->data.type)
   {
      case COMP_MULTIPLY:
	 printf("* ");
	 break;
      case COMP_DIVISION:
	 printf("/ ");
	 break;
      case COMP_REMAIN:
	 printf("% ");
	 break;
      case COMP_PLUS:
	 printf("+ ");
	 break;
      case COMP_SHIFTLEFT:
	 printf("<< ");
	 break;
      case COMP_SHIFTRIGHT:
	 printf(">> ");
	 break;
      case COMP_LESS:
	 printf("< ");
	 break;
      case COMP_GREATER:
	 printf("> ");
	 break;
      case COMP_LESSEQ:
	 printf("<= ");
	 break;
      case COMP_GREATEQ:
	 printf(">= ");
	 break;
      case COMP_EQUAL:
	 printf("== ");
	 break;
      case COMP_NOTEQUAL:
	 printf("!= ");
	 break;
      case COMP_BINARYAND:
	 printf("& ");
	 break;
      case COMP_XOR:
	 printf("^ ");
	 break;
      case COMP_BINARYOR:
	 printf("| ");
	 break;
      case COMP_LOGICAND:
	 printf("&& ");
	 break;
      case COMP_LOGICOR:
	 printf("|| ");
	 break;
   }

   OutputTree(tree->right);
}

/***************************************************************
** Calculate()	- This is the function that combines two nodes
**		  into one.
**		  Error codes are stored in 'OptError'
**
*****************/
long Calculate(Pass1 type, long a, long b)
{
   switch (type)
   {
      case COMP_MULTIPLY:
	 printf("%d * %d, ", a, b);
	 a *= b;
	 break;

      case COMP_DIVISION:
	 printf("%d / %d, ", a, b);
	 if(!b)
	 {
	    /* Division by zero!!! */
	    OptError = OPT_DIVISION_BY_ZERO;
	    printf("[Div by 0]");
	    return(0);
	 }
	 a /= b;
	 break;

      case COMP_REMAIN:
	 printf("%d % %d, ", a, b);
	 if(!b)
	 {
	    /* Division by zero!!! */
	    OptError = OPT_DIVISION_BY_ZERO;
	    printf("[Div by 0]");
	    return(0);
	 }
	 a %= b;
	 break;

      case COMP_PLUS:
	 printf("%d + %d, ", a, b);
	 a += b;
	 break;

      case COMP_SHIFTLEFT:
	 printf("%d << %d, ", a, b);
	 a <<= b;
	 break;

      case COMP_SHIFTRIGHT:
	 printf("%d >> %d, ", a, b);
	 a >>= b;
	 break;

      case COMP_LESS:
	 printf("%d < %d, ", a, b);
	 a = (a < b);
	 break;

      case COMP_GREATER:
	 printf("%d > %d, ", a, b);
	 a = (a > b);
	 break;

      case COMP_LESSEQ:
	 printf("%d <= %d, ", a, b);
	 a = (a <= b);
	 break;

      case COMP_GREATEQ:
	 printf("%d >= %d, ", a, b);
	 a = (a >= b);
	 break;

      case COMP_EQUAL:
	 printf("%d == %d, ", a, b);
	 a = (a == b);
	 break;

      case COMP_NOTEQUAL:
	 printf("%d != %d, ", a, b);
	 a = (a != b);
	 break;

      case COMP_BINARYAND:
	 printf("%d & %d, ", a, b);
	 a &= b;
	 break;

      case COMP_XOR:
	 printf("%d ^ %d, ", a, b);
	 a ^= b;
	 break;

      case COMP_BINARYOR:
	 printf("%d | %d, ", a, b);
	 a |= b;
	 break;

      case COMP_LOGICAND:
	 printf("%d && %d, ", a, b);
	 a = (a && b);
	 break;

      case COMP_LOGICOR:
	 printf("%d || %d, ", a, b);
	 a = (a || b);
	 break;
   }
   return(a);
}

/*******************************************************************
** Reducable()	- Tells if a certain expression is reducable. It
**		  returns a code telling which optimization that can
**		  be done.
**		  Error codes are stored in 'OptError'
**
**********************/
OptType Reducable(Pass1 type, struct Oper *left, struct Oper *right)
{
   /* If the two operands are constant, then just combine them */
   if(left->constant && right->constant)
   {
      return(OPT_TWO_CONSTANTS);
   }

   /* If the expression is a logical AND, and the left operand is FALSE,
      then we can substitute the expression with 0 */
   if(type == COMP_LOGICAND)
   {
      if(left->constant && !left->num)
      {
	 return(OPT_AND_IS_FALSE);
      }
   }

   /* If the expression is a logical OR, and the left operand is TRUE,
      then we can substitute the expression with 1 */
   if(type == COMP_LOGICOR)
   {
      if(left->constant && left->num)
      {
	 return(OPT_OR_IS_TRUE);
      }
   }

   /* If the expression is a multiplication, and one operand is 1,
      then we can eliminate the multiplication */
   if(type == COMP_MULTIPLY)
   {
      if((left->constant && left->num == 1) ||
         (right->constant && right->num == 1))
      {
	 return(OPT_MULTIPLY_WITH_1);
      }
   }

   /* If the expression is a division with 1,
      then we can eliminate the division */
   if(type == COMP_DIVISION)
   {
      if(right->constant && right->num == 1)
      {
	 return(OPT_DIVISION_WITH_1);
      }
   }

   return(OPT_IMPOSSIBLE);
}

/**********************************************************************
** FreeBranch	- Frees an entire tree recursively
**
************************/
void FreeBranch(struct PNode *top)
{
   /* First free all nodes in the left branch */
   if(top->left)
   {
      FreeBranch(top->left);
   }
   else
   {
      free(top->left);
   }

   /* Then free all nodes in the right one */
   if(top->right)
   {
      FreeBranch(top->right);
   }
   else
   {
      free(top->right);
   }
}

/*********************************************************************
** Reduce()	- Searches the tree recursively for constant expressions
**		  to combine. All branches with a left and a right node
**		  that can be reduced are combined.
**		  If any changes was made, the 'changes' variable will be
**		  non-zero.
**
***************************/
struct PNode *Reduce(struct PNode *node)
{
   struct PNode *left, *right;
   OptType opt_type;	/* Type of optimization that can be made */

   /* Return if the node is already an operand */
   if (node->data.type == COMP_NOTHING)
   {
      return (node);
   }

   /* Search the tree recursively to the left and the right */
   left = Reduce(node->left);
   right = Reduce(node->right);

   /* Find out if the branch can be combined */
   opt_type = Reducable(node->data.type, &left->data, &right->data);

   /* Perform the proper optimizations */
   switch (opt_type)
   {
      /* If both operands are constants, then just combine them */
      case OPT_TWO_CONSTANTS:
	 left->data.num = Calculate(node->data.type,
				    left->data.num,
				    right->data.num);

	 node->data = left->data;
	 free(left);
	 free(right);
	 node->left = NULL;
	 node->right = NULL;
	 node->data.type = COMP_NOTHING;
	 node->data.data = NULL;
	 node->data.constant = TRUE;
	 changed++;
	 return (node);

      /* If the operator is a logical AND, and the left operand is FALSE,
         then the result is 0 */
      case OPT_AND_IS_FALSE:
	 /* Delete the entire expression tree under this node */
	 FreeBranch(node->left);
	 FreeBranch(node->right);
	 node->left = NULL;
	 node->right = NULL;
	 node->data.type = COMP_NOTHING;
	 node->data.num = 0;
	 node->data.data = NULL;
	 node->data.constant = TRUE;
	 changed++;
	 return(node);
	 
      case OPT_OR_IS_TRUE:
	 FreeBranch(node->left);
	 FreeBranch(node->right);
	 node->left = NULL;
	 node->right = NULL;
	 node->data.type = COMP_NOTHING;
	 node->data.num = 1;
	 node->data.data = NULL;
	 node->data.constant = TRUE;
	 changed++;
	 return(node);

      case OPT_MULTIPLY_WITH_1:
	 if(node->left->data.constant && node->left->data.num == 1)
	 {
	    free(node->left);
	    right = node->right;
	    *node = *right;
	    free(right);
	 }
	 else
	 {
	    FreeBranch(node->right);
	    left = node->left;
	    *node = *left;
	    free(left);
	 }
	 changed++;
	 return(node);

      case OPT_DIVISION_WITH_1:
	 free(node->right);
	 left = node->left;
	 *node = *left;
	 free(left);
	 changed++;
	 return(node);

      /* If no optimization could be made, then just return */	 
      case OPT_IMPOSSIBLE:
	 break;
   }
   return (node);
}

/******************************************************************
** MergeConstants	- Move around constants in the tree to create
**			  reducable expressions.
**			  If any changes was made, the 'changes' variable will be
**			  non-zero.
**
**************************/
void MergeConstants(struct PNode *top)
{
   struct PNode *swap;
   BOOL merged;			/* True if a merge was made */

   while (top->left)
   {
      merged = FALSE;

      /* If the right node is an operator, we go down and try to
         merge that tree. If is is an operand and is constant, we
         swap that one with the one on the left branch, but only if
         the operators are the same. If the left branch is also a constant
         we combine them instead of swapping them. */
      if (top->right->data.type != COMP_NOTHING)
      {
	 MergeConstants(top->right);
      }
      else if (top->right->data.constant)
      {
	 if (top->left->data.type == top->data.type)
	 {
	    if (top->left->right->data.constant)
	    {
	       /* Combine the operands if they have the same operator
	          and are constants */
	       top->right->data.num = Calculate(top->data.type,
						top->left->right->data.num,
						top->right->data.num);
	       free(top->left->right);
	       swap = top->left->left;
	       free(top->left);
	       top->left = swap;
	       changed++;
	       merged = TRUE;	/* Signal that we should not step down
	       			   in the tree */
	    }
	    else
	    {
	       /* If the left operand is variable, then just swap */
	       swap = top->right;
	       top->right = top->left->right;
	       top->left->right = swap;
	       changed++;
	    }
	 }
      }
      /* Step down in the tree if we haven't combined any nodes */
      if (!merged)
	 top = top->left;
   }
}

/*****************************************************************
** OptimizeTree	- Optimize the entire expression tree. Reduce()
**		  and MergeConstants() are called until no changes
**		  are made.
**
***************************/
OptErr OptimizeTree(struct PNode *top)
{
   int pass_number = 1;

   OptError = OPT_OK;	/* Clear the error variable */
   
   do
   {
      changed = FALSE;

      printf("------------------------------------------------------------------\n");
      printf("Pass %d:\t\t", pass_number++);
      OutputTree(top);
      printf("\nReducing:\t");

      Reduce(top);
      if(OptError)	/* Break if an error has occurred */
      {
	 break;
      }
      printf("\nReduced:\t");
      OutputTree(top);
      printf("\nMerging:\t");

      MergeConstants(top);
      if(OptError)	/* Break if an error has occurred */
      {
	 break;
      }
      printf("\nMerged:\t\t");
      OutputTree(top);
      printf("\n");

   } while(changed);
   return(OptError);
}

/*****************************************************************
** ReturnExpression()	- Put back the optimized expression tree.
**
***************************/
int ReturnExpression(void *user, struct PNode *tree, BOOL first)
{
   static int x;

   if(first)
   {
      x = 0;
   }

   if (tree->data.type == COMP_NOTHING)
   {
      PutOper(user, &tree->data, x++);
      return(x);
   }

   ReturnExpression(user, tree->left, FALSE);

   PutOper(user, &tree->data, x++);

   ReturnExpression(user, tree->right, FALSE);
   return(x);
}

/*****************************************************************
** OptimizeExpression()	- Optimize an entire expression tree.
**
***************************/
OptErr OptimizeExpression(void *user)
{
   struct PNode *top;	/* The tree root */
   struct Oper oper;
   OptErr ret;
   
   top = ParseExpression(user);

   printf("Original:\t");
   OutputTree(top);
   printf("\n");

   ret = OptimizeTree(top);

   printf("------------------------------------------------------------------\n");
   printf("Optimized:\t");
   OutputTree(top);
   printf("\n");

   oper.type = COMP_LAST;
   oper.num = 0;
   oper.data = NULL;
   oper.constant = 0;
   PutOper(user, &oper, ReturnExpression(user, top, TRUE));
   return(ret);
}
