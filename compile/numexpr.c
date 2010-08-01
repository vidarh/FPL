/******************************************************************************
 *                        FREXX PROGRAMMING LANGUAGE                          *
 ******************************************************************************

 numexpr.c

 Supports *FULL* C language expression operator priority and much more...!

 *****************************************************************************/

/************************************************************************
 *                                                                      *
 * fpl.library - A shared library interpreting script langauge.         *
 * Copyright (C) 1992-1996 FrexxWare                                    *
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

#ifdef AMIGA
#include <exec/types.h>
#include <proto/exec.h>
#elif defined(UNIX)
#include <sys/types.h>
#endif

#include "script.h"
#include <stdio.h>
#include <stddef.h>
#include <limits.h>

static ReturnCode AddUnary(struct Data *, struct Expr *, Operator);
static ReturnCode Calc(struct Data *, struct Expr *, struct Expr *, char);
static ReturnCode INLINE GetArrayInfo(struct Data *, long *, long *, long, uchar *);
static ReturnCode INLINE Convert(struct Expr *, struct Data *);
static void Clean(struct Data *, struct Expr *);
static ReturnCode INLINE PrototypeInside(struct Data *,
					 struct Expr *val,
					 long,
					 struct Identifier *);
static ReturnCode INLINE inside(struct Data *, struct fplArgument *,
                                struct Identifier *);


/***********************************************************************
 *
 * int Expression(struct Expr *, struct Data *, uchar, struct Local *)
 *
 * Returns a nonzero value if any error occured.
 * The result of the Expression is returned in the Expr structure which you
 * give the pointer to in the first argument.
 *
 *****************/

ReturnCode REGARGS
Expression(struct Expr *val, /* return value struct pointer */
           struct Data *scr, /* everything */
           long control,    /* ESPECIALLLY DEFINED */
           struct Identifier *ident) /* pointer to the pointer holding
                                        the local variable names linked
                                        list */
{
  struct Expr *expr, *basexpr;
  ReturnCode ret;
  struct Identifier *pident; /* general purpose struct identifier pointer */
  struct Unary *un; /* general purpose struct Unary pointers */
  long *dims=NULL; /* dimension pointer for variable arrays! */
  uchar *text;     /* general purpose char pointer */
  uchar hit;
  uchar *array;
  long num;
  struct fplStr *string;

  GETMEM(expr, sizeof(struct Expr));
  memset(expr, 0, sizeof(struct Expr));
  basexpr=expr;

  while (1) {
#if 0     
    INFO(scr, CERROR_CMPPOS);
#endif
    if(ret=Eat(scr)) {       /* getaway blanks and comments */
      if(control&CON_END && ret==FPLERR_UNEXPECTED_END) {
        /* If there can be an unexpected ending, break out of the loop
           with a nice return code! */
        break;
      }
    }
#if 1
    else if(expr->flags&FPL_STRING && !(control&CON_GROUNDLVL))
      /* get outta string calcs if not on ground level! */
      break;
#endif
    if(!(expr->flags&FPL_OPERAND)) {  /* operand coming up */

      if(control&CON_IDENT ||
         isident(*scr->text)) {
        /*
         * It's a valid identifier character.
         */
        uchar *point;
        num=0; /* Dimension counter when taking care of array variables */

        if(control&CON_IDENT) {
          if(!(control&CON_LEVELOK) && !ident)
            ret=FPLERR_IDENTIFIER_NOT_FOUND;
          control&=~CON_IDENT; /* switch off that bit to get away from any
                                  trouble such as double using this! */
        } else {
          CALL(Getword(scr));
          ret=GetIdentifier(scr, scr->buf, &ident);
	  if(ret)
	    ident=NULL;
        }

        point=scr->text;
        Eat(scr); /* getaway blanks */

        /*
         * `ret' can only be FPL_OK or FPLERR_IDENTIFIER_NOT_FOUND at this
         * position.
         */

        if(control&CON_DECLARE && *scr->text==CHAR_OPEN_PAREN) {
	  CALL(PrototypeInside(scr, val, control, ident));
	  expr->flags|=FPL_OPERAND|FPL_ACTION;

        } else if(control&CON_DECLARE ||
                  (ident && ident->flags&FPL_VARIABLE) ||
                  (!ident && scr->text[0]!=CHAR_OPEN_PAREN)) {
          /* The ident check above really must be there, otherwise we might
             read it when it is a NULL pointer" */

          /* it's a variable */
          pident=ident;
          if(ret &&                     /* we didn't find it... */
             !(control&CON_DECLARE)) {  /* and we're not declaring! */
            /*
             * We didn't find the requested identifier and we're *NOT*
             * declaring. This means exported, name-referenced identifier!
             */
            CALL(PutArg(scr, COMP_REF_EXPORT_SYMBOL, Gethash(scr->buf) ));
            CALL(PutString(scr, COMP_NOTHING, scr->buf, -1));
          }
          else if(!ret) {
	    if((ident && ident->flags&FPL_REFERENCE) &&
               !(control&CON_LEVELOK)) {
	      INFO(scr, CERROR_ILLEGAL_REFERENCE, ident->name);
	      return FPLERR_ILLEGAL_VARIABLE; /* this is a reference _only_! */
	    }
            /* The symbol was found */
	    if(control&CON_LEVELOK) /* level _is_ OK! */
	      ;
            else if(control&CON_DECLARE &&
	       (ident->level>=scr->varlevel || scr->varlevel==1)) {
              /*
               * If the name already declared in this (or higher) level
               * and declaration is wanted.
               */
              INFO(scr, CERROR_IDENTIFIER_USED, scr->buf);
              return FPLERR_IDENTIFIER_USED;
            } else if(!(control&CON_DECLARE) &&
                      (ident->level && /* not global */
                       ident->level<(scr->varlevel-scr->level))) {
              /*
               * From the wrong program level and we're not declaring.
               */
              INFO(scr, CERROR_IDENTIFIER_NOT_FOUND, scr->buf);
              return FPLERR_IDENTIFIER_NOT_FOUND;
            }
            else if(ident->flags&FPL_STATIC_VARIABLE &&
                    ((ident->func && (ident->func==scr->func)) ||
                     ident->level>scr->varlevel)
                    ) {
              /*
               * A static variable declared either in the wrong function or
               * in a higher level!
               */
              INFO(scr, CERROR_IDENTIFIER_NOT_FOUND, scr->buf);
              return FPLERR_IDENTIFIER_NOT_FOUND;
            }
          }

          text = NULL; /* no name information yet! */

	  control &= ~CON_LEVELOK; /* forget about the level OK stuff!! */

          if(control&CON_DECLARE) {
            expr->flags|=FPL_ACTION;
            GETMEM(pident, sizeof(struct Identifier));

            pident->level=
              (control&(CON_DECLEXP|CON_DECLGLOB))?0:scr->varlevel;
            pident->flags=
              (control&CON_DECLINT?FPL_INT_VARIABLE:FPL_STRING_VARIABLE)|
                (control&CON_DECLEXP?FPL_EXPORT_SYMBOL:0)|
                  (control&CON_DECLGLOB?FPL_GLOBAL_SYMBOL:
                    (control&CON_DECLSTATIC?FPL_STATIC_VARIABLE:0))|
                    (control&CON_DECL8?FPL_CHAR_VARIABLE:
                     (control&CON_DECL16?FPL_SHORT_VARIABLE:0))|
                       (control&CON_DECLCONST?FPL_READONLY:0);

            pident->file=NULL; /* file */

            pident->func=scr->func; /* declared in this function */

            /* Get variable name */
            if(text)
              /*
               * The name has already been allocated above!
               */
              pident->name = text;
            else {
              /*
               * Get the name!
               */
              STRDUP(pident->name, scr->buf); /* no real strdup */
            }
	    pident->linenum = scr->virprg;
            /*
             * We add the symbol to the local data in all cases except when
             * the symbol is global or static.
             */
            CALL(AddVar(scr, pident,
                        control&(CON_DECLGLOB|CON_DECLSTATIC)?
                        &scr->globals:&scr->locals,
                        TRUE));

	    if(*scr->text==CHAR_OPEN_BRACKET) {
	      /*
	       * It's an array. Get the result of the expression within the
	       * square brackets.
	       */
	      
	      /* emulate declaration time if the variable wasn't found and
		 is expected to be an exported one! */
	      num = MAX_DIMS;
	      CALL(GetArrayInfo(scr, dims, &num, control|CON_DECLARE, text));
	      point=scr->text; /* move point to current location  */
	      Eat(scr); /* pass all traling whitespaces */
	    }
            if(num) {
              /*
               * Array variable declaration! It is a bit different from
               * common variable declaration.
               */
              pident->data.variable.num=num;   /* number of dimensions */

              /* reset num: */
              num=1;
            } else {
              pident->data.variable.num=0;
            }

          }
          else {
            if(pident) {
              if(!(pident->flags&FPL_EXPORT_SYMBOL)) {
                /* local, number-referenced identifier! */
                CALL(PutArg(scr, pident->flags&FPL_GLOBAL_SYMBOL?
                            COMP_REF_GLOBAL_SYMBOL:
                            COMP_REF_LOCAL_SYMBOL, pident->number));
              }
              else {
                /* exported, name-referenced identifier! */
                CALL(PutArg(scr, COMP_REF_EXPORT_SYMBOL, pident->hash));
                CALL(PutString(scr, COMP_NOTHING, pident->name, -1));
              }
            }
	    
	    if(*scr->text==CHAR_OPEN_BRACKET) {
	      /*
	       * It's an array. Get the result of the expression within the
	       * square brackets.
	       */
	      
	      /* emulate declaration time if the variable wasn't found and
		 is expected to be an exported one! */
	      if(num = (pident?pident->data.variable.num:MAX_DIMS)) {
		CALL(GetArrayInfo(scr, dims, &num, control|CON_DECLARE, text));
		point=scr->text; /* move point to current location  */
		Eat(scr); /* pass all traling whitespaces */
	      }
	    }

          }

          /*
           * Now when all declarations is done, all assigns are left:
           */

          expr->flags|=FPL_OPERAND;
          if ((!pident && control&CON_STRING) ||
              (pident && pident->flags&FPL_STRING_VARIABLE)) {

            /*
             * String variable
             */

            if(*scr->text==CHAR_OPEN_BRACKET) { /* just one character */
              /*
               * Get the result of the expression.
               */
              ++scr->text;
              CALL(Put(scr, COMP_OPEN_BRACKET));
              CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
              if(*scr->text!=CHAR_CLOSE_BRACKET) {
                INFO(scr, CERROR_MISSING_BRACKET, CHAR_CLOSE_BRACKET);
              }
              else
                ++scr->text;
              CALL(Put(scr, COMP_CLOSE_BRACKET));

              CALL(Eat(scr)); /* eat white space */

              if(ASSIGN_OPERATOR) {
                uchar was=*scr->text;
                if(pident && pident->flags&FPL_READONLY) {
                  INFO(scr, CERROR_READONLY_VIOLATE, pident->name);
                  return FPLERR_READONLY_VIOLATE;
                }
                expr->flags|=FPL_ACTION;
                if(*scr->text==CHAR_ASSIGN)
                  scr->text++;
                else if(scr->text[2]==CHAR_ASSIGN)
                  scr->text+=3;
                else
                  scr->text+=2;
                /* single assign */
                CALL(PutArg(scr, COMP_ASSIGN, was));
                CALL(Put(scr, COMP_START_OF_EXPR));
                CALL(Expression(val, scr, CON_NORMAL|CON_NUM, NULL));
                CALL(Put(scr, COMP_END_OF_EXPR));
              }

              expr->val.val=0;
              CALL(NewMember(scr, &expr));
            } else if(control&CON_NUM) {
              /* NO strings allowed! */
              INFO(scr, CERROR_ILLEGAL_NUMERICAL);
              return FPLERR_UNEXPECTED_STRING_STATEMENT;
              /* be able to continue here, we must pass everything that has to
                 to with the strings in this expression */
            } else if (*scr->text==CHAR_ASSIGN || (*scr->text==CHAR_PLUS &&
                        scr->text[1]==CHAR_ASSIGN)) {
              uchar array=FALSE;
              uchar multi=FALSE;
              uchar app=(*scr->text==CHAR_PLUS);

              if(pident && pident->flags&FPL_READONLY &&
                 !(control&CON_DECLARE)) {
                INFO(scr, CERROR_READONLY_VIOLATE, pident->name);
                return FPLERR_READONLY_VIOLATE;
              }
              CALL(PutArg(scr, COMP_ASSIGN, *scr->text));
              scr->text+=1+app;
              expr->flags|=FPL_ACTION;
              if(pident && pident->data.variable.num) { /* if array member assign */
                Eat(scr);
                if(*scr->text==CHAR_OPEN_BRACE) {
                  CALL(Put(scr, COMP_OPEN_BRACE));
                  /* multi-array assign */
                  multi=TRUE;
                  ++scr->text;
                  CALL(Eat(scr));
                }
                array=TRUE;
              }

              if(!multi) {
                /* single (array) variable assign */
                CALL(Put(scr, COMP_START_OF_EXPR));
		CALL(Expression(val, scr, CON_STRING, NULL));
		CALL(StringExpr(val, scr)); /* get more strings? */
                CALL(Put(scr, COMP_END_OF_EXPR));

                if(app && !(val->flags&FPL_NOFREE) && val->val.str)
                  /* Only do this if appending! */
                  FREE(val->val.str);
              } else {
                /* multi [compound] assign! */

                do {
                  hit=TRUE;
                  while(hit) {
                    /* parse the controlling braces and commas */
                    switch(*scr->text) {
                    case CHAR_COMMA:
                      CALL(Put(scr, COMP_COMMA));
                      ++scr->text;
                      break;
                    case CHAR_CLOSE_BRACE:
                      CALL(Put(scr, COMP_CLOSE_BRACE));
                      if(--num<0) {
			INFO(scr, CERROR_ILLEGAL_ARRAY);
                        return FPLERR_ILLEGAL_ARRAY;
		      }
                      else
                        ++scr->text;
                      break;
                    case CHAR_OPEN_BRACE:
                      CALL(Put(scr, COMP_OPEN_BRACE));
                      if(++num>(pident?pident->data.variable.num:MAX_DIMS)) {
			INFO(scr, CERROR_ILLEGAL_ARRAY);
                        return FPLERR_ILLEGAL_ARRAY;
                      }
		      else
                        ++scr->text;
                      break;
                    default:
                      hit=FALSE;
                      break;
                    }
                    if(hit) {
                      CALL(Eat(scr));
                    }
                    else
                      break;
                  }

                  if(!num)
                    break;

                  /* assign! */

                  CALL(Put(scr, COMP_START_OF_EXPR));
		  CALL(Expression(val, scr, CON_STRING, NULL));
		  CALL(StringExpr(val, scr)); /* get more strings? */
                  CALL(Put(scr, COMP_END_OF_EXPR));

                  if(app && !(val->flags&FPL_NOFREE) && val->val.str) {
                    /* only if we're appending! */
                    FREE(val->val.str);
                  }

                 /* while  */
                } while(1);
              }
              expr->val.str=NULL;
              expr->flags|=FPL_STRING|FPL_NOFREE;
            } else {
              expr->val.str=NULL;
              expr->flags|=FPL_STRING|FPL_NOFREE;
	      CALL(StringExpr(expr, scr));
            }
          } else {
            /*
             * Integer variable...
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
              INFO(scr, CERROR_ILLEGAL_NUMERICAL);
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            }
            if(!expr->operator && !expr->unary &&
               ASSIGN_OPERATOR) {

              /* integer assign */

              uchar array=FALSE;    /* is it an array variable */
              uchar multi=FALSE;    /* mutiple variable */
              uchar was=*scr->text;

              if(pident && pident->flags&FPL_READONLY &&
                 !(control&CON_DECLARE)) {
                INFO(scr, CERROR_READONLY_VIOLATE, pident->name);
                return FPLERR_READONLY_VIOLATE;
              }
              CALL(PutArg(scr, COMP_ASSIGN, was));

              expr->flags|=FPL_ACTION;
              if(*scr->text==CHAR_ASSIGN)
                scr->text++;
              else if(scr->text[2]==CHAR_ASSIGN)
                scr->text+=3;
              else
                scr->text+=2;
              if(pident && pident->data.variable.num) { /* if array member assign */
                Eat(scr);
                if(*scr->text==CHAR_OPEN_BRACE) {
                  CALL(Put(scr, COMP_OPEN_BRACE));

                  /* multi-array assign */
                  multi=TRUE;
                  ++scr->text;
                  CALL(Eat(scr));
                }
                array=TRUE;
              }

              if(!multi) {
                CALL(Put(scr, COMP_START_OF_EXPR));
                CALL(Expression(val, scr, CON_NORMAL|(pident?CON_NUM:0), NULL));
                CALL(Put(scr, COMP_END_OF_EXPR));

                expr->val.val=0;
              } else {
                /* multi [compound] assign */

                do {
                  hit=TRUE;
                  while(hit) {
                    /* parse the controlling braces and commas */
                    switch(*scr->text) {
                    case CHAR_COMMA:
                      CALL(Put(scr, COMP_COMMA));
                      ++scr->text;
                      break;
                    case CHAR_CLOSE_BRACE:
                      CALL(Put(scr, COMP_CLOSE_BRACE));
                      if(--num<0) {
			INFO(scr, CERROR_ILLEGAL_ARRAY);
                        return FPLERR_ILLEGAL_ARRAY;
                      }
		      else
                        ++scr->text;
                      break;
                    case CHAR_OPEN_BRACE:
                      CALL(Put(scr, COMP_OPEN_BRACE));
                      if(++num>(pident?pident->data.variable.num:MAX_DIMS)) {
			INFO(scr, CERROR_ILLEGAL_ARRAY);
                        return FPLERR_ILLEGAL_ARRAY;
                      }
		      else
                        ++scr->text;
                      break;
                    default:
                      hit=FALSE;
                      break;
                    }
                    if(hit) {
                      CALL(Eat(scr));
                    }
                    else
                      break;
                  }

                  if(!num)
                    break;

                  /* assign! */
                  CALL(Put(scr, COMP_START_OF_EXPR));
                  CALL(Expression(val, scr, CON_NORMAL|(pident?CON_NUM:0), NULL));
                  CALL(Put(scr, COMP_END_OF_EXPR));
                  expr->val.val=0;

                  /* while  */
                } while(1);
              }
              expr->flags|=FPL_NOFREE; /* the memory pointed to by the expr->val.val
                                          is strings of proper variables. Do
                                          not free them now! */
            } else {
              /*
               * No assignment, primary operator or none at all!
               */
              if(control&CON_DECLARE)
                expr->val.val=0;
              else {
                if(*point==CHAR_PLUS && point[1]==CHAR_PLUS) {
                  /*post increment*/
                  if(pident && pident->flags&FPL_READONLY) {
                    INFO(scr, CERROR_READONLY_VIOLATE, pident->name);
                    return FPLERR_READONLY_VIOLATE;                  
                  }
                  expr->flags|=FPL_ACTION;
                  expr->val.val=0;
                  scr->text+=2;
                  CALL(Put(scr, COMP_POSTINC));
                } else if(*point==CHAR_MINUS && point[1]==CHAR_MINUS) {
                  /* post decrement */
                  if(pident && pident->flags&FPL_READONLY) {
                    INFO(scr, CERROR_READONLY_VIOLATE, pident->name);
                    return FPLERR_READONLY_VIOLATE;                  
                  }

                  expr->flags|=FPL_ACTION;
                  expr->val.val=0;
                  scr->text+=2;
                  CALL(Put(scr, COMP_POSTDEC));
                } else {
                  /* plain variable or pre operation */
                  if(un=expr->unary) {
                    if(un->unary!=OP_PREINC && un->unary!=OP_PREDEC) {
                      expr->val.val=0;
                    } else {
                      if(pident && pident->flags&FPL_READONLY) {
                        INFO(scr, CERROR_READONLY_VIOLATE, pident->name);
                        return FPLERR_READONLY_VIOLATE;
                      }
                      if(un->unary==OP_PREINC) {
                        expr->val.val=0;
                      }
                      else {
                        expr->val.val=0;
                      }
                      expr->unary=un->next;
                      FREE(un);
                    }
                  } else
                    expr->val.val=0;
                }
              }
              CALL(NewMember(scr, &expr));
            }
          }   /* end of integer handling */
        }
        else {                     /* some sort of function */
          /*
           * FUNCTION HANDLER PART:
           */

          struct fplArgument *pass; /* struct pointer to send as argument to
                                       the function handler */
          long allocspace;

          if(ret) {
            if(*scr->text!=CHAR_OPEN_PAREN)
              /* If the following character is not an open parenthesis, fail! */
              return(ret);
          }

          num=0;    /* number of arguments */

          expr->flags|=FPL_OPERAND|FPL_ACTION; /* This sure is action...! */

          GETMEM(pass, sizeof(struct fplArgument));

          if(!ident) {
            /* The function does not exist as a declared function! */
            STRDUP(pass->name, scr->buf);
            pass->ID=FPL_UNKNOWN_FUNCTION;
            text="a>"; /* optional parameter list as argument! */
          } else {
            pass->name=ident->name;
            pass->ID=ident->data.external.ID;
            text=ident->data.inside.format;
          }
          pass->argc=0;
          pass->key=(void *)scr;

          if(!ident || FPL_OPTEXPRARG == ident->data.inside.ret) {
            /*
             * The function we invoked was not found regularly!
	     * Set return type!
	     */

	    /*
             * We try to determine whether it should return an int or a string.
             * We interpret the return value as we should do to make it pass
             * as a valid expression. That is, if the flag tells us this
             * should be a string expression to be valid, we take it as a
             * string, but if it tells us its an integer expression, we read
             * it as an integer!!!
             */

            if(control&CON_STRING)
              hit = FPL_STRARG;
            else {
              if(control&CON_NUM)
                hit = FPL_INTARG;
              else
                /*
                 * We don't know which kind of return code the function
                 * should give us!
                 */
                hit = FPL_OPTEXPRARG;
            }

	  } else {
            hit = UPPER(ident->data.inside.ret);
            if(control&CON_STRING && (hit!=FPL_STRARG)) {
              INFO(scr, CERROR_ILLEGAL_STRING);
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            }
            if(control&CON_NUM && (hit!=FPL_INTARG)) {
              INFO(scr, CERROR_ILLEGAL_NUMERICAL);
              return FPLERR_UNEXPECTED_STRING_STATEMENT;
            }
          }

          pass->ret = hit;
#if 0 /* old style local function call */
          if(ident && !(ident->flags&FPL_EXPORT_SYMBOL)) {
            /* local, number-referenced identifier! */
            CALL(PutArg(scr, COMP_CALL_LOCAL_FUNCTION, ident->number));
          }
          else
#endif
          if(ident && (ident->flags&FPL_INTERNAL_FUNCTION)) {
            /* internal function call! */
            CALL(PutArg(scr, COMP_CALL_INTERNAL_FUNCTION, ident->data.external.ID));
          }
          else {
            /* name-referenced identifier! */
            CALL(PutArg(scr, COMP_CALL_FUNCTION, Gethash(pass->name) ));
            CALL(PutString(scr, COMP_NOTHING, pass->name, -1));
          }

          if(*scr->text!=CHAR_OPEN_PAREN) {
            INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_OPEN_PAREN);
            return FPLERR_MISSING_PARENTHESES;
          }
          else
            ++scr->text;

          CALL(Eat(scr));

          CALL(Put(scr, COMP_OPEN_PAREN));

          if(text && *text) {
            uchar b='a';
            uchar a;

            /* if the function takes arguments */

            /*
             * Allocate arrays to use for data storage while parsing
             * the arguments. Maximum number of arguments is
             * MAX_ARGUMENTS.
             */

            num=strlen(text);   /* number of arguments to this function */

            if(text[num-1]!=FPL_ARGLIST)
              allocspace=num+1;
            else
              allocspace=MAX_ARGUMENTS;

            /*
             * By adjusting the number of allocated bytes to the smallest
             * necessary, my recursive example program used only a fifth
             * as much memory as when always allocating memory for
             * MAX_ARGUMENTS.
             */

            /* allocate an array */
            GETMEM(pass->argv, sizeof(uchar *)*allocspace);

            /* allocate new format string */
            GETMEM(pass->format, sizeof(uchar)*allocspace);

            /* allocate allocate-flag string */
            GETMEM(array, sizeof(uchar)*allocspace);

            while(!ret && *scr->text!=CHAR_CLOSE_PAREN && text && *text) {
              CALL(Eat(scr));
              b=(*text==FPL_ARGLIST)?b:UPPER(*text);
	      if(FPL_OPTARG == b &&
		 CHAR_AND == scr->text[0])
                a = FPL_OPTVARARG;
	      else
                a = b;

              if(pass->argc==allocspace) {
                uchar *temp;
                GETMEM(temp, sizeof(uchar *)*(allocspace+MAX_ARGUMENTS));
                memcpy(temp, pass->argv, sizeof(uchar *)*allocspace);
                FREE(pass->argv);
                pass->argv=(void **)temp;

                GETMEM(temp, sizeof(uchar)*(allocspace+MAX_ARGUMENTS));
                memcpy(temp, pass->format, sizeof(uchar)*allocspace);
                FREE(pass->format);
                pass->format=temp;

                GETMEM(temp, sizeof(uchar)*(allocspace+MAX_ARGUMENTS));
                memcpy(temp, array, sizeof(uchar)*allocspace);
                FREE(array);
                array=temp;
                
                allocspace += MAX_ARGUMENTS;
              }

              switch(a) {
	      case FPL_OPTEXPRARG:
              case FPL_OPTARG:
              case FPL_STRARG:
                CALL(Expression(val, scr, (a==FPL_STRARG?CON_STRING:0), NULL));

                if(a==FPL_STRARG || val->flags&FPL_STRING) {
		  CALL(StringExpr(val, scr)); /* get more strings? */

                  /* Enter string symbol in the created format string! */
                  pass->format[pass->argc]=FPL_STRARG;

                  if(val->val.str) {
                    /* Set this to TRUE if deallocation is wanted on this
                       string after the function call! */
                    array[pass->argc]=!(val->flags&FPL_NOFREE);
                    /*
                     * Point to the string (that is zero terminated)!
                     */
                    pass->argv[pass->argc]=val->val.str->string;
                  } else {
                    GETMEM(string, sizeof(struct fplStr));
		    memset(string, 0, sizeof(struct fplStr));
		    pass->argv[pass->argc]=string->string;
                    array[pass->argc]=1; /* allocation has been done! */
                  }
                } else {
                  pass->format[pass->argc]=a;
                  pass->argv[pass->argc]=0;
                }
                pass->argc++;
                break;
              case FPL_INTARG:
                CALL(Expression(val, scr, CON_NUM, NULL));
                pass->format[pass->argc]=FPL_INTARG;
                pass->argv[pass->argc++]=(void *)val->val.val;
                break;
	      case FPL_OPTVARARG:
              case FPL_STRVARARG:
              case FPL_INTVARARG:
	      case FPL_INTARRAYVARARG:
	      case FPL_STRARRAYVARARG:
                {
                  register ReturnCode ok;
                  if(*scr->text != CHAR_AND) {
                    ok = FPLERR_ILLEGAL_REFERENCE;
                  }
                  else {
                      CALL(Put(scr, COMP_VARIABLE_REFERENCE));
                      scr->text++;
                      ok = FPL_OK;
                  }
                  CALL(Getword(scr));
                  /* Use the `pident' pointer here, cause the `ident' pointer
                     is already being used by the function we're about to
                     invoke! */
                  GetIdentifier(scr, scr->buf, &pident);

                  if(pident && ok && !(pident->flags&FPL_REFERENCE)) {
                    /*
                     * No &-character, and the variable isn't an existing
                     * reference type!
                     */
                    INFO(scr, CERROR_ILLEGAL_REFERENCE, pident->name);
                    return FPLERR_ILLEGAL_REFERENCE; /* no reference! */
                  }
                  if(pident && !(pident->flags&FPL_EXPORT_SYMBOL)) {
                    /*
                     * This should contain many more error checks!!!
                     */

                    /* local, number-referenced identifier! */
                    CALL(PutArg(scr,  pident->flags&FPL_GLOBAL_SYMBOL?
                            COMP_REF_GLOBAL_SYMBOL:
                            COMP_REF_LOCAL_SYMBOL, pident->number));
                  }
                  else {
                    /* exported, name-referenced identifier! */
                    CALL(PutArg(scr, COMP_REF_EXPORT_SYMBOL,
                                Gethash(scr->buf) ));
                    CALL(PutString(scr, COMP_NOTHING, scr->buf, -1));
                  }
                  
                }

		if(FPL_INTARRAYVARARG == a || FPL_STRARRAYVARARG == a) {
		  if(!pident->data.variable.num) {
		    INFO(scr, CERROR_ILLEGAL_REFERENCE, pident->name);
		    return FPLERR_ILLEGAL_REFERENCE;
		  }
		}
		else if(FPL_OPTVARARG != a && pident->data.variable.num) {
		  /* only straight variables! */
		  INFO(scr, CERROR_ILLEGAL_PARAMETER);
		  return FPLERR_ILLEGAL_PARAMETER;
		}
                if( (pident->flags&FPL_INT_VARIABLE &&
		     (a==FPL_STRVARARG || a == FPL_STRARRAYVARARG)) ||
		   (pident->flags&FPL_STRING_VARIABLE &&
		    (a==FPL_INTVARARG || a == FPL_INTARRAYVARARG))) {
		  INFO(scr, CERROR_ILLEGAL_PARAMETER);
		  return FPLERR_ILLEGAL_PARAMETER;
                } else
		    pass->argv[pass->argc]=NULL;

                pass->format[pass->argc++]=
		  (pident->flags&FPL_STRING?
		 (pident->data.variable.num?FPL_STRARRAYVARARG:FPL_STRVARARG):
		   (pident->data.variable.num?FPL_INTARRAYVARARG:
		    FPL_INTVARARG));
                Eat(scr);
                break;
              default:
		INFO(scr, CERROR_ILLEGAL_PARAMETER);
		return FPLERR_ILLEGAL_PARAMETER;
              }
              if(*text!=FPL_ARGLIST)
                text++;
              if(*scr->text==CHAR_COMMA && *text) {
                scr->text++;
		CALL(Eat(scr)); /* eat white space! */
                CALL(Put(scr, COMP_COMMA));
              }
              else if(*scr->text!=CHAR_CLOSE_PAREN) {
                if(*text)
                  INFO(scr, CERROR_ILLEGAL_PARAMETER);
                else
                  INFO(scr, CERROR_EXPECTED_PAREN, CHAR_CLOSE_PAREN);
                return FPLERR_SYNTAX_ERROR;
              }
            }
            pass->format[pass->argc]=CHAR_ASCII_ZERO;
            if(text && *text && !(*text&CASE_BIT)) {
              /*
               * This is a serious mis-use. The function is called with to few
               * parameters. At least one parameter missing is a required one.
               */
	      INFO(scr, CERROR_MISSING_ARGUMENT, pass->name);
              return FPLERR_MISSING_ARGUMENT;
            }
          } else
            pass->format=NULL;

          if(*scr->text!=CHAR_CLOSE_PAREN) {
	    INFO(scr, CERROR_EXPECTED_PAREN, CHAR_CLOSE_PAREN);
            return FPLERR_TOO_MANY_PARAMETERS; /* too many parameters! */
	  }
	  ++scr->text;
          CALL(Put(scr, COMP_CLOSE_PAREN));

          switch(hit) {
            case FPL_STRARG:
              /* We got a zero length string or no string at all! */
              expr->val.str=NULL; /* no string! */
            
              expr->flags=FPL_STRING|FPL_ACTION;
	      control |= CON_STRING;
              break;
            case FPL_INTARG:
            default:
              /* only if integer! or the function is non-existent */
              expr->val.val=0;
              break;
          }

          if(!ident) {
            /*
             * The function we invoked was not found regularly!
	     * Free the name we allocated temporarily.
	     */
            FREE(pass->name); /* the name was strdup()'ed! */
	  }
          CALL(PutString(scr, COMP_TYPE_OF_ARGUMENTS, pass->format?pass->format:"", -1));

          while(pass->argc--) {
            if(pass->format[pass->argc]==FPL_STRARG && array[pass->argc]) {
              /* free the string if it's been marked to be freed!! */
              FREE((uchar *)pass->argv[pass->argc]-
                   offsetof(struct fplStr, string));
            }
          }
          if(pass->format) {
            FREE(pass->argv);
            FREE(pass->format);
            FREE(array);
          }
          FREE(pass);
#if 0
          CALL(NewMember(scr, &expr));
#endif
        }
      }
      else {

          switch(*scr->text) {
          case CHAR_AND:
            /* This is the reference-this operator! */
            CALL(Put(scr, COMP_VARIABLE_REFERENCE));
            ++scr->text; /* pass the & */
            CALL(Getword(scr));
            GetIdentifier(scr, scr->buf, &ident);

            if(ident && !(ident->flags&FPL_EXPORT_SYMBOL)) {
              /*
               * This should contain many more error checks!!!
               */

              /* local, number-referenced identifier! */
              CALL(PutArg(scr,  ident->flags&FPL_GLOBAL_SYMBOL?
                                COMP_REF_GLOBAL_SYMBOL:
                                COMP_REF_LOCAL_SYMBOL, ident->number));
            }
            else {
              /* exported, name-referenced identifier! */
              CALL(PutArg(scr, COMP_REF_EXPORT_SYMBOL,
                          Gethash(scr->buf) ));
              CALL(PutString(scr, COMP_NOTHING, scr->buf, -1));
            }
            CALL(NewMember(scr, &expr));
            break;
	  case CHAR_MULTIPLY:
	    /*
	     * This is the 'contents of' operator!
	     * The contents of the variable that follows this sign should
	     * get the following rvalue.
	     * Of course, we must first check that this really is a
	     * 'pointer' to a variable.
	     */

	    while(*++scr->text==CHAR_MULTIPLY); /* just in case! */

            CALL(Put(scr, COMP_CONTENTSOF));

	    CALL(Getword(scr));
	    if(control&CON_DECLARE) {
              INFO(scr, CERROR_NOT_YET_SUPPORTED);
	      return FPLERR_SYNTAX_ERROR; /* not yet supported */
            }
            CALL(GetIdentifier(scr, scr->buf, &ident));
	    if(ident && !(ident->flags&FPL_REFERENCE)) {
              INFO(scr, CERROR_ILLEGAL_REFERENCE, scr->buf);
	      return FPLERR_ILLEGAL_REFERENCE; /* referenced a non-reference! */
            }
	    /* we have an identifier and the level is OK! */
	    control |= CON_IDENT|CON_LEVELOK;
#if 0
            if(ident && !(ident->flags&FPL_EXPORT_SYMBOL)) {
              /* local, number-referenced identifier! */
              CALL(PutArg(scr,  ident->flags&FPL_GLOBAL_SYMBOL?
                            COMP_REF_GLOBAL_SYMBOL:
                            COMP_REF_LOCAL_SYMBOL, ident->number));
            }
            else {
              /* exported, name-referenced identifier! */
              CALL(PutArg(scr, COMP_REF_EXPORT_SYMBOL, Gethash(scr->buf)));
              CALL(PutString(scr, COMP_NOTHING, scr->buf, -1));
            }
#endif
            continue;

          case CHAR_ZERO:
            /*
             * Numbers starting with a '0' can be hex/oct/bin.
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
              INFO(scr, CERROR_ILLEGAL_STRING);
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            }
            switch(scr->text[1]) {
            case CHAR_X:
            case CHAR_UPPER_X:
              /* hexadecimal number parser */
              for(scr->text+=2; isxdigit(*scr->text); scr->text++)
                expr->val.val=expr->val.val*16+ (isdigit(*scr->text)?
                                         *scr->text-CHAR_ZERO:
                                         UPPER(*scr->text)-CHAR_UPPER_A+10);
              break;
            case CHAR_B:
            case CHAR_UPPER_B:
              /* binary number parser */
              for(scr->text+=2;*scr->text==CHAR_ZERO || *scr->text==CHAR_ONE;)
                expr->val.val=expr->val.val*2+ *scr->text++ - CHAR_ZERO;
              break;
            case CHAR_ZERO:
            case CHAR_ONE:
            case CHAR_TWO:
            case CHAR_THREE:
            case CHAR_FOUR:
            case CHAR_FIVE:
            case CHAR_SIX:
            case CHAR_SEVEN:
              /* octal number parser */
              for(scr->text++; isodigit(*scr->text);)
                expr->val.val=expr->val.val*8+ *scr->text++ - CHAR_ZERO;
              break;
            default:
              /* a single zero is simply 0 */
              scr->text++;
              expr->val.val=0;
              break;
            }
            CALL(PutArg(scr, COMP_NUM_CONSTANT, expr->val.val));
            CALL(NewMember(scr, &expr));
            break;
  	  /* end of case CHAR_ZERO: */

          case CHAR_ONE:
          case CHAR_TWO:
          case CHAR_THREE:
          case CHAR_FOUR:
          case CHAR_FIVE:
          case CHAR_SIX:
          case CHAR_SEVEN:
          case CHAR_EIGHT:
          case CHAR_NINE:
            /*
             * We hit a number between 1 and 9.
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
              INFO(scr, CERROR_ILLEGAL_STRING);
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            }
            do
              expr->val.val= expr->val.val*10 + *scr->text++ - CHAR_ZERO;
            while(isdigit(*scr->text));
            CALL(PutArg(scr, COMP_NUM_CONSTANT, expr->val.val));
            CALL(NewMember(scr, &expr));
  	  break;

  	  case CHAR_QUOTATION_MARK:
            if(control&CON_NUM) {
              /* NO integers allowed! */
              INFO(scr, CERROR_ILLEGAL_NUMERICAL);
              return FPLERR_UNEXPECTED_STRING_STATEMENT;
            }
            CALL(Convert(val, scr));
            /* This returned a string! */
            expr->val.str=val->val.str;
            expr->flags=FPL_STRING;
            CALL(PutString(scr, COMP_STRING_CONSTANT, expr->val.str->string,
                           expr->val.str->len));
	    CALL(StringExpr(expr, scr));
  	  break;

  	  case CHAR_APOSTROPHE:
            /*
             * Apostrophes surround character. Returns ASCII code.
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
              INFO(scr, CERROR_ILLEGAL_STRING);
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            }
            CALL(ReturnChar((scr->text++, scr), &expr->val.val, FALSE));
            if(*scr->text!=CHAR_APOSTROPHE)
              INFO(scr, CERROR_MISSING_SINGLEQUOTE);
            else
              ++scr->text;
            CALL(PutArg(scr, COMP_NUM_CONSTANT, expr->val.val));
            CALL(NewMember(scr, &expr));
  	  break;

  	  case CHAR_OPEN_PAREN:
            CALL(Put(scr, COMP_OPEN_PAREN));
	    ++scr->text;
            CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));
            if(*scr->text!=CHAR_CLOSE_PAREN)
              INFO(scr, CERROR_MISSING_PARENTHESIS);
            else
              ++scr->text;
            CALL(Put(scr, COMP_CLOSE_PAREN));
            expr->val.val=val->val.val;
            CALL(NewMember(scr, &expr));
            break;

  	  case CHAR_NOT_OPERATOR:
            CALL(AddUnary(scr, expr, OP_NOT));
            CALL(Put(scr, COMP_NOTOPERATOR));
            ++scr->text;
            break;

  	  case CHAR_ONCE_COMPLEMENT:
            CALL(AddUnary(scr, expr, OP_COMPL));
            CALL(Put(scr, COMP_ONCECOMPLEMENT));
            ++scr->text;
  	    break;

  	  case CHAR_PLUS:
            if(scr->text[1]==CHAR_PLUS) {
              expr->flags|=FPL_ACTION;
              scr->text+=2;
              CALL(AddUnary(scr, expr, OP_PREINC));
              CALL(Put(scr, COMP_PREINC));
            } else {
              CALL(AddUnary(scr, expr, OP_PLUS));
              scr->text++;
            }
            break;

  	  case CHAR_MINUS:
            if(scr->text[1]==CHAR_MINUS) {
              expr->flags|=FPL_ACTION;
              scr->text+=2;
              CALL(AddUnary(scr, expr, OP_PREDEC));
              CALL(Put(scr, COMP_PREDEC));
            } else {
              CALL(AddUnary(scr, expr, OP_MINUS));
              CALL(Put(scr, COMP_NEGATE));
              scr->text++;
            }
            break;

          default:
	    INFO(scr, CERROR_MISSING_OPERAND);
	    return FPLERR_MISSING_OPERAND;
	  }
	}
#if 0
        if(control&CON_STRING)
         /* get outta string calcs */
         break;
#endif

    } else {                                         /* waiting for operator */
      uchar *point=scr->text;

      switch(*scr->text) {
      case CHAR_ASSIGN:
        if(scr->text[1]==CHAR_ASSIGN) {
          expr->operator=OP_EQUAL;
          CALL(Put(scr, COMP_EQUAL));
          scr->text+=2;
        }
        break;
      case CHAR_AND:
	if(scr->text[1]==CHAR_AND) {
          /*
           * This is a logical AND (&&)
           */
          expr->operator=OP_LOGAND;
          scr->text+=2;
          CALL(Put(scr, COMP_LOGICAND));

        } else {
          expr->operator=OP_BINAND;
          scr->text++;
          CALL(Put(scr, COMP_BINARYAND));
        }
        break;
      case CHAR_OR:
        if(scr->text[1]==CHAR_OR) {
          /*
           * This is a logical OR operator (||)
           */
          expr->operator=OP_LOGOR;
          CALL(Put(scr, COMP_LOGICOR));
          scr->text+=2;

        } else {
          expr->operator=OP_BINOR;
          CALL(Put(scr, COMP_BINARYOR));
          scr->text++;
        }
        break;
      case CHAR_PLUS:
        expr->operator=OP_PLUS;
        CALL(Put(scr, COMP_PLUS));
        ++scr->text;
        break;
      case CHAR_MINUS:
        expr->operator=OP_MINUS;
        CALL(Put(scr, COMP_MINUS));
        ++scr->text;
        break;
      case CHAR_QUESTION:
        CALL(Put(scr, COMP_CONDOPSTART));
        ++scr->text;
        /*
         * This is the first operator in a conditional operator sequence (?)
         */

        /*
         * Clean the expression so far.
         */
        CALL(Calc(scr, val, basexpr, (char)(control&CON_NUM)));    /* erase the list */
	expr = basexpr;
	
        CALL(Put(scr, COMP_START_OF_EXPR));
        CALL(Expression(val, scr, CON_NORMAL, NULL));
        CALL(Put(scr, COMP_END_OF_EXPR));

        if(*scr->text!=CHAR_COLON) {
          INFO(scr, CERROR_ILLEGAL_CONDOPER);
          return FPLERR_ILLEGAL_CONDOP;
        } else
          ++scr->text;
        CALL(Put(scr, COMP_CONDOPEND));

        CALL(Put(scr, COMP_START_OF_EXPR));
        CALL(Expression(val, scr, CON_NORMAL, NULL));
        CALL(Put(scr, COMP_END_OF_EXPR));
        point=scr->text;
        break;
      case CHAR_MULTIPLY:
        expr->operator=OP_MULTIPLY;
        CALL(Put(scr, COMP_MULTIPLY));
        ++scr->text;
        break;
      case CHAR_DIVIDE:
        expr->operator=OP_DIVISION;
        CALL(Put(scr, COMP_DIVISION));
        ++scr->text;
        break;
      case CHAR_REMAIN:
        expr->operator=OP_REMAIN;
        CALL(Put(scr, COMP_REMAIN));
        ++scr->text;
        break;
      case CHAR_XOR:
        expr->operator=OP_BINXOR;
        CALL(Put(scr, COMP_XOR));
        ++scr->text;
        break;
      case CHAR_LESS_THAN:
        if(scr->text[1]==CHAR_ASSIGN) {
          CALL(Put(scr, COMP_LESSEQ));
          scr->text+=2;
          expr->operator=OP_LESSEQ;
        } else if(scr->text[1]==CHAR_LESS_THAN) {
          CALL(Put(scr, COMP_SHIFTLEFT));
          scr->text+=2;
          expr->operator=OP_SHIFTL;
        } else {
          CALL(Put(scr, COMP_LESS));
          scr->text++;
          expr->operator=OP_LESS;
        }
        break;
      case CHAR_GREATER_THAN:
	if(scr->text[1]==CHAR_ASSIGN) {
          CALL(Put(scr, COMP_GREATEQ));
          expr->operator= OP_GRETEQ;
          scr->text+=2;
        } else if(scr->text[1]==CHAR_GREATER_THAN) {
          CALL(Put(scr, COMP_SHIFTRIGHT));
          scr->text+=2;
          expr->operator=OP_SHIFTR;
        } else {
          CALL(Put(scr, COMP_GREATER));
          scr->text++;
          expr->operator=OP_GRET;
        }
        break;
      case CHAR_NOT_OPERATOR:
        if(scr->text[1]==CHAR_ASSIGN) {
          expr->operator=OP_NOTEQ;
          CALL(Put(scr, COMP_NOTEQUAL));
          scr->text+=2;
        }
        break;
      case CHAR_COMMA:
        if(control&CON_GROUNDLVL) {
          Clean(scr, basexpr);
          GETMEM(basexpr, sizeof(struct Expr));
          expr=basexpr;
          expr->val.val=0;
          expr->unary=NULL;
          expr->operator=expr->flags=OP_NOTHING;
          expr->next=NULL;
	  if(!(control&CON_DECLARE)) {
	    /* only output this when not declaring */
	    CALL(Put(scr, COMP_COMMA));
	  }
          ++scr->text;
        }
        break;
      }
      if(point==scr->text)
        break;
      expr->flags&=~FPL_OPERAND; /* clear the operand bit */
    }
  }

  if(!(control&(CON_DECLARE /* |CON_ACTION */ ))) {
    /*
     * Get result of the current expression only if this isn't called
     * as a declaring (no one wants the return code from 'int a'!)
     * or a stand-alone (they have no receiver anyway) statement.
     */
    CALL(Calc(scr, val, basexpr, (char)(control&CON_NUM)));

    /*
     * If this was a stand alone statement, including no action returns an
     * error!
     */
    if(control&CON_ACTION && !(val->flags&FPL_ACTION)) {
      INFO(scr, CERROR_INCOMPLETE_STATEMENT);
      return FPLERR_NO_ACTION;
    }
  }

  Clean(scr, basexpr);    /* erase the rest of the list */
  if(dims)
    FREE(dims);
  return(FPL_OK);
}

/**********************************************************************
 *
 * ReturnCode Calc();
 *
 * Returns the result in the first Expr struct of the expression that
 * the second parameter holds. This function does not free the expression
 * list.
 *
 *******/

static ReturnCode
Calc(struct Data *scr,
     struct Expr *val,
     struct Expr *basexpr,
     char numeric)
{
  struct Expr *expr=basexpr, *last;
  struct Unary *un, *next;

  /* first all Unary expressions */
  if(numeric) {
    while(expr) {
      if(expr->flags&FPL_STRING) {
        return FPLERR_ILLEGAL_VARIABLE;
      } else {
        un=expr->unary;
        while(un) {
          switch(un->unary) {
            case OP_PREDEC:
            case OP_PREINC:
              return FPLERR_ILLEGAL_PREOPERATION;
          }
          next=un->next;
          FREE(un);
          un=next;
        }
      }
      expr=expr->next;
    }
  }

  last=expr=basexpr;
  while(expr=expr->next) {
    last->flags|=expr->flags;
    last->next=expr->next;
    FREE(expr);
    expr=last;
  }
  val->val.val=basexpr->val.val; /* get the final value */
  val->flags=basexpr->flags; /* copy the flags */
  return(FPL_OK);
}

/**********************************************************************
 *
 * AddUnary();
 *
 * Build a linked list on the unary member of the Expr struct!
 *
 ******/

static ReturnCode
AddUnary(struct Data *scr,
         struct Expr *expr,
         Operator unary)
{
  struct Unary *next=expr->unary;

  GETMEM(expr->unary, sizeof(struct Unary));
  expr->unary->unary=unary;
  expr->unary->next=next;

  return(FPL_OK);
}


/**********************************************************************
 *
 * Clean()
 *
 * Erases every track of the linked TalStruct list...
 *
 ******/

static void Clean(struct Data *scr, struct Expr *basexpr)
{
  struct Expr *last;
  while(basexpr) {
    last=basexpr->next;
    FREE(basexpr);
    basexpr=last;
  }
}

/**********************************************************************
 *
 * Convert()
 *
 * Converts the following "string" in the line to a string which it returns.
 *
 *********/

static ReturnCode INLINE Convert(struct Expr *expr, struct Data *scr)
{
  ReturnCode ret=FPL_OK;
  long a;
  long pos=0;  /* start position */

  struct fplStr *pointer, *pek;

  expr->flags|=FPL_STRING;


  GETMEM(pointer, sizeof(struct fplStr) + ADDSTRING_DEFAULT);
  /* create default string space */

  pointer->alloc=ADDSTRING_DEFAULT;
  pointer->len=0;

  expr->val.str=pointer;

  do {
    scr->text++;
    while(*scr->text!=CHAR_QUOTATION_MARK) {
      CALL(ReturnChar(scr, &a, TRUE));
      if(a<256) {
        pointer->string[pos]=(uchar)a;
        if(++pos>=pointer->alloc) {
          GETMEM(pek, (pointer->alloc+=ADDSTRING_INC)+sizeof(struct fplStr));
          memcpy(pek, pointer, pos+sizeof(struct fplStr));
          FREE(pointer);
          pointer=pek;
          expr->val.str=pointer;
        }
      }
    }
    scr->text++;
    CALL(Eat(scr));
  } while(*scr->text==CHAR_QUOTATION_MARK);
  pointer->string[pos]=0; /* zero terminate */
  pointer->len=pos;       /* length of string */
  expr->val.str=pointer;

  return(ret);
}

#ifdef STRING_STACK
static ReturnCode INLINE StringToStack(struct Data *scr,
                                       struct fplStr **string)
{
  if(scr->stringstackptr >= scr->strings_in_stack_max) {
    FREE(scr->stringkeeper[ 0 ]); /* free the previous holder of that position! */
    scr->stringstackptr = 0;
  } else
    scr->strings_in_stack_now++;

  scr->stringstack[ current_entry ].string = *string;
  scr->stringstack[ current_entry ].text = scr->text;
  scr->stringstack[ current_entry ].prg = scr->prg;
  scr->stringstack[ current_entry ].virprg = scr->virprg;
  scr->stringstackptr++;
}

static ReturnCode INLINE StringFromStack(struct Data *scr,
                                         struct fplStr **string)
{
  const long num = scr->stringstackptr;
  const long max = scr->strings_in_stack_max;
  long count;
  for(count=0; count<scr->strings_in_stack_now; count++) {
    if(scr->stringprogram[ (num-count) >= 0 ?
                           num-count :
                           max-count] == scr->text) {
      *string = scr->stringstack[ count ].string;
      scr->text = scr->stringstack[ count ].text;
      scr->prg = scr->stringstack[ count ].prg;
      scr->virprg = scr->stringstack[ count ].virprg;
      return FPL_OK;
    }
  }
  *string=NULL;
  return FPL_OK;
}

#endif

/**********************************************************************
 *
 * GetArrayInfo()
 *
 * Read the []'s and store the information. Make sure you're standing on
 * the open bracket!
 *
 * Set the int num points to, to any number if you want to limit the number
 * of dimension reads.
 */

static ReturnCode GetArrayInfo(struct Data *scr,
                               long *dims,  /* long array */
                               long *num,   /* number of dims */
                               long control,
                               uchar *name)  /* variable name */
{
  struct Expr *val;
  ReturnCode ret=FPL_OK;
  long maxnum=*num;
  GETMEM(val, sizeof(struct Expr));
  *num=0;
  while(*scr->text==CHAR_OPEN_BRACKET) {
    CALL(Put(scr, COMP_OPEN_BRACKET));
    scr->text++; /* pass the open bracket */
    /* eval the expression: */
    CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));

    if(*scr->text!=CHAR_CLOSE_BRACKET) {
      /* no close bracket means warning */
      INFO(scr, CERROR_MISSING_BRACKET, CHAR_CLOSE_BRACKET);
    } else
      ++scr->text;
    CALL(Put(scr, COMP_CLOSE_BRACKET));

    if(++(*num)>=maxnum) {
      /* we've hit the roof! */
      break;
    } else if(*num==MAX_DIMS) {
      /* if we try to use too many dimensions... */
      INFO(scr, CERROR_TOO_LARGE_ARRAY, name);
      ret=FPLERR_ILLEGAL_ARRAY;
      break;
    }
    /*
     * Go on as long there are braces and we are declaring OR
     * as long the `num' variable tells us (you, know: when
     * you want to read character five in a member of a
     * three dimensional string array, it could look like
     * "int a=string[2][3][4][5];" ... :-)
     */
  }
  FREE(val);
  return(ret);
}

/**********************************************************************
 *
 * inside();
 *
 * This function takes care of the inside function callings within a
 * FPL program (or in a FPL program where the function was declared using
 * `export').
 *
 ******/

static ReturnCode INLINE inside(struct Data *scr,
                                struct fplArgument *arg,
                                struct Identifier *func)
{
  /*
   * The function has been declared as an `inside' one.
   */

  ReturnCode ret=FPL_OK;
  struct Identifier *pident; /* pointer to identifier */
  struct Identifier *ident;
  struct Local *locals=NULL;
  uchar count; /* parameter counter */
  uchar *text;
  struct Expr *val;
  uchar oldret;
  struct fplVariable *tempvar;
  uchar reference;
  long breaks;

  GETMEM(val, sizeof(struct Expr));

  /*
   * We know where to find this function!
   */

  scr->prg=func->data.inside.prg;
  scr->text=scr->program+func->data.inside.col;
  scr->virprg=func->data.inside.virprg;
  scr->virfile=func->data.inside.virfile;

  /**********************************
   * PARSE THE PARAMETER LIST HERE! *
   **********************************/

  CALL(Eat(scr));

  if(*scr->text!=CHAR_OPEN_PAREN) {
    INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_OPEN_PAREN);
  }
  else
    ++scr->text;

  CALL(Put(scr, COMP_OPEN_PAREN));

  if(func->data.inside.format) {
    /*
     * We won't hit this if no arguments is prototyped.
     */

    count=0; /* parameter counter */
    text=func->data.inside.format;

    if(!*text) {
      if(!Getword(scr) && strcmp(scr->buf, "void")) {
        /* it should be "void" or nothing! If it wasn't we fail! */
	INFO(scr, CERROR_ILLEGAL_PARAMETER);
        return FPLERR_ILLEGAL_DECLARE;
      }
    } else {
      while(*text && !ret) {
        CALL(Getword(scr));
        CALL(GetIdentifier(scr, scr->buf, &ident));
	CALL(Eat(scr));
        if(scr->text[0]==CHAR_MULTIPLY) {
	  reference=TRUE;
	  scr->text++; /* pass it! */
	}
	else
          reference=FALSE; /* no reference! */

        switch(*text) {
        case FPL_STRARG:
        case FPL_INTARG:
	  if(reference) {
	    /*
	     * It was said to a symbol reference!!
	     */
	    INFO(scr, CERROR_ILLEGAL_REFERENCE, ident->name);
            return FPLERR_ILLEGAL_REFERENCE;
	  }

          if((*text==FPL_STRARG &&
             ident->data.external.ID!=CMD_STRING) ||
             (*text==FPL_INTARG &&
             ident->data.external.ID!=CMD_INT)) {
	    INFO(scr, CERROR_ILLEGAL_PARAMETER);
            return FPLERR_ILLEGAL_DECLARE;
	  }
          /*
           * Declare the following word as a string or integer
           * variable.
           */
          GETMEM(pident, sizeof(struct Identifier));

          CALL(Getword(scr));

          tempvar=&pident->data.variable;

          pident->flags=(*text==FPL_INTARG?FPL_INT_VARIABLE:
                         FPL_STRING_VARIABLE)|
                           (ident->flags&FPL_VARIABLE_LESS32);

          STRDUP(pident->name, scr->buf);

          tempvar->num=0; /* This is not an array */

          /*
           * Emulate next level variable declaration by adding one
           * to the ->level member here... dirty but (fully?)
           * functional!!!! ;-)
           */

          pident->level=scr->varlevel+1;
          pident->file=NULL;
          pident->func=func;
	  pident->linenum = scr->virprg;
          CALL(AddVar(scr, pident, &locals, TRUE));
          break;
        case FPL_STRVARARG:
        case FPL_INTVARARG:
	case FPL_STRARRAYVARARG:
	case FPL_INTARRAYVARARG:
	  if(!reference) {
	    /*
	     * It was never said to be a symbol reference!!
	     */
	    INFO(scr, CERROR_ILLEGAL_REFERENCE, ident->name);
            return FPLERR_ILLEGAL_REFERENCE;
	  }
          if((*text==FPL_STRVARARG || *text == FPL_STRARRAYVARARG) &&
	     ident->data.external.ID!=CMD_STRING) {
	    INFO(scr, CERROR_ILLEGAL_PARAMETER);
	    return FPLERR_ILLEGAL_DECLARE;

          } else if((*text==FPL_INTVARARG || *text == FPL_INTARRAYVARARG) &&
		    ident->data.external.ID!=CMD_INT) {
	    INFO(scr, CERROR_ILLEGAL_PARAMETER);
            return FPLERR_ILLEGAL_DECLARE;
          }
          /*
           * Declare the following word as a variable which
           * will use the struct fplVariable pointer as given in the
           * calling parameter list.
           */

          CALL(Getword(scr));

          GETMEM(pident, sizeof(struct Identifier));

	  if(*text == FPL_INTARRAYVARARG ||
	     *text == FPL_STRARRAYVARARG) {
	      CALL(Eat(scr));
              if(CHAR_OPEN_BRACKET != scr->text[0]) {
		INFO(scr, CERROR_ILLEGAL_PARAMETER);
                return FPLERR_ILLEGAL_DECLARE;
	      }
	      if(GetEnd(scr, CHAR_CLOSE_BRACKET, CHAR_OPEN_BRACKET, FALSE)) {
		INFO(scr, CERROR_MISSING_BRACKET, CHAR_CLOSE_BRACKET);
                return FPLERR_MISSING_BRACKET;
	      }
	  }

          if(ident->data.external.ID==CMD_STRING)
            pident->flags = FPL_STRING_VARIABLE|FPL_REFERENCE;
          else
            pident->flags = FPL_INT_VARIABLE|FPL_REFERENCE;
          STRDUP(pident->name, scr->buf);

          pident->level=scr->varlevel+1;
          pident->file=NULL;
          pident->func=func;
	  pident->linenum = scr->virprg;
          CALL(AddVar(scr, pident, &locals, TRUE));
          break;
        }
        CALL(Eat(scr));

        if(*++text && *scr->text++!=CHAR_COMMA)
          /*
           * There is no way out from this error exception. Leaving a parameter
           * really is a sever thing!
           */
          return(FPLERR_MISSING_ARGUMENT);
        count++;
      }
    }

    CALL(Eat(scr));

    if(*scr->text!=CHAR_CLOSE_PAREN) {
      INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
    } else
      ++scr->text;
  }
  else {
    /*
     * No argument is useable to this function. There might be a
     * `void' keyword here, but nothing else! Just search for the
     * closing parenthesis to fasten interpreting!
     */

    if(ret=GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, FALSE)) {
      INFO(scr, CERROR_MISSING_PARENTHESIS, CHAR_CLOSE_PAREN);
      return FPLERR_MISSING_PARENTHESES;
    }
  }
  CALL(Put(scr, COMP_CLOSE_PAREN));

  /*************************
   * COMPILE THE FUNCTION! *
   *************************/

  oldret=scr->strret;
  scr->strret=func->data.inside.ret==FPL_STRARG; /* should we receive a string? */
  CALL(Eat(scr));
  if(*scr->text!=CHAR_OPEN_BRACE)
    INFO(scr, CERROR_MISSING_BRACE, CHAR_CLOSE_BRACE);
  else
    ++scr->text;
  CALL(Put(scr, COMP_OPEN_BRACE));

  text=(void *)scr->func; /* backup current */
  scr->func=func;

  breaks = scr->breaks;
  scr->breaks=0;

  ret=Script(scr, val, SCR_BRACE|SCR_FUNCTION);

  scr->breaks=breaks;

  /*
   * Delete all variables created on our list for use
   * only in the function we just came back from!
   */
  DelLocalVar(scr, &locals);

  if(!ret && val->flags & FPL_CONTINUE)
    ret = FPLERR_ILLEGAL_CONTINUE;

  if(ret)
    return ret;

  scr->func=(void *)text; /* restore last */

  FREE(val);

  scr->strret=oldret;

  return FPL_OK;
}

static ReturnCode INLINE PrototypeInside(struct Data *scr,
					 struct Expr *val,
					 long control,
					 struct Identifier *ident)
{
  struct Identifier *pident;
  long pos=0;
  ReturnCode ret = FPL_OK;
  uchar *array;
  uchar found=ident?TRUE:FALSE;

  if(!found) {
    GETMEM(pident, sizeof(struct Identifier));
    STRDUP(pident->name, scr->buf);
  }
  else {
    /* we already know about this function! */
    if(ident->flags&
       (FPL_INTERNAL_FUNCTION|FPL_KEYWORD|FPL_EXTERNAL_FUNCTION)) {
      INFO(scr, CERROR_IDENTIFIER_USED, ident->name);
      return FPLERR_IDENTIFIER_USED;
    }
    else if(!(ident->flags&FPL_INSIDE_NOTFOUND)) {
      /* already defined!!! */
      INFO(scr, CERROR_ALREADY_DEFINED, ident->name, ident->linenum);
      return FPLERR_SYNTAX_ERROR;
    }
    pident = ident;
  }

  if(!found || (found && ident->flags&FPL_INSIDE_NOTFOUND)) {
    /* we know where this is... */
    pident->data.inside.col=scr->text-scr->program;
    pident->data.inside.prg=scr->prg;
    pident->data.inside.file=NULL;
    pident->data.inside.virprg=scr->virprg;
    pident->data.inside.virfile=scr->virfile;

    pident->file=NULL;
    pident->func=scr->func; /* declared in this function */
    pident->level=control&CON_DECLGLOB?0:scr->varlevel;
  }

  if(found) {
    /*
     * We already know about this function!
     */
     CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));
  }
  else {
  
    pident->flags=FPL_INSIDE_FUNCTION|
      (control&CON_DECLEXP?FPL_EXPORT_SYMBOL:0)|
        (control&CON_DECLGLOB?FPL_GLOBAL_SYMBOL:0);
  
    scr->text++; /* pass the open parenthesis */
  
    CALL(Eat(scr));
  
    GETMEM(array, MAX_ARGUMENTS * sizeof(uchar));
  
    while(pos<MAX_ARGUMENTS) {
      if(*scr->text==CHAR_CLOSE_PAREN) {
        scr->text++;
        break;
      }
      CALL(Getword(scr));
      CALL(GetIdentifier(scr, scr->buf, &ident));
      CALL(Eat(scr));
      switch(ident->data.external.ID) {
      case CMD_VOID:
        if(*scr->text++!=CHAR_CLOSE_PAREN)
          return FPLERR_ILLEGAL_PROTOTYPE;
        break;
  
      case CMD_STRING:
      case CMD_INT:
        if(*scr->text==CHAR_MULTIPLY) {
          scr->text++;
          Getword(scr); /* eat word if there's any! */
          if(CHAR_OPEN_BRACKET == scr->text[0]) {
            if(GetEnd(scr, CHAR_CLOSE_BRACKET, CHAR_OPEN_BRACKET, FALSE))
              return FPLERR_MISSING_BRACKET;
            array[pos]=(ident->data.external.ID==CMD_STRING)?FPL_STRARRAYVARARG:
            FPL_INTARRAYVARARG;
          }
          else
            array[pos]=(ident->data.external.ID==CMD_STRING)?FPL_STRVARARG:
            FPL_INTVARARG;
        } else
          array[pos]=(ident->data.external.ID==CMD_STRING)?FPL_STRARG:
          FPL_INTARG;
        break;
  
      default:
        return FPLERR_ILLEGAL_PROTOTYPE;
      }
      if(CMD_VOID == ident->data.external.ID)
        break;
  
      pos++;
      if(isident(*scr->text)) {
        Getword(scr);
        CALL(Eat(scr));
      }
  
      if(*scr->text==CHAR_COMMA)
        scr->text++;
      else if(*scr->text!=CHAR_CLOSE_PAREN) {
        return FPLERR_ILLEGAL_PROTOTYPE;
        /* we can go on if we just forgot the closing parenthesis */
      }
    }
  
    array[pos]=0; /* terminate string */
  
    /*
     * We have all information now. AddIdentifier().
     */
  
    pident->data.inside.ret=(control&CON_DECLSTR)?FPL_STRARG:
      (control&CON_DECLINT)?FPL_INTARG:FPL_VOIDARG;
    GETMEM(pident->data.inside.format, pos+1);
    strcpy(pident->data.inside.format, array);
    FREE(array);
  }
  
  CALL(Eat(scr)); /* Eat white space */
  if(*scr->text==CHAR_OPEN_BRACE) {
    long variables;
    if(!found) {
      CALL(AddVar(scr, pident,
                  control&CON_DECLGLOB?&scr->globals:&scr->locals,
                  FALSE));
    }
    pident->linenum = scr->virprg;

    pident->flags &= ~FPL_INSIDE_NOTFOUND; /* switch off */

    CALL(PutArg(scr, COMP_FUNCTION_DECLARE, pident->flags));
    if(pident->flags&FPL_EXPORT_SYMBOL) {
      CALL(PutArg(scr, COMP_NOTHING, pident->hash));
    }
    else {
      CALL(PutArg(scr, COMP_NOTHING, pident->number));
    }
    CALL(PutString(scr, COMP_NOTHING, pident->name, -1));

    scr->text++;
    CALL(PutString(scr, COMP_PARAM_LIST, pident->data.inside.format, -1));

    variables= scr->currvariables;
    CALL(inside(scr, NULL, pident)); /* compile function */
    /* fprintf(stderr, "----------> %d\n", scr->currvariables-variables); */

    scr->text--;  /* back on brace */
    val->flags|=FPL_DEFUNCTION;

    CALL(Put(scr, COMP_END_OF_FUNCTION));

  } else {
    CALL(AddVar(scr, pident,
                control&CON_DECLGLOB?&scr->globals:&scr->locals,
                FALSE));

    val->flags&=~FPL_DEFUNCTION;
    pident->flags|=FPL_INSIDE_NOTFOUND;
  }

  return(ret);
}
