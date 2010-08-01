/******************************************************************************
 *                        FREXX PROGRAMMING LANGUAGE                          *
 ******************************************************************************

 numexpr.c

 Supports *FULL* C language expression operator priority and much more...!

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

#ifdef AMIGA
#include <exec/types.h>
#include <proto/exec.h>
#elif defined(UNIX) || defined(WIN32)
#include <sys/types.h>
#endif

#include "script.h"
#include <stdio.h>
#include <stddef.h>
#include <limits.h>

#include "debug.h"

static ReturnCode INLINE GetArrayInfo(struct Data *, long *, long *, long, uchar *);
static ReturnCode INLINE Convert(struct Expr *, struct Data *);
static ReturnCode INLINE PrototypeInside(struct Data *,
					 struct Expr *val,
					 long,
					 struct Identifier *);
static ReturnCode INLINE inside(struct Data *, struct fplArgument *,
                                struct Identifier *);

#ifdef STRING_STACK
static ReturnCode INLINE StringToStack(struct Data *,
                                       struct fplStr **);
static ReturnCode INLINE StringFromStack(struct Data *,
                                         struct fplStr **);
#endif

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
  long pos;       /* general purpose integer */
  uchar *text;     /* general purpose char pointer */
  uchar hit;
  uchar *array;
  long num;
  long *nump;     /* for general purpose long pointers */
  struct fplMsg *msg;
  struct fplStr *string;
#if defined(AMIGA) && defined(SHARED)
  if(ret=CheckStack(scr, scr->stack_limit, scr->stack_margin)) {
    if(ret==1)
      return(FPLERR_OUT_OF_MEMORY);
    else
      return(FPLERR_OUT_OF_STACK);
  }
#endif

  GETMEM(expr, sizeof(struct Expr));
  memset(expr, 0, sizeof(struct Expr));
  basexpr=expr;

  do {
    if(ret=Eat(scr)) {       /* getaway blanks and comments */
      if(control&CON_END && ret==FPLERR_UNEXPECTED_END) {
        /* If there can be an unexpected ending, break out of the loop
           with a nice return code! */
        break;
      }
    } else if(expr->flags&FPL_STRING && !(control&CON_GROUNDLVL))
      /* get outta string calcs if not on ground level! */
      break;

    if(!(expr->flags&FPL_OPERAND)) {  /* operand coming up */

      if(control&CON_IDENT || isident(*scr->text)) {
        /*
         * It's a valid identifier character.
         */
        uchar *point;
        num=0; /* Dimension counter when taking care of array variables */

        if(control&CON_IDENT) {
          if(!ident)
            ret=FPLERR_IDENTIFIER_NOT_FOUND;
          control&=~CON_IDENT; /* switch off that bit to get away from any
                                  trouble such as double using this! */
        } else {
          CALL(Getword(scr));
          ret=GetIdentifier(scr, scr->buf, &ident);
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
                  (ident && ident->flags&FPL_VARIABLE)) {
          /* The ident check above really must be there, otherwise we might
             read it when it is a NULL pointer" */

          /* it's a variable */
          pident=ident;
          if(ret &&                     /* we didn't find it... */
             !(control&CON_DECLARE))    /* and we're not declaring! */
            /*
             * We didn't find the requested identifier and we're *NOT*
             * declaring. This means error!
             */
            return(ret);
          else if(!ret) {
	    if(ident->flags&FPL_REFERENCE)
	      return FPLERR_ILLEGAL_VARIABLE; /* this is a reference _only_! */

            /* The symbol was found */
	    if(control&CON_LEVELOK) /* level _is_ OK! */
	      ;
            else if(control&CON_DECLARE &&
	       (ident->level>=scr->varlevel || scr->varlevel==1)) {
              /*
               * If the name already declared in this (or higher) level
               * and declaration is wanted.
               */
              if((ident->flags&FPL_STATIC_VARIABLE &&
                  control&CON_DECLSTATIC &&
                  ident->level==scr->varlevel) ||
                 /*
                  * If this is a `static' variable and the variable already
                  * exists on this very level in this very function as static,
                  * then skip this. It's perfectly OK to jump to the ending
                  * semicolon since this has been parsed before!
                  */

                 (ident->flags&FPL_EXPORT_SYMBOL && control&CON_DECLEXP)) {

                /*
                 * If this is an `export' symbol and it already exists as an
                 * `export' symbol! Then just ignore this!
                 */

                /*
                 * The current implementation unfortunately uses the statement
                 * below to pass this declaration. That means comma-
                 * separated exported symbols will be passed if only the first
                 * is alredy declared... This will although work in all those
                 * cases it is the SAME code that is executed twice!
                 */


                if(GetEnd(scr, CHAR_SEMICOLON, (uchar)255, FALSE))
                  return FPLERR_MISSING_SEMICOLON;
                scr->text--; /* get back on the semicolon! */
                break;
              } else {
                CALL(Warn(scr, FPLERR_IDENTIFIER_USED));
                /* run it over! */
                DelIdentifier(scr, ident->name, NULL);
              }
            } else if(!(control&CON_DECLARE) &&
                      (ident->level && /* not global */
                       ident->level<(scr->varlevel-scr->level)))
              /*
               * From the wrong program level and we're not declaring.
               */
              return(FPLERR_IDENTIFIER_NOT_FOUND);
            else if(ident->flags&FPL_STATIC_VARIABLE &&
                    ((ident->func && (ident->func==scr->func)) ||
                     ident->level>scr->varlevel)
                    )
              /*
               * A static variable declared either in the wrong function or
               * in a higher level!
               */
              return(FPLERR_IDENTIFIER_NOT_FOUND);
          }

          text = NULL; /* no name information yet! */

	  control &= ~CON_LEVELOK; /* forget about the level OK stuff!! */

          if(*scr->text==CHAR_OPEN_BRACKET) {
            /*
             * It's an array. Get the result of the expression within the
             * square brackets.
             */

            if(!dims) {
              GETMEM(dims, MAX_DIMS*sizeof(long));
            }
            if(!(control&CON_DECLARE) && pident->data.variable.size)
              num=pident->data.variable.num;
            if(control&CON_DECLARE || num) {
              /*
               * Get the name now, cause the GetArrayInfo() call may
               * destroy the 'scr->buf' buffer!
               */
              STRDUP(text, scr->buf);

              GETMEM(nump, sizeof(long));
              *nump = num;
              CALL(GetArrayInfo(scr, dims, nump, control, text));
              num = *nump;
              FREE(nump);
              if(!(control&CON_DECLARE)) {
                /*
                 * Free the name now, cause we don't declare anything
                 * and this isn't needed any more!
                 */
                FREE(text);
                text = NULL;
              }
              if(!(control&CON_DECLARE)) {
                if(num > pident->data.variable.num) {
                  /*
                   * If not declaring and overfilled quota: fail!
                   *
                   *
                   * Copy the variable name to the buffer to make the
                   * error message look good!
                   */
                  strcpy(scr->buf, pident->name);
                  return FPLERR_ILLEGAL_ARRAY;
                  
                } else {
                  for(pos=0; pos<num; pos++)
                    if(pident->data.variable.dims[pos]<=dims[pos]) {
                      /*
                       * Copy the variable name to the buffer to make the
                       * error message look good!
                       */
                      strcpy(scr->buf, pident->name);
                      return FPLERR_ILLEGAL_ARRAY;
                    }
                }
              }
              point=scr->text; /* move point to current location  */
              Eat(scr); /* pass all traling whitespaces */
            }
          }
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

            pident->file=scr->prog->name; /* file */

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
            if(num) {
              /*
               * Array variable declaration! It is a bit different from
               * common variable declaration so I decided to put the code
               * for it right here:
               */
              long size=dims[0]; /* array size */

              for(pos=1; pos<num; pos++)
                size*=dims[pos];

              /* Now `size' is the total number of members in the array we're
                 about to declare */

              /* Get memory for the dimension array */
              GETMEM(pident->data.variable.dims, num * sizeof(long));

              /* Copy the dim info to the newly allocated area */
              memcpy((void *)pident->data.variable.dims, dims, num*sizeof(long));

              /* Get memory for the array  */
              GETMEM(pident->data.variable.var.val32, size * sizeof(long));

              /* Set all string lengths to NULL and integers to zero: */
              memset(pident->data.variable.var.val32, 0, size * sizeof(void *));

              pident->data.variable.size=size; /* total number of array members */
              pident->data.variable.num=num;   /* number of dimensions */

              /* reset the dims array! */
              memset((void *)dims, 0, sizeof(long) * num);

              /* reset num: */
              num=1;

            } else {
#ifdef DEBUG
              CheckMem(scr, pident);
#endif

              GETMEM(pident->data.variable.var.val32, sizeof(long));
              *pident->data.variable.var.val32=0;
              pident->data.variable.num=0;
              pident->data.variable.size=1;
            }
            /*
             * We add the symbol to the local data in all cases except when
             * the symbol is global or static.
             */
            CALL(AddVar(scr, pident,
                        control&(CON_DECLGLOB|CON_DECLSTATIC)?
                        &scr->globals:&scr->locals));
          }

          /*
           * Now when all declarations is done, all assigns are left:
           */

          expr->flags|=FPL_OPERAND;
          if (pident->flags&FPL_STRING_VARIABLE) { /* string variable */
            if(*scr->text==CHAR_OPEN_BRACKET) { /* just one character */
              /*
               * Get the result of the expression.
               */
              uchar *value;
              if(control&CON_STRING) {
                /* NO integers allowed! */
                return FPLERR_UNEXPECTED_INT_STATEMENT;
              }
              CALL(Expression(val, (scr->text++, scr),
                              CON_GROUNDLVL|CON_NUM, NULL));
              if(val->val.val<0) {
                strcpy(scr->buf, pident->name);
                return FPLERR_STRING_INDEX; /* we don't know what was meant! */
              }

              if(*scr->text!=CHAR_CLOSE_BRACKET) {
                CALL(Warn(scr, FPLERR_MISSING_BRACKET));
                /* we can continue anyway! */
              } else
                scr->text++;

              CALL(Eat(scr)); /* eat white space */

              if(pident->data.variable.num) {
                /* pick out the proper array member */
                pos=ArrayNum(num, pident->data.variable.num,
                             dims, pident->data.variable.dims);
                if(pos<0) {
                  strcpy(scr->buf, pident->name);
                  return FPLERR_ILLEGAL_ARRAY; /* we don't know what was meant! */
                }
              } else
                pos=0;

              if(!pident->data.variable.var.str[pos] ||
                 !pident->data.variable.var.str[pos]->len)
                /* no-length-string */
                return FPLERR_STRING_INDEX;
              
              if(val->val.val >= pident->data.variable.var.str[pos]->len) {
                /* force to zero! */
                val->val.val=0;
              }
              /*
               * (I) Here we should be able to operate the character read
               * from the string. ++ and -- should work to enable advanced
               * string modification handling without the
               * overhead of getting the string, changing it and then re-
               * assign it back. This *MUST* be implemented soon cause
               * it's a real killer!
               */

              value=(uchar *)&pident->data.variable.var.str[pos]->string[val->val.val];

              if(ASSIGN_OPERATOR) {
                uchar was=*scr->text;
                long valint=*value;
                if(pident->flags&FPL_READONLY)
                  return FPLERR_READONLY_VIOLATE;                  
                expr->flags|=FPL_ACTION;
                if(*scr->text==CHAR_ASSIGN)
                  scr->text++;
                else if(scr->text[2]==CHAR_ASSIGN)
                  scr->text+=3;
                else
                  scr->text+=2;
                /* single assign */
                CALL(Expression(val, scr, CON_NORMAL|CON_NUM, NULL));
                CALL(CmpAssign(scr, val->val.val, &valint, FPL_CHAR_VARIABLE,
                               was));
                *value=(uchar)valint;
              }

              expr->val.val=*value; /* only one byte */
              CALL(NewMember(scr, &expr));
            } else if(control&CON_NUM) {
              /* NO strings allowed! */
              return FPLERR_UNEXPECTED_STRING_STATEMENT;
              /* be able to continue here, we must pass everything that has to
                 to with the strings in this expression */
            } else if (*scr->text==CHAR_ASSIGN || (*scr->text==CHAR_PLUS &&
                        scr->text[1]==CHAR_ASSIGN)) {
              uchar array=FALSE;
              uchar multi=FALSE;
              struct fplStr **string; /* current string */
              uchar app=(*scr->text==CHAR_PLUS);

              if(pident->flags&FPL_READONLY && !(control&CON_DECLARE))
                return FPLERR_READONLY_VIOLATE;

              scr->text+=1+app;
              expr->flags|=FPL_ACTION;
              if(pident->data.variable.num) { /* if array member assign */
                Eat(scr);
                if(*scr->text==CHAR_OPEN_BRACE) {
                  /* array assign */
                  multi=TRUE;
                  scr->text++;
                  CALL(Eat(scr));
                }
                array=TRUE;
              }

              if(!multi) {
                /* single (array) variable assign */
                if(array) {
                  pos=ArrayNum(num, pident->data.variable.num,
                               dims, pident->data.variable.dims);
                  if(pos<0) {
                    CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                    pos=0; /* we don't know what was meant! */
                  }
                } else
                  pos=0;
		CALL(Expression(val, scr, CON_STRING, NULL));
		CALL(StringExpr(val, scr)); /* get more strings? */
                string=&pident->data.variable.var.str[pos];
                if(!app && val->flags&FPL_NOFREE) {
                  /*
                   * Only do this this is not an append action _and_
                   * we can't free this string (== someone else is
                   * taking care of this string!)
                   */
                  if(*string) {
                    FREE_KIND(*string); /* free old string */
                  }
                  if(val->val.str) {
                    /* duplicate string */
                    STRFPLDUP((*string), val->val.str);
                  }
                  else
                    *string=NULL;
                } else {
                  CALL(StrAssign(val->val.str, scr, string, app));
                }
                if(*string && MALLOC_STATIC == TypeMem(pident) )
                  SwapMem(scr, *string, MALLOC_STATIC);
                if(app && !(val->flags&FPL_NOFREE) && val->val.str)
                  /* Only do this if appending! */
                  FREE(val->val.str);
              } else {
                /* multi [compound] assign! */

                /*
                 * Count the preceding open braces to get proper level
                 * to assign in.
                 */
                while(*scr->text==CHAR_OPEN_BRACE) {
                  num++; /* next dimension */
                  scr->text++; /* pass it! */
                  CALL(Eat(scr));
                }

                do {
                  do {
                    hit=TRUE;

                    /* parse the controlling braces and commas */
                    switch(*scr->text) {
                    case CHAR_CLOSE_BRACE:

                      num--; /* back one dimension */
                      if(num>=0 && num<pident->data.variable.num)
                        dims[num]=0;
                      else {
                        CALL(Warn(scr,FPLERR_ILLEGAL_ARRAY));
                        num=0; /* force counter to zero! */
                      }
                      scr->text++;
                      break;
                    case CHAR_COMMA:
                      /*
                       * Increase the last dimension member for next loop:
                       */

                      if(num>0 && num<=pident->data.variable.num)
                        dims[num-1]++;
                      else {
                        CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                        /* force counter back to top position! */
                        num=pident->data.variable.num;
                      } scr->text++;
                      break;
                    case CHAR_OPEN_BRACE:
                      num++; /* next dimension */
                      scr->text++;
                      break;
                    default:
                      hit=FALSE;
                      break;
                    }
                    if(hit && !ret) {
                      CALL(Eat(scr));
                    } else
                      break;
                  } while(1);


                  if(!num)
                    break;

                  pos=ArrayNum(num, pident->data.variable.num,
                               dims, pident->data.variable.dims);
                  if(pos<0) {
                    CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                    pos=0; /* force back to sane number */
                  }

                  /* assign! */

                  string=&pident->data.variable.var.str[pos];

		  CALL(Expression(val, scr, CON_STRING, NULL));
		  CALL(StringExpr(val, scr)); /* get more strings? */

                  if(!app && val->flags&FPL_NOFREE) {
                    /*
                     * Only do this this is not an append action _and_
                     * we can't free this string (== someone else is
                     * taking care of this string!)
                     */
                    if(*string) {
                      FREE_KIND(*string); /* free old string */
                    }
                    if(val->val.str) {
                      STRFPLDUP((*string), val->val.str); /* duplicate string */
                    }
                    else
                      *string = NULL;
                  } else {
                    CALL(StrAssign(val->val.str, scr, string, app));
                  }
                  if(*string && MALLOC_STATIC == TypeMem(pident))
                    SwapMem(scr, *string, MALLOC_STATIC);

                  if(app && !(val->flags&FPL_NOFREE) && val->val.str) {
                    /* only if we're appending! */
                    FREE(val->val.str);
                  }

#ifdef STRING_STACK
                  if(app)
                    /* the string couldn't be freed, but we let them know that
                       we don't use it anymore! */
                    val->val.str->flags=FPLSTR_UNUSED;
#endif
                  /* while  */
                } while(1);
              }
              expr->val.str=*string;
              expr->flags|=FPL_STRING|FPL_NOFREE;
            } else {
              if(control&CON_DECLARE)
                expr->val.val=0;
              else if(pident->data.variable.num) {
                pos=ArrayNum(num, pident->data.variable.num,
                             dims, pident->data.variable.dims);
                if(pos<0) {
                  CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                  pos=0; /* force back to sane number */
                }
                expr->val.str=pident->data.variable.var.str[pos];
              } else
                expr->val.str=pident->data.variable.var.str[0];
              expr->flags|=FPL_STRING|FPL_NOFREE;
	      CALL(StringExpr(expr, scr));
            }
          } else {
            /*
             * Integer variable...
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            }
#if 0
            if(pident->flags&FPL_READONLY && !(control&CON_DECLARE)) {
              if(!pident->data.variable.num)
                expr->val.val=pident->data.variable.var.val32[0];
              else {
                pos=ArrayNum(num, pident->data.variable.num,
                             dims, pident->data.variable.dims);
                if(pos<0) {
                  CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                  pos=0; /* force back to sane number */
                }

                expr->val.val=pident->data.variable.var.val32[pos];
              }
            } else
#endif
              if(!expr->operator && !expr->unary &&
                      ASSIGN_OPERATOR) {

              /* integer assign */

              uchar array=FALSE;    /* is it an array variable */
              uchar multi=FALSE;    /* mutiple variable */
              uchar was=*scr->text;

              if(pident->flags&FPL_READONLY && !(control&CON_DECLARE))
                return FPLERR_READONLY_VIOLATE;

              expr->flags|=FPL_ACTION;
              if(*scr->text==CHAR_ASSIGN)
                scr->text++;
              else if(scr->text[2]==CHAR_ASSIGN)
                scr->text+=3;
              else
                scr->text+=2;
              if(pident->data.variable.num) { /* if array member assign */
                Eat(scr);
                if(*scr->text==CHAR_OPEN_BRACE) {

                  /* array assign */
                  multi=TRUE;
                  scr->text++;
                  CALL(Eat(scr));
                }
                array=TRUE;
              }

              if(!multi) {
                if(!array)
                  pos=0;
                else {
                  /* single (array) variable assign */
                  pos=ArrayNum(num, pident->data.variable.num,
                               dims, pident->data.variable.dims);
                  if(pos<0) {
                    CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                    pos=0; /* force back to a decent number */
                  }
                }

                CALL(Expression(val, scr, CON_NORMAL|CON_NUM, NULL));

                CALL(CmpAssign(scr, val->val.val,
                               &pident->data.variable.var.val32[pos],
                               pident->flags, was));
                expr->val.val=pident->data.variable.var.val32[pos];
              } else {
                /* multi [compound] assign */

                /*
                 * Count the preceding open braces to get proper level
                 * to assign in.
                 */
                while(*scr->text==CHAR_OPEN_BRACE) {
                  num++; /* next dimension */
                  scr->text++; /* pass it! */
                  CALL(Eat(scr));
                }

                do {
                  while(1) {
                    uchar hit=TRUE;

                    /* parse the controlling braces and commas */
                    switch(*scr->text) {
                    case CHAR_CLOSE_BRACE:

                      num--; /* back one dimension */
                      if(num>=0 && num<pident->data.variable.num)
                        dims[num]=0;
                      else {
                        CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                        num=0;
                      }
                      scr->text++;
                      break;
                    case CHAR_COMMA:
                      /*
                       * Increase the last dimension member for next loop:
                       */
                      if(num>0 && num<=pident->data.variable.num)
                        dims[num-1]++;
                      else {
                        CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                        num=pident->data.variable.num;
                      }
                      scr->text++;
                      break;
                    case CHAR_OPEN_BRACE:
                      num++; /* next dimension */
                      scr->text++;
                      break;
                    default:
                      hit=FALSE;
                      break;
                    }
                    if(hit && !ret) {
                      CALL(Eat(scr));
                    } else
                      break;
                  }

                  if(!num)
                    break;

                  pos=ArrayNum(num, pident->data.variable.num,
                               dims, pident->data.variable.dims);
                  if(pos<0) {
                    CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                    pos=0;
                  }

                  /* assign! */
                  CALL(Expression(val, scr, CON_NORMAL|CON_NUM, NULL));
                  CALL(CmpAssign(scr, val->val.val, &pident->data.variable.var.val32[pos],
                                 pident->flags, was));
                  expr->val.val=pident->data.variable.var.val32[pos];

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
              long *value;
              if(control&CON_DECLARE)
                expr->val.val=0;
              else {
                if(pident->data.variable.num) {
                  pos=ArrayNum(num, pident->data.variable.num,
                               dims, pident->data.variable.dims);
                  if(pos<0) {
                    CALL(Warn(scr, FPLERR_ILLEGAL_ARRAY));
                    pos=0;
                  }
                } else
                  pos=0;
                value=&pident->data.variable.var.val32[pos];

                if(*point==CHAR_PLUS && point[1]==CHAR_PLUS) {
                  /*post increment*/
                  if(pident->flags&FPL_READONLY)
                    return FPLERR_READONLY_VIOLATE;                  
                  expr->flags|=FPL_ACTION;
                  expr->val.val=(*value)++;
                  scr->text+=2;
                } else if(*point==CHAR_MINUS && point[1]==CHAR_MINUS) {
                  /* post decrement */
                  if(pident->flags&FPL_READONLY)
                    return FPLERR_READONLY_VIOLATE;                  

                  expr->flags|=FPL_ACTION;
                  expr->val.val=(*value)--;
                  scr->text+=2;
                } else {
                  /* plain variable or pre operation */
                  if(un=expr->unary) {
                    if(un->unary!=OP_PREINC && un->unary!=OP_PREDEC) {
                      expr->val.val=*value;
                    } else {
                      if(pident->flags&FPL_READONLY)
                        return FPLERR_READONLY_VIOLATE;
                      if(un->unary==OP_PREINC)
                        expr->val.val=++(*value);
                      else
                        expr->val.val=--(*value);
                      expr->unary=un->next;
                      FREE(un);
                    }
                  } else
                    expr->val.val=*value;
                }
                if(pident->flags&FPL_VARIABLE_LESS32) {
                  if(pident->flags&FPL_CHAR_VARIABLE) {
                    expr->val.val=(long)((signed char)expr->val.val);
                    *value=(long)((signed char)*value);
                  } else {
                    /* sixteen bits */
                    expr->val.val=(long)((signed short)expr->val.val);
                    *value=(long)((signed short)*value);
                  }
                }
              }
              CALL(NewMember(scr, &expr));
            }
          }   /* end of integer handling */
        } else if(ret && (*scr->text!=CHAR_OPEN_PAREN))
          return(ret); /* FPLERR_IDENTIFIER_NOT_FOUND */
        else {                     /* some sort of function */
          /*
           * FUNCTION HANDLER PART:
           */

          struct fplArgument *pass; /* struct pointer to send as argument to
                                       the function handler */
          long allocspace;

          if(ret) {
            if(!(scr->flags&FPLDATA_ALLFUNCTIONS) ||
               *scr->text!=CHAR_OPEN_PAREN)
              /* If the ability to parse all functions isn't turned on, or if
                 the following character is not an open parenthesis, fail! */
              return(ret);
          }

          num=0;    /* number of arguments */

          expr->flags|=FPL_OPERAND|FPL_ACTION; /* This sure is action...! */

          GETMEM(pass, sizeof(struct fplArgument));

          if(!ident) {
            /* The function does not exist as a declared function! */
            STRDUP(pass->name, scr->buf);
            pass->ID=FPL_UNKNOWN_FUNCTION;
            text="o>"; /* optional parameter list as argument! */
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
            if(control&CON_STRING && (hit!=FPL_STRARG))
              return FPLERR_UNEXPECTED_INT_STATEMENT;
            if(control&CON_NUM && (hit!=FPL_INTARG))
              return FPLERR_UNEXPECTED_STRING_STATEMENT;
          }

          pass->ret = hit;

          if(*scr->text!=CHAR_OPEN_PAREN) {
            CALL(Warn(scr, FPLERR_MISSING_PARENTHESES));  /* >warning< */
          } else
            scr->text++;

          CALL(Eat(scr));

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
                  pass->format[pass->argc]=FPL_INTARG;
                  pass->argv[pass->argc]=(void *)val->val.val;
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
                      scr->text++;
                      ok = FPL_OK;
                  }
                  CALL(Getword(scr));
                  /* Use the `pident' pointer here, cause the `ident' pointer
                     is already being used by the function we're about to
                     invoke! */
                  CALL(GetIdentifier(scr, scr->buf, &pident));

                  if(ok) {
                      /* missing &-character! */
                      if(pident->flags&FPL_REFERENCE)
                        /* get the referenced variable instead! */
                        pident = pident->data.variable.ref;
                      else
                  	return FPLERR_ILLEGAL_REFERENCE; /* no reference! */
                  }
                }

		if(FPL_INTARRAYVARARG == a || FPL_STRARRAYVARARG == a) {
		    if(!pident->data.variable.num)
			return FPLERR_ILLEGAL_REFERENCE;
		}
		else if(FPL_OPTVARARG != a && pident->data.variable.num)
		    /* only straight variables! */
		    return FPLERR_ILLEGAL_PARAMETER;

                if( (pident->flags&FPL_INT_VARIABLE &&
		     (a==FPL_STRVARARG || a == FPL_STRARRAYVARARG)) ||
		   (pident->flags&FPL_STRING_VARIABLE &&
		    (a==FPL_INTVARARG || a == FPL_INTARRAYVARARG))) {
		    CALL(Warn(scr, FPLERR_ILLEGAL_VARIABLE));
		    /* can't copy wrong variable! */
		    pass->argv[pass->argc]=NULL;
                } else
		    pass->argv[pass->argc]=(void *)pident;

                pass->format[pass->argc++]=
		  (pident->flags&FPL_STRING?
		 (pident->data.variable.num?FPL_STRARRAYVARARG:FPL_STRVARARG):
		   (pident->data.variable.num?FPL_INTARRAYVARARG:
		    FPL_INTVARARG));
                Eat(scr);
                break;
              default:
                CALL(Warn(scr, FPLERR_ILLEGAL_PARAMETER));
                break; /* just ignore it and be happy! */
              }
              if(*text!=FPL_ARGLIST)
                text++;
              if(*scr->text==CHAR_COMMA) {
                scr->text++;
		CALL(Eat(scr)); /* eat white space! */

              }
            }
            pass->format[pass->argc]=CHAR_ASCII_ZERO;
            if(text && *text && !(*text&CASE_BIT)) {
              return FPLERR_MISSING_ARGUMENT;
              /*
               * This is a serious mis-use. The function is called with to few
               * parameters. At least one parameter missing is a required one.
               * I really can't figure out a way to survive such a shock!
               */
            }
          } else
            pass->format=NULL;
          if(*scr->text!=CHAR_CLOSE_PAREN) {
            CALL(Warn(scr, FPLERR_TOO_MANY_PARAMETERS)); /* too many parameters! */
            /* It's ok to continue without the parenthesis! */
          } else
            scr->text++;

          /*
           * Call the function!
           */

          CALL(CallFunction(scr, pass, ident));

#if 0
	  fprintf(stderr, "**Return\n");
#endif
          CALL(GetMessage(scr, FPLMSG_RETURN, &msg));
          if(FPL_OPTEXPRARG == hit) {
            if(msg) {
              if(msg->flags&FPLMSG_FLG_INT) {
                /* There is a return 'int' message! This may well be a
                   function returning int! */
                hit = FPL_INTARG;
              }
              else {
                /* found string, it returned a 'string' !!! */
                hit = FPL_STRARG;
              }
            }
            /* There is no return nor hint! */
          }

          switch(hit) {
            case FPL_STRARG:
#if 0
	      fprintf(stderr, "**String from %s\n", pass->name);
	      fprintf(stderr, "**Flags %d %d\n", msg->flags&FPLMSG_FLG_BITS,
		      FPLMSG_FLG_STRING);
#endif
              if(msg && ((msg->flags&FPLMSG_FLG_BITS) != FPLMSG_FLG_STRING))
                return FPLERR_UNEXPECTED_INT_STATEMENT;
              if(!msg || !msg->message[0])
                /* We got a zero length string or no string at all! */
                expr->val.str=NULL; /* no string! */
              else
                /* the copied string! */
                expr->val.str=(struct fplStr *)msg->message[0];
            
#ifdef DEBUGMAIL
              DebugMail(scr, MAIL_RETURN_STRING, (long)pass->name,
                        expr->val.str);
#endif
              expr->flags=FPL_STRING|FPL_ACTION;
              break;
            case FPL_INTARG:
            default:
#if 0
	      fprintf(stderr, "**Int from %s\n", pass->name);
#endif
              if(msg && ((msg->flags&FPLMSG_FLG_BITS) != FPLMSG_FLG_INT))
                return FPLERR_UNEXPECTED_STRING_STATEMENT;
              /* only if integer! or the function is non-existent */
              expr->val.val=(msg?(long)msg->message[0]:0);
#ifdef DEBUGMAIL
              DebugMail(scr, MAIL_RETURN_INTEGER, (long)pass->name,
                        (void *)expr->val.val);
#endif
              CALL(NewMember(scr, &expr));
              break;
          }
          if(msg)
            DeleteMessage(scr, msg);

          if(!ident) {
            /*
             * The function we invoked was not found regularly!
	     * Free the name we allocated temporarily.
	     */
            FREE(pass->name); /* the name was strdup()'ed! */
	  }

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
        }
      } else {

          pos=0;
          switch(*scr->text) {
	  case CHAR_MULTIPLY:
	    /*
	     * This is the 'contents of' operator!
	     * The contents of the variable that follows this sign should
	     * get the following rvalue.
	     * Of course, we must first check that this really is a
	     * 'pointer' to a variable.
	     * If we declare this, make sure that it doesn't point to
	     * anything at all!
	     */

	    while(*++scr->text==CHAR_MULTIPLY); /* just in case! */

	    CALL(Getword(scr));
	    if(control&CON_DECLARE) {
	      return FPLERR_SYNTAX_ERROR; /* not yet supported */
	    }
	    else {
              CALL(GetIdentifier(scr, scr->buf, &ident));
	      if(!(ident->flags&FPL_REFERENCE))
	        return FPLERR_ILLEGAL_REFERENCE; /* referenced a non-reference! */
	      if(!ident->data.variable.ref)
		return FPLERR_ILLEGAL_REFERENCE; /* illegal reference! */

	      ident = ident->data.variable.ref; /* use the "actual" variable! */

	      /* we have an identifier and the level is OK! */
	      control |= CON_IDENT|CON_LEVELOK;
	      continue; /* now we have the pointer for the *real* variable! */
	    }
	    break;
          case CHAR_ZERO:
            /*
             * Numbers starting with a '0' can be hex/oct/bin.
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
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
              CALL(Warn(scr, FPLERR_UNEXPECTED_INT_STATEMENT));
            }
            do
              expr->val.val= expr->val.val*10 + *scr->text++ - CHAR_ZERO;
            while(isdigit(*scr->text));
            CALL(NewMember(scr, &expr));
  	  break;

  	  case CHAR_QUOTATION_MARK:
            if(control&CON_NUM) {
              /* NO integers allowed! */
              CALL(Warn(scr, FPLERR_UNEXPECTED_STRING_STATEMENT));
            }
            CALL(Convert(val, scr));
            /* This returned a string! */
            expr->val.str=val->val.str;
            expr->flags=FPL_STRING;
	    CALL(StringExpr(expr, scr));
  	  break;

  	  case CHAR_APOSTROPHE:
            /*
             * Apostrophes surround character. Returns ASCII code.
             */
            if(control&CON_STRING) {
              /* NO integers allowed! */
              CALL(Warn(scr, FPLERR_UNEXPECTED_INT_STATEMENT));
            }
            CALL(ReturnChar((scr->text++, scr), &expr->val.val, FALSE));
            if(*scr->text!=CHAR_APOSTROPHE) {
              CALL(Warn(scr, FPLERR_MISSING_APOSTROPHE)); /* >warning< */
              /* just continue as nothing has ever happened! */
            } else
              scr->text++;
            CALL(NewMember(scr, &expr));
  	  break;

  	  case CHAR_OPEN_PAREN:
            CALL(Expression(val, (++scr->text, scr), CON_GROUNDLVL|CON_NUM, NULL));
            if(*scr->text!=CHAR_CLOSE_PAREN) {
              CALL(Warn(scr, FPLERR_MISSING_PARENTHESES)); /* >warning< */
              /* Go on anyway! */
            } else
              scr->text++;
            expr->val.val=val->val.val;
            CALL(NewMember(scr, &expr));
            break;

  	  case CHAR_NOT_OPERATOR:
            CALL(AddUnary(scr, expr, OP_NOT));
            ++scr->text;
            break;

  	  case CHAR_ONCE_COMPLEMENT:
            CALL(AddUnary(scr, expr, OP_COMPL));
            ++scr->text;
  	    break;

  	  case CHAR_PLUS:
            if(scr->text[1]==CHAR_PLUS) {
              expr->flags|=FPL_ACTION;
              scr->text+=2;
              CALL(AddUnary(scr, expr, OP_PREINC));
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
            } else {
              CALL(AddUnary(scr, expr, OP_MINUS));
              scr->text++;
            }
            break;

          default:

            if((*scr->text==CHAR_SEMICOLON && control&CON_SEMICOLON) ||
               (*scr->text==CHAR_CLOSE_PAREN && control&CON_PAREN)
               && basexpr==expr && expr->operator==OP_NOTHING) {
              /* for(;;) support.
                 There must not have been a previous operand or operator */
              pos=expr->val.val=TRUE;
            } else {   /* no operand results in error! */
              CALL(Warn(scr, FPLERR_MISSING_OPERAND)); /* WARNING! */
              expr->operator=OP_NOTHING; /* reset */
            }
  	    break;
	  }
		  if(pos)
          break;
      }

    } else {                                         /* waiting for operator */
      uchar *point=scr->text;

      switch(*scr->text) {
      case CHAR_ASSIGN:
        if(scr->text[1]==CHAR_ASSIGN) {
          expr->operator=OP_EQUAL;
          scr->text+=2;
        }
        break;
      case CHAR_AND:
	if(scr->text[1]==CHAR_AND) {
          /*
           * This is a logical AND (&&)
           */
          scr->text+=2;

          /*
           * Get result from everything to the left of this!
           */
          CALL(Calc(scr, val, basexpr));

          /*
           * Clean the expression so far.
           */
          Clean(scr, basexpr);    /* erase the list */

          /*
           * Start a new list with this result
           */
          GETMEM(expr, sizeof(struct Expr));
          memset(expr, 0, sizeof(struct Expr));
          basexpr=expr;
          expr->val.val = val->val.val;

          if(!expr->val.val) {
            /*
             * In this case, its like in the 'a && b' expression and 'a'
             * equals 0. Then we should skip the 'b' expression.
             */
            CALL(ScanForNext(scr, OP_LOGAND));
            expr->flags = FPL_OPERAND;
          }
          continue;

        } else {
          expr->operator=OP_BINAND;
          scr->text++;
        }
        break;
      case CHAR_OR:
        if(scr->text[1]==CHAR_OR) {
          /*
           * This is a logical OR operator (||)
           */
          scr->text+=2;

          /*
           * Get result from everything to the left of this!
           */
          CALL(Calc(scr, val, basexpr));

          /*
           * Clean the expression so far.
           */
          Clean(scr, basexpr);    /* erase the list */

          /*
           * Start a new list with this result
           */
          GETMEM(expr, sizeof(struct Expr));
          memset(expr, 0, sizeof(struct Expr));
          basexpr=expr;
          expr->val.val = val->val.val;

          if(expr->val.val) {
            /*
             * In this case, its like in the 'a || b' expression and 'a'
             * equals 1. Then we should skip the 'b' expression.
             */
            CALL(ScanForNext(scr, OP_LOGOR));
            expr->flags = FPL_OPERAND;
          }
          continue;

        } else {
          expr->operator=OP_BINOR;
          scr->text++;
        }
        break;
      case CHAR_PLUS:
        expr->operator=OP_PLUS;
        ++scr->text;
        break;
      case CHAR_MINUS:
        expr->operator=OP_MINUS;
        ++scr->text;
        break;
      case CHAR_QUESTION:
        ++scr->text;
        /*
         * This is the first operator in a conditional operator sequence (?)
         */

        /*
         * Get result from everything to the left of this!
         */
        CALL(Calc(scr, val, basexpr));

        /*
         * Clean the expression so far.
         */
        Clean(scr, basexpr);    /* erase the list */

        /*
         * Start a new list with this result
         */
        GETMEM(expr, sizeof(struct Expr));
        memset(expr, 0, sizeof(struct Expr));
        expr->flags = FPL_OPERAND;
        basexpr=expr;

        if(val->val.val) {
          /*
           * In this case, its like in the 'a ? b : c' expression and 'a'
           * equals 1. Then we should skip the 'c' expression.
           */
          CALL(Expression(val, scr, CON_NORMAL, NULL));
          if(*scr->text++!=CHAR_COLON)
            return FPLERR_ILLEGAL_CONDOP;
          CALL(ScanForNext(scr, OP_COND2));          
        }
        else {
          /*
           * In this case, its like in the 'a ? b : c' expression and 'a'
           * equals 0. Then we should skip the 'b' expression.
           */
          CALL(ScanForNext(scr, OP_COND1));
          if(*scr->text++!=CHAR_COLON)
            return FPLERR_ILLEGAL_CONDOP;
          CALL(Expression(val, scr, CON_NORMAL, NULL));
        }
        expr->val.val = val->val.val;
        continue; /* check for next operator */

        break;
#if 0
      case CHAR_COLON:
        if(conditional) {
          /* only if preceeded with the regular '?' operator! */
	  conditional--;
          expr->operator=OP_COND2;
          ++scr->text;
        }
        break;
#endif
      case CHAR_MULTIPLY:
        expr->operator=OP_MULTIPLY;
        ++scr->text;
        break;
      case CHAR_DIVIDE:
        expr->operator=OP_DIVISION;
        ++scr->text;
        break;
      case CHAR_REMAIN:
        expr->operator=OP_REMAIN;
        ++scr->text;
        break;
      case CHAR_XOR:
        expr->operator=OP_BINXOR;
        ++scr->text;
        break;
      case CHAR_LESS_THAN:
        if(scr->text[1]==CHAR_ASSIGN) {
          scr->text+=2;
          expr->operator=OP_LESSEQ;
        } else if(scr->text[1]==CHAR_LESS_THAN) {
          scr->text+=2;
          expr->operator=OP_SHIFTL;
        } else {
          scr->text++;
          expr->operator=OP_LESS;
        }
        break;
      case CHAR_GREATER_THAN:
	if(scr->text[1]==CHAR_ASSIGN) {
          expr->operator= OP_GRETEQ;
          scr->text+=2;
        } else if(scr->text[1]==CHAR_GREATER_THAN) {
          scr->text+=2;
          expr->operator=OP_SHIFTR;
        } else {
          scr->text++;
          expr->operator=OP_GRET;
        }
        break;
      case CHAR_NOT_OPERATOR:
        if(scr->text[1]==CHAR_ASSIGN) {
          expr->operator=OP_NOTEQ;
          scr->text+=2;
        }
        break;
      case CHAR_COMMA:
        if(control&CON_GROUNDLVL) {
          /*
           * Get result from everything to the left of this!
           * For unary operators.
           */
          CALL(Calc(scr, val, basexpr));

          Clean(scr, basexpr);
          GETMEM(basexpr, sizeof(struct Expr));
          expr=basexpr;
          expr->val.val=0;
          expr->unary=NULL;
          expr->operator=expr->flags=OP_NOTHING;
          expr->next=NULL;
          scr->text++;
        }
        break;
      }
      if(point==scr->text)
        break;
      expr->flags&=~FPL_OPERAND; /* clear the operand bit */
    }
  } while(1);

  if(!(control&(CON_DECLARE /* |CON_ACTION */ ))) {
    /*
     * Get result of the current expression only if this isn't called
     * as a declaring (no one wants the return code from 'int a'!)
     * or a stand-alone (they have no receiver anyway) statement.
     */
    CALL(Calc(scr, val, basexpr));

    /*
     * If this was a stand alone statement, including no action returns an
     * error!
     */
    if(control&CON_ACTION && !(val->flags&FPL_ACTION)) {
      CALL(Warn(scr, FPLERR_NO_ACTION));
      /* but we can just as good keep on anyway! */
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

ReturnCode REGARGS
Calc(struct Data *scr,
     struct Expr *val,
     struct Expr *basexpr)
{
  /* lower value=higher priority. Order as the operator list in script.h:
   *|    +  -  /  * << >>  %  &  |  ^ && ||  ~    ?   :  == <= >=  <  > != ! */
  const static uchar priority[]={
    (uchar)255, 1, 1, 0, 0, 2, 2, 0, 5, 7, 6, 8, 9, (uchar)255, 10, 10, 4, 3, 3, 3, 3, 4, (uchar)255
    };

  ReturnCode ret;
  uchar pri, minpri=(uchar)255, maxpri=0;
  struct Expr *expr=basexpr, *last;
  struct Unary *un, *next;

  /* first all Unary expressions */
  if(!(expr->flags&FPL_STRING)) {
    while(expr) {
      if(priority[expr->operator]<minpri)
        minpri=priority[expr->operator]; /* get the lowest priority */
      if(priority[expr->operator]>maxpri && expr->operator!=OP_NOTHING)
        maxpri=priority[expr->operator]; /* get the highest priority */
      if(expr->flags&FPL_STRING) {
        CALL(Warn(scr, FPLERR_ILLEGAL_VARIABLE));
        /*
         * A string among the integers!
         * We remove this and try next!
         */

        last=expr->next;
        FREE(expr); /* delete this bastard from the expression!!! */
        expr=last;
      } else {
        un=expr->unary;
        while(un) {
          switch(un->unary) {
          case OP_NOT:
            expr->val.val=!expr->val.val;
            break;
          case OP_COMPL:
            expr->val.val=~expr->val.val;
            break;
          case OP_MINUS:
            expr->val.val=-expr->val.val;
            break;
            /*simply ignored!
              case OP_PLUS:
              break;
              */
          case OP_PREDEC:
          case OP_PREINC:
            CALL(Warn(scr, FPLERR_ILLEGAL_PREOPERATION));
            /* just ignore it! */
          }
          next=un->next;
          FREE(un);
          un=next;
        }
      }
      expr=expr->next;
    }
  }
  /*
   * Calculate all members of the linked list in the proper way and put
   * the result in "val->val.val" before returning "ret". Check for operators
   * with priority within `minpri' and `maxpri' which we got in the loop
   * above.
   *
   * Check priority level by priority level and perform the right actions.
   * When reaching the maxpri, there is only one number left: the result!
   */

  for(pri=minpri; pri<=maxpri; pri++) {
    last=expr=basexpr;
    while(expr=expr->next) {
      if(priority[expr->operator]==pri) {
        last->flags|=expr->flags;
        switch(expr->operator) {
        case OP_MULTIPLY:
          last->val.val*=expr->val.val;
          break;
        case OP_DIVISION:
          if(!expr->val.val) {
            CALL(Warn(scr, FPLERR_DIVISION_BY_ZERO));
            /* we give a zero as result! */
            last->val.val=0;
          } else
            last->val.val/=expr->val.val;
          break;
        case OP_REMAIN:
          if(!expr->val.val) {
            CALL(Warn(scr, FPLERR_DIVISION_BY_ZERO));
            last->val.val=0;
          } else
            last->val.val%=expr->val.val;
          break;
        case OP_SHIFTL:
          last->val.val<<=expr->val.val;
          break;
        case OP_SHIFTR:
          last->val.val>>=expr->val.val;
          break;
        case OP_BINAND:
          last->val.val&=expr->val.val;
          break;
        case OP_BINOR:
          last->val.val|=expr->val.val;
          break;
        case OP_BINXOR:
          last->val.val^=expr->val.val;
          break;
        case OP_PLUS:
          last->val.val+=expr->val.val;
          break;
        case OP_MINUS:
          last->val.val-=expr->val.val;
          break;
        case OP_EQUAL:
          last->val.val=last->val.val==expr->val.val;
          break;
        case OP_NOTEQ:
          last->val.val=last->val.val!=expr->val.val;
          break;
        case OP_LESSEQ:
          last->val.val=last->val.val<=expr->val.val;
          break;
        case OP_LESS:
          last->val.val=last->val.val<expr->val.val;
          break;
        case OP_GRETEQ:
          last->val.val=last->val.val>=expr->val.val;
          break;
        case OP_GRET:
          last->val.val=last->val.val>expr->val.val;
          break;
        case OP_LOGOR:
          last->val.val=last->val.val||expr->val.val;
          break;
        case OP_LOGAND:
          last->val.val=last->val.val&&expr->val.val;
          break;
        case OP_COND1:
          if(expr->next && expr->next->operator==OP_COND2) {
            last->val.val=last->val.val?expr->val.val:expr->next->val.val;
          } else {
            CALL(Warn(scr, FPLERR_ILLEGAL_CONDOP)); /* WARNING! */
            last->val.val=expr->val.val; /* get the number we have! */
          }
          break;
        }
        last->next=expr->next;
        FREE(expr);
        expr=last;
      } else
        last=expr;
    }
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

ReturnCode REGARGS
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

void REGARGS Clean(struct Data *scr, struct Expr *basexpr)
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

#ifdef DEBUG
  CheckMem(scr, pointer);
#endif
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

static ReturnCode INLINE GetArrayInfo(struct Data *scr,
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
  if(*scr->text==CHAR_OPEN_BRACKET) {
    do {
      scr->text++; /* pass the open bracket */
      /* eval the expression: */
      CALL(Expression(val, scr, CON_GROUNDLVL|CON_NUM, NULL));

      if(*scr->text!=CHAR_CLOSE_BRACKET) {
        /* no close bracket means error */
        CALL(Warn(scr, FPLERR_MISSING_BRACKET)); /* >warning< */
        /* go on anyway! */
      } else
        scr->text++;

      if(val->val.val<(control&CON_DECLARE?1:0)) {
        /* illegal result of the expression */
        /*
         * Write back the original variable name to the buffer!
         */
        strcpy(scr->buf, name);
        ret = FPLERR_ILLEGAL_ARRAY;
        break;
      }

      dims[(*num)++]=val->val.val; /* Add another dimension */
      if(*num==maxnum) {
        /* we've hit the roof! */
        break;
      } else if(*num==MAX_DIMS) {
        /* if we try to use too many dimensions... */
        ret=FPLERR_ILLEGAL_ARRAY;
        /*
         * Write back the original variable name to the buffer!
         */
        strcpy(scr->buf, name);
        break;
      }
      /*
       * Go on as long there are braces and we are declaring OR
       * as long the `num' variable tells us (you, know: when
       * you want to read character five in a member of a
       * three dimensional string array, it could look like
       * "int a=string[2][3][4][5];" ... :-)
       */
    } while(*scr->text==CHAR_OPEN_BRACKET);
  }
  FREE(val);
  return(ret);
}

/**********************************************************************
 *
 * ArrayNum()
 *
 * Return which array position we should look in when the user wants the
 * array member presented as a number of dimensions and an array with the
 * dimension sizes.
 *
 ******/

long REGARGS
ArrayNum(long num,     /* number of dimensions specified */
         long dnum,    /* number of dimensions declared  */
         long *dims,   /* dimensions specified */
         long *decl)   /* declared dimension information */
{
  long i;
  long pos=0;
  long base=1;
  if(num!=dnum)
    /*
     * Then we can't get proper information!!!
     */
    return(-1);
  for(i=0; i<num; i++) {
    if(dims[i]>=decl[i])
      return(-1);

    pos+=dims[i]*base;
    base*=decl[i];
  }
  return(pos);
}


/**********************************************************
 *
 * CallFunction()
 *
 * Calls a function. Internal, external or inside!!
 *
 *******/

ReturnCode REGARGS
CallFunction(struct Data *scr,
             struct fplArgument *pass,
             struct Identifier *ident)
{
  ReturnCode ret;
  if(ident && ident->flags&FPL_INTERNAL_FUNCTION) {
    CALL(functions(pass));
  } else if(ident && ident->flags&FPL_INSIDE_FUNCTION) {
    CALL(inside(scr, pass, ident));
  } else { /* if (EXTERNAL_FUNCTION) */
    pass->funcdata=ident?ident->data.external.data:(void *)NULL;

#if defined(AMIGA) && defined(SHARED)
    if(ret=CheckStack(scr, scr->stack_limit, scr->stack_margin)) {
      if(ret==1)
        return(FPLERR_OUT_OF_MEMORY);
      else
        return(FPLERR_OUT_OF_STACK);
    }
#endif

    if(ident && ident->data.external.func) {
      /*
       * If this is non-zero, a function specific function pointer
       * has been assigned to it! In that case we should call that
       * function instead of the traditional, global one!
       */
      CALL(InterfaceCall(scr, pass, ident->data.external.func));
    } else {
      CALL(InterfaceCall(scr, pass, scr->function));
    }

  }
  return(FPL_OK);
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
  uchar *t=scr->text;
  struct Local *locals=NULL;
  long p=scr->prg;
  long vp=scr->virprg;
  uchar *vf=scr->virfile;
  uchar count; /* parameter counter */
  uchar *text;
  struct Condition con;
  struct Expr *val;
  uchar oldret;
  uchar cont;
  long search;
  struct Program *prog=scr->prog;
  struct fplVariable *tempvar;
  uchar reference;
  long breaks;

  GETMEM(val, sizeof(struct Expr));
  if(scr->prog->name != func->data.inside.file) {
    struct Program *prog=scr->programs;
    while(prog) {
      if(prog->name && !strcmp(prog->name, func->data.inside.file))
        break;
      prog=prog->next;
    }
    if(prog) {
      CALL(LeaveProgram(scr, scr->prog));
      CALL(GetProgram(scr, prog));
      scr->prog=prog;
    } else
      return FPLERR_INTERNAL_ERROR; /* This is a dead-end error! */
  }

  if(func->flags&FPL_INSIDE_NOTFOUND) {
    /*
     * We have no current information about where this function
     * is to be found. Search for it and store the location in
     * ->text and ->prg.
     */

    cont=TRUE;
    search=(func->data.inside.ret==FPL_STRARG)?CMD_STRING:
    (func->data.inside.ret==FPL_INTARG)?CMD_INT:CMD_VOID;

    /*
     * Start searching from the declaration position to enable local functions!
     */

    scr->text=(&scr->prog->program)[scr->prog->startprg-1]+
      func->data.inside.col;
    scr->prg=func->data.inside.prg;
    scr->virprg=func->data.inside.virprg;
    scr->virfile=func->data.inside.virfile;
    while(cont && !ret) {
      switch(*scr->text) {
      case CHAR_OPEN_BRACE:
        /* ...go to the corresponding brace */
        ret=GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, TRUE);
        break;
      case CHAR_OPEN_PAREN:
        /* ...go to the corresponding parenthesis */
        ret=GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE);
        break;
      case CHAR_QUOTATION_MARK:
        scr->text++;
        /* dirty use of function: */
        ret=GetEnd(scr, CHAR_QUOTATION_MARK, CHAR_QUOTATION_MARK, FALSE);
        break;
      case CHAR_ASCII_ZERO:
        if(Newline(scr))
          ret=FPLERR_INSIDE_NOT_FOUND;
        break;
      case CHAR_DIVIDE: /* to eat comments */
      case CHAR_SPACE:
      case CHAR_TAB:
      case CHAR_NEWLINE:
        if(Eat(scr))
          ret=FPLERR_INSIDE_NOT_FOUND;
        if(*scr->text==CHAR_HASH) {
          /* This should read a #line statement for new virtual line number */
          while(*scr->text++!=CHAR_NEWLINE);
          scr->virprg++;
        }
        break;
      case CHAR_CLOSE_BRACE: /* local function searches might hit this! */
        ret=FPLERR_INSIDE_NOT_FOUND;
        break;
      default:
        if(isident(*scr->text)) {
          Getword(scr);
          GetIdentifier(scr, scr->buf, &pident);
          if(pident && /* valid identifier */
             pident->data.external.ID==search) {  /* and it's the right one */
            if(!Getword(scr)) {
              GetIdentifier(scr, scr->buf, &pident);
              if(pident && pident->flags&FPL_INSIDE_FUNCTION) /* an inside */
                cont=strcmp(pident->name, func->name); /* is it the right? */
            }
          } else
            while(isident(*scr->text))
              scr->text++;
        } else
          scr->text++;
        break;
      }
    }
    if(ret) {
      strcpy(scr->buf, func->name); /* enable better error report! */
      scr->prg=p;
      scr->text=t;
      scr->virprg=vp;
      return FPLERR_INSIDE_NOT_FOUND; /* dead end error */
    }
    func->data.inside.col=scr->text-(&scr->prog->program)[scr->prg-1];
    func->data.inside.prg=scr->prg;
    func->data.inside.virprg=scr->virprg;
    func->data.inside.virfile=scr->virfile;
    func->flags&=~FPL_INSIDE_NOTFOUND; /* we have found it! */
  } else {
    /*
     * We know where to find this function!
     */

    scr->prg=func->data.inside.prg;
    scr->text=(&scr->prog->program)[scr->prg-1]+func->data.inside.col;
    scr->virprg=func->data.inside.virprg;
    scr->virfile=func->data.inside.virfile;
  }

  /*
   * Some of this boring stuff only has to be done on interepreted function
   * calls, since compiled ones have much smarted function argument
   * handling!
   */
  if(!(func->flags & FPL_COMPILER_ADDED)) {
    
    /**********************************
     * PARSE THE PARAMETER LIST HERE! *
     **********************************/
  
    CALL(Eat(scr));
  
    if(*scr->text!=CHAR_OPEN_PAREN) {
      CALL(Warn(scr, FPLERR_MISSING_PARENTHESES));
      /* we can survive without that! */
    } else
      scr->text++;
  
    if(func->data.inside.format) {
      /*
       * We won't hit this if no arguments is prototyped.
       */
  
      count=0; /* parameter counter */
      text=func->data.inside.format;
  
      if(!*text) {
        if(!Getword(scr) && strcmp(scr->buf, "void")) {
          /* it should be "void" or nothing! If it wasn't we fail! */
          CALL(Warn(scr, FPLERR_ILLEGAL_DECLARE));
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
              return FPLERR_ILLEGAL_REFERENCE;
  	  }
  
            if((*text==FPL_STRARG &&
               ident->data.external.ID!=CMD_STRING) ||
               (*text==FPL_INTARG &&
               ident->data.external.ID!=CMD_INT))
              return FPLERR_ILLEGAL_DECLARE;
  
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
            tempvar->size=1; /* This is not an array */
            GETMEM(tempvar->var.val32, sizeof(void *));
            if(*text==FPL_INTARG) {
              tempvar->var.val32[0]=(long)arg->argv[count];
            } else {
              /* Store string length in variable `len' */
              register long len=GETSTRLEN(arg->argv[count]);
              GETMEM(tempvar->var.str[0], sizeof(struct fplStr)+len);
              tempvar->var.str[0]->alloc=len;
  
              /* We copy the ending zero termination too! */
              memcpy(tempvar->var.str[0]->string, ((uchar *)arg->argv[count]), len+1);
              tempvar->var.str[0]->len=len;
            }
            /*
             * Emulate next level variable declaration by adding one
             * to the ->level member here... dirty but (fully?)
             * functional!!!! ;-)
             */
  
            pident->level=scr->varlevel+1;
            pident->file=scr->prog->name;
            pident->func=func;
            CALL(AddVar(scr, pident, &locals));
            break;
          case FPL_STRVARARG:
          case FPL_INTVARARG:
  	case FPL_STRARRAYVARARG:
  	case FPL_INTARRAYVARARG:
  	  if(!reference) {
  	    /*
  	     * It was never said to be a symbol reference!!
  	     */
              return FPLERR_ILLEGAL_REFERENCE;
  	  }
            if((*text==FPL_STRVARARG || *text == FPL_STRARRAYVARARG) &&
  	     ident->data.external.ID!=CMD_STRING) {
  	    return FPLERR_ILLEGAL_DECLARE;
  
            } else if((*text==FPL_INTVARARG || *text == FPL_INTARRAYVARARG) &&
  		    ident->data.external.ID!=CMD_INT) {
              return FPLERR_ILLEGAL_DECLARE;
            }
            /*
             * Declare the following word as a variable which
             * will use the struct fplVariable pointer as given in the
             * calling parameter list.
             */
  
            CALL(Getword(scr));
  
  	  if(*text == FPL_INTARRAYVARARG ||
  	     *text == FPL_STRARRAYVARARG) {
  	      CALL(Eat(scr));
                if(CHAR_OPEN_BRACKET != scr->text[0])
                  return FPLERR_ILLEGAL_DECLARE;
  	      if(GetEnd(scr, CHAR_CLOSE_BRACKET, CHAR_OPEN_BRACKET, FALSE))
                  return FPLERR_MISSING_BRACKET;
  	  }
  
            if(arg->argv[count]) {
              /*
               * If the wrong kind of variable was sent in the function call, no
               * varible will be sent, and no one will be declared.
               */
  
              GETMEM(pident, sizeof(struct Identifier));
  
              *pident=*(struct Identifier *)arg->argv[count];
              pident->flags |= FPL_REFERENCE;
              pident->data.variable.ref= (struct Identifier *)arg->argv[count];
              /* original fplVariable position */
  
              STRDUP(pident->name, scr->buf);
  
              pident->level=scr->varlevel+1;
              pident->file=scr->prog->name;
              pident->func=func;
              CALL(AddVar(scr, pident, &locals));
            }
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
        CALL(Warn(scr, FPLERR_MISSING_PARENTHESES));
        /* who needs ending parentheses? */
      } else
        scr->text++;
    } else {
      /*
       * No argument is useable to this function. There might be a
       * `void' keyword here, but nothing else! Just search for the
       * closing parenthesis to fasten interpreting!
       */
  
      if(ret=GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, FALSE)) {
        CALL(Warn(scr, FPLERR_MISSING_PARENTHESES));
        /* ok, then search for the open brace where the program starts! */
        ret=GetEnd(scr, CHAR_OPEN_BRACE, CHAR_OPEN_PAREN, FALSE);
        if(ret) {
          CALL(Warn(scr, FPLERR_MISSING_BRACE));
        } else
          scr->text--; /* back on brace */
        /* ok, then we say that the program starts right here! */
      }
    }
  
    CALL(Eat(scr));
    if(*scr->text!=CHAR_OPEN_BRACE) {
      CALL(Warn(scr, FPLERR_MISSING_BRACE));
      /* we can do with a start without it! */
    } else
      scr->text++;

  } /* end of non-compiled function specific setups */
  
  oldret=scr->strret;
  scr->strret=func->data.inside.ret==FPL_STRARG; /* should we receive a string? */

  con.bracetext=scr->text;
  con.braceprg=scr->prg;
  text=(void *)scr->func; /* backup current */
  scr->func=func;

  breaks = scr->breaks;
  scr->breaks=0;

  {
    register struct fplArgument *targ=arg;
    scr->arg = arg; /* for compiled functions to use */
    arg = targ; /* store the old */
  }

  /*********************
   * RUN THE FUNCTION! *
   *********************/

  scr->prog->openings++;
  ret=Script(scr, val, SCR_BRACE|SCR_FUNCTION, &con);
  scr->prog->openings--;

  scr->breaks=breaks;

  if(!(func->flags&FPL_COMPILER_ADDED)) {
    /*
     * Delete all variables created on our list for use
     * only in the function we just came back from!
     */
    DelLocalVar(scr, &locals);
  }

  scr->arg = arg; /* get back previous */

  if(!ret && val->flags & FPL_CONTINUE)
    ret = FPLERR_ILLEGAL_CONTINUE;

  if(ret) {
    if(scr->prog != prog) {
      LeaveProgram(scr, scr->prog); /* leave the failed program! */
      GetProgram(scr, prog); /* fetch the previous program again! */
    }
    return(ret);
  }
  scr->func=(void *)text; /* restore last */

  FREE(val);

  scr->text=t;
  scr->prg=p;
  scr->virprg=vp;
  scr->virfile=vf;
  scr->strret=oldret;
  if(scr->prog!=prog) {
    CALL(LeaveProgram(scr, scr->prog));
    scr->prog=prog;
    CALL(GetProgram(scr, scr->prog));
  }
  return(FPL_OK);
}

static ReturnCode INLINE PrototypeInside(struct Data *scr,
					 struct Expr *val,
					 long control,
					 struct Identifier *ident)
{
  /*
   * Prototyping an `inside' function!
   *
   * We have already received the return type, now we must
   * parse the paraters given within the parentheses. Legal
   * parameters are only combinations of `string', `int',
   * `string &' and `int &', or a single `void' (if no argument
   * should be sent to the function). Arguments specified in
   * a prototype is required, there is no way to specify an
   * optional parameter or a parameter list.
   */

  struct Identifier *pident;
  long pos=0;
  ReturnCode ret = FPL_OK;
  uchar *array;
  uchar found=ident?TRUE:FALSE;

  if(!found) {
    GETMEM(pident, sizeof(struct Identifier));
    STRDUP(pident->name, scr->buf);
  } else {
    /* we already know about this function! */
    if(ident->flags&(FPL_INTERNAL_FUNCTION|FPL_KEYWORD|FPL_EXTERNAL_FUNCTION))
      return FPLERR_IDENTIFIER_USED;
    pident = ident;
  }

  if(!found || (found && ident->flags&FPL_INSIDE_NOTFOUND)) {
    /* we know where this is... */
    pident->data.inside.col=scr->text-(&scr->prog->program)[scr->prg-1];
    pident->data.inside.prg=scr->prg;
    pident->data.inside.file=scr->prog->name;
    pident->data.inside.virprg=scr->virprg;
    pident->data.inside.virfile=scr->virfile;

    pident->file=scr->prog->name; /* file! */
    pident->func=scr->func; /* declared in this function */
    pident->level=control&CON_DECLGLOB?0:scr->varlevel;
  }

  if(found) {
    /* we already know about this function! */

    CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));

    CALL(Eat(scr));

    if(scr->text[0]==CHAR_OPEN_BRACE) {
      /* now the function is found! */
      if(!(ident->flags&FPL_INSIDE_NOTFOUND))
        /* the function has already been defined and is defined here again! */
        return FPLERR_IDENTIFIER_USED;

      ident->flags&=~FPL_INSIDE_NOTFOUND;

      if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, TRUE))
        return FPLERR_MISSING_BRACE;
      scr->text--; /* back on close brace */
      val->flags|=FPL_DEFUNCTION;
    }

    return FPL_OK;
  }

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
      if(*scr->text!=CHAR_CLOSE_PAREN) {
        CALL(Warn(scr, FPLERR_ILLEGAL_PROTOTYPE));
        CALL(GetEnd(scr, CHAR_CLOSE_PAREN, CHAR_OPEN_PAREN, TRUE));
      } else
        scr->text++;
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
      CALL(Warn(scr, FPLERR_ILLEGAL_PROTOTYPE));
      continue; /* if we against all odds are ordered to go on! */
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
      CALL(Warn(scr, FPLERR_ILLEGAL_PROTOTYPE));
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

  CALL(Eat(scr)); /* Eat white space */


  if(*scr->text==CHAR_OPEN_BRACE) {
    /* It's the actual function!!! */
	if(GetEnd(scr, CHAR_CLOSE_BRACE, CHAR_OPEN_BRACE, TRUE))
	  return FPLERR_MISSING_BRACE;
	scr->text--; /* back on close brace */
    val->flags|=FPL_DEFUNCTION;
  } else {
    val->flags&=~FPL_DEFUNCTION;
    pident->flags|=FPL_INSIDE_NOTFOUND;
  }
  CALL(AddVar(scr, pident,
              control&CON_DECLGLOB?&scr->globals:&scr->locals));

  return(ret);
}
