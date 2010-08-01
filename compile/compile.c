/************************************************************************
 *		         FREXX PROGRAMMING LANGUAGE                     *
 ************************************************************************

 Compile.c
 
 Functions to support the compiled programs executions.

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

#ifdef COMPILE
 
#include "script.h"
#include "compile.h"
#include <stddef.h>

ReturnCode REGARGS
FixVariable(struct Data *scr,
            struct Identifier *ident,
            long control,
            struct Expr *expr);
ReturnCode REGARGS FixFunction(struct Data *,
                               struct Expr **,
                               struct Expr *,
                               Pass2,
                               long);
ReturnCode REGARGS
CmpStringExpr(struct Expr *val,		/* original string -> new */
	     struct Data *scr);		/* standard */

ReturnCode REGARGS
GetArrayNum(struct Data *,
            struct Expr *,
            long *,
            struct Identifier *);

/************************************************************************
 *
 * IsCompiled()
 *
 * Returns the start-index if the program sent as parameter is compiled,
 * or a negative value if not.
 *
 *********************************/
 
ReturnCode REGARGS SetupCompiled(struct Program *prog)
{
  uchar *original;
  uchar *progpnt = prog->program;
  if(!progpnt || strncmp(progpnt, COMPILED_HEADER, strlen(COMPILED_HEADER)))
    return -1; /* not compiled, run as usual! */

  if(!(prog->flags&PR_COMPILED)) {
    
    original = progpnt; /* store where we start at! */
  
    /*
     * Compiled programs contain at least this following header that
     * we must pass in an elegant and forward compatible way!
     * 'CODE' is the hunk we're after!
     */
    progpnt += COMPILED_HEADER_LEN; /* get to the first hunk */
  
    while(strcmp(progpnt, COMPILED_HUNK_CODE)) {
      /*
       * As long as we haven't found the 'CODE' hunk, skip the unknown ones.
       */
      progpnt += COMPILED_HUNKNAME_LEN;
      progpnt += *(long *)progpnt + COMPILED_HUNKLENGTH_LEN;
    }
    
    progpnt += COMPILED_HUNKNAME_LEN + COMPILED_HUNKLENGTH_LEN;
  
    /*
     * Set the information
     */
    
    prog->flags |= PR_COMPILED;
    prog->index = progpnt - original; /* index from start */
    prog->startcol = prog->index;
  }
  return FPL_OK;
}

/*
 * CmpReset() - clears a local variable
 */
 
ReturnCode REGARGS
CmpReset(struct Data *scr,
         long num)
{
  register long size=0;
  long loop;
  struct fplVariable *var = & scr->localinfo.list [ num ]->data.variable;
  loop = var->size;
  if( scr->localinfo.list [ num ]->flags & FPL_INT_VARIABLE) {
    while(size < loop )
      var->var.val32[ size++ ] = 0; /* reset to zero */
  }
  else {
    while(size < loop ) {
      if( var->var.str[ size ] ) {
         /* The string has been set, make it zero length and zero
            terminated */
        var->var.str[ size ]->len = 0;
        var->var.str[ size ]->string[0] = 0;
      }
      size++;
    }
  }
  return FPL_OK;
}

/*
 * ReturnVariable() - returns the identifier pointer to the variable
 */
ReturnCode REGARGS
ReturnVariable(struct Data *scr,
               struct Identifier **ident,
               long flags)
{
  struct Identifier *pident;

  GETMEM(pident, sizeof(struct Identifier));
  memset(pident, 0, sizeof(struct Identifier));
  pident->flags =flags;

  GETMEM(pident->data.variable.var.val32, sizeof(long));
  *pident->data.variable.var.val32=0;
  pident->data.variable.num=0;
  pident->data.variable.size=1;
  
  *ident = pident;
  return FPL_OK;
}

/*
 * CmpExport() - exports a specified function
 */
ReturnCode REGARGS CmpExport(struct Data *scr)
{
  struct Identifier *pident;
  ReturnCode ret;

  GETMEM(pident, sizeof(struct Identifier));
  memset(pident, 0, sizeof(struct Identifier));
  pident->flags = GETLONG | FPL_COMPILER_ADDED;
  P_LONG;
  
  /* start position index (add the actual index too) */
  pident->data.inside.col = scr->prog->index + GETLONG;
  P_LONG;
  pident->data.inside.virfile = scr->virfile;
  if( pident->flags & FPL_INT_VARIABLE)
    pident->data.inside.ret = FPL_INTARG;
  else
    pident->data.inside.ret = FPL_STRARG;

  /*
   * Get name!
   */
  pident->name =
    &scr->prog->program [ scr->prog->index + GETLONG + sizeof(long)];
  P_LONG;

  /*
   * Get parameter format!
   */
  pident->data.inside.format =
    &scr->prog->program [ scr->prog->index + GETLONG + sizeof(long)];
  P_LONG;


  /*
   * Setup the exported variable:
   */
  pident->data.inside.file = scr->prog->name;
  pident->data.inside.virfile = scr->virfile;
  pident->data.inside.prg = 1; /* always first line! */
  
  CALL(AddVar(scr, pident, &scr->globals ));

  return FPL_OK;
}

/*
 * CmpDeclare() - declares all kinds of variables
 */
ReturnCode REGARGS
CmpDeclare(struct Data *scr)
{
  long flags;
  long amount;
  long firstid;
  struct Identifier **temp;
  struct Identifier *ident;
  ReturnCode ret;
  
  flags   = GETLONG | FPL_COMPILER_ADDED;
  firstid = *(long *)(scr->text+sizeof(long));

  if(!(flags&FPL_EXPORT_SYMBOL)) {
    amount  = *(long *)(scr->text+sizeof(long)*2);
    
    scr->text += sizeof(long)*3; /* pass the three data longs */
    
    if(!(flags&FPL_GLOBAL_SYMBOL)) {
      /* These are local ones */
      
      if(!scr->localinfo.listentries) {
        scr->localinfo.listsize = DEFAULT_LISTSIZE;
        GETMEM(scr->localinfo.list,
               scr->localinfo.listsize*sizeof(struct Identifier *));
      }
      
      /*
       * Have we room for those new local symbols?
       */
      if(firstid + amount >= scr->localinfo.listsize) {
        scr->localinfo.listsize = firstid + amount+1; /* OLD += DEFAULT_LISTSIZE; */
        GETMEM(temp,
               scr->localinfo.listsize*sizeof(struct Identifier *));
        memcpy(temp, scr->localinfo.list, 
               scr->localinfo.listentries * sizeof(struct Identifier *) );
        FREE(scr->localinfo.list);
        scr->localinfo.list = temp;
      }

      
      while(amount--) {
        CALL(ReturnVariable(scr, 
                            &scr->localinfo.list [ firstid ],
                            flags));
        CALL(AddToList(scr, scr->localinfo.list [ firstid ], &scr->locals));
        if(++firstid > scr->localinfo.listentries)
          scr->localinfo.listentries = firstid;
      }
    }
    else {
      /* add to the global list */

      if(!scr->globalinfo->listentries) {
        scr->globalinfo->listsize = DEFAULT_LISTSIZE;
        GETMEMA(scr->globalinfo->list,
               scr->globalinfo->listsize*sizeof(struct Identifier *));
      }
      
      /* this is a certain amount of local symbols */
      if(firstid + amount >= scr->globalinfo->listsize) {
        scr->globalinfo->listsize = firstid + amount + 1;
        GETMEMA(temp,
               scr->globalinfo->listsize*sizeof(struct Identifier *));
        memcpy(temp, scr->globalinfo->list, 
               scr->globalinfo->listentries * sizeof(struct Identifier *) );
        FREEA(scr->globalinfo->list);
        scr->globalinfo->list = temp;
      }
      
      while(amount--) {
        CALL(ReturnVariable(scr, 
                            &scr->globalinfo->list [ firstid ],
                            flags));
        CALL(AddToList(scr, scr->globalinfo->list [ firstid ],
                       &scr->globals));
        if(++firstid > scr->globalinfo->listentries)
          scr->globalinfo->listentries = firstid;
      }
    }
  }
  else {
    CALL(ReturnVariable(scr, &ident, flags));
    ident->name = scr->prog->program + scr->prog->index + firstid +
      sizeof(long); /* skip the hash for now */
    CALL(AddVar(scr, ident, &scr->globals));
    scr->text += sizeof(long)*2;
  }
  
  return FPL_OK;
}

ReturnCode REGARGS
CmpStringExpr(struct Expr *val,		/* original string -> new */
	     struct Data *scr)		/* standard */
{
  ReturnCode ret;
  struct fplStr *whole;

  if(PASS2_STRING_APPEND == GETSHORT) {

    GETMEM(whole, sizeof(struct fplStr));
    memset(whole, 0, sizeof(struct fplStr));
    
    /* put string in new string variable */
    CALL(StrAssign(val->val.str, scr, &whole,
                   TRUE)); /* TRUE == append */
    
    do {
      
      P_SHORT; /* pass the add instruction */
      CALL(CmpExpr(val, scr, CON_STRING));

      /* append string to that new variable */
      CALL(StrAssign(val->val.str, scr, &whole, TRUE));
      
      if(!(val->flags&FPL_NOFREE) && val->val.str)
	FREE(val->val.str);

    } while(PASS2_STRING_APPEND == GETSHORT);
      
    val->val.str = whole; /* get the string info! */
    val->flags&=~FPL_NOFREE; /* free this, yes! */
  }
  return FPL_OK;
}


/*
 * Let's fix this bloody assign, and leave the return code in the
 * (struct Expr *) we get!
 */


ReturnCode REGARGS
AssignVar(struct Data *scr,
          struct Expr *val,
          struct Identifier *ident,
          long type) /* assign type */
{
  ReturnCode ret;
  long pos=0;
  Pass2 code;
  long dim;
  long dimensions=0;
  long *array=NULL;
  char multi=FALSE;

  long value;
  uchar *valuep;
  struct fplStr *string=NULL;

  scr->text += sizeof(long)*2; /* pass the information data */
  code = GETSHORT;
  if( PASS2_OPEN_BRACKET == code && ident->data.variable.num ) {
    /*
     * This is an array member assign!
     */
    GETMEM(array, ident->data.variable.num*sizeof(long));
    do {
      P_SHORT; /* pass open bracket */
      CALL(CmpExpr(val, scr, CON_GROUNDLVL|CON_NUM));
      P_SHORT; /* pass close bracket */
      if(val->val.val < 0)
        /* illegal result of the expression */
        return FPLERR_ILLEGAL_ARRAY;
      array[ dimensions++ ] = val->val.val;
      if(dimensions == ident->data.variable.num )
        /* we've hit the roof! */
        break;
    } while(PASS2_OPEN_BRACKET == GETSHORT);
    code = GETSHORT;
  }
  if(PASS2_OPEN_BRACE == code) {
    P_SHORT;
    dim=1; /* first dimension assign */
    multi=TRUE;
    if(!array) {
      /* then get an array! */
      GETMEM(array, ident->data.variable.num * sizeof(long));
      /* and clear it */
      memset(array, 0, ident->data.variable.num * sizeof(long) );
      /* set number of dimensions */
      dimensions = ident->data.variable.num;
    }
  }
  do {
    if(multi) {
      code = GETSHORT;
      switch(code) {
      case PASS2_OPEN_BRACE:
        ++dim;
        P_SHORT;
        continue;
      case PASS2_CLOSE_BRACE:
        --dim;
        P_SHORT;
        array[dim]=0; /* start over at zero at this dimension */
        continue;
      case PASS2_COMMA:
        array[ dim-1 ] ++;
        P_SHORT;
        continue;
      }
    }
    if(array) {
      pos = ArrayNum(dimensions, ident->data.variable.num,
                     array, ident->data.variable.dims);
      if( 0 > pos) {
        scr->buf[0]=0;
        return FPLERR_ILLEGAL_ARRAY;
      }
    }
    if(ident->flags&FPL_INT_VARIABLE) {
      CALL(CmpExpr(val, scr, CON_NORMAL));
      CALL(CmpAssign(scr, val->val.val,
                     &ident->data.variable.var.val32[pos],
                     ident->flags, type));
      val->val.val=ident->data.variable.var.val32[pos];
    }
    else {
      /*
       * String assigns
       */
      if(!multi &&
         PASS2_OPEN_BRACKET == code) {
        /* single character assign! */
        P_SHORT; /* pass open bracket */
        CALL(CmpExpr(val, scr, CON_GROUNDLVL|CON_NUM));
        P_SHORT; /* pass close bracket */
        if(!ident->data.variable.var.str[ pos ] ||
           !ident->data.variable.var.str[ pos ] ->len)
          /* no-length-string */
          return FPLERR_STRING_INDEX;
        if(val->val.val >= ident->data.variable.var.str[ pos ]->len)
          /* force to zero! */
          val->val.val=0;
        valuep = (uchar *)&ident->data.variable.var.str[ pos ]->string[val->val.val];
        value = *(uchar *)valuep;
        CALL(CmpExpr(val, scr, CON_NORMAL));
        CALL(CmpAssign(scr, val->val.val, &value, FPL_CHAR_VARIABLE, type));
        val->val.val= *valuep = (uchar)value; /* assign it for real! */
      }
      else {
        CALL( CmpExpr(val, scr, CON_STRING) );
        CALL( StrAssign(val->val.str, scr,
                        &ident->data.variable.var.str[pos],
                        CHAR_PLUS == type) ); /* TRUE or FALSE if append */
        string = ident->data.variable.var.str[pos];
      }
    }
    if(!multi)
      break;
      
    P_SHORT; /* pass END_OF_EXPR */

#if 0
    code = GETSHORT;
    
    P_SHORT; /* pass COMMA or CLOSE_BRACE */
#endif

  } while( dim > 0 ); /* repeat while we still assign first or more dims */

  if(string) {
    val->val.str=string;
    val->flags=FPL_STRING|FPL_NOFREE;
  }
  else
    val->flags=0;

  if(array)
    FREE(array); /* free temporary space */
  return FPL_OK;
}

static ReturnCode ReferToThis(struct Identifier **ident)
{
  if(!((*ident)->flags&FPL_REFERENCE))
    return FPLERR_ILLEGAL_REFERENCE; /* referenced a non-reference! */
  if(!(*ident)->data.variable.ref)
    return FPLERR_ILLEGAL_REFERENCE; /* illegal reference! */
  *ident = (*ident)->data.variable.ref; /* use the "actual" variable! */
  return FPL_OK;
}

ReturnCode REGARGS
CmpExpr(struct Expr *val, /* return value struct pointer */
        struct Data *scr, /* everything */
        long control)     /* ESPECIALLLY DEFINED */
{
  struct Expr *expr, *basexpr;
  ReturnCode ret;
  struct Identifier *ident; /* general purpose struct identifier pointer */
  long num;
  uchar *pnt;
  Pass2 code;
  uchar contentsof=FALSE;
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
    code = GETSHORT;
    P_SHORT; /* pass the instruction code */

    switch(code) {
      /*
       * Three cases of simple references.
       */
      case PASS2_REF_LOCAL_SYMBOL:
        ident = scr->localinfo.list[ GETLONG ];
        P_LONG;
        if(contentsof) {
          CALL(ReferToThis(&ident));
          contentsof=FALSE;
        }
        CALL(FixVariable(scr, ident, control, expr));
        CALL(NewMember(scr, &expr));
        break;
      case PASS2_REF_GLOBAL_SYMBOL:
        ident = scr->globalinfo->list[ GETLONG ];
        P_LONG;
        if(contentsof) {
          CALL(ReferToThis(&ident));
          contentsof=FALSE;
        }
        CALL(FixVariable(scr, ident, control, expr));
        CALL(NewMember(scr, &expr));
        break;
      case PASS2_REF_EXPORT_SYMBOL:
        pnt = (uchar *) &scr->prog->program[ GETLONG +
          scr->prog->index +
            sizeof(long)];  /* skip hash for now */
        CALL(GetIdentifier(scr, pnt, &ident));
        P_LONG;
        if(contentsof) {
          CALL(ReferToThis(&ident));
          contentsof=FALSE;
        }
        CALL(FixVariable(scr, ident, control, expr));
        CALL(NewMember(scr, &expr));
        break;

      /*
       * Three cases of assigns.
       */
      case PASS2_ASSIGN_LOCAL_SYMBOL:
        /* (varnum) (assign type);... local list */
        ident = scr->localinfo.list[ GETLONG ];
        if(contentsof) {
          CALL(ReferToThis(&ident));
          contentsof=FALSE;
        }
        CALL(AssignVar(scr, expr, ident, *(long *)(scr->text+sizeof(long)) ));
        CALL(NewMember(scr, &expr));
        P_SHORT; /* pass the end of expr code */
        break;
      case PASS2_ASSIGN_GLOBAL_SYMBOL:
        /* (varnum) (assign type);... global list */
        ident = scr->globalinfo->list[ GETLONG ];
        if(contentsof) {
          CALL(ReferToThis(&ident));
          contentsof=FALSE;
        }
        CALL(AssignVar(scr, expr, ident, *(long *)(scr->text+sizeof(long)) ));
        CALL(NewMember(scr, &expr));
        P_SHORT; /* pass the end of expr code */
        break;
      case PASS2_ASSIGN_EXPORT_SYMBOL:
        /* (assigntyp) (string-offset) */
        num = GETLONG; /* assign type */
        pnt = (uchar *)
          &scr->prog->program[ (*(long *)(scr->text+ sizeof(long)))+
                               scr->prog->index +
                               sizeof(long) ];  /* skip hash for now */
        CALL(GetIdentifier(scr, pnt, &ident));
        if(contentsof) {
          CALL(ReferToThis(&ident));
          contentsof=FALSE;
        }
        CALL(AssignVar(scr, expr, ident, num ));
        CALL(NewMember(scr, &expr));
        P_SHORT; /* pass the end of expr code */
        break;

      case PASS2_CALL_LOCAL_FUNCTION:
      case PASS2_CALL_INTERNAL_FUNCTION:
      case PASS2_CALL_EXPORT_FUNCTION:
        {
          struct Expr **exprp;
          GETMEM(exprp, sizeof(struct Expr *));
          *exprp = expr;
          CALL(FixFunction(scr, exprp, val, code, control));
          expr = *exprp;
          FREE(exprp);
        }
        break;

      case PASS2_CONTENTSOF:
        /*
         * This is the 'contents of' operator!
         */
        contentsof=TRUE;
        break;

      case PASS2_NUM_CONSTANT:
        expr->val.val=GETLONG;
        P_LONG;
        CALL(NewMember(scr, &expr));
        break;

      case PASS2_STRING_CONSTANT: /* OFFSET to <length> <string> */
        pnt = GETLONG + scr->prog->index + scr->prog->program;
        num = *(long *)pnt;
        GETMEM(expr->val.str, sizeof(struct fplStr)+ num);
        expr->val.str->alloc = expr->val.str->len = num;
        memcpy(expr->val.str->string,
               pnt+sizeof(long), num + 1); /* copy the zero termination too */
        expr->flags=FPL_STRING;
        P_LONG; /* pass the string offset */
        CALL(CmpStringExpr(expr, scr));
        break;
        
      case PASS2_OPEN_PAREN:
        CALL(CmpExpr(val, scr, CON_GROUNDLVL|CON_NUM));
        expr->val.val=val->val.val;
        CALL(NewMember(scr, &expr));
        P_SHORT; /* pass close paren */
        break;

      case PASS2_NOTOPERATOR:
        CALL(AddUnary(scr, expr, OP_NOT));
        break;

      case PASS2_ONCECOMPLEMENT:
        CALL(AddUnary(scr, expr, OP_COMPL));
        break;

      case PASS2_PREINC:
        CALL(AddUnary(scr, expr, OP_PREINC));
        break;

      case PASS2_PREDEC:
        CALL(AddUnary(scr, expr, OP_PREDEC));
        break;

      case PASS2_NEGATE:
        CALL(AddUnary(scr, expr, OP_MINUS));
        break;

      case PASS2_EQUAL:
        expr->operator=OP_EQUAL;
        break;
      case PASS2_LOGICAND:
        /*
         * This is a logical AND (&&)
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
        basexpr=expr;
        expr->val.val = val->val.val;

        if(!expr->val.val) {
          /*
           * In this case, its like in the 'a && b' expression and 'a'
           * equals 0. Then we should skip the 'b' expression.
           */
          scr->text =
            &scr->prog->program [ scr->prog->index + GETLONG ];
        }
        else
          P_LONG; /* pass index */
        break;

      case PASS2_BINARYAND:
        expr->operator=OP_BINAND;
        break;
      case PASS2_LOGICOR:
        /*
         * This is a logical OR operator (||)
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
        basexpr=expr;
        expr->val.val = val->val.val;

        if(expr->val.val) {
          /*
           * In this case, its like in the 'a || b' expression and 'a'
           * equals 1. Then we should skip the 'b' expression.
           */
          scr->text =
            &scr->prog->program [ scr->prog->index + GETLONG ];
        }
        else
          P_LONG; /* pass index */
        break;
      case PASS2_BINARYOR:
        expr->operator=OP_BINOR;
        break;
      case PASS2_PLUS:
        expr->operator=OP_PLUS;
        break;
      case PASS2_MINUS:
        expr->operator=OP_MINUS;
        break;
      case PASS2_CONDOPSTART:
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
          P_LONG; /* pass index */
          
          CALL(CmpExpr(val, scr, CON_NORMAL));
          /*
           * We're on a LABEL_GOTO right now!
           */
          P_SHORT; /* pass the instruction */
          /* goto the new position */
          scr->text =
            &scr->prog->program [ scr->prog->index + GETLONG ];
        }
        else {
          /*
           * In this case, its like in the 'a ? b : c' expression and 'a'
           * equals 0. Then we should skip the 'b' expression.
           */
           /* goto position */
          scr->text =
            &scr->prog->program [ scr->prog->index + GETLONG ];
          CALL(CmpExpr(val, scr, CON_NORMAL));
        }
        expr->val.val = val->val.val;
        break;
      case PASS2_MULTIPLY:
        expr->operator=OP_MULTIPLY;
        break;
      case PASS2_DIVISION:
        expr->operator=OP_DIVISION;
        break;
      case PASS2_REMAIN:
        expr->operator=OP_REMAIN;
        break;
      case PASS2_XOR:
        expr->operator=OP_BINXOR;
        break;
      case PASS2_LESSEQ:
        expr->operator=OP_LESSEQ;
        break;
      case PASS2_SHIFTLEFT:
        expr->operator=OP_SHIFTL;
        break;
      case PASS2_LESS:
        expr->operator=OP_LESS;
        break;
      case PASS2_GREATEQ:
        expr->operator= OP_GRETEQ;
        break;
      case PASS2_SHIFTRIGHT:
        expr->operator=OP_SHIFTR;
        break;
      case PASS2_GREATER:
        expr->operator=OP_GRET;
        break;
      case PASS2_NOTEQUAL:
        expr->operator=OP_NOTEQ;
        break;
      case PASS2_COMMA:
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
          break;
        }
        /* FALLS THROUGH */
      /* case PASS2_END_OF_EXPR: */
      default:
        scr->text -= sizeof(short); /* back on the instruction code */
        code = PASS2_END_OF_EXPR; /* force break-out-of-loop */
        break;
    }
#if 0
    if(expr->flags&FPL_STRING && !(control&CON_GROUNDLVL))
      /* get outta string calcs if not on ground level! */
      break;
#endif
  } while(PASS2_END_OF_EXPR != code);

  if(!(control&CON_NORETURN)) {
    /*
     * Get result of the current expression.
     */
    CALL(Calc(scr, val, basexpr));
  }
  Clean(scr, basexpr);    /* erase the rest of the list */
  return(FPL_OK);
}

ReturnCode REGARGS
GetArrayNum(struct Data *scr,
            struct Expr *expr,
            long *dims,
            struct Identifier *ident)
{
  long *array;
  ReturnCode ret;
  *dims=0;
  /*
   * This is an array reference!
   */
  GETMEM(array, ident->data.variable.num*sizeof(long));
  do {
    P_SHORT; /* pass open bracket */
    CALL(CmpExpr(expr, scr, CON_GROUNDLVL|CON_NUM));
    P_SHORT; /* pass close bracket */
    if(expr->val.val < 0)
      /* illegal result of the expression */
      return FPLERR_ILLEGAL_ARRAY;
    array[ (*dims)++ ] = expr->val.val;
    if(*dims == ident->data.variable.num )
      /* we've hit the roof! */
      break;
  } while(PASS2_OPEN_BRACKET == GETSHORT);
  *dims = ArrayNum(*dims, ident->data.variable.num, array,
                    ident->data.variable.dims);
  if( 0 > *dims)
    return FPLERR_ILLEGAL_ARRAY;
  FREE(array); /* free temporary space */
  
  return FPL_OK;
}

ReturnCode REGARGS
FixVariable(struct Data *scr,
            struct Identifier *ident,
            long control,
            struct Expr *expr)
{
  Pass2 code;
  long *array=NULL;
  long dims=0;
  ReturnCode ret;
  
  if(!ident)
    return FPLERR_IDENTIFIER_NOT_FOUND;

  code = GETSHORT;

  if(PASS2_RESIZE == code) {
    /*
     * Ooops! ;) This is a resize operation and not at all any
     * 'real' variable reference.
     */
    P_SHORT; /* pass resize instruction */
    GETMEM(array, MAX_DIMS*sizeof(long));
    do {
      P_SHORT; /* pass open bracket */
      CALL(CmpExpr(expr, scr, CON_GROUNDLVL|CON_NUM));
      P_SHORT; /* pass close bracket */
      array[ dims++ ] = expr->val.val;
    } while(PASS2_OPEN_BRACKET == GETSHORT);
    CALL(ArrayResize(scr, dims, array, ident));
    FREE(array); /* free temporary space */
    return FPL_OK;
  }

  if( PASS2_OPEN_BRACKET == code && ident->data.variable.num ) {
    /*
     * This is an array reference!
     */
    CALL(GetArrayNum(scr, expr, &dims, ident));
    code = GETSHORT;
  }

  if(ident->flags&FPL_STRING_VARIABLE) {
    if(PASS2_OPEN_BRACKET == code) {
      
      /*
       * Yet another bracket means this is a single-character access
       * from a string!
       */
       
      P_SHORT; /* pass open bracket */
      CALL(CmpExpr(expr, scr, CON_GROUNDLVL|CON_NUM));
      P_SHORT; /* pass close bracket */
      if(!ident->data.variable.var.str[ dims ] ||
         !ident->data.variable.var.str[ dims ] ->len)
        /* no-length-string */
        return FPLERR_STRING_INDEX;
      if(expr->val.val >= ident->data.variable.var.str[ dims ]->len)
        /* force to zero! */
        expr->val.val=0;

      expr->val.val =
        ident->data.variable.var.str[ dims ]->string[expr->val.val];
    }
    else {
      expr->val.str = ident->data.variable.var.str[ dims ];
      expr->flags |= FPL_NOFREE; /* don't free this! */
    }
  }
  else {
    struct Unary *un; /* Unary information struct pointer */
    long *value = &ident->data.variable.var.val32[ dims ];
    if(PASS2_POSTINC == code ) {
      expr->val.val=(*value)++;
      P_SHORT;
    }
    else if(PASS2_POSTDEC == code) {
      expr->val.val=(*value)--;
      P_SHORT;
    }
    else if(un=expr->unary) {
      if(un->unary!=OP_PREINC && un->unary!=OP_PREDEC) {
        expr->val.val=*value;
      } else {
        if(ident->flags&FPL_READONLY)
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
  return FPL_OK;
}

ReturnCode REGARGS
FixFunction(struct Data *scr,
            struct Expr **exprp,
            struct Expr *val, /* pass on struct pointer */
            Pass2 origcode,
            long control)
{
  struct fplArgument *pass; /* struct pointer to send as argument to
                               the function handler */
  struct fplArgument *arg2; /* backup pointer */
  ReturnCode ret;

  long numofargs; /* amount of arguments used in program */
  uchar *text; /* pointer to argument format string */
  uchar *run; /* pointer to new interpret position (local) */
  uchar *array;
  struct Expr *expr=*exprp;
  struct Identifier *ident;
  struct fplMsg *msg;
  struct CompiledInfo *comp;

  uchar hit;
  expr->flags|=FPL_OPERAND|FPL_ACTION; /* This sure is action...! */

  GETMEM(pass, sizeof(struct fplArgument));

  switch(origcode) {
    case PASS2_CALL_INTERNAL_FUNCTION:
      pass->name=NULL;
      pass->ID=GETLONG;
      break;
      
    case PASS2_CALL_LOCAL_FUNCTION:
      run = &scr->prog->program [ GETLONG + scr->prog->index ];
      /* 'run' points to the new interpret position */
      break;
      
    case PASS2_CALL_EXPORT_FUNCTION:
      /* this function is called and recognized by actual name */
      pass->name =
        &scr->prog->program [ GETLONG + scr->prog->index + sizeof(long)];
      break;
  }
  
  P_LONG; /* pass function ID or index or string pointer */
  
  P_SHORT; /* pass PASS2_TYPE_OF_ARGUMENTS */
  
  text = &scr->prog->program [ GETLONG + scr->prog->index ];
  /* 'text' points to a string now, that holds the length in the
     first 32 bits */
  numofargs = *(long *)text; /* thats the length */
  text += sizeof(long); /* now point to the actual zero terminated string */

  P_LONG; /* pass parameter string index */
  
  pass->argc=0;
  pass->key=(void *)scr;
  pass->format = text; /* already set and known */
  /*
   * FIX the other pass members to be set correctly too!
   */
  
  if(numofargs) {
    uchar a;

    /* if the function takes arguments */

    /*
     * Allocate arrays to use for data storage while parsing
     * the arguments.
     */

    /* allocate an array */
    GETMEM(pass->argv, sizeof(uchar *)* (numofargs+1) );

    /* allocate allocate-flag string */
    GETMEM(array, sizeof(uchar)* (numofargs+1) );

    do {
      a=*text;

      switch(a) {
      case FPL_STRARG:
        CALL(CmpExpr(val, scr, CON_STRING));
        CALL(CmpStringExpr(val, scr)); /* get more strings? */

        if(val->val.str) {
          /* Set this to TRUE if deallocation is wanted on this
             string after the function call! */
          array[pass->argc]=!(val->flags&FPL_NOFREE);
          /*
           * Point to the string (that is zero terminated)!
           */
          pass->argv[pass->argc]=val->val.str->string;
        } else {
          register struct fplStr *string;
          GETMEM(string, sizeof(struct fplStr));
          memset(string, 0, sizeof(struct fplStr));
          pass->argv[pass->argc]=string->string;
          array [ pass->argc ] = TRUE; /* allocation has been done! */
        }
        pass->argc++;
        break;
      case FPL_INTARG:
        CALL(CmpExpr(val, scr, CON_NUM));
        pass->argv[pass->argc++]=(void *)val->val.val;
        break;
      case FPL_STRVARARG:
      case FPL_INTVARARG:
      case FPL_INTARRAYVARARG:
      case FPL_STRARRAYVARARG:
        {
          register ReturnCode ok;
          register Pass2 code;
          if(GETSHORT != PASS2_VARIABLE_REFERENCE) {
            ok = FPLERR_ILLEGAL_REFERENCE;
          }
          else {
            ok = FPL_OK;
            P_SHORT; /* pass that one */
          }
          
          /* Get identifier */
          
          code = GETSHORT;
          P_SHORT; 
          switch(code) {
          case PASS2_REF_LOCAL_SYMBOL:
            ident = scr->localinfo.list[ GETLONG ];
            break;
          case PASS2_REF_GLOBAL_SYMBOL:
            ident = scr->globalinfo->list[ GETLONG ];
            break;
          case PASS2_REF_EXPORT_SYMBOL:
            {
              register char *pnt;
              pnt = (uchar *) &scr->prog->program[ GETLONG +
                scr->prog->index + sizeof(long)];  /* skip hash for now */
              CALL(GetIdentifier(scr, pnt, &ident));
            }
            break;
          }
          P_LONG; /* pass data */

          if(ok) {
            /* missing contensof-operator! */
            if(ident->flags&FPL_REFERENCE)
              /* get the referenced variable instead! */
              ident = ident->data.variable.ref;
            else
              return ok; /* no reference! */
          }
        }

        if(FPL_INTARRAYVARARG == a || FPL_STRARRAYVARARG == a) {
          if(!ident->data.variable.num)
            return FPLERR_ILLEGAL_REFERENCE;
        }
        else if(FPL_OPTVARARG != a && ident->data.variable.num)
          /* only straight variables! */
          return FPLERR_ILLEGAL_PARAMETER;

        if( (ident->flags&FPL_INT_VARIABLE &&
             (a==FPL_STRVARARG || a == FPL_STRARRAYVARARG)) ||
           (ident->flags&FPL_STRING_VARIABLE &&
            (a==FPL_INTVARARG || a == FPL_INTARRAYVARARG))) {
          return FPLERR_ILLEGAL_VARIABLE;
        } else
          pass->argv[pass->argc]=(void *)ident;
        break;
      }
      P_SHORT; /* pass the COMMA or CLOSE_PAREN */
    } while (*++text);
  }
  else
    P_SHORT; /* pass the closing paren */

  /*
   * Call the function!
   */

  if(PASS2_CALL_INTERNAL_FUNCTION == origcode) {
    CALL(functions(pass));
  }
  else {
    /*
     * Allocate temporary storage for our local symbols.
     */
    GETMEM(comp, sizeof(struct CompiledInfo));
    memcpy(comp, &scr->localinfo, sizeof(struct CompiledInfo)); /* copy */
    /*
     * Clear the items to enforce a new allocated list
     */
    scr->localinfo.listentries = scr->localinfo.listsize =0;
      
    if(PASS2_CALL_LOCAL_FUNCTION == origcode) {

      arg2 = scr->arg; /* store the old */
      scr->arg = pass; /* for compiled functions */
      
      text = scr->text; /* store current interpret position */
      scr->text = run; /* set interpret point to local function index */
      /*
       * Recurse this at the new position.
       */
      CALL(Script(scr, val, SCR_BRACE|SCR_FUNCTION, NULL));
      scr->text = text; /* restore previous execute point */
      scr->arg = arg2;  /* restore previous argument pointer */
    }
    else {
      /* EXPORTED FUNCTION */
      ret=GetIdentifier(scr, pass->name, &ident);
      if(ret) {
        /* copy the variable name to make a decent error */
        strcpy(scr->buf, pass->name);
        return ret;
      }
      pass->ID=ident->data.external.ID; /* set ID */
      CALL(CallFunction(scr, pass, ident));
    }
    
    /*
     * Free the previous local variables and get back our old
     */
    if(scr->localinfo.listsize) {
      /* There is an allocated one here */
      FREE(scr->localinfo.list);
    }
    memcpy(&scr->localinfo, comp,
           sizeof(struct CompiledInfo)); /* copy */
    FREE(comp);
  }

  CALL(GetMessage(scr, FPLMSG_RETURN, &msg));
  if(control & CON_NUM)
    hit = FPL_INTARG;
  else if(control & CON_STRING)
    hit = FPL_STRARG;
  else {
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
    else
      hit = FPL_INTARG;
    /* There is no return nor hint! */
  }

  switch(hit) {
    case FPL_STRARG:
      if(msg && ((msg->flags&FPLMSG_FLG_BITS) != FPLMSG_FLG_STRING))
        return FPLERR_UNEXPECTED_INT_STATEMENT;
      if(!msg || !msg->message[0])
        /* We got a zero length string or no string at all! */
        expr->val.str=NULL; /* no string! */
      else
        /* the copied string! */
        expr->val.str=(struct fplStr *)msg->message[0];
      expr->flags=FPL_STRING|FPL_ACTION;
      break;
    case FPL_INTARG:
    default:
      if(msg && ((msg->flags&FPLMSG_FLG_BITS) != FPLMSG_FLG_INT))
        return FPLERR_UNEXPECTED_STRING_STATEMENT;
      /* only if integer! or the function is non-existent */
      expr->val.val=(msg?(long)msg->message[0]:0);
      CALL(NewMember(scr, exprp));
      break;
  }
  if(msg)
    DeleteMessage(scr, msg);

  while(pass->argc--) {
    if(pass->format[pass->argc]==FPL_STRARG && array[pass->argc]) {
      /* free the string if it's been marked to be freed!! */
      FREE((uchar *)pass->argv[pass->argc]-
           offsetof(struct fplStr, string));
    }
  }
  if(numofargs) {
    FREE(pass->argv);
    FREE(array);
  }
  FREE(pass);
  return FPL_OK;
}

ReturnCode REGARGS AssignArg(struct Data *scr)
{
  long varnum;
  long argnum;
  struct Identifier *ident;
  struct fplVariable *tempvar;
  
  varnum = GETLONG;
  P_LONG;
  argnum = GETLONG;
  P_LONG;
  
  ident = scr->localinfo.list[ varnum ]; /* the local variable */
  tempvar=&ident->data.variable;
  
  if(ident->flags & FPL_REFERENCE)
    ident->data.variable.ref = (struct Identifier *)scr->arg->argv[ argnum ];
  else if(ident->flags & FPL_STRING_VARIABLE) {
    /* Store string length in variable `len' */
    register long len=GETSTRLEN(scr->arg->argv[ argnum ]);
    GETMEM(tempvar->var.str[0], sizeof(struct fplStr)+len);
    tempvar->var.str[0]->alloc=len;

    /* We copy the ending zero termination too! */
    memcpy(tempvar->var.str[0]->string,
           ((uchar *)scr->arg->argv[ argnum ]),
           len+1);
    tempvar->var.str[0]->len=len;
  }
  else {
    /* Integer assign */
    tempvar->var.val32[0]=(long)scr->arg->argv[ argnum ];
  }
  return FPL_OK;
}

ReturnCode REGARGS CmpSwitch(struct Data *scr,
                             struct Expr *val)
{
  ReturnCode ret;
  struct fplStr *string;
  long value;
  long index; /* current index information */
  char wasstring=FALSE;
  char jump=FALSE;

  /* Get expression, string or int, static or dynamic! */
  CALL(CmpExpr(val, scr, CON_NORMAL));
  if(val->flags&FPL_STRING) {
    /* string statement! */
    string = val->val.str;
    wasstring=TRUE;
  }
  else {
    /* integer expression */
    value = val->val.val;
  }
  P_SHORT; /* pass the END_OF_EXPR mark */
  do {
    P_SHORT; /* pass the CASE mark */
    index = GETLONG;
    P_LONG;  /* pass the index */
  
    /* Get expression, string or int! */
    CALL(CmpExpr(val, scr, wasstring?CON_STRING:CON_NUM));
    if(wasstring) {
      /*
       * String comparison:
       */
      value = val->val.str?val->val.str->len:0; /* get length */
  
      if(value == (string?string->len:0)) { /* compare lengts */
        if(value) {
          if(!memcmp(val->val.str->string, string->string, value)) {
            /* match! */
            jump=TRUE;
          }
        } else
          jump=TRUE;
      }
      if(!val->flags&FPL_NOFREE)
        FREE(val->val.str);
    }
    else {
      /*
       * Integer comparison:
       */
      if(val->val.val == value)
        jump = TRUE; /* match */
    }
    if(jump) {
      /* goto index */
      scr->text = &scr->prog->program[scr->prog->index + index];
      break; /* we're done! */
    }
    P_SHORT; /* pass the END_OF_EXPR */
  } while(PASS2_CASE == GETSHORT);
  
  return FPL_OK;
}

ReturnCode REGARGS CmpBreak(struct Data *scr,
                            struct Expr *val)
{
  ReturnCode ret;

  /* Get integer expression */
  CALL(CmpExpr(val, scr, CON_NUM));
  
  P_SHORT; /* pass END_OF_EXPR */
  
  if(val->val.val<=0)
    return FPLERR_ILLEGAL_BREAK;
    
  while(--val->val.val && PASS2_LABEL_GOTO == GETSHORT)
    scr->text += sizeof(short)+sizeof(long);

  if(PASS2_END_OF_EXPR == GETSHORT) {
    P_SHORT; /* just pass it and act cool! */
  }
  return FPL_OK; /* leave this standing on the goto! */
}
#endif
