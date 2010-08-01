/******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************

 Pass3.c

 Compiling functions

 *****************************************************************************/

/************************************************************************
 *                                                                      *
 * fpl.library - A shared library interpreting script langauge.         *
 * Copyright (C) 1992-1995 FrexxWare                                    *
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
#include <libraries/dos.h>
#include <proto/dos.h>

#include <exec/libraries.h>
#include <dos.h>

#elif defined(UNIX)
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include "script.h"

#include "pass2.h"
#include "pass3.h"

extern struct Pass2Data pass2_first_data;

struct Pass2Data pass3_first_data;
struct Pass2Data *pass3_last_data=&pass3_first_data;
struct Pass2Data pass3_current_data;

struct Pass3String pass3_first_string;
struct Pass3String *pass3_last_string=&pass3_first_string;

long final_offset_count=0;
long string_offset_count=0;

long *var_global_number;
long *var_local_number;

struct Pass3LabelRef *label_list;
struct Pass3LabelRef *continue_list;
struct Pass3LabelRef *break_list;
struct Pass3LabelRef *function_list;

extern long labelcount; /* Number of labels (from pass2) */
extern long total_number_of_variables; /* Number of variables (from pass2) */
extern long total_number_of_functions;
extern long max_level; /* Number of levels (from pass2) */

char link_flags=0;

#ifdef DEBUG_OUTPUT
static void Describe(struct Data *scr, struct Pass2Data *data)
{
  char *s[]={
  "NOTHING",
  "DECLARE",

  "REF_LOCAL_SYMBOL",
  "REF_GLOBAL_SYMBOL",
  "REF_EXPORT_SYMBOL",

  "POSTINC",
  "POSTDEC",
  "PREINC",
  "PREDEC",
  "CONTENTSOF",
  "NUM_CONSTANT",
  "STRING_CONSTANT",
  "OPEN_PAREN",
  "CLOSE_PAREN",
  "ONCECOMPLEMENT",
  "NOTOPERATOR",
  "NEGATE",

  "COMMA",
  "NOTEQUAL",
  "GREATER",
  "SHIFTRIGHT",
  "GREATEQ",
  "LESS",
  "SHIFTLEFT",
  "LESSEQ",
  "XOR",
  "REMAIN",
  "DIVISION",
  "MULTIPLY",
  "CONDOPEND",
  "CONDOPSTART",
  "MINUS",
  "PLUS",
  "BINARYOR",
  "BINARYAND",
  "LOGICOR",
  "LOGICAND",
  "EQUAL",
  "STRING_APPEND",

  "END_OF_EXPR",

  "ASSIGN_LOCAL_SYMBOL",  /* [var num] [assign type] */
  "ASSIGN_GLOBAL_SYMBOL",  /* [var num] [assign type] */
  "ASSIGN_EXPORT_SYMBOL", /* [hash num] [assign type] [name] */

  "LABEL_GOTO",	/* [number] */

  "IFNOT_BRANCH",	/* if branch */
  "IF_BRANCH",	/* if branch */

  "CALL_LOCAL_FUNCTION",
  "CALL_EXPORT_FUNCTION",
  "CALL_INTERNAL_FUNCTION",

  "SWITCH",
  "CASE",	/* [label number] */

  "BREAK_EXPR", /* Followed by an expression and several GOTO_BREAK and END_OF_EXPR */
  "RETURN",

  "RESIZE",
  "OPEN_BRACKET",
  "CLOSE_BRACKET",

  "ASSIGN_ARGUMENT", /* [var number], [arg number] */

  "EXPORT_FUNCTION", /* [type], [number], [offset] */

  "RESET_VARIABLE", /* [number] */
  "OPEN_BRACE",
  "CLOSE_BRACE",

  "EXIT", /* expression */
  "VARIABLE_REFERENCE",

  "TYPE_OF_ARGUMENTS", /* [string] */

  "JUMP_OVER_ELSE",	/* jump over olse */

  "MAIN_START",
  "MAIN_END",

  "IFNOT_BRANCH_BREAK",/* if not branch to break */
  "IFNOT_BRANCH_ELSE",/* if not branch to else */
  "LOCAL_FUNCTION", /* [type], [number], [offset] */
  "LABEL",
  "LABEL_BEGIN",	/* [number] */
  "LABEL_CONTINUE",	/* [level] */
  "LABEL_BREAK",	/* [level] */
  "GOTO_BEGIN",	/* [number] */
  "GOTO_BREAK",	/* [level] */
  "LABEL_ELSE",		/* else [number]*/
  "END_OF_ELSE",	/* else [number]*/

  "FUNCTION_START", /* [function number] */
  "CONTINUE", /* level */
  "AMOUNT_VARIABLES",
  "END_OF_FUNCTION",

  "LINE_NUMBER",

  };
  if(data->code>sizeof(s)/sizeof(char *))
    printf("UNKNOWN\n");
  else {
    printf("%04x %2d [%02x] %-20s", data->final_offset, data->final_size, data->code, s[data->code]);
    if (data->flags&P2D_FLAG_NUM0)
      printf(" [%08x]", data->num[0]);
    if (data->flags&P2D_FLAG_NUM1)
      printf(" [%08x]", data->num[1]);
    if (data->flags&P2D_FLAG_NUM2)
      printf(" [%08x]", data->num[2]);
    if (data->flags&P2D_FLAG_STRING) {
      if (data->pass3_string_offset[0])
        printf(" [%08x]", data->pass3_string_offset[0]);
    }
    if (data->flags&P2D_FLAG_STRING2) {
      if (data->pass3_string_offset[1])
        printf(" [%08x]", data->pass3_string_offset[1]);
    }
    if (data->flags&P2D_FLAG_STRING)
      printf(" (%.*s) (%04x) (%d)", data->strlen[0], data->string[0], data->pass3_string_offset[0], data->strlen[0]);
    if (data->flags&P2D_FLAG_STRING2)
      printf(" (%.*s) (%04x) (%d)", data->strlen[1], data->string[1], data->pass3_string_offset[1], data->strlen[1]);
    printf("\n");
  }
}
#endif
ReturnCode Pass3Open(struct Data *scr, char *file)
{
#ifdef DEBUG_OUTPUT
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS3)
    printf("\n-------------------------------\nPASS3 START\n-------------------------------\n");
#endif
  max_level+=2; /* for safety */
  GETMEM(label_list, sizeof(struct Pass3LabelRef)*(labelcount+1));
  memset(label_list, 0, sizeof(struct Pass3LabelRef)*(labelcount+1));
  GETMEM(continue_list, sizeof(struct Pass3LabelRef)*(max_level+1));
  memset(continue_list, 0, sizeof(struct Pass3LabelRef)*(max_level+1));
  GETMEM(break_list, sizeof(struct Pass3LabelRef)*(max_level+1));
  memset(break_list, 0, sizeof(struct Pass3LabelRef)*(max_level+1));
  GETMEM(function_list, sizeof(struct Pass3LabelRef)*(total_number_of_functions+1));
  memset(function_list, 0, sizeof(struct Pass3LabelRef)*(total_number_of_functions+1));
  GETMEM(var_global_number, sizeof(long)*(total_number_of_variables+1));
  memset(var_global_number, 0, sizeof(long)*(total_number_of_variables+1));
  GETMEM(var_local_number, sizeof(long)*(total_number_of_variables+1));
  memset(var_local_number, 0, sizeof(long)*(total_number_of_variables+1));

  memset(&pass3_first_data, 0, sizeof(struct Pass2Data));
  pass3_last_data=&pass3_first_data;
  memset(&pass3_current_data, 0, sizeof(struct Pass2Data));

  memset(&pass3_first_string, 0, sizeof(struct Pass3String));
  pass3_last_string=&pass3_first_string;

  final_offset_count=0;
  string_offset_count=0;

  return FPL_OK;
}

void Pass3CheckSystem()
{
  long test=1;
/*  if (check commandline option) */

  link_flags |= FLAG_WORD_ALIGNED;

  if (*(char *)&test==1)
    link_flags |= FLAG_LOW_BYTE_FIRST;
}

ReturnCode Pass3PushPut(struct Data *scr, struct Pass2Data *pass2_data)
{
  struct Pass2Data *new;

  {
    long size=sizeof(struct Pass2Data);
    if (!pass2_data->flags)
      size=offsetof(struct Pass2Data, num);  /* OBS: The structure is allocated shorter now */
    GETMEM(new, size);
    memcpy(new, pass2_data, size);
  }
  pass3_last_data->next=new;
  new->prev=pass3_last_data;
  pass3_last_data=new;
  new->final_offset=final_offset_count;
  final_offset_count+=new->final_size*2;
#ifdef DEBUG_OUTPUT
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS3) {
    Describe(scr, new);
  }
#endif

  return FPL_OK;
}

ReturnCode Pass3PushLineNumber(struct Data *scr, long line)
{
  struct Pass2Data *new;

  GETMEM(new, sizeof(struct Pass2Data));
  memset(new, 0, sizeof(struct Pass2Data));

  new->code=PASS2_LINE_NUMBER;
  new->line=line;
  new->num[0]=line;
  new->flags|=P2D_FLAG_NUM0;
  new->final_size=3;

  pass3_last_data->next=new;
  new->prev=pass3_last_data;
  pass3_last_data=new;
  new->final_offset=final_offset_count;
  final_offset_count+=new->final_size*2;
#ifdef DEBUG_OUTPUT
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_PASS3) {
    Describe(scr, new);
  }
#endif
  return FPL_OK;
}

void write_num(char *dest, long num, long size)
{
  long count;
  if (link_flags&FLAG_LOW_BYTE_FIRST) {
    for (count=0; count<size; count++) {
      dest[count]=num&255;
      num>>=8;
    }
  } else {
    for (count=size-1; count>=0; count--) {
      dest[count]=num&255;
      num>>=8;
    }
  }
}

void PutNumHunk(char *hunk_name, long num, FILE *fp)
{
  char tempbuffer[sizeof(long)];
  fwrite(hunk_name, 1, 4, fp);
  write_num(tempbuffer, sizeof(long), sizeof(long)); /* Put size of hunk */
  fwrite(tempbuffer, 1, sizeof(long), fp);
  write_num(tempbuffer, num, sizeof(long)); /* Put data of hunk */
  fwrite(tempbuffer, 1, sizeof(long), fp);
}

void PutInteger(long num, FILE *fp)
{
  char tempbuffer[sizeof(long)];
  write_num(tempbuffer, num, sizeof(long)); /* Put data of hunk */
  fwrite(tempbuffer, 1, sizeof(long), fp);
}

void PutStringHunk(char *hunk_name, char *string, FILE *fp)
{
  char tempbuffer[sizeof(long)];
  long len=strlen(string);

  len+=3-((len+1)%3);
  fwrite(hunk_name, 1, 4, fp);
  write_num(tempbuffer, len, sizeof(long)); /* Put size of hunk */
  fwrite(tempbuffer, 1, sizeof(long), fp);
  fwrite(string, 1, len, fp);
}

ReturnCode Pass3Close(struct Data *scr, char *file, char *origfile)
{
  struct Pass2Data *count;
  FILE *fp;
  char *dest;
  char *destcount;
  char *filename_buffer;

  final_offset_count+=sizeof(long); /* append size of length */
  GETMEM(dest, final_offset_count);
  destcount=dest;

  count=&pass3_first_data;
#ifdef DEBUG_OUTPUT
  if (scr->cmdline[LINE_VERBOSE]&VERBOSE_FINAL)
    printf("\nRESULT PASS3\n");
#endif
  while (count=count->next) {
    if (count->final_size) { /* ignore if no size */
      long size=sizeof(short);

#ifdef DEBUG_OUTPUT
      if (scr->cmdline[LINE_VERBOSE]&VERBOSE_FINAL) {
        Describe(scr, count);
      }
#endif
      write_num(destcount, (long)count->code, sizeof(short));
      destcount+=sizeof(short);
      if (count->flags) {
        /* If no 'flags' then the structure is shorter and can't be read */
        if (count->flags&P2D_FLAG_NUM0) {
          write_num(destcount, count->num[0], sizeof(long));
          destcount+=sizeof(long);
          size+=sizeof(long);
        }
        if (count->flags&P2D_FLAG_NUM1) {
          write_num(destcount, count->num[1], sizeof(long));
          destcount+=sizeof(long);
          size+=sizeof(long);
        }
        if (count->flags&P2D_FLAG_NUM2) {
          write_num(destcount, count->num[2], sizeof(long));
          destcount+=sizeof(long);
          size+=sizeof(long);
        }
        if (count->pass3_string_offset[0]) {
          write_num(destcount, count->pass3_string_offset[0], sizeof(long));
          destcount+=sizeof(long);
          size+=sizeof(long);
        }
        if (count->pass3_string_offset[1]) {
          write_num(destcount, count->pass3_string_offset[1], sizeof(long));
          destcount+=sizeof(long);
          size+=sizeof(long);
        }
      }
      if (size!=count->final_size*2)
        printf("ERROR\n");
      if (destcount>dest+final_offset_count) {
        printf("Overflow error!\n");
        exit(0);
      }
    }
  }
  {
    struct Pass3String *count;
    count=&pass3_first_string;
    while (count=count->next) {
      if (count->strlen<0) {
        write_num(destcount, count->hash, sizeof(long));
      } else {
        write_num(destcount, count->strlen, sizeof(long));
      }
      destcount+=sizeof(long);
      memcpy(destcount, count->string, count->total_length);
      destcount+=count->total_length;
#ifdef DEBUG_OUTPUT
      if (scr->cmdline[LINE_VERBOSE]&VERBOSE_FINAL) {
        printf("%04x (%d) \"%.*s\"\n", count->offset, count->strlen, count->strlen, count->string);
      }
#endif
      if (destcount>dest+final_offset_count) {
        printf("Overflow2 error!\n");
        exit(0);
      }
    }
  }
  {
    long len = strlen(file);
    if(
#ifdef AMIGA
       (':' == file[len-1]) ||
#endif
       ('/' == file[len-1]) ) {
      long len2=strlen(origfile)-1;
      while(len2>=0) {
        if('/' == origfile[len2]
#ifdef AMIGA
           || ':' == origfile[len2]
#endif
           ) {
          origfile = &origfile[++len2];
          break;
        }
        --len2;
      }
      GETMEM(filename_buffer, len+strlen(origfile)+5);
      strcpy(filename_buffer, file);
      strcat(filename_buffer, origfile);
      file = filename_buffer;
    }
    else {
     GETMEM(filename_buffer, len+5);
     strcpy(filename_buffer, file);
    }
  }
  {
    long stringlen=strlen(file);
    if (stringlen >3) {
      if(!stricmp(&filename_buffer[stringlen-4], ".fpl")) {
        if (isupper(filename_buffer[stringlen-1]))
          filename_buffer[stringlen-1]='C';
        else
          filename_buffer[stringlen-1]='c';
      }
      else if (stricmp(&filename_buffer[stringlen-4], ".fpc")) {
        strcat(filename_buffer, ".FPC");
      }
    }
  }
		/* Open file, and write hunks */
  fp=fopen(filename_buffer, "w");
  if (!fp)
    return FPLERR_OPEN_ERROR;
  fwrite(HUNK_HEADER, 1, strlen(HUNK_HEADER), fp);
  fwrite(&link_flags, 1, 1, fp);

  PutNumHunk(HUNK_VERSION, VERSION_NUM, fp);
  PutNumHunk(HUNK_REQUIRE_FPL, REQUIRE_FPL, fp);
  PutStringHunk(HUNK_FILE, origfile, fp);

  fwrite(HUNK_CODE, 1, strlen(HUNK_CODE), fp);
  PutInteger(final_offset_count, fp); /* Put size of hunk */
  fwrite(dest, 1, final_offset_count, fp);

  /*
    Construct symbol hunk
  */
  {
    struct Pass3String *count;
    long size=0;
    count=&pass3_first_string;
    while (count=count->next) {
      if (count->type & (STRING_IS_FUNCTION|STRING_IS_VARIABLE))
        size+=sizeof(long)*2;
    }
    destcount=dest;
    count=&pass3_first_string;
    while (count=count->next) {
      if (count->type & (STRING_IS_FUNCTION|STRING_IS_VARIABLE)) {
        write_num(destcount, count->type, sizeof(long)); /* Put type */
        destcount+=sizeof(long);
        write_num(destcount, count->offset, sizeof(long)); /* Put offset */
        destcount+=sizeof(long);
#ifdef DEBUG_OUTPUT
      if (scr->cmdline[LINE_VERBOSE]&VERBOSE_SYMBOL) {
        if (count->type & STRING_IS_DECLARED)
          printf("Declare ");
        else
          printf("Reference ");
        if (count->type & STRING_IS_FUNCTION)
          printf("function");
        else
          printf("variable");
        printf(":%.*s\n", count->strlen, count->string);
      }
#endif
      }
    }
    fwrite(HUNK_SYMBOLS, 1, strlen(HUNK_SYMBOLS), fp);
    PutInteger(destcount-dest, fp);  /* Put size of hunk */
    fwrite(dest, 1, destcount-dest, fp);
  }

  fclose(fp);
  return FPL_OK;
}


ReturnCode Pass3Put(struct Data *scr, Pass2 code, struct Pass2Data *pass2_data,
                    char size)
{
  ReturnCode ret;

  pass3_current_data.code=code;
  pass3_current_data.final_size=size;
  if (pass3_current_data.code!=PASS2_NOTHING) {
    CALL(Pass3PushPut(scr, &pass3_current_data));
  }
  memset(&pass3_current_data, 0, sizeof(pass3_current_data));
  pass3_current_data.code=PASS2_NOTHING;
  pass3_current_data.level=0;
  pass3_current_data.line=0;
  pass3_current_data.row=0;
  pass3_current_data.next=NULL;
  return FPL_OK;
}

ReturnCode Pass3PutArg(struct Data *scr, long arg, struct Pass2Data *pass2_data)
{
  if (!(pass2_data->flags&P2D_FLAG_NUM0)) {
    pass2_data->num[0]=arg;
    pass2_data->flags|=P2D_FLAG_NUM0;
  } else if (!(pass2_data->flags&P2D_FLAG_NUM1)) {
    pass2_data->num[1]=arg;
    pass2_data->flags|=P2D_FLAG_NUM1;
  } else if (!(pass2_data->flags&P2D_FLAG_NUM2)) {
    pass2_data->num[2]=arg;
    pass2_data->flags|=P2D_FLAG_NUM2;
  }
  return FPL_OK;
}


ReturnCode Pass3PutLabelRef(struct Data *scr, struct Pass3LabelRef *label_list,
                            long number, long *address)
{
  struct Pass3LabelRef *new;

  GETMEM(new, sizeof(struct Pass3LabelRef));
  memset(new, 0, sizeof(struct Pass3LabelRef));
  new->label_store=address;
  new->next=label_list[number].next;
  label_list[number].next=new;
  return FPL_OK;
}

long Pass3FindLocalIdentifier(char *name)
{
  struct Pass2Data *count=pass2_first_data.next;
  while (count) {
    if (count->flags&P2D_FLAG_STRING) {
      if (count->code==PASS2_FUNCTION_START) { /* Function */
        if (!(count->num[0]&FPL_EXPORT_SYMBOL)) {
          if (!strcmp(count->string[0], name))
            return count->num[0];
        }
      }
      if (count->code==PASS2_DECLARE) { /* Variable */
        if ((count->num[0]&FPL_GLOBAL_SYMBOL) &&
            !(count->num[0]&FPL_EXPORT_SYMBOL)) {
          if (!strcmp(count->string[0], name))
            return var_global_number[count->num[1]];
        }
      }
    }
    count=count->next;
  }
  return -1;
}


ReturnCode Pass3ExtractLabelRef(struct Data *scr, struct Pass3LabelRef *label_list,
                                long number)
{
  struct Pass3LabelRef *count;
  struct Pass3LabelRef *next;

  count=label_list[number].next;
  if (count) {
    if (!label_list[number].label_offset) {
      printf("Label error: %ld\n", number);
      return FPLERR_IDENTIFIER_NOT_FOUND;
    }
    while (count) {
      *(count->label_store)=label_list[number].label_offset;
      next=count->next;
      FREE(count);
      count=next;
    }
  }
  label_list[number].label_offset=0;
  label_list[number].next=NULL;
  return FPL_OK;
}

ReturnCode Pass3PutString(struct Data *scr, struct Pass2Data *pass2_data,
                          long hash, char ishash, short num, char type)
{
  ReturnCode ret;
  struct Pass3String *new=NULL;
  struct Pass3String *count;

  count=pass3_first_string.next;
  while (count) {
    if (ishash) {
      if (hash==count->hash &&
          !strcmp(pass2_data->string[num], count->string)) {
        new=count;
        break;
      }
    } else {
      if (pass2_data->strlen[num]==count->strlen &&
          !memcmp(pass2_data->string[num], count->string, count->strlen)) {
        new=count;
        break;
      }
    }
    count=count->next;
  }
  if (!new) {
    pass2_data->pass3_string_offset[num]=string_offset_count;

    GETMEM(new, sizeof(struct Pass3String));
    memset(new, 0, sizeof(struct Pass3String));
  
    new->offset=string_offset_count;
    new->total_length=pass2_data->strlen[num]+1;
    if (link_flags&FLAG_WORD_ALIGNED) {
      new->total_length=(new->total_length+1)&~1;
    }
    string_offset_count+=new->total_length;
    new->string=pass2_data->string[num];
    new->type=type;
    if (ishash) {
      new->strlen=-1;
      new->hash=hash;
    } else
      new->strlen=pass2_data->strlen[num];
    pass3_last_string->next=new;
    pass3_last_string=new;
  } else {
    new->type|=type;
    pass2_data->pass3_string_offset[num]=new->offset;
  }
  CALL(Pass3PutLabelRef(scr, &new->references, 0, &pass2_data->pass3_string_offset[num]));
  return FPL_OK;
}

ReturnCode Pass3Examine(struct Data *scr, struct Pass2Data *pass2_data)
{
  ReturnCode ret;
  long local_varcount=0;
  long global_varcount=0;
  long declare_hit=0;
  long line_number=0;

  while (pass2_data) {
    if (scr->cmdline[LINE_DEBUG]&DEBUG_LINE) {
      if (line_number!=pass2_data->line) {
        line_number=pass2_data->line;
        CALL(Pass3PushLineNumber(scr, line_number));
      }
    }
    switch (pass2_data->code) {
    case PASS2_DECLARE:
      if (pass2_data->num[0]&FPL_EXPORT_SYMBOL) {
        pass2_data->flags&=~P2D_FLAG_NUM1; /* remove num */
        pass2_data->final_size=5;
        CALL(Pass3PushPut(scr, pass2_data));
        CALL(Pass3PutString(scr, pass3_last_data, pass2_data->num[1], TRUE, 0, STRING_IS_VARIABLE|STRING_IS_DECLARED));
        pass2_data=pass2_data->next;
      } else {
        long start_type=pass2_data->num[0];
        long start_num=local_varcount;
        if (start_type&FPL_GLOBAL_SYMBOL)
          start_num=global_varcount;
        while (pass2_data->code==PASS2_DECLARE &&
               pass2_data->num[0]==start_type) {
          if (start_type&FPL_GLOBAL_SYMBOL) {
            var_global_number[pass2_data->num[1]]=global_varcount;
            global_varcount++;
          } else {
            var_local_number[pass2_data->num[1]]=local_varcount;
            local_varcount++;
          }
          pass2_data=pass2_data->next;
        }
        CALL(Pass3PutArg(scr, start_type, &pass3_current_data)); /* Push type */
        CALL(Pass3PutArg(scr, start_num, &pass3_current_data)); /* Push start number */
        if (start_type&FPL_GLOBAL_SYMBOL) {
          CALL(Pass3PutArg(scr, global_varcount-start_num, &pass3_current_data)); /* Push number of variables*/
        } else {
          CALL(Pass3PutArg(scr, local_varcount-start_num, &pass3_current_data)); /* Push number of variables*/
        }
        CALL(Pass3Put(scr, PASS2_DECLARE, &pass3_current_data, 7));
        if (declare_hit) { /* if we have a former declare, lets swap them 
                              in order to get the declare with the higher
                              numbers first. */
          pass3_last_data->next=pass3_last_data->prev;
          pass3_last_data->prev->prev->next=pass3_last_data;
          pass3_last_data->prev->prev=pass3_last_data;
          pass3_last_data->prev->next=NULL;
          pass3_last_data->prev=pass3_last_data->next->prev;
          pass3_last_data=pass3_last_data->next;
        }
      }
      declare_hit=2; /* Remember we found a declare */
      break;
    case PASS2_END_OF_FUNCTION:
    case PASS2_MAIN_END:
      local_varcount=0;
      pass2_data=pass2_data->next;
      break;
    case PASS2_REF_LOCAL_SYMBOL:
    case PASS2_ASSIGN_LOCAL_SYMBOL:  /* [var num] [assign type] */
    case PASS2_ASSIGN_ARGUMENT: /* [var number], [arg number] */
    case PASS2_RESET_VARIABLE: /* [var number] */
      pass2_data->num[0]=var_local_number[pass2_data->num[0]];
      CALL(Pass3PushPut(scr, pass2_data));
      pass2_data=pass2_data->next;
      break;
    case PASS2_REF_GLOBAL_SYMBOL:
    case PASS2_ASSIGN_GLOBAL_SYMBOL:  /* [var num] [assign type] */
      pass2_data->num[0]=var_global_number[pass2_data->num[0]];
      CALL(Pass3PushPut(scr, pass2_data));
      pass2_data=pass2_data->next;
      break;
    case PASS2_REF_EXPORT_SYMBOL:
    case PASS2_ASSIGN_EXPORT_SYMBOL: /* [hash num] [assign type] [name] */
      {
        long num;
        num=Pass3FindLocalIdentifier(pass2_data->string[0]);
        if (num<0) {
          pass2_data->flags&=~P2D_FLAG_NUM0; /* remove num */
          CALL(Pass3PushPut(scr, pass2_data));
          CALL(Pass3PutString(scr, pass3_last_data, pass2_data->num[0], TRUE, 0, STRING_IS_VARIABLE));
        } else {
          if (pass2_data->code==PASS2_REF_EXPORT_SYMBOL)
            pass2_data->code=PASS2_REF_GLOBAL_SYMBOL; /* [function/var number] */
          else
            pass2_data->code=PASS2_ASSIGN_GLOBAL_SYMBOL; /* [function/var number] */
          pass2_data->flags&=~P2D_FLAG_STRING; /* remove num */
          pass2_data->num[0]=num;
          CALL(Pass3PushPut(scr, pass2_data));
        }
      }
      pass2_data=pass2_data->next;
      break;

    case PASS2_CALL_EXPORT_FUNCTION:
      {
        long num;
        num=Pass3FindLocalIdentifier(pass2_data->string[0]);
        if (num<0) {
          pass2_data->flags&=~P2D_FLAG_NUM0; /* remove num */
          CALL(Pass3PushPut(scr, pass2_data));
          CALL(Pass3PutString(scr, pass3_last_data, pass2_data->num[0], TRUE, 0, STRING_IS_FUNCTION));
        } else {
          pass2_data->code=PASS2_CALL_LOCAL_FUNCTION; /* [function/var number] */
          pass2_data->flags&=~P2D_FLAG_STRING; /* remove num */
          pass2_data->num[0]=num;
          CALL(Pass3PushPut(scr, pass2_data));
          CALL(Pass3PutLabelRef(scr, function_list,
                                pass2_data->num[0], &pass3_last_data->num[0]));
        }
      }
      pass2_data=pass2_data->next;
      break;

    case PASS2_LABEL_BEGIN:	/* [number] */
      CALL(Pass3PushPut(scr, pass2_data));
      label_list[pass2_data->num[0]].label_offset=pass3_last_data->final_offset;
      pass2_data=pass2_data->next;
      break;

    case PASS2_LABEL:
    case PASS2_LABEL_ELSE:		/* else [number]*/
    case PASS2_END_OF_ELSE:	/* else [number]*/
      CALL(Pass3PushPut(scr, pass2_data));
      label_list[pass2_data->num[0]].label_offset=pass3_last_data->final_offset;
      pass2_data=pass2_data->next;
      break;

    case PASS2_FUNCTION_START: /* [label/var number] */
      CALL(Pass3PushPut(scr, pass2_data));
      function_list[pass2_data->num[0]].label_offset=pass3_last_data->final_offset;
      pass2_data=pass2_data->next;
      break;

    case PASS2_LABEL_CONTINUE:	/* [level] */
      CALL(Pass3PushPut(scr, pass2_data));
      continue_list[pass2_data->num[0]].label_offset=pass3_last_data->final_offset;
      CALL(Pass3ExtractLabelRef(scr, continue_list, pass2_data->num[0]));
      pass2_data=pass2_data->next;
      break;

    case PASS2_LABEL_BREAK:	/* [level] */
      CALL(Pass3PushPut(scr, pass2_data));
      break_list[pass2_data->num[0]].label_offset=pass3_last_data->final_offset;
      CALL(Pass3ExtractLabelRef(scr, break_list, pass2_data->num[0]));
      pass2_data=pass2_data->next;
      break;

    case PASS2_CONTINUE: /* level */
      pass2_data->code=PASS2_LABEL_GOTO;
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutLabelRef(scr, continue_list,
                            pass2_data->num[0], &pass3_last_data->num[0]));
      pass2_data=pass2_data->next;
      break;

    case PASS2_GOTO_BEGIN:	/* [number] */
      pass2_data->code=PASS2_LABEL_GOTO;
      /* falling down */
    case PASS2_MAIN_START:	/* [label number] */
    case PASS2_IF_BRANCH:	/* if branch [label] */
    case PASS2_CASE:	/* [label number] */
    case PASS2_LABEL_GOTO:	/* [number] */
    case PASS2_CONDOPSTART:
    case PASS2_LOGICOR:
    case PASS2_LOGICAND:
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutLabelRef(scr, label_list,
                            pass2_data->num[0], &pass3_last_data->num[0]));
      pass2_data=pass2_data->next;
      break;
    case PASS2_JUMP_OVER_ELSE:	/* [number] jump over else */
      pass2_data->code=PASS2_LABEL_GOTO;
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutLabelRef(scr, label_list,
                            pass2_data->num[0], &pass3_last_data->num[0]));
      pass2_data=pass2_data->next;
      break;
    case PASS2_IFNOT_BRANCH_ELSE:/* if not branch to else */
      pass2_data->code=PASS2_IFNOT_BRANCH;
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutLabelRef(scr, label_list,
                            pass2_data->num[0], &pass3_last_data->num[0]));
      pass2_data=pass2_data->next;
      break;

    case PASS2_GOTO_BREAK:	/* [level] */
      pass2_data->code=PASS2_LABEL_GOTO;
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutLabelRef(scr, break_list,
                            pass2_data->num[0], &pass3_last_data->num[0]));
      pass2_data=pass2_data->next;
      break;
    case PASS2_IFNOT_BRANCH_BREAK:/* [level] if not branch to break */
      pass2_data->code=PASS2_IFNOT_BRANCH;
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutLabelRef(scr, break_list,
                            pass2_data->num[0], &pass3_last_data->num[0]));
      pass2_data=pass2_data->next;
      break;

    case PASS2_LOCAL_FUNCTION: /* [type], [number], [offset] */
      printf("No local symbol\n");
      exit(0);
    case PASS2_EXPORT_FUNCTION: /* [type], [num], [hash], [offset] */
      pass2_data->flags&=~P2D_FLAG_NUM2; /* remove hash */
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutString(scr, pass3_last_data, pass2_data->num[2], TRUE, 0, STRING_IS_FUNCTION|STRING_IS_DECLARED));
      CALL(Pass3PutLabelRef(scr, label_list,
                            pass2_data->num[1], &pass3_last_data->num[1]));
      CALL(Pass3PutString(scr, pass3_last_data, pass2_data->num[2], FALSE, 1, STRING_IS_CONSTANT|STRING_IS_DECLARED));
      CALL(Pass3PutLabelRef(scr, label_list,
                            pass2_data->num[1], &pass3_last_data->num[1]));
      pass2_data=pass2_data->next;
      break;
    case PASS2_STRING_CONSTANT:
    case PASS2_TYPE_OF_ARGUMENTS:
      CALL(Pass3PushPut(scr, pass2_data));
      CALL(Pass3PutString(scr, pass3_last_data, 0, FALSE, 0, STRING_IS_CONSTANT|STRING_IS_DECLARED));
      pass2_data=pass2_data->next;
      break;
  

/* Don't change the syntax on these commands*/  
    case PASS2_POSTINC:
    case PASS2_POSTDEC:
    case PASS2_PREINC:
    case PASS2_PREDEC:
    case PASS2_CONTENTSOF:
    case PASS2_NUM_CONSTANT:
    case PASS2_OPEN_PAREN:
    case PASS2_CLOSE_PAREN:
    case PASS2_ONCECOMPLEMENT:
    case PASS2_NOTOPERATOR:
    case PASS2_NEGATE:
  
    case PASS2_COMMA:
    case PASS2_NOTEQUAL:
    case PASS2_GREATER:
    case PASS2_SHIFTRIGHT:
    case PASS2_GREATEQ:
    case PASS2_LESS:
    case PASS2_SHIFTLEFT:
    case PASS2_LESSEQ:
    case PASS2_XOR:
    case PASS2_REMAIN:
    case PASS2_DIVISION:
    case PASS2_MULTIPLY:
    case PASS2_MINUS:
    case PASS2_PLUS:
    case PASS2_BINARYOR:
    case PASS2_BINARYAND:
    case PASS2_EQUAL:
    case PASS2_STRING_APPEND:
  
    case PASS2_END_OF_EXPR:
  
    case PASS2_BREAK_EXPR: /* Followed by an expression and several GOTO_BREAK and END_OF_EXPR */
    case PASS2_RETURN:
    case PASS2_SWITCH:
  
    case PASS2_RESIZE:
    case PASS2_OPEN_BRACKET:
    case PASS2_CLOSE_BRACKET:

    case PASS2_CALL_INTERNAL_FUNCTION:
  
    case PASS2_OPEN_BRACE:
    case PASS2_CLOSE_BRACE:
  
    case PASS2_EXIT: /* expression */
    case PASS2_VARIABLE_REFERENCE:
    case PASS2_AMOUNT_VARIABLES:

    default:
      CALL(Pass3PushPut(scr, pass2_data));
      pass2_data=pass2_data->next;
      break;
    }
    if (declare_hit) /* Keep track of the declares */
      declare_hit--;
#if 0
    {
      static long count=0;
      count++;
      if (continue_list[2].next!=0) {
        printf("Potential %d\n", count);
      }
      if (count==6642 || count==6055) {
        printf("here %d\n", count);
        if (DBG_MemListCheck(__FILE__, __LINE__)) {
          printf("\nMEMORY ERROR %ld  \n", count);
          exit(0);
        }
      }
    }
#endif
  }
  {
    long count;
    for (count=0; count<=labelcount; count++) {
      CALL(Pass3ExtractLabelRef(scr, label_list, count));
    }
  }
  {
    long count;
    for (count=0; count<=max_level; count++) {
      CALL(Pass3ExtractLabelRef(scr, continue_list, count));
    }
  }
  {
    long count;
    for (count=0; count<=total_number_of_functions; count++) {
      CALL(Pass3ExtractLabelRef(scr, function_list, count));
    }
  }
  {
    struct Pass3String *count;
    count=pass3_first_string.next;
    while (count) {
      count->offset=final_offset_count;
      count->references.label_offset=count->offset;
      final_offset_count+=count->total_length+sizeof(long);
      CALL(Pass3ExtractLabelRef(scr, &count->references, 0));
      count=count->next;
    }
  }
  return FPL_OK;
}

ReturnCode Pass3Start(struct Data *scr, char *file, char *origfile)
{
  ReturnCode ret;

  Pass3CheckSystem();
  CALL(Pass3Open(scr, file));
  CALL(Pass3Examine(scr, pass2_first_data.next));
  CALL(Pass3Close(scr, file, origfile));

  return FPL_OK;
}
