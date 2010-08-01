/************************************************************************
 *	            FREXX PROGRAMMING LANGUAGE DEBUGGER		        *
 ************************************************************************
 
 FPL debugger instruction sender program
 
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

#include <dos/dos.h>
#include <exec/execbase.h>
#include <exec/memory.h>
#include <exec/types.h>
#include <exec/ports.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "/src/script.h"
#include "debugmail.h"

extern struct ExecBase *SysBase;
struct GfxBase *GfxBase=NULL;
struct IntuitionBase *IntuitionBase=NULL;

#define TEMPLATE "Send/k,H=Hide/k,S=Show/k,Q=Quit/s,??=Help/s"
struct args {
  char *message;
  char *ignore;
  char *allow;
  int *Quit;
  ULONG as_help;
};

struct args argarray;
struct RDArgs *rdargs=NULL;
struct MsgPort *TimerMP=NULL;
struct timerequest *TimerIO=NULL;

#define HELPTEXT "\
Usage: FPLdb <COMMAND> [ARGUMENT]\n\n\
Available commands are:\n\
send <string>\n\
hide <function>\n\
show <function>\n\
quit\n\
help\n"

void PrintError(DebugRet);
DebugRet SendToDb(long, long, void *);

int main()
{
  DebugRet ret;
  if (SysBase->SoftVer>36) {
    if ((rdargs=ReadArgs(TEMPLATE,(LONG *)&argarray,NULL))) {
      if (argarray.as_help) {
        printf(HELPTEXT);
      }
      if (argarray.Quit) {
        ret = SendToDb(MAIL_EXITALL, 0, NULL);
        PrintError(ret);
      }
      if (argarray.message) {
        ret = SendToDb(MAIL_STRING,
                       500,
                       argarray.message);
        if(ret==FPLDB_OK)
          printf("Message sent!\n");
        else
          PrintError(ret);
      }
      if(argarray.ignore || argarray.allow) {
        ret = SendToDb(argarray.ignore?MAIL_HIDE:MAIL_SHOW,
                       500,
                       argarray.ignore?argarray.ignore:argarray.allow);
        if(ret == FPLDB_OK)
          printf("All mails of the type '%s' is now %s!\n",
                 argarray.ignore?argarray.ignore:argarray.allow,
                 argarray.ignore?"ignored":"allowed");
        else
          PrintError(ret);
      }

      FreeArgs(rdargs);

    } else
      printf("Bad input!\n");
  } else
    printf("Kickstart must be over 36!\n");
  return 0;
}

void PrintError(DebugRet error)
{
  char *ErrorStrings[]={
    "OK",
    "Can't ignore that",
    "Unrecognized parameter",
    "Failed to get a system resource",
    "FPLdb isn't running"
  };
  printf("%s!\n", error>=NO_OF_RETCODES?"System error":ErrorStrings[error]);
}

DebugRet SendToDb(long subject, long data2, void *data)
{
  DebugRet ret = FPLDB_OK;
  struct Messy mess;
  struct MsgPort *port;
  Forbid();
  port=FindPort(MSGPORT_NAME);
  if (port) {
    struct MsgPort *answerport;
    answerport=CreateMsgPort();
    if (answerport) {
      mess.mess.mn_Length=sizeof(struct Messy);
      mess.mess.mn_ReplyPort=answerport;
      mess.flag = subject;
      mess.data = data;
      mess.data2 = (void *)data2;
      mess.ret = FPLDB_OK; /* init result code to OK! */
      PutMsg(port, (struct Message *)&mess);
      Permit();
      WaitPort(answerport);
      GetMsg(answerport);
      DeleteMsgPort(answerport);
      ret = mess.ret;
    } else {
      Permit();
      ret = FPLDB_RESOURCE;
    }
  } else {
    Permit();
    ret = FPLDB_NOTALIVE;
  }
  return ret;
}

