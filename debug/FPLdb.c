/************************************************************************
 *	            FREXX PROGRAMMING LANGUAGE DEBUGGER		        *
 ************************************************************************
 
 FPL debugger process main source
 
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
#include <stdarg.h>

#include "/src/script.h"
#include "debugmail.h"

#include "SimpleRexx.h"

extern struct ExecBase *SysBase;
struct GfxBase *GfxBase=NULL;
struct IntuitionBase *IntuitionBase=NULL;

#define TEMPLATE "W=Window/k,H=Help/s,R=Return/s,L=Libcalls/s,I=Ident/k,Inactive/s"
struct args {
  char *window;
  ULONG help;
  ULONG returncodes;
  ULONG libcalls;
  ULONG ident;
  ULONG inactive;
};

#define HELPTEXT "\
FPLdb version 2\n\
Simply run this file to start the FPL debug process!\n\n\
Command line options to play with:\n\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\
WINDOW -   How to present all data.\n\
           Default window string is \"%s\"\n\
RETURN -   will display function return codes\n\
LIBCALLS - will display fpl.library function calls\n\
IDENT -    Always display process identifier\n\
INACTIVE - Start-up with inactive mode\n"

typedef enum {
  REXX_FIST_DOES_NOTHING, /* Must be the first in the list! */

  REXX_SET_RETURN,   /* control return code display! */
  REXX_SET_IDENT,    /* control process identifier display */
  REXX_SET_LIBCALLS, /* control libcall display */
  REXX_VIEW_SETUP,   /* display current setup! */

  REXX_ACTIVE,       /* toggle debugger active/inactive */

  REXX_LAST_COMMAND /* Must be the last in the list! */
} ARexxCommand;

struct ARexxStruct {
  char *name;
  ARexxCommand rexx;
  long parameters;
};

struct ARexxStruct commands[]={
  {"setreturn", REXX_SET_RETURN, 1},
  {"setident", REXX_SET_IDENT, 1},
  {"setlibcalls", REXX_SET_LIBCALLS, 1},
  {"viewsetup", REXX_VIEW_SETUP, 0},
  {"active", REXX_ACTIVE, 1},
};

#define STRUCT_ENTRIES(x) sizeof(x)/sizeof(x[0])

struct args argarray;
struct RDArgs *rdargs=NULL;
struct MsgPort *TimerMP=NULL;
struct timerequest *TimerIO=NULL;

BPTR out; /* output file handle for FPrintf() */
BPTR output; /* stdout */

void Main(void);
void DisplaySetup(void);

#define DEFAULT_WINDOW "CON:0/0/640/200/FPLdb/AUTO/CLOSE/WAIT/INACTIVE"

long Debugging; /* number of current debugger users */
AREXXCONTEXT	RexxStuff;

void InputLoop(void);

int GoSleeping(int signalbits)
{
  int signals;
  struct RexxMsg *rmsg;
  signals = Wait(signalbits); /* simply Wait()!!! */
  while (rmsg=GetARexxMsg(RexxStuff)) {
    char *error=NULL;
    char *result=NULL;
    long errlevel=0;
    char command[32];
    char setit[10];
    int words;

    words = sscanf(ARG0(rmsg), "%31s %8s", command, setit);
    if(!words--)
      FPrintf(out, "Unknown message received: '%s'\n", ARG0(rmsg));
    else {
      int a;
      ARexxCommand cmd;
      int hit=FALSE;
      for(a=0; a<STRUCT_ENTRIES(commands); a++) {
        if(!stricmp(commands[a].name, command)) {
          cmd = commands[a].rexx;
          hit = TRUE;
          break;
        }
      }
      if(hit) {
        if(words != commands[a].parameters) {
          FPrintf(out, "Illegal syntax for the '%s' command (%ld/%ld):\n%s",
                  command, commands[a].parameters, words, ARG0(rmsg));
        }
        else {
          hit = atoi(setit);
          switch(cmd) {
            case REXX_VIEW_SETUP:
              DisplaySetup();
              break;
            case REXX_SET_IDENT:
              argarray.ident = hit;
              DisplaySetup();
              break;
            case REXX_SET_RETURN:
              argarray.returncodes = hit;
              DisplaySetup();
              break;
            case REXX_SET_LIBCALLS:
              argarray.libcalls = hit;
              DisplaySetup();
              break;
            case REXX_ACTIVE:
              argarray.inactive = !hit;
              DisplaySetup();
              break;
          }
        }
      }
      else
        FPrintf(out, "Unknown command received: '%s'\n", command);    
    }

    if (error) {
      SetARexxLastError(RexxStuff,rmsg,error);
    }
    ReplyARexxMsg(RexxStuff,rmsg,result,errlevel);
  }

  return signals;
}

int main()
{
  output = Output();
  if (SysBase->SoftVer>36) {
    if ((rdargs=ReadArgs(TEMPLATE,(LONG *)&argarray,NULL))) {
      if(argarray.help)
        FPrintf(output, HELPTEXT, DEFAULT_WINDOW);
      else {
        RexxStuff=InitARexx("FPLdb", NULL);
        out = Open(argarray.window?argarray.window:DEFAULT_WINDOW,
                      MODE_NEWFILE);
        if(out) {
          InputLoop();
          Close(out);
        } else {
          FPrintf(output, "Couldn't open window!\n");
        }
        FreeARexx(RexxStuff);
      }
      FreeArgs(rdargs);
    } else
      FPrintf(output, "Bad input!\n");
  } else
    FPrintf(output, "Kickstart must be V37 or later!\n");
  return 0;
}

void displayus(struct Data *scr)
{
  if(argarray.ident) {
    FPrintf(out, "%s | ",
            scr->identifier?scr->identifier:"<unknown>");
  }
}

void InputLoop()
{
  int count;
  struct MsgPort *port;
  char stop=FALSE;
  int signal;
  char *text;
  struct Messy *mess;
  struct fplStr *str;
  long arexxsignals=ARexxSignal(RexxStuff);

  if (!FindPort(MSGPORT_NAME)) {
    port=CreatePort(MSGPORT_NAME, 0);
    if (port) {
      FPrintf(out, "FPLdb is ready to receive!\n");
      DisplaySetup(); /* display startup-setup! */
      while (!stop) {
        struct Data *scr;
        signal= GoSleeping((1<<port->mp_SigBit) | SIGBREAKF_CTRL_C |
                           arexxsignals| SIGBREAKF_CTRL_F );
        if (signal&SIGBREAKF_CTRL_C) {
          stop=TRUE;
        }
        if (signal&SIGBREAKF_CTRL_F) {
          argarray.inactive = !argarray.inactive;
          DisplaySetup();
        }
        while(mess=(struct Messy *)GetMsg(port)) {
          scr = mess->scr;
          if(argarray.inactive) {
            /*
             * We act like dead! ;)
             */
            ReplyMsg((struct Message *)mess);
            continue;
          }
          switch(mess->flag) {
          case MAIL_STRING:
            /*
             * Just a plain text string received from another process!
             */
            displayus(scr);
            FPrintf(out, "> %s\n", (char *)mess->data);
            break;
          case MAIL_FUNCTION:
            if(argarray.libcalls) {
              displayus(scr);
              /*
               * Someone invoked a fpl.library function!
               */
              FPrintf(out, "called fpl.library/%s();\n",
                      (char *)mess->data);
            }
            break;
          case MAIL_START:
            displayus(scr);
            Debugging++;
            FPrintf(out, "  just entered debug mode.\n");
            break;
          case MAIL_EXIT:
            /*
             * A process just exited an FPL debug session!
             */
            displayus(scr);
	    Debugging--;
            FPrintf(out, "  just switched off debug mode\n");
            break;
	    
          case MAIL_EXITALL:
            /*
             * We should exit!
             */
            displayus(scr);
            stop=TRUE;
            break;

          case MAIL_EXECUTE:
            displayus(scr);
            text = scr->text;
            FPrintf(out, "%ld :", scr->virprg);
            while(*text && *text!='\n') {
              FPutC(out, *text);
              text++;
            }
            FPutC(out, '\n');
            break;

          case MAIL_RETURN_STRING:
            if(argarray.returncodes) {
              displayus(scr);
              str = (struct fplStr *)mess->data;
              text = str?str->string:"<NIL>";
              FPrintf(out, "  %s() => \"", mess->data2);
              while(*text) {
                if((*text>=0x20 && *text<0x80) ||
                   (*text>=0xa0))
                  FPutC(out, *text);
                else
                  FPrintf(out, "\\x%02lX", (int)*text);
                text++;
              }
              FPrintf(out, "\"\n");
            }
            break;
          case MAIL_RETURN_INTEGER:
            if(argarray.returncodes) {
              displayus(scr);
              FPrintf(out, "  %s() => %ld\n", mess->data2, mess->data);
            }
            break;
#if 0
  /* use it like this... */
          case MAIL_IDENTIFIER_ACCESS:
            {
              struct Identifier *get = mess->data;
              if(argarray.check &&
                 !strcmp(argarray.check, get->name)) {
                displayus(scr);
                FPrintf(out, "  %s was used on line %ld.\n", get->name, scr->virprg);
              }
            }
            break;
#endif
          default:
            break;
          }
          ReplyMsg((struct Message *)mess);
        }
      }
      FPrintf(out, "Exiting...\n");
      Forbid();
      while(mess=(struct Messy *)GetMsg(port))
        ReplyMsg((struct Message *)mess);
      DeletePort(port);
      Permit();
    }
  } else
    FPrintf(output, "FPLdb is already running\n");
}

void DisplaySetup(void)
{
  FPrintf(out, "** CURRENT FPLDB SETUP **\n");

  if(argarray.inactive)
    FPrintf(out, "  Mode: inactive\n");

  FPrintf(out, "  ARexx port: '%s'\n",
          ARexxName(RexxStuff)?ARexxName(RexxStuff):"<none>");

  FPrintf(out, "  Display: %s%s%s.\n",
          argarray.returncodes?"returncodes ":"",
          argarray.libcalls?"libcalls ":"",
          argarray.ident?"process ":"");
}
