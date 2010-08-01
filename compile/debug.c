/******************************************************************************
 *			  FREXX PROGRAMMING LANGUAGE			      *
 ******************************************************************************
 
 Debug.c
 
 Routines only part of debug versions
 
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

#if defined(AMIGA) && defined(DEBUGMAIL)
#include "script.h"

#include <exec/execbase.h>
#include <exec/types.h>
#include <exec/ports.h>
#include <pragmas/exec_sysbase_pragmas.h>
#include <clib/exec_protos.h>

#include "/debug/debugmail.h"

void REGARGS
DebugMail(struct Data *scr, MailSubj flag, long data2, void *data)
{
  struct ExecBase *SysBase = *(struct ExecBase **)4;
  struct Messy mess;
  struct MsgPort *port;
  if(scr->flags&FPLDATA_DEBUG) {
    Forbid();
    port=FindPort(MSGPORT_NAME);
    if (port) {
      struct MsgPort *answerport;
      answerport=CreateMsgPort();
      if (answerport) {
        mess.mess.mn_Length=sizeof(struct Messy);
        mess.mess.mn_ReplyPort=answerport;
        mess.scr = scr; /* give them the world of FPL! */
        mess.flag = flag;
        switch(flag) {
        case MAIL_START:
        case MAIL_EXIT:
          mess.data = NULL;
          break;
        default:
          mess.data = data;
        }
        mess.data2 = (void *)data2;
        PutMsg(port, (struct Message *)&mess);
        Permit();
        WaitPort(answerport);
        DeleteMsgPort(answerport);
        /* Message sent */
      } else {
        Permit();
      }
    } else {
      Permit();
    }
  }
}

#endif
