/********************************************************************
 *
 * This is a small program that starts and stops another program.
 * The 'other program' is supposed to add functions to FPL at startup
 * with the given anchor, and is also supposed to remove them when
 * told to close down.
 *
 * This program can be made pure/re-entrant.
 *
 */
#include <proto/exec.h>
#include <proto/dos.h>
#include <exec/execbase.h>
#include <exec/ports.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "funclib.h"

#define OURPORTNAME "FPLfunc"
#define OTHERPORTNAME "FPLlib"

#define VERSION 2 /* we say that this is the version 2 of the funclib */

struct ReportMsg {
  struct Message msg;
  int ErrorNumber;
};

extern struct ExecBase *SysBase;

void CloseOurPort(struct MsgPort *port)
{
  if (port){
    RemPort(port);
    DeleteMsgPort(port);
  }
}

FuncRet main(int argc, char *argv[])
{
  long open=FALSE;
  long close=FALSE;
  long version;
  long anchor;
  char ourportname[40];
  char otherportname[40];
  char systemline[80];
  struct MsgPort *port;
  struct MsgPort *otherport;
  if(argc<3)
    return(FUNCLIB_PARAMETER);

  if(!strcmp("open", argv[1])) {
    open = TRUE;
    version = atoi(argv[3]);
    if (version > VERSION)
      /* the requested version is larger than this is, fail! */
      return FUNCLIB_VERSION;
  } else {
    close = TRUE;
  }
  anchor = atoi(argv[2]);

  sprintf(ourportname, "%s.%d", OURPORTNAME, anchor);
  sprintf(otherportname, "%s.%d", OTHERPORTNAME, anchor);

  /* Make our port, so that the funclib can find us */
  if (port=CreateMsgPort()){
    port->mp_Node.ln_Name=ourportname;
    port->mp_Node.ln_Pri=0;
    AddPort(port);
  } else
    return FUNCLIB_INTERNAL; /* We failed... */

  if(open) {
    /*
     * Search for an already running funclib.
     */
    Forbid();
    otherport = FindPort(otherportname);
    Permit();
    if (!otherport) {
      /*
       * Start the funclib program.
       * We send it the 'version' and 'anchor' parameters as we know
       * about.
       */
      sprintf(systemline,
              "run >NIL: FPLLIBS:lib version %d anchor %d",
              version,
              anchor);
      if(-1 != System(systemline, NULL) ) {
        struct ReportMsg *Mymsg;
        WaitPort(port);
        Mymsg = (struct ReportMsg *) GetMsg(port);
        if (!Mymsg->ErrorNumber) {
          ReplyMsg((struct Message *) Mymsg);
        } else {
          /* We failed opening the funclib! */
          CloseOurPort(port);
          return Mymsg->ErrorNumber;
        }
      } else {
        /* We failed opening the funclib! */
        CloseOurPort(port);
        return FUNCLIB_INTERNAL;
      }
    } else {
      /* the funclib is already running! */
      return FUNCLIB_OK;
    }
  } else {
    /* We tell the funclib to close down! */
    struct ReportMsg Closedown;
    Closedown.msg.mn_ReplyPort=port;
    Closedown.ErrorNumber=0;
    Forbid(); /* Search for an already running funclib. */
    if (otherport = FindPort(otherportname)) {
      PutMsg(otherport,(struct Message *) &Closedown);
      Permit();
      /* Now let's wait for a reply */
      WaitPort(port);
      GetMsg(port) ;
      if (Closedown.ErrorNumber) {
        /* Somehow we failed to close */
        CloseOurPort(port);
        return Closedown.ErrorNumber;
      }
    } else {
      /* there is no funclib to close! */
      Permit();
      return FUNCLIB_OK;
    }
  }

  /*
   * We close our port.
   */
  CloseOurPort(port);

  return FUNCLIB_OK;
}
