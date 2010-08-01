/********************************************************************
 *
 * This is a funclib program example. It is run from the from the
 * "func" program example. It is started with a simple run and
 * stopped when it receives a break-signal.
 *
 * This program can be made pure/re-entrant.
 *
 */

#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/fpl.h>
#include <libraries/fpl.h>
#include <exec/execbase.h>
#include <exec/ports.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "funclib.h"

#define TEMPLATE "Version/N/A,Anchor/N/A"

#define OURTASKNAME "FPLlib"
#define OTHERTASKNAME "FPLfunc"

enum {
  opt_VERSION,
  opt_ANCHOR,
  opt_COUNT,
};

struct ReportMsg {
struct Message msg;
int ErrorNumber;
};

extern struct ExecBase *SysBase;
struct Library *FPLBase=NULL;

long __asm func(register __a0 struct fplArgument *);

void CloseOurPort(struct MsgPort *port)
{
  if (port){
    RemPort(port);
    DeleteMsgPort(port);
  }
}

int main(int argc, char **argv)
{
  LONG opts[opt_COUNT];
  long version;
  long anchor;
  char ourportname[40];
  char otherportname[40];
  struct RDArgs *argsptr;
  struct MsgPort *port;
  struct MsgPort *otherport;
  struct ReportMsg *receive;
  int Error=0;
  if (SysBase->LibNode.lib_Version > 36) {
    argsptr = ReadArgs(TEMPLATE, opts, NULL);
    if (argsptr) {
      version = *((LONG *)opts[opt_VERSION]);
      anchor = *((LONG *)opts[opt_ANCHOR]);
      FreeArgs(argsptr);
    } else {
      PrintFault(IoErr(), NULL);	/* prints the appropriate err message */
      return FUNCLIB_PARAMETER;
    }
  } else {
    char Nomatch=0;
    if(argc<5)
      return FUNCLIB_PARAMETER;
    else {
      if(!strcmp("version", argv[version=1]) ||
         !strcmp("anchor", argv[anchor=3]))
        Nomatch=TRUE;
      else if(!strcmp("version", argv[version=3]) ||
         !strcmp("anchor", argv[anchor=1]))
        Nomatch=TRUE;
      if(Nomatch)
        return FUNCLIB_PARAMETER;
      version=atoi(argv[version]);
      anchor=atoi(argv[anchor]);
    }
  }

  sprintf(ourportname, "%s.%d", OURTASKNAME, anchor);
  sprintf(otherportname, "%s.%d", OTHERTASKNAME, anchor);

  /* Make our port, so that our father can find us */
  if (port=CreateMsgPort()){
    port->mp_Node.ln_Name=ourportname;
    port->mp_Node.ln_Pri=0;
    AddPort(port);
  } else
    return FUNCLIB_INTERNAL; /* We failed... */

  /*
   * Here should the funclib perform its initializations.
   */

  /*
   * We check that the anchor is above 400 to prevent test
   * runnings to actually try to add functions to a non-existant
   * anchor.
   */
  if(anchor > 400) {
    /* open fpl.library */
    FPLBase = OpenLibrary("fpl.library", 6);

    if(!FPLBase) {
      CloseOurPort(port);
      return FUNCLIB_RESOURCE;
    } else {
      fplAddFunctionTags((void *)anchor, "libTest", 1, 'S', NULL,
                         FPLTAG_FUNCTION, func, FPLTAG_DONE);
      fplAddFunctionTags((void *)anchor, "libTest2", 2, 'I', NULL,
                         FPLTAG_FUNCTION, func, FPLTAG_DONE);
    }
  }

  /* Tell papa we have initialized and are ready to be used! */
  Forbid();
  if (otherport = FindPort(otherportname)) {
    struct ReportMsg Iamup;
    Iamup.msg.mn_ReplyPort=port;
    Iamup.ErrorNumber=Error;
    PutMsg(otherport,&Iamup.msg);
    Permit();
    /* Check if we have had any errors */
    if (!Error) {
      /* Now let's wait for our reply */
      WaitPort(port);
      GetMsg(port);
      /* Now Iamup.ErrorNumber could be read to check for information */
      /* We wait here until they tell us to close down the funclib. */
      WaitPort(port);
      /* (No reply before we have finished things up ...) */

    } else { /* We had an error. No reply will come. Finnish up */
      /* Wait for 5 seconds, so that we are sure the message has been received */
      Delay(50*5);
      CloseOurPort(port);
      CloseLibrary(FPLBase);
      /* Return to nowhere */
      return FUNCLIB_INTERNAL;
    }
  } else { /*
            * No fatherport. This error SHOULD not be able to happen !
            * since our father would not have started us if it failed
            * opening that port
            */
    Permit();
    CloseOurPort(port);
    CloseLibrary(FPLBase);
    return FUNCLIB_INTERNAL;
  }

  /* here are all the closing down functions performed. */

  /*
   * We check that the anchor is above 400 to prevent test
   * runnings to actually try to delete functions with a non-existant
   * anchor.
   */
  if (anchor > 400 && otherport) {
    /* remove our test functions */
    fplDelFunction((void *)anchor, "libTest");
    fplDelFunction((void *)anchor, "libTest2");

    /* close library */
    CloseLibrary(FPLBase);
  }

  /* tell our controller that we are about to die! */
  receive=(struct ReportMsg *) GetMsg(port);
  /* We could use the receive.ErrorNumber field to check for special commandos */
  /* We also could write the errorfield to notify our father
     if we had any errors */
  ReplyMsg((struct Message *) receive);

  /*
   * Right after we send that reply to our controlling process, that
   * process will consider us done and continue. We do our best by
   * exiting silently but have no longer any speed demands...
   */

  /*
   * We close our port.
   */
  CloseOurPort(port);

  /*
   * Returning to nowhere...
   */
  return FUNCLIB_OK;
}

long __asm __saveds func(register __a0 struct fplArgument *arg)
{
  /*
   * Here are all our functions!
   */
  switch(arg->ID) {
  case 1:
    fplSendTags(arg->key, FPLSEND_STRING, "testlibstring", FPLTAG_DONE);
    break;
  case 2:
    {
      char *prog = "Lazer();";
      fplExecuteScript(arg->key, &prog, 1, FPLTAG_DONE);
    }
    break; 
  default:
    break;
  }
  return FPL_OK;
}
