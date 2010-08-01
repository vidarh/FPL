#include "SimpleRexx.h"

#define MSGPORT_NAME "FPLdb_2" /* message port name */

/* flag defines coming up */
typedef enum {
  MAIL_STRING=0, /* data points to a zero terminated string */
  MAIL_FUNCTION, /* data points to the name of the fpl.library
		    function just invoked (should be an ID number instead) */
  MAIL_START,    /* *NO* data, just intialized a new FPL session */
  MAIL_EXIT,	 /* *NO* data, just exited an FPL session */
  MAIL_HIDE,	 /* data is which mail subject that should be hidden */
  MAIL_SHOW,	 /* data is which mail subject that should be shown */

  MAIL_EXITALL,	 /* *NO* data, FPLdb should quit! */
  MAIL_EXECUTE,  /* *NO* data, output current line! */

  MAIL_RETURN_STRING, /* data = returned string, data2 = function name */
  MAIL_RETURN_INTEGER, /* data = returned value, data2 = function name */

  MAIL_IDENTIFIER_ACCESS, /* a variable was just "accessed" */

  MAIL_ACTIVE,  /* data is TRUE or FALSE, FALSE makes the debugger inactive
                   and should *not* do anything until ACTIVE TRUE arrives! */

  NO_OF_SUBJECTS /* NO SUBJECT */
} MailSubj;

/*
 *  Return codes from the debugger!
 */

typedef enum {
  FPLDB_OK,
  FPLDB_RESOURCE,	/* failed to get resource */
  FPLDB_NOTALIVE,	/* FPLdb is not alive! */
  FPLDB_FULL,		/* FPLdb is full! No more debugging allowed! */

  NO_OF_RETCODES	/* LAST CODE */
} DebugRet;

struct Messy {
  struct Message mess;
  struct Data *scr; /* the *universe* */
  MailSubj flag; /* message subject */
  void *data;	 /* custom data, read 'flag' to find what kind */
  void *data2;   /* custom data */
  DebugRet ret;  /* result from the debugger */
};

