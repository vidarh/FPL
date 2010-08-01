#ifndef FPL_PROTOS_H
#define FPL_PROTOS_H

/*
**   $Filename: clib/FPL_protos.h $
**   $Release: 10.0 $
**   $Date: 94/12/12 $
**
**   (C) Copyright 1992-1994 by FrexxWare
**       All Rights Reserved
*/

void *fplAlloc(void *, long);
void *fplAlloca(void *, long);
long fplConvertString(void *, char *, char *);
void fplDealloc(void *, void *);
void fplDealloca(void *, void *);
long fplExecuteScript(void *, char **, long, unsigned long *);
long fplExecuteFile(void *, char *, unsigned long *);
char *fplGetErrorMsg(void *, long, char *);
void *fplInit(long (*)(struct fplArgument *), unsigned long *);
void fplFree(void *);
long fplAddFunction(void *, char *, long, char, char *, unsigned long *);
long fplDelFunction(void *, char *);
long fplReset(void *, unsigned long *);
long fplSend(void *, unsigned long *);
void *fplAllocString(void *, long);
void fplFreeString(void *, void *);
long fplOpenLib(void *, char *, long, long);
long fplCloseLib(void *, char *, long);

/* from version 8: */

long fplStrtol(char *, long, char **);
char *fplLtostr(void *, long, long);

/* from version 9: */

long fplReference(void *, void *, unsigned long *);

/* from version 10: */

long fplAddVariable(void *, char *, long, char, void *, unsigned long *);

/* special tags functions: */

#ifndef VARARG_FUNCTIONS
long fplExecuteScriptTags(void *, char **, long, unsigned long, ...);
long fplExecuteFileTags(void *, char *, unsigned long, ...);
void *fplInitTags(long (*)(struct fplArgument *), unsigned long, ...);
long fplAddFunctionTags(void *, char *, long, char, char *, unsigned long, ...);
long fplAddVariableTags(void *, char *, long, char, void *, unsigned long, ...);
long fplResetTags(void *, unsigned long, ...);
long fplSendTags(void *, unsigned long, ...);
long fplReferenceTags(void *, void *, unsigned long, ...);
#else /* if VARARG_FUNCTIONS: */
long fplExecuteScriptTags(void *, char **, long, ...);
long fplExecuteFileTags(void *, char *, ...);
void *fplInitTags(long (*)(struct fplArgument *), ...);
long fplAddFunctionTags(void *, char *, long, char, char *, ...);
long fplAddVariableTags(void *, char *, long, char, void *, ...);
long fplResetTags(void *, ...);
long fplSendTags(void *, ...);
long fplReferenceTags(void *, void *, ...);
#endif
#endif
