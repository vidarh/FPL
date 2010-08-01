#ifndef FPL_PRAGMAS_H
#define FPL_PRAGMAS_H

/*
**   $Filename: pragmas/FPL_pragmas.h $
**   $Release: 10.0 $
**   $Date: 94/12/12 $
**
**   (C) Copyright 1992 - 1994 by FrexxWare
**       All Rights Reserved
**
**  <0 - 7> data register
**  <8 - f> address register
**
**  backward argument order + return register + number of parameters
**
*/

#pragma libcall FPLBase fplAddFunction 1E BA109806
#pragma tagcall FPLBase fplAddFunctionTags 1E BA109806
#pragma libcall FPLBase fplDelFunction 24 9802
#pragma libcall FPLBase fplExecuteFile 2A A9803
#pragma tagcall FPLBase fplExecuteFileTags 2A A9803
#pragma libcall FPLBase fplExecuteScript 30 A19804
#pragma tagcall FPLBase fplExecuteScriptTags 30 A19804
#pragma libcall FPLBase fplFree 36 801
#pragma libcall FPLBase fplGetErrorMsg 3C 90803
#pragma libcall FPLBase fplInit 42 9802
#pragma tagcall FPLBase fplInitTags 42 9802
#pragma libcall FPLBase fplReset 48 9802
#pragma tagcall FPLBase fplResetTags 48 9802
#pragma libcall FPLBase fplSend 4E 9802
#pragma tagcall FPLBase fplSendTags 4E 9802
#pragma libcall FPLBase fplAlloc 54 0802
#pragma libcall FPLBase fplDealloc 5A 9802
#pragma libcall FPLBase fplConvertString 60 A9803
#pragma libcall FPLBase fplAlloca 66 0802
#pragma libcall FPLBase fplDealloca 6C 9802
#pragma libcall FPLBase fplAllocString 72 0802
#pragma libcall FPLBase fplFreeString 78 9802
#pragma libcall FPLBase fplOpenLib 7e 109804
#pragma libcall FPLBase fplCloseLib 84 09803
/* From version 8: */
#pragma libcall FPLBase fplStrtol 8a 90803
#pragma libcall FPLBase fplLtostr 90 10803
/* From version 9: */
#pragma libcall FPLBase fplReference 96 A9803
#pragma tagcall FPLBase fplReferenceTags 96 A9803
/* From version 10: */
#pragma libcall FPLBase fplAddVariable 9c BA109806
#pragma tagcall FPLBase fplAddVariableTags 9c BA109806
#endif
