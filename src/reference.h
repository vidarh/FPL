#ifndef FPL_REFERENCE_H
#define FPL_REFERENCE_H
/*
**   $Filename: FPL/Reference.h $
**   $Release: 9.0 $
**   $Date: 1994/08/06 12:48:56 $
**
**   (C) Copyright 1992, 1993 by FrexxWare
**       All Rights Reserved
*/

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

/*********                        *********
 *********  INTRODUCED IN FPL 9:  *********
 *********                        *********/

#define FPLREF_TYPE_STRING   (1<<0)  /* string   */
#define FPLREF_TYPE_INTEGER  (1<<1)  /* integer  */
#define FPLREF_TYPE_ARRAY    (1<<2)  /* array    (V10) */
#define FPLREF_TYPE_FUNCTION (1<<3)  /* function (CNS) */

/*
 * CNS = Currently Not Supported.
 * Those things are expected to appear in a future version.
 */

#define FR_OFFSET 200  /* ignore */

#define FPLREF_NAME (FR_OFFSET + 200) /* data is (char **) */
/* Supply a pointer to a (char *) to this tag and you'll have the pointer
   to the zero terminated string containing the name of the referenced
   variable! */

#define FPLREF_TYPE (FR_OFFSET + 201) /* data is (long *) */
/* Supply the address of a 'long', and FPL will store the type bits in
   that address! The bits are defined as FPLREF_TYPE_XXXX below: */

#define FPLREF_GET_STRING (FR_OFFSET + 202) /* data is (char **) */
/* Supply a pointer to a (char *) to this tag and you'll have the pointer
   to the zero terminated string (in standard FPL format, use FPLSTRLEN()
   macro to get real string length) */

#define FPLREF_SET_STRING (FR_OFFSET + 203) /* data is (char *) */
/* Set the supplied string in the string variable referenced
   THE STRING MUST HAVE BEEN ALLOCATED WITH fplAllocString() or FPL will
   fail hard! */

#define FPLREF_GET_INTEGER (FR_OFFSET + 204) /* data is (long **) */
/* Supply a pointer to a (long *) to this tag and you'll have the pointer
   to the long word holding the contents of the integer variable, or NULL
   if something is wrong! */

#define FPLREF_SET_INTEGER (FR_OFFSET + 205) /* data is (long) */
/* Set the supplied number in the integer variable referenced */

/*********                         *********
 *********  INTRODUCED IN FPL 10:  *********
 *********                         *********/

#define FPLREF_ARRAY_ITEM (FR_OFFSET + 206) /* data is pointer to long array */
/* Select which array item to read/write. Specify a pointer to an array
   holding one long integers for each dimension specified ending with -1, like:
   {dim1, dim2, -1} */

#define FPLREF_ARRAY_INFO (FR_OFFSET + 207)/* data: pointer to fplRef struct */
/* Array variable reference information */

#define FPLREF_SET_MY_STRING (FR_OFFSET + 208) /* data is char pointer */
/* To be used when the new string _isn't_ a FPL string. Supply a regular
   char pointer and FPL will use that as the new string. This tag may be
   combined with the FPLREG_SET_MY_STRLEN (which should be specified before
   this tag) to set the actual string length. Without the length tag, FPL
   will do a strlen() call to get the length of the string. */

#define FPLREF_SET_MY_STRLEN (FR_OFFSET + 209) /* data is long */
/* Use this tag before the FPLREG_SET_MY_STRING tag, to make FPL use this
   as string length instead of making a strlen() on the supplied char pointer
   (which is slow and stops on ASCII zero). */

struct fplRef {
  long Dimensions; /* number of array dimensions */
  long *ArraySize; /* Array size array pointer */
};

/*********                         *********
 *********  INTRODUCED IN FPL 11:  *********
 *********                         *********/

#define FPLREF_ARRAY_RESIZE (FR_OFFSET + 210) /* data is struct pointer */
/* Fill in a fplRef struct with the new values of the array reference you
   want to resize. You may very well alter the number of dimensions too.
   Using more dimensions than 40 is considered illegal and may cause
   unpredictable results. */

#endif /* FPL_REFERENCE_H */
