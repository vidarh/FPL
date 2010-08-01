/*
 * General config setup headers.
 */

#ifdef SUNOS
#include <varargs.h>
#define VARARG_FUNCTIONS
#ifndef UNIX
#define UNIX
#endif
#endif


#if defined(__GCC_NEW_VARARGS__) /* && defined(__GCC__) */
/*
 * If you want this file to be compiled using the va_* macros instead of the
 * dirty "address of parameter version", define as below!
 * (Without this define, SunOS 4.1.x versions will likely crash!)
 */

#define VARARG_FUNCTIONS /* should be defined before the includes below */
#endif


