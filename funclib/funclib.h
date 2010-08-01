/*
 * Here are all funclib structures, defines, macros and return codes.
 */

typedef enum {
  FUNCLIB_OK,
  /* The funclib execution (start/close) went OK */

  FUNCLIB_PARAMETER,
  /* parameter error when the funclib was called */

  FUNCLIB_INTERNAL,
  /* some kind of internal, undefined error! */

  FUNCLIB_RESOURCE,
  /* failed getting a system resource */

  FUNCLIB_MEMORY,
  /* failed getting enough memory from the system */

  FUNCLIB_LOAD,
  /* failed loading the funclib */

  FUNCLIB_VERSION
  /* the requested funclib version wasn't found */
} FuncRet;

