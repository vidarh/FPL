/******************************************************************************
 *                        FREXX PROGRAMMING LANGUAGE                          *
 ******************************************************************************

 error.h

 All error message defines!

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

/*
 * All error numbers as typedefed enum!
 */

typedef enum {
  CERROR_NOTHING, /* first, not in use */

  CERROR_MISSING_SEMICOLON, /* none */
  CERROR_MISSING_BRACKET, /* bracket char */
  CERROR_MISSING_PARENTHESIS, /* parenthesis char */
  CERROR_IDENTIFIER_USED, /* identifier name */
  CERROR_IDENTIFIER_NOT_FOUND, /* identifier name */
  CERROR_READONLY_VIOLATE, /* variable name */
  CERROR_ILLEGAL_NUMERICAL, /* none */
  CERROR_ILLEGAL_STRING, /* none */
  CERROR_ILLEGAL_REFERENCE, /* variable name */
  CERROR_MISSING_SINGLEQUOTE, /* none */
  CERROR_MISSING_OPERAND, /* none */
  CERROR_ILLEGAL_CONDOPER, /* none */
  CERROR_INCOMPLETE_STATEMENT, /* none */
  CERROR_TOO_LARGE_ARRAY, /* variable array name */
  CERROR_CANT_REACH_STATEMENT, /* none */
  CERROR_MISSING_BRACE, /* brace character */
  CERROR_MISSING_COLON, /* none */
  CERROR_ILLEGAL_DECLARATION, /* none */
  CERROR_MISSING_WHILE, /* none */
  CERROR_NOT_YET_SUPPORTED, /* none */
  CERROR_ELSE, /* none */
  CERROR_ILLEGAL_BRACE, /* brace */
  CERROR_ILLEGAL_XXX, /* keyword string */
  CERROR_ALREADY_DEFINED, /* identifier name, line number of prev define */
  CERROR_MISSING_ARGUMENT, /* function name */
  CERROR_EXPECTED_PAREN, /* paren char */
  CERROR_ILLEGAL_PARAMETER, /* none */
  CERROR_ILLEGAL_ARRAY, /* none */
  CERROR_FILE_NOT_FOUND, /* file name */
  CERROR_CMPPOS, /* none */  
  
  CERROR_LAST /* last, not in use */
} ErrorCode;

/*
 * Error string defines should have the same name as the error enums, with a
 * '_TEXT' appended.
 */
#define CERROR_MISSING_SEMICOLON_TEXT "missing semicolon"
#define CERROR_IDENTIFIER_USED_TEXT "identifier '%s' already used"
#define CERROR_IDENTIFIER_NOT_FOUND_TEXT "identifier '%s' not found"
#define CERROR_MISSING_BRACKET_TEXT "missing bracket (%c)"
#define CERROR_READONLY_VIOLATE_TEXT "'%s' is a read-only variable"
#define CERROR_ILLEGAL_NUMERICAL_TEXT "expected a numerical statement"
#define CERROR_ILLEGAL_STRING_TEXT "expected a string statement"
#define CERROR_ILLEGAL_REFERENCE_TEXT "illegal use of the reference variable '%s'"
#define CERROR_MISSING_PARENTHESIS_TEXT "missing parenthesis '%c'"
#define CERROR_MISSING_SINGLEQUOTE_TEXT "missing single quote (\')"
#define CERROR_MISSING_OPERAND_TEXT "operator wasn't followed by an operand"
#define CERROR_ILLEGAL_CONDOPER_TEXT "illegal use of conditional operator"
#define CERROR_INCOMPLETE_STATEMENT_TEXT "incomplete statement"
#define CERROR_TOO_LARGE_ARRAY_TEXT "too many dimensions in the array '%s'"
#define CERROR_CANT_REACH_STATEMENT_TEXT "cannot reach statement"
#define CERROR_MISSING_BRACE_TEXT "missing brace (%c)"
#define CERROR_MISSING_COLON_TEXT "missing colon"
#define CERROR_ILLEGAL_DECLARATION_TEXT "illegal declarator position"
#define CERROR_MISSING_WHILE_TEXT "missing 'while' keyword"
#define CERROR_NOT_YET_SUPPORTED_TEXT "operation not currently supported"
#define CERROR_ELSE_TEXT "else not associated with if or while"
#define CERROR_ILLEGAL_BRACE_TEXT "illegally placed brace '%c'"
#define CERROR_ILLEGAL_XXX_TEXT "illegal %s usage"
#define CERROR_ALREADY_DEFINED_TEXT "'%s' has already been defined on line %d"
#define CERROR_MISSING_ARGUMENT_TEXT "missing argument to function '%s'"
#define CERROR_EXPECTED_PAREN_TEXT "expected a '%c' character"
#define CERROR_ILLEGAL_PARAMETER_TEXT "illegal parameter"
#define CERROR_ILLEGAL_ARRAY_TEXT "illegal array"
#define CERROR_FILE_NOT_FOUND_TEXT "file not found '%s'"
#define CERROR_CMPPOS_TEXT ""
/*
 * Information type defines:
 */
#define CERROR_ERROR     1 /* This error aborts the compilation */
#define CERROR_WARNING   2 /* This informs only about an error */
#define CERROR_INFORM    3 /* This is information only */

/*
 * struct setup macro
 */
#define ERR(x,y) x, x ## _TEXT, y

/*
 * Information struct
 */
struct Error {
  ErrorCode error;
  char *text;
  long type;
};

void CompilerInfo(struct Data *scr, ErrorCode error, ...);
#define INFO CompilerInfo
