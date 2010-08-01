#ifndef _COMP_H
typedef enum {
  COMP_NOTHING, /* First, does NOTHING */
  COMP_OPEN_BRACE,
  COMP_CLOSE_BRACE,
  COMP_MAIN_START,
  COMP_MAIN_END,
  COMP_CASE,
  COMP_DEFAULT,
  COMP_RETURN,
  COMP_EXIT,
  COMP_SWITCH,
  COMP_IF,
  COMP_WHILE,
  COMP_ELSE,
  COMP_BREAK,
  COMP_CONTINUE,
  COMP_DO,
  COMP_FOR,
  COMP_RESIZE,
  COMP_DECLARE,
  COMP_PARAM_LIST, /* string follows */
  COMP_ASSIGN, /* assign type follows */
  COMP_REF_LOCAL_SYMBOL, /* refers to a local symbol, number follows! */
  COMP_REF_GLOBAL_SYMBOL, /* refers to a global symbol, number follows! */
  COMP_REF_EXPORT_SYMBOL, /* refers to a export symbol, hash and name
                             follows */
  COMP_OPEN_BRACKET,  /* [ */
  COMP_CLOSE_BRACKET, /* ] */

  COMP_POSTINC,
  COMP_POSTDEC,
  COMP_PREINC,
  COMP_PREDEC,
  COMP_CONTENTSOF,
  COMP_NUM_CONSTANT,
  COMP_STRING_CONSTANT,
  COMP_OPEN_PAREN,
  COMP_CLOSE_PAREN,
  COMP_ONCECOMPLEMENT,
  COMP_NOTOPERATOR,
  COMP_NEGATE,

  COMP_COMMA,
  COMP_NOTEQUAL,
  COMP_GREATER,
  COMP_SHIFTRIGHT,
  COMP_GREATEQ,
  COMP_LESS,
  COMP_SHIFTLEFT,
  COMP_LESSEQ,
  COMP_XOR,
  COMP_REMAIN,
  COMP_DIVISION,
  COMP_MULTIPLY,
  COMP_CONDOPEND,
  COMP_CONDOPSTART,
  COMP_MINUS,
  COMP_PLUS,
  COMP_BINARYOR,
  COMP_BINARYAND,
  COMP_LOGICOR,
  COMP_LOGICAND,
  COMP_EQUAL,

  COMP_CALL_FUNCTION,
  COMP_CALL_INTERNAL_FUNCTION,

  COMP_DO_CONDITION,     /* the condition followin in a do-while expression */
  COMP_FUNCTION_DECLARE, /* function define coming up! */
  COMP_END_OF_DECLARE,   /* end of allowed declaration phase */
  COMP_VARIABLE_REFERENCE, /* a standard '&' in front of a variable name! */
  COMP_START_OF_EXPR, /* start of expression */
  COMP_END_OF_EXPR, /* end of expression */
  COMP_APPEND_STRING, /* '+' in a string expression */

  COMP_TYPE_OF_ARGUMENTS,

  COMP_AMOUNT_VARIABLES, /* the amount of variables declared within the
							just passed scope */
  COMP_END_OF_FUNCTION,
                                                        
  COMP_LAST     /* Last, does NOTHING */
} Pass1;

#define P1D_FLAG_NUM0 1
#define P1D_FLAG_NUM1  2
#define P1D_FLAG_STRING 4
struct Pass1Data
{
  Pass1 code;
  char flags;
  long num[2];
  char *string;
  long strlen;
  long line;
  long row;
  long counter;
  struct Pass1Data *next;
};

ReturnCode PutOpen(struct Data *scr, char *file);
void PutClose(struct Data *scr);
ReturnCode Put(struct Data *scr, Pass1 code);
ReturnCode PutArg(struct Data *scr, Pass1 code, long arg);
ReturnCode PutString(struct Data *scr, Pass1 code, char *arg, long len);
void Pass1Free(struct Data *scr);
#define _COMP_H
#endif
