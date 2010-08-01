

typedef enum {
  PASS2_NOTHING,
  PASS2_DECLARE,

  PASS2_REF_LOCAL_SYMBOL,
  PASS2_REF_GLOBAL_SYMBOL,
  PASS2_REF_EXPORT_SYMBOL,

  PASS2_POSTINC,
  PASS2_POSTDEC,
  PASS2_PREINC,
  PASS2_PREDEC,
  PASS2_CONTENTSOF,
  PASS2_NUM_CONSTANT,
  PASS2_STRING_CONSTANT,
  PASS2_OPEN_PAREN,
  PASS2_CLOSE_PAREN,
  PASS2_ONCECOMPLEMENT,
  PASS2_NOTOPERATOR,
  PASS2_NEGATE,

  PASS2_COMMA,
  PASS2_NOTEQUAL,
  PASS2_GREATER,
  PASS2_SHIFTRIGHT,
  PASS2_GREATEQ,
  PASS2_LESS,
  PASS2_SHIFTLEFT,
  PASS2_LESSEQ,
  PASS2_XOR,
  PASS2_REMAIN,
  PASS2_DIVISION,
  PASS2_MULTIPLY,
  PASS2_CONDOPEND,
  PASS2_CONDOPSTART,
  PASS2_MINUS,
  PASS2_PLUS,
  PASS2_BINARYOR,
  PASS2_BINARYAND,
  PASS2_LOGICOR,
  PASS2_LOGICAND,
  PASS2_EQUAL,
  PASS2_STRING_APPEND,

  PASS2_END_OF_EXPR,

  PASS2_ASSIGN_LOCAL_SYMBOL,  /* [var num] [assign type] */
  PASS2_ASSIGN_GLOBAL_SYMBOL,  /* [var num] [assign type] */
  PASS2_ASSIGN_EXPORT_SYMBOL, /* [hash num] [assign type] [name] */

  PASS2_LABEL_GOTO,	/* [number] */

  PASS2_IFNOT_BRANCH,	/* if branch */
  PASS2_IF_BRANCH,	/* if branch */

  PASS2_CALL_LOCAL_FUNCTION,
  PASS2_CALL_EXPORT_FUNCTION,
  PASS2_CALL_INTERNAL_FUNCTION,

  PASS2_SWITCH,
  PASS2_CASE,	/* [label number] */

  PASS2_BREAK_EXPR, /* Followed by an expression and several GOTO_BREAK and END_OF_EXPR */
  PASS2_RETURN,

  PASS2_RESIZE,
  PASS2_OPEN_BRACKET,
  PASS2_CLOSE_BRACKET,

  PASS2_ASSIGN_ARGUMENT, /* [var number], [arg number] */

  PASS2_EXPORT_FUNCTION, /* [type], [number], [offset] */

  PASS2_RESET_VARIABLE, /* [number] */
  PASS2_OPEN_BRACE,
  PASS2_CLOSE_BRACE,

  PASS2_EXIT, /* expression */
  PASS2_VARIABLE_REFERENCE,

  PASS2_TYPE_OF_ARGUMENTS, /* [string] */


  PASS2_JUMP_OVER_ELSE,	/* jump over olse */

  PASS2_MAIN_START,
  PASS2_MAIN_END,

  PASS2_IFNOT_BRANCH_BREAK,/* if not branch to break */
  PASS2_IFNOT_BRANCH_ELSE,/* if not branch to else */
  PASS2_LOCAL_FUNCTION, /* [type], [number], [offset] */
  PASS2_LABEL,
  PASS2_LABEL_BEGIN,	/* [number] */
  PASS2_LABEL_CONTINUE,	/* [level] */
  PASS2_LABEL_BREAK,	/* [level] */
  PASS2_GOTO_BEGIN,	/* [number] */
  PASS2_GOTO_BREAK,	/* [level] */
  PASS2_LABEL_ELSE,		/* else [number]*/
  PASS2_END_OF_ELSE,	/* else [number]*/

  PASS2_FUNCTION_START, /* [function number] */
  PASS2_CONTINUE, /* level */
  PASS2_AMOUNT_VARIABLES,
  PASS2_END_OF_FUNCTION,

  PASS2_LINE_NUMBER,

  PASS2_LAST     /* Last, does NOTHING */
} Pass2;



#define P2D_FLAG_NUM0 1
#define P2D_FLAG_NUM1  2
#define P2D_FLAG_NUM2  4
#define P2D_FLAG_STRING 8
#define P2D_FLAG_STRING2 16

struct Pass2Data
{
  Pass2 code;
  long line;
  long row;
  long level;
  char flags;
  char final_size;
  struct Pass2Data *next;
  struct Pass2Data *prev; /* used in pass3 */
  long final_offset;

  long num[3];		/* numeric argument */
  char *string[2];	/* string argument */
  long strlen[2];	/* string argument length */
  long pass3_string_offset[2];

};

ReturnCode Pass2PutOpen(struct Data *scr, char *file);
void Pass2PutClose(struct Data *scr);
ReturnCode Pass2Start(struct Data *scr);
