struct Pass3LabelRef {
  long *label_store;	/* where to store the offset */
  long label_offset;
  struct Pass3LabelRef *next;
};

struct Pass3String {
  long offset;		/* offset of this string */
  long total_length;	/* total length of 'string'+'0'+'outfill' */
  struct Pass3String *next;
  long hash;
  char type;            /* type of string */
  struct Pass3LabelRef references;
  long strlen;		/* length of string */
  char *string;		/* string */
};

ReturnCode Pass3Start(struct Data *scr, char *file, char *origfile);

#define HUNK_HEADER ".FC"
#define HUNK_CODE "CODE"
#define HUNK_VERSION "VERS"
#define HUNK_FILE "FILE"
#define HUNK_REQUIRE_FPL "REQU"
#define HUNK_SYMBOLS "SYMB"

#define FLAG_LOW_BYTE_FIRST 1
#define FLAG_WORD_ALIGNED 2

#define VERSION     "0.36" /* compiler version */
#define VERSION_NUM    36  /* compiler version */
#define REQUIRE_FPL 14000  /* requires at least FPL version 14 */

#define STRING_IS_FUNCTION 1
#define STRING_IS_VARIABLE 2
#define STRING_IS_CONSTANT 4
#define STRING_IS_DECLARED 8

