
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "script.h"

#define	inchar()	((c = string[pos++]),                \
			 pos>stringlen ?                \
			 EOF : (++read_in, c))
#define	conv_error()	return(done)
#define input_error()	return(-1)
#define	memory_error()	return(-1)

#define SCALL(x)        if(x) return -1;

/* Read formatted input from String according to the format string
   FORMAT, using the argument list in ARG.
   Return the number of assignments made, or -1 for an input error.  */
long
Sscanf(struct Data *scr,
       uchar *string,
       uchar *format,
       long argc,
       void **param_array,
       uchar *param_types)
{
  struct Identifier *ident;
  long stringlen = string?FPL_STRLEN(string):0; /* length of string to parse */
  long pos=0; /* current scan point! */
  char *f = format;
  char fc;		/* Current character of the format.  */
  long done = 0;	/* Assignments done.  */
  unsigned long read_in = 0;	/* Chars read in.  */
  long c;		/* Last char read.  */
  long do_assign;	/* Whether to do an assignment.  */
  long width;		/* Maximum field width.  */

  /* Type modifiers.  */
  char is_short, is_long, is_long_double;
#ifdef	HAVE_LONGLONG
      /* We use the `L' modifier for `long long int'.  */
#define	is_longlong	is_long_double
#else
#define	is_longlong	0
#endif
  /* If a [...] is a [^...].  */
  char not_in;
  /* Base for integral numbers.  */
  long base;
  /* Integral holding variables.  */
  long num;
  unsigned long unum;

  /* Workspace.  */
  uchar work[200];
  uchar *w;		/* Pointer into WORK.  */
  long param_num = 1; /* start on (0 and 1 is the first strings) 2nd param! */

  if (format == NULL) {
    input_error();
  }

  c = inchar();

  /* Run through the format string.  */
  while (*f != '\0') {
    fc = *f++;
    if (fc != '%') {
      /* Characters other than format specs must just match.  */
      if (c == EOF)
	  input_error();
      if (isspace(fc)) {
	/* Whitespace characters match any amount of whitespace.  */
	while (isspace (c))
	    inchar ();
	continue;
      }
      else if (c == fc)
	  (void) inchar();
      else
	  conv_error();
      continue;
    }

    /* Check for the assignment-suppressant.  */
    if (*f == '*') {
      do_assign = 0;
      ++f;
    }
    else
	do_assign = 1;
		
    /* Find the maximum field width.  */
    width = 0;
    while (isdigit(*f)) {
      width *= 10;
      width += *f++ - '0';
    }
    if (width == 0)
	width = -1;

    /* Check for type modifiers.  */
    is_short = is_long = is_long_double = 0;
    while (*f == 'h' || *f == 'l' || *f == 'L')
	switch (*f++) {
	case 'h':
#if 0
	  /* int's are short int's.  */
	  is_short = 1;
	  break;
#endif
	case 'l':
#if 0
	  if (is_long)
	      /* A double `l' is equivalent to an `L'.  */
	      is_longlong = 1;
	  else
	      /* int's are long int's.  */
	      is_long = 1;
	  break;
#endif
	case 'L':
#if 0
	  /* double's are long double's, and int's are long long int's.  */
	  is_long_double = 1;
#endif
	  break;
	}

    /* End of the format string?  */
    if (*f == '\0')
	conv_error();

    /* Find the conversion specifier.  */
    w = work;
    fc = *f++;
    if (fc != '[' && fc != 'c' && fc != 'n')
	/* Eat whitespace.  */
	while (isspace(c))
	    (void) inchar();
    switch (fc) {
    case '%':	/* Must match a literal '%'.  */
      if (c != fc)
	  conv_error();
      break;

    case 'n':	/* Answer number of assignments done.  */
      if (do_assign) {
	if(++param_num>=argc)
	    return -1;
	if(param_types[param_num] == FPL_INTVARARG) {
	  ident = (struct Identifier *) param_array[param_num];
	  ident->data.variable.var.val32[0] = read_in;
	}
	else
	    conv_error();
      }
      break;

    case 'c':	/* Match characters.  */
      if (do_assign) {
	if(++param_num>=argc)
	    return -1;
	if(param_types[param_num] == FPL_STRVARARG) {
	  ident = (struct Identifier *) param_array[param_num];
	  if(ident->data.variable.var.str[0])
	      FREE_KIND(ident->data.variable.var.str[0]);
	  ident->data.variable.var.str[0]=NULL;
	}
	else
	    conv_error();
      }
      if (c == EOF)
	  input_error();
      if (width == -1)
	  width = 1;
      if (do_assign) {
	SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0],
			   ADD_RESET));
	do {
	  SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0], c));
	} while (inchar() != EOF && --width > 0);
      }
      else
	  while (inchar() != EOF && width > 0)
	      --width;
      
      if (do_assign) {
	SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0], ADD_FLUSH));
	++done;
      }

      if (c == EOF)
	  input_error();
      break;
      
    case 's':	/* Read a string.  */
      if (do_assign) {
	if(++param_num>=argc)
	    return -1;
	if(param_types[param_num] == FPL_STRVARARG) {
	  ident = (struct Identifier *) param_array[param_num];
	  if(ident->data.variable.var.str[0])
	      FREE_KIND(ident->data.variable.var.str[0]);
	  ident->data.variable.var.str[0]=NULL;
	}
	else
	    conv_error();
      }
      if (c == EOF)
	  input_error();

      if(do_assign) {
	SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0],
			   ADD_RESET));
      }
      do {
	if (isspace(c))
	    break;
	if (do_assign) {
	  SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0], c));
	}
      } while (inchar() != EOF && (width <= 0 || --width > 0));

      if (do_assign) {
	SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0], ADD_FLUSH));
	++done;
      }
      break;

    case 'x':	/* Hexadecimal integer.  */
    case 'X':	/* Ditto.  */ 
      base = 16;
      goto number;

    case 'o':	/* Octal integer.  */
      base = 8;
      goto number;

    case 'u':	/* Decimal integer.  */
    case 'd':	/* Ditto.  */
      base = 10;
      goto number;

    case 'i':	/* Generic number.  */
      base = 0;

    number:;
      if (c == EOF)
	  input_error();

      /* Check for a sign.  */
      if (c == '-' || c == '+') {
	*w++ = (uchar)c;
	if (width > 0)
	    --width;
	(void) inchar();
      }

      /* Look for a leading indication of base.  */
      if (c == '0') {
	if (width > 0)
	    --width;
	*w++ = '0';
	(void) inchar();
	if (tolower(c) == 'x') {
	  if (base == 0)
	      base = 16;
	  if (base == 16) {
	    if (width > 0)
		--width;
	    (void) inchar();
	  }
	}
	else if (base == 0)
	    base = 8;
      }
      if (base == 0)
	  base = 10;

      /* Read the number into WORK.  */
      do {
	if (base == 16 ? !isxdigit(c) :
	    (!isdigit(c) || c - '0' >= base))
	    break;
	*w++ = (uchar)c;
	if (width > 0)
	    --width;
      } while (inchar() != EOF && width != 0);

      if (w == work ||
	  (w - work == 1 && (work[0] == '+' || work[0] == '-')))
	  /* There was on number.  */
	  conv_error();

      /* Convert the number.  */
      *w = '\0';
      num = Strtol(work, base, &w);
      if (w == work)
	  conv_error();

      if (do_assign) {
	if(++param_num>=argc)
	    return -1;
	if(param_types[param_num] == FPL_INTVARARG) {
	  ident = (struct Identifier *) param_array[param_num];
	  ident->data.variable.var.val32[0] = num;
	}
	else
	    conv_error();
	++done;
      }
      break;

    case '[':	/* Character class.  */
      if (do_assign) {
	if(++param_num>=argc)
	    return -1;
	if(param_types[param_num] == FPL_STRVARARG) {
	  ident = (struct Identifier *) param_array[param_num];
	  if(ident->data.variable.var.str[0])
	      FREE_KIND(ident->data.variable.var.str[0]);
	  ident->data.variable.var.str[0]=NULL;
	}
	else
	    conv_error();
      }
      if (c == EOF)
	  input_error();
      if (*f == '^') {
	++f;
	not_in = 1;
      }
      else
	  not_in = 0;
      while ((fc = *f++) != '\0' && fc != ']') {
	if (fc == '-' && *f != '\0' && *f != ']' &&
	    w > work && w[-1] <= *f)
	    /* Add all characters from the one before the '-'
	       up to (but not including) the next format char.  */
	    for (fc = w[-1] + 1; fc < *f; ++fc)
		*w++ = fc;
	else
	    /* Add the character to the list.  */
	    *w++ = fc;
      }
      if (fc == '\0')
	  conv_error();
      *w = '\0';
      unum = read_in;
      if(do_assign) {
	SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0],
			   ADD_RESET));
      }

      do {
	if ((strchr(work, c) == NULL) != not_in)
	    break;
	if (do_assign) {
	  SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0], c));
	}
	if (width > 0)
	    --width;
      } while (inchar() != EOF && width != 0);

      if (read_in == unum)
	  conv_error();

      if (do_assign) {
	SCALL(AddCharBuffer(scr, &ident->data.variable.var.str[0], ADD_FLUSH));
	++done;
      }
      break;

    case 'p':	/* Generic pointer.  */
      base = 16;
      /* A PTR must be the same size as a `long int'.  */
      is_long = 1;
      goto number;
    }
  }
  conv_error();
}
