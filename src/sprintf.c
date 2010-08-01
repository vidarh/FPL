/******************************************************************************
 *		           FREXX PROGRAMMING LANGUAGE    		      *
 ******************************************************************************

 sprintf.c

 Sprintf() function source code.

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

#ifdef AMIGA
#include <exec/types.h>
#ifndef __AROS__
#include <dos.h>
#endif

#elif defined(UNIX)
#include <sys/types.h>
#include <stdio.h>
#endif

#include "script.h"
#include "debug.h"

/* Lower-case digits.  */
const uchar lower_digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";

/* Upper-case digits.  */
const uchar upper_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

#define	OUTCHAR(x) do { CALL(AddCharBuffer(scr, string, (x))); done++; } while(0)

#define BUFFSIZE 40 /* 40 bytes buffer for ltostr() calcs */

#define ADD_STRING_LIMIT 10 /* strings longer than this get appended right
                               away instead of added char by char */

ReturnCode REGARGS
AddCharBuffer(struct Data *scr,
	      struct fplStr **string,
	      long add)
{
  ReturnCode ret;
  switch(add) {
    default:
      if(scr->addchar_len<ADDBUFFER_SIZE) {
               scr->addchar_buffer[scr->addchar_len++] = (uchar)add;
               break;
      }
    case ADD_FLUSH:
      if(scr->addchar_len) {
        CALL(AppendStringToString(scr, string,
                                  scr->addchar_buffer,
                                  scr->addchar_len));
      }
    case ADD_RESET:
      scr->addchar_buffer[0]=(add>=0?add:'\0');
      scr->addchar_len=(add>=0?1:0);
      break;
  }
  return FPL_OK;
}

ReturnCode REGARGS
Sprintf(struct Data *scr,
	struct fplStr **string,
	uchar *format,
	void **param_array,
	uchar *param_types,
	long argc)
{
  /* Base-36 digits for numbers.  */
  const uchar *digits = lower_digits;

  /* Pointer into the format string.  */
  register uchar *f;

  /* Number of characters written.  */
  register size_t done = 0;

  long param_num=1; /* start with the first parameter */
  ReturnCode ret;

  long param; /* current parameter to read */

  void *value; /* general purpose value holder */

  CALL(AddCharBuffer(scr, string, ADD_RESET));

  f = format;
  while (*f != '\0') {
      /* Type modifiers.  */
      uchar is_short, is_long, is_long_double;

      /* Format spec modifiers.  */
      uchar space, showsign, left, alt;

      /* Padding character: ' ' or '0'.  */
      uchar pad;
      /* Width of a field.  */
      register long width;
      /* Precision of a field.  */
      long prec;

      /* Decimal integer is negative.  */
      uchar is_neg;

      /* Current character of the format.  */
      uchar fc;

      /* Base of a number to be written.  */
      long base;
      /* Integral values to be written.  */
      unsigned long num;
      long signed_num;

      if (*f != '%') {
	  /* This isn't a format spec, so write
	     everything out until the next one.  */
	  uchar *next = strchr(f + 1, '%');
	  if (next == NULL)
	    next = strchr(f + 1, '\0');
	  if (next - f > ADD_STRING_LIMIT) {
	      /*
	       * bonka pa hela klabbet pa en gang!
	       */
              CALL(AddCharBuffer(scr, string, ADD_FLUSH));
	      CALL(AppendStringToString(scr, string, f, next - f));
	      done += next - f;
	      f = next;
	  }
	  else
	    while (f < next)
	      OUTCHAR(*f++);
	  continue;
      }

      ++f;

      /* Check for "%%".  Note that although the ANSI standard lists
	 '%' as a conversion specifier, it says "The complete format
	 specification shall be `%%'," so we can avoid all the width
	 and precision processing.  */
      if (*f == '%') {
	  ++f;
	  OUTCHAR('%');
	  continue;
      }

      /* Check for spec modifiers.  */
      space = showsign = left = alt = 0;
      pad = ' ';
      while (*f == ' ' || *f == '+' || *f == '-' || *f == '#' || *f == '0') {
	switch (*f++) {
	  case ' ':
	    /* Output a space in place of a sign, when there is no sign.  */
	    space = 1;
	    break;
	  case '+':
	    /* Always output + or - for numbers.  */
	    showsign = 1;
	    break;
	  case '-':
	    /* Left-justify things.  */
	    left = 1;
	    break;
	  case '#':
	    /* Use the "alternate form":
	       Hex has 0x or 0X, FP always has a decimal point.  */
	    alt = 1;
	    break;
	  case '0':
	    /* Pad with 0s.  */
	    pad = '0';
	    break;
	}
      }
      if (left)
	pad = ' ';

      /* Get the field width.  */
      width = 0;
      param = 0;
      if (*f == '*') {
	  /* The field width is given in an argument.
	     A negative field width indicates left justification.  */
        if(isdigit(f[1]) && '$' == f[2]) {
          width = f[1] - '0';
          if(width<1)
            width=1;
	  if(width<argc)
	      width = (long) param_array[ width ];
	  else
	      /* BEEEEEEEEEP Tried to read unset parameter, use 1 ! */
	      width = 1;
        }
        else {
	  if(param_num<argc)
	      width = (long) param_array[param_num++];
	  else
	      /* BEEEEEEEEEP Tried to read unset parameter, use 1 ! */
	      width = 1;
	}
        if (width < 0) {
          width = - width;
          left = 1;
        }
        ++f;
      }
      else {
        if(isdigit(f[1]) && '$' == f[2]) {
          param = f[1]-'0';
          if(param<1)
            param=1; /* use no less than 1! */
        }
	while (isdigit(*f)) {
          width *= 10;
          width += *f++ - '0';
	}
      }

      /* Get the precision.  */
      /* -1 means none given; 0 means explicit 0.  */
      prec = -1;
      if (*f == '.') {
	  ++f;
	  if (*f == '*') {
            if(isdigit(f[1]) && '$' == f[2]) {
              prec = f[1] - '0';
              if(prec<1)
                prec=1;
	      if(prec<argc)
		  prec = (long) param_array[ prec ];
	      else
		  /* BEEEEEEEEEP Tried to read unset parameter, use -1 ! */
		  prec = -1;
            }
            else {
              /* The precision is given in an argument.  */
	      if(param_num<argc)
		  prec = (long) param_array[param_num++];
	      else
		  /* BEEEEEEEEEP Tried to read unset parameter, use 1 ! */
		  prec = -1;
	    }
            /* Avoid idiocy.  */
            if (prec < 0)
              prec = -1;
            ++f;
          }
	  else if (isdigit(*f)) {
	      prec = 0;
	      while (*f != '\0' && isdigit(*f))
		{
		  prec *= 10;
		  prec += *f++ - '0';
		}
	    }
      }

      /* Check for type modifiers.  */
      is_short = is_long = is_long_double = 0;
      while (*f == 'h' || *f == 'l' || *f == 'L') {
	switch (*f++) {
        case 'h':
          /* int's are short int's.  */
#if 0
          /* ignored in FPL since there are no true shorts! */
          is_short = 1;
#endif
          break;
        case 'l':
          /* int's are long int's.  */
          is_long = 1;
          break;
        case 'L':
          /* double's are long double's, and int's are long long int's.  */
          is_long_double = 1;
          break;
        }
      }
      /* Format specification.  */
      fc = *f++;

      if((param && param<argc) || (!param && param_num<argc))
	  value = (void *)(param?param_array[param]:param_array[param_num++]);
      else
	  value = (void *)-1; /* illegal use causes -1 ! */

      switch (fc) {
	  case 'i':
	  case 'd':
	    /* Decimal integer.  */
	    base = 10;
            signed_num = (long) value;

	    is_neg = signed_num < 0;
	    num = is_neg ? (- signed_num) : signed_num;
	    goto number;

	  case 'u':
	    /* Decimal unsigned integer.  */
	    base = 10;
	    goto unsigned_number;

          case 'b':
            /* Binary unsigned integer.  */
            base = 2;
            goto unsigned_number;

	  case 'o':
	    /* Octal unsigned integer.  */
	    base = 8;
	    goto unsigned_number;

	  case 'X':
	    /* Hexadecimal unsigned integer.  */
	  case 'x':
	    /* Hex with lower-case digits.  */

	    digits = fc == 'X' ? upper_digits : lower_digits;

	    base = 16;

	  unsigned_number:;
	    /* Unsigned number of base BASE.  */
            num = (unsigned long) value;

	    is_neg = 0;

	  number:;
	    /* Number of base BASE.  */
	    {
	      uchar work[BUFFSIZE];
	      uchar *workend = &work[sizeof(work) - 1];
	      register uchar *w;

	      /* Supply a default precision if none was given.  */
	      if (prec == -1)
		prec = 1;

	      /* Put the number in WORK.  */
	      w = workend;
	      while (num > 0) {
                *w-- = digits[num % base];
                num /= base;
              }
	      width -= workend - w;
	      prec -= workend - w;

	      if (alt && base == 8 && prec <= 0) {
                *w-- = '0';
                --width;
              }

	      if (prec > 0) {
                width -= prec;
                while (prec-- > 0)
                  *w-- = '0';
              }

	      if (alt && base == 16)
		width -= 2;

	      if (is_neg || showsign || space)
		--width;

	      if (!left && pad == ' ')
		while (width-- > 0)
		  OUTCHAR(' ');

	      if (is_neg)
		OUTCHAR('-');
	      else if (showsign)
		OUTCHAR('+');
	      else if (space)
		OUTCHAR(' ');

	      if (!left && pad == '0')
		while (width-- > 0)
		  OUTCHAR('0');

	      if (alt && base == 16) {
		  OUTCHAR('0');
		  OUTCHAR(fc);
              }

	      /* Write the number.  */
	      while (++w <= workend) {
		OUTCHAR(*w);
              }

	      if (left)
		while (width-- > 0)
		  OUTCHAR(' ');
	    }
	    break;

	  case 'c':
	    /* Character.  */
	    num = (long) value;
	    if (!left)
	      while (--width > 0)
		OUTCHAR(' ');
	    OUTCHAR((uchar) num);
	    if (left)
	      while (--width > 0)
		OUTCHAR(' ');
	    break;

	  case 's':
	    /* String.  */
	    {
	      static uchar null[] = "[ERR]";
              uchar *str;
	      size_t len;

	      str = (uchar *) value;
	      if ( str == NULL ||
		  (param && param>=argc) || (!param && param_num-1 >= argc) ||
                  (param?param_types[param]:param_types[param_num-1]) !=
                  FPL_STRARG )
		  /* Write "(err)" if there's space.  */
		  if (prec == -1 || prec >= (long) sizeof(null) - 1) {
		    str = null;
		    len = sizeof(null) - 1;
		  }
		  else {
		    str = "";
		    len = 0;
		  }
	      else
		  len = strlen(str);

	      if (prec != -1 && (size_t) prec < len)
		len = prec;
	      width -= len;

	      if (!left)
		while (width-- > 0)
		  OUTCHAR(' ');

	      if (len < ADD_STRING_LIMIT)
                  while (len-- > 0)
                      OUTCHAR(*str++);
	      else {
                  CALL(AddCharBuffer(scr, string, ADD_FLUSH));
                  CALL(AppendStringToString(scr, string, str, len));
                  done += len;
              }
	      if (left)
		while (width-- > 0)
		  OUTCHAR(' ');
	    }
	    break;

	  case 'p':
          case 'P': /* support this too, not ANSI though! */
	    /* Generic pointer.  */
	    {
              void *ptr;
	      ptr = (void *) value;
	      if (ptr != NULL) {
                /* If the pointer is not NULL, write it as a %#x spec.  */
                base = 16;
                digits = fc == 'P' ? upper_digits : lower_digits;
                fc = fc == 'P'?'X':'x';
                alt = 1;
                num = (unsigned long) ptr;
                is_neg = 0;
                goto number;
              }
	      else {
                /* Write "(nil)" for a nil pointer.  */
                static uchar nil[] = "(nil)";
                register uchar *p;

                width -= sizeof(nil) - 1;
                if (left)
                  while (width-- > 0)
                    OUTCHAR(' ');
                for (p = nil; *p != '\0'; ++p)
                  OUTCHAR(*p);
                if (!left)
                  while (width-- > 0)
                    OUTCHAR(' ');
              }
	    }
	    break;

	  case 'n':
	    /* Answer the count of characters written.  */
            {
              struct Identifier *ident;
	      if((param && param<argc) || (!param && param_num < argc)) {
		if((param?param_types[param]:param_types[param_num]) ==
		   FPL_INTVARARG) {
		  ident = (struct Identifier *) value;
		  ident->data.variable.var.val32[0] = done;
		}
	      }
	      /* else
		 ILLEGALY USED !!!!! */
            }
	    break;
	}

  }

  CALL(AddCharBuffer(scr, string, ADD_FLUSH)); /* flush temp buffer */

  return FPL_OK;
}

