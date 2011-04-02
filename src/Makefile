# General UNIX Makefile v2 August 21 1996

# This makefile should compile FPL under most UNIXes that has gcc _and_
# support shared (dynamically linked) libraries.

# fpl.library - A shared library interpreting script langauge. 
# Copyright (C) 1992-1996 FrexxWare
# Author: Daniel Stenberg

.SUFFIXES: .o .c .c~ .h .h~ .a .i

################################################
# Below follows all UNIX macros/actions/lines: #
################################################
LIB     = libfpl.a
TARGET  = FPL
OBJS    = script.o numexpr.o caller.o hash.o statement.o memory.o frontend.o \
          reference.o sprintf.o scan.o compile.o sscanf.o
LIBOBJS = script.o numexpr.o hash.o statement.o memory.o frontend.o \
          reference.o sprintf.o scan.o compile.o sscanf.o
LIBDEP  = $(LIBOBJS)

# if using the `gcc' compiler:
CFLAGS  = -DUNIX -DSHARED -ansi -pedantic -g -funsigned-char
CC      = gcc
LD      = ld

UNAME := $(shell uname)
TARGETOS=$(UNAME)

ifdef CROSS
TARGETOS=$(CROSS)
ifeq ($(CROSS),AROS)
CFLAGS	= -DAMIGA -D__AMIGA__ -D__AROS__  -ansi -pedantic -g -funsigned-char
CC	= gcc
LD	= ld
endif

ifneq ($(UNAME),AROS)
CC	= i386-aros-gcc
LD	= i386-aros-ld
endif
endif

ifeq ($(TARGETOS),AROS)
LDFLAGS += -larosc -ldos -lexec
endif


# -G makes the destination to become a sharable object!
#    -B symbolic  In dynamic mode only, when  building  a  shared
#                 object,  bind  references  to  global symbols to
#                 their definitions within the object, if  defini-
#                 tions  are  available.   Normally, references to
#                 global symbols within  shared  objects  are  not
#                 bound  until  runtime,  even  if definitions are
#                 available, so that definitions of the same  sym-
#                 bol in an executable or other shared objects can
#                 override the object's own definition.
# (The Send() function collided with the one with the same name used in
# Dancer.)
#

all:	$(LIB) SFPL

$(LIB) : $(LIBDEP)
#	$(LD) -r $(LDFLAGS) $(LIBOBJS) -o $(LIB).o
	$(AR) cr $(LIB).tmp $(LDFLAGS) $(LIBOBJS)
	objcopy -w --strip-unneeded --localize-symbol='!fpl*'  $(LIB).tmp $(LIB)
	cp -p FPL.h ../include/libraries

$(TARGET) : $(OBJS)
	$(CC) $(OBJS) -o $(TARGET)

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *~ $(LIB) SFPL

# This compiling is using -DSFPL to make the MALLOC() macro not to use the
# global `mem' variable!
SFPL: caller.o FPL.h script.h
	$(CC) $(CFLAGS) -DSFPL -o SFPL caller.o -L. -lfpl $(LDFLAGS)

$(OBJS): script.h FPL.h

caller.o:caller.c
script.o:script.c
numexpr.o:numexpr.c
caller.o:caller.c
statement.o:statement.c
hash.o:hash.c
memory.o:memory.c memory.h
frontend.o:frontend.c
reference.o:reference.c
sprintf.o:sprintf.c
scan.o:scan.c
