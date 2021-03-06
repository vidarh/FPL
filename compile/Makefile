# $VER: FPL Makefile v1.0 94-08-11

########################################################################
#                                                                      #
# fpl.library - A shared library interpreting script langauge.         #
# Copyright (C) 1992-1994 FrexxWare                                    #
# Author: Daniel Stenberg                                              #
#                                                                      #
# This program is free software; you may redistribute for non          #
# commercial purposes only. Commercial programs must have a written    #
# permission from the author to use FPL. FPL is *NOT* public domain!   #
# Any provided source code is only for reference and for assurance     #
# that users should be able to compile FPL on any operating system     #
# he/she wants to use it in!                                           #
#                                                                      #
# You may not change, resource, patch files or in any way reverse      #
# engineer anything in the FPL package.                                #
#                                                                      #
# This program is distributed in the hope that it will be useful,      #
# but WITHOUT ANY WARRANTY; without even the implied warranty of       #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 #
#                                                                      #
# Daniel Stenberg                                                      #
# Ankdammsgatan 36, 4tr                                                #
# S-171 43 Solna                                                       #
# Sweden                                                               #
#                                                                      #
# FidoNet 2:201/328    email:dast@sth.frontec.se                       #
#                                                                      #
########################################################################

.SUFFIXES: .o .c .c~ .h .h~ .a .i

################################################
# Below follows all UNIX macros/actions/lines: #
################################################

TARGET  = FPLc
OBJS = script.o numexpr.o hash.o statement.o error.o debugmem.o\
       frontend.o scan.o caller.o comp.o pass2.o pass3.o optimize.o

# if using the `gcc' compiler:
CFLAGS  = -DUNIX -ansi -pedantic -pipe -funsigned-char
CC      = gcc
LD      = ld

all:	$(TARGET)

$(TARGET) : $(OBJS)
	$(CC) $(OBJS) -o $(TARGET)

clean:
	cd cpp; make clean
	rm -f *.o *~ $(TARGET)

.c.o:
	$(CC) $(CFLAGS) -c $<

$(OBJS): script.h FPL.h Makefile.FILE

script.o:script.c
numexpr.o:numexpr.c
caller.o:caller.c
statement.o:statement.c
hash.o:hash.c
memory.o:memory.c memory.h
frontend.o:frontend.c
scan.o:scan.c
error.o:error.c
comp.o:comp.c
pass2.o: pass2.c comp.h script.h
pass3.o: pass3.c comp.h script.h
optimize.o: optimize.c optimize.h comp.h script.h
debugmem.o: debugmem.c debugmem.h
