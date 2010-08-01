########################################################################
#                                                                      #
# fpl - A shared library interpreting script langauge.                 #
# Copyright (C) 1992-1998 FrexxWare                                    #
# Author: Daniel Stenberg                                              #
#                                                                      #
# This program is distributed in the hope that it will be useful,      #
# but WITHOUT ANY WARRANTY; without even the implied warranty of       #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                 #
#                                                                      #
# Daniel Stenberg <Daniel.Stenberg@sth.frontec.se>                     #
#                                                                      #
########################################################################

all:
	@echo "no default action"

tgz:
	@(dir=`pwd`;name=`basename $$dir`;echo Creates $$name.tar.gz; cd .. ; \
	tar -cf $$name.tar `cat $$name/FILES | sed "s:^:$$name/:g"` ; \
	gzip $$name.tar ; chmod a+r $$name.tar.gz ; mv $$name.tar.gz $$name/)

clean:
	rm -f *~
	cd src; make clean
	cd compile; make clean
