#!/usr/bin/make
# FILE IDENTIFICATION
#
#  Name:         Makefile
#  Purpose:      Makefile for CLSQL MySQL interface
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
# This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

SUBDIRS=

include ../Makefile.common

base=clsql_mysql
source=$(base).c
object=$(base).o
shared_lib=$(base).so
shared64_lib=$(base)64.so


.PHONY: all
all: $(shared_lib)

CFLAGS=-I /usr/local/include/mysql -I /usr/include/mysql -I /sw/include/mysql -I /opt/local/include/mysql -I /opt/local/include/mysql5/mysql -I /usr/local/mysql/include
LDFLAGS=-L/usr/local/lib64/mysql -L/usr/local/mysql/lib32 -L/usr/local/lib/mysql -L/usr/lib/mysql -L/sw/lib -L/opt/local/lib/mysql -L/opt/local/lib/mysql5/mysql -L/usr/local/mysql/lib -L/usr/lib/gcc/i686-pc-cygwin/3.4.4 -lmysqlclient -lz -lc

CFLAGS32=-I /usr/local/include/mysql -I /usr/include/mysql -I /sw/include/mysql -I /opt/local/include/mysql -I /opt/local/include/mysql5/mysql -I /usr/local/mysql/include -m32
LDFLAGS32=-L/lib32 -L/usr/lib32 -melf_i386 -lmysqlclient -lz -lc

ifneq ($(OS_CYGWIN),0)
  CFLAGS=-I /cygdrive/c/Program\ Files/MySQL/MySQL\ Server\ 5.0/include
  LDFLAGS=-L/usr/local/lib64/mysql -L/usr/local/lib/mysql -L/usr/lib/mysql -L/sw/lib -L/opt/local/lib/mysql -L/usr/lib/gcc/i686-pc-cygwin/3.4.4 -lmysqlclient -lpthread -lz -lm -lgcc -lc
endif

$(shared_lib): $(source) Makefile
ifneq ($(OS_AIX),0)
	gcc $(CFLAGS) -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $(source)
	make_shared $(LDFLAGS) -o $(shared_lib) $(object)
else
  ifneq ($(OS_SUNOS),0)
	cc $(CFLAGS) -KPIC -c $(source) -o $(object)
	cc -G $(object) $(LDFLAGS) -o $(shared_lib)
  else
    ifneq ($(OS_DARWIN),0)
	cc $(CFLAGS) -dynamic -c $(source) -o $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $(base).dylib $(object)
	ld -bundle /usr/lib/bundle1.o $(LDFLAGS) -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib
    else
      ifneq ($(OS_CYGWIN),0)
	gcc $(CFLAGS) -DWIN32 -c $(source) -o $(object)
	ld -shared -soname=$(base)  $(object) $(LDFLAGS) -o $(shared_lib)
      else
        ifneq ($(OS_LINUX64),0)
	  gcc $(CFLAGS) -fPIC -c $(source) -o $(object)
	  ld -shared -soname=$(base) $(object) $(LDFLAGS) -o $(shared64_lib)
	  rm $(object)
	  @echo "Ensure that you have multiarch i386 build tools and libraries if you want to build 32-bit library"
	  -gcc $(CFLAGS32) -fPIC -c $(source) -o $(object)
	  -ld -shared -soname=$(base) $(object) $(LDFLAGS32) -o $(shared_lib)
        else
	  gcc $(CFLAGS) -fPIC -c $(source) -o $(object)
	  ld -shared -soname=$(base) $(object) $(LDFLAGS) -o $(shared_lib)
        endif
      endif
    endif
  endif
endif
	rm -f $(object)

.PHONY: distclean
distclean: clean
	rm -f $(base).dylib $(base).dylib $(base).so $(base).o
