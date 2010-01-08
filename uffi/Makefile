#!/usr/bin/make
# FILE IDENTIFICATION
#
#  Name:         Makefile
#  Purpose:      Makefile for CLSQL UFFI interface
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
# This file, part of CLSQL, is Copyright (c) 2002-2006 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.

SUBDIRS=

include ../Makefile.common

base=clsql_uffi
source=$(base).c
object=$(base).o
shared_lib=$(base).so
shared64_lib=$(base)64.so

.PHONY: all
all: $(shared_lib)

$(shared_lib): $(source) Makefile
ifneq ($(OS_AIX),0)
	gcc -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $(source)
	make_shared -o $(shared_lib) $(object)
else
  ifneq ($(OS_SUNOS),0)
	cc -KPIC -c $(source) -o $(object)
	cc -G $(object) -o $(shared_lib)
  else
    ifneq ($(OS_DARWIN),0)
	cc -dynamic -c $(source) -o $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $(base).dylib $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib
    else
      ifneq ($(OS_CYGWIN),0)
	gcc -c $(source) -o $(object)
	ld -shared -soname=$(base) $(LDFLAGS) $(object) -o $(shared_lib)
      else
        ifneq ($(OS_LINUX64),0)
	  gcc -fPIC -DPIC -c $(source) -o $(object)
	  ld -shared -soname=$(base) -lc $(object) -o $(shared64_lib)
	  rm -f $(object)
	  @echo "Ensure that you have multiarch i386 build tools if you want to build 32-bit library"
	  -gcc -m32 -fPIC -DPIC -c $(source) -o $(object)
	  -ld -melf_i386 -shared -soname=$(base) -lc $(object) -o $(shared_lib)
        else
	  gcc -fPIC -DPIC -c $(source) -o $(object)
	  ld -shared -soname=$(base) -lc $(object) -o $(shared_lib)
        endif
      endif
    endif
  endif
endif
	rm -f $(object)


.PHONY: distclean
distclean: clean
	rm -f $(base).dylib $(base).dylib $(base).so $(base).o
