##############################################################################
# FILE IDENTIFICATION
# 
#  Name:         Makefile
#  Purpose:      Makefile for the clsql documentation
#  Programer:    Kevin M. Rosenberg
#  Date Started: Mar 2002
#
#  CVS Id:   $Id$
#
# This file, part of CLSQL, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
#
# CLSQL users are granted the rights to distribute and use this software
# as governed by the terms of the Lisp Lesser GNU Public License
# (http://opensource.franz.com/preamble.html), also known as the LLGPL.
##############################################################################

DOCFILE_BASE_DEFAULT:=clsql
DOCFILE_EXT_DEFAULT:=xml


# Standard docfile processing

DEBIAN=$(shell expr "`cat /etc/issue 2> /dev/null`" : '.*Debian.*')
SUSE=$(shell expr "`cat /etc/issue 2> /dev/null`" : '.*SuSE.*')
SUSE91=$(shell expr "`cat /etc/issue 2> /dev/null`" : '.*SuSE Linux 9.1.*')
REDHAT=$(shell expr "`cat /etc/issue 2> /dev/null`" : '.*Red Hat.*')
MANDRAKE=$(shell expr "`cat /etc/issue 2> /dev/null`" : '.*Mandrake.*')
DARWIN=$(shell expr "`uname -a`" : '.*Darwin.*')


ifneq (${DEBIAN},0)
OS:=debian
else
  ifneq (${SUSE91},0)
    OS=suse91
  else
    ifneq (${SUSE},0)
      OS=suse
    else   
      ifneq (${REDHAT},0)
        OS=redhat
      else   
        ifneq (${MANDRAKE},0)
          OS=mandrake
	else   
          ifneq (${DARWIN},0)
            OS=darwin
	  endif 
        endif
      endif
    endif
  endif
endif

ifndef DOCFILE_BASE
DOCFILE_BASE=${DOCFILE_BASE_DEFAULT}
endif

ifndef DOCFILE_EXT
DOCFILE_EXT=${DOCFILE_EXT_DEFAULT}
endif

DOCFILE:=${DOCFILE_BASE}.${DOCFILE_EXT}
FOFILE:=${DOCFILE_BASE}.fo
PDFFILE:=${DOCFILE_BASE}.pdf
PSFILE:=${DOCFILE_BASE}.ps
DVIFILE:=${DOCFILE_BASE}.dvi
TXTFILE:=${DOCFILE_BASE}.txt
HTMLFILE:=${DOCFILE_BASE}.html
TMPFILES:=${DOCFILE_BASE}.aux ${DOCFILE_BASE}.out ${DOCFILE_BASE}.log
DOCFILES:=$(shell echo *.xml *.xsl)

ifeq ($(XSLTPROC),)
  XSLTPROC:=xsltproc
endif

CATALOG:=`pwd`/catalog-${OS}.xml
CHECK:=XML_CATALOG_FILES="$(CATALOG)" xmllint --noout --xinclude --postvalid $(DOCFILE) || exit 1

.PHONY: all
all: html pdf 

.PHONY: dist
dist: html pdf

.PHONY: doc
doc: html pdf

.PHONY: check
check:
	@echo "Operating system detected: ${OS}"
	@$(CHECK)

.PHONY: html
html: html.tar.gz

html.tar.gz: $(DOCFILES) Makefile 
	@rm -rf html
	@mkdir html
	@XML_CATALOG_FILES="$(CATALOG)" $(XSLTPROC) --stringparam chunker.output.encoding UTF-8 \
		 --xinclude --output html/ html_chunk.xsl $(DOCFILE)
	@GZIP='-9' tar czf html.tar.gz html 

.PHONY: fo
fo: ${FOFILE}

${FOFILE}: $(DOCFILES) Makefile 
	@XML_CATALOG_FILES="$(CATALOG)" xsltproc --xinclude --stringparam fop1.extensions 1 --output $(FOFILE) fo.xsl $(DOCFILE)

.PHONY: pdf
pdf: ${PDFFILE}

${PDFFILE}: ${DOCFILES} Makefile
	@$(MAKE) fo
	@fop $(FOFILE) -pdf $(PDFFILE) > /dev/null

.PHONY: dvi
dvi: ${DVIFILE}

.PHONY: ps
ps: ${PSFILE}

${PSFILE}: ${DOCFILES} Makefile
	@$(MAKE) fo
	@fop $(FOFILE) -ps $(PSFILE) > /dev/null


.PHONY: txt
txt: ${TXTFILE}

${TXTFILE}: ${FOFILE}
	@XML_CATALOG_FILES="$(CATALOG)" xsltproc --xinclude --output ${HTMLFILE} html.xsl $(DOCFILE)
	lynx -dump ${HTMLFILE} > ${TXTFILE}

.PHONY: clean
clean: 
	@rm -f *~ *.bak *.orig \#*\# .\#* texput.log
	@rm -rf html ${PSFILE} ${HTMLFILE}
	@rm -f ${TMPFILES} ${FOFILE}
	@rm -f  ${DVIFILE} ${TXTFILE}

.PHONY: distclean
distclean: clean
