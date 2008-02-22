;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-odbc.asd
;;;; Purpose:  ASDF definition file for CLSQL ODBC backend
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  April 2004
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 200d42 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-odbc-system (:use #:asdf #:cl))
(in-package #:clsql-odbc-system)

(defsystem clsql-odbc
  :name "clsql-odbc"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL ODBC Driver"
  :long-description "cl-sql-odbc package provides a database driver to the ODBC database system."

  :depends-on (cffi-uffi-compat clsql clsql-uffi)
  :components
  ((:module :db-odbc
	    :components
	    ((:file "odbc-package")
	     (:file "odbc-loader" :depends-on ("odbc-package"))
	     (:file "odbc-constants" :depends-on ("odbc-loader"))
	     (:file "odbc-ff-interface" :depends-on ("odbc-constants"))
	     (:file "odbc-api" :depends-on ("odbc-ff-interface" "odbc-constants"))
	     (:file "odbc-dbi" :depends-on ("odbc-api"))
	     (:file "odbc-sql" :depends-on ("odbc-dbi"))))))

