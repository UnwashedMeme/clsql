;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-postgresql.asd
;;;; Purpose:       ASDF file for CLSQL PostgresSQL backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-postgresql-system (:use #:asdf #:cl))
(in-package #:clsql-postgresql-system)

#+(and allegro macosx) (push "so" excl::*load-foreign-types*)

(defsystem clsql-postgresql
  :name "cl-sql-postgresql"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp PostgreSQL API Driver"
  :long-description "cl-sql-postgresql package provides a the database driver for the PostgreSQL API."

  :depends-on (cffi-uffi-compat clsql clsql-uffi)
  :components
  ((:module :db-postgresql
	    :components
	    ((:file "postgresql-package")
	     (:file "postgresql-loader" :depends-on ("postgresql-package"))
	     (:file "postgresql-api" :depends-on ("postgresql-loader"))
	     (:file "postgresql-sql" :depends-on ("postgresql-api"))))))
