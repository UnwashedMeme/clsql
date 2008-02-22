;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-postgresql-socket.asd
;;;; Purpose:       ASDF file for CLSQL PostgresSQL socket backend
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

(defpackage #:clsql-postgresql-socket-system (:use #:asdf #:cl))
(in-package #:clsql-postgresql-socket-system)

;;; System definition

(defsystem clsql-postgresql-socket
  :name "cl-sql-postgresql-socket"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL PostgreSQL Socket Driver"
  :long-description "cl-sql-postgresql-socket package provides a database driver to the PostgreSQL database via a socket interface."

  :depends-on (clsql cffi-uffi-compat md5 #+sbcl sb-bsd-sockets)
  :components
  ((:module :db-postgresql-socket
	    :components
	    ((:file "postgresql-socket-package")
	     (:file "postgresql-socket-api"
		    :depends-on ("postgresql-socket-package"))
	     (:file "postgresql-socket-sql"
		    :depends-on ("postgresql-socket-api"))))))
