;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-postgresql-socket.asd
;;;; Purpose:       ASDF file for CLSQL PostgresSQL socket (protocol vs 3) backend
;;;; Programmer:    Russ Tyndall
;;;; Date Started:  Sept 2009
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

(defsystem clsql-postgresql-socket3
  :name "cl-sql-postgresql-socket3"
  :author "Russ Tyndall <russ@acceleration.net>"
  :maintainer "Russ Tyndall <russ@acceleration.net>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL PostgreSQL Socket Driver"
  :long-description "cl-sql-postgresql-socket package provides a database driver to the PostgreSQL database via a socket interface."

  :depends-on (clsql md5 :cl-postgres #+sbcl sb-bsd-sockets)
  :components
  ((:module :db-postgresql-socket3
	    :serial T
	    :components ((:file "package")
			 (:file "api")
			 (:file "sql")))))
