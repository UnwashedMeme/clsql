;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     odbc-package.lisp
;;;; Purpose:  Package definition for low-level ODBC interface
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  April 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:odbc
  (:use #:cl #:uffi)
  (:export
     #:database-library-loaded

     #:*null*
     #:+null-ptr+
     #:+max-precision+
     #:*info-output*
     #:*time-conversion-function*
     #:get-cast-long
     #:%free-statement
     #:%close-cursor
     #:%disconnect
     #:%commit
     #:%rollback
     #:%sql-fetch
     #:%sql-cancel
     #:db-connect
     #:%new-db-connection-handle
     #:%new-environment-handle
     #:%sql-connect
     #:%sql-driver-connect
     #:disable-autocommit
     #:enable-autocommit
     #:%sql-free-environment
     #:%sql-data-sources
     #:%sql-get-info
     #:%sql-param-data
     #:%sql-execute
     #:%put-str
     #:%sql-bind-parameter
     #:%sql-prepare
     #:sqlfetch
     #:%bind-column
     #:%allocate-bindings
     #:%describe-column
     #:%describe-columns
     #:read-data
     #:read-data-in-chunks
     #:query-database
     #:%new-statement-handle
     #:%sql-exec-direct
     #:result-columns-count
     #:result-rows-count
     #:sql-to-c-type
     #:%list-tables
     #:%table-statistics
     #:%list-data-sources
     )
  (:documentation "This is the low-level interface ODBC."))

