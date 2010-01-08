;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     mysql-loader.sql
;;;; Purpose:  MySQL library loader using UFFI
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

;; searches clsql_mysql64 to accomodate both 32-bit and 64-bit libraries on same system
(defparameter *clsql-mysql-library-candidate-names*
  `(,@(when (> most-positive-fixnum (expt 2 32)) (list "clsql_mysql64"))
    "clsql_mysql"))

(defvar *mysql-library-candidate-names*
  '("libmysqlclient" "libmysql"))

(defvar *mysql-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the MySQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *mysql-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :mysql)))
  *mysql-library-loaded*)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :mysql)))
  (clsql:push-library-path clsql-mysql-system::*library-file-dir*)

  (clsql-uffi:find-and-load-foreign-library *mysql-library-candidate-names*
                                            :module "mysql"
                                            :supporting-libraries *mysql-supporting-libraries*)

  (clsql-uffi:find-and-load-foreign-library *clsql-mysql-library-candidate-names*
                                            :module "clsql-mysql"
                                            :supporting-libraries *mysql-supporting-libraries*)
  (setq *mysql-library-loaded* t))


(clsql-sys:database-type-load-foreign :mysql)

