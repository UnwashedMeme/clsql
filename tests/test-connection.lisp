;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     test-connection.lisp
;;;; Purpose:  Tests for CLSQL database connections
;;;; Authors:  Marcus Pearce and Kevin M. Rosenberg
;;;; Created:  March 2004
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)

(setq *rt-connection*
      '(

(deftest :connection/1
    (let ((database (clsql:find-database
                     (clsql:database-name clsql:*default-database*)
                     :db-type (clsql-sys:database-type clsql:*default-database*))))
      (eql (clsql-sys:database-type database) *test-database-type*))
  t)

(deftest :connection/2
    (clsql-sys::string-to-list-connection-spec
     "localhost/dbname/user/passwd")
  ("localhost" "dbname" "user" "passwd"))

(deftest :connection/3
    (clsql-sys::string-to-list-connection-spec
     "dbname/user@hostname")
  ("hostname" "dbname" "user"))

(deftest :connection/execute-command
    ;;check that we can issue basic commands.
    (values
      (clsql-sys:execute-command "CREATE TABLE DUMMY (foo integer)")
      (clsql-sys:execute-command "DROP TABLE DUMMY"))
  nil nil)

(deftest :connection/query
    ;;check that we can do a basic query
    (first (clsql:query "SELECT 1" :flatp t :field-names nil))
  1)

(deftest :connection/query-command
    ;;queries that are commands (no result set) shouldn't cause breakage
    (values
      (clsql-sys:query "CREATE TABLE DUMMY (foo integer)")
      (clsql-sys:query "DROP TABLE DUMMY"))
  nil nil)

(deftest :connection/pool/procedure-mysql
 (unwind-protect
      (progn
        (clsql-sys:disconnect)
        (test-connect :pool t)
        (clsql-sys:execute-command
         "CREATE PROCEDURE prTest () BEGIN SELECT 1 \"a\",2 \"b\",3 \"c\" ,4  \"d\" UNION SELECT 5,6,7,8; END;")
        (clsql-sys:disconnect)
        (test-connect :pool t)
        (let ((p0 (clsql-sys:query "CALL prTest();" :flatp t)))
          (clsql-sys:disconnect)
          (test-connect :pool t)
          (let ((p1 (clsql-sys:query "CALL prTest();" :flatp t)))
            (clsql-sys:disconnect)
            (test-connect :pool t)
            (values p0 p1))))
   (ignore-errors
    (clsql-sys:execute-command "DROP PROCEDURE prTest;"))
   (test-connect))
 ((1 2 3 4) (5 6 7 8))
 ((1 2 3 4) (5 6 7 8)))

))
