;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-clsql.cl
;;;; Purpose:       Basic test of CLSQL
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id: test-clsql.cl,v 1.9 2002/03/26 14:12:12 kevin Exp $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)


(defvar *config-pathname* (make-pathname :name "test-clsql"
					 :type "config"
					 :defaults *load-truename*))
(defparameter *config* nil)

(defun do-test (&optional (interactive nil))
  (if interactive
      (test-interactive)
    (if (probe-file *config-pathname*)
	(with-open-file (stream *config-pathname* :direction :input)
	  (setq *config* (read stream))
	  (test-automated *config*))
      (test-interactive))))
  
(defun test-interactive ()
  (do ((done nil))
      (done)
    (multiple-value-bind (spec type) (get-spec-and-type)
      (if spec
	  (clsql-test-table spec type)
	  (setq done t)))))

(defun test-automated (config)
  (dolist (elem config)
    (let ((type (car elem))
	  (spec (cadr elem)))
      #-allegro
      (unless (eq type :aodbc)
	(clsql-test-table spec type))
      #+allegro
      (clsql-test-table spec type)))
  )


(defun create-test-table (db)
  (ignore-errors
    (clsql:execute-command 
     "DROP TABLE test_clsql" :database db))
  (clsql:execute-command 
   "CREATE TABLE test_clsql (n integer, n_pi float, n_pi_str CHAR(20))" 
   :database db)
  (dotimes (i 11)
    (let ((n (- i 5)))
      (clsql:execute-command
       (format nil "INSERT INTO test_clsql VALUES (~a,~a,'~a')"
	       n (clsql:float-to-sql-string (* pi n))
	       (clsql:float-to-sql-string (* pi n)))
       :database db))))

(defun drop-test-table (db)
  (clsql:execute-command "DROP TABLE test_clsql"))

(defun clsql-test-table (spec type)
  (when (eq type :mysql)
    (test-clsql-mysql spec))
  (let ((db (clsql:connect spec :database-type type :if-exists :new)))
    (unwind-protect
	(progn
	  (create-test-table db)
	  (pprint (clsql:query "select * from test_clsql" 
			       :database db
			       :types :auto))
	  (pprint (clsql:map-query 'vector #'list "select * from test_clsql" 
				   :database db
				   :types :auto)) ;;'(:int :double t)))
	  (drop-test-table db))
      (clsql:disconnect :database db)))
  )

(defun test-clsql-mysql (spec)
  (let ((db (clsql-mysql::database-connect spec :mysql)))
    (clsql-mysql::database-execute-command "DROP TABLE IF EXISTS test_clsql" db)
    (clsql-mysql::database-execute-command 
     "CREATE TABLE test_clsql (i integer, sqrt double, sqrt_str CHAR(20))" db)
    (dotimes (i 10)
      (clsql-mysql::database-execute-command
       (format nil "INSERT INTO test_clsql VALUES (~d,~d,'~a')"
	       i (sqrt i) (format nil "~d" (sqrt i)))
       db))
    (let ((res (clsql-mysql::database-query-result-set "select * from test_clsql" db :full-set t :types nil)))
      (format t "~&Number rows: ~D~%" (mysql:mysql-num-rows (clsql-mysql::mysql-result-set-res-ptr res)))
      (clsql-mysql::database-dump-result-set res db))
    (clsql-mysql::database-execute-command "DROP TABLE test_clsql" db)
    (clsql-mysql::database-disconnect db)))


(defun get-spec-and-type ()
  (format t "~&Test CLSQL")
  (format t "~&==========~%")
  (format t "~&Enter connection type (:mysql :postgresql :postgresql-socket")
  #+allegro (format t " :aodbc")
  (format t ") [default END]: ")
  (let ((type-string (read-line)))
    (if (zerop (length type-string))
	(values nil nil)
	(get-spec-for-type (read-from-string type-string)))))

(defun get-spec-for-type (type)
  (let ((spec (get-spec-using-format type
			(ecase type
			  ((:mysql :postgresql :postgresql-socket)
			   '("host" "database" "user" "password"))
			  (:aodbc
			   '("dsn" "user" "password"))))))
    (values spec type)))


(defun get-spec-using-format (type spec-format)
  (let (spec)
    (format t "~&Connection Spec for ~A" (symbol-name type))
    (format t "~&------------------------------")
    
    (dolist (elem spec-format)
      (format t "~&Enter ~A: " elem)
      (push (read-line) spec))
    (nreverse spec)))