;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    utils.lisp
;;;; Purpose: Classes and utilities for testing
;;;; Author:  Kevin M. Rosenberg
;;;; Created: Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)

(defun %get-int (v)
  (etypecase v
    (string (parse-integer v :junk-allowed t))
    (integer v)
    (number (truncate v))))

(defvar *config-pathname*
  (make-pathname :defaults (user-homedir-pathname)
                 :name ".clsql-test"
                 :type "config"))

(defvar +all-db-types+
  '(:postgresql :postgresql-socket :postgresql-socket3 :mysql :sqlite :sqlite3 :odbc :oracle
    #+allegro :aodbc))

(defclass conn-specs ()
  ((aodbc :accessor aodbc-spec :initform nil)
   (mysql :accessor mysql-spec :initform nil)
   (postgresql :accessor postgresql-spec :initform nil)
   (postgresql-socket :accessor postgresql-socket-spec :initform nil)
   (postgresql-socket3 :accessor postgresql-socket3-spec :initform nil)
   (sqlite :accessor sqlite-spec :initform nil)
   (sqlite3 :accessor sqlite3-spec :initform nil)
   (odbc :accessor odbc-spec :initform nil)
   (oracle :accessor oracle-spec :initform nil))
  (:documentation "Connection specs for CLSQL testing"))


(defun read-specs (&optional (path *config-pathname*))
  (if (probe-file path)
      (with-open-file (stream path :direction :input)
        (let ((specs (make-instance 'conn-specs)))
          (dolist (spec (read stream) specs)
            (push (second spec)
                  (slot-value specs (intern (symbol-name (first spec))
                                            (find-package '#:clsql-tests)))))))
      (progn
        (warn "CLSQL test config file ~S not found" path)
        nil)))

(defun spec-fn (db-type)
  (intern (concatenate 'string (symbol-name db-type)
                       (symbol-name '#:-spec))
          (find-package '#:clsql-tests)))

(defun db-type-spec (db-type specs)
  (funcall (spec-fn db-type) specs))


(defun summarize-test-report (sexp &optional (output *standard-output*))
  (flet ((db-title (db-type underlying-db-type)
           (format nil "~A~A"
                   db-type
                   (if (eq db-type underlying-db-type)
                       ""
                       (format nil "/~A" underlying-db-type)))))
    (with-open-file (in sexp :direction :input)
      (let ((eof (cons nil nil)))
        (do ((form (read in nil eof) (read in nil eof)))
            ((eq form eof))
          (destructuring-bind (db-type
                               underlying-db-type
                               utime
                               total-tests
                               failed-tests
                               impl-type
                               impl-version
                               machine-type)
              form
            (declare (ignorable utime impl-version))
            (if failed-tests
                (format output "~&~A: ~D of ~D tests failed (~A, ~A).~&"
                        (db-title db-type underlying-db-type)
                        (length failed-tests)
                        total-tests
                        machine-type
                        impl-type)
                (format output "~&~A: All ~D tests passed (~A, ~A).~%"
                        (db-title db-type underlying-db-type)
                        total-tests
                        machine-type
                        impl-type))))))))
