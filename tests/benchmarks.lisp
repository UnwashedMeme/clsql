;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    benchmarks.lisp
;;;; Authors: Kevin Rosenberg
;;;; Created: 03/05/2004
;;;; Updated: $Id$
;;;;
;;;; Benchmark suite
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

(defun run-benchmarks-append-report-file (report-file)
  (run-function-append-report-file 'run-benchmarks report-file))

(clsql:def-view-class bench ()
  ((a :initarg :a
      :type integer)
   (b :initarg :b
      :type (string 100))
   (c :initarg :c
      :type float)))

(defun run-benchmarks (&key (report-stream *standard-output*) (sexp-report-stream nil) (count 10000))
  (let ((specs (read-specs))
        (*report-stream* report-stream)
        (*sexp-report-stream* sexp-report-stream))
    (unless specs
      (warn "Not running benchmarks because test configuration file is missing")
      (return-from run-benchmarks :skipped))
    (load-necessary-systems specs)
    (dolist (db-type +all-db-types+)
      (dolist (spec (db-type-spec db-type specs))
        (do-benchmarks-for-backend db-type spec count))))
  (values))

(defun do-benchmarks-for-backend (db-type spec count)
  (test-connect-to-database db-type spec)
  (write-report-banner "Benchmarks" db-type *report-stream*)

  (create-view-from-class 'bench)
  (benchmark-init)
  (benchmark-selects count)
  (drop-view-from-class 'bench))

(defun benchmark-init ()
  (dotimes (i 10)
    (execute-command "INSERT INTO BENCH (A,B,C) VALUES (123,'A Medium size string',3.14159)")))

(defun benchmark-selects (n)
  (let ((*trace-output* *report-stream*))
    (format *report-stream* "~&~%*** QUERY ***~%")
    (time
     (dotimes (i n)
       (query "SELECT * FROM BENCH")))
    (format *report-stream* "~&~%*** QUERY WITH RESULT-TYPES NIL ***~%")
    (time
     (dotimes (i n)
       (query "SELECT * FROM BENCH" :result-types nil)))
    (format *report-stream* "~&~%*** QUERY WITH FIELD-NAMES NIL ***~%")
    (time
     (dotimes (i n)
       (query "SELECT * FROM BENCH" :field-names nil)))

    (with-dataset *ds-employees*
      (format *report-stream* "~&~%*** JOINED OBJECT QUERY RETRIEVAL IMMEDIATE ***~%")
      (time
       (dotimes (i (truncate n 10))
	 (mapcar #'(lambda (ea) (slot-value ea 'address)) (select 'employee-address :flatp t))))

      (format *report-stream* "~&~%*** JOINED OBJECT QUERY RETRIEVAL DEFERRED ***~%")
      (let* ((slotdef (find 'address (clsql-sys::class-slots (find-class 'employee-address))
			    :key #'clsql-sys::slot-definition-name))
	     (dbi (when slotdef (clsql-sys::view-class-slot-db-info slotdef))))
	(setf (gethash :retrieval dbi) :deferred)
	(time
	 (dotimes (i (truncate n 10))
	   (mapcar #'(lambda (ea) (slot-value ea 'address)) (select 'employee-address :flatp t))))
	(setf (gethash :retrieval dbi) :immediate)))))
