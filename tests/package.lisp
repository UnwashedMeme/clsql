;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     package.lisp
;;;; Purpose:  Package definition for CLSQL test suite
;;;; Authors:  Marcus Pearce and Kevin M. Rosenberg
;;;; Created:  March 2004
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage #:clsql-tests
  (:use #:clsql #:common-lisp #:rtest)
  (:shadowing-import-from #:clsql-sys #:%get-int )
  (:export
   #:run-tests
   #:run-tests-append-report-file
   #:run-benchmarks
   #:run-benchmarks-append-report-file
   #:summarize-test-report
   #:test-initialise-database
   #:test-connect-to-database
   )
  (:documentation "Regression tests for CLSQL."))

