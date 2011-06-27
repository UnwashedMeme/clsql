;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; File:     clsql-tests.asd
;;;; Authors:  Marcus Pearce and Kevin Rosenberg
;;;; Created:  30/03/2004
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:clsql-tests-system (:use #:asdf #:cl))
(in-package #:clsql-tests-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:uffi)
    (asdf:operate 'asdf:load-op 'uffi)))

(defsystem clsql-tests
    :name "CLSQL Tests"
    :author ""
    :maintainer ""
    :version ""
    :licence ""
    :description "A regression test suite for CLSQL."
    :depends-on (clsql rt)
    :components
    ((:module tests
	      :serial t
	      :components ((:file "package")
			   (:file "utils")
			   (:file "test-init")
			   (:file "datasets")
			   (:file "ds-employees")
			   (:file "ds-nodes")
			   (:file "ds-artists")
			   (:file "benchmarks")
			   (:file "test-internal")
			   (:file "test-basic")
			   (:file "test-time")
			   (:file "test-connection")
			   (:file "test-fddl")
			   (:file "test-fdml")
			   (:file "test-ooddl")
			   (:file "test-oodml")
			   (:file "test-syntax")
                           (:file "test-pool")
                           ; #-uffi:no-i18n (:file "test-i18n")
                           ))))

(defmethod perform ((o test-op) (c (eql (find-system 'clsql-tests))))
  (operate 'load-op 'clsql)
  (unless (funcall (intern (symbol-name '#:run-tests)
			   (find-package '#:clsql-tests)))
    (error "test-op failed")))
