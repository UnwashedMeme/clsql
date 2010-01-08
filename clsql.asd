;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql.asd
;;;; Purpose:  System definition for CLSQL-CLASSIC
;;;; Authors:  Marcus Pearce and Kevin M. Rosenberg
;;;; Created:  March 2004
;;;;
;;;; $Id$
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:clsql-system (:use #:asdf #:cl))
(in-package #:clsql-system)

#+clisp
(progn
  (asdf:operate 'asdf:load-op 'cffi)
  (asdf:operate 'asdf:load-op 'cffi-uffi-compat)
  (asdf:defsystem uffi :depends-on (cffi-uffi-compat)))

;; need to load uffi for below perform :after method
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'uffi)
    (asdf:operate 'asdf:load-op 'uffi)))

(defsystem clsql
    :name "CLSQL"
    :author "Kevin Rosenberg <kevin@rosenberg.net>"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "Lessor Lisp General Public License"
    :description "Common Lisp SQL Interface library"
    :long-description "A Common Lisp interface to SQL RDBMS based on
the Xanalys CommonSQL interface for Lispworks. It depends on the
low-level database interfaces as well as a functional and an object
oriented interface."
    :components
    ((:module sql
	      :components
	      ((:module base
			:pathname ""
			:components
			((:file "cmucl-compat")
			 (:file "package")
			 (:file "kmr-mop" :depends-on ("package"))
			 (:file "base-classes" :depends-on ("package"))
                         (:file "conditions" :depends-on ("base-classes"))
                         (:file "db-interface" :depends-on ("conditions"))
			 (:file "utils" :depends-on ("package" "db-interface"))
			 (:file "time" :depends-on ("package" "conditions" "utils"))
                         (:file "generics" :depends-on ("package"))))
               (:module database
                        :pathname ""
                        :components
                        ((:file "initialize")
			 (:file "database" :depends-on ("initialize"))
			 (:file "recording" :depends-on ("database"))
			 (:file "pool" :depends-on ("database")))
                        :depends-on (base))
	       (:module syntax
			:pathname ""
			:components ((:file "expressions")
				     (:file "operations"
                                            :depends-on ("expressions"))
				     (:file "syntax" :depends-on ("operations")))
			:depends-on (database))
	       (:module functional
			:pathname ""
			:components ((:file "fdml")
                                     (:file "transaction" :depends-on ("fdml"))
                                     #+clisp (:file "ansi-loop")
                                     (:file "loop-extension"
                                            :depends-on ("fdml" #+clisp "ansi-loop"))
				     (:file "fddl" :depends-on ("fdml")))
			:depends-on (syntax))
	       (:module object
			:pathname ""
			:components ((:file "metaclasses")
				     (:file "ooddl" :depends-on ("metaclasses"))
				     (:file "oodml" :depends-on ("ooddl")))
			:depends-on (functional))
	       (:module generic
			:pathname ""
			:components ((:file "generic-postgresql")
				     (:file "generic-odbc")
				     (:file "sequences"))
			:depends-on (functional))))))


(defmethod perform ((o test-op) (c (eql (find-system 'clsql))))
  (operate 'load-op 'clsql-tests)
  (operate 'test-op 'clsql-tests :force t))

(defmethod perform :after ((o load-op) (c (eql (find-system 'clsql))))
  (let* ((init-var (uffi:getenv "CLSQLINIT"))
         (init-file (or (when init-var (probe-file init-var))
                        (probe-file
                         (concatenate 'string
                                      (namestring (user-homedir-pathname))
                                      ".clsql-init.lisp"))
                        (probe-file "/etc/clsql-init.lisp")
                        #+(or mswin windows win32 win64 mswindows)
                        (probe-file "c:\\etc\\clsql-init.lisp"))))
    (when init-file (load init-file))))

