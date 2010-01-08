;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.asd
;;;; Purpose:       ASDF definition file for CLSQL UFFI Helper package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package cl-user)

(defpackage clsql-uffi-system (:use #:asdf #:cl))
(in-package clsql-uffi-system)

(defvar *clsql-uffi-library-dir*
  (merge-pathnames "uffi/"
		   (make-pathname :name nil :type nil :defaults *load-truename*)))

(defclass clsql-uffi-source-file (c-source-file)
  ())


(defmethod output-files ((o compile-op) (c clsql-uffi-source-file))
  (let* ((library-file-type
	  (funcall (intern (symbol-name'#:default-foreign-library-type)
			   (symbol-name '#:uffi))))
	 (found
	  (some #'(lambda (dir)
		    (probe-file (make-pathname
				 :directory dir
				 :name (component-name c)
				 :type library-file-type)))
		'((:absolute "usr" "lib" "clsql")))))
    (list (if found
	      found
	      (make-pathname :name (component-name c)
			     :type library-file-type
			     :defaults *clsql-uffi-library-dir*)))))

(defmethod perform ((o load-op) (c clsql-uffi-source-file))
  nil) ;;; library will be loaded by a loader file

(defmethod operation-done-p ((o load-op) (c clsql-uffi-source-file))
  (and (symbol-function (intern (symbol-name '#:atol64)
				(find-package '#:clsql-uffi)))
       t))

(defmethod perform ((o compile-op) (c clsql-uffi-source-file))
  (unless (operation-done-p o c)
    #-(or win32 win64 windows mswindows)
    (unless (zerop (run-shell-command
		    #-(or freebsd netbsd) "cd ~A; make"
		    #+(or freebsd netbsd) "cd ~A; gmake"
		    (namestring *clsql-uffi-library-dir*)))
      (error 'operation-error :component c :operation o))))

(defmethod operation-done-p ((o compile-op) (c clsql-uffi-source-file))
  (or (and (probe-file #p"/usr/lib/clsql/clsql_uffi.so") t)
      (let ((lib (make-pathname :defaults (component-pathname c)
				:type (funcall (intern (symbol-name '#:default-foreign-library-type)
                                                       (find-package '#:uffi))))))
	(and (probe-file lib) (probe-file (component-pathname c))
	     (> (file-write-date lib) (file-write-date (component-pathname c)))))))

(defsystem clsql-uffi
  :name "cl-sql-base"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common UFFI Helper functions for Common Lisp SQL Interface Library"
  :long-description "cl-sql-uffi package provides common helper functions using the UFFI for the CLSQL package."

  :depends-on (uffi clsql)

  :components
  ((:module :uffi
	    :components
	    ((:file "clsql-uffi-package")
	     (:clsql-uffi-source-file "clsql_uffi" :depends-on ("clsql-uffi-package"))
	     (:file "clsql-uffi-loader" :depends-on ("clsql-uffi-package" "clsql_uffi"))
	     (:file "clsql-uffi" :depends-on ("clsql-uffi-loader"))))))
