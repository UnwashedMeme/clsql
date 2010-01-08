;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-mysql.asd
;;;; Purpose:       ASDF definition file for CLSQL MySQL backend
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

(defpackage #:clsql-mysql-system (:use #:asdf #:cl))
(in-package #:clsql-mysql-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'uffi)
    (asdf:operate 'asdf:load-op 'uffi)))

(defvar *library-file-dir* 
  (merge-pathnames "db-mysql/" 
                   (make-pathname :name nil :type nil 
                                  :defaults *load-truename*)))

(defclass clsql-mysql-source-file (c-source-file)
  ())

(defmethod output-files ((o compile-op) (c clsql-mysql-source-file))
  (let* ((library-file-type
	  (funcall (intern (symbol-name'#:default-foreign-library-type)
			   (symbol-name '#:uffi))))
         (found (some #'(lambda (dir)
			    (probe-file (make-pathname :directory dir
						       :name (component-name c)
						       :type library-file-type)))
			'((:absolute "usr" "lib" "clsql")))))
    (list (if found
	      found
	      (make-pathname :name (component-name c)
			     :type library-file-type
			     :defaults *library-file-dir*)))))

(defmethod perform ((o load-op) (c clsql-mysql-source-file))
  t)

(defmethod operation-done-p ((o load-op) (c clsql-mysql-source-file))
  (and (symbol-function (intern (symbol-name '#:mysql-get-client-info)
				(find-package '#:mysql)))
       t))

(defmethod perform ((o compile-op) (c clsql-mysql-source-file))
  (unless (operation-done-p o c)
    #-(or win32 win64 windows mswindows)
    (unless (zerop (run-shell-command
		    #-freebsd "cd ~A; make"
		    #+freebsd "cd ~A; gmake"
		    (namestring *library-file-dir*)))
      (error 'operation-error :component c :operation o))))

(defmethod operation-done-p ((o compile-op) (c clsql-mysql-source-file))
  (or (and (probe-file #p"/usr/lib/clsql/clsql_mysql.so") t)
      (let ((lib (make-pathname :defaults (component-pathname c)
				:type (uffi:default-foreign-library-type))))
	(and (probe-file lib) (probe-file (component-pathname c))
	     (> (file-write-date lib) (file-write-date (component-pathname c)))))))

;;; System definition

(defsystem :clsql-mysql
  :name "cl-sql-mysql"
  :author "Kevin M. Rosenberg <kmr@debian.org>"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp SQL MySQL Driver"
  :long-description "cl-sql-mysql package provides a database driver to the MySQL database system."

  :depends-on (uffi clsql clsql-uffi)
  :components
  ((:module :db-mysql
	    :components
	    ((:file "mysql-package")
	     (:clsql-mysql-source-file "clsql_mysql" :depends-on ("mysql-package"))
	     (:file "mysql-loader" :depends-on ("mysql-package" "clsql_mysql"))
	     (:file "mysql-client-info" :depends-on ("mysql-loader"))
	     (:file "mysql-api" :depends-on ("mysql-client-info"))
	     (:file "mysql-sql" :depends-on ("mysql-api"))
	     (:file "mysql-objects" :depends-on ("mysql-sql"))))))
