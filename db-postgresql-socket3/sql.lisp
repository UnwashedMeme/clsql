;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     postgresql-socket-sql.sql
;;;; Purpose:  High-level PostgreSQL interface using socket
;;;; Authors:  Kevin M. Rosenberg based on original code by Pierre R. Mai
;;;; Created:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2007 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage :clsql-postgresql-socket3
    (:use #:common-lisp #:clsql-sys #:postgresql-socket3)
    (:export #:postgresql-socket3-database)
    (:documentation "This is the CLSQL socket interface (protocol version 3) to PostgreSQL."))

(in-package #:clsql-postgresql-socket3)

(defvar *sqlreader* (cl-postgres:copy-sql-readtable))
(let ((dt-fn (lambda (useconds-since-2000)
	       (let ((sec (truncate
			   (/ useconds-since-2000
			      1000000)))
		     (usec (mod useconds-since-2000
				1000000)))
		 (clsql:make-time :year 2000 :second sec :usec usec)))))
  (cl-postgres:set-sql-datetime-readers
   :table *sqlreader*
   :date (lambda (days-since-2000)
	   (clsql:make-date :year 2000 :day (+ 1 days-since-2000)))
   :timestamp dt-fn
   :timestamp-with-timezone dt-fn))



;; interface foreign library loading routines

(clsql-sys:database-type-load-foreign :postgresql-socket3)


(defmethod database-initialize-database-type ((database-type
                                               (eql :postgresql-socket3)))
  t)


;; Field type conversion
(defun convert-to-clsql-warning (database condition)
  (ecase *backend-warning-behavior*
    (:warn
     (warn 'sql-database-warning :database database
           :message (cl-postgres:database-error-message condition)))
    (:error
     (error 'sql-database-error :database database
            :message (format nil "Warning upgraded to error: ~A"
                             (cl-postgres:database-error-message condition))))
    ((:ignore nil)
     ;; do nothing
     )))

(defun convert-to-clsql-error (database expression condition)
  (error 'sql-database-data-error
         :database database
         :expression expression
         :error-id (type-of condition)
         :message (cl-postgres:database-error-message condition)))

(defmacro with-postgresql-handlers
    ((database &optional expression)
     &body body)
  (let ((database-var (gensym))
        (expression-var (gensym)))
    `(let ((,database-var ,database)
           (,expression-var ,expression))
       (handler-bind ((postgresql-warning
                       (lambda (c)
                         (convert-to-clsql-warning ,database-var c)))
                      (cl-postgres:database-error
                       (lambda (c)
                         (convert-to-clsql-error
                          ,database-var ,expression-var c))))
         ,@body))))



(defclass postgresql-socket3-database (generic-postgresql-database)
  ((connection :accessor database-connection :initarg :connection
               :type cl-postgres:database-connection)))

(defmethod database-type ((database postgresql-socket3-database))
  :postgresql-socket3)

(defmethod database-name-from-spec (connection-spec (database-type (eql :postgresql-socket3)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (declare (ignore password options tty))
    (concatenate 'string
      (etypecase host
        (null
         "localhost")
        (pathname (namestring host))
        (string host))
      (when port
        (concatenate 'string
                     ":"
                     (etypecase port
                       (integer (write-to-string port))
                       (string port))))
      "/" db "/" user)))

(defmethod database-connect (connection-spec
                             (database-type (eql :postgresql-socket3)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional
                            (port +postgresql-server-default-port+)
                            (options "") (tty ""))
      connection-spec
    (declare (ignore options tty))
    (handler-case
        (handler-bind ((warning
                        (lambda (c)
                          (warn 'sql-warning
                                :format-control "~A"
                                :format-arguments
                                (list (princ-to-string c))))))
          (cl-postgres:open-database db user password host port))
      (cl-postgres:database-error (c)
        ;; Connect failed
        (error 'sql-connection-error
               :database-type database-type
               :connection-spec connection-spec
               :error-id (type-of c)
               :message (cl-postgres:database-error-message c)))
      (:no-error (connection)
                 ;; Success, make instance
                 (make-instance 'postgresql-socket3-database
                                :name (database-name-from-spec connection-spec database-type)
                                :database-type :postgresql-socket3
                                :connection-spec connection-spec
                                :connection connection)))))

(defmethod database-disconnect ((database postgresql-socket3-database))
  (cl-postgres:close-database (database-connection database))
  t)

(defvar *include-field-names* nil)


;; THE FOLLOWING MACRO EXPANDS TO THE FUNCTION BELOW IT,
;; BUT TO GET null CONVENTIONS CORRECT I NEEDED TO TWEAK THE EXPANSION
;;
;; (cl-postgres:def-row-reader clsql-default-row-reader (fields)
;;   (values (loop :while (cl-postgres:next-row)
;; 		:collect (loop :for field :across fields
;; 			       :collect (cl-postgres:next-field field)))
;; 	  (when *include-field-names*
;; 	    (loop :for field :across fields
;; 		  :collect (cl-postgres:field-name field)))))



(defun clsql-default-row-reader (stream fields)
  (declare (type stream stream)
           (type (simple-array cl-postgres::field-description) fields))
  (flet ((cl-postgres:next-row ()
	   (cl-postgres::look-for-row stream))
	 (cl-postgres:next-field (cl-postgres::field)
	   (declare (type cl-postgres::field-description cl-postgres::field))
	   (let ((cl-postgres::size (cl-postgres::read-int4 stream)))
	     (declare (type (signed-byte 32) cl-postgres::size))
	     (if (eq cl-postgres::size -1)
		 nil
		 (funcall (cl-postgres::field-interpreter cl-postgres::field)
			  stream cl-postgres::size)))))
    (let ((results (loop :while (cl-postgres:next-row)
			 :collect (loop :for field :across fields
					:collect (cl-postgres:next-field field))))
	  (col-names (when *include-field-names*
		       (loop :for field :across fields
			     :collect (cl-postgres:field-name field)))))
      ;;multiple return values were not working here
      (list results col-names))))

(defmethod database-query ((expression string) (database postgresql-socket3-database) result-types field-names)
  (let ((connection (database-connection database))
	(cl-postgres:*sql-readtable* *sqlreader*))
    (with-postgresql-handlers (database expression)
      (let ((*include-field-names* field-names))
	(apply #'values (cl-postgres:exec-query connection expression #'clsql-default-row-reader)))
      )))

(defmethod query ((obj command-object) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (clsql-sys::record-sql-command (expression obj) database)
  (multiple-value-bind (rows names)
      (database-query obj database result-types field-names)
    (let ((result (if (and flatp (= 1 (length (car rows))))
                      (mapcar #'car rows)
		      rows)))
      (clsql-sys::record-sql-result result database)
      (if field-names
          (values result names)
	  result))))

(defmethod database-query ((obj command-object) (database postgresql-socket3-database) result-types field-names)
  (let ((connection (database-connection database))
	(cl-postgres:*sql-readtable* *sqlreader*))
    (with-postgresql-handlers (database obj)
      (let ((*include-field-names* field-names))
	(unless (has-been-prepared obj)
	  (cl-postgres:prepare-query connection (prepared-name obj) (expression obj))
	  (setf (has-been-prepared obj) T))
	(apply #'values (cl-postgres:exec-prepared
			 connection
			 (prepared-name obj)
			 (parameters obj)
			 #'clsql-default-row-reader))))))

(defmethod database-execute-command
    ((expression string) (database postgresql-socket3-database))
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      ;; return row count?
      (second (multiple-value-list (cl-postgres:exec-query connection expression))))))

(defmethod execute-command ((obj command-object)
                            &key (database *default-database*))
  (clsql-sys::record-sql-command (expression obj) database)
  (let ((res (database-execute-command obj database)))
    (clsql-sys::record-sql-result res database)
    ;; return row count?
    res))

(defmethod database-execute-command
    ((obj command-object) (database postgresql-socket3-database))
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database obj)
      (unless (has-been-prepared obj)
	(cl-postgres:prepare-query connection (prepared-name obj) (expression obj))
	(setf (has-been-prepared obj) T))
      (second (multiple-value-list (cl-postgres:exec-prepared connection (prepared-name obj) (parameters obj)))))))

;;;; Cursoring interface


(defmethod database-query-result-set ((expression string)
                                      (database postgresql-socket3-database)
                                      &key full-set result-types)
  (declare (ignore result-types))
  (declare (ignore full-set))
  (error "Cursoring interface is not supported for postgresql-socket3-database try cl-postgres:exec-query with a custom row-reader"))

(defmethod database-dump-result-set (result-set
                                     (database postgresql-socket3-database))
  (error "Cursoring interface is not supported for postgresql-socket3-database try cl-postgres:exec-query with a custom row-reader")
  T)

(defmethod database-store-next-row (result-set
                                    (database postgresql-socket3-database)
                                    list)
  (error "Cursoring interface is not supported for postgresql-socket3-database try cl-postgres:exec-query with a custom row-reader"))


;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod database-create (connection-spec (type (eql :postgresql-socket3)))
  (destructuring-bind (host name user password &optional port options tty) connection-spec
    (declare (ignore port options tty))
    (let ((database (database-connect (list host "postgres" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
           (database-execute-command (format nil "create database ~A" name) database)
        (database-disconnect database)))))

(defmethod database-destroy (connection-spec (type (eql :postgresql-socket3)))
  (destructuring-bind (host name user password &optional port options tty) connection-spec
    (declare (ignore port options tty))
    (let ((database (database-connect (list host "postgres" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
          (database-execute-command (format nil "drop database ~A" name) database)
        (database-disconnect database)))))


(defmethod database-probe (connection-spec (type (eql :postgresql-socket3)))
  (when (find (second connection-spec) (database-list connection-spec type)
              :test #'string-equal)
    t))


;; Database capabilities

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :postgresql-socket3)))
  nil)

(defmethod db-type-has-fancy-math? ((db-type (eql :postgresql-socket3)))
  t)

(defmethod db-type-default-case ((db-type (eql :postgresql-socket3)))
  :lower)

(defmethod database-underlying-type ((database postgresql-socket3-database))
  :postgresql)

(when (clsql-sys:database-type-library-loaded :postgresql-socket3)
  (clsql-sys:initialize-database-type :database-type :postgresql-socket3))


;; Type munging functions

(defmethod read-sql-value (val (type (eql 'boolean)) (database postgresql-socket3-database) db-type)
  (declare (ignore database db-type))
  val)

(defmethod read-sql-value (val (type (eql 'generalized-boolean)) (database postgresql-socket3-database) db-type)
  (declare (ignore database db-type))
  val)
