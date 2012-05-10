;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          aodbc-sql.cl
;;;; Purpose:       Low-level interface for CLSQL AODBC backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-aodbc)

;; interface foreign library loading routines
(defmethod clsql-sys:database-type-library-loaded ((database-type (eql :aodbc)))
  "T if foreign library was able to be loaded successfully. "
  (when (find-package :dbi) ;; finds Allegro's DBI (AODBC) package
    t))

(defmethod clsql-sys:database-type-load-foreign ((databae-type (eql :aodbc)))
  t)

(when (find-package :dbi)
  (clsql-sys:database-type-load-foreign :aodbc))


;; AODBC interface

(defclass aodbc-database (generic-odbc-database)
  ((aodbc-db-type :accessor database-aodbc-db-type :initform :unknown)))

(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :aodbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (declare (ignore password))
    (concatenate 'string dsn "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :aodbc)))
  (check-connection-spec connection-spec database-type (dsn user password))
  #+aodbc-v2
  (destructuring-bind (dsn user password) connection-spec
    (handler-case
        (clsql-sys:build-database-object
	 'aodbc-database
	 :database-type database-type
	 :connection-spec connection-spec
          :dbi-package (find-package '#:dbi)
          :odbc-conn
          (dbi:connect :user user
                       :password password
                       :data-source-name dsn))
      (sql-error (e)
        (error e))
      (error ()         ;; Init or Connect failed
        (error 'sql-connection-error
               :database-type database-type
               :connection-spec connection-spec
               :message "Connection failed")))))


(defmethod database-query (query-expression (database aodbc-database)
                           result-types field-names)
  #+aodbc-v2
  (handler-case
      (dbi:sql query-expression
               :db (clsql-sys::odbc-conn database)
               :types result-types
               :column-names field-names)
    #+ignore
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :message "Query failed"))))

(defmethod database-create (connection-spec (type (eql :aodbc)))
  (warn "Not implemented."))

(defmethod database-destroy (connection-spec (type (eql :aodbc)))
  (warn "Not implemented."))

(defmethod database-probe (connection-spec (type (eql :aodbc)))
  (warn "Not implemented."))

;;; Backend capabilities

(defmethod database-underlying-type ((database aodbc-database))
  (database-aodbc-db-type database))

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :aodbc)))
  nil)

(defmethod database-initialize-database-type ((database-type (eql :aodbc)))
  t)

(when (clsql-sys:database-type-library-loaded :aodbc)
  (clsql-sys:initialize-database-type :database-type :aodbc))
