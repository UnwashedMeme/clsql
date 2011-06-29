;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          odbc-sql.cl
;;;; Purpose:       Medium-level interface for CLSQL ODBC backend
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-odbc
    (:use #:common-lisp #:clsql-sys)
    (:export #:odbc-database)
    (:documentation "This is the CLSQL interface to ODBC."))

(in-package #:clsql-odbc)

;; ODBC interface

(defclass odbc-database (generic-odbc-database)
  ((odbc-db-type :accessor database-odbc-db-type)))

(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :odbc)))
  (check-connection-spec connection-spec database-type
      (dsn user password &key connection-string completion window-handle))
  (destructuring-bind (dsn user password &key connection-string completion window-handle) connection-spec
    (declare (ignore password connection-string completion window-handle))
    (concatenate 'string dsn "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :odbc)))
  (check-connection-spec connection-spec database-type
      (dsn user password &key connection-string completion window-handle))
  (destructuring-bind (dsn user password &key connection-string (completion :no-prompt) window-handle) connection-spec
    (handler-case
        (let ((db (make-instance 'odbc-database
                                 :name (database-name-from-spec connection-spec :odbc)
                                 :database-type :odbc
                                 :dbi-package (find-package '#:odbc-dbi)
                                 :odbc-conn
                                 (odbc-dbi:connect :user user
                                                   :password password
                                                   :data-source-name dsn
                                                   :connection-string connection-string
                                                   :completion completion
                                                   :window-handle window-handle))))
          (store-type-of-connected-database db)
          ;; Ensure this database type is initialized so can check capabilities of
          ;; underlying database
          (initialize-database-type :database-type database-type)
          db)
      #+ignore
      (error ()         ;; Init or Connect failed
        (error 'sql-connection-error
               :database-type database-type
               :connection-spec connection-spec
               :message "Connection failed")))))

(defmethod database-underlying-type ((database odbc-database))
  (database-odbc-db-type database))

(defun store-type-of-connected-database (db)
  (let* ((odbc-conn (clsql-sys::odbc-conn db))
         (server-name (odbc-dbi::get-odbc-info odbc-conn odbc::$SQL_SERVER_NAME))
         (dbms-name (odbc-dbi::get-odbc-info odbc-conn odbc::$SQL_DBMS_NAME))
         (type
          ;; need SERVER-NAME and DBMS-NAME because many drivers mix this up
          (cond
           ((or (search "postgresql" server-name :test #'char-equal)
                (search "postgresql" dbms-name :test #'char-equal))
            (unless (find-package 'clsql-postgresql)
              (ignore-errors (asdf:operate 'asdf:load-op 'clsql-postgresql-socket)))
            :postgresql)
           ((or (search "Microsoft SQL Server" server-name :test #'char-equal)
                (search "Microsoft SQL Server" dbms-name :test #'char-equal))
            :mssql)
           ((or (search "mysql" server-name :test #'char-equal)
                (search "mysql" dbms-name :test #'char-equal))
            (unless (find-package 'clsql-mysql)
              ;; ignore errors on platforms where the shared libraries are not available
              (ignore-errors (asdf:operate 'asdf:load-op 'clsql-mysql)))
            :mysql)
           ((or (search "oracle" server-name :test #'char-equal)
                (search "oracle" dbms-name :test #'char-equal))
            :oracle))))
    (setf (database-odbc-db-type db) type)))



(defmethod database-create (connection-spec (type (eql :odbc)))
  (declare (ignore connection-spec))
  (warn "Not implemented."))

(defmethod database-destroy (connection-spec (type (eql :odbc)))
  (declare (ignore connection-spec))
  (warn "Not implemented."))

(defmethod database-probe (connection-spec (type (eql :odbc)))
  (when (find (car connection-spec) (database-list connection-spec type)
              :test #'string-equal)
    t))

(defmethod database-list (connection-spec (type (eql :odbc)))
  (declare (ignore connection-spec))
  (odbc-dbi:list-all-data-sources))

(defmethod database-list-indexes ((database odbc-database)
                                  &key (owner nil))
  (let ((result '()))
    (dolist (table (database-list-tables database :owner owner) result)
      (setq result
        (append (database-list-table-indexes table database :owner owner)
                result)))))

(defmethod database-list-table-indexes (table (database odbc-database)
                                        &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (odbc-dbi:list-table-indexes
       table
       :db (clsql-sys::odbc-conn database))
    (declare (ignore col-names))
    ;; INDEX_NAME is hard-coded in sixth position by ODBC driver
    ;; FIXME: ??? is hard-coded in the fourth position
    (do ((results nil)
         (loop-rows rows (cdr loop-rows)))
        ((null loop-rows) (nreverse results))
      (let* ((row (car loop-rows))
             (col (nth 5 row)))
        (unless (or (null col) (find col results :test #'string-equal))
          (push col results))))))

;;; Database capabilities

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :odbc)))
  nil)


(defmethod database-initialize-database-type ((database-type (eql :odbc)))
  ;; nothing to do
  t)

(when (clsql-sys:database-type-library-loaded :odbc)
  (clsql-sys:initialize-database-type :database-type :odbc))
