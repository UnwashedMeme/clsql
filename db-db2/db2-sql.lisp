;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db2-sql.lisp
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-db2)

(defmethod database-initialize-database-type ((database-type (eql :db2)))
  t)

(defclass db2-database (database)
  ((henv :initform nil :allocation :class :initarg :henv :accessor henv)
   (hdbc :initform nil :initarg :hdbc :accessor hdbc)))


(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :db2)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (dsn user password) connection-spec
    (declare (ignore password))
    (concatenate 'string dsn "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :db2)))
  (check-connection-spec connection-spec database-type (dsn user password))
  (destructuring-bind (server user password) connection-spec
    (handler-case
        (let ((db (clsql-sys:build-database-object
		   'db2-database
		   :database-type database-type
		   :connection-spec connection-spec))))
          (db2-connect db server user password)
          db)
      (error ()         ;; Init or Connect failed
        (error 'sql-connection-error
               :database-type database-type
               :connection-spec connection-spec
               :message "Connection failed")))))


;; API Functions

(uffi:def-type handle-type cli-handle)
(uffi:def-type handle-ptr-type (* cli-handle))

(defmacro deref-vp (foreign-object)
  `(the handle-type (uffi:deref-pointer (the handle-ptr-type ,foreign-object) cli-handle)))

(defun db2-connect (db server user password)
  (let ((henv (uffi:allocate-foreign-object 'cli-handle))
        (hdbc (uffi:allocate-foreign-object 'cli-handle)))
    (sql-alloc-handle SQL_HANDLE_ENV SQL_NULL_HANDLE henv)
    (setf (slot-value db 'henv) henv)
    (setf (slot-value db 'hdbc) hdbc)

    (sql-alloc-handle SQL_HANDLE_DBC (deref-vp henv) hdbc)
    (uffi:with-cstrings ((native-server server)
                         (native-user user)
                         (native-password password))
      (sql-connect (deref-vp hdbc)
                   native-server SQL_NTS
                   native-user SQL_NTS
                   native-password SQL_NTS)))
    db)
