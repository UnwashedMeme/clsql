;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     conditions.lisp
;;;; Purpose:  Error conditions for CLSQL
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar *backend-warning-behavior* :warn
  "Action to perform on warning messages from backend. Default is
to :warn. May also be set to :error to signal an error
or :ignore/nil to silently ignore the warning.")

;;; CommonSQL-compatible conditions

(define-condition sql-condition ()
  ())

(define-condition sql-error (simple-error sql-condition)
  ())

(define-condition sql-database-error (sql-error)
  ((error-id :initarg :error-id
             :initform nil
             :reader sql-error-error-id)
   (secondary-error-id :initarg :secondary-error-id
                       :initform nil
                       :reader sql-error-secondary-error-id)
   (database-message :initarg :message
                     :initform nil
                     :reader sql-error-database-message)
   (database :initarg :database
                     :initform nil
                     :reader sql-error-database))
  (:report (lambda (c stream)
             (format stream "A database error occurred~@[ on database ~A~]: ~A / ~A~%  ~A"
                     (sql-error-database c)
                     (sql-error-error-id c)
                     (sql-error-secondary-error-id c)
                     (sql-error-database-message c))))
  (:documentation "Used to signal an error in a CLSQL database interface."))

(define-condition sql-connection-error (sql-database-error)
  ((database-type :initarg :database-type :initform nil
                  :reader sql-error-database-type)
   (connection-spec :initarg :connection-spec :initform nil
                  :reader sql-error-connection-spec))
  (:report (lambda (c stream)
             (format stream "While trying to connect to database ~A~%  using database-type ~A:~%  Error ~D / ~A~%  has occurred."
                     (when (and (sql-error-connection-spec c)
                                (sql-error-database-type c))
                       (database-name-from-spec
                        (sql-error-connection-spec c)
                        (sql-error-database-type c)))
                     (sql-error-database-type c)
                     (sql-error-error-id c)
                     (sql-error-database-message c))))
  (:documentation "Used to signal an error in connecting to a database."))

(define-condition sql-database-data-error (sql-database-error)
  ((expression :initarg :expression :initarg nil
               :reader sql-error-expression))
  (:report (lambda (c stream)
             (format stream "While accessing database ~A~%  with expression ~S:~%  Error ~D / ~A~%  has occurred."
                     (sql-error-database c)
                     (sql-error-expression c)
                     (sql-error-error-id c)
                     (sql-error-database-message c))))
  (:documentation "Used to signal an error with the SQL data
  passed to a database."))

(define-condition sql-temporary-error (sql-database-error)
  ()
  (:documentation "Used to signal an error when the database
cannot currently process a valid interaction because, for
example, it is still executing another command possibly issued by
another user."))

(define-condition sql-timeout-error (sql-connection-error)
  ()
  (:documentation "Used to signal an error when the database
times out while processing some operation."))

(define-condition sql-fatal-error (sql-connection-error)
  ()
  (:documentation "Used to signal an error when the database
connection is no longer usable."))

(define-condition sql-user-error (sql-error)
  ((message :initarg :message
            :initform "Unspecified error"
            :reader sql-user-error-message))
  (:report (lambda (c stream)
             (format stream "A CLSQL lisp code error occurred: ~A "
                     (sql-user-error-message c))))
  (:documentation  "Used to signal lisp errors inside CLSQL."))



;; Signal conditions

(defun signal-closed-database-error (database)
  (error 'sql-fatal-error
         :database database
         :connection-spec (when database (connection-spec database))
         :database-type (when database (database-type database))
         :message "Database is closed."))

(defun signal-no-database-error (database)
  (error 'sql-database-error
         :database database
         :message (format nil "~A is not a database." database)))


;;; CLSQL Extensions

(define-condition sql-warning (warning sql-condition)
  ((message :initarg :message :initform nil :reader sql-warning-message))
  (:report (lambda (c stream)
             (format stream "~A" (sql-warning-message c)))))

(define-condition sql-database-warning (sql-warning)
  ((database :initarg :database :reader sql-warning-database))
  (:report (lambda (c stream)
             (format stream
                     "While accessing database ~A~%  Warning: ~A~%  has occurred."
                     (sql-warning-database c)
                     (sql-warning-message c)))))

(define-condition database-too-strange (sql-user-error)
  ()
  (:documentation "Used to signal cases where CLSQL is going to fail at
    mapping your database correctly"))

(defun signal-database-too-strange (message)
  (error 'database-too-strange :message message))
