;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base-classes.lisp
;;;; Purpose:       Base classes for high-level SQL interface
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                 original code by Pierre R. Mai
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


(defclass database ()
  ((name :initform nil :initarg :name :reader database-name)
   (connection-spec :initform nil :initarg :connection-spec
                    :reader connection-spec
                    :documentation "Required to use connection pool.")
   (database-type :initarg :database-type :initform :unknown
                  :reader database-type)
   (encoding :initarg :encoding :initform nil
             :documentation "External format character encoding.")
   (state :initform :closed :reader database-state)
   (autocommit :initform t :accessor database-autocommit)
   (command-recording-stream :accessor command-recording-stream :initform nil)
   (result-recording-stream :accessor result-recording-stream :initform nil)
   (record-caches :accessor record-caches :initform nil)
   (view-classes :accessor database-view-classes :initform nil)
   (transaction-level :initform 0 :accessor transaction-level)
   (transaction :initform nil :accessor transaction)
   (conn-pool :initform nil :initarg :conn-pool :accessor conn-pool)
   (sql-operations-executed :initform 0 :accessor sql-operations-executed)
   (attribute-cache :initform (make-hash-table :size 100 :test 'equal)
                    :accessor attribute-cache
                    :documentation "Internal cache of table attributes. It is keyed by table-name. Values
are a list of ACTION specified for table and any cached value of list-attributes-types."))
  (:documentation
   "This class is the supertype of all databases handled by CLSQL."))

(defmethod print-object ((object database) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
            (if (slot-boundp object 'name)
                (database-name object)
              "<unbound>")
            (database-state object)))
  object)

(setf (documentation 'database-name 'function)
      "Returns the name of a database.")
