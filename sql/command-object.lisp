;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     postgresql-socket-sql.sql
;;;; Purpose:  High-level PostgreSQL interface using socket
;;;; Authors:  Russ Tyndall (at Acceleration.net) based on original code by
;;;;           Kevin M. Rosenberg based on original code by Pierre R. Mai
;;;; Created:  Sep 2009
;;;;
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2007 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass command-object ()
  ((expression :accessor expression :initarg :expression :initform nil
               :documentation "query that refers to parameters using \"$1\", \"$2\", \"$n\".
       These match positions in the parameters list.")
   (parameters :accessor parameters :initarg :parameters :initform nil
               :documentation "list of parameters")
   (prepared-name :accessor prepared-name :initarg :prepared-name :initform ""
    :documentation "If we want this to be a prepared statement, give it a name
       to identify it to this session")
   (has-been-prepared :accessor has-been-prepared :initarg :has-been-prepared :initform nil
		      :documentation "Have we already prepared this command object?")
   ))


(defgeneric prepare-sql-parameter (sql-parameter)
  (:documentation "This method is responsible for formatting parameters
     as the database expects them (eg: :false is nil, nil is :null, dates are iso8601 strings)")
  (:method (sql-parameter)
    (typecase sql-parameter
      (null :null)
      (symbol
       (if (member sql-parameter (list :false :F))
           nil
           (princ-to-string sql-parameter)))
      (clsql-sys:date (format-date nil sql-parameter :format :iso8601))
      (clsql-sys:wall-time (format-time nil sql-parameter :format :iso8601))
      (t sql-parameter))))

(defmethod initialize-instance :after ((o command-object) &key &allow-other-keys )
  ;; Inits parameter value coersion
  (setf (parameters o) (parameters o)))

(defmethod (setf parameters) (new (o command-object))
  " This causes the semantics to match cl-sql instead of cl-postgresql
  "
  (setf (slot-value o 'parameters)
	(loop for p in new collecting (prepare-sql-parameter p))))

(defun reset-command-object (co)
  "Resets the command object to have no name and to be unprepared
     (This is useful if you want to run a command against a second database)"
  (setf (prepared-name co) ""
	(has-been-prepared co) nil))

(defun command-object (expression &optional parameters (prepared-name ""))
  (make-instance 'command-object
		 :expression expression
		 :parameters parameters
		 :prepared-name prepared-name))
