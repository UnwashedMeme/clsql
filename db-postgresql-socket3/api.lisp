;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     postgresql-socket-api.lisp
;;;; Purpose:  Low-level PostgreSQL interface using sockets
;;;; Authors:  Kevin M. Rosenberg based on original code by Pierre R. Mai
;;;; Created:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:postgresql-socket3)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :postgresql-socket3)))
  t)

(defmethod clsql-sys:database-type-library-loaded ((database-type
                                          (eql :postgresql-socket3)))
  "T if foreign library was able to be loaded successfully. Always true for
socket interface"
  t)

(defparameter +postgresql-server-default-port+ 5432
  "Default port of PostgreSQL server.")

;;;; Condition hierarchy

(define-condition postgresql-condition (condition)
  ((connection :initarg :connection :reader postgresql-condition-connection)
   (message :initarg :message :reader postgresql-condition-message))
  (:report
   (lambda (c stream)
     (format stream "~@<~A occurred on connection ~A. ~:@_Reason: ~A~:@>"
             (type-of c)
             (postgresql-condition-connection c)
             (postgresql-condition-message c)))))

(define-condition postgresql-error (error postgresql-condition)
  ())

(define-condition postgresql-fatal-error (postgresql-error)
  ())

(define-condition postgresql-login-error (postgresql-fatal-error)
  ())

(define-condition postgresql-warning (warning postgresql-condition)
  ())

(define-condition postgresql-notification (postgresql-condition)
  ()
  (:report
   (lambda (c stream)
     (format stream "~@<Asynchronous notification on connection ~A: ~:@_~A~:@>"
             (postgresql-condition-connection c)
             (postgresql-condition-message c)))))