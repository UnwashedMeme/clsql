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
  ((expression :accessor expression :initarg :expression :initform nil)
   (parameters :accessor parameters :initarg :parameters :initform nil)
   (prepared-name :accessor prepared-name :initarg :prepared-name :initform ""
    :documentation "If we want this to be a prepared statement, give it a name
       to identify it to this session")
   (has-been-prepared :accessor has-been-prepared :initarg :has-been-prepared :initform nil
		      :documentation "Have we already prepared this command object")
   ))

(export '(expression parameters prepared-name has-been-prepared command-object))


