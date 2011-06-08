;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postgresql-socket-package.lisp
;;;; Purpose:       Package definition for PostgreSQL interface using sockets
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

#+lispworks (require "comm")

(defpackage #:postgresql-socket3
  (:use #:cl md5 #:cl-postgres)
  (:shadow #:postgresql-warning)
  (:export #:+postgresql-server-default-port+
	   #:postgresql-condition
	   #:postgresql-error
	   #:postgresql-fatal-error
	   #:postgresql-login-error
	   #:postgresql-warning
	   #:postgresql-notification
	   #:postgresql-condition-message
	   #:postgresql-condition-connection))

