;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          initialize.lisp
;;;; Purpose:       Initializion routines for db backend
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  May 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar *loaded-database-types* nil
  "Contains a list of database types which have been defined/loaded.")

(defmethod database-type-load-foreign (x)
  (error "No generic function defined for database-type-load-foreign with parameters of ~S" x))

(defmethod database-type-load-foreign :after (database-type)
  (when (database-type-library-loaded database-type)
     (pushnew database-type *loaded-database-types*)))

(defun reload-database-types ()
  "Reloads any foreign code for the loaded database types after a dump."
  (mapc #'database-type-load-foreign *loaded-database-types*))

(defvar *default-database-type* nil
  "Designates the default database type which is initialised by
  the function INITIALISE-DATABASE-TYPE.")

(defvar *initialized-database-types* nil
  "A list of database types which have currently been initialised
by calling INITIALIZE-DATABASE-TYPE.")

(defun initialize-database-type (&key (database-type *default-database-type*))
  "Initializes the supplied DATABASE-TYPE, if it is not already
initialized, as indicated by *INITIALIZED-DATABASE-TYPES* and
returns DATABASE-TYPE. *DEFAULT-DATABASE-TYPE* is set to
DATABASE-TYPE and, if DATABASE-TYPE has not been initialised, it
is added to *INITIALIZED-DATABASE-TYPES*. "
  (when (member database-type *initialized-database-types*)
    (return-from initialize-database-type database-type))

  (let ((system (intern (join
                          (symbol-name '#:clsql-)
                          (symbol-name database-type)))))
    (when (not (find-package system))
      (asdf:operate 'asdf:load-op system)))

  (when (database-initialize-database-type database-type)
    (push database-type *initialized-database-types*)
    (setf *default-database-type* database-type)
    database-type))

