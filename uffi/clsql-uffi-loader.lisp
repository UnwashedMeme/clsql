;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     clsql-uffi-loader.sql
;;;; Purpose:  Library loader using CLSQL UFFI helper library
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-uffi)

(defun find-and-load-foreign-library (filenames &key module supporting-libraries (errorp t))
  "Attempt to load a foreign library. This will search for any of the filenames, as
well as any of the filenames in any of the clsql:*foreign-library-search-paths*"
  (setq filenames (if (listp filenames) filenames (list filenames)))

  (flet ((try-load (testpath)
           (handler-case
               (uffi:load-foreign-library testpath
                                          :module module
                                          :supporting-libraries supporting-libraries)
             (error nil)))) ;(c) (warn "~A" c) nil))))
    (or
     (loop for type in (uffi:foreign-library-types)
           thereis
           (loop for name in filenames
                 for pn = (make-pathname :name name :type type)
                 thereis (or
                          (try-load pn)
                          (loop for search-path in clsql:*foreign-library-search-paths*
                                thereis (try-load (merge-pathnames pn search-path))))))
     (when errorp
       (error "Couldn't load foreign librar~@P ~{~S~^, ~}. (searched ~S)"
              (length filenames) filenames
              'clsql:*foreign-library-search-paths*)))))

;; searches clsql_uffi64 to accomodate both 32-bit and 64-bit libraries on same system
(defvar *clsql-uffi-library-filenames*
  `(,@(when (> most-positive-fixnum (expt 2 32)) (list "clsql_uffi64"))
    "clsql_uffi"))

(defvar *clsql-uffi-supporting-libraries* '("c")
  "Used only by CMU. List of library flags needed to be passed to ld to
load the MySQL client library succesfully.  If this differs at your site,
set to the right path before compiling or loading the system.")

(defvar *uffi-library-loaded* nil
  "T if foreign library was able to be loaded successfully")

;; (defun load-uffi-foreign-library ()
;;   (clsql:push-library-path clsql-uffi-system::*clsql-uffi-library-dir*)
;;   (find-and-load-foreign-library *clsql-uffi-library-filenames*
;;                                  :module "clsql-uffi"
;;                                  :supporting-libraries
;;                                  *clsql-uffi-supporting-libraries*)
;;   (setq *uffi-library-loaded* t))

;; (load-uffi-foreign-library)
