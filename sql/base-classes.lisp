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

;; the cache of a database.  It is a separate object, in case it should be made
;; global.  It lives inside a DATABASE-DESC
(defclass database-cache ()
  ;; we probably don't need a cache lock if a database-cache is not shared across threads, but
  ;; it can't hurt to cover the possibility it isn't
  ((cache-lock  :initform  (make-process-lock "database-cache") :accessor cache-lock)
   (record-caches :accessor record-caches :initform nil)
   (attribute-cache :initform (make-hash-table :size 100 :test 'equal)
                    :accessor attribute-cache
                    :documentation "Internal cache of table attributes. It is keyed by table-name. Values
are a list of ACTION specified for table and any cached value of list-attributes-types.")))


;; database recording is a separate class kept inside database-desc, so it is tied
;; to a database's immediate usage (ie, gets swapped in and out)
(defclass database-recorder ()
  ((recorder-lock  :initform  (make-process-lock "database-recorder") :accessor recorder-lock)
   (command-recording-stream :accessor command-recording-stream :initform nil)
   (result-recording-stream :accessor result-recording-stream :initform nil)))


;; a descriptor of a database, containing its identifying details, plus its cache.
;; this is inserted into the view-database slot of database objects, rather than
;; the whole (thread-unsafe) database.
(defclass database-desc ()
  ((name :initform nil :initarg :name :reader database-name)
   (connection-spec :initform nil :initarg :connection-spec
                    :reader connection-spec
                    :documentation "Required to use connection pool.")
   (database-type :initarg :database-type :initform :unknown
                  :reader database-type)
   (cache :initform (make-instance 'database-cache) :accessor database-cache)
   (recorder :initform (make-instance 'database-recorder) :accessor database-recorder)
   ;; the conn-pool in database-desc and database should be the same
   (conn-pool :initform nil :initarg :conn-pool :accessor conn-pool))
  (:documentation
   "Descriptor object identifying a database, carried inside a DATABASE object (which has the connection),
and placed into the VIEW-DATABASE slot of database objects."))

;; parent class of databases
(defclass database ()
  ((desc :initform (make-instance 'database-desc) :initarg :desc :accessor database-desc)
   ;; we also place the identifiers in database as well as in the
   ;; database-desc - there's no real reason for this because they
   ;; should always match, but this will allow sanity checks, and will
   ;; make the qualities genuinely invariant, because the
   ;; database-desc gets swapped in and out
   (name :initform nil :initarg :name :reader database-name)
   (connection-spec :initform nil :initarg :connection-spec
                    :reader connection-spec
                    :documentation "Required to use connection pool.")
   (database-type :initarg :database-type :initform :unknown
                  :reader database-type)
   ;;
   (state :initform :closed :reader database-state)
   (view-classes :accessor database-view-classes :initform nil)
   (transaction-level :initform 0 :accessor transaction-level)
   (transaction :initform nil :accessor transaction)
   ;; the conn-pool in database-desc and database should be the same
   ;; and they are crosschecked as a sanity check
   (conn-pool :initform nil :initarg :conn-pool :accessor conn-pool)
   ;; autocommit is an internal state variable of clsql (it seems) so
   ;; we put it in database, not database-desc
   (autocommit :initform t :accessor database-autocommit)
   (encoding :initarg :encoding :initform nil
             :documentation "External format character encoding.")
   ;; lock for changing thread
   (lock :initform  (make-process-lock "Database") :accessor database-lock)
   ;; the thread that currently owns this database
   (thread :initform (current-thread) :accessor database-thread
	   :documentation "The thread currently using this database"))
  (:documentation
   "This supertype class of all databases handled by CLSQL."))

;; helper function used in the back ends to build a database object
(defun build-database-object (class
			      &rest keyword-arg-list
			      &key connection-spec database-type
			      &allow-other-keys)
  (let ((name (database-name-from-spec connection-spec database-type)))
    (apply
     'make-instance
     class
     :desc (make-instance
	    'database-desc
	    :name name
	    :connection-spec connection-spec
	    :database-type database-type)
     :name name
     :connection-spec connection-spec
     :database-type database-type
     keyword-arg-list)))





;; define some easier accessors to get at database-desc fields in database
(defmethod record-caches ((db database))
  (record-caches (database-cache (database-desc db))))
(defmethod attribute-cache ((db database))
  (attribute-cache (database-cache (database-desc db))))

(defmethod attribute-cache ((db-desc database-desc))
  (attribute-cache (database-cache db-desc)))
(defmethod record-caches ((db-desc database-desc))
  (record-caches (database-cache db-desc)))

(defmethod cache-lock ((db-desc database-desc))
  (cache-lock (database-cache db-desc)))
(defmethod cache-lock ((db database))
  (cache-lock (database-cache (database-desc db))))

(defmethod (setf record-caches) ((thing t) (db-desc database-desc))
  (setf (record-caches (database-cache db-desc)) thing))
(defmethod (setf record-caches) ((thing t) (db database))
  (setf (record-caches (database-cache (database-desc db))) thing))


(defmethod command-recording-stream ((db-desc database-desc))
  (command-recording-stream (database-recorder db-desc)))
(defmethod command-recording-stream ((db database))
  (command-recording-stream (database-recorder (database-desc db))))
(defmethod (setf command-recording-stream) ((thing t) (db-desc database-desc))
  (setf (command-recording-stream (database-recorder db-desc)) thing))
(defmethod (setf command-recording-stream) ((thing t) (db database))
  (setf (command-recording-stream (database-recorder (database-desc db))) thing))

(defmethod result-recording-stream ((db-desc database-desc))
  (result-recording-stream (database-recorder db-desc)))
(defmethod result-recording-stream ((db database))
  (result-recording-stream (database-recorder (database-desc db))))
(defmethod (setf result-recording-stream) ((thing t) (db-desc database-desc))
  (setf (result-recording-stream (database-recorder db-desc)) thing))
(defmethod (setf result-recording-stream) ((thing t) (db database))
  (setf (result-recording-stream (database-recorder (database-desc db))) thing))

(defmethod database-recorder ((db database))
  (database-recorder (database-desc db)))

(defmethod recorder-lock ((db database))
  (recorder-lock (database-recorder (database-desc db))))
(defmethod recorder-lock ((db-desc database-desc))
  (recorder-lock (database-recorder db-desc)))



(defmethod print-object ((object database) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
            (if (and (slot-boundp object 'desc)
		     (typep (database-desc object) 'database-desc)
		     (slot-boundp  (database-desc object) 'name))
                (database-name object)
              "<unbound>")
            (database-state object)))
  object)

(defmethod print-object ((object database-desc) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
	    (if (slot-boundp object 'database-type)
                (database-type object)
		"<unknown-dataase-type>")
            (if (slot-boundp object 'name)
                (database-name object)
		"<unbound>")))
  object)


(setf (documentation 'database-name 'function)
      "Returns the name of a database.")

