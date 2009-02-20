;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          pool.lisp
;;;; Purpose:       Support function for connection pool
;;;; Programmers:   Kevin M. Rosenberg, Marc Battyani
;;;; Date Started:  Apr 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar *db-pool* (make-hash-table :test #'equal))
(defvar *db-pool-lock* (make-process-lock "DB Pool lock"))

(defclass conn-pool ()
  ((connection-spec :accessor connection-spec :initarg :connection-spec)
   (database-type :accessor pool-database-type :initarg :pool-database-type)
   (free-connections :accessor free-connections
                     :initform (make-array 5 :fill-pointer 0 :adjustable t))
   (all-connections :accessor all-connections
                    :initform (make-array 5 :fill-pointer 0 :adjustable t))
   (lock :accessor conn-pool-lock
         :initform (make-process-lock "Connection pool"))))

(defun acquire-from-conn-pool (pool)
  "Try to find a working database connection in the pool or create a new
one if needed. This performs 1 query against the DB to ensure it's still
valid. When possible (postgres, mssql) that query will be a reset
command to put the connection back into its default state."
  (or (with-process-lock ((conn-pool-lock pool) "Acquire from pool")
        (when (plusp (length (free-connections pool)))
          (let ((pconn (vector-pop (free-connections pool))))
            ;; test if connection still valid.
	    ;; (e.g. db reboot -> invalid connection )
	    (handler-case
		(case (database-underlying-type pconn)
		  (:postgresql
		     (database-execute-command "RESET ALL" pconn))
		  (:mysql
		     (database-query "SHOW ERRORS LIMIT 1" pconn nil nil))
		  (:mssql
		     ;; rpc escape sequence since this can't be called as a normal sp.
		     ;;http://msdn.microsoft.com/en-us/library/aa198358%28SQL.80%29.aspx
		     (database-execute-command "{rpc sp_reset_connection}" pconn))
		  (T
		     (database-query "SELECT 1;"  pconn '(integer) nil)))
	      (sql-database-error (e)
		;; we could check for a specific error,
		;; but, it's safer just to disconnect the pooled conn for any error ?
		(warn "Database connection ~S had an error when attempted to be acquired from the pool:
  ~S
Disconnecting.~%"
		      pconn e)
		(ignore-errors (database-disconnect pconn))
		nil)
	      (:no-error (&rest args)
		(declare (ignore args))
		pconn)))))
      (let ((conn (connect (connection-spec pool)
                           :database-type (pool-database-type pool)
                           :if-exists :new
                           :make-default nil)))
        (with-process-lock ((conn-pool-lock pool) "Acquire from pool")
          (vector-push-extend conn (all-connections pool))
          (setf (conn-pool conn) pool))
        conn)))

(defun release-to-conn-pool (conn)
  (let ((pool (conn-pool conn)))
    (with-process-lock ((conn-pool-lock pool) "Release to pool")
      (vector-push-extend conn (free-connections pool)))))

(defun clear-conn-pool (pool)
  (with-process-lock ((conn-pool-lock pool) "Clear pool")
    (loop for conn across (all-connections pool)
          do (setf (conn-pool conn) nil)
          ;; disconnect may error if remote side closed connection
          (ignore-errors (disconnect :database conn)))
    (setf (fill-pointer (free-connections pool)) 0)
    (setf (fill-pointer (all-connections pool)) 0))
  nil)

(defun find-or-create-connection-pool (connection-spec database-type)
  "Find connection pool in hash table, creates a new connection pool
if not found"
  (with-process-lock (*db-pool-lock* "Find-or-create connection")
    (let* ((key (list connection-spec database-type))
           (conn-pool (gethash key *db-pool*)))
      (unless conn-pool
        (setq conn-pool (make-instance 'conn-pool
                                       :connection-spec connection-spec
                                       :pool-database-type database-type))
        (setf (gethash key *db-pool*) conn-pool))
      conn-pool)))

(defun acquire-from-pool (connection-spec database-type &optional pool)
  (unless (typep pool 'conn-pool)
    (setf pool (find-or-create-connection-pool connection-spec database-type)))
  (acquire-from-conn-pool pool))

(defun release-to-pool (database)
  (release-to-conn-pool database))

(defun disconnect-pooled (&optional clear)
  "Disconnects all connections in the pool."
  (with-process-lock (*db-pool-lock* "Disconnect pooled")
    (maphash
     #'(lambda (key conn-pool)
         (declare (ignore key))
         (clear-conn-pool conn-pool))
     *db-pool*)
    (when clear (clrhash *db-pool*)))
  t)

;(defun pool-start-sql-recording (pool &key (types :command))
;  "Start all stream in the pool recording actions of TYPES"
;  (dolist (con (pool-connections pool))
;    (start-sql-recording :type types
;                        :database (connection-database con))))

;(defun pool-stop-sql-recording (pool &key (types :command))
;  "Start all stream in the pool recording actions of TYPES"
;  (dolist (con (pool-connections pool))
;    (stop-sql-recording :type types
;                         :database (connection-database con))))

;(defmacro with-database-connection (pool &body body)
;  `(let ((connection (obtain-connection ,pool))
;         (results nil))
;    (unwind-protect
;         (with-database ((connection-database connection))
;           (setq results (multiple-value-list (progn ,@body))))
;      (release-connection connection))
;    (values-list results)))
