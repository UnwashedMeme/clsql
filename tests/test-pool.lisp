;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    test-pool.lisp
;;;; Purpose: Tests for connection pools
;;;; Author:  Ryan Davis
;;;; Created: June 27 2011
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************
(in-package #:clsql-tests)

;; setup a dummy database for the pool to use
(pushnew :dummy clsql-sys:*loaded-database-types*)
(defclass dummy-database (clsql-sys:database) ()
  (:default-initargs :database-type :dummy))
(defmethod clsql-sys:database-connect (connection-spec (database-type (eql :dummy)))
  (let ((db (make-instance 'dummy-database :connection-spec connection-spec)))
    (setf (slot-value db 'clsql-sys::state) :open)
    db))
(defmethod clsql-sys::database-name-from-spec (connection-spec (database-type (eql :dummy)))
  "dummy")
(defmethod clsql-sys::database-acquire-from-conn-pool ((db dummy-database)) T)

(setq *rt-pool*
  '(
    (deftest :pool/acquire
     (let ((pool (clsql-sys::find-or-create-connection-pool nil :dummy))
           dbx res)
       (clsql-sys::clear-conn-pool pool)
       (flet ((test-result (x) (push x res)))
         (test-result (length (clsql-sys::all-connections pool)))
         (test-result (length (clsql-sys::free-connections pool)))

         (clsql-sys:with-database (db nil :database-type :dummy :pool T)
           (test-result (not (null db)))
           (test-result (length (clsql-sys::all-connections pool)))
           (test-result (length (clsql-sys::free-connections pool)))
           (setf dbx db))
         (test-result (length (clsql-sys::all-connections pool)))
         (test-result (length (clsql-sys::free-connections pool)))
         (clsql-sys:with-database (db nil :database-type :dummy :pool T)
           (test-result (eq db dbx)))
         )
       (nreverse res))
     (0 0 T 1 0 1 1 T)
     )

    (deftest :pool/max-free-connections
     (let ((pool (clsql-sys::find-or-create-connection-pool nil :dummy)))
       (flet ((run (max-free dbs-to-release)
                (let ((clsql-sys:*db-pool-max-free-connections* max-free)
                      dbs)
                  (clsql-sys::clear-conn-pool pool)
                  (dotimes (i dbs-to-release dbs)
                    (push (clsql-sys:connect nil :database-type :dummy
                                                 :pool T :if-exists :new)
                          dbs))
                  (list (length (clsql-sys::all-connections pool))
                        (progn
                          (dolist (db dbs) (clsql-sys:disconnect :database db))
                          (length (clsql-sys::free-connections pool))
                          )))))
         (append
          (run 5 10)
          (run nil 10))))
     (10 5 10 10)
     )

    (deftest :pool/too-many-sql-operations-db-eviction
     (let* ((spec (get-universal-time))
            (pool (clsql-sys::find-or-create-connection-pool spec :dummy)))
       (flet ((run (max-ops ops)
                (let ((clsql-sys:*db-pool-max-sql-op-executions* max-ops)
                      (db (clsql-sys:connect spec :database-type :dummy :pool T)))
                  (setf (clsql-sys::sql-operations-executed db) ops)
                  (clsql-sys:disconnect :database db)
                  ;;db should be in the free list
                  (prog1
                      (if (member db (clsql-sys::free-connections pool))
                          T nil)
                    (clsql-sys:disconnect-pooled )))

                ))
         (values
          (run nil 50000)
          (run 10 5)
          (run 10 10)
          (run 10 100))))
     T T nil nil)

    (deftest :pool/find-or-create-connection-pool
     (let ((p (clsql-sys::find-or-create-connection-pool nil :dummy)))
       (values (null p)
               (eq p (clsql-sys::find-or-create-connection-pool nil :dummy))
               (eq p (clsql-sys::find-or-create-connection-pool :spec :dummy))))
     nil T nil)

    (deftest :pool/sql-operations-executed
     (let* ((spec (get-universal-time))
            (pool (clsql-sys::find-or-create-connection-pool spec :dummy))
            res)
       (flet ((test-result (x) (push x res))
              (bool (x) (if x T nil)))
         ;;should start with no operations
         (test-result (clsql-sys::sql-operations-executed pool))
         ;;fake 10 ops on the db
         (clsql-sys:with-database (db spec :pool T :database-type :dummy)
           (incf (clsql-sys::sql-operations-executed db) 10))
         ;;pool should have the 10 ops
         (test-result (clsql-sys::sql-operations-executed pool))

         (clsql-sys:with-database (db spec :pool T :database-type :dummy)
           ;;don't do any queries
           )
         ;;should still have 10 ops
         (test-result (clsql-sys::sql-operations-executed pool))
         ;; simulate a full pool
         (let ((clsql-sys:*db-pool-max-free-connections* 0) dbx)
           (clsql-sys:with-database (db spec :pool T :database-type :dummy)
             (incf (clsql-sys::sql-operations-executed db) 10))
           ;; no more connections
           (test-result (length (clsql-sys::all-connections pool)))
           ;;pool should have another 10 ops
           (test-result (clsql-sys::sql-operations-executed pool))
           ;;db should not be in the free list
           (test-result (length (clsql-sys::free-connections pool))))
         (clsql-sys:disconnect-pooled)
         ;;should be all be cleared
         (test-result (clsql-sys::sql-operations-executed pool)))
       (nreverse res))
     (0 10 10 0 20 0 0)
     )
    ))
