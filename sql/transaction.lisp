;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; Transaction support
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass transaction ()
  ((commit-hooks :initform () :accessor commit-hooks)
   (rollback-hooks :initform () :accessor rollback-hooks)
   (previous-autocommit :initarg :previous-autocommit
                        :reader previous-autocommit)
   (status :initform nil :accessor transaction-status
           :documentation "nil or :committed")))

(defun add-transaction-commit-hook (commit-hook &key
                                    (database *default-database*))
  "Adds COMMIT-HOOK, which should a designator for a function
with no required arguments, to the list of hooks run when COMMIT
is called on DATABASE which defaults to *DEFAULT-DATABASE*."
  (when (transaction database)
    (push commit-hook (commit-hooks (transaction database)))))

(defun add-transaction-rollback-hook (rollback-hook
                                      &key (database *default-database*))
  "Adds ROLLBACK-HOOK, which should a designator for a function
with no required arguments, to the list of hooks run when ROLLBACK
is called on DATABASE which defaults to *DEFAULT-DATABASE*."
  (when (transaction database)
    (push rollback-hook (rollback-hooks (transaction database)))))

(defmethod database-start-transaction ((database database))
  (unless (transaction database)
    (setf (transaction database)
          (make-instance 'transaction :previous-autocommit
                         (database-autocommit database))))
  (setf (database-autocommit database) nil)
  (when (= (incf (transaction-level database)) 1)
    (let ((transaction (transaction database)))
      (setf (commit-hooks transaction) nil
            (rollback-hooks transaction) nil
            (transaction-status transaction) nil)
      (case (database-underlying-type database)
        (:oracle nil)
        (:mssql (execute-command "BEGIN TRANSACTION" :database database))
        (t (execute-command "BEGIN" :database database))))))

;;ODBC should potentially be using it's scheme for transactions:
;; turn off autocommit for begin. then use sqlendtran (or maybe sqltransact)
;; whatever is appropriate for this version of odbc.
(defmethod database-commit-transaction ((database database))
  (with-slots (transaction transaction-level autocommit) database
    (if (plusp transaction-level)
        (if (zerop (decf transaction-level))
            (progn
              (case (database-underlying-type database)
                (:mssql (execute-command "COMMIT TRANSACTION" :database database))
                (t (execute-command "COMMIT" :database database)))
              (setf autocommit (previous-autocommit transaction))
              (map nil #'funcall (commit-hooks transaction)))
            (setf (transaction-status (transaction database)) nil))
        (warn 'sql-warning
              :message
              (format nil "Cannot commit transaction against ~A because there is no transaction in progress."
                      database)))))

(defmethod database-abort-transaction ((database database))
  (with-slots (transaction transaction-level autocommit) database
    (if (plusp transaction-level)
        (when (zerop (decf transaction-level))
          (unwind-protect
               (case (database-underlying-type database)
                 (:mssql (execute-command "ROLLBACK TRANSACTION" :database database))
                 (t (execute-command "ROLLBACK" :database database)))
            (setf autocommit (previous-autocommit transaction))
            (map nil #'funcall (rollback-hooks transaction))))
        (warn 'sql-warning
              :message
              (format nil "Cannot abort transaction against ~A because there is no transaction in progress."
                      database)))))

(defun mark-transaction-committed (database)
  (when (and (transaction database)
             (not (transaction-status (transaction database))))
    (setf (transaction-status (transaction database)) :committed)))

(defmacro with-transaction ((&key (database '*default-database*)) &rest body)
  "Starts a transaction in the database specified by DATABASE,
which is *DEFAULT-DATABASE* by default, and executes BODY within
that transaction. If BODY aborts or throws, DATABASE is rolled
back and otherwise the transaction is committed."
  (let ((db (gensym "db-")))
    `(let ((,db ,database))
      (unwind-protect
           (prog2
             (database-start-transaction ,db)
             (progn
               ,@body)
             (mark-transaction-committed ,db))
        (if (eq (transaction-status (transaction ,db)) :committed)
            (database-commit-transaction ,db)
            (database-abort-transaction ,db))))))

(defun commit (&key (database *default-database*))
  "If DATABASE, which defaults to *DEFAULT-DATABASE*, is
currently within the scope of a transaction, commits changes made
since the transaction began."
  (database-commit-transaction database)
  nil)

(defun rollback (&key (database *default-database*))
  "If DATABASE, which defaults to *DEFAULT-DATABASE*, is
currently within the scope of a transaction, rolls back changes
made since the transaction began."
  (database-abort-transaction database)
  nil)

(defun start-transaction (&key (database *default-database*))
  "Starts a transaction block on DATABASE which defaults to
*DEFAULT-DATABASE* and which continues until ROLLBACK or COMMIT
are called."
  (unless (in-transaction-p :database database)
    (database-start-transaction database))
  nil)

(defun in-transaction-p (&key (database *default-database*))
  "A predicate to test whether DATABASE, which defaults to
*DEFAULT-DATABASE*, is currently within the scope of a
transaction."
  (and database (transaction database) (= (transaction-level database) 1)))

(defun set-autocommit (value &key (database *default-database*))
  "Turns autocommit off for DATABASE if VALUE is NIL, and
otherwise turns it on. Returns the old value of autocommit flag.
For RDBMS (such as Oracle) which don't automatically commit
changes, turning autocommit on has the effect of explicitly
committing changes made whenever SQL statements are executed.
Autocommit is turned on by default."
  (let ((old-value (database-autocommit database)))
    (setf (database-autocommit database) value)
    (database-autocommit database)
    old-value))

