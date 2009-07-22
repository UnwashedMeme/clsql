;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     postgresql-socket-sql.sql
;;;; Purpose:  High-level PostgreSQL interface using socket
;;;; Authors:  Kevin M. Rosenberg based on original code by Pierre R. Mai
;;;; Created:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage :clsql-postgresql-socket
    (:use #:common-lisp #:clsql-sys #:postgresql-socket)
    (:export #:postgresql-socket-database)
    (:documentation "This is the CLSQL socket interface to PostgreSQL."))

(in-package #:clsql-postgresql-socket)

;; interface foreign library loading routines


(clsql-sys:database-type-load-foreign :postgresql-socket)


;; Field type conversion

(defun make-type-list-for-auto (cursor)
  (let* ((fields (postgresql-cursor-fields cursor))
         (num-fields (length fields))
         (new-types '()))
    (dotimes (i num-fields)
      (declare (fixnum i))
      (push (canonical-field-type fields i) new-types))
    (nreverse new-types)))

(defun canonical-field-type (fields index)
  "Extracts canonical field type from fields list"
  (let ((oid (cadr (nth index fields))))
    (case oid
      ((#.pgsql-ftype#bytea
        #.pgsql-ftype#int2
        #.pgsql-ftype#int4)
       :int32)
      (#.pgsql-ftype#int8
       :int64)
      ((#.pgsql-ftype#float4
        #.pgsql-ftype#float8)
       :double)
      (otherwise
       t))))

(defun canonicalize-types (types cursor)
  (if (null types)
      nil
      (let ((auto-list (make-type-list-for-auto cursor)))
        (cond
          ((listp types)
           (canonicalize-type-list types auto-list))
          ((eq types :auto)
           auto-list)
          (t
           nil)))))

(defun canonicalize-type-list (types auto-list)
  "Ensure a field type list meets expectations.  Essentially if we get a
   generic term for a type that our auto typer pulls a better type for,
   use it instead"
  (let ((length-types (length types)))
    (loop for i from 0 below (length auto-list)
          for auto = (nth i auto-list)
          collect
       (if (or (>= i length-types)
               (member (nth i types) (list T :int :double)))
           auto
           (nth i types)))))


(defun convert-to-clsql-warning (database condition)
  (ecase *backend-warning-behavior*
    (:warn
     (warn 'sql-database-warning :database database
           :message (postgresql-condition-message condition)))
    (:error
     (error 'sql-database-error :database database
            :message (format nil "Warning upgraded to error: ~A"
                             (postgresql-condition-message condition))))
    ((:ignore nil)
     ;; do nothing
     )))

(defun convert-to-clsql-error (database expression condition)
  (error 'sql-database-data-error
         :database database
         :expression expression
         :error-id (type-of condition)
         :message (postgresql-condition-message condition)))

(defmacro with-postgresql-handlers
    ((database &optional expression)
     &body body)
  (let ((database-var (gensym))
        (expression-var (gensym)))
    `(let ((,database-var ,database)
           (,expression-var ,expression))
       (handler-bind ((postgresql-warning
                       (lambda (c)
                         (convert-to-clsql-warning ,database-var c)))
                      (postgresql-error
                       (lambda (c)
                         (convert-to-clsql-error
                          ,database-var ,expression-var c))))
         ,@body))))

(defmethod database-initialize-database-type ((database-type
                                               (eql :postgresql-socket)))
  t)

(defclass postgresql-socket-database (generic-postgresql-database)
  ((connection :accessor database-connection :initarg :connection
               :type postgresql-connection)))

(defmethod database-type ((database postgresql-socket-database))
  :postgresql-socket)

(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :postgresql-socket)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional port options tty)
      connection-spec
    (declare (ignore password options tty))
    (concatenate 'string
      (etypecase host
        (null
         "localhost")
        (pathname (namestring host))
        (string host))
      (when port
        (concatenate 'string
                     ":"
                     (etypecase port
                       (integer (write-to-string port))
                       (string port))))
      "/" db "/" user)))

(defmethod database-connect (connection-spec
                             (database-type (eql :postgresql-socket)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port options tty))
  (destructuring-bind (host db user password &optional
                            (port +postgresql-server-default-port+)
                            (options "") (tty ""))
      connection-spec
    (handler-case
        (handler-bind ((postgresql-warning
                        (lambda (c)
                          (warn 'sql-warning
                                :format-control "~A"
                                :format-arguments
                                (list (princ-to-string c))))))
          (open-postgresql-connection :host host :port port
                                      :options options :tty tty
                                      :database db :user user
                                      :password password))
      (postgresql-error (c)
        ;; Connect failed
        (error 'sql-connection-error
               :database-type database-type
               :connection-spec connection-spec
               :error-id (type-of c)
               :message (postgresql-condition-message c)))
      (:no-error (connection)
                 ;; Success, make instance
                 (make-instance 'postgresql-socket-database
                                :name (database-name-from-spec connection-spec
                                                               database-type)
                                :database-type :postgresql-socket
                                :connection-spec connection-spec
                                :connection connection)))))

(defmethod database-disconnect ((database postgresql-socket-database))
  (close-postgresql-connection (database-connection database))
  t)

(defmethod database-query (expression (database postgresql-socket-database) result-types field-names)
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      (start-query-execution connection expression)
      (multiple-value-bind (status cursor)
          (wait-for-query-results connection)
        (unless (eq status :cursor)
          (close-postgresql-connection connection)
          (error 'sql-database-data-error
                 :database database
                 :expression expression
                 :error-id "missing-result"
                 :message "Didn't receive result cursor for query."))
        (setq result-types (canonicalize-types result-types cursor))
        (values
         (loop for row = (read-cursor-row cursor result-types)
               while row
               collect row
               finally
               (unless (null (wait-for-query-results connection))
                 (close-postgresql-connection connection)
                 (error 'sql-database-data-error
                        :database database
                        :expression expression
                        :error-id "multiple-results"
                        :message "Received multiple results for query.")))
         (when field-names
           (mapcar #'car (postgresql-cursor-fields cursor))))))))

(defmethod database-execute-command
    (expression (database postgresql-socket-database))
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      (start-query-execution connection expression)
      (multiple-value-bind (status result)
          (wait-for-query-results connection)
        (when (eq status :cursor)
          (loop
            (multiple-value-bind (row stuff)
                (skip-cursor-row result)
              (unless row
                (setq status :completed result stuff)
                (return)))))
        (cond
         ((null status)
          t)
         ((eq status :completed)
          (unless (null (wait-for-query-results connection))
             (close-postgresql-connection connection)
             (error 'sql-database-data-error
                    :database database
                    :expression expression
                    :error-id "multiple-results"
                    :message "Received multiple results for command."))
          result)
          (t
           (close-postgresql-connection connection)
           (error 'sql-database-data-error
                  :database database
                  :expression expression
                  :errno "missing-result"
                  :message "Didn't receive completion for command.")))))))

(defstruct postgresql-socket-result-set
  (done nil)
  (cursor nil)
  (types nil))

(defmethod database-query-result-set ((expression string)
                                      (database postgresql-socket-database)
                                      &key full-set result-types)
  (declare (ignore full-set))
  (let ((connection (database-connection database)))
    (with-postgresql-handlers (database expression)
      (start-query-execution connection expression)
      (multiple-value-bind (status cursor)
          (wait-for-query-results connection)
        (unless (eq status :cursor)
          (close-postgresql-connection connection)
          (error 'sql-database-data-error
                 :database database
                 :expression expression
                 :error-id "missing-result"
                 :message "Didn't receive result cursor for query."))
        (values (make-postgresql-socket-result-set
                 :done nil
                 :cursor cursor
                 :types (canonicalize-types result-types cursor))
                (length (postgresql-cursor-fields cursor)))))))

(defmethod database-dump-result-set (result-set
                                     (database postgresql-socket-database))
  (if (postgresql-socket-result-set-done result-set)
      t
      (with-postgresql-handlers (database)
        (loop while (skip-cursor-row
                     (postgresql-socket-result-set-cursor result-set))
          finally (setf (postgresql-socket-result-set-done result-set) t)))))

(defmethod database-store-next-row (result-set
                                    (database postgresql-socket-database)
                                    list)
  (let ((cursor (postgresql-socket-result-set-cursor result-set)))
    (with-postgresql-handlers (database)
      (if (copy-cursor-row cursor
                           list
                           (postgresql-socket-result-set-types
                            result-set))
          t
          (prog1 nil
            (setf (postgresql-socket-result-set-done result-set) t)
            (wait-for-query-results (database-connection database)))))))

(defmethod database-create (connection-spec (type (eql :postgresql-socket)))
  (destructuring-bind (host name user password &optional port options tty) connection-spec
    (let ((database (database-connect (list host "postgres" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
           (database-execute-command (format nil "create database ~A" name) database)
        (database-disconnect database)))))

(defmethod database-destroy (connection-spec (type (eql :postgresql-socket)))
  (destructuring-bind (host name user password &optional port optional tty) connection-spec
    (let ((database (database-connect (list host "postgres" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
          (database-execute-command (format nil "drop database ~A" name) database)
        (database-disconnect database)))))


(defmethod database-probe (connection-spec (type (eql :postgresql-socket)))
  (when (find (second connection-spec) (database-list connection-spec type)
              :test #'string-equal)
    t))


;; Database capabilities

(defmethod db-backend-has-create/destroy-db? ((db-type (eql :postgresql-socket)))
  nil)

(defmethod db-type-has-fancy-math? ((db-type (eql :postgresql-socket)))
  t)

(defmethod db-type-default-case ((db-type (eql :postgresql-socket)))
  :lower)

(defmethod database-underlying-type ((database postgresql-socket-database))
  :postgresql)

(when (clsql-sys:database-type-library-loaded :postgresql-socket)
  (clsql-sys:initialize-database-type :database-type :postgresql-socket))
