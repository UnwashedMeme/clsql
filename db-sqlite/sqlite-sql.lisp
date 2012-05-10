;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     sqlite-sql.lisp
;;;; Purpose:  High-level SQLite interface
;;;; Authors:  Aurelio Bignoli, Kevin Rosenberg, Marcus Pearce
;;;; Created:  Aug 2003
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2003 by Aurelio Bignoli and
;;;; Copyright (c) 2003-2010 by Kevin Rosenberg and Marcus Pearce.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sqlite)

(defclass sqlite-database (database)
  ((sqlite-db :initarg :sqlite-db :accessor sqlite-db)))

(defmethod database-type ((database sqlite-database))
  :sqlite)

(defmethod database-initialize-database-type ((database-type (eql :sqlite)))
  t)

(defun check-sqlite-connection-spec (connection-spec)
  (check-connection-spec connection-spec :sqlite (name)))

(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :sqlite)))
  (check-sqlite-connection-spec connection-spec)
  (first connection-spec))

(defmethod database-connect (connection-spec (database-type (eql :sqlite)))
  (check-sqlite-connection-spec connection-spec)
  (handler-case
      (clsql-sys:build-database-object
       'sqlite-database
       :database-type database-type
       :connection-spec connection-spec
       :sqlite-db (sqlite:sqlite-open (first connection-spec)))
    (sqlite:sqlite-error (err)
      (error 'sql-connection-error
             :database-type database-type
             :connection-spec connection-spec
             :error-id (sqlite:sqlite-error-code err)
             :message (sqlite:sqlite-error-message err)))))

(defmethod database-disconnect ((database sqlite-database))
  (sqlite:sqlite-close (sqlite-db database))
  (setf (sqlite-db database) nil)
  t)

(defmethod database-execute-command (sql-expression (database sqlite-database))
  (handler-case
      (multiple-value-bind (data row-n col-n)
          (sqlite:sqlite-get-table (sqlite-db database) sql-expression)
        (sqlite:sqlite-free-table data)
        (unless (= row-n 0)
          (error 'sql-warning
                 :format-control
                 "Result set not empty: ~@(~A~) row~:P, ~@(~A~) column~:P "
                 :format-arguments (list row-n col-n))))
    (sqlite:sqlite-error (err)
      (error 'sql-database-data-error
             :database database
             :expression sql-expression
             :error-id (sqlite:sqlite-error-code err)
             :message (sqlite:sqlite-error-message err))))
  t)

(defstruct sqlite-result-set
  (vm (sqlite:make-null-vm)
      :type sqlite:sqlite-vm-pointer)
  (first-row (sqlite:make-null-row)
             :type sqlite:sqlite-row-pointer-type)
  (col-names (sqlite:make-null-row)
             :type sqlite:sqlite-row-pointer-type)
  (result-types nil)
  (n-col 0 :type fixnum))

(defmethod database-query (query-expression (database sqlite-database) result-types field-names)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (handler-case
      (let ((vm (sqlite:sqlite-compile (sqlite-db database)
                                       query-expression))
            (rows '())
            (col-names '()))
        (unwind-protect
             ;; Read the first row to get column number and names.
             (multiple-value-bind (n-col new-row sqlite-col-names)
                 (sqlite:sqlite-step vm)
               (declare (type sqlite:sqlite-row-pointer-type new-row))
               (when (> n-col 0)
                 (when field-names
                   (setf col-names (loop for i from 0 below n-col
                                         collect (sqlite:sqlite-aref sqlite-col-names i (encoding database)))))
                 (let ((canonicalized-result-types
                        (canonicalize-result-types result-types n-col sqlite-col-names)))
                   (flet ((extract-row-data (row)
                            (declare (type sqlite:sqlite-row-pointer-type row))
                            (loop for i from 0 below n-col
                                  collect (clsql-uffi:convert-raw-field
                                           (sqlite:sqlite-raw-aref row i)
                                           (nth i canonicalized-result-types)
                                           :encoding (encoding database)))))
                     (push (extract-row-data new-row) rows)

                     ;; Read subsequent rows.
                     (do () (nil)
                       (multiple-value-bind (n-col new-row)
                           (sqlite:sqlite-step vm)
                         (declare (type sqlite:sqlite-row-pointer-type new-row))
                         (if (> n-col 0)
                             (push (extract-row-data new-row) rows)
                             (return))))))))
          (sqlite:sqlite-finalize vm))
        (values (nreverse rows) col-names))
    (sqlite:sqlite-error (err)
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :error-id (sqlite:sqlite-error-code err)
             :message (sqlite:sqlite-error-message err)))))

(defmethod database-query-result-set ((query-expression string)
                                      (database sqlite-database)
                                      &key result-types full-set)
  (let ((vm nil))
    (handler-case
        (progn
          (setf vm (sqlite:sqlite-compile (sqlite-db database)
                                          query-expression))
          ;;; To obtain column number/datatypes we have to read the first row.
          (multiple-value-bind (n-col cols col-names)
              (sqlite:sqlite-step vm)
            (declare (type sqlite:sqlite-row-pointer-type cols))
            (let ((result-set (make-sqlite-result-set
                               :vm vm
                               :first-row cols
                               :n-col n-col
                               :col-names col-names
                               :result-types
                               (canonicalize-result-types
                                result-types
                                n-col
                                col-names))))
              (if full-set
                  (values result-set n-col nil)
                  (values result-set n-col)))))
      (sqlite:sqlite-error (err)
        (progn
          (when vm
            ;; The condition was thrown by sqlite-step, vm must be
            ;; deallocated.
            (ignore-errors
              (sqlite:sqlite-finalize vm)))
          (error 'sql-database-data-error
                 :database database
                 :expression query-expression
                 :error-id (sqlite:sqlite-error-code err)
                 :message (sqlite:sqlite-error-message err))11)))))

(defun canonicalize-result-types (result-types n-col col-names)
  (when result-types
    (let ((raw-types (if (eq :auto result-types)
                         (loop for j from n-col below (* 2 n-col)
                               collect (ensure-keyword (sqlite:sqlite-aref col-names j)))
                       result-types)))
      (loop for type in raw-types
            collect
            (case type
              ((:int :integer :tinyint)
               :int32)
              (:long #+(or x86-64 64bit) :int64 #-(or x86-64 64bit) :int32)
              (:bigint
               :int64)
              ((:float :double)
               :double)
              ((:numeric)
               :number)
              (otherwise
               :string))))))

(defmethod database-dump-result-set (result-set (database sqlite-database))
  (handler-case
      (sqlite:sqlite-finalize (sqlite-result-set-vm result-set))
    (sqlite:sqlite-error (err)
      (error 'sql-database-error
             :message
             (format nil "Error finalizing SQLite VM: ~A"
                     (sqlite:sqlite-error-message err))))))

(defmethod database-store-next-row (result-set (database sqlite-database) list)
  (let ((n-col (sqlite-result-set-n-col result-set))
        (result-types (sqlite-result-set-result-types result-set)))
    (if (= n-col 0)
        ;; empty result set
        nil
        (let ((row (sqlite-result-set-first-row result-set)))
          (if (sqlite:null-row-p row)
              ;; First row already used. fetch another row from DB.
              (handler-case
                  (multiple-value-bind (n new-row col-names)
                      (sqlite:sqlite-step (sqlite-result-set-vm result-set))
                    (declare (ignore n col-names)
                             (type sqlite:sqlite-row-pointer-type new-row))
                    (if (sqlite:null-row-p new-row)
                        (return-from database-store-next-row nil)
                        (setf row new-row)))
                (sqlite:sqlite-error (err)
                  (error 'sql-database-error
                         :message
                         (format nil "Error in sqlite-step: ~A"
                                 (sqlite:sqlite-error-message err)))))

            ;; Use the row previously read by database-query-result-set.
              (setf (sqlite-result-set-first-row result-set)
                    (sqlite:make-null-row)))
          (loop for i = 0 then (1+ i)
                for rest on list
                do (setf (car rest)
                         (clsql-uffi:convert-raw-field
                          (sqlite:sqlite-raw-aref row i)
                          (nth i result-types)
                          :encoding (encoding database))))
          (sqlite:sqlite-free-row row)
          t))))

;;; Object listing

(defmethod database-list-tables-and-sequences ((database sqlite-database) &key owner)
  (declare (ignore owner))
  ;; Query is copied from .table command of sqlite comamnd line utility.
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
                 database nil nil)))

(defmethod database-list-tables ((database sqlite-database) &key owner)
  (remove-if #'(lambda (s)
                 (and (>= (length s) 11)
                      (string-equal (subseq s 0 11) "_CLSQL_SEQ_")))
             (database-list-tables-and-sequences database :owner owner)))

(defmethod database-list-views ((database sqlite-database)
                                &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='view' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='view' ORDER BY name"
                 database nil nil)))

(defmethod database-list-indexes ((database sqlite-database)
                                  &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='index' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' ORDER BY name"
                 database nil nil)))

(defmethod database-list-table-indexes (table (database sqlite-database)
                                        &key (owner nil))
  (declare (ignore owner))
  (let ((*print-circle* nil))
    (mapcar #'car
            (database-query
             (format
              nil
              "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='~A' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' AND tbl_name='~A' ORDER BY name"
              table table)
             database nil nil))))

(declaim (inline sqlite-table-info))
(defun sqlite-table-info (table database)
  (database-query (format nil "PRAGMA table_info('~A')" table)
                  database nil nil))

(defmethod database-list-attributes (table (database sqlite-database)
                                           &key (owner nil))
  (declare (ignore owner))
  (mapcar #'(lambda (table-info) (second table-info))
          (sqlite-table-info table database)))

(defmethod database-attribute-type (attribute table
                                    (database sqlite-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (loop for field-info in (sqlite-table-info table database)
      when (string= attribute (second field-info))
      return
        (let* ((raw-type (third field-info))
               (start-length (position #\( raw-type))
               (type (if start-length
                         (subseq raw-type 0 start-length)
                       raw-type))
               (length (if start-length
                           (parse-integer (subseq raw-type (1+ start-length))
                                          :junk-allowed t)
                         nil)))
          (values (when type (ensure-keyword type))
                  length
                  nil
                  (if (string-equal (fourth field-info) "0")
                      1 0)))))

(defmethod database-create (connection-spec (type (eql :sqlite)))
  (declare (ignore connection-spec))
  ;; databases are created automatically by SQLite
  t)

(defmethod database-destroy (connection-spec (type (eql :sqlite)))
  (destructuring-bind (name) connection-spec
    (if (probe-file name)
        (delete-file name)
        nil)))

(defmethod database-probe (connection-spec (type (eql :sqlite)))
  (destructuring-bind (name) connection-spec
    ;; TODO: Add a test that this file is a real sqlite database
    (or (string-equal ":memory:" name)
        (and (probe-file name) t))))

;;; Database capabilities

(defmethod db-type-has-boolean-where? ((db-type (eql :sqlite)))
  nil)



