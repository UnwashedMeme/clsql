;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     sqlite-sql.lisp
;;;; Purpose:  High-level SQLite3 interface
;;;; Authors:  Aurelio Bignoli & Kevin Rosenberg
;;;; Created:  Oct 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004-2010 by Aurelio Bignoli & Kevin Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sqlite3)

(defclass sqlite3-database (database)
  ((sqlite3-db :initarg :sqlite3-db :accessor sqlite3-db)))

(defmethod database-type ((database sqlite3-database))
  :sqlite3)

(defmethod database-initialize-database-type ((database-type (eql :sqlite3)))
  t)

(defun check-sqlite3-connection-spec (connection-spec)
  (check-connection-spec connection-spec :sqlite3 (name &optional init-foreign-func)))

(defmethod database-name-from-spec (connection-spec
                                    (database-type (eql :sqlite3)))
  (check-sqlite3-connection-spec connection-spec)
  (princ-to-string (first connection-spec)))

(defmethod database-connect (connection-spec (database-type (eql :sqlite3)))
  (check-sqlite3-connection-spec connection-spec)
    (handler-case
        (let ((db (sqlite3:sqlite3-open (first connection-spec)))
              (init-foreign-func (second connection-spec)))
          (declare (type sqlite3:sqlite3-db-type db))
          (when init-foreign-func
            (handler-case
                (funcall init-foreign-func db)
              (condition (c)
                (progn
                  (sqlite3:sqlite3-close db)
                  (error c)))))
          (make-instance 'sqlite3-database
                         :name (database-name-from-spec connection-spec :sqlite3)
                         :database-type :sqlite3
                         :connection-spec connection-spec
                         :sqlite3-db db))
      (sqlite3:sqlite3-error (err)
        (error 'sql-connection-error
               :database-type database-type
               :connection-spec connection-spec
               :error-id (sqlite3:sqlite3-error-code err)
               :message (sqlite3:sqlite3-error-message err)))))

(defmethod database-disconnect ((database sqlite3-database))
  (sqlite3:sqlite3-close (sqlite3-db database))
  (setf (sqlite3-db database) nil)
  t)

(defmethod database-execute-command (sql-expression (database sqlite3-database))
  (handler-case
      (let ((stmt (sqlite3:sqlite3-prepare (sqlite3-db database) sql-expression)))
        (declare (type sqlite3:sqlite3-stmt-type stmt))
        (when stmt
          (unwind-protect
               (sqlite3:sqlite3-step stmt)
            (sqlite3:sqlite3-finalize stmt))))
    (sqlite3:sqlite3-error (err)
      (error 'sql-database-data-error
             :database database
             :expression sql-expression
             :error-id (sqlite3:sqlite3-error-code err)
             :message (sqlite3:sqlite3-error-message err))))
  t)

(defstruct sqlite3-result-set
  (stmt sqlite3:null-stmt
        :type sqlite3:sqlite3-stmt-type)
  (n-col 0 :type fixnum)
  (col-names '())
  (result-types '()))

(declaim (ftype (function (sqlite3:sqlite3-stmt-type fixnum t) list) get-result-types))
(defun get-result-types (stmt n-col result-types)
  (declare (type sqlite3:sqlite3-stmt-type stmt) (type fixnum n-col))
  (if (eq :auto result-types)
      (loop for n from 0 below n-col
            collect (let ((column-type (sqlite3:sqlite3-column-type stmt n)))
                      (cond
                        ((= column-type sqlite3:SQLITE-INTEGER) :int64)
                        ((= column-type sqlite3:SQLITE-FLOAT) :double)
                        ((= column-type sqlite3:SQLITE-TEXT) :string)
                        ((= column-type sqlite3:SQLITE-BLOB) :blob)
                        ((= column-type sqlite3:SQLITE-NULL) :string)
                        (t :string))))
      (loop for type in result-types
            collect (case type
                      ((:int :integer :tinyint) :int32)
                      (:long #+(or x86-64 64bit) :int64 #-(or x86-64 64bit) :int32)
                      (:bigint :int64)
                      ((:float :double) :double)
                      ((:numeric) :number)
                      (otherwise :string)))))

(defmethod database-query-result-set ((query-expression string)
                                      (database sqlite3-database)
                                      &key result-types full-set)
  (let ((stmt sqlite3:null-stmt))
    (declare (type sqlite3:sqlite3-stmt-type stmt))
    (handler-case
        (progn
          (setf stmt (sqlite3:sqlite3-prepare (sqlite3-db database)
                                              query-expression))
          (let* ((n-col (if (sqlite3:sqlite3-step stmt)
                            ;; Non empty result set.
                            (sqlite3:sqlite3-column-count stmt)
                            ;; Empty result set.
                            0))
                 (result-set (make-sqlite3-result-set
                              :stmt stmt
                              :n-col n-col
                              :col-names (loop for n from 0 below n-col
                                               collect (sqlite3:sqlite3-column-name stmt n))
                              :result-types (when (> n-col 0)
                                              (get-result-types stmt n-col result-types)))))
            (if full-set
                (values result-set n-col nil)
                (values result-set n-col))))
    (sqlite3:sqlite3-error (err)
        (progn
          (unless (eq stmt sqlite3:null-stmt)
            (ignore-errors
              (sqlite3:sqlite3-finalize stmt)))
          (error 'sql-database-data-error
                 :database database
                 :expression query-expression
                 :error-id (sqlite3:sqlite3-error-code err)
                 :message (sqlite3:sqlite3-error-message err)))))))

(defmethod database-dump-result-set (result-set (database sqlite3-database))
  (handler-case
      (sqlite3:sqlite3-finalize (sqlite3-result-set-stmt result-set))
    (sqlite3:sqlite3-error (err)
      (error 'sql-database-error
             :message
             (format nil "Error finalizing SQLite3 statement: ~A"
                     (sqlite3:sqlite3-error-message err))))))

(defmethod database-store-next-row (result-set (database sqlite3-database) list)
  (let ((n-col (sqlite3-result-set-n-col result-set)))
    (if (= n-col 0)
        ;; empty result set.
        nil
        ;; Non-empty set.
        (let ((stmt (sqlite3-result-set-stmt result-set)))
          (declare (type sqlite3:sqlite3-stmt-type stmt))
          ;; Store row in list.
          (loop for i = 0 then (1+ i)
                for rest on list
                for types = (sqlite3-result-set-result-types result-set) then (rest types)
                do (setf (car rest)
                         (if (eq (first types) :blob)
                             (clsql-uffi:convert-raw-field
                              (sqlite3:sqlite3-column-blob stmt i)
                              (car types)
                              :length (sqlite3:sqlite3-column-bytes stmt i)
                              :encoding (encoding database))
                             (clsql-uffi:convert-raw-field
                              (sqlite3:sqlite3-column-text stmt i)
                              (car types)
                              :encoding (encoding database)))))
          ;; Advance result set cursor.
          (handler-case
              (unless (sqlite3:sqlite3-step stmt)
                (setf (sqlite3-result-set-n-col result-set) 0))
            (sqlite3:sqlite3-error (err)
              (error 'sql-database-error
                     :message (format nil "Error in sqlite3-step: ~A"
                                      (sqlite3:sqlite3-error-message err)))))
          t))))


(defmethod database-query (query-expression (database sqlite3-database) result-types field-names)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (handler-case
      (let ((stmt (sqlite3:sqlite3-prepare (sqlite3-db database)
                                           query-expression))
            (rows '())
            (col-names '()))
        (declare (type sqlite3:sqlite3-stmt-type stmt))
        (unwind-protect
             (when (sqlite3:sqlite3-step stmt)
               (let ((n-col (sqlite3:sqlite3-column-count stmt)))
                 (flet ((extract-row-data ()
                          (loop for i from 0 below n-col
                                for types = (get-result-types stmt n-col result-types) then (rest types)
                                collect (if (eq (first types) :blob)
                                            (clsql-uffi:convert-raw-field
                                             (sqlite3:sqlite3-column-blob stmt i)
                                             (car types)
                                             :length (sqlite3:sqlite3-column-bytes stmt i)
                                             :encoding (encoding database))
                                            (clsql-uffi:convert-raw-field
                                             (sqlite3:sqlite3-column-text stmt i)
                                             (car types)
                                             :encoding (encoding database))))))
                   (when field-names
                     (setf col-names (loop for n from 0 below n-col
                                           collect (sqlite3:sqlite3-column-name stmt n))))
                   (push (extract-row-data) rows)
                   (do* () (nil)
                     (if (sqlite3:sqlite3-step stmt)
                         (push (extract-row-data) rows)
                         (return))))))
               (sqlite3:sqlite3-finalize stmt))
        (values (nreverse rows) col-names))
    (sqlite3:sqlite3-error (err)
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :error-id (sqlite3:sqlite3-error-code err)
             :message (sqlite3:sqlite3-error-message err)))))

;;; Object listing

(defmethod database-list-tables-and-sequences ((database sqlite3-database) &key owner)
  (declare (ignore owner))
  ;; Query is copied from .table command of sqlite3 command line utility.
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='table' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='table' ORDER BY name"
                 database nil nil)))

(defmethod database-list-tables ((database sqlite3-database) &key owner)
  (remove-if #'(lambda (s)
                 (and (>= (length s) 11)
                      (string-equal (subseq s 0 11) "_CLSQL_SEQ_")))
             (database-list-tables-and-sequences database :owner owner)))

(defmethod database-list-views ((database sqlite3-database)
                                &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='view' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='view' ORDER BY name"
                 database nil nil)))

(defmethod database-list-indexes ((database sqlite3-database)
                                  &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car (database-query
                 "SELECT name FROM sqlite_master WHERE type='index' UNION ALL SELECT name FROM sqlite_temp_master WHERE type='index' ORDER BY name"
                 database nil nil)))

(defmethod database-list-table-indexes (table (database sqlite3-database)
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

(declaim (inline sqlite3-table-info))
(defun sqlite3-table-info (table database)
  (database-query (format nil "PRAGMA table_info('~A')" table)
                  database nil nil))

(defmethod database-list-attributes (table (database sqlite3-database)
                                           &key (owner nil))
  (declare (ignore owner))
  (mapcar #'(lambda (table-info) (second table-info))
          (sqlite3-table-info table database)))

(defmethod database-attribute-type (attribute table
                                    (database sqlite3-database)
                                    &key (owner nil))
  (declare (ignore owner))

  (loop for field-info in (sqlite3-table-info table database)
      when (string= attribute (second field-info))
      return
        (let* ((raw-type (third field-info))
               (start-length (position #\( raw-type))
               (type (string-trim clsql-sys::+whitespace-chars+
				  (if start-length
				      (subseq raw-type 0 start-length)
				      raw-type)))
               (length (if start-length
                           (parse-integer (subseq raw-type (1+ start-length))
                                          :junk-allowed t)
                         nil)))
          (values (when type (ensure-keyword type))
                  length
                  nil
                  (if (string-equal (fourth field-info) "0")
                      1 0)))))

(defmethod database-last-auto-increment-id ((database sqlite3-database) table column)
  (declare (ignore table column))
  (car (query "SELECT LAST_INSERT_ROWID();"
	      :flatp t :field-names nil
	      :database database)))

(defmethod database-create (connection-spec (type (eql :sqlite3)))
  (declare (ignore connection-spec))
  ;; databases are created automatically by Sqlite3
  t)

(defmethod database-destroy (connection-spec (type (eql :sqlite3)))
  (destructuring-bind (name) connection-spec
    (if (probe-file name)
        (delete-file name)
        nil)))

(defmethod database-probe (connection-spec (type (eql :sqlite3)))
  (destructuring-bind (name) connection-spec
    ;; TODO: Add a test that this file is a real sqlite3 database
    (or (string-equal ":memory:" name)
        (and (probe-file name) t))))

(defmethod database-get-type-specifier ((type (eql 'integer))
                                        args database
                                        (db-type (eql :sqlite3)))
  (declare (ignore database))
  (if args
      (format nil "INTEGER(~A)" (car args))
      "INTEGER"))

(defmethod database-get-type-specifier ((type (eql 'integer))
                                        args database
                                        (db-type (eql :sqlite3)))
  (declare (ignore database))
  (if args
      (format nil "INTEGER(~A)" (car args))
      "INTEGER"))

;;; Database capabilities

(defmethod db-type-has-boolean-where? ((db-type (eql :sqlite3)))
  nil)

(defmethod db-type-has-auto-increment? ((db-type (eql :sqlite3)))
  t)
