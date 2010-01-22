;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Generic ODBC layer, used by db-odbc and db-aodbc backends
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass generic-odbc-database (database)
  ((dbi-package :initarg :dbi-package :reader dbi-package)
   (odbc-conn :initarg :odbc-conn :initform nil :accessor odbc-conn)
   (disconnect-fn :reader disconnect-fn)
   (sql-fn :reader sql-fn)
   (close-query-fn :reader close-query-fn)
   (fetch-row :reader fetch-row-fn)
   (list-all-database-tables-fn :reader list-all-database-tables-fn)
   (list-all-table-columns-fn :reader list-all-table-columns-fn))
  (:documentation "Encapsulate same behavior across odbc and aodbc backends."))

(defmethod initialize-instance :after ((db generic-odbc-database)
                                        &rest all-keys)
  (declare (ignore all-keys))
  (unless (slot-boundp db 'dbi-package)
    (error "dbi-package not specified."))
  (let ((pkg (slot-value db 'dbi-package)))
    (unless pkg
      (error "dbi-package is nil."))
    (setf (slot-value db 'disconnect-fn)
          (intern (symbol-name '#:disconnect) pkg)
          (slot-value db 'sql-fn)
          (intern (symbol-name '#:sql) pkg)
          (slot-value db 'close-query-fn)
          (intern (symbol-name '#:close-query) pkg)
          (slot-value db 'fetch-row)
          (intern (symbol-name '#:fetch-row) pkg)
          (slot-value db 'list-all-database-tables-fn)
          (intern (symbol-name '#:list-all-database-tables) pkg)
          (slot-value db 'list-all-table-columns-fn)
          (intern (symbol-name '#:list-all-table-columns) pkg))))

;;; Object methods

(defmethod read-sql-value (val (type (eql 'boolean))
                           (database generic-odbc-database)
                           (db-type (eql :postgresql)))
  (if (string= "0" val) nil t))

(defmethod read-sql-value (val (type (eql 'generalized-boolean))
                           (database generic-odbc-database)
                           (db-type (eql :postgresql)))
  (if (string= "0" val) nil t))

(defmethod read-sql-value (val (type (eql 'boolean)) database
                           (db-type (eql :mssql)))
  (declare (ignore database))
  (etypecase val
    (string (if (string= "0" val) nil t))
    (integer (if (zerop val) nil t))))

(defmethod read-sql-value (val (type (eql 'generalized-boolean)) database
                           (db-type (eql :mssql)))
  (declare (ignore database))
  (etypecase val
    (string (if (string= "0" val) nil t))
    (integer (if (zerop val) nil t))))

;;; Type methods

(defmethod database-get-type-specifier ((type (eql 'wall-time)) args database
                                        (db-type (eql :mssql)))
  (declare (ignore args database))
  "DATETIME")

(defmethod database-get-type-specifier ((type (eql 'boolean)) args database
                                        (db-type (eql :mssql)))
  (declare (ignore args database))
  "BIT")

(defmethod database-get-type-specifier ((type (eql 'generalized-boolean)) args database
                                        (db-type (eql :mssql)))
  (declare (ignore args database))
  "BIT")

;;; Generation of SQL strings from lisp expressions

(defmethod database-output-sql ((tee (eql t)) (database generic-odbc-database))
  (case (database-underlying-type database)
    (:mssql "1")
    (t "'Y'")))

(defmethod database-output-sql-as-type ((type (eql 'boolean)) val database
                                        (db-type (eql :mssql)))
  (declare (ignore database))
  (if val 1 0))

(defmethod database-output-sql-as-type ((type (eql 'generalized-boolean)) val database
                                        (db-type (eql :mssql)))
  (declare (ignore database))
  (if val 1 0))

;;; Database backend capabilities

(defmethod db-type-use-fully-qualified-column-on-drop-index? ((db-type (eql :mssql)))
  t)

(defmethod db-type-has-boolean-where? ((db-type (eql :mssql)))
  nil)

(defmethod db-type-has-intersect? ((db-type (eql :mssql)))
  nil)

(defmethod db-type-has-top-instead-of-limit? ((db-type (eql :mssql)))
  T)

(defmethod db-type-has-except? ((db-type (eql :mssql)))
  nil)

;;; Backend methods

(defmethod database-disconnect ((database generic-odbc-database))
  (funcall (disconnect-fn database) (odbc-conn database))
  (setf (odbc-conn database) nil)
  t)

(defmethod database-query (query-expression (database generic-odbc-database)
                           result-types field-names)
  (handler-case
      (funcall (sql-fn database)
               query-expression :db (odbc-conn database)
               :result-types result-types
               :column-names field-names)
    #+ignore
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :message "Query failed"))))


(defmethod database-execute-command (sql-expression (database generic-odbc-database))
  (handler-case
      (funcall (sql-fn database)
               sql-expression :db (odbc-conn database))
    #+ignore
    (sql-error (e)
      (error e))
    #+ignore
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression sql-expression
             :message "Execute command failed"))))


(defstruct odbc-result-set
  (query nil)
  (types nil)
  (full-set nil :type boolean))




(defmethod database-query-result-set ((query-expression string)
                                      (database generic-odbc-database)
                                      &key full-set result-types)
  (handler-case
      (multiple-value-bind (query column-names)
          (funcall (sql-fn database)
                   query-expression
                   :db (odbc-conn database)
                   :row-count nil
                   :column-names t
                   :query t
                   :result-types result-types)
        (values
         (make-odbc-result-set :query query :full-set full-set
                               :types result-types)
         (length column-names)
         nil ;; not able to return number of rows with odbc
         ))
    (error ()
      (error 'sql-database-data-error
             :database database
             :expression query-expression
             :message "Query result set failed"))))

(defmethod database-dump-result-set (result-set (database generic-odbc-database))
  (funcall (close-query-fn database) (odbc-result-set-query result-set))
  t)

(defmethod database-store-next-row (result-set
                                    (database generic-odbc-database)
                                    list)
  (let ((row (funcall (fetch-row-fn database)
                      (odbc-result-set-query result-set) nil 'eof)))
    (if (eq row 'eof)
        nil
      (progn
        (loop for elem in row
            for rest on list
            do
              (setf (car rest) elem))
        list))))

(defmethod database-list-tables ((database generic-odbc-database)
                                 &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (funcall (list-all-database-tables-fn database) :db (odbc-conn database))
    (declare (ignore col-names))
    ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
    ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
    (loop for row in rows
          when (and (not (string-equal "information_schema" (nth 1 row)))
                    (string-equal "TABLE" (nth 3 row))
                    (not (and (eq :mssql (database-underlying-type database))
                              (string-equal "dtproperties" (nth 2 row)))))
          collect (nth 2 row))))


(defmethod database-list-views ((database generic-odbc-database)
                                 &key (owner nil))
  (multiple-value-bind (rows col-names)
      (funcall (list-all-database-tables-fn database) :db (odbc-conn database))
    (declare (ignore col-names))
    ;; TABLE_SCHEM is hard-coded in second column by ODBC Driver Manager
    ;; TABLE_NAME in third column, TABLE_TYPE in fourth column
    (loop for (category view-owner name type . rest) in rows
          when (and (or (null owner) (string-equal owner view-owner) )
		    (not (string-equal "information_schema" view-owner))
                    (string-equal "VIEW" type)
                    (not (and (eq :mssql (database-underlying-type database))
                              (member name '("sysconstraints" "syssegments") :test #'string-equal))))
          collect name)))


(defmethod database-list-attributes ((table string) (database generic-odbc-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (funcall (list-all-table-columns-fn database) table
               :db (odbc-conn database))
    (declare (ignore col-names))
    ;; COLUMN_NAME is hard-coded by odbc spec as fourth position
    (loop for row in rows
        collect (fourth row))))

(defmethod database-attribute-type ((attribute string) (table string) (database generic-odbc-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (multiple-value-bind (rows col-names)
      (funcall (list-all-table-columns-fn database) table
               :db (odbc-conn database))
    (declare (ignore col-names))
    ;; COLUMN_NAME is hard-coded by odbc spec as fourth position
    ;; TYPE_NAME is the sixth column
    ;; PRECISION/COLUMN_SIZE is the seventh column
    ;; SCALE/DECIMAL_DIGITS is the ninth column
    ;; NULLABLE is the eleventh column
    (loop for row in rows
        when (string-equal attribute (fourth row))
        do
        (let ((size (seventh row))
              (precision (ninth row))
              (scale (nth 10 row)))
          (return (values (ensure-keyword (sixth row))
                          (when size (parse-integer size))
                          (when precision (parse-integer precision))
                          (when scale (parse-integer scale))))))))
