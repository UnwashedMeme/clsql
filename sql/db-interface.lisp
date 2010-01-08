;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db-interface.lisp
;;;; Purpose:       Generic function definitions for DB interfaces
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai. Additions from
;;;;                onShoreD to support UncommonSQL front-end
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai, and onShoreD
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defgeneric database-type-load-foreign (database-type)
  (:documentation
   "The internal generic implementation of reload-database-types."))

(defgeneric database-type-library-loaded (database-type)
  (:documentation
   "The internal generic implementation for checking if
database type library loaded successfully."))

(defgeneric database-initialize-database-type (database-type)
  (:documentation
   "The internal generic implementation of initialize-database-type."))

(defgeneric database-name-from-spec (connection-spec database-type)
  (:documentation
   "Returns the name of the database that would be created if connect
was called with the connection-spec."))

(defgeneric database-connect (connection-spec database-type)
  (:documentation "Internal generic implementation of connect."))

(defgeneric database-reconnect (database)
  (:method ((database t))
           (signal-no-database-error database))
  (:documentation "Internal generic implementation of reconnect."))

(defgeneric database-disconnect (database)
  (:method ((database t))
           (signal-no-database-error database))
  (:documentation "Internal generic implementation of disconnect."))

(defgeneric database-query (query-expression database result-types field-names)
  (:method (query-expression (database t) result-types field-names)
           (declare (ignore query-expression result-types field-names))
           (signal-no-database-error database))
  (:method (query-expression (database database) result-types field-names)
             (declare (ignore query-expression result-types field-names))
             (warn "database-query not implemented for database type ~A."
                   (database-type database)))
  (:documentation "Internal generic implementation of query."))


(defgeneric database-execute-command (sql-expression database)
  (:method (sql-expression (database t))
           (declare (ignore sql-expression))
           (signal-no-database-error database))
  (:method (sql-expression (database database))
           (declare (ignore sql-expression))
           (warn "database-execute-command not implemented for database type ~A."
                 (database-type database)))
  (:documentation "Internal generic implementation of execute-command."))

;;; Mapping and iteration
(defgeneric database-query-result-set
    (query-expression database &key full-set result-types)
  (:method (query-expression (database t) &key full-set result-types)
           (declare (ignore query-expression full-set result-types))
           (signal-no-database-error database)
           (values nil nil nil))
  (:method (query-expression (database database) &key full-set result-types)
           (declare (ignore query-expression full-set result-types))
           (warn "database-query-result-set not implemented for database type ~A."
                 (database-type database))
           (values nil nil nil))
  (:documentation
   "Internal generic implementation of query mapping.  Starts the
query specified by query-expression on the given database and returns
a result-set to be used with database-store-next-row and
database-dump-result-set to access the returned data.  The second
value is the number of columns in the result-set, if there are any.
If full-set is true, the number of rows in the result-set is returned
as a third value, if this is possible (otherwise nil is returned for
the third value).  This might have memory and resource usage
implications, since many databases will require the query to be
executed in full to answer this question.  If the query produced no
results then nil is returned for all values that would have been
returned otherwise.  If an error occurs during query execution, the
function should signal a sql-database-data-error."))

(defgeneric database-dump-result-set (result-set database)
  (:method (result-set (database t))
           (declare (ignore result-set))
           (signal-no-database-error database))
    (:method (result-set (database database))
           (declare (ignore result-set))
           (warn "database-dump-result-set not implemented for database type ~A."
                 (database-type database)))
  (:documentation "Dumps the received result-set."))

(defgeneric database-store-next-row (result-set database list)
  (:method (result-set (database t) list)
           (declare (ignore result-set list))
           (signal-no-database-error database))
    (:method (result-set (database database) list)
           (declare (ignore result-set list))
           (warn "database-store-next-row not implemented for database type ~A."
                 (database-type database)))
  (:documentation
   "Returns t and stores the next row in the result set in list or
returns nil when result-set is finished."))

(defgeneric database-create (connection-spec type)
  (:documentation
   "Creates a database, returns T if successfull or signals an error."))

(defgeneric database-probe (connection-spec type)
  (:method (spec type)
    (declare (ignore spec))
    (warn "database-probe not support for database-type ~A." type))
  (:documentation
   "Probes for the existence of a database, returns T if database found or NIL
if not found. May signal an error if unable to communicate with database server."))

(defgeneric database-list (connection-spec type)
  (:method (spec type)
    (declare (ignore spec))
    (warn "database-list not support for database-type ~A." type))
  (:documentation
   "Lists all databases found for TYPE. May signal an error if unable to communicate with database server."))

(defgeneric database-truncate (database)
  (:method ((database t))
    (signal-no-database-error database))
  (:documentation "Remove all data from database."))

(defgeneric database-destroy (connection-spec type)
  (:documentation
   "Destroys (drops) a database, returns T if successfull or signals an error
if unable to destory."))

(defgeneric database-create-sequence (name database)
  (:documentation "Create a sequence in DATABASE."))

(defgeneric database-drop-sequence (name database)
  (:documentation "Drop a sequence from DATABASE."))

(defgeneric database-sequence-next (name database)
  (:documentation "Increment a sequence in DATABASE."))

(defgeneric database-list-sequences (database &key owner)
  (:documentation "List all sequences in DATABASE."))

(defgeneric database-set-sequence-position (name position database)
  (:documentation "Set the position of the sequence called NAME in DATABASE."))

(defgeneric database-sequence-last (name database)
  (:documentation "Select the last value in sequence NAME in DATABASE."))

(defgeneric database-start-transaction (database)
  (:documentation "Start a transaction in DATABASE.")
  (:method ((database t))
           (signal-no-database-error database)))

(defgeneric database-commit-transaction (database)
  (:documentation "Commit current transaction in DATABASE.")
  (:method ((database t))
           (signal-no-database-error database)))

(defgeneric database-abort-transaction (database)
  (:documentation "Abort current transaction in DATABASE.")
  (:method ((database t))
           (signal-no-database-error database)))

(defgeneric database-get-type-specifier (type args database db-underlying-type)
  (:documentation "Return the type SQL type specifier as a string, for
the given lisp type and parameters."))

(defgeneric database-list-tables (database &key owner)
  (:documentation "List all tables in the given database")
  (:method ((database database) &key owner)
           (declare (ignore owner))
           (warn "database-list-tables not implemented for database type ~A."
                 (database-type database)))
  (:method ((database t) &key owner)
           (declare (ignore owner))
           (signal-no-database-error database)))

(defgeneric database-list-tables-and-sequences (database &key owner)
  (:documentation "List all tables in the given database, may include seqeneces")
  (:method ((database t) &key owner)
           (declare (ignore owner))
           (signal-no-database-error database))
  (:method ((database database) &key owner)
           (database-list-tables database :owner owner)))

(defgeneric database-list-views (database &key owner)
  (:documentation "List all views in the DATABASE.")
  (:method ((database database) &key owner)
           (declare (ignore owner))
           (warn "database-list-views not implemented for database type ~A."
                 (database-type database)))
  (:method ((database t) &key owner)
           (declare (ignore owner))
           (signal-no-database-error database)))

(defgeneric database-list-indexes (database &key owner)
  (:documentation "List all indexes in the DATABASE.")
  (:method ((database database) &key owner)
           (declare (ignore owner))
           (warn "database-list-indexes not implemented for database type ~A."
                 (database-type database)))
  (:method ((database t) &key owner)
    (declare (ignore owner))
    (signal-no-database-error database)))

(defgeneric database-list-table-indexes (table database &key owner)
  (:documentation "List all indexes for a table in the DATABASE.")
  (:method (table (database database) &key owner)
           (declare (ignore table owner))
           (warn "database-list-table-indexes not implemented for database type ~A."
                 (database-type database)))
  (:method (table (database t) &key owner)
           (declare (ignore table owner))
           (signal-no-database-error database)))

(defgeneric database-list-attributes (table database &key owner)
  (:documentation "List all attributes in TABLE.")
  (:method (table (database database) &key owner)
           (declare (ignore table owner))
           (warn "database-list-attributes not implemented for database type ~A."
                 (database-type database)))
  (:method (table (database t) &key owner)
           (declare (ignore table owner))
           (signal-no-database-error database)))

(defgeneric database-attribute-type (attribute table database &key owner)
  (:documentation "Return the type of ATTRIBUTE in TABLE. Returns multiple values
of TYPE_NAME (keyword) PRECISION SCALE NULLABLE.")
  (:method (attribute table (database database) &key owner)
           (declare (ignore attribute table owner))
           (warn "database-list-attribute-type not implemented for database type ~A."
                 (database-type database)))
  (:method (attribute table (database t) &key owner)
           (declare (ignore attribute table owner))
           (signal-no-database-error database)))

(defgeneric database-add-attribute (table attribute database)
  (:documentation "Add the attribute to the table.")
  (:method (table attribute (database database))
           (declare (ignore table attribute))
           (warn "database-add-attribute not implemented for database type ~A."
                 (database-type database)))
  (:method (table attribute (database t))
           (declare (ignore table attribute))
           (signal-no-database-error database)))

(defgeneric database-rename-attribute (table oldatt newname database)
  (:documentation "Rename the attribute in the table to NEWNAME.")
  (:method (table oldatt newname (database database))
           (declare (ignore table oldatt newname))
           (warn "database-rename-attribute not implemented for database type ~A."
                 (database-type database)))
  (:method (table oldatt newname (database t))
           (declare (ignore table oldatt newname))
           (signal-no-database-error database)))

(defgeneric oid (object)
  (:documentation "Return the unique ID of a database object."))

;;; Database backend capabilities

(defgeneric database-underlying-type (database)
  (:method (database)
    (database-type database))
  (:documentation "Returns the type of the underlying database. For ODBC, needs to query ODBC driver."))

(defgeneric db-type-use-column-on-drop-index? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           nil)
  (:documentation "NIL [default] if database-type does not use column name on DROP INDEX."))

(defgeneric db-type-use-fully-qualified-column-on-drop-index? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           nil)
  (:documentation "NIL [default] if database-type does not require fully qualified column name on DROP INDEX."))

(defgeneric db-type-has-views? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           ;; SQL92 has views
           t)
  (:documentation "T [default] if database-type supports views."))

(defgeneric db-type-has-bigint? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           ;; SQL92 has bigint
           t)
  (:documentation "T [default] if database-type supports bigint."))

(defgeneric db-type-default-case (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           ;; By default, CommonSQL converts identifiers to UPPER case.
           :upper)
  (:documentation ":upper [default] if means identifiers mapped to UPPER case SQL like CommonSQL API. However, Postgresql maps identifiers to lower case, so PostgreSQL uses a value of :lower for this result."))

(defgeneric db-type-has-fancy-math? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           nil)
  (:documentation "NIL [default] if database-type does not have fancy math."))

(defgeneric db-type-has-subqueries? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           t)
  (:documentation "T [default] if database-type supports views."))

(defgeneric db-type-has-boolean-where? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           ;; SQL99 has boolean where
           t)
  (:documentation "T [default] if database-type supports boolean WHERE clause, such as 'WHERE MARRIED'."))

(defgeneric db-type-has-union? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           t)
  (:documentation "T [default] if database-type supports boolean UNION."))

(defgeneric db-backend-has-create/destroy-db? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           t)
  (:documentation "T [default] if backend can destroy and create databases."))

(defgeneric db-type-transaction-capable? (db database)
  (:method (db database)
           (declare (ignore db database))
           t)
  (:documentation "T [default] if database can supports transactions."))

(defgeneric db-type-has-prepared-stmt? (db-type)
  (:method ((db-type t))
    nil)
  (:documentation "T if database backend supports prepared statements."))

(defgeneric db-type-has-intersect? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           t)
  (:documentation "T [default] if database-type supports INTERSECT."))

(defgeneric db-type-has-except? (db-type)
  (:method (db-type)
           (declare (ignore db-type))
           t)
  (:documentation "T [default] if database-type supports EXCEPT."))

;;; Large objects support (Marc Battyani)

(defgeneric database-create-large-object (database)
  (:documentation "Creates a new large object in the database and returns the object identifier"))

(defgeneric database-write-large-object (object-id data database)
  (:documentation "Writes data to the large object"))

(defgeneric database-read-large-object (object-id database)
  (:documentation "Reads the large object content"))

(defgeneric database-delete-large-object (object-id database)
  (:documentation "Deletes the large object in the database"))

;; Prepared statements

(defgeneric database-prepare (stmt types database result-types field-names)
  (:method (stmt types (database t) result-types field-names)
    (declare (ignore stmt types result-types field-names))
    (signal-no-database-error database))
  (:method (stmt types (database database) result-types field-names)
    (declare (ignore stmt types result-types field-names))
    (error 'sql-database-error
           :message
           (format nil "DATABASE-PREPARE not implemented for ~S" database)))
  (:documentation "Prepare a statement for later execution."))

(defgeneric database-bind-parameter (prepared-stmt position value)
  (:method ((pstmt t) position value)
    (declare (ignore position value))
    (error 'sql-database-error
           :message
           (format nil "database-bind-paremeter not implemented for ~S" pstmt)))
  (:documentation "Bind a parameter for a prepared statement."))

(defgeneric database-run-prepared (prepared-stmt)
  (:method ((pstmt t))
    (error 'sql-database-error
           :message (format nil "database-run-prepared not specialized for ~S" pstmt)))
  (:documentation "Execute a prepared statement."))

(defgeneric database-free-prepared (prepared-stmt)
  (:method ((pstmt t))
    ;; nothing to do by default
    nil)
  (:documentation "Free the resources of a prepared statement."))

;; Checks for closed database

(defmethod database-disconnect :before ((database database))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-query :before (query-expression (database database)
                                   result-set field-names)
  (declare (ignore query-expression result-set field-names))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-execute-command :before (sql-expression (database database))
  (declare (ignore sql-expression))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-query-result-set :before (expr (database database)
                                            &key full-set result-types)
  (declare (ignore expr full-set result-types))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-dump-result-set :before (result-set (database database))
  (declare (ignore result-set))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-store-next-row :before (result-set (database database) list)
  (declare (ignore result-set list))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-commit-transaction :before ((database database))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-start-transaction :before ((database database))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defmethod database-abort-transaction :before ((database database))
  (unless (is-database-open database)
    (signal-closed-database-error database)))

(defvar *foreign-library-search-paths* nil
  "A list of pathnames denoting directories where CLSQL will look
for foreign libraries \(in addition to the default places).")

(defun push-library-path (path)
  "Adds the pathspec PATH \(which should denote a directory) to
the list *FOREIGN-LIBRARY-SEARCH-PATHS*."
  (pushnew path *foreign-library-search-paths* :test #'equal))
