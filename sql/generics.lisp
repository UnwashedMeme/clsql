;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     generics.lisp
;;;; Purpose:  Generic function definitions for DB interfaces
;;;; Author:   Kevin M. Rosenberg
;;;; Created:  Apr 2004
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


;; FDML
(defgeneric choose-database-for-instance (object &optional database)
  (:documentation "Used by the oodml functions to select which
 database object to use. Chooses the database associated with the
 object primarily, falls back to the database provided as an argument
 or the *DEFAULT-DATABASE*."))

(defgeneric execute-command (expression &key database)
  (:documentation
   "Executes the SQL command EXPRESSION, which may be an SQL
expression or a string representing any SQL statement apart from
a query, on the supplied DATABASE which defaults to
*DEFAULT-DATABASE*."))


(defgeneric query (query-expression &key database result-types flatp field-names)
  (:documentation
   "Executes the SQL query expression QUERY-EXPRESSION, which may
be an SQL expression or a string, on the supplied DATABASE which
defaults to *DEFAULT-DATABASE*. RESULT-TYPES is a list of symbols
which specifies the lisp type for each field returned by
QUERY-EXPRESSION. If RESULT-TYPES is nil all results are returned
as strings whereas the default value of :auto means that the lisp
types are automatically computed for each field. FIELD-NAMES is t
by default which means that the second value returned is a list
of strings representing the columns selected by
QUERY-EXPRESSION. If FIELD-NAMES is nil, the list of column names
is not returned as a second value. FLATP has a default value of
nil which means that the results are returned as a list of
lists. If FLATP is t and only one result is returned for each
record selected by QUERY-EXPRESSION, the results are returned as
elements of a list."))


;; OODML

(defgeneric update-record-from-slot (object slot &key database)
  (:documentation
   "Updates the value stored in the column represented by the
slot, specified by the CLOS slot name SLOT, of View Class
instance OBJECT. DATABASE defaults to *DEFAULT-DATABASE* and
specifies the database in which the update is made only if OBJECT
is not associated with a database. In this case, a record is
created in DATABASE and the attribute represented by SLOT is
initialised from the value of the supplied slots with other
attributes having default values. Furthermore, OBJECT becomes
associated with DATABASE."))

(defgeneric update-record-from-slots (object slots &key database)
  (:documentation
   "Updates the values stored in the columns represented by the
slots, specified by the CLOS slot names SLOTS, of View Class
instance OBJECT. DATABASE defaults to *DEFAULT-DATABASE* and
specifies the database in which the update is made only if OBJECT
is not associated with a database. In this case, a record is
created in the appropriate table of DATABASE and the attributes
represented by SLOTS are initialised from the values of the
supplied slots with other attributes having default
values. Furthermore, OBJECT becomes associated with DATABASE."))

(defgeneric update-records-from-instance (object &key database this-class)
  (:documentation
   "Using an instance of a View Class, OBJECT, update the table
that stores its instance data. DATABASE defaults to
*DEFAULT-DATABASE* and specifies the database in which the update
is made only if OBJECT is not associated with a database. In this
case, a record is created in the appropriate table of DATABASE
using values from the slot values of OBJECT, and OBJECT becomes
associated with DATABASE."))

(defgeneric delete-instance-records (object &key database)
  (:documentation
   "Deletes the records represented by OBJECT in the appropriate
table of the database associated with OBJECT. If OBJECT is not
yet associated with a database, an error is signalled."))

(defgeneric update-instance-from-records (object &key database this-class)
  (:documentation
   "Updates the slot values of the View Class instance OBJECT
using the attribute values of the appropriate table of DATABASE
which defaults to the database associated with OBJECT or, if
OBJECT is not associated with a database, *DEFAULT-DATABASE*.
Join slots are updated but instances of the class on which the
join is made are not updated."))

(defgeneric update-slot-from-record (object slot &key database)
  (:documentation
   "Updates the slot value, specified by the CLOS slot name SLOT,
of the View Class instance OBJECT using the attribute values of
the appropriate table of DATABASE which defaults to the database
associated with OBJECT or, if OBJECT is not associated with a
database, *DEFAULT-DATABASE*.  Join slots are updated but
instances of the class on which the join is made are not
updated."))

(defgeneric instance-refreshed (object)
  (:documentation
   "Provides a hook which is called within an object oriented
call to SELECT with a non-nil value of REFRESH when the View
Class instance OBJECT has been updated from the database. A
method specialised on STANDARD-DB-OBJECT is provided which has no
effects. Methods specialised on particular View Classes can be
used to specify any operations that need to be made on View
Classes instances which have been updated in calls to SELECT."))

(defgeneric update-slot-with-null (instance slotname slotdef)
  (:documentation "Called to update a slot when its column has a NULL
value.  If nulls are allowed for the column, the slot's value will be
nil, otherwise its value will be set to the result of calling
DATABASE-NULL-VALUE on the type of the slot."))

(defgeneric database-pkey-constraint  (class database)
  )
(defgeneric %install-class  (class database &key transactions)
  )
(defgeneric database-generate-column-definition  (class slotdef database)
  )
(defgeneric update-slot-from-db  (instance slotdef val)
  )
(defgeneric key-value-from-db  (slotdef value database)
  )
(defgeneric get-slot-values-from-view  (obj slotdeflist values)
  )
(defgeneric database-output-sql-as-type  (type val database db-type)
  )
(defgeneric read-sql-value  (val type database db-type)
  )
(defgeneric database-add-autoincrement-sequence (class database)
  (:method (class database) nil)
  (:documentation "If a database needs to add a sequence for its
    autoincrement to work, this is where it should go.  Default is
    that it doesnt so just return nil"))
(defgeneric database-remove-autoincrement-sequence (class database)
  (:method (class database) nil)
  (:documentation "If a database needs to add a sequence for its
    autoincrement to work, this is where it should go.  Default is
    that it doesnt so just return nil"))
(defgeneric auto-increment-sequence-name (class slotdef database)
  (:documentation "The sequence name to create for this autoincremnt column on this class
   if returns nil, there is no associated sequence "))

(defmethod auto-increment-sequence-name :around (class slot database)
  (when (auto-increment-column-p slot database)
    (call-next-method)))

(defgeneric database-last-auto-increment-id (database table column)
  )



;; Generation of SQL strings from lisp expressions

(defgeneric output-sql (expr database)
  (:documentation "Writes an SQL string appropriate for DATABASE
  and corresponding to the lisp expression EXPR to
  *SQL-STREAM*. The function SQL-OUTPUT is a top-level call for
  generating SQL strings which initialises *SQL-STREAM*, calls
  OUTPUT-SQL and reads the generated SQL string from
  *SQL-STREAM*."))

(defgeneric database-output-sql (expr database)
  (:documentation "Returns an SQL string appropriate for DATABASE
  and corresponding to the lisp expression
  EXPR. DATABASE-OUTPUT-SQL is called by OUTPUT-SQL when no more
  specific method exists for EXPR."))

(defgeneric output-sql-hash-key (expr database)
  (:documentation "Returns a list (or other object suitable for
use as the key of an EQUAL hash table) which uniquely identifies
the arguments EXPR and DATABASE."))

(defgeneric collect-table-refs (sql)
  )

(defgeneric database-constraint-statement  (constraints database)
  )
