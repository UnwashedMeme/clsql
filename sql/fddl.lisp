;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; The CLSQL Functional Data Definition Language (FDDL)
;;;; including functions for schema manipulation. Currently supported
;;;; SQL objects include tables, views, indexes, attributes and
;;;; sequences.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


;; Truncate database

(defun truncate-database (&key (database *default-database*))
  "Drops all tables, views, indexes and sequences in DATABASE which
defaults to *DEFAULT-DATABASE*."
  (unless (typep database 'database)
    (signal-no-database-error database))
  (unless (is-database-open database)
    (database-reconnect database))
  (when (eq :oracle (database-type database))
    (ignore-errors (execute-command "PURGE RECYCLEBIN" :database database)))
  (when (db-type-has-views? (database-underlying-type database))
    (dolist (view (list-views :database database))
      (drop-view view :database database)))
  (dolist (table (list-tables :database database))
    (drop-table table :database database))
  (dolist (index (list-indexes :database database))
    (drop-index index :database database))
  (dolist (seq (list-sequences :database database))
    (drop-sequence seq :database database))
  (when (eq :oracle (database-type database))
    (ignore-errors (execute-command "PURGE RECYCLEBIN" :database database)))
  (values))


;; Tables

(defun create-table (name description &key (database *default-database*)
                          (constraints nil) (transactions t))
  "Creates a table called NAME, which may be a string, symbol or
SQL table identifier, in DATABASE which defaults to
*DEFAULT-DATABASE*. DESCRIPTION is a list whose elements are
lists containing the attribute names, types, and other
constraints such as not-null or primary-key for each column in
the table.  CONSTRAINTS is a string representing an SQL table
constraint expression or a list of such strings. With MySQL
databases, if TRANSACTIONS is t an InnoDB table is created which
supports transactions."
  (execute-command
   (make-instance 'sql-create-table
                  :name name
                  :columns description
                  :modifiers constraints
                  :transactions transactions)
   :database database))

(defun drop-table (name &key (if-does-not-exist :error)
                             (database *default-database*)
                             (owner nil))
  "Drops the table called NAME from DATABASE which defaults to
*DEFAULT-DATABASE*. If the table does not exist and
IF-DOES-NOT-EXIST is :ignore then DROP-TABLE returns nil whereas
an error is signalled if IF-DOES-NOT-EXIST is :error."
    (ecase if-does-not-exist
      (:ignore
       (unless (table-exists-p name :database database :owner owner)
         (return-from drop-table nil)))
      (:error
       t))
  
    (let ((expr (concatenate 'string "DROP TABLE " (escaped-database-identifier name database))))
      ;; Fixme: move to clsql-oracle
      (when (and (find-package 'clsql-oracle)
                 (eq :oracle (database-type database))
                 (eql 10 (slot-value database
                                     (intern (symbol-name '#:major-server-version)
                                             (symbol-name '#:clsql-oracle)))))
        (setq expr (concatenate 'string expr " PURGE")))

      (execute-command expr :database database)))

(defun list-tables (&key (owner nil) (database *default-database*))
  "Returns a list of strings representing table names in DATABASE
which defaults to *DEFAULT-DATABASE*. OWNER is nil by default
which means that only tables owned by users are listed. If OWNER
is a string denoting a user name, only tables owned by OWNER are
listed. If OWNER is :all then all tables are listed."
  (database-list-tables database :owner owner))

(defmethod %table-exists-p (name (database T) &key owner )
  (unless database (setf database *default-database*))
  (let ((name (database-identifier name database))
        (tables (list-tables :owner owner :database database)))
    (when (member name tables :test #'database-identifier-equal)
      t)))

(defun table-exists-p (name &key (owner nil) (database *default-database*))
  "Tests for the existence of an SQL table called NAME in DATABASE
which defaults to *DEFAULT-DATABASE*. OWNER is nil by default
which means that only tables owned by users are examined. If
OWNER is a string denoting a user name, only tables owned by
OWNER are examined. If OWNER is :all then all tables are
examined."
  (%table-exists-p name database :owner owner))


;; Views

(defun create-view (name &key as column-list (with-check-option nil)
                         (database *default-database*))
  "Creates a view called NAME in DATABASE which defaults to
*DEFAULT-DATABASE*. The view is created using the query AS and
the columns of the view may be specified using the COLUMN-LIST
parameter. The WITH-CHECK-OPTION is nil by default but if it has
a non-nil value, then all insert/update commands on the view are
checked to ensure that the new data satisfy the query AS."
  (let* ((view-name (database-identifier name))
         (stmt (make-instance 'sql-create-view
                              :name view-name
                              :column-list column-list
                              :query as
                              :with-check-option with-check-option)))
    (execute-command stmt :database database)))

(defun drop-view (name &key (if-does-not-exist :error)
                       (database *default-database*))
  "Drops the view called NAME from DATABASE which defaults to
*DEFAULT-DATABASE*. If the view does not exist and
IF-DOES-NOT-EXIST is :ignore then DROP-VIEW returns nil whereas
an error is signalled if IF-DOES-NOT-EXIST is :error."
    (ecase if-does-not-exist
      (:ignore
       (unless (view-exists-p name :database database)
         (return-from drop-view)))
      (:error
       t))
    (let ((expr (concatenate 'string "DROP VIEW " (escaped-database-identifier name database))))
      (execute-command expr :database database)))

(defun list-views (&key (owner nil) (database *default-database*))
  "Returns a list of strings representing view names in DATABASE
which defaults to *DEFAULT-DATABASE*. OWNER is nil by default
which means that only views owned by users are listed. If OWNER
is a string denoting a user name, only views owned by OWNER are
listed. If OWNER is :all then all views are listed."
  (database-list-views database :owner owner))

(defun view-exists-p (name &key (owner nil) (database *default-database*))
  "Tests for the existence of an SQL view called NAME in DATABASE
which defaults to *DEFAULT-DATABASE*. OWNER is nil by default
which means that only views owned by users are examined. If OWNER
is a string denoting a user name, only views owned by OWNER are
examined. If OWNER is :all then all views are examined."
  (when (member (database-identifier name database)
                (list-views :owner owner :database database)
                :test #'database-identifier-equal)
    t))


;; Indexes

(defun create-index (name &key on (unique nil) attributes
                          (database *default-database*))
  "Creates an index called NAME on the table specified by ON in
DATABASE which default to *DEFAULT-DATABASE*. The table
attributes to use in constructing the index NAME are specified by
ATTRIBUTES. The UNIQUE argument is nil by default but if it has a
non-nil value then the indexed attributes must have unique
values."
  (let* ((index-name (escaped-database-identifier name database))
         (table-name (escaped-database-identifier on database))
         (attributes (mapcar #'(lambda (a) (escaped-database-identifier a database))
                             (listify attributes)))
         (stmt (format nil "CREATE ~A INDEX ~A ON ~A (~{~A~^, ~})"
                       (if unique "UNIQUE" "")
                       index-name table-name attributes)))
    (execute-command stmt :database database)))

(defun drop-index (name &key (if-does-not-exist :error)
                        (on nil)
                        (database *default-database*))
  "Drops the index called NAME in DATABASE which defaults to
*DEFAULT-DATABASE*. If the index does not exist and
IF-DOES-NOT-EXIST is :ignore then DROP-INDEX returns nil whereas
an error is signalled if IF-DOES-NOT-EXIST is :error. The
argument ON allows the optional specification of a table to drop
the index from."
  (ecase if-does-not-exist
    (:ignore
     (unless (index-exists-p name :database database)
       (return-from drop-index)))
    (:error t))
  (let* ((db-type (database-underlying-type database))
         (on (when on (escaped-database-identifier on database)))
         (index-name (escaped-database-identifier name database))
         (index-identifier
           (cond ((db-type-use-fully-qualified-column-on-drop-index? db-type)
                  (format nil "~A.~A"  on index-name))
                 ((db-type-use-column-on-drop-index? db-type)
                  (format nil "~A ON ~A" index-name on))
                 (t index-name))))
    (execute-command (format nil "DROP INDEX ~A" index-identifier)
                     :database database)))

(defun list-indexes (&key (owner nil) (database *default-database*) (on nil))
  "Returns a list of strings representing index names in DATABASE
which defaults to *DEFAULT-DATABASE*. OWNER is nil by default
which means that only indexes owned by users are listed. If OWNER
is a string denoting a user name, only indexes owned by OWNER are
listed. If OWNER is :all then all indexes are listed. The keyword
argument ON limits the results to indexes on the specified
tables. Meaningful values for ON are nil (the default) which
means that all tables are considered, a string, symbol or SQL
expression representing a table name in DATABASE or a list of
such table identifiers."
  (if (null on)
      (database-list-indexes database :owner owner)
      (let ((tables (typecase on
                      (cons on)
                      (t (list on)))))
        (reduce
         #'append
         (mapcar #'(lambda (table)
                     (database-list-table-indexes table database :owner owner))
                 tables)))))

(defun index-exists-p (name &key (owner nil) (database *default-database*))
  "Tests for the existence of an SQL index called NAME in DATABASE
which defaults to *DEFAULT-DATABASE*. OWNER is nil by default
which means that only indexes owned by users are examined. If
OWNER is a string denoting a user name, only indexes owned by
OWNER are examined. If OWNER is :all then all indexes are
examined."
  (when (member (database-identifier name database)
                (list-indexes :owner owner :database database)
                :test #'database-identifier-equal)
    t))

;; Attributes

(defvar *cache-table-queries-default* nil
  "Specifies the default behaivour for caching of attribute
  types. Meaningful values are t, nil and :flush as described for
  the action argument to CACHE-TABLE-QUERIES.")

(defun cache-table-queries (table &key (action nil) (database *default-database*))
  "Controls the caching of attribute type information on the
table specified by TABLE in DATABASE which defaults to
*DEFAULT-DATABASE*. ACTION specifies the caching behaviour to
adopt. If its value is t then attribute type information is
cached whereas if its value is nil then attribute type
information is not cached. If ACTION is :flush then all existing
type information in the cache for TABLE is removed, but caching
is still enabled. TABLE may be a string representing a table for
which the caching action is to be taken while the caching action
is applied to all tables if TABLE is t. Alternativly, when TABLE
is :default, the default caching action specified by
*CACHE-TABLE-QUERIES-DEFAULT* is applied to all table for which a
caching action has not been explicitly set."
  (with-slots (attribute-cache) database
    (cond
      ((stringp table)
       (multiple-value-bind (val found) (gethash table attribute-cache)
         (cond
           ((and found (eq action :flush))
            (setf (gethash table attribute-cache) (list t nil)))
           ((and found (eq action t))
            (setf (gethash table attribute-cache) (list t (second val))))
           ((and found (null action))
            (setf (gethash table attribute-cache) (list nil nil)))
           ((not found)
            (setf (gethash table attribute-cache) (list action nil))))))
      ((eq table t)
       (maphash (lambda (k v)
                  (cond
                    ((eq action :flush)
                     (setf (gethash k attribute-cache) (list t nil)))
                    ((null action)
                     (setf (gethash k attribute-cache) (list nil nil)))
                    ((eq t action)
                     (setf (gethash k attribute-cache) (list t (second v))))))
                attribute-cache))
      ((eq table :default)
       (maphash (lambda (k v)
                  (when (eq (first v) :unspecified)
                    (cond
                      ((eq action :flush)
                       (setf (gethash k attribute-cache) (list t nil)))
                      ((null action)
                       (setf (gethash k attribute-cache) (list nil nil)))
                      ((eq t action)
                       (setf (gethash k attribute-cache) (list t (second v)))))))
                attribute-cache))))
  (values))


(defun list-attributes (name &key (owner nil) (database *default-database*))
  "Returns a list of strings representing the attributes of table
NAME in DATABASE which defaults to *DEFAULT-DATABASE*. OWNER is
nil by default which means that only attributes owned by users
are listed. If OWNER is a string denoting a user name, only
attributes owned by OWNER are listed. If OWNER is :all then all
attributes are listed."
  (database-list-attributes (escaped-database-identifier name database) database
                            :owner owner))

(defun attribute-type (attribute table &key (owner nil)
                                 (database *default-database*))
  "Returns a keyword representing the vendor-specific field type
of the supplied attribute ATTRIBUTE in the table specified by
TABLE in DATABASE which defaults to *DEFAULT-DATABASE*. OWNER is
nil by default which means that the attribute specified by
ATTRIBUTE, if it exists, must be user owned else nil is
returned. If OWNER is a string denoting a user name, the
attribute, if it exists, must be owned by OWNER else nil is
returned, whereas if OWNER is :all then the attribute, if it
exists, will be returned regardless of its owner."
  (database-attribute-type (escaped-database-identifier attribute database)
                           (escaped-database-identifier table database)
                           database
                           :owner owner))

(defun list-attribute-types (table &key (owner nil)
                                   (database *default-database*))
  "Returns a list containing information about the SQL types of
each of the attributes in the table specified by TABLE in
DATABASE which has a default value of *DEFAULT-DATABASE*. OWNER
is nil by default which means that only attributes owned by users
are listed. If OWNER is a string denoting a user name, only
attributes owned by OWNER are listed. If OWNER is :all then all
attributes are listed. The elements of the returned list are
lists where the first element is the name of the attribute, the
second element is its SQL type, the third is the type precision,
the fourth is the scale of the attribute and the fifth is 1 if
the attribute accepts null values and otherwise 0."
  (with-slots (attribute-cache) database
    (let ((table-ident (escaped-database-identifier table database)))
      (multiple-value-bind (val found) (gethash table-ident attribute-cache)
        (if (and found (second val))
            (second val)
            (let ((types (mapcar #'(lambda (attribute)
                                     (cons attribute
                                           (multiple-value-list
                                            (database-attribute-type
                                             (escaped-database-identifier attribute
                                                                  database)
                                             table-ident
                                             database
                                             :owner owner))))
                                 (list-attributes table :database database
                                                  :owner owner))))
              (cond
                ((and (not found) (eq t *cache-table-queries-default*))
                 (setf (gethash table-ident attribute-cache)
                       (list :unspecified types)))
                ((and found (eq t (first val))
                      (setf (gethash table-ident attribute-cache)
                            (list t types)))))
              types))))))


;; Sequences

(defun create-sequence (name &key (database *default-database*))
  "Creates a sequence called NAME in DATABASE which defaults to
*DEFAULT-DATABASE*."
  (let ((sequence-name (database-identifier name database)))
    (database-create-sequence sequence-name database))
  (values))

(defun drop-sequence (name &key (if-does-not-exist :error)
                           (database *default-database*))
  "Drops the sequence called NAME from DATABASE which defaults to
*DEFAULT-DATABASE*. If the sequence does not exist and
IF-DOES-NOT-EXIST is :ignore then DROP-SEQUENCE returns nil
whereas an error is signalled if IF-DOES-NOT-EXIST is :error."
  (ecase if-does-not-exist
    (:ignore
     (unless (sequence-exists-p name :database database)
       (return-from drop-sequence)))
    (:error t))
  (database-drop-sequence name database)
  (values))

(defun list-sequences (&key (owner nil) (database *default-database*))
  "Returns a list of strings representing sequence names in
DATABASE which defaults to *DEFAULT-DATABASE*. OWNER is nil by
default which means that only sequences owned by users are
listed. If OWNER is a string denoting a user name, only sequences
owned by OWNER are listed. If OWNER is :all then all sequences
are listed."
  (database-list-sequences database :owner owner))

(defun sequence-exists-p (name &key (owner nil)
                               (database *default-database*))
  "Tests for the existence of an SQL sequence called NAME in
DATABASE which defaults to *DEFAULT-DATABASE*. OWNER is nil by
default which means that only sequences owned by users are
examined. If OWNER is a string denoting a user name, only
sequences owned by OWNER are examined. If OWNER is :all then all
sequences are examined."
  (let ((seqs (list-sequences :owner owner :database database))
        ;; handle symbols, we know the db will return strings
        (n1 (database-identifier name database))
        (n2 (%sequence-name-to-table name database)))
    (when (or (member n1 seqs :test #'database-identifier-equal)
              (member n2 seqs :test #'database-identifier-equal))
      t)))

(defun sequence-next (name &key (database *default-database*))
  "Increment and return the next value in the sequence called
  NAME in DATABASE which defaults to *DEFAULT-DATABASE*."
  (database-sequence-next (database-identifier name database) database))

(defun set-sequence-position (name position &key (database *default-database*))
  "Explicitly set the the position of the sequence called NAME in
DATABASE, which defaults to *DEFAULT-DATABASE*, to POSITION which
is returned."
  (database-set-sequence-position (database-identifier name database)
                                  position database))

(defun sequence-last (name &key (database *default-database*))
  "Return the last value allocated in the sequence called NAME in DATABASE
  which defaults to *DEFAULT-DATABASE*."
  (database-sequence-last (database-identifier name database) database))

