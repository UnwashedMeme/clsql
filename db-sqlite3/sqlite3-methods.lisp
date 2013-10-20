;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:clsql-sys)

;; This method generates primary key constraints part of the table
;; definition. For Sqlite autoincrement primary keys to work properly
;; this part of the table definition must be left out.
(defmethod database-pkey-constraint ((class standard-db-class)
				     (database clsql-sqlite3:sqlite3-database)))

(defmethod database-translate-constraint (constraint
					  (database clsql-sqlite3:sqlite3-database))
  ;; Primary purpose of this is method is to intecept and translate
  ;; auto-increment primary keys constraints.
  (let ((constraint-name (symbol-name constraint)))
    (if (eql constraint :auto-increment)
	(cons constraint "PRIMARY KEY AUTOINCREMENT")
	(call-next-method))))

;; EOF
