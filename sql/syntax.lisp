;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; CLSQL square bracket symbolic query syntax. Functions for
;;;; enabling and disabling the syntax and for building SQL
;;;; expressions using the syntax.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar *original-readtable* nil)

(defvar *sql-macro-open-char* #\[)

(defvar *sql-macro-close-char* #\])

(defvar *restore-sql-reader-syntax* nil)


;; Exported functions for disabling SQL syntax.

(defmacro disable-sql-reader-syntax ()
  "Turns off the SQL reader syntax setting the syntax state such
that if the syntax is subsequently enabled,
RESTORE-SQL-READER-SYNTAX-STATE will disable it again."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-sql-reader-syntax* nil)
    (%disable-sql-reader-syntax)))

(defmacro locally-disable-sql-reader-syntax ()
  "Turns off the SQL reader syntax without changing the syntax
state such that RESTORE-SQL-READER-SYNTAX-STATE will re-establish
the current syntax state."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-sql-reader-syntax)))

(defun %disable-sql-reader-syntax ()
  (when *original-readtable*
    (setf *readtable* *original-readtable*
          *original-readtable* nil))
  (values))


;; Exported functions for enabling SQL syntax.

(defmacro enable-sql-reader-syntax ()
  "Turns on the SQL reader syntax setting the syntax state such
that if the syntax is subsequently disabled,
RESTORE-SQL-READER-SYNTAX-STATE will enable it again."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-sql-reader-syntax* t)
    (%enable-sql-reader-syntax)))

(defmacro locally-enable-sql-reader-syntax ()
  "Turns on the SQL reader syntax without changing the syntax
state such that RESTORE-SQL-READER-SYNTAX-STATE will re-establish
the current syntax state."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-sql-reader-syntax)))

(defmacro file-enable-sql-reader-syntax ()
  "Turns on the SQL reader syntax for the rest of the file.
The CL spec says that when finished loading a file the original
*readtable* is restored.  clhs COMPILE-FILE"
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (copy-readtable))
    (set-macro-character *sql-macro-open-char* #'sql-reader-open)
    (set-macro-character *sql-macro-close-char* (get-macro-character #\)))))

(defun %enable-sql-reader-syntax ()
  (unless *original-readtable*
    (setf *original-readtable* *readtable*
          *readtable* (copy-readtable))
    (set-macro-character *sql-macro-open-char* #'sql-reader-open)
    (set-macro-character *sql-macro-close-char* (get-macro-character #\))))
  (values))

(defmacro restore-sql-reader-syntax-state ()
  "Enables the SQL reader syntax if ENABLE-SQL-READER-SYNTAX has
been called more recently than DISABLE-SQL-READER-SYNTAX and
otherwise disables the SQL reader syntax. By default, the SQL
reader syntax is disabled."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (if *restore-sql-reader-syntax*
        (%enable-sql-reader-syntax)
        (%disable-sql-reader-syntax))))

(defun sql-reader-open (stream char)
  (declare (ignore char))
  (let ((sqllist (read-delimited-list #\] stream t)))
    (unless *read-suppress*
      (handler-case
          (cond ((string= (write-to-string (car sqllist)) "||")
                 (cons (sql-operator 'concat-op) (cdr sqllist)))
                ((and (= (length sqllist) 1) (eql (car sqllist) '*))
                 (apply #'generate-sql-reference sqllist))
                ((sql-operator (car sqllist))
                 (cons (sql-operator (car sqllist)) (cdr sqllist)))
                (t (apply #'generate-sql-reference sqllist)))
        (sql-user-error (c)
          (error 'sql-user-error
                 :message (format nil "Error ~A occured while attempting to parse '~A' at file position ~A"
                                  (sql-user-error-message c) sqllist (file-position stream))))))))

(defun generate-sql-reference (&rest arglist)
  (cond ((= (length arglist) 1) ; string, table or attribute
         (let ((arg (first arglist)))
           (typecase arg
             (string (sql-expression :string arg))
             (symbol ;; handle . separated names
              (let* ((sn (symbol-name arg))
                     (idx (position #\. sn)))
                (cond
                  (idx (sql-expression :table (intern (subseq sn 0 idx))
                                       :attribute (intern (subseq sn (+ idx 1))) ))
                  (T (sql-expression :attribute arg))))
              ))))
        ((<= 2 (length arglist))
         (let ((sqltype (when (keywordp (caddr arglist)) (caddr arglist) nil)))
           (cond
             ((stringp (cadr arglist))
             (sql-expression :table (car arglist)
                             :alias (cadr arglist)
                             :type sqltype))
            ((keywordp (cadr arglist))
             (sql-expression :attribute (car arglist)
                             :type (cadr arglist)))
            (t
             (sql-expression :attribute (cadr arglist)
                             :table (car arglist)
                             :type sqltype)))))
        (t
         (error 'sql-user-error :message "bad expression syntax"))))


;; Exported functions for dealing with SQL syntax

(defun sql (&rest args)
  "Returns an SQL string generated from the expressions ARGS. The
expressions are translated into SQL strings and then concatenated
with a single space delimiting each expression. An error of type
SQL-USER-ERROR is signalled if any element in ARGS is not of the
supported types (a symbol, string, number or symbolic SQL
expression) or a list or vector containing only these supported
types."
  (format nil "~{~A~^ ~}" (mapcar #'sql-output args)))

(defun sql-expression (&key string table alias attribute type)
  "Returns an SQL expression constructed from the supplied
arguments which may be combined as follows: ATTRIBUTE and TYPE;
ATTRIBUTE; ALIAS or TABLE and ATTRIBUTE and TYPE; ALIAS or TABLE
and ATTRIBUTE; TABLE, ATTRIBUTE and TYPE; TABLE and ATTRIBUTE;
TABLE and ALIAS; TABLE; and STRING. An error of type
SQL-USER-ERROR is signalled if an unsupported combination of
keyword arguments is specified."
  (cond
    (string
     (make-instance 'sql :string string))
    (attribute
     (make-instance 'sql-ident-attribute :name attribute
                    :qualifier (or table alias)
                    :type type))
    ((and table (not attribute))
     (make-instance 'sql-ident-table :name table
                    :table-alias alias))))

(defun sql-operator (operator)
  "Returns the Lisp symbol corresponding to the SQL operator
  represented by the symbol OPERATOR. If OPERATOR does not
  represent a supported SQL operator or is not a symbol, nil is
  returned."
  (typecase operator
    (string nil)
    (symbol (values (gethash (symbol-name-default-case (symbol-name operator))
                             *sql-op-table*)))))

(defun sql-operation (operator &rest args)
  "Returns an SQL expression constructed from the supplied symbol
OPERATOR representing an SQL operator or function and its
arguments ARGS. An error of type SQL-USER-ERROR is signalled if
OPERATOR is not a symbol representing a supported SQL
operator. If OPERATOR is passed the symbol FUNCTION then the
first value in ARGS must be a string representing a valid SQL
function and the remaining values in ARGS its arguments as
strings."
  (if (sql-operator operator)
      (apply (symbol-function (sql-operator operator)) args)
      (error 'sql-user-error
             :message
             (format nil "~A is not a recognized SQL operator." operator))))


