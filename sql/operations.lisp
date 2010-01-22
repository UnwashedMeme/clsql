;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
;;;;
;;;; Definition of SQL operations used with the symbolic SQL syntax.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

;; Keep a hashtable for mapping symbols to sql generator functions,
;; for use by the bracketed reader syntax.

(defvar *sql-op-table* (make-hash-table :test #'equal))


;; Define an SQL operation type.

(defmacro defsql (function definition-keys &body body)
  `(progn
     (defun ,function ,@body)
     (let ((symbol (cadr (member :symbol ',definition-keys))))
       (setf (gethash (if symbol (symbol-name-default-case symbol) ',function)
                      *sql-op-table*)
             ',function))))


;; SQL operations

(defsql sql-query (:symbol "select") (&rest args)
  (apply #'make-query args))

(defsql sql-any (:symbol "any") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'any :args rest))

(defsql sql-some (:symbol "some") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'some :args rest))

(defsql sql-all (:symbol "all") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'all :args rest))

(defsql sql-not (:symbol "not") (&rest rest)
  (make-instance 'sql-value-exp
                 :modifier 'not :components rest))

(defsql sql-union (:symbol "union") (&rest rest)
  (make-instance 'sql-set-exp
                 :operator 'union :sub-expressions rest))

(defsql sql-intersect (:symbol "intersect") (&rest rest)
  (make-instance 'sql-set-exp
                 :operator 'intersect :sub-expressions rest))

(defsql sql-except (:symbol "except") (&rest rest)
  (make-instance 'sql-set-exp
                 :operator 'except :sub-expressions rest))

(defsql sql-minus (:symbol "minus") (&rest rest)
  (make-instance 'sql-set-exp
                 :operator 'minus :sub-expressions rest))

(defsql sql-limit (:symbol "limit") (&rest rest)
  (make-instance 'sql-query-modifier-exp
                 :modifier 'limit :components rest))

(defsql sql-group-by (:symbol "group-by") (&rest rest)
  (make-instance 'sql-query-modifier-exp
                 :modifier '|GROUP BY| :components rest))

(defsql sql-order-by (:symbol "order-by") (&rest rest)
  (make-instance 'sql-query-modifier-exp
                 :modifier '|ORDER BY| :components rest))

(defsql sql-having (:symbol "having") (&rest rest)
  (make-instance 'sql-query-modifier-exp
                 :modifier 'having :components rest))

(defsql sql-null (:symbol "null") (&rest rest)
  (if rest
      (make-instance 'sql-relational-exp :operator 'is
                     :sub-expressions (list (car rest) nil))
      (make-instance 'sql-value-exp :components 'null)))

(defsql sql-not-null (:symbol "not-null") ()
  (make-instance 'sql-value-exp
                 :components '|NOT NULL|))

(defsql sql-exists (:symbol "exists") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'exists :args rest))

(defsql sql-* (:symbol "*") (&rest rest)
  (if (zerop (length rest))
      (make-instance 'sql-ident :name '*)
      (make-instance 'sql-relational-exp :operator '* :sub-expressions rest)))

(defsql sql-+ (:symbol "+") (&rest rest)
  (if (cdr rest)
      (make-instance 'sql-relational-exp
                     :operator '+ :sub-expressions rest)
      (make-instance 'sql-value-exp :modifier '+ :components rest)))

(defsql sql-/ (:symbol "/") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '/ :sub-expressions rest))

(defsql sql-- (:symbol "-") (&rest rest)
        (if (cdr rest)
            (make-instance 'sql-relational-exp
                           :operator '- :sub-expressions rest)
            (make-instance 'sql-value-exp :modifier '- :components rest)))

(defsql sql-like (:symbol "like") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator 'like :sub-expressions rest))

(defsql sql-~* (:symbol "~*") (&rest rest)
	"This is the postgres regex match operator"
  (make-instance 'sql-relational-exp
                 :operator '~* :sub-expressions rest))

(defsql |sql-~* ANY| (:symbol "~* ANY") (&rest rest)
	"This is the postgres regex match any in an array"
  (make-instance 'sql-relational-exp
                 :operator '|~* ANY| :sub-expressions rest))

(defsql sql-array (:symbol "array") (&rest rest)
	"This is the postgres regex match any in an array"
  (make-instance 'sql-array-exp
                 :operator 'array :sub-expressions rest))

(defsql sql-uplike (:symbol "uplike") (&rest rest)
  (make-instance 'sql-upcase-like
                 :sub-expressions rest))

(defsql sql-and (:symbol "and") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator 'and :sub-expressions rest))

(defsql sql-or (:symbol "or") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator 'or :sub-expressions rest))

(defsql sql-in (:symbol "in") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator 'in :sub-expressions rest))

(defsql sql-concat-op (:symbol "concat-op") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '\|\| :sub-expressions rest))

(defsql sql-concat (:symbol "concat") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'concat :args rest))

(defsql sql-substr (:symbol "substr") (&rest rest)
  (if (= (length rest) 3)
      (make-instance 'sql-function-exp
                     :name 'substr :args rest)
      (error 'sql-user-error :message "SUBSTR must have 3 arguments.")))

(defsql sql-substring (:symbol "substring") (&rest rest)
  (if (= (length rest) 3)
      (make-instance 'sql-function-exp
                     :name 'substring :args rest)
      (error 'sql-user-error :message "SUBSTRING must have 3 arguments.")))

(defsql sql-is (:symbol "is") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator 'is :sub-expressions rest))

(defsql sql-= (:symbol "=") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '= :sub-expressions rest))

(defsql sql-== (:symbol "==") (&rest rest)
  (make-instance 'sql-assignment-exp
                 :operator '= :sub-expressions rest))

(defsql sql-< (:symbol "<") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '< :sub-expressions rest))


(defsql sql-> (:symbol ">") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '> :sub-expressions rest))

(defsql sql-<> (:symbol "<>") (&rest rest)
        (make-instance 'sql-relational-exp
                       :operator '<> :sub-expressions rest))

(defsql sql->= (:symbol ">=") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '>= :sub-expressions rest))

(defsql sql-<= (:symbol "<=") (&rest rest)
  (make-instance 'sql-relational-exp
                 :operator '<= :sub-expressions rest))

(defsql sql-count (:symbol "count") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'count :args rest))

(defsql sql-max (:symbol "max") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'max :args rest))

(defsql sql-min (:symbol "min") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'min :args rest))

(defsql sql-avg (:symbol "avg") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'avg :args rest))

(defsql sql-sum (:symbol "sum") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'sum :args rest))

(defsql sql-the (:symbol "the") (&rest rest)
  (make-instance 'sql-typecast-exp
                 :modifier (first rest) :components (second rest)))

(defsql sql-function (:symbol "function") (&rest args)
        (make-instance 'sql-function-exp
                       :name (make-symbol (car args)) :args (cdr args)))

(defsql sql-between (:symbol "between") (&rest rest)
  (if (= (length rest) 3)
      (make-instance 'sql-between-exp :name 'between :args rest)
      (error 'sql-user-error :message "BETWEEN must have 3 arguments.")))

(defsql sql-distinct (:symbol "distinct") (&rest rest)
  (make-instance 'sql-query-modifier-exp :modifier 'distinct
                 :components rest))

(defsql sql-coalesce (:symbol "coalesce") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'coalesce :args rest))

(defsql sql-nvl (:symbol "nvl") (&rest rest)
  (if (= (length rest) 2)
      (make-instance 'sql-function-exp
                     :name 'coalesce :args rest)
      (error 'sql-user-error :message "NVL accepts exactly 2 arguments.")))

(defsql sql-userenv (:symbol "userenv") (&rest rest)
  (make-instance 'sql-function-exp
                 :name 'userenv :args rest))

(defsql sql-lower  (:symbol "lower") (&rest rest)
  (if (= (length rest) 1)
      (make-instance 'sql-function-exp
                     :name 'lower :args rest)
    (error 'sql-user-error :message "LOWER must have 1 argument.")))

(defsql sql-upper  (:symbol "upper") (&rest rest)
  (if (= (length rest) 1)
      (make-instance 'sql-function-exp
                     :name 'upper :args rest)
    (error 'sql-user-error :message "UPPER must have 1 argument.")))
