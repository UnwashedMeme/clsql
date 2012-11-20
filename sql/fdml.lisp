;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; The CLSQL Functional Data Manipulation Language (FDML).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

;; some helpers to make dealing with attribute-value-pairs a bit more structured
(defclass attribute-value-pair ()
  ((attribute :accessor attribute :initarg :attribute :initform nil)
   (db-value :accessor db-value :initarg :db-value :initform nil))
  (:documentation "Represents an attribute-sql-expression and its value, used
   to pass to insert/update. Was previously a two list"))

(defun make-attribute-value-pair (slot-def value database)
  (check-slot-type slot-def value)
  (make-instance
   'attribute-value-pair
   :attribute (sql-expression :attribute (database-identifier slot-def database))
   :db-value (db-value-from-slot slot-def value database)))

(defun to-attributes-and-values (av-pairs)
  (etypecase (first av-pairs)
    (list
     (loop for (a v) in av-pairs
           collect a into attributes
           collect v into db-values
           finally (return (values attributes db-values))))
    (attribute-value-pair
     (loop for pair in av-pairs
           collecting (attribute pair) into attributes
           collecting (db-value pair) into db-values
           finally (return (values attributes db-values))))))

;;; Basic operations on databases

(defmethod database-query-result-set ((expr %sql-expression) database
                                      &key full-set result-types)
  (database-query-result-set (sql-output expr database) database
                             :full-set full-set :result-types result-types))

(defmethod execute-command ((sql-expression string)
                            &key (database *default-database*))
  (record-sql-command sql-expression database)
  (let ((res (database-execute-command sql-expression database)))
    (record-sql-result res database))
  (values))

(defmethod execute-command ((expr %sql-expression)
                            &key (database *default-database*))
  (execute-command (sql-output expr database) :database database)
  (values))

(defmethod query ((query-expression string) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (record-sql-command query-expression database)
  (multiple-value-bind (rows names)
      (database-query query-expression database result-types field-names)
    (let ((result (if (and flatp (= 1 (length (car rows))))
                      (mapcar #'car rows)
                    rows)))
      (record-sql-result result database)
      (if field-names
          (values result names)
        result))))

(defmethod query ((expr %sql-expression) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (query (sql-output expr database) :database database :flatp flatp
         :result-types result-types :field-names field-names))

(defmethod query ((expr sql-object-query) &key (database *default-database*)
                  (result-types :auto) (flatp nil) (field-names t))
  (declare (ignore result-types field-names))
  (apply #'select (append (slot-value expr 'objects)
                          (slot-value expr 'exp)
                          (when (slot-value expr 'refresh)
                            (list :refresh (sql-output expr database)))
                          (when (or flatp (slot-value expr 'flatp) )
                            (list :flatp t))
                          (list :database database))))


(defun print-query (query-exp &key titles (formats t) (sizes t) (stream t)
                              (database *default-database*))
  "Prints a tabular report of the results returned by the SQL
query QUERY-EXP, which may be a symbolic SQL expression or a
string, in DATABASE which defaults to *DEFAULT-DATABASE*. The
report is printed onto STREAM which has a default value of t
which means that *STANDARD-OUTPUT* is used. The TITLE argument,
which defaults to nil, allows the specification of a list of
strings to use as column titles in the tabular output. SIZES
accepts a list of column sizes, one for each column selected by
QUERY-EXP, to use in formatting the tabular report. The default
value of t means that minimum sizes are computed. FORMATS is a
list of format strings to be used for printing each column
selected by QUERY-EXP. The default value of FORMATS is t meaning
that ~A is used to format all columns or ~VA if column sizes are
used."
  (flet ((compute-sizes (data)
           (mapcar #'(lambda (x)
                       (apply #'max (mapcar #'(lambda (y)
                                                (if (null y) 3 (length y)))
                                            x)))
                   (apply #'mapcar (cons #'list data))))
         (format-record (record control sizes)
           (format stream "~&~?" control
                   (if (null sizes) record
                       (mapcan #'(lambda (s f) (list s f)) sizes record)))))
    (let* ((query-exp (etypecase query-exp
                        (string query-exp)
                        (sql-query (sql-output query-exp database))))
           (data (query query-exp :database database :result-types nil
                        :field-names nil))
           (sizes (if (or (null sizes) (listp sizes)) sizes
                      (compute-sizes (if titles (cons titles data) data))))
           (formats (if (or (null formats) (not (listp formats)))
                        (make-list (length (car data)) :initial-element
                                   (if (null sizes) "~A " "~VA "))
                        formats))
           (control-string (format nil "~{~A~}" formats)))
      (when titles (format-record titles control-string sizes))
      (dolist (d data (values)) (format-record d control-string sizes)))))

(defun insert-records (&key (into nil)
                            (attributes nil)
                            (values nil)
                            (av-pairs nil)
                            (query nil)
                            (database *default-database*))
  "Inserts records into the table specified by INTO in DATABASE
which defaults to *DEFAULT-DATABASE*. There are five ways of
specifying the values inserted into each row.

In the first VALUES contains a list of values to insert and ATTRIBUTES,
AV-PAIRS and QUERY are nil. This can be used when values are supplied for all
attributes in INTO.

In the second, ATTRIBUTES is a list of column names, VALUES is a corresponding
list of values and AV-PAIRS and QUERY are nil.

In the third, ATTRIBUTES, VALUES and QUERY are nil and AV-PAIRS is a list
of (attribute value) pairs, or attribute-value-pair objects.

In the fourth, VALUES, AV-PAIRS and ATTRIBUTES are nil and QUERY is a symbolic
SQL query expression in which the selected columns also exist in INTO.

In the fifth method, VALUES and AV-PAIRS are nil and ATTRIBUTES is a list of
column names and QUERY is a symbolic SQL query expression which returns values
for the specified columns."
  (let ((stmt (make-sql-insert :into into :attrs attributes
                               :vals values :av-pairs av-pairs
                               :subquery query)))
    (execute-command stmt :database database)))

(defun make-sql-insert (&key (into nil)
                        (attrs nil)
                        (vals nil)
                        (av-pairs nil)
                        (subquery nil))
  (unless into
    (error 'sql-user-error :message ":into keyword not supplied"))
  (let ((insert (make-instance 'sql-insert :into (database-identifier into nil))))
    (with-slots (attributes values query) insert

      (cond ((and vals (not attrs) (not query) (not av-pairs))
             (setf values vals))

            ((and vals attrs (not subquery) (not av-pairs))
             (setf attributes attrs)
             (setf values vals))

            ((and av-pairs (not vals) (not attrs) (not subquery))
             (multiple-value-setq (attributes values)
               (to-attributes-and-values av-pairs)))

            ((and subquery (not vals) (not attrs) (not av-pairs))
             (setf query subquery))

            ((and subquery attrs (not vals) (not av-pairs))
             (setf attributes attrs)
             (setf query subquery))

            (t (error 'sql-user-error
                      :message "bad or ambiguous keyword combination.")))
      insert)))

(defun delete-records (&key (from nil)
                            (where nil)
                            (database *default-database*))
  "Deletes records satisfying the SQL expression WHERE from the
table specified by FROM in DATABASE specifies a database which
defaults to *DEFAULT-DATABASE*."
  (let ((stmt (make-instance 'sql-delete :from (database-identifier from database) :where where)))
    (execute-command stmt :database database)))

(defun update-records (table &key (attributes nil)
                            (values nil)
                            (av-pairs nil)
                            (where nil)
                            (database *default-database*))
  "Updates the attribute values of existing records satsifying
the SQL expression WHERE in the table specified by TABLE in
DATABASE which defaults to *DEFAULT-DATABASE*. There are three
ways of specifying the values to update for each row. In the
first, VALUES contains a list of values to use in the update and
ATTRIBUTES and AV-PAIRS are nil. This can be used when values are
supplied for all attributes in TABLE. In the second, ATTRIBUTES
is a list of column names, VALUES is a corresponding list of
values and AV-PAIRS is nil. In the third, ATTRIBUTES and VALUES
are nil and AV-PAIRS is an alist of (attribute value) pairs."
  (when av-pairs
    (multiple-value-setq (attributes values)
      (to-attributes-and-values av-pairs)))
  (let ((stmt (make-instance 'sql-update :table (database-identifier table database)
                             :attributes attributes
                             :values values
                             :where where)))
    (execute-command stmt :database database)))


;;; Iteration

(defmacro do-query (((&rest args) query-expression
                     &key (database '*default-database*) (result-types :auto))
                    &body body)
  "Repeatedly executes BODY within a binding of ARGS on the
fields of each row selected by the SQL query QUERY-EXPRESSION,
which may be a string or a symbolic SQL expression, in DATABASE
which defaults to *DEFAULT-DATABASE*. The values returned by the
execution of BODY are returned. RESULT-TYPES is a list of symbols
which specifies the lisp type for each field returned by
QUERY-EXPRESSION. If RESULT-TYPES is nil all results are returned
as strings whereas the default value of :auto means that the lisp
types are automatically computed for each field."
  (let ((result-set (gensym "RESULT-SET-"))
        (qe (gensym "QUERY-EXPRESSION-"))
        (columns (gensym "COLUMNS-"))
        (row (gensym "ROW-"))
        (db (gensym "DB-"))
        (last-form-eval (gensym "LFE-")))
    `(let ((,qe ,query-expression)
           (,db ,database))
      (typecase ,qe
        (sql-object-query
         (dolist (,row (query ,qe :database ,db))
           (destructuring-bind ,args
               ,row
             ,@body)))
        (t
         ;; Functional query
         (multiple-value-bind (,result-set ,columns)
             (database-query-result-set ,qe ,db
                                        :full-set nil
                                          :result-types ,result-types)
           (when ,result-set
             (unwind-protect
                  (do ((,row (make-list ,columns))
                       (,last-form-eval nil))
                      ((not (database-store-next-row ,result-set ,db ,row))
                       ,last-form-eval)
                    (destructuring-bind ,args ,row
                      (setq ,last-form-eval
                            (progn
                              ,@body))))
               (database-dump-result-set ,result-set ,db)))))))))

(defun map-query (output-type-spec function query-expression
                  &key (database *default-database*)
                  (result-types :auto))
  "Map the function FUNCTION over the attribute values of each
row selected by the SQL query QUERY-EXPRESSION, which may be a
string or a symbolic SQL expression, in DATABASE which defaults
to *DEFAULT-DATABASE*. The results of the function are collected
as specified in OUTPUT-TYPE-SPEC and returned like in
MAP. RESULT-TYPES is a list of symbols which specifies the lisp
type for each field returned by QUERY-EXPRESSION. If RESULT-TYPES
is nil all results are returned as strings whereas the default
value of :auto means that the lisp types are automatically
computed for each field."
  (typecase query-expression
    (sql-object-query
     (map output-type-spec #'(lambda (x) (apply function x))
          (query query-expression)))
    (t
     ;; Functional query
     (macrolet ((type-specifier-atom (type)
                  `(if (atom ,type) ,type (car ,type))))
       (case (type-specifier-atom output-type-spec)
         ((nil)
          (map-query-for-effect function query-expression database
                                result-types))
         (list
          (map-query-to-list function query-expression database result-types))
         ((simple-vector simple-string vector string array simple-array
                         bit-vector simple-bit-vector base-string
                         simple-base-string)
          (map-query-to-simple output-type-spec function query-expression
                               database result-types))
         (t
          (funcall #'map-query
                   (cmucl-compat:result-type-or-lose output-type-spec t)
                   function query-expression :database database
                   :result-types result-types)))))))

(defun map-query-for-effect (function query-expression database result-types)
  (multiple-value-bind (result-set columns)
      (database-query-result-set query-expression database :full-set nil
                                 :result-types result-types)
    (let ((flatp (and (= columns 1)
                      (typep query-expression 'sql-query)
                      (slot-value query-expression 'flatp))))
      (when result-set
        (unwind-protect
             (do ((row (make-list columns)))
                 ((not (database-store-next-row result-set database row))
                  nil)
               (if flatp
                   (apply function row)
                   (funcall function row)))
          (database-dump-result-set result-set database))))))

(defun map-query-to-list (function query-expression database result-types)
  (multiple-value-bind (result-set columns)
      (database-query-result-set query-expression database :full-set nil
                                 :result-types result-types)
    (let ((flatp (and (= columns 1)
                      (typep query-expression 'sql-query)
                      (slot-value query-expression 'flatp))))
      (when result-set
        (unwind-protect
             (let ((result (list nil)))
               (do ((row (make-list columns))
                    (current-cons result (cdr current-cons)))
                   ((not (database-store-next-row result-set database row))
                    (cdr result))
                 (rplacd current-cons
                         (list (if flatp
                                   (apply function row)
                                   (funcall function (copy-list row)))))))
          (database-dump-result-set result-set database))))))

(defun map-query-to-simple (output-type-spec function query-expression database result-types)
  (multiple-value-bind (result-set columns rows)
      (database-query-result-set query-expression database :full-set t
                                 :result-types result-types)
    (let ((flatp (and (= columns 1)
                      (typep query-expression 'sql-query)
                      (slot-value query-expression 'flatp))))
      (when result-set
        (unwind-protect
             (if rows
                 ;; We know the row count in advance, so we allocate once
                 (do ((result
                       (cmucl-compat:make-sequence-of-type output-type-spec rows))
                      (row (make-list columns))
                      (index 0 (1+ index)))
                     ((not (database-store-next-row result-set database row))
                      result)
                   (declare (fixnum index))
                   (setf (aref result index)
                         (if flatp
                             (apply function row)
                             (funcall function (copy-list row)))))
                 ;; Database can't report row count in advance, so we have
                 ;; to grow and shrink our vector dynamically
                 (do ((result
                       (cmucl-compat:make-sequence-of-type output-type-spec 100))
                      (allocated-length 100)
                      (row (make-list columns))
                      (index 0 (1+ index)))
                     ((not (database-store-next-row result-set database row))
                      (cmucl-compat:shrink-vector result index))
                   (declare (fixnum allocated-length index))
                   (when (>= index allocated-length)
                     (setq allocated-length (* allocated-length 2)
                           result (adjust-array result allocated-length)))
                   (setf (aref result index)
                         (if flatp
                             (apply function row)
                             (funcall function (copy-list row))))))
          (database-dump-result-set result-set database))))))

;;; Row processing macro from CLSQL

(defmacro for-each-row (((&rest fields) &key from order-by where distinct limit)
                        &body body)
  (let ((d (gensym "DISTINCT-"))
        (bind-fields (loop for f in fields collect (car f)))
        (w (gensym "WHERE-"))
        (o (gensym "ORDER-BY-"))
        (frm (gensym "FROM-"))
        (l (gensym "LIMIT-"))
        (q (gensym "QUERY-")))
    `(let ((,frm ,from)
           (,w ,where)
           (,d ,distinct)
           (,l ,limit)
           (,o ,order-by))
      (let ((,q (query-string ',fields ,frm ,w ,d ,o ,l)))
        (loop for tuple in (query ,q)
              collect (destructuring-bind ,bind-fields tuple
                   ,@body))))))

(defun query-string (fields from where distinct order-by limit)
  (concatenate
   'string
   (format nil "select ~A~{~A~^,~} from ~{~A~^ and ~}"
           (if distinct "distinct " "") (field-names fields)
           (from-names from))
   (if where (format nil " where ~{~A~^ ~}"
                     (where-strings where)) "")
   (if order-by (format nil " order by ~{~A~^, ~}"
                        (order-by-strings order-by)))
   (if limit (format nil " limit ~D" limit) "")))

(defun lisp->sql-name (field)
  (typecase field
    (string field)
    (symbol (string-upcase (symbol-name field)))
    (cons (cadr field))
    (t (format nil "~A" field))))

(defun field-names (field-forms)
  "Return a list of field name strings from a fields form"
  (loop for field-form in field-forms
        collect
        (lisp->sql-name
         (if (cadr field-form)
             (cadr field-form)
             (car field-form)))))

(defun from-names (from)
  "Return a list of field name strings from a fields form"
  (loop for table in (if (atom from) (list from) from)
        collect (lisp->sql-name table)))


(defun where-strings (where)
  (loop for w in (if (atom (car where)) (list where) where)
        collect
        (if (consp w)
            (format nil "~A ~A ~A" (second w) (first w) (third w))
            (format nil "~A" w))))

(defun order-by-strings (order-by)
  (loop for o in order-by
        collect
        (if (atom o)
            (lisp->sql-name o)
            (format nil "~A ~A" (lisp->sql-name (car o))
                    (lisp->sql-name (cadr o))))))


;;; Large objects support

(defun create-large-object (&key (database *default-database*))
  "Creates a new large object in the database and returns the object identifier"
  (database-create-large-object database))

(defun write-large-object (object-id data &key (database *default-database*))
  "Writes data to the large object"
  (database-write-large-object object-id data database))

(defun read-large-object (object-id &key (database *default-database*))
  "Reads the large object content"
  (database-read-large-object object-id database))

(defun delete-large-object (object-id &key (database *default-database*))
  "Deletes the large object in the database"
  (database-delete-large-object object-id database))


;;; Prepared statements

(defun prepare-sql (sql-stmt types &key (database *default-database*) (result-types :auto) field-names)
  "Prepares a SQL statement for execution. TYPES contains a
list of types corresponding to the input parameters. Returns a
prepared-statement object.

A type can be
  :int
  :double
  :null
  (:blob n)
  (:string n)
"
  (unless (db-type-has-prepared-stmt? (database-type database))
    (error 'sql-user-error
           :message
           (format nil
                   "Database backend type ~:@(~A~) does not support prepared statements."
                   (database-type database))))

  (database-prepare sql-stmt types database result-types field-names))

(defun bind-parameter (prepared-stmt position value)
  "Sets the value of a parameter is in prepared statement."
  (database-bind-parameter prepared-stmt position value)
  value)

(defun run-prepared-sql (prepared-stmt)
  "Execute the prepared sql statment. All input parameters must be bound."
  (database-run-prepared prepared-stmt))

(defun free-prepared-sql (prepared-stmt)
  "Delete the objects associated with a prepared statement."
  (database-free-prepared prepared-stmt))
