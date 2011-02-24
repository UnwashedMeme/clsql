;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; Classes defining SQL expressions and methods for formatting the
;;;; appropriate SQL commands.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defvar +empty-string+ "''")

(defvar +null-string+ "NULL")

(defvar *sql-stream* nil
  "stream which accumulates SQL output")

(defun sql-output (sql-expr &optional (database *default-database*))
  "Top-level call for generating SQL strings. Returns an SQL
  string appropriate for DATABASE which corresponds to the
  supplied lisp expression SQL-EXPR."
  (progv '(*sql-stream*)
      `(,(make-string-output-stream))
    (output-sql sql-expr database)
    (get-output-stream-string *sql-stream*)))

(defmethod output-sql (expr database)
  (write-string (database-output-sql expr database) *sql-stream*)
  (values))

(defvar *output-hash* (make-hash-table :test #'equal)
  "For caching generated SQL strings.")

(defmethod output-sql :around ((sql t) database)
  (let* ((hash-key (output-sql-hash-key sql database))
         (hash-value (when hash-key (gethash hash-key *output-hash*))))
    (cond ((and hash-key hash-value)
           (write-string hash-value *sql-stream*))
          (hash-key
           (let ((*sql-stream* (make-string-output-stream)))
             (call-next-method)
             (setf hash-value (get-output-stream-string *sql-stream*))
             (setf (gethash hash-key *output-hash*) hash-value))
           (write-string hash-value *sql-stream*))
          (t
           (call-next-method)))))

(defmethod output-sql-hash-key (expr database)
  (declare (ignore expr database))
  nil)


(defclass %sql-expression ()
  ())

(defmethod output-sql ((expr %sql-expression) database)
  (declare (ignore database))
  (write-string +null-string+ *sql-stream*))

(defmethod print-object ((self %sql-expression) stream)
  (print-unreadable-object
   (self stream :type t)
   (write-string (sql-output self) stream))
  self)

;; For straight up strings

(defclass sql (%sql-expression)
  ((text
    :initarg :string
    :initform ""))
  (:documentation "A literal SQL expression."))

(defmethod make-load-form ((sql sql) &optional environment)
  (declare (ignore environment))
  (with-slots (text)
    sql
    `(make-instance 'sql :string ',text)))

(defmethod output-sql ((expr sql) database)
  (declare (ignore database))
  (write-string (slot-value expr 'text) *sql-stream*)
  t)

(defmethod print-object ((ident sql) stream)
  (format stream "#<~S \"~A\">"
          (type-of ident)
          (sql-output ident nil))
  ident)

;; For SQL Identifiers of generic type

(defclass sql-ident (%sql-expression)
  ((name
    :initarg :name
    :initform +null-string+))
  (:documentation "An SQL identifer."))

(defmethod make-load-form ((sql sql-ident) &optional environment)
  (declare (ignore environment))
  (with-slots (name)
    sql
    `(make-instance 'sql-ident :name ',name)))

(defmethod output-sql ((expr sql-ident) database)
  (with-slots (name) expr
    (write-string
     (etypecase name
       (string name)
       (symbol (symbol-name name)))
     *sql-stream*))
  t)

;; For SQL Identifiers for attributes

(defclass sql-ident-attribute (sql-ident)
  ((qualifier
    :initarg :qualifier
    :initform +null-string+)
   (type
    :initarg :type
    :initform +null-string+))
  (:documentation "An SQL Attribute identifier."))

(defmethod collect-table-refs (sql)
  (declare (ignore sql))
  nil)

(defmethod collect-table-refs ((sql sql-ident-attribute))
  (let ((qual (slot-value sql 'qualifier)))
    (when qual
      (list (make-instance 'sql-ident-table :name qual)))))

(defmethod make-load-form ((sql sql-ident-attribute) &optional environment)
  (declare (ignore environment))
  (with-slots (qualifier type name)
    sql
    `(make-instance 'sql-ident-attribute :name ',name
      :qualifier ',qualifier
      :type ',type)))

(defmethod output-sql ((expr sql-ident-attribute) database)
  (with-slots (qualifier name type) expr
    (if (and (not qualifier) (not type))
        (etypecase name
          (string
           (write-string name *sql-stream*))
          (symbol
           (write-string
            (sql-escape (symbol-name name)) *sql-stream*)))

        ;;; KMR: The TYPE field is used by CommonSQL for type conversion -- it
      ;;; should not be output in SQL statements
      #+ignore
      (format *sql-stream* "~@[~A.~]~A~@[ ~A~]"
              (when qualifier
                (sql-escape qualifier))
              (sql-escape name)
              (when type
                (symbol-name type)))
      (format *sql-stream* "~@[~A.~]~A"
              (when qualifier
                (typecase qualifier
                  (string (format nil "~s" qualifier))
                  (t (sql-escape qualifier))))
              (typecase name
                (string (format nil "~s" (sql-escape name)))
                (t (sql-escape name)))))
    t))

(defmethod output-sql-hash-key ((expr sql-ident-attribute) database)
  (with-slots (qualifier name type)
      expr
    (list (and database (database-underlying-type database))
          'sql-ident-attribute qualifier name type)))

;; For SQL Identifiers for tables

(defclass sql-ident-table (sql-ident)
  ((alias
    :initarg :table-alias :initform nil))
  (:documentation "An SQL table identifier."))

(defmethod make-load-form ((sql sql-ident-table) &optional environment)
  (declare (ignore environment))
  (with-slots (alias name)
    sql
    `(make-instance 'sql-ident-table :name ',name :table-alias ',alias)))

(defun special-char-p (s)
  "Check if a string has any special characters"
  (loop for char across s
       thereis (find char '(#\space #\, #\. #\! #\@ #\# #\$ #\%
                                 #\^ #\& #\* #\| #\( #\) #\- #\+))))

(defmethod output-sql ((expr sql-ident-table) database)
  (with-slots (name alias) expr
    (flet ((p (s) ;; the etypecase is in sql-escape too
	     (let ((sym? (symbolp s))
		   (s (sql-escape s)))
	       (format *sql-stream*
		       (if (and sym? (not (special-char-p s)))
			   "~a" "~s")
		       s))))
      (p name)
      (when alias
	(princ #\space *sql-stream*)
	(p alias))))
  t)

(defmethod output-sql-hash-key ((expr sql-ident-table) database)
  (with-slots (name alias)
      expr
    (list (and database (database-underlying-type database))
          'sql-ident-table name alias)))

(defclass sql-relational-exp (%sql-expression)
  ((operator
    :initarg :operator
    :initform nil)
   (sub-expressions
    :initarg :sub-expressions
    :initform nil))
  (:documentation "An SQL relational expression."))

(defmethod make-load-form ((self sql-relational-exp) &optional environment)
  (make-load-form-saving-slots self
                               :slot-names '(operator sub-expressions)
                               :environment environment))

(defmethod collect-table-refs ((sql sql-relational-exp))
  (let ((tabs nil))
    (dolist (exp (slot-value sql 'sub-expressions))
      (let ((refs (collect-table-refs exp)))
        (if refs (setf tabs (append refs tabs)))))
    (remove-duplicates tabs
                       :test (lambda (tab1 tab2)
                               (equal (slot-value tab1 'name)
                                      (slot-value tab2 'name))))))




;; Write SQL for relational operators (like 'AND' and 'OR').
;; should do arity checking of subexpressions

(defmethod output-sql ((expr sql-relational-exp) database)
  (with-slots (operator sub-expressions) expr
     ;; we do this as two runs so as not to emit confusing superflous parentheses
     ;; The first loop renders all the child outputs so that we can skip anding with
     ;; empty output (which causes sql errors)
     ;; the next loop simply emits each sub-expression with the appropriate number of
     ;; parens and operators
     (flet ((trim (sub)
	      (string-trim +whitespace-chars+
			   (with-output-to-string (*sql-stream*)
			     (output-sql sub database)))))
       (let ((str-subs (loop for sub in sub-expressions
			     for str-sub = (trim sub)
			   when (and str-sub (> (length str-sub) 0))
			     collect str-sub)))
	 (case (length str-subs)
	   (0 nil)
	   (1 (write-string (first str-subs) *sql-stream*))
	   (t
	      (write-char #\( *sql-stream*)
	      (write-string (first str-subs) *sql-stream*)
	      (loop for str-sub in (rest str-subs)
		    do
		 (write-char #\Space *sql-stream*)
		 (output-sql operator database)
		 (write-char #\Space *sql-stream*)
		 (write-string str-sub *sql-stream*))
	      (write-char #\) *sql-stream*))
	   ))))
  t)

(defclass sql-upcase-like (sql-relational-exp)
  ()
  (:documentation "An SQL 'like' that upcases its arguments."))

(defmethod output-sql ((expr sql-upcase-like) database)
  (flet ((write-term (term)
           (write-string "upper(" *sql-stream*)
           (output-sql term database)
           (write-char #\) *sql-stream*)))
    (with-slots (sub-expressions)
      expr
      (let ((subs (if (consp (car sub-expressions))
                      (car sub-expressions)
                      sub-expressions)))
        (write-char #\( *sql-stream*)
        (do ((sub subs (cdr sub)))
            ((null (cdr sub)) (write-term (car sub)))
          (write-term (car sub))
          (write-string " LIKE " *sql-stream*))
        (write-char #\) *sql-stream*))))
  t)

(defclass sql-assignment-exp (sql-relational-exp)
  ()
  (:documentation "An SQL Assignment expression."))


(defmethod output-sql ((expr sql-assignment-exp) database)
  (with-slots (operator sub-expressions)
    expr
    (do ((sub sub-expressions (cdr sub)))
        ((null (cdr sub)) (output-sql (car sub) database))
      (output-sql (car sub) database)
      (write-char #\Space *sql-stream*)
      (output-sql operator database)
      (write-char #\Space *sql-stream*)))
  t)

(defclass sql-value-exp (%sql-expression)
  ((modifier
    :initarg :modifier
    :initform nil)
   (components
    :initarg :components
    :initform nil))
  (:documentation
   "An SQL value expression.")
  )

(defmethod collect-table-refs ((sql sql-value-exp))
  (let ((tabs nil))
    (if (listp (slot-value sql 'components))
        (progn
          (dolist (exp (slot-value sql 'components))
            (let ((refs (collect-table-refs exp)))
              (if refs (setf tabs (append refs tabs)))))
          (remove-duplicates tabs
                             :test (lambda (tab1 tab2)
                                     (equal (slot-value tab1 'name)
                                            (slot-value tab2 'name)))))
        nil)))



(defmethod output-sql ((expr sql-value-exp) database)
  (with-slots (modifier components)
    expr
    (if modifier
        (progn
          (write-char #\( *sql-stream*)
          (output-sql modifier database)
          (write-char #\Space *sql-stream*)
          (output-sql components database)
          (write-char #\) *sql-stream*))
        (output-sql components database))))

(defclass sql-typecast-exp (sql-value-exp)
  ()
  (:documentation "An SQL typecast expression."))

(defmethod output-sql ((expr sql-typecast-exp) database)
  (with-slots (components)
    expr
    (output-sql components database)))

(defmethod collect-table-refs ((sql sql-typecast-exp))
  (when (slot-value sql 'components)
    (collect-table-refs (slot-value sql 'components))))

(defclass sql-function-exp (%sql-expression)
  ((name
    :initarg :name
    :initform nil)
   (args
    :initarg :args
    :initform nil))
  (:documentation
   "An SQL function expression."))

(defmethod collect-table-refs ((sql sql-function-exp))
  (let ((tabs nil))
    (dolist (exp (slot-value sql 'args))
      (let ((refs (collect-table-refs exp)))
        (if refs (setf tabs (append refs tabs)))))
    (remove-duplicates tabs
                       :test (lambda (tab1 tab2)
                               (equal (slot-value tab1 'name)
                                      (slot-value tab2 'name))))))
(defvar *in-subselect* nil)

(defmethod output-sql ((expr sql-function-exp) database)
  (with-slots (name args)
    expr
    (output-sql name database)
    (let ((*in-subselect* nil)) ;; aboid double parens
      (when args (output-sql args database))))
  t)


(defclass sql-between-exp (sql-function-exp)
  ()
  (:documentation "An SQL between expression."))

(defmethod output-sql ((expr sql-between-exp) database)
  (with-slots (args)
      expr
    (output-sql (first args) database)
    (write-string " BETWEEN " *sql-stream*)
    (output-sql (second args) database)
    (write-string " AND " *sql-stream*)
    (output-sql (third args) database))
  t)

(defclass sql-query-modifier-exp (%sql-expression)
  ((modifier :initarg :modifier :initform nil)
   (components :initarg :components :initform nil))
  (:documentation "An SQL query modifier expression."))

(defmethod output-sql ((expr sql-query-modifier-exp) database)
  (with-slots (modifier components)
      expr
    (output-sql modifier database)
    (write-string " " *sql-stream*)
    (output-sql (car components) database)
    (when components
      (mapc #'(lambda (comp)
                (write-string ", " *sql-stream*)
                (output-sql comp database))
            (cdr components))))
  t)

(defclass sql-set-exp (%sql-expression)
  ((operator
    :initarg :operator
    :initform nil)
   (sub-expressions
    :initarg :sub-expressions
    :initform nil))
  (:documentation "An SQL set expression."))

(defmethod collect-table-refs ((sql sql-set-exp))
  (let ((tabs nil))
    (dolist (exp (slot-value sql 'sub-expressions))
      (let ((refs (collect-table-refs exp)))
        (if refs (setf tabs (append refs tabs)))))
    (remove-duplicates tabs
                       :test (lambda (tab1 tab2)
                               (equal (slot-value tab1 'name)
                                      (slot-value tab2 'name))))))

(defmethod output-sql ((expr sql-set-exp) database)
  (with-slots (operator sub-expressions)
      expr
    (let ((subs (if (consp (car sub-expressions))
                    (car sub-expressions)
                    sub-expressions)))
      (when (= (length subs) 1)
        (output-sql operator database)
        (write-char #\Space *sql-stream*))
      (do ((sub subs (cdr sub)))
          ((null (cdr sub)) (output-sql (car sub) database))
        (output-sql (car sub) database)
        (write-char #\Space *sql-stream*)
        (output-sql operator database)
        (write-char #\Space *sql-stream*))))
  t)

(defclass sql-query (%sql-expression)
  ((selections
    :initarg :selections
    :initform nil)
   (all
    :initarg :all
    :initform nil)
   (flatp
    :initarg :flatp
    :initform nil)
   (set-operation
    :initarg :set-operation
    :initform nil)
   (distinct
    :initarg :distinct
    :initform nil)
   (from
    :initarg :from
    :initform nil)
   (where
    :initarg :where
    :initform nil)
   (group-by
    :initarg :group-by
    :initform nil)
   (having
    :initarg :having
    :initform nil)
   (limit
    :initarg :limit
    :initform nil)
   (offset
    :initarg :offset
    :initform nil)
   (order-by
    :initarg :order-by
    :initform nil)
   (inner-join
    :initarg :inner-join
    :initform nil)
   (on
    :initarg :on
    :initform nil))
  (:documentation "An SQL SELECT query."))

(defclass sql-object-query (%sql-expression)
  ((objects
    :initarg :objects
    :initform nil)
   (flatp
    :initarg :flatp
    :initform nil)
   (exp
    :initarg :exp
    :initform nil)
   (refresh
    :initarg :refresh
    :initform nil)))

(defmethod collect-table-refs ((sql sql-query))
  (remove-duplicates (collect-table-refs (slot-value sql 'where))
                     :test (lambda (tab1 tab2)
                             (equal (slot-value tab1 'name)
                                    (slot-value tab2 'name)))))

(defvar *select-arguments*
  '(:all :database :distinct :flatp :from :group-by :having :order-by
    :set-operation :where :offset :limit :inner-join :on
    ;; below keywords are not a SQL argument, but these keywords may terminate select
    :caching :refresh))

(defun query-arg-p (sym)
  (member sym *select-arguments*))

(defun query-get-selections (select-args)
  "Return two values: the list of select-args up to the first keyword,
uninclusive, and the args from that keyword to the end."
  (let ((first-key-arg (position-if #'query-arg-p select-args)))
    (if first-key-arg
        (values (subseq select-args 0 first-key-arg)
                (subseq select-args first-key-arg))
        select-args)))

(defun make-query (&rest args)
  (flet ((select-objects (target-args)
           (and target-args
                (every #'(lambda (arg)
                           (and (symbolp arg)
                                (find-class arg nil)))
                       target-args))))
    (multiple-value-bind (selections arglist)
        (query-get-selections args)
      (if (select-objects selections)
          (destructuring-bind (&key flatp refresh &allow-other-keys) arglist
            (make-instance 'sql-object-query :objects selections
                           :flatp flatp :refresh refresh
                           :exp arglist))
          (destructuring-bind (&key all flatp set-operation distinct from where
                                    group-by having order-by
                                    offset limit inner-join on &allow-other-keys)
              arglist
            (if (null selections)
                (error "No target columns supplied to select statement."))
            (if (null from)
                (error "No source tables supplied to select statement."))
            (make-instance 'sql-query :selections selections
                           :all all :flatp flatp :set-operation set-operation
                           :distinct distinct :from from :where where
                           :limit limit :offset offset
                           :group-by group-by :having having :order-by order-by
                           :inner-join inner-join :on on))))))

(defmethod output-sql ((query sql-query) database)
  (with-slots (distinct selections from where group-by having order-by
                        limit offset inner-join on all set-operation)
      query
    (when *in-subselect*
      (write-string "(" *sql-stream*))
    (write-string "SELECT " *sql-stream*)
    (when (and limit (eql :mssql (database-underlying-type database)))
      (write-string " TOP " *sql-stream*)
      (output-sql limit database)
      (write-string " " *sql-stream*))
    (when all
      (write-string " ALL " *sql-stream*))
    (when (and distinct (not all))
      (write-string " DISTINCT " *sql-stream*)
      (unless (eql t distinct)
        (write-string " ON " *sql-stream*)
        (output-sql distinct database)
        (write-char #\Space *sql-stream*)))
    (let ((*in-subselect* t))
      (output-sql (apply #'vector selections) database))
    (when from
      (write-string " FROM " *sql-stream*)
      (labels ((ident-string-val (a)
                 (typecase a
                   (sql-ident
                    (or (ignore-errors (slot-value a 'alias))
                        (ignore-errors (slot-value a 'name))))
                   (string a)))
               (ident-table-equal (a b)
                 ;; The things should be type compatable
                 (string-equal (ident-string-val a)
                               (ident-string-val b))))
        (typecase from
          (list (output-sql (apply #'vector
                                   (remove-duplicates from
                                                      :test #'ident-table-equal))
                            database))
          (string (format *sql-stream* "~s" (sql-escape from)))
          (t (let ((*in-subselect* t))
               (output-sql from database))))))
    (when inner-join
      (write-string " INNER JOIN " *sql-stream*)
      (output-sql inner-join database))
    (when on
      (write-string " ON " *sql-stream*)
      (output-sql on database))
    (when where
      (let ((where-out (string-trim
                        '(#\newline #\space #\tab #\return)
                        (with-output-to-string (*sql-stream*)
                          (let ((*in-subselect* t))
                            (output-sql where database))))))
        (when (> (length where-out) 0)
          (write-string " WHERE " *sql-stream*)
          (write-string where-out *sql-stream*))))
    (when group-by
      (write-string " GROUP BY " *sql-stream*)
      (if (listp group-by)
          (do ((order group-by (cdr order)))
              ((null order))
            (let ((item (car order)))
              (typecase item
                (cons
                 (output-sql (car item) database)
                 (format *sql-stream* " ~A" (cadr item)))
                (t
                 (output-sql item database)))
              (when (cdr order)
                (write-char #\, *sql-stream*))))
          (output-sql group-by database)))
    (when having
      (write-string " HAVING " *sql-stream*)
      (output-sql having database))
    (when order-by
      (write-string " ORDER BY " *sql-stream*)
      (if (listp order-by)
          (do ((order order-by (cdr order)))
              ((null order))
            (let ((item (car order)))
              (typecase item
                (cons
                 (output-sql (car item) database)
                 (format *sql-stream* " ~A" (cadr item)))
                (t
                 (output-sql item database)))
              (when (cdr order)
                (write-char #\, *sql-stream*))))
          (output-sql order-by database)))
    (when (and limit (not (eql :mssql (database-underlying-type database))))
      (write-string " LIMIT " *sql-stream*)
      (output-sql limit database))
    (when offset
      (write-string " OFFSET " *sql-stream*)
      (output-sql offset database))
    (when *in-subselect*
      (write-string ")" *sql-stream*))
    (when set-operation
      (write-char #\Space *sql-stream*)
      (output-sql set-operation database)))
  t)

(defmethod output-sql ((query sql-object-query) database)
  (declare (ignore database))
  (with-slots (objects)
      query
    (when objects
      (format *sql-stream* "(~{~A~^ ~})" objects))))


;; INSERT

(defclass sql-insert (%sql-expression)
  ((into
    :initarg :into
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)
   (values
    :initarg :values
    :initform nil)
   (query
    :initarg :query
    :initform nil))
  (:documentation
   "An SQL INSERT statement."))

(defmethod output-sql ((ins sql-insert) database)
  (with-slots (into attributes values query)
    ins
    (write-string "INSERT INTO " *sql-stream*)
    (output-sql
     (typecase into
       (string (sql-expression :table into))
       (t into))
     database)
    (when attributes
      (write-char #\Space *sql-stream*)
      (output-sql attributes database))
    (when values
      (write-string " VALUES " *sql-stream*)
      (output-sql values database))
    (when query
      (write-char #\Space *sql-stream*)
      (output-sql query database)))
  t)

;; DELETE

(defclass sql-delete (%sql-expression)
  ((from
    :initarg :from
    :initform nil)
   (where
    :initarg :where
    :initform nil))
  (:documentation
   "An SQL DELETE statement."))

(defmethod output-sql ((stmt sql-delete) database)
  (with-slots (from where)
    stmt
    (write-string "DELETE FROM " *sql-stream*)
    (typecase from
      ((or symbol string) (write-string (sql-escape from) *sql-stream*))
      (t  (output-sql from database)))
    (when where
      (write-string " WHERE " *sql-stream*)
      (output-sql where database)))
  t)

;; UPDATE

(defclass sql-update (%sql-expression)
  ((table
    :initarg :table
    :initform nil)
   (attributes
    :initarg :attributes
    :initform nil)
   (values
    :initarg :values
    :initform nil)
   (where
    :initarg :where
    :initform nil))
  (:documentation "An SQL UPDATE statement."))

(defmethod output-sql ((expr sql-update) database)
  (with-slots (table where attributes values)
    expr
    (flet ((update-assignments ()
             (mapcar #'(lambda (a b)
                         (make-instance 'sql-assignment-exp
                                        :operator '=
                                        :sub-expressions (list a b)))
                     attributes values)))
      (write-string "UPDATE " *sql-stream*)
      (output-sql table database)
      (write-string " SET " *sql-stream*)
      (output-sql (apply #'vector (update-assignments)) database)
      (when where
        (write-string " WHERE " *sql-stream*)
        (output-sql where database))))
  t)

;; CREATE TABLE

(defclass sql-create-table (%sql-expression)
  ((name
    :initarg :name
    :initform nil)
   (columns
    :initarg :columns
    :initform nil)
   (modifiers
    :initarg :modifiers
    :initform nil)
   (transactions
    :initarg :transactions
    :initform nil))
  (:documentation
   "An SQL CREATE TABLE statement."))

;; Here's a real warhorse of a function!

(declaim (inline listify))
(defun listify (x)
  (if (listp x)
      x
      (list x)))

(defmethod output-sql ((stmt sql-create-table) database)
  (flet ((output-column (column-spec)
           (destructuring-bind (name type &optional db-type &rest constraints)
               column-spec
             (let ((type (listify type)))
               (output-sql name database)
               (write-char #\Space *sql-stream*)
               (write-string
                (if (stringp db-type) db-type ; override definition
                  (database-get-type-specifier (car type) (cdr type) database
                                               (database-underlying-type database)))
                *sql-stream*)
               (let ((constraints (database-constraint-statement
                                   (if (and db-type (symbolp db-type))
                                       (cons db-type constraints)
                                       constraints)
                                   database)))
                 (when constraints
                   (write-string " " *sql-stream*)
                   (write-string constraints *sql-stream*)))))))
    (with-slots (name columns modifiers transactions)
      stmt
      (write-string "CREATE TABLE " *sql-stream*)
      (etypecase name
          (string (format *sql-stream* "~s" (sql-escape name)))
          (symbol (write-string (sql-escape name) *sql-stream*))
          (sql-ident (output-sql name database)))
      (write-string " (" *sql-stream*)
      (do ((column columns (cdr column)))
          ((null (cdr column))
           (output-column (car column)))
        (output-column (car column))
        (write-string ", " *sql-stream*))
      (when modifiers
        (do ((modifier (listify modifiers) (cdr modifier)))
            ((null modifier))
          (write-string ", " *sql-stream*)
          (write-string (car modifier) *sql-stream*)))
      (write-char #\) *sql-stream*)
      (when (and (eq :mysql (database-underlying-type database))
                 transactions
                 (db-type-transaction-capable? :mysql database))
        (write-string " Type=InnoDB" *sql-stream*))))
  t)


;; CREATE VIEW

(defclass sql-create-view (%sql-expression)
  ((name :initarg :name :initform nil)
   (column-list :initarg :column-list :initform nil)
   (query :initarg :query :initform nil)
   (with-check-option :initarg :with-check-option :initform nil))
  (:documentation "An SQL CREATE VIEW statement."))

(defmethod output-sql ((stmt sql-create-view) database)
  (with-slots (name column-list query with-check-option) stmt
    (write-string "CREATE VIEW " *sql-stream*)
    (output-sql name database)
    (when column-list (write-string " " *sql-stream*)
          (output-sql (listify column-list) database))
    (write-string " AS " *sql-stream*)
    (output-sql query database)
    (when with-check-option (write-string " WITH CHECK OPTION" *sql-stream*))))


;;
;; DATABASE-OUTPUT-SQL
;;

(defmethod database-output-sql ((str string) database)
  (declare (optimize (speed 3) (safety 1)
                     #+cmu (extensions:inhibit-warnings 3)))
  (let ((len (length str)))
    (declare (type fixnum len))
    (cond ((zerop len)
           +empty-string+)
          ((and (null (position #\' str))
                (null (position #\\ str)))
           (concatenate 'string "'" str "'"))
          (t
           (let ((buf (make-string (+ (* len 2) 2) :initial-element #\')))
             (declare (simple-string buf))
             (do* ((i 0 (incf i))
                   (j 1 (incf j)))
                  ((= i len) (subseq buf 0 (1+ j)))
               (declare (type fixnum i j))
               (let ((char (aref str i)))
                 (declare (character char))
                 (cond ((char= char #\')
                        (setf (aref buf j) #\')
                        (incf j)
                        (setf (aref buf j) #\'))
                       ((and (char= char #\\)
                             ;; MTP: only escape backslash with pgsql/mysql
                             (member (database-underlying-type database)
                                     '(:postgresql :mysql)
                                     :test #'eq))
                        (setf (aref buf j) #\\)
                        (incf j)
                        (setf (aref buf j) #\\))
                       (t
                        (setf (aref buf j) char))))))))))

(let ((keyword-package (symbol-package :foo)))
  (defmethod database-output-sql ((sym symbol) database)
  (if (null sym)
      +null-string+
    (if (equal (symbol-package sym) keyword-package)
        (concatenate 'string "'" (string sym) "'")
      (symbol-name sym)))))

(defmethod database-output-sql ((tee (eql t)) database)
  (if database
      (let ((val (database-output-sql-as-type 'boolean t database (database-type database))))
        (when val
          (typecase val
            (string (format nil "'~A'" val))
            (integer (format nil "~A" val)))))
    "'Y'"))

#+nil(defmethod database-output-sql ((tee (eql t)) database)
  (declare (ignore database))
  "'Y'")

(defmethod database-output-sql ((num number) database)
  (declare (ignore database))
  (number-to-sql-string num))

(defmethod database-output-sql ((arg list) database)
  (if (null arg)
      +null-string+
      (format nil "(~{~A~^,~})" (mapcar #'(lambda (val)
                                            (sql-output val database))
                                        arg))))

(defmethod database-output-sql ((arg vector) database)
  (format nil "~{~A~^,~}" (map 'list #'(lambda (val)
                                         (sql-output val database))
                               arg)))

(defmethod output-sql-hash-key ((arg vector) database)
  (list 'vector (map 'list (lambda (arg)
                             (or (output-sql-hash-key arg database)
                                 (return-from output-sql-hash-key nil)))
                     arg)))

(defmethod database-output-sql ((self wall-time) database)
  (declare (ignore database))
  (db-timestring self))

(defmethod database-output-sql ((self date) database)
  (declare (ignore database))
  (db-datestring self))

(defmethod database-output-sql ((self duration) database)
  (declare (ignore database))
  (format nil "'~a'" (duration-timestring self)))

#+ignore
(defmethod database-output-sql ((self money) database)
  (database-output-sql (slot-value self 'odcl::units) database))

(defmethod database-output-sql (thing database)
  (if (or (null thing)
          (eq 'null thing))
      +null-string+
    (error 'sql-user-error
           :message
           (format nil
                   "No type conversion to SQL for ~A is defined for DB ~A."
                   (type-of thing) (type-of database)))))


;;
;; Column constraint types and conversion to SQL
;;

(defparameter *constraint-types*
  (list
   (cons (symbol-name-default-case "NOT-NULL") "NOT NULL")
   (cons (symbol-name-default-case "PRIMARY-KEY") "PRIMARY KEY")
   (cons (symbol-name-default-case "NOT") "NOT")
   (cons (symbol-name-default-case "NULL") "NULL")
   (cons (symbol-name-default-case "PRIMARY") "PRIMARY")
   (cons (symbol-name-default-case "KEY") "KEY")
   (cons (symbol-name-default-case "UNSIGNED") "UNSIGNED")
   (cons (symbol-name-default-case "ZEROFILL") "ZEROFILL")
   (cons (symbol-name-default-case "AUTO-INCREMENT") "AUTO_INCREMENT")
   (cons (symbol-name-default-case "DEFAULT") "DEFAULT")
   (cons (symbol-name-default-case "UNIQUE") "UNIQUE")
   (cons (symbol-name-default-case "IDENTITY") "IDENTITY (1,1)") ;; added for sql-server support
   ))

(defmethod database-constraint-statement (constraint-list database)
  (declare (ignore database))
  (make-constraints-description constraint-list))

(defun make-constraints-description (constraint-list)
  (if constraint-list
      (let ((string ""))
        (do ((constraint constraint-list (cdr constraint)))
            ((null constraint) string)
          (let ((output (assoc (symbol-name (car constraint))
                               *constraint-types*
                               :test #'equal)))
            (if (null output)
                (error 'sql-user-error
                       :message (format nil "unsupported column constraint '~A'"
                                        constraint))
                (setq string (concatenate 'string string (cdr output))))
	    (when (equal (symbol-name (car constraint)) "DEFAULT")
	      (setq constraint (cdr constraint))
	      (setq string (concatenate 'string string " " (car constraint))))
            (if (< 1 (length constraint))
                (setq string (concatenate 'string string " "))))))))

