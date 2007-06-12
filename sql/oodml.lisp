;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; The CLSQL Object Oriented Data Manipulation Language (OODML).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


(defun key-qualifier-for-instance (obj &key (database *default-database*) this-class)
  (let* ((obj-class (or this-class (class-of obj)))
         (tb (view-table obj-class)))
    (flet ((qfk (k)
             (sql-operation '==
                            (sql-expression :attribute
                                            (view-class-slot-column k)
                                            :table tb)
                            (db-value-from-slot
                             k
                             (slot-value obj (slot-definition-name k))
                             database))))
      (let* ((keys (keyslots-for-class obj-class))
             (keyxprs (mapcar #'qfk (reverse keys))))
        (cond
          ((= (length keyxprs) 0) nil)
          ((= (length keyxprs) 1) (car keyxprs))
          ((> (length keyxprs) 1) (apply #'sql-operation 'and keyxprs)))))))

;;
;; Function used by 'generate-selection-list'
;;

(defun generate-attribute-reference (vclass slotdef)
  (cond
    ((eq (view-class-slot-db-kind slotdef) :base)
     (sql-expression :attribute (view-class-slot-column slotdef)
                     :table (view-table vclass)))
    ((eq (view-class-slot-db-kind slotdef) :key)
     (sql-expression :attribute (view-class-slot-column slotdef)
                     :table (view-table vclass)))
    (t nil)))

;;
;; Function used by 'find-all'
;;

(defun generate-selection-list (vclass)
  (let* ((sels nil)
         (this-class vclass)
         (slots (if (normalizedp vclass)
                    (labels ((getdslots ()
                               (let ((sl (ordered-class-direct-slots this-class)))
                                 (cond (sl)
                                       (t
                                        (setf this-class
                                              (car (class-direct-superclasses this-class)))
                                        (getdslots))))))
                      (getdslots))
                    (ordered-class-slots this-class))))
    (dolist (slotdef slots)
      (let ((res (generate-attribute-reference this-class slotdef)))
        (when res
          (push (cons slotdef res) sels))))
    (if sels
        sels
        (error "No slots of type :base in view-class ~A" (class-name vclass)))))



(defun generate-retrieval-joins-list (vclass retrieval-method)
  "Returns list of immediate join slots for a class."
  (let ((join-slotdefs nil))
    (dolist (slotdef (ordered-class-slots vclass) join-slotdefs)
      (when (and (eq :join (view-class-slot-db-kind slotdef))
                 (eq retrieval-method (gethash :retrieval (view-class-slot-db-info slotdef))))
        (push slotdef join-slotdefs)))))

(defun generate-immediate-joins-selection-list (vclass)
  "Returns list of immediate join slots for a class."
  (let (sels)
    (dolist (joined-slot (generate-retrieval-joins-list vclass :immediate) sels)
      (let* ((join-class-name (gethash :join-class (view-class-slot-db-info joined-slot)))
             (join-class (when join-class-name (find-class join-class-name))))
        (dolist (slotdef (ordered-class-slots join-class))
          (let ((res (generate-attribute-reference join-class slotdef)))
            (when res
              (push (cons slotdef res) sels))))))
    sels))

(defmethod choose-database-for-instance ((obj standard-db-object) &optional database)
  "Determine which database connection to use for a standard-db-object.
        Errs if none is available."
  (or (find-if #'(lambda (db)
                   (and db (is-database-open db)))
               (list (view-database obj)
                     database
                     *default-database*))
      (signal-no-database-error nil)))



;; Called by 'get-slot-values-from-view'
;;

(defmethod update-slot-from-db ((instance standard-db-object) slotdef value)
  (declare (optimize (speed 3) #+cmu (extensions:inhibit-warnings 3)))
  (let* ((slot-reader (view-class-slot-db-reader slotdef))
         (slot-name   (slot-definition-name slotdef))
         (slot-type   (specified-type slotdef)))
    (cond ((and value (null slot-reader))
           (setf (slot-value instance slot-name)
                 (read-sql-value value (delistify slot-type)
                                 (choose-database-for-instance instance)
                                 (database-underlying-type
                                  (choose-database-for-instance instance)))))
          ((null value)
           (update-slot-with-null instance slot-name slotdef))
          ((typep slot-reader 'string)
           (setf (slot-value instance slot-name)
                 (format nil slot-reader value)))
          ((typep slot-reader '(or symbol function))
           (setf (slot-value instance slot-name)
                 (apply slot-reader (list value))))
          (t
           (error "Slot reader is of an unusual type.")))))

(defmethod key-value-from-db (slotdef value database)
  (declare (optimize (speed 3) #+cmu (extensions:inhibit-warnings 3)))
  (let ((slot-reader (view-class-slot-db-reader slotdef))
        (slot-type (specified-type slotdef)))
    (cond ((and value (null slot-reader))
           (read-sql-value value (delistify slot-type) database
                           (database-underlying-type database)))
          ((null value)
           nil)
          ((typep slot-reader 'string)
           (format nil slot-reader value))
          ((typep slot-reader '(or symbol function))
           (apply slot-reader (list value)))
          (t
           (error "Slot reader is of an unusual type.")))))

(defun db-value-from-slot (slotdef val database)
  (let ((dbwriter (view-class-slot-db-writer slotdef))
        (dbtype (specified-type slotdef)))
    (typecase dbwriter
      (string (format nil dbwriter val))
      ((and (or symbol function) (not null)) (apply dbwriter (list val)))
      (t
       (database-output-sql-as-type
        (typecase dbtype
          (cons (car dbtype))
          (t dbtype))
        val database (database-underlying-type database))))))

(defun check-slot-type (slotdef val)
  (let* ((slot-type (specified-type slotdef))
         (basetype (if (listp slot-type) (car slot-type) slot-type)))
    (when (and slot-type val)
      (unless (typep val basetype)
        (error 'sql-user-error
               :message
               (format nil "Invalid value ~A in slot ~A, not of type ~A."
                       val (slot-definition-name slotdef) slot-type))))))

;;
;; Called by find-all
;;

(defmethod get-slot-values-from-view (obj slotdeflist values)
  (flet ((update-slot (slot-def values)
           (update-slot-from-db obj slot-def values)))
    (mapc #'update-slot slotdeflist values)
    obj))

(defmethod update-record-from-slot ((obj standard-db-object) slot &key
                                    (database *default-database*))
  (let* ((database (choose-database-for-instance obj database))
         (view-class (class-of obj)))
    (when (normalizedp view-class)
      ;; If it's normalized, find the class that actually contains
      ;; the slot that's tied to the db
      (setf view-class
            (do ((this-class view-class
                             (car (class-direct-superclasses this-class))))
                ((member slot
                         (mapcar #'(lambda (esd) (slot-definition-name esd))
                                 (ordered-class-direct-slots this-class)))
                 this-class))))
    (let* ((vct (view-table view-class))
           (sd (slotdef-for-slot-with-class slot view-class)))
      (check-slot-type sd (slot-value obj slot))
      (let* ((att (view-class-slot-column sd))
             (val (db-value-from-slot sd (slot-value obj slot) database)))
        (cond ((and vct sd (view-database obj))
               (update-records (sql-expression :table vct)
                               :attributes (list (sql-expression :attribute att))
                               :values (list val)
                               :where (key-qualifier-for-instance
                                       obj :database database :this-class view-class)
                               :database database))
              ((and vct sd (not (view-database obj)))
               (insert-records :into (sql-expression :table vct)
                               :attributes (list (sql-expression :attribute att))
                               :values (list val)
                               :database database)
               (setf (slot-value obj 'view-database) database))
              (t
               (error "Unable to update record.")))))
    (values)))

(defmethod update-record-from-slots ((obj standard-db-object) slots &key
                                     (database *default-database*))
  (when (normalizedp (class-of obj))
    ;; FIXME: Rewrite to bundle slots for same table to be written
    ;; as avpairs (like how is done for non-normalized view-classes below)
    (dolist (slot slots)
      (update-record-from-slot obj slot :database database))
    (return-from update-record-from-slots (values)))

  (let* ((database (choose-database-for-instance obj database))
         (vct (view-table (class-of obj)))
         (sds (slotdefs-for-slots-with-class slots (class-of obj)))
         (avps (mapcar #'(lambda (s)
                           (let ((val (slot-value
                                       obj (slot-definition-name s))))
                             (check-slot-type s val)
                             (list (sql-expression
                                    :attribute (view-class-slot-column s))
                                   (db-value-from-slot s val database))))
                       sds)))
    (cond ((and avps (view-database obj))
           (let ((where (key-qualifier-for-instance
                         obj :database database)))
             (unless where
               (error "update-record-from-slots: could not generate a where clause for ~a" obj))
             (update-records (sql-expression :table vct)
                             :av-pairs avps
                             :where where
                             :database database)))
          ((and avps (not (view-database obj)))
           (insert-records :into (sql-expression :table vct)
                           :av-pairs avps
                           :database database)
           (setf (slot-value obj 'view-database) database))
          (t
           (error "Unable to update records"))))
  (values))

(defmethod update-records-from-instance ((obj standard-db-object)
                                         &key database this-class)
  (let ((database (choose-database-for-instance obj database))
        (pk nil))
    (labels ((slot-storedp (slot)
               (and (member (view-class-slot-db-kind slot) '(:base :key))
                    (slot-boundp obj (slot-definition-name slot))))
             (slot-value-list (slot)
               (let ((value (slot-value obj (slot-definition-name slot))))
                 (check-slot-type slot value)
                 (list (sql-expression :attribute (view-class-slot-column slot))
                       (db-value-from-slot slot value database)))))
      (let* ((view-class (or this-class (class-of obj)))
             (pk-slot (car (keyslots-for-class view-class)))
             (view-class-table (view-table view-class))
             (pclass (car (class-direct-superclasses view-class))))
        (when (normalizedp view-class)
          (setf pk (update-records-from-instance obj :database database
                                                 :this-class pclass))
          (when pk-slot
            (setf (slot-value obj (slot-definition-name pk-slot)) pk)))
        (let* ((slots (remove-if-not #'slot-storedp
                                     (if (normalizedp view-class)
                                         (ordered-class-direct-slots view-class)
                                         (ordered-class-slots view-class))))
               (record-values (mapcar #'slot-value-list slots)))

          (cond ((and (not (normalizedp view-class))
                      (not record-values))
                 (error "No settable slots."))
                ((and (normalizedp view-class)
                      (not record-values))
                 nil)
                ((view-database obj)
                 ;; if this slot is set, the database object was returned from a select
                 ;; and has already been in the database, so we must need an update
                 (update-records (sql-expression :table view-class-table)
                                 :av-pairs record-values
                                 :where (key-qualifier-for-instance
                                         obj :database database
                                         :this-class view-class)
                                 :database database)
                 (when pk-slot
                   (setf pk (or pk
                                (slot-value obj (slot-definition-name pk-slot))))))
                (t
		 (insert-records :into (sql-expression :table view-class-table)
                                 :av-pairs record-values
                                 :database database)

		  (when (and pk-slot (not pk))
		    (setf pk (if (or (member :auto-increment (listify (view-class-slot-db-constraints pk-slot)))
				     (not (null (view-class-slot-autoincrement-sequence pk-slot))))
				 (setf (slot-value obj (slot-definition-name pk-slot))
				       (database-last-auto-increment-id database
								       view-class-table
								       pk-slot)))))
		  (when pk-slot
		    (setf pk (or pk
				 (slot-value
				  obj (slot-definition-name pk-slot)))))
		  (when (eql this-class nil)
		    (setf (slot-value obj 'view-database) database)))))))
    ;; handle slots with defaults
    (let* ((view-class (or this-class (class-of obj)))
	   (slots (if (normalizedp view-class)
		     (ordered-class-direct-slots view-class)
		     (ordered-class-slots view-class)))) 
      (dolist (slot slots)
	(when (and (slot-exists-p slot 'db-constraints)
		   (listp (view-class-slot-db-constraints slot))
		   (member :default (view-class-slot-db-constraints slot)))
	  (unless (and (slot-boundp obj (slot-definition-name slot))
		       (slot-value obj (slot-definition-name slot)))
	    (update-slot-from-record obj (slot-definition-name slot))))))

    pk))

(defmethod delete-instance-records ((instance standard-db-object) &key database)
  (let ((database (choose-database-for-instance instance database))
        (vt (sql-expression :table (view-table (class-of instance)))))
    (if database
        (let ((qualifier (key-qualifier-for-instance instance :database database)))
          (delete-records :from vt :where qualifier :database database)
          (setf (record-caches database) nil)
          (setf (slot-value instance 'view-database) nil)
          (values))
        (signal-no-database-error database))))

(defmethod update-instance-from-records ((instance standard-db-object)
                                         &key (database *default-database*)
                                         this-class)
  (let* ((view-class (or this-class (class-of instance)))
         (pclass (car (class-direct-superclasses view-class)))
         (pres nil))
    (when (normalizedp view-class)
      (setf pres (update-instance-from-records instance :database database
                                               :this-class pclass)))
    (let* ((view-table (sql-expression :table (view-table view-class)))
           (vd (choose-database-for-instance instance database))
           (view-qual (key-qualifier-for-instance instance :database vd
                                                           :this-class view-class))
           (sels (generate-selection-list view-class))
           (res nil))
      (cond (view-qual
             (setf res (apply #'select (append (mapcar #'cdr sels)
                                               (list :from  view-table
                                                     :where view-qual
                                                     :result-types nil
                                                     :database vd))))
             (when res
	       (setf (slot-value instance 'view-database) vd)
               (get-slot-values-from-view instance (mapcar #'car sels) (car res))))
            (pres)
            (t nil)))))

(defmethod update-slot-from-record ((instance standard-db-object)
                                    slot &key (database *default-database*))
  (let* ((view-class (find-class (class-name (class-of instance))))
         (slot-def (slotdef-for-slot-with-class slot view-class)))
    (when (normalizedp view-class)
      ;; If it's normalized, find the class that actually contains
      ;; the slot that's tied to the db
      (setf view-class
            (do ((this-class view-class
                             (car (class-direct-superclasses this-class))))
                ((member slot
                         (mapcar #'(lambda (esd) (slot-definition-name esd))
                                 (ordered-class-direct-slots this-class)))
                 this-class))))
    (let* ((view-table (sql-expression :table (view-table view-class)))
           (vd (choose-database-for-instance instance database))
           (view-qual (key-qualifier-for-instance instance :database vd
                                                           :this-class view-class))
           (att-ref (generate-attribute-reference view-class slot-def))
           (res (select att-ref :from  view-table :where view-qual
                                                  :result-types nil)))
      (when res
	(setf (slot-value instance 'view-database) vd)
        (get-slot-values-from-view instance (list slot-def) (car res))))))

(defmethod update-slot-with-null ((object standard-db-object)
                                  slotname
                                  slotdef)
  (setf (slot-value object slotname) (slot-value slotdef 'void-value)))

(defvar +no-slot-value+ '+no-slot-value+)

(defsql sql-slot-value (:symbol "slot-value") (classname slot &optional (value +no-slot-value+) (database *default-database*))
        (let* ((class (find-class classname))
               (sld (slotdef-for-slot-with-class slot class)))
          (if sld
              (if (eq value +no-slot-value+)
                  (sql-expression :attribute (view-class-slot-column sld)
                                  :table (view-table class))
                  (db-value-from-slot
                   sld
                   value
                   database))
              (error "Unknown slot ~A for class ~A" slot classname))))

(defsql sql-view-class (:symbol "view-class") (classname &optional (database *default-database*))
        (declare (ignore database))
        (let* ((class (find-class classname)))
          (unless (view-table class)
            (error "No view-table for class ~A"  classname))
          (sql-expression :table (view-table class))))


(defmethod database-get-type-specifier (type args database db-type)
  (declare (ignore type args database db-type))
  (format nil "VARCHAR(~D)" *default-string-length*))

(defmethod database-get-type-specifier ((type (eql 'integer)) args database db-type)
  (declare (ignore database db-type))
  (if args
      (format nil "INT(~A)" (car args))
      "INT"))

(deftype tinyint ()
  "An 8-bit integer, this width may vary by SQL implementation."
  'integer)

(defmethod database-get-type-specifier ((type (eql 'tinyint)) args database db-type)
  (declare (ignore args database db-type))
  "INT")

(deftype smallint ()
  "An integer smaller than a 32-bit integer. this width may vary by SQL implementation."
  'integer)

(defmethod database-get-type-specifier ((type (eql 'smallint)) args database db-type)
  (declare (ignore args database db-type))
  "INT")

(deftype mediumint ()
  "An integer smaller than a 32-bit integer, but may be larger than a smallint. This width may vary by SQL implementation."
  'integer)

(defmethod database-get-type-specifier ((type (eql 'mediumint)) args database db-type)
  (declare (ignore args database db-type))
  "INT")

(deftype bigint ()
  "An integer larger than a 32-bit integer, this width may vary by SQL implementation."
  'integer)

(defmethod database-get-type-specifier ((type (eql 'bigint)) args database db-type)
  (declare (ignore args database db-type))
  "BIGINT")

(deftype varchar (&optional size)
  "A variable length string for the SQL varchar type."
  (declare (ignore size))
  'string)

(defmethod database-get-type-specifier ((type (eql 'varchar)) args
                                        database db-type)
  (declare (ignore database db-type))
  (if args
      (format nil "VARCHAR(~A)" (car args))
      (format nil "VARCHAR(~D)" *default-string-length*)))

(defmethod database-get-type-specifier ((type (eql 'string)) args database db-type)
  (declare (ignore database db-type))
  (if args
      (format nil "CHAR(~A)" (car args))
      (format nil "VARCHAR(~D)" *default-string-length*)))

(deftype universal-time ()
  "A positive integer as returned by GET-UNIVERSAL-TIME."
  '(integer 1 *))

(defmethod database-get-type-specifier ((type (eql 'universal-time)) args database db-type)
  (declare (ignore args database db-type))
  "BIGINT")

(defmethod database-get-type-specifier ((type (eql 'wall-time)) args database db-type)
  (declare (ignore args database db-type))
  "TIMESTAMP")

(defmethod database-get-type-specifier ((type (eql 'date)) args database db-type)
  (declare (ignore args database db-type))
  "DATE")

(defmethod database-get-type-specifier ((type (eql 'duration)) args database db-type)
  (declare (ignore database args db-type))
  "VARCHAR")

(defmethod database-get-type-specifier ((type (eql 'money)) args database db-type)
  (declare (ignore database args db-type))
  "INT8")

#+ignore
(deftype char (&optional len)
  "A lisp type for the SQL CHAR type."
  `(string ,len))

(defmethod database-get-type-specifier ((type (eql 'float)) args database db-type)
  (declare (ignore database db-type))
  (if args
      (format nil "FLOAT(~A)" (car args))
      "FLOAT"))

(defmethod database-get-type-specifier ((type (eql 'long-float)) args database db-type)
  (declare (ignore database db-type))
  (if args
      (format nil "FLOAT(~A)" (car args))
      "FLOAT"))

(deftype generalized-boolean ()
  "A type which outputs a SQL boolean value, though any lisp type can be stored in the slot."
  t)

(defmethod database-get-type-specifier ((type (eql 'boolean)) args database db-type)
  (declare (ignore args database db-type))
  "BOOL")

(defmethod database-get-type-specifier ((type (eql 'generalized-boolean)) args database db-type)
  (declare (ignore args database db-type))
  "BOOL")

(defmethod database-get-type-specifier ((type (eql 'number)) args database db-type)
  (declare (ignore database db-type))
  (cond
    ((and (consp args) (= (length args) 2))
     (format nil "NUMBER(~D,~D)" (first args) (second args)))
    ((and (consp args) (= (length args) 1))
     (format nil "NUMBER(~D)" (first args)))
    (t
     "NUMBER")))

(defmethod database-get-type-specifier ((type (eql 'char)) args database db-type)
  (declare (ignore database db-type))
  (if args
      (format nil "CHAR(~D)" (first args))
      "CHAR(1)"))


(defmethod database-output-sql-as-type (type val database db-type)
  (declare (ignore type database db-type))
  val)

(defmethod database-output-sql-as-type ((type (eql 'list)) val database db-type)
  (declare (ignore database db-type))
  (progv '(*print-circle* *print-array*) '(t t)
    (let ((escaped (prin1-to-string val)))
      (substitute-char-string
       escaped #\Null " "))))

(defmethod database-output-sql-as-type ((type (eql 'symbol)) val database db-type)
  (declare (ignore database db-type))
  (if val
      (concatenate 'string
                   (package-name (symbol-package val))
                   "::"
                   (symbol-name val))
      ""))

(defmethod database-output-sql-as-type ((type (eql 'keyword)) val database db-type)
  (declare (ignore database db-type))
  (if val
      (symbol-name val)
      ""))

(defmethod database-output-sql-as-type ((type (eql 'vector)) val database db-type)
  (declare (ignore database db-type))
  (progv '(*print-circle* *print-array*) '(t t)
    (prin1-to-string val)))

(defmethod database-output-sql-as-type ((type (eql 'array)) val database db-type)
  (declare (ignore database db-type))
  (progv '(*print-circle* *print-array*) '(t t)
    (prin1-to-string val)))

(defmethod database-output-sql-as-type ((type (eql 'boolean)) val database db-type)
  (declare (ignore database db-type))
  (if val "t" "f"))

(defmethod database-output-sql-as-type ((type (eql 'generalized-boolean)) val database db-type)
  (declare (ignore database db-type))
  (if val "t" "f"))

(defmethod database-output-sql-as-type ((type (eql 'string)) val database db-type)
  (declare (ignore database db-type))
  val)

(defmethod database-output-sql-as-type ((type (eql 'char)) val database db-type)
  (declare (ignore database db-type))
  (etypecase val
    (character (write-to-string val))
    (string val)))

(defmethod database-output-sql-as-type ((type (eql 'float)) val database db-type)
  (declare (ignore database db-type))
  (if (eq (type-of val) 'null)
      nil
      (let ((*read-default-float-format* (type-of val)))
       (format nil "~F" val))))

(defmethod read-sql-value (val type database db-type)
  (declare (ignore database db-type))
  (cond
    ((null type) val) ;;we have no desired type, just give the value
    ((typep val type) val) ;;check that it hasn't already been converted.
    ((typep val 'string) (read-from-string val)) ;;maybe read will just take care of it?
    (T (error "Unable to read-sql-value ~a as type ~a" val type))))

(defmethod read-sql-value (val (type (eql 'string)) database db-type)
  (declare (ignore database db-type))
  val)

(defmethod read-sql-value (val (type (eql 'varchar)) database db-type)
  (declare (ignore database db-type))
  val)

(defmethod read-sql-value (val (type (eql 'char)) database db-type)
  (declare (ignore database db-type))
  (schar val 0))

(defmethod read-sql-value (val (type (eql 'keyword)) database db-type)
  (declare (ignore database db-type))
  (when (< 0 (length val))
    (intern (symbol-name-default-case val)
            (find-package '#:keyword))))

(defmethod read-sql-value (val (type (eql 'symbol)) database db-type)
  (declare (ignore database db-type))
  (when (< 0 (length val))
    (unless (string= val (symbol-name-default-case "NIL"))
      (read-from-string val))))

(defmethod read-sql-value (val (type (eql 'integer)) database db-type)
  (declare (ignore database db-type))
  (etypecase val
    (string
     (unless (string-equal "NIL" val)
       (parse-integer val)))
    (number val)))

(defmethod read-sql-value (val (type (eql 'smallint)) database db-type)
  (declare (ignore database db-type))
  (etypecase val
    (string
     (unless (string-equal "NIL" val)
       (parse-integer val)))
    (number val)))

(defmethod read-sql-value (val (type (eql 'bigint)) database db-type)
  (declare (ignore database db-type))
  (etypecase val
    (string
     (unless (string-equal "NIL" val)
       (parse-integer val)))
    (number val)))

(defmethod read-sql-value (val (type (eql 'float)) database db-type)
  (declare (ignore database db-type))
  ;; writing 1.0 writes 1, so we we *really* want a float, must do (float ...)
  (etypecase val
    (string (float (read-from-string val)))
    (float val)))

(defmethod read-sql-value (val (type (eql 'double-float)) database db-type)
  (declare (ignore database db-type))
  ;; writing 1.0 writes 1, so if we *really* want a float, must do (float ...)
  (etypecase val
    (string (float
	     (let ((*read-default-float-format* 'double-float))
	       (read-from-string val))
	     1.0d0))
    (double-float val)
    (float (coerce val 'double-float))))

(defmethod read-sql-value (val (type (eql 'boolean)) database db-type)
  (declare (ignore database db-type))
  (equal "t" val))

(defmethod read-sql-value (val (type (eql 'generalized-boolean)) database db-type)
  (declare (ignore database db-type))
  (equal "t" val))

(defmethod read-sql-value (val (type (eql 'number)) database db-type)
  (declare (ignore database db-type))
  (etypecase val
    (string
     (unless (string-equal "NIL" val)
       (read-from-string val)))
    (number val)))

(defmethod read-sql-value (val (type (eql 'universal-time)) database db-type)
  (declare (ignore database db-type))
  (unless (eq 'NULL val)
    (etypecase val
      (string
       (parse-integer val))
      (number val))))

(defmethod read-sql-value (val (type (eql 'wall-time)) database db-type)
  (declare (ignore database db-type))
  (unless (eq 'NULL val)
    (parse-timestring val)))

(defmethod read-sql-value (val (type (eql 'date)) database db-type)
  (declare (ignore database db-type))
  (unless (eq 'NULL val)
    (parse-datestring val)))

(defmethod read-sql-value (val (type (eql 'duration)) database db-type)
  (declare (ignore database db-type))
  (unless (or (eq 'NULL val)
              (equal "NIL" val))
    (parse-timestring val)))

;; ------------------------------------------------------------
;; Logic for 'faulting in' :join slots

;; this works, but is inefficient requiring (+ 1 n-rows)
;; SQL queries
#+ignore
(defun fault-join-target-slot (class object slot-def)
  (let* ((res (fault-join-slot-raw class object slot-def))
         (dbi (view-class-slot-db-info slot-def))
         (target-name (gethash :target-slot dbi))
         (target-class (find-class target-name)))
    (when res
      (mapcar (lambda (obj)
                (list
                 (car
                  (fault-join-slot-raw
                   target-class
                   obj
                   (find target-name (class-slots (class-of obj))
                         :key #'slot-definition-name)))
                 obj))
              res)
      #+ignore ;; this doesn't work when attempting to call slot-value
      (mapcar (lambda (obj)
                (cons obj (slot-value obj ts))) res))))

(defun fault-join-target-slot (class object slot-def)
  (let* ((dbi (view-class-slot-db-info slot-def))
         (ts (gethash :target-slot dbi))
         (jc  (gethash :join-class dbi))
         (jc-view-table (view-table (find-class jc)))
         (tdbi (view-class-slot-db-info
                (find ts (class-slots (find-class jc))
                      :key #'slot-definition-name)))
         (retrieval (gethash :retrieval tdbi))
         (tsc (gethash :join-class tdbi))
         (ts-view-table (view-table (find-class tsc)))
         (jq (join-qualifier class object slot-def))
         (key (slot-value object (gethash :home-key dbi))))

    (when jq
      (ecase retrieval
        (:immediate
         (let ((res
                (find-all (list tsc)
                          :inner-join (sql-expression :table jc-view-table)
                          :on (sql-operation
                               '==
                               (sql-expression
                                :attribute (gethash :foreign-key tdbi)
                                :table ts-view-table)
                               (sql-expression
                                :attribute (gethash :home-key tdbi)
                                :table jc-view-table))
                          :where jq
                          :result-types :auto
                          :database (choose-database-for-instance object))))
           (mapcar #'(lambda (i)
                       (let* ((instance (car i))
                              (jcc (make-instance jc :view-database (choose-database-for-instance instance))))
                         (setf (slot-value jcc (gethash :foreign-key dbi))
                               key)
                         (setf (slot-value jcc (gethash :home-key tdbi))
                               (slot-value instance (gethash :foreign-key tdbi)))
                         (list instance jcc)))
                   res)))
        (:deferred
         ;; just fill in minimal slots
         (mapcar
          #'(lambda (k)
              (let ((instance (make-instance tsc :view-database (choose-database-for-instance object)))
                    (jcc (make-instance jc :view-database (choose-database-for-instance object)))
                    (fk (car k)))
                (setf (slot-value instance (gethash :home-key tdbi)) fk)
                (setf (slot-value jcc (gethash :foreign-key dbi))
                      key)
                (setf (slot-value jcc (gethash :home-key tdbi))
                      fk)
                (list instance jcc)))
          (select (sql-expression :attribute (gethash :foreign-key tdbi) :table jc-view-table)
                  :from (sql-expression :table jc-view-table)
                  :where jq
                  :database (choose-database-for-instance object))))))))


;;; Remote Joins

(defvar *default-update-objects-max-len* nil
  "The default value to use for the MAX-LEN keyword argument to
  UPDATE-OBJECT-JOINS.")

(defun update-objects-joins (objects &key (slots t) (force-p t)
                             class-name (max-len
                                         *default-update-objects-max-len*))
  "Updates from the records of the appropriate database tables
the join slots specified by SLOTS in the supplied list of View
Class instances OBJECTS.  SLOTS is t by default which means that
all join slots with :retrieval :immediate are updated. CLASS-NAME
is used to specify the View Class of all instance in OBJECTS and
default to nil which means that the class of the first instance
in OBJECTS is used. FORCE-P is t by default which means that all
join slots are updated whereas a value of nil means that only
unbound join slots are updated. MAX-LEN defaults to
*DEFAULT-UPDATE-OBJECTS-MAX-LEN* and when non-nil specifies that
UPDATE-OBJECT-JOINS may issue multiple database queries with a
maximum of MAX-LEN instances updated in each query."
  (assert (or (null max-len) (plusp max-len)))
  (when objects
    (unless class-name
      (setq class-name (class-name (class-of (first objects)))))
    (let* ((class (find-class class-name))
           (class-slots (ordered-class-slots class))
           (slotdefs
            (if (eq t slots)
                (generate-retrieval-joins-list class :deferred)
                (remove-if #'null
                           (mapcar #'(lambda (name)
                                       (let ((slotdef (find name class-slots :key #'slot-definition-name)))
                                         (unless slotdef
                                           (warn "Unable to find slot named ~S in class ~S." name class))
                                         slotdef))
                                   slots)))))
      (dolist (slotdef slotdefs)
        (let* ((dbi (view-class-slot-db-info slotdef))
               (slotdef-name (slot-definition-name slotdef))
               (foreign-key (gethash :foreign-key dbi))
               (home-key (gethash :home-key dbi))
               (object-keys
                (remove-duplicates
                 (if force-p
                     (mapcar #'(lambda (o) (slot-value o home-key)) objects)
                     (remove-if #'null
                                (mapcar
                                 #'(lambda (o) (if (slot-boundp o slotdef-name)
                                                   nil
                                                   (slot-value o home-key)))
                                 objects)))))
               (n-object-keys (length object-keys))
               (query-len (or max-len n-object-keys)))

          (do ((i 0 (+ i query-len)))
              ((>= i n-object-keys))
            (let* ((keys (if max-len
                             (subseq object-keys i (min (+ i query-len) n-object-keys))
                             object-keys))
                   (results (unless (gethash :target-slot dbi)
                              (find-all (list (gethash :join-class dbi))
                                        :where (make-instance 'sql-relational-exp
                                                              :operator 'in
                                                              :sub-expressions (list (sql-expression :attribute foreign-key)
                                                                                     keys))
                                        :result-types :auto
                                        :flatp t)) ))

              (dolist (object objects)
                (when (or force-p (not (slot-boundp object slotdef-name)))
                  (let ((res (if results
                                 (remove-if-not #'(lambda (obj)
                                                    (equal obj (slot-value
                                                                object
                                                                home-key)))
                                                results
                                                :key #'(lambda (res)
                                                         (slot-value res
                                                                     foreign-key)))

                                 (progn
                                   (when (gethash :target-slot dbi)
                                     (fault-join-target-slot class object slotdef))))))
                    (when res
                      (setf (slot-value object slotdef-name)
                            (if (gethash :set dbi) res (car res)))))))))))))
  (values))

(defun fault-join-slot-raw (class object slot-def)
  (let* ((dbi (view-class-slot-db-info slot-def))
         (jc (gethash :join-class dbi)))
    (let ((jq (join-qualifier class object slot-def)))
      (when jq
        (select jc :where jq :flatp t :result-types nil
                :database (view-database object))))))

(defun fault-join-slot (class object slot-def)
  (let* ((dbi (view-class-slot-db-info slot-def))
         (ts (gethash :target-slot dbi)))
    (if (and ts (gethash :set dbi))
        (fault-join-target-slot class object slot-def)
        (let ((res (fault-join-slot-raw class object slot-def)))
          (when res
            (cond
              ((and ts (not (gethash :set dbi)))
               (mapcar (lambda (obj) (slot-value obj ts)) res))
              ((and (not ts) (not (gethash :set dbi)))
               (car res))
              ((and (not ts) (gethash :set dbi))
               res)))))))

;;;; Should we not return the whole result, instead of only
;;;; the one slot-value? We get all the values from the db
;;;; anyway, so?
(defun fault-join-normalized-slot (class object slot-def)
  (labels ((getsc (this-class)
             (let ((sc (car (class-direct-superclasses this-class))))
               (if (key-slots sc)
                   sc
                   (getsc sc)))))
    (let* ((sc (getsc class))
           (hk (slot-definition-name (car (key-slots class))))
           (fk (slot-definition-name (car (key-slots sc)))))
      (let ((jq (sql-operation '==
                               (typecase fk
                                 (symbol
                                  (sql-expression
                                   :attribute
                                   (view-class-slot-column
                                    (slotdef-for-slot-with-class fk sc))
                                   :table (view-table sc)))
                                 (t fk))
                               (typecase hk
                                 (symbol
                                  (slot-value object hk))
                                 (t hk)))))

        ;; Caching nil in next select, because in normalized mode
        ;; records can be changed through other instances (children,
        ;; parents) so changes possibly won't be noticed
        (let ((res (car (select (class-name sc) :where jq
                                                :flatp t :result-types nil
                                                :caching nil
                                                :database (view-database object))))
              (slot-name (slot-definition-name slot-def)))

          ;; If current class is normalized and wanted slot is not
          ;; a direct member, recurse up
          (if (and (normalizedp class)
                   (not (member slot-name
                                (mapcar #'(lambda (esd) (slot-definition-name esd))
                                        (ordered-class-direct-slots class))))
                   (not (slot-boundp res slot-name)))
              (fault-join-normalized-slot sc res slot-def)
              (slot-value res slot-name)))))) )

(defun join-qualifier (class object slot-def)
  (declare (ignore class))
  (let* ((dbi (view-class-slot-db-info slot-def))
         (jc (find-class (gethash :join-class dbi)))
         ;;(ts (gethash :target-slot dbi))
         ;;(tsdef (if ts (slotdef-for-slot-with-class ts jc)))
         (foreign-keys (gethash :foreign-key dbi))
         (home-keys (gethash :home-key dbi)))
    (when (every #'(lambda (slt)
                     (and (slot-boundp object slt)
                          (not (null (slot-value object slt)))))
                 (if (listp home-keys) home-keys (list home-keys)))
      (let ((jc
             (mapcar #'(lambda (hk fk)
                         (let ((fksd (slotdef-for-slot-with-class fk jc)))
                           (sql-operation '==
                                          (typecase fk
                                            (symbol
                                             (sql-expression
                                              :attribute
                                              (view-class-slot-column fksd)
                                              :table (view-table jc)))
                                            (t fk))
                                          (typecase hk
                                            (symbol
                                             (slot-value object hk))
                                            (t
                                             hk)))))
                     (if (listp home-keys)
                         home-keys
                         (list home-keys))
                     (if (listp foreign-keys)
                         foreign-keys
                         (list foreign-keys)))))
        (when jc
          (if (> (length jc) 1)
              (apply #'sql-and jc)
              jc))))))

;; FIXME: add retrieval immediate for efficiency
;; For example, for (select 'employee-address) in test suite =>
;; select addr.*,ea_join.* FROM addr,ea_join WHERE ea_join.aaddressid=addr.addressid\g

(defun build-objects (vals sclasses immediate-join-classes sels immediate-joins database refresh flatp instances)
  "Used by find-all to build objects."
  (labels ((build-object (vals vclass jclasses selects immediate-selects instance)
             (let* ((db-vals (butlast vals (- (list-length vals)
                                              (list-length selects))))
                    (obj (if instance instance (make-instance (class-name vclass) :view-database database)))
                    (join-vals (subseq vals (list-length selects)))
                    (joins (mapcar #'(lambda (c) (when c (make-instance c :view-database database)))
                                   jclasses)))

               ;;(format t "joins: ~S~%db-vals: ~S~%join-values: ~S~%selects: ~S~%immediate-selects: ~S~%"
               ;;joins db-vals join-vals selects immediate-selects)

               ;; use refresh keyword here
               (setf obj (get-slot-values-from-view obj (mapcar #'car selects) db-vals))
               (mapc #'(lambda (jo)
                         ;; find all immediate-select slots and join-vals for this object
                         (let* ((jo-class (class-of jo))
                                (slots
                                 (if (normalizedp jo-class)
                                     (class-direct-slots jo-class)
                                     (class-slots jo-class)))
                                (pos-list (remove-if #'null
                                                     (mapcar
                                                      #'(lambda (s)
                                                          (position s immediate-selects
                                                                    :key #'car
                                                                    :test #'eq))
                                                      slots))))
                           (get-slot-values-from-view jo
                                                      (mapcar #'car
                                                              (mapcar #'(lambda (pos)
                                                                          (nth pos immediate-selects))
                                                                      pos-list))
                                                      (mapcar #'(lambda (pos) (nth pos join-vals))
                                                              pos-list))))
                     joins)
               (mapc
                #'(lambda (jc)
                    (let* ((vslots
                            (class-slots vclass))
                           (slot (find (class-name (class-of jc)) vslots
                                       :key #'(lambda (slot)
                                                (when (and (eq :join (view-class-slot-db-kind slot))
                                                           (eq (slot-definition-name slot)
                                                               (gethash :join-class (view-class-slot-db-info slot))))
                                                  (slot-definition-name slot))))))
                      (when slot
                        (setf (slot-value obj (slot-definition-name slot)) jc))))
                joins)
               (when refresh (instance-refreshed obj))
               obj)))
    (let* ((objects
            (mapcar #'(lambda (sclass jclass sel immediate-join instance)
                        (prog1
                            (build-object vals sclass jclass sel immediate-join instance)
                          (setf vals (nthcdr (+ (list-length sel) (list-length immediate-join))
                                             vals))))
                    sclasses immediate-join-classes sels immediate-joins instances)))
      (if (and flatp (= (length sclasses) 1))
          (car objects)
          objects))))

(defun find-all (view-classes
                 &rest args
                 &key all set-operation distinct from where group-by having
                 order-by offset limit refresh flatp result-types
                 inner-join on
                 (database *default-database*)
                 instances)
  "Called by SELECT to generate object query results when the
  View Classes VIEW-CLASSES are passed as arguments to SELECT."
  (declare (ignore all set-operation group-by having offset limit inner-join on))
  (flet ((ref-equal (ref1 ref2)
           (string= (sql-output ref1 database)
                    (sql-output ref2 database)))
         (table-sql-expr (table)
           (sql-expression :table (view-table table)))
         (tables-equal (table-a table-b)
           (when (and table-a table-b)
             (string= (string (slot-value table-a 'name))
                      (string (slot-value table-b 'name))))))
    (remf args :from)
    (remf args :where)
    (remf args :flatp)
    (remf args :additional-fields)
    (remf args :result-types)
    (remf args :instances)
    (let* ((*db-deserializing* t)
           (sclasses (mapcar #'find-class view-classes))
           (immediate-join-slots
            (mapcar #'(lambda (c) (generate-retrieval-joins-list c :immediate)) sclasses))
           (immediate-join-classes
            (mapcar #'(lambda (jcs)
                        (mapcar #'(lambda (slotdef)
                                    (find-class (gethash :join-class (view-class-slot-db-info slotdef))))
                                jcs))
                    immediate-join-slots))
           (immediate-join-sels (mapcar #'generate-immediate-joins-selection-list sclasses))
           (sels (mapcar #'generate-selection-list sclasses))
           (fullsels (apply #'append (mapcar #'append sels immediate-join-sels)))
           (sel-tables (collect-table-refs where))
           (tables (remove-if #'null
                              (remove-duplicates
                               (append (mapcar #'table-sql-expr sclasses)
                                       (mapcan #'(lambda (jc-list)
                                                   (mapcar
                                                    #'(lambda (jc) (when jc (table-sql-expr jc)))
                                                    jc-list))
                                               immediate-join-classes)
                                       sel-tables)
                               :test #'tables-equal)))
           (order-by-slots (mapcar #'(lambda (ob) (if (atom ob) ob (car ob)))
                                   (listify order-by)))
           (join-where nil))

      ;;(format t "sclasses: ~W~%ijc: ~W~%tables: ~W~%" sclasses immediate-join-classes tables)

      (dolist (ob order-by-slots)
        (when (and ob (not (member ob (mapcar #'cdr fullsels)
                                   :test #'ref-equal)))
          (setq fullsels
                (append fullsels (mapcar #'(lambda (att) (cons nil att))
                                         order-by-slots)))))
      (dolist (ob (listify distinct))
        (when (and (typep ob 'sql-ident)
                   (not (member ob (mapcar #'cdr fullsels)
                                :test #'ref-equal)))
          (setq fullsels
                (append fullsels (mapcar #'(lambda (att) (cons nil att))
                                         (listify ob))))))
      (mapcar #'(lambda (vclass jclasses jslots)
                  (when jclasses
                    (mapcar
                     #'(lambda (jclass jslot)
                         (let ((dbi (view-class-slot-db-info jslot)))
                           (setq join-where
                                 (append
                                  (list (sql-operation '==
                                                       (sql-expression
                                                        :attribute (gethash :foreign-key dbi)
                                                        :table (view-table jclass))
                                                       (sql-expression
                                                        :attribute (gethash :home-key dbi)
                                                        :table (view-table vclass))))
                                  (when join-where (listify join-where))))))
                     jclasses jslots)))
              sclasses immediate-join-classes immediate-join-slots)
      ;; Reported buggy on clsql-devel
      ;; (when where (setq where (listify where)))
      (cond
        ((and where join-where)
         (setq where (list (apply #'sql-and where join-where))))
        ((and (null where) (> (length join-where) 1))
         (setq where (list (apply #'sql-and join-where)))))

      (let* ((rows (apply #'select
                          (append (mapcar #'cdr fullsels)
                                  (cons :from
                                        (list (append (when from (listify from))
                                                      (listify tables))))
                                  (list :result-types result-types)
                                  (when where
                                    (list :where where))
                                  args)))
             (instances-to-add (- (length rows) (length instances)))
             (perhaps-extended-instances
              (if (plusp instances-to-add)
                  (append instances (do ((i 0 (1+ i))
                                         (res nil))
                                        ((= i instances-to-add) res)
                                      (push (make-list (length sclasses) :initial-element nil) res)))
                  instances))
             (objects (mapcar
                       #'(lambda (row instance)
                           (build-objects row sclasses immediate-join-classes sels
                                          immediate-join-sels database refresh flatp
                                          (if (and flatp (atom instance))
                                              (list instance)
                                              instance)))
                       rows perhaps-extended-instances)))
        objects))))

(defmethod instance-refreshed ((instance standard-db-object)))

(defvar *default-caching* t
  "Controls whether SELECT caches objects by default. The CommonSQL
specification states caching is on by default.")

(defun select (&rest select-all-args)
  "Executes a query on DATABASE, which has a default value of
*DEFAULT-DATABASE*, specified by the SQL expressions supplied
using the remaining arguments in SELECT-ALL-ARGS. The SELECT
argument can be used to generate queries in both functional and
object oriented contexts.

In the functional case, the required arguments specify the
columns selected by the query and may be symbolic SQL expressions
or strings representing attribute identifiers. Type modified
identifiers indicate that the values selected from the specified
column are converted to the specified lisp type. The keyword
arguments ALL, DISTINCT, FROM, GROUP-by, HAVING, ORDER-BY,
SET-OPERATION and WHERE are used to specify, using the symbolic
SQL syntax, the corresponding components of the SQL query
generated by the call to SELECT. RESULT-TYPES is a list of
symbols which specifies the lisp type for each field returned by
the query. If RESULT-TYPES is nil all results are returned as
strings whereas the default value of :auto means that the lisp
types are automatically computed for each field. FIELD-NAMES is t
by default which means that the second value returned is a list
of strings representing the columns selected by the query. If
FIELD-NAMES is nil, the list of column names is not returned as a
second value.

In the object oriented case, the required arguments to SELECT are
symbols denoting View Classes which specify the database tables
to query. In this case, SELECT returns a list of View Class
instances whose slots are set from the attribute values of the
records in the specified table. Slot-value is a legal operator
which can be employed as part of the symbolic SQL syntax used in
the WHERE keyword argument to SELECT. REFRESH is nil by default
which means that the View Class instances returned are retrieved
from a cache if an equivalent call to SELECT has previously been
issued. If REFRESH is true, the View Class instances returned are
updated as necessary from the database and the generic function
INSTANCE-REFRESHED is called to perform any necessary operations
on the updated instances.

In both object oriented and functional contexts, FLATP has a
default value of nil which means that the results are returned as
a list of lists. If FLATP is t and only one result is returned
for each record selected in the query, the results are returned
as elements of a list."

  (flet ((select-objects (target-args)
           (and target-args
                (every #'(lambda (arg)
                           (and (symbolp arg)
                                (find-class arg nil)))
                       target-args))))
    (multiple-value-bind (target-args qualifier-args)
        (query-get-selections select-all-args)
      (unless (or *default-database* (getf qualifier-args :database))
        (signal-no-database-error nil))

      (cond
        ((select-objects target-args)
         (let ((caching (getf qualifier-args :caching *default-caching*))
               (result-types (getf qualifier-args :result-types :auto))
               (refresh (getf qualifier-args :refresh nil))
               (database (or (getf qualifier-args :database) *default-database*))
               (order-by (getf qualifier-args :order-by)))
           (remf qualifier-args :caching)
           (remf qualifier-args :refresh)
           (remf qualifier-args :result-types)

           ;; Add explicity table name to order-by if not specified and only
           ;; one selected table. This is required so FIND-ALL won't duplicate
           ;; the field
           (when (and order-by (= 1 (length target-args)))
             (let ((table-name (view-table (find-class (car target-args))))
                   (order-by-list (copy-seq (listify order-by))))
               (labels ((set-table-if-needed (val)
                          (typecase val
                            (sql-ident-attribute
                             (handler-case
                                 (unless (slot-value val 'qualifier)
                                   (setf (slot-value val 'qualifier) table-name))
                               (simple-error ()
                                 ;; TODO: Check for a specific error we expect
                                 )))
                            (cons (set-table-if-needed (car val))))))
                 (loop for i from 0 below (length order-by-list)
                       for id = (nth i order-by-list)
                       do (set-table-if-needed id)))
               (setf (getf qualifier-args :order-by) order-by-list)))

           (cond
             ((null caching)
              (apply #'find-all target-args
                     (append qualifier-args
                             (list :result-types result-types :refresh refresh))))
             (t
              (let ((cached (records-cache-results target-args qualifier-args database)))
                (cond
                  ((and cached (not refresh))
                   cached)
                  ((and cached refresh)
                   (let ((results (apply #'find-all (append (list target-args) qualifier-args `(:instances ,cached :result-types :auto :refresh ,refresh)))))
                     (setf (records-cache-results target-args qualifier-args database) results)
                     results))
                  (t
                   (let ((results (apply #'find-all target-args (append qualifier-args
                                                                        `(:result-types :auto :refresh ,refresh)))))
                     (setf (records-cache-results target-args qualifier-args database) results)
                     results))))))))
        (t
         (let* ((expr (apply #'make-query select-all-args))
                (specified-types
                 (mapcar #'(lambda (attrib)
                             (if (typep attrib 'sql-ident-attribute)
                                 (let ((type (slot-value attrib 'type)))
                                   (if type
                                       type
                                       t))
                                 t))
                         (slot-value expr 'selections))))
           (destructuring-bind (&key (flatp nil)
                                     (result-types :auto)
                                     (field-names t)
                                     (database *default-database*)
                                     &allow-other-keys)
               qualifier-args
             (query expr :flatp flatp
                    :result-types
                    ;; specifying a type for an attribute overrides result-types
                    (if (some #'(lambda (x) (not (eq t x))) specified-types)
                        specified-types
                        result-types)
                    :field-names field-names
                    :database database))))))))

(defun compute-records-cache-key (targets qualifiers)
  (list targets
        (do ((args *select-arguments* (cdr args))
             (results nil))
            ((null args) results)
          (let* ((arg (car args))
                 (value (getf qualifiers arg)))
            (when value
              (push (list arg
                          (typecase value
                            (cons (cons (sql (car value)) (cdr value)))
                            (%sql-expression (sql value))
                            (t value)))
                    results))))))

(defun records-cache-results (targets qualifiers database)
  (when (record-caches database)
    (gethash (compute-records-cache-key targets qualifiers) (record-caches database))))

(defun (setf records-cache-results) (results targets qualifiers database)
  (unless (record-caches database)
    (setf (record-caches database)
          (make-hash-table :test 'equal
                           #+allegro   :values    #+allegro :weak
                           #+clisp     :weak      #+clisp :value
                           #+lispworks :weak-kind #+lispworks :value)))
  (setf (gethash (compute-records-cache-key targets qualifiers)
                 (record-caches database)) results)
  results)



;;; Serialization functions

(defun write-instance-to-stream (obj stream)
  "Writes an instance to a stream where it can be later be read.
NOTE: an error will occur if a slot holds a value which can not be written readably."
  (let* ((class (class-of obj))
         (alist '()))
    (dolist (slot (ordered-class-slots (class-of obj)))
      (let ((name (slot-definition-name slot)))
        (when (and (not (eq 'view-database name))
                   (slot-boundp obj name))
          (push (cons name (slot-value obj name)) alist))))
    (setq alist (reverse alist))
    (write (cons (class-name class) alist) :stream stream :readably t))
  obj)

(defun read-instance-from-stream (stream)
  (let ((raw (read stream nil nil)))
    (when raw
      (let ((obj (make-instance (car raw))))
        (dolist (pair (cdr raw))
          (setf (slot-value obj (car pair)) (cdr pair)))
        obj))))
