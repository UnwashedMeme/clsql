;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; The CLSQL Object Oriented Data Definitional Language (OODDL)
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:clsql-sys)

(defclass standard-db-object ()
  ((view-database :initform nil :initarg :view-database :reader view-database
                  :db-kind :virtual))
  (:metaclass standard-db-class)
  (:documentation "Superclass for all CLSQL View Classes."))

(defparameter *default-string-length* 255
  "The length of a string which does not have a user-specified length.")

(defvar *db-auto-sync* nil
  "A non-nil value means that creating View Class instances or
  setting their slots automatically creates/updates the
  corresponding records in the underlying database.")

(defvar *db-deserializing* nil)
(defvar *db-initializing* nil)

(defmethod slot-value-using-class ((class standard-db-class) instance slot-def)
  (declare (optimize (speed 3)))
  (unless *db-deserializing*
    (let* ((slot-name (%svuc-slot-name slot-def))
           (slot-object (%svuc-slot-object slot-def class))
           (slot-kind (view-class-slot-db-kind slot-object)))
      (if (and (eql slot-kind :join)
               (not (slot-boundp instance slot-name)))
          (let ((*db-deserializing* t))
            (if (view-database instance)
                (setf (slot-value instance slot-name)
                      (fault-join-slot class instance slot-object))
                (setf (slot-value instance slot-name) nil)))
          (when (and (normalizedp class)
                     (not (member slot-name
                                  (mapcar #'(lambda (esd) (slot-definition-name esd))
                                          (ordered-class-direct-slots class))))
                     (not (slot-boundp instance slot-name)))
            (let ((*db-deserializing* t))
              (if (view-database instance)
                  (setf (slot-value instance slot-name)
                        (fault-join-normalized-slot class instance slot-object))
                  (setf (slot-value instance slot-name) nil)))))))
  (call-next-method))

(defmethod (setf slot-value-using-class) (new-value (class standard-db-class)
                                          instance slot-def)
  (declare (ignore new-value))
  (let* ((slot-name (%svuc-slot-name slot-def))
         (slot-object (%svuc-slot-object slot-def class))
         (slot-kind (view-class-slot-db-kind slot-object)))
    (prog1
        (call-next-method)
      (when (and *db-auto-sync*
                 (not *db-initializing*)
                 (not *db-deserializing*)
                 (not (eql slot-kind :virtual)))
        (update-record-from-slot instance slot-name)))))

(defmethod initialize-instance ((object standard-db-object)
                                &rest all-keys &key &allow-other-keys)
  (declare (ignore all-keys))
  (let ((*db-initializing* t))
    (call-next-method)
    (when (and *db-auto-sync*
               (not *db-deserializing*))
      (update-records-from-instance object))))

;;
;; Build the database tables required to store the given view class
;;

(defun create-view-from-class (view-class-name
                               &key (database *default-database*)
                               (transactions t))
  "Creates a table as defined by the View Class VIEW-CLASS-NAME
in DATABASE which defaults to *DEFAULT-DATABASE*."
  (let ((tclass (find-class view-class-name)))
    (if tclass
        (let ((*default-database* database)
              (pclass (car (class-direct-superclasses tclass))))
          (when (and (normalizedp tclass) (not (table-exists-p pclass)))
            (create-view-from-class (class-name pclass)
                                    :database database :transactions transactions))
          (%install-class tclass database :transactions transactions))
        (error "Class ~s not found." view-class-name)))
  (values))

(defmethod auto-increment-column-p (slotdef &optional (database clsql-sys:*default-database*))
  (declare (ignore database))
  (or (member :auto-increment (listify (view-class-slot-db-constraints slotdef)))
      (slot-value slotdef 'autoincrement-sequence)))

(defmethod %install-class ((self standard-db-class) database
                           &key (transactions t))
  (let ((schemadef '())
        (ordered-slots (if (normalizedp self)
                           (ordered-class-direct-slots self)
                           (ordered-class-slots self))))
    (dolist (slotdef ordered-slots)
      (let ((res (database-generate-column-definition self slotdef database)))
        (when res
          (push res schemadef))))
    (if (not schemadef)
        (unless (normalizedp self)
          (error "Class ~s has no :base slots" self))
        (progn
          (database-add-autoincrement-sequence self database)
          (create-table (sql-expression :table (database-identifier self database))
                        (nreverse schemadef)
                        :database database
                        :transactions transactions
                        :constraints (database-pkey-constraint self database))
          (push self (database-view-classes database)))))
  t)

(defmethod database-pkey-constraint ((class standard-db-class) database)
  ;; Keylist will always be a list of escaped-indentifier
  (let ((keylist (mapcar #'(lambda (x) (escaped-database-identifier x database))
                         (keyslots-for-class class)))
        (table (escaped (combine-database-identifiers
                         (list class 'PK)
                         database))))
    (when keylist
      (format nil "CONSTRAINT ~A PRIMARY KEY (~{~A~^,~})" table
              keylist))))

(defmethod database-generate-column-definition (class slotdef database)
  (declare (ignore class))
  (when (member (view-class-slot-db-kind slotdef) '(:base :key))
    (let ((cdef
           (list (sql-expression :attribute (database-identifier slotdef database))
                 (specified-type slotdef))))
      (setf cdef (append cdef (list (view-class-slot-db-type slotdef))))
      (let ((const (view-class-slot-db-constraints slotdef)))
        (when const
          (setq cdef (append cdef (listify const)))))
      cdef)))


;;
;; Drop the tables which store the given view class
;;

(defun drop-view-from-class (view-class-name &key (database *default-database*)
                             (owner nil))
  "Removes a table defined by the View Class VIEW-CLASS-NAME from
DATABASE which defaults to *DEFAULT-DATABASE*."
  (let ((tclass (find-class view-class-name)))
    (if tclass
        (let ((*default-database* database))
          (%uninstall-class tclass :owner owner))
        (error "Class ~s not found." view-class-name)))
  (values))

(defun %uninstall-class (self &key
                         (database *default-database*)
                         (owner nil))
  (drop-table (sql-expression :table (database-identifier self database))
              :if-does-not-exist :ignore
              :database database
              :owner owner)
  (database-remove-autoincrement-sequence self database)
  (setf (database-view-classes database)
        (remove self (database-view-classes database))))


;;
;; List all known view classes
;;

(defun list-classes (&key (test #'identity)
                     (root-class (find-class 'standard-db-object))
                     (database *default-database*))
  "Returns a list of all the View Classes which are connected to
DATABASE, which defaults to *DEFAULT-DATABASE*, and which descend
from the class ROOT-CLASS and which satisfy the function TEST. By
default ROOT-CLASS is STANDARD-DB-OBJECT and TEST is IDENTITY."
  (flet ((find-superclass (class)
           (member root-class (class-precedence-list class))))
    (let ((view-classes (and database (database-view-classes database))))
      (when view-classes
        (remove-if #'(lambda (c) (or (not (funcall test c))
                                     (not (find-superclass c))))
                   view-classes)))))

;;
;; Define a new view class
;;

(defmacro def-view-class (class supers slots &rest cl-options)
  "Creates a View Class called CLASS whose slots SLOTS can map
onto the attributes of a table in a database. If SUPERS is nil
then the superclass of CLASS will be STANDARD-DB-OBJECT,
otherwise SUPERS is a list of superclasses for CLASS which must
include STANDARD-DB-OBJECT or a descendent of this class. The
syntax of DEFCLASS is extended through the addition of a class
option :base-table which defines the database table onto which
the View Class maps and which defaults to CLASS. The DEFCLASS
syntax is also extended through additional slot
options. The :db-kind slot option specifies the kind of DB
mapping which is performed for this slot and defaults to :base
which indicates that the slot maps to an ordinary column of the
database table. A :db-kind value of :key indicates that this slot
is a special kind of :base slot which maps onto a column which is
one of the unique keys for the database table, the value :join
indicates this slot represents a join onto another View Class
which contains View Class objects, and the value :virtual
indicates a standard CLOS slot which does not map onto columns of
the database table. If a slot is specified with :db-kind :join,
the slot option :db-info contains a list which specifies the
nature of the join. For slots of :db-kind :base or :key,
the :type slot option has a special interpretation such that Lisp
types, such as string, integer and float are automatically
converted into appropriate SQL types for the column onto which
the slot maps. This behaviour may be over-ridden using
the :db-type slot option which is a string specifying the
vendor-specific database type for this slot's column definition
in the database. The :column slot option specifies the name of
the SQL column which the slot maps onto, if :db-kind is
not :virtual, and defaults to the slot name. The :void-value slot
option specifies the value to store if the SQL value is NULL and
defaults to NIL. The :db-constraints slot option is a string
representing an SQL table constraint expression or a list of such
strings."
  `(progn
     (defclass ,class ,supers ,slots
       ,@(if (find :metaclass `,cl-options :key #'car)
             `,cl-options
             (cons '(:metaclass clsql-sys::standard-db-class) `,cl-options)))
     (finalize-inheritance (find-class ',class))
     (find-class ',class)))

(defun keyslots-for-class (class)
  (slot-value class 'key-slots))
