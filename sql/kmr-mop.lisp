;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          kmr-mop.lisp
;;;; Purpose:       MOP support for multiple-implementions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; This file imports MOP symbols into the CLSQL-MOP package and then
;;;; re-exports into CLSQL-SYS them to hide differences in
;;;; MOP implementations.
;;;;
;;;; This file was extracted from the KMRCL utilities
;;;; *************************************************************************

(in-package #:clsql-sys)

#+lispworks
(defun intern-eql-specializer (slot)
  `(eql ,slot))

(defmacro process-class-option (metaclass slot-name &optional required)
  #+lispworks
  `(defmethod clos:process-a-class-option ((class ,metaclass)
                                           (name (eql ,slot-name))
                                           value)
    (when (and ,required (null value))
      (error "metaclass ~A class slot ~A must have a value" (quote ,metaclass) name))
    (list name `',value))
  #-lispworks
    (declare (ignore metaclass slot-name required))
    )

(defmacro process-slot-option (metaclass slot-name)
  #+lispworks
  `(defmethod clos:process-a-slot-option ((class ,metaclass)
                                          (option (eql ,slot-name))
                                          value
                                          already-processed-options
                                          slot)
    (list* option `',value already-processed-options))
  #-lispworks
  (declare (ignore metaclass slot-name))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass %slot-order-test-class ()
    ((a)
     (b)))
  (finalize-inheritance (find-class '%slot-order-test-class))
  (let ((slots (class-slots (find-class '%slot-order-test-class))))
    (ecase (slot-definition-name (first slots))
      (a)
      (b (pushnew :mop-slot-order-reversed cl:*features*)))))

(defun ordered-class-slots (class)
  #+mop-slot-order-reversed (reverse (class-slots class))
  #-mop-slot-order-reversed (class-slots class))

(defun ordered-class-direct-slots (class)
  "Gets an ordered list of direct class slots"
  ;; NB: this used to return effective-slot-definitions in direct
  ;; opposition to the function name.  Not sure why
  (setf class (to-class class))
  #+mop-slot-order-reversed (reverse (class-direct-slots class))
  #-mop-slot-order-reversed (class-direct-slots class))

(defun find-slot-if (class predicate &optional direct? recurse?)
  "Looks up a direct-slot-definition by name"
  (setf class (to-class class))
  (labels ((find-it (class)
             (let* ((slots (if direct?
                               (ordered-class-direct-slots class)
                               (ordered-class-slots class)))
                    (it (find-if predicate slots)))
               (or it
                   (when recurse?
                     (loop for sup in (class-direct-superclasses class)
                           for rtn = (find-it sup)
                           until rtn
                           finally (return rtn)))))))
    (find-it class)))

(defun find-slot-by-name (class slot-name &optional direct? recurse?)
  "Looks up a direct-slot-definition by name"
  (setf class (to-class class)
        slot-name (to-slot-name slot-name))
  (find-slot-if class (lambda (slot-def) (eql (to-slot-name slot-def) slot-name))
                direct? recurse?))

;; Lispworks has symbol for slot rather than the slot instance
(defun %svuc-slot-name (slot)
  #+lispworks slot
  #-lispworks (slot-definition-name slot))

(defun %svuc-slot-object (slot class)
  (declare (ignorable class))
  #+lispworks (clos:find-slot-definition slot class)
  #-lispworks slot)

