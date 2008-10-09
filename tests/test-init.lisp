;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-init.lisp
;;;; Authors: Marcus Pearce <m.t.pearce@city.ac.uk>, Kevin Rosenberg
;;;; Created: 30/03/2004
;;;; Updated: $Id$
;;;;
;;;; Initialisation utilities for running regression tests on CLSQL.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

(defvar *report-stream* *standard-output* "Stream to send text report.")
(defvar *sexp-report-stream* nil "Stream to send sexp report.")
(defvar *rt-internal*)
(defvar *rt-basic*)
(defvar *rt-connection*)
(defvar *rt-fddl*)
(defvar *rt-fdml*)
(defvar *rt-ooddl*)
(defvar *rt-oodml*)
(defvar *rt-syntax*)
(defvar *rt-time*)

(defvar *test-database-type* nil)
(defvar *test-database-underlying-type* nil)
(defvar *test-database-user* nil)
(defvar *test-start-utime* nil)
(defvar *test-connection-spec* nil)
(defvar *test-connection-db-type* nil)

(defclass thing ()
  ((extraterrestrial :initform nil :initarg :extraterrestrial)))

(def-view-class person (thing)
  ((height :db-kind :base :accessor height :type float
           :initarg :height)
   (married :db-kind :base :accessor married :type boolean
            :initarg :married)
   (birthday :type clsql:wall-time :initarg :birthday)
   (bd-utime :type clsql:universal-time :initarg :bd-utime)
   (hobby :db-kind :virtual :initarg :hobby :initform nil)))

(def-view-class employee (person)
  ((emplid
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :emplid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (first-name
    :accessor first-name
    :type (varchar 30)
    :initarg :first-name)
   (last-name
    :accessor last-name
    :type (varchar 30)
    :initarg :last-name)
   (email
    :accessor employee-email
    :type (varchar 100)
    :initarg :email)
   (ecompanyid
    :type integer
    :initarg :companyid)
   (company
    :accessor employee-company
    :db-kind :join
    :db-info (:join-class company
                          :home-key ecompanyid
                          :foreign-key companyid
                          :set nil))
   (managerid
    :type integer
    :initarg :managerid)
   (manager
    :accessor employee-manager
    :db-kind :join
    :db-info (:join-class employee
                          :home-key managerid
                          :foreign-key emplid
                          :set nil))
   (addresses
    :accessor employee-addresses
    :db-kind :join
    :db-info (:join-class employee-address
                          :home-key emplid
                          :foreign-key aemplid
                          :target-slot address
                          :set t)))
  (:base-table employee))

(def-view-class company ()
  ((companyid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :companyid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (name
    :type (varchar 100)
    :initarg :name)
   (presidentid
    :type integer
    :initarg :presidentid)
   (president
    :reader president
    :db-kind :join
    :db-info (:join-class employee
                          :home-key presidentid
                          :foreign-key emplid
                          :set nil))
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class employee
                          :home-key (companyid groupid)
                          :foreign-key (ecompanyid groupid)
                          :set t))))

(def-view-class address ()
  ((addressid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :addressid)
   (street-number
    :type integer
    :initarg :street-number)
   (street-name
    :type (varchar 30)
    :void-value ""
    :initarg :street-name)
   (city
    :column "city_field"
    :void-value "no city"
    :type (varchar 30)
    :initarg :city)
   (postal-code
    :column zip
    :type integer
    :void-value 0
    :initarg :postal-code))
  (:base-table addr))

;; many employees can reside at many addressess
(def-view-class employee-address ()
  ((aemplid :type integer :initarg :emplid)
   (aaddressid :type integer :initarg :addressid)
   (verified :type boolean :initarg :verified)
   (address :db-kind :join
            :db-info (:join-class address
                                  :home-key aaddressid
                                  :foreign-key addressid
                                  :retrieval :immediate))
   (employee :db-kind :join
             :db-info (:join-class employee
                                  :home-key aemplid
                                  :foreign-key emplid
                                  :retrieval :immediate)))
  (:base-table "ea_join"))

(def-view-class deferred-employee-address ()
  ((aemplid :type integer :initarg :emplid)
   (aaddressid :type integer :initarg :addressid)
   (verified :type boolean :initarg :verified)
   (address :db-kind :join
            :db-info (:join-class address
                                  :home-key aaddressid
                                  :foreign-key addressid
                                  :retrieval :deferred
                                  :set nil)))
  (:base-table "ea_join"))

(def-view-class big ()
  ((i :type integer :initarg :i)
   (bi :type bigint :initarg :bi)))

(defun test-connect-to-database (db-type spec)
  (when (clsql-sys:db-backend-has-create/destroy-db? db-type)
    (ignore-errors (destroy-database spec :database-type db-type))
    (ignore-errors (create-database spec :database-type db-type)))

  (setf *test-database-type* db-type)
  (setf *test-database-user*
    (cond
     ((eq :oracle db-type) (second spec))
     ((>= (length spec) 3) (third spec))))

  ;; Connect to the database
  (clsql:connect spec
                 :database-type db-type
                 :make-default t
                 :if-exists :old)

  ;; Ensure database is empty
  (truncate-database :database *default-database*)

  (setf *test-database-underlying-type*
        (clsql-sys:database-underlying-type *default-database*))

  *default-database*)

(defparameter company1 nil)
(defparameter employee1 nil)
(defparameter employee2 nil)
(defparameter employee3 nil)
(defparameter employee4 nil)
(defparameter employee5 nil)
(defparameter employee6 nil)
(defparameter employee7 nil)
(defparameter employee8 nil)
(defparameter employee9 nil)
(defparameter employee10 nil)
(defparameter address1 nil)
(defparameter address2 nil)
(defparameter employee-address1 nil)
(defparameter employee-address2 nil)
(defparameter employee-address3 nil)
(defparameter employee-address4 nil)
(defparameter employee-address5 nil)

(defun test-initialise-database ()
  (test-basic-initialize)
  (let ((*backend-warning-behavior*
         (if (member *test-database-type* '(:postgresql :postgresql-socket))
             :ignore
           :warn)))
    (clsql:create-view-from-class 'employee)
    (clsql:create-view-from-class 'company)
    (clsql:create-view-from-class 'address)
    (clsql:create-view-from-class 'employee-address)
    (clsql:create-view-from-class 'big))

  (setq *test-start-utime* (get-universal-time))
  (let* ((*db-auto-sync* t)
         (now-time (clsql:utime->time *test-start-utime*)))
    (setf company1 (make-instance 'company
                                  :presidentid 1
                                  :companyid 1
                                  :groupid 1
                                  :name "Widgets Inc.")
          employee1 (make-instance 'employee
                                   :emplid 1
                                   :groupid 1
                                   :married t
                                   :height (1+ (random 1.00))
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Vladimir"
                                   :last-name "Lenin"
                                   :email "lenin@soviet.org"
                                   :companyid 1)
          employee2 (make-instance 'employee
                                   :emplid 2
                                   :groupid 1
                                   :height (1+ (random 1.00))
                                   :married t
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Josef"
                                   :last-name "Stalin"
                                   :email "stalin@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee3 (make-instance 'employee
                                   :emplid 3
                                   :groupid 1
                                   :height (1+ (random 1.00))
                                   :married t
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Leon"
                                   :last-name "Trotsky"
                                   :email "trotsky@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee4 (make-instance 'employee
                                   :emplid 4
                                   :groupid 1
                                   :height (1+ (random 1.00))
                                   :married nil
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Nikita"
                                   :last-name "Kruschev"
                                   :email "kruschev@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee5 (make-instance 'employee
                                   :emplid 5
                                   :groupid 1
                                   :married nil
                                   :height (1+ (random 1.00))
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Leonid"
                                   :last-name "Brezhnev"
                                   :email "brezhnev@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee6 (make-instance 'employee
                                   :emplid 6
                                   :groupid 1
                                   :married nil
                                   :height (1+ (random 1.00))
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Yuri"
                                   :last-name "Andropov"
                                   :email "andropov@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee7 (make-instance 'employee
                                   :emplid 7
                                   :groupid 1
                                   :height (1+ (random 1.00))
                                   :married nil
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Konstantin"
                                   :last-name "Chernenko"
                                   :email "chernenko@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee8 (make-instance 'employee
                                   :emplid 8
                                   :groupid 1
                                   :height (1+ (random 1.00))
                                   :married nil
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Mikhail"
                                   :last-name "Gorbachev"
                                   :email "gorbachev@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee9 (make-instance 'employee
                                   :emplid 9
                                   :groupid 1
                                   :married nil
                                   :height (1+ (random 1.00))
                                   :bd-utime *test-start-utime*
                                   :birthday now-time
                                   :first-name "Boris"
                                   :last-name "Yeltsin"
                                   :email "yeltsin@soviet.org"
                                   :managerid 1
                                   :companyid 1)
          employee10 (make-instance 'employee
                                    :emplid 10
                                    :groupid 1
                                    :married nil
                                    :height (1+ (random 1.00))
                                    :bd-utime *test-start-utime*
                                    :birthday now-time
                                    :first-name "Vladimir"
                                    :last-name "Putin"
                                    :email "putin@soviet.org"
                                    :managerid 1
                                    :companyid 1)
          address1 (make-instance 'address
                                  :addressid 1
                                  :street-number 10
                                  :street-name "Park Place"
                                  :city "Leningrad"
                                  :postal-code 123)
          address2 (make-instance 'address
                                  :addressid 2)
          employee-address1 (make-instance 'employee-address
                                           :emplid 1
                                           :addressid 1
                                           :verified t)
          employee-address2 (make-instance 'employee-address
                                           :emplid 2
                                           :addressid 2
                                           :verified t)
          employee-address3 (make-instance 'employee-address
                                           :emplid 3
                                           :addressid 1
                                           :verified nil)
          employee-address4 (make-instance 'employee-address
                                           :emplid 1
                                           :addressid 2
                                           :verified nil)
          employee-address5 (make-instance 'employee-address
                                           :emplid 3
                                           :addressid 2))

    (let ((max (expt 2 60)))
      (dotimes (i 555)
        (make-instance 'big :i (1+ i) :bi (truncate max (1+ i))))))

  ;; sleep to ensure birthdays are no longer at current time
  (sleep 1)

  #||
  ;; Lenin manages everyone
  (clsql:add-to-relation employee2 'manager employee1)
  (clsql:add-to-relation employee3 'manager employee1)
  (clsql:add-to-relation employee4 'manager employee1)
  (clsql:add-to-relation employee5 'manager employee1)
  (clsql:add-to-relation employee6 'manager employee1)
  (clsql:add-to-relation employee7 'manager employee1)
  (clsql:add-to-relation employee8 'manager employee1)
  (clsql:add-to-relation employee9 'manager employee1)
  (clsql:add-to-relation employee10 'manager employee1)
  ;; Everyone works for Widgets Inc.
  (clsql:add-to-relation company1 'employees employee1)
  (clsql:add-to-relation company1 'employees employee2)
  (clsql:add-to-relation company1 'employees employee3)
  (clsql:add-to-relation company1 'employees employee4)
  (clsql:add-to-relation company1 'employees employee5)
  (clsql:add-to-relation company1 'employees employee6)
  (clsql:add-to-relation company1 'employees employee7)
  (clsql:add-to-relation company1 'employees employee8)
  (clsql:add-to-relation company1 'employees employee9)
  (clsql:add-to-relation company1 'employees employee10)
  ;; Lenin is president of Widgets Inc.
  (clsql:add-to-relation company1 'president employee1)
  ||#

  ;; store these instances
  #||
  (clsql:update-records-from-instance employee1)
  (clsql:update-records-from-instance employee2)
  (clsql:update-records-from-instance employee3)
  (clsql:update-records-from-instance employee4)
  (clsql:update-records-from-instance employee5)
  (clsql:update-records-from-instance employee6)
  (clsql:update-records-from-instance employee7)
  (clsql:update-records-from-instance employee8)
  (clsql:update-records-from-instance employee9)
  (clsql:update-records-from-instance employee10)
  (clsql:update-records-from-instance company1)
  (clsql:update-records-from-instance address1)
  (clsql:update-records-from-instance address2)
  ||#
  )

(defvar *error-count* 0)
(defvar *error-list* nil)

(defun run-function-append-report-file (function report-file)
    (let* ((report-path (etypecase report-file
                        (pathname report-file)
                        (string (parse-namestring report-file))))
         (sexp-report-path (make-pathname :defaults report-path
                                          :type "sexp")))
      (with-open-file (rs report-path :direction :output
                          :if-exists :append
                      :if-does-not-exist :create)
        (with-open-file (srs sexp-report-path :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (funcall function :report-stream rs :sexp-report-stream srs)))))

(defun run-tests-append-report-file (report-file)
  (run-function-append-report-file 'run-tests report-file))


(defun run-tests (&key (report-stream *standard-output*) (sexp-report-stream nil))
  ;; clear SQL-OUTPUT cache
  (setq clsql-sys::*output-hash* (make-hash-table :test #'equal))
  (let ((specs (read-specs))
        (*report-stream* report-stream)
        (*sexp-report-stream* sexp-report-stream)
        (*error-count* 0)
        (*error-list* nil))
    (unless specs
      (warn "Not running tests because test configuration file is missing")
      (return-from run-tests :skipped))
    (load-necessary-systems specs)
    (dolist (db-type +all-db-types+)
      (dolist (spec (db-type-spec db-type specs))
        (let ((*test-connection-spec* spec)
              (*test-connection-db-type* db-type))
          (do-tests-for-backend db-type spec)))))
  (zerop *error-count*))

(defun load-necessary-systems (specs)
  (dolist (db-type +all-db-types+)
    (when (db-type-spec db-type specs)
      (clsql-sys:initialize-database-type :database-type db-type))))

(defun write-report-banner (report-type db-type stream)
  (format stream
          "~&
******************************************************************************
***     CLSQL ~A begun at ~A
***     ~A
***     ~A on ~A
***     Database ~:@(~A~) backend~A.
******************************************************************************
"
          report-type
          (clsql:format-time
           nil
           (clsql:utime->time (get-universal-time)))
          (lisp-implementation-type)
          (lisp-implementation-version)
          (machine-type)
          db-type
          (if (not (eq db-type *test-database-underlying-type*))
              (format nil " with underlying type ~:@(~A~)"
                      *test-database-underlying-type*)
              "")
          ))

(defun do-tests-for-backend (db-type spec)
  (test-connect-to-database db-type spec)

  (unwind-protect
       (multiple-value-bind (test-forms skip-tests)
           (compute-tests-for-backend db-type *test-database-underlying-type*)

           (write-report-banner "Test Suite" db-type *report-stream*)

           (test-initialise-database)

           (regression-test:rem-all-tests)
           (dolist (test-form test-forms)
             (eval test-form))

           (let ((remaining (regression-test:do-tests *report-stream*)))
             (when (regression-test:pending-tests)
               (incf *error-count* (length remaining))))

           (let ((sexp-error (list db-type
                                   *test-database-underlying-type*
                                   (get-universal-time)
                                   (length test-forms)
                                   (regression-test:pending-tests)
                                   (lisp-implementation-type)
                                   (lisp-implementation-version)
                                   (machine-type))))
             (when *sexp-report-stream*
               (write sexp-error :stream *sexp-report-stream* :readably t))
             (push sexp-error *error-list*))

           (format *report-stream* "~&Tests skipped:")
           (if skip-tests
               (dolist (skipped skip-tests)
                 (format *report-stream*
                         "~&   ~20A ~A~%" (car skipped) (cdr skipped)))
               (format *report-stream* " None~%")))
    (disconnect)))


(defun compute-tests-for-backend (db-type db-underlying-type)
  (let ((test-forms '())
        (skip-tests '()))
    (dolist (test-form (append *rt-internal* *rt-connection* *rt-basic* *rt-fddl* *rt-fdml*
                               *rt-ooddl* *rt-oodml* *rt-syntax* *rt-time*
			       ))
      (let ((test (second test-form)))
        (cond
          ((and (null (clsql-sys:db-type-has-views? db-underlying-type))
                (clsql-sys:in test :fddl/view/1 :fddl/view/2 :fddl/view/3 :fddl/view/4))
           (push (cons test "views not supported") skip-tests))
          ((and (null (clsql-sys:db-type-has-boolean-where? db-underlying-type))
                (clsql-sys:in test :fdml/select/11 :oodml/select/5))
           (push (cons test "boolean where not supported") skip-tests))
          ((and (null (clsql-sys:db-type-has-subqueries? db-underlying-type))
                (clsql-sys:in test :fdml/select/5 :fdml/select/10
                              :fdml/select/32 :fdml/select/33))
           (push (cons test "subqueries not supported") skip-tests))
          ((and (null (clsql-sys:db-type-transaction-capable? db-underlying-type
                                                    *default-database*))
                (clsql-sys:in test :fdml/transaction/1 :fdml/transaction/2 :fdml/transaction/3 :fdml/transaction/4))
           (push (cons test "transactions not supported") skip-tests))
          ((and (null (clsql-sys:db-type-has-fancy-math? db-underlying-type))
                (clsql-sys:in test :fdml/select/1))
           (push (cons test "fancy math not supported") skip-tests))
          ((and (eql *test-database-type* :sqlite)
                (clsql-sys:in test :fddl/view/4 :fdml/select/10
                                :fdml/select/21 :fdml/select/32
                                :fdml/select/33))
           (push (cons test "not supported by sqlite") skip-tests))
          ((and (eql *test-database-type* :sqlite3)
                (clsql-sys:in test :fddl/view/4 :fdml/select/10
                              :fdml/select/21 :fdml/select/32
                              :fdml/select/33))
           (push (cons test "not supported by sqlite3") skip-tests))
          ((and (not (clsql-sys:db-type-has-bigint? db-type))
                (clsql-sys:in test :basic/bigint/1))
           (push (cons test "bigint not supported") skip-tests))
          ((and (eql *test-database-underlying-type* :mysql)
                (clsql-sys:in test :fdml/select/26))
           (push (cons test "string table aliases not supported on all mysql versions") skip-tests))
          ((and (eql *test-database-underlying-type* :mysql)
                (clsql-sys:in test :fdml/select/22 :fdml/query/5
                                :fdml/query/7 :fdml/query/8))
           (push (cons test "not supported by mysql") skip-tests))
          ((and (null (clsql-sys:db-type-has-union? db-underlying-type))
                (clsql-sys:in test :fdml/query/6 :fdml/select/31))
           (push (cons test "union not supported") skip-tests))
          ((and (eq *test-database-type* :oracle)
                (clsql-sys:in test :fdml/query/8 :fdml/select/21
                              :fddl/table/6))
           (push (cons test "syntax not supported") skip-tests))
          ((and (eq *test-database-type* :odbc)
                (eq *test-database-underlying-type* :postgresql)
                (clsql-sys:in test :fddl/owner/1))
           (push (cons test "table ownership not supported by postgresql odbc driver") skip-tests))
          ((and (not (member *test-database-underlying-type*
                             '(:postgresql :oracle)))
                (clsql-sys:in test :fddl/owner/1))
           (push (cons test "table ownership not supported") skip-tests))
          ((and (null (clsql-sys:db-type-has-intersect? db-underlying-type))
                (clsql-sys:in test :fdml/query/7))
           (push (cons test "intersect not supported") skip-tests))
          ((and (null (clsql-sys:db-type-has-except? db-underlying-type))
                (clsql-sys:in test :fdml/query/8))
           (push (cons test "except not supported") skip-tests))
          ((and (eq *test-database-underlying-type* :mssql)
                (clsql-sys:in test :fdml/select/9))
           (push (cons test "mssql uses integer math for AVG") skip-tests))
          ((and (not (member *test-database-underlying-type*
                             '(:postgresql :mysql :sqlite3)))
                (clsql-sys:in test :fdml/select/37 :fdml/select/38))
           (push (cons test "LIMIT keyword not supported in SELECT") skip-tests))
          (t
           (push test-form test-forms)))))
      (values (nreverse test-forms) (nreverse skip-tests))))

(defun rapid-load (type &optional (position 0))
  "Rapid load for interactive testing."
  (when *default-database*
      (disconnect :database *default-database*))
  (test-connect-to-database type (nth position (db-type-spec type (read-specs))))
  (test-initialise-database)
  *default-database*)

(defun rl ()
  (rapid-load :postgresql))

(defun rlm ()
  (rapid-load :mysql))

(defun rlo ()
  (rapid-load :oracle))
