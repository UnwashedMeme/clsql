;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-init.lisp
;;;; Authors: Marcus Pearce <m.t.pearce@city.ac.uk>, Kevin Rosenberg
;;;; Created: 30/03/2004
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

;           (test-initialise-database)

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
                             '(:postgresql :mysql :sqlite3 :mssql)))
                (clsql-sys:in test :fdml/select/37 :fdml/select/38))
           (push (cons test "LIMIT keyword not supported in SELECT") skip-tests))
	  ((and (not (clsql-sys:db-type-has-auto-increment? db-underlying-type))
		(clsql-sys:in test :oodml/select/12 :oodml/select/13 :oodml/select/14
			      :oodml/select/15 :oodml/select/16 :oodml/select/17
			      :oodml/select/18 :oodml/select/19 :oodml/select/20
			      :oodml/select/21 :oodml/select/22
			      :oodml/update-records/4 :oodml/update-records/4-slots
			      :oodml/update-records/5 :oodml/update-records/5-slots
			      :oodml/update-records/6 :oodml/update-records/7
			      :oodml/update-records/8 :oodml/update-records/9
			      :oodml/update-records/9-slots :oodml/update-instance/3
			      :oodml/update-instance/4 :oodml/update-instance/5
			      :oodml/update-instance/6 :oodml/update-instance/7
			      :oodml/db-auto-sync/3 :oodml/db-auto-sync/4))
	   (push (cons test ":auto-increment not by backend.") skip-tests))
          (t
           (push test-form test-forms)))))
      (values (nreverse test-forms) (nreverse skip-tests))))

(defun rapid-load (type &optional (position 0))
  "Rapid load for interactive testing."
  (when *default-database*
      (disconnect :database *default-database*))
  (test-connect-to-database type (nth position (db-type-spec type (read-specs))))
  ;(test-initialise-database)
  *default-database*)

(defun rl ()
  (rapid-load :postgresql))

(defun rlm ()
  (rapid-load :mysql))

(defun rlo ()
  (rapid-load :oracle))
