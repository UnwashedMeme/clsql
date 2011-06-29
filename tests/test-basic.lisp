;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:    test-basic.lisp
;;;; Purpose: Tests for clsql string-based queries and result types
;;;; Author:  Kevin M. Rosenberg
;;;; Created: Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-tests)


(setq *rt-basic*
  '(
    (deftest :basic/type/1
	(with-dataset *ds-basic*
	  (let ((results '()))
	    (dolist (row (query "select * from TYPE_TABLE" :result-types :auto)
		     results)
	      (destructuring-bind (int float str) row
		(push (list (integerp int)
			    (typep float 'double-float)
			    (stringp str))
		      results)))))
      ((t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t)))

    (deftest :basic/type/2
	(with-dataset *ds-basic*
	  (let ((results '()))
	    (dolist (row (query "select * from TYPE_TABLE" :result-types :auto)
		     results)
	      (destructuring-bind (int float str) row
		(setq results
		      (cons (list (double-float-equal
				   (transform-float-1 int)
				   float)
				  (double-float-equal
				   (parse-double str)
				   float))
			    results))))
	    results))
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

    (deftest :basic/select/1
	(with-dataset *ds-basic*
	  (let ((rows (query "select * from TYPE_TABLE" :result-types :auto)))
	    (values
	      (length rows)
	      (length (car rows)))))
      11 3)

    (deftest :BASIC/SELECT/2
	(with-dataset *ds-basic*
	  (let ((results '()))
	    (dolist (row (query "select * from TYPE_TABLE" :result-types nil)
		     results)
	      (destructuring-bind (int float str) row
		(push (list (stringp int)
			    (stringp float)
			    (stringp str))
		      results)))))
      ((t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t)))

    (deftest :basic/select/3
	(with-dataset *ds-basic*
	  (let ((results '()))
	    (dolist (row (query "select * from TYPE_TABLE" :result-types nil)
		     results)
	      (destructuring-bind (int float str) row
		(push (list (double-float-equal
			     (transform-float-1 (parse-integer int))
			     (parse-double float))
			    (double-float-equal
			     (parse-double str)
			     (parse-double float)))
		      results)))))
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

    (deftest :basic/map/1
	(with-dataset *ds-basic*
	  (let ((results '())
		(rows (map-query 'vector #'identity "select * from TYPE_TABLE"
				 :result-types nil)))
	    (declare (type (simple-array list (*)) rows))
	    (dotimes (i (length rows) results)
	      (push
	       (list
		(listp (aref rows i))
		(length (aref rows i))
		(eql (- i 5)
		     (parse-integer (first (aref rows i))
				    :junk-allowed nil))
		(double-float-equal
		 (transform-float-1 (parse-integer (first (aref rows i))))
		 (parse-double (second (aref rows i)))))
	       results))))
      ((t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t)))


    (deftest :basic/map/2
	(with-dataset *ds-basic*
	  (let ((results '())
		(rows (map-query 'list #'identity "select * from TYPE_TABLE"
				 :result-types nil)))
	    (dotimes (i (length rows) results)
	      (push
	       (list
		(listp (nth i rows))
		(length (nth i rows))
		(eql (- i 5)
		     (parse-integer (first (nth i rows))
				    :junk-allowed nil))
		(double-float-equal
		 (transform-float-1 (parse-integer (first (nth i rows))))
		 (parse-double (second (nth i rows)))))
	       results))))
      ((t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t)))

    (deftest :basic/map/3
	(with-dataset *ds-basic*
	  (let ((results '())
		(rows (map-query 'list #'identity "select * from TYPE_TABLE"
				 :result-types :auto)))
	    (dotimes (i (length rows) results)
	      (push
	       (list
		(listp (nth i rows))
		(length (nth i rows))
		(eql (- i 5)
		     (first (nth i rows)))
		(double-float-equal
		 (transform-float-1 (first (nth i rows)))
		 (second (nth i rows))))
	       results))))
      ((t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t) (t 3 t t)))

    ;; confirm that a query on a single element returns a list of one element
    (deftest :basic/map/4
	(with-dataset *ds-basic*
	  (let ((rows (map-query 'list #'identity "select t_int from TYPE_TABLE"
				 :result-types nil)))
	    (values
	      (consp (first rows))
	      (length (first rows)))))
      t 1)

    (deftest :basic/do/1
	(with-dataset *ds-basic*
	  (let ((results '()))
	    (do-query ((int float str) "select * from TYPE_TABLE" :result-types nil)
	      (let ((int-number (parse-integer int)))
		(setq results
		      (cons (list (double-float-equal (transform-float-1
						       int-number)
						      (parse-double float))
				  (double-float-equal (parse-double str)
						      (parse-double float)))
			    results))))
	    results))
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

    (deftest :basic/do/2
	(with-dataset *ds-basic*
	  (let ((results '()))
	    (do-query ((int float str) "select * from TYPE_TABLE" :result-types :auto)
	      (setq results
		    (cons
		     (list (double-float-equal
			    (transform-float-1 int)
			    float)
			   (double-float-equal
			    (parse-double str)
			    float))
		     results)))
	    results))
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))


    (deftest :basic/bigint/1
	(with-dataset *ds-bigint*
	  (let ((results '()))
	    (dolist (row (query "select * from TYPE_BIGINT" :result-types :auto)
		     results)
	      (destructuring-bind (int bigint) row
		(push (list (integerp int)
                            (integerp bigint))
		      results)))))
      ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))


    (deftest :basic/bigtext/1
	(with-dataset *ds-bigtext*
	  (let* ((len 7499)
		 (str (make-string len :initial-element #\a))
		 (cmd (format nil "INSERT INTO testbigtext (a) VALUES ('~a')" str)))
	    (execute-command cmd)
	    (let ((a (first (query "SELECT a from testbigtext"
				   :flatp t :field-names nil))))
	      (assert (string= str a) (str a)
		      "mismatch on a. inserted: ~a returned: ~a" len (length a)))
	    ))
      nil)
    (deftest :basic/bigtext/2
     (flet ((random-char ()
              (let ((alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
                    (idx (random 52)))
                (elt alphabet idx))))
       (dotimes (n 10)
         (with-dataset *ds-bigtext*
           (let* ((len (random 7500))
                  (str (coerce (make-array len
                                           :initial-contents (loop repeat len collect (random-char)))
                               'string))
                  (cmd (format nil "INSERT INTO testbigtext (a) VALUES ('~a')" str)))
             (execute-command cmd)
             (let ((a (first (query "SELECT a from testbigtext"
                                    :flatp t :field-names nil))))
               (assert (string= str a) (str a)
                       "mismatch on randomized bigtext(~a) inserted: ~s returned: ~s" len str a))
             ))))
     nil)
    ))


(def-dataset *ds-basic*
  (:setup (lambda ()
	    (ignore-errors
	      (clsql:execute-command "DROP TABLE TYPE_TABLE")
	      (clsql:execute-command "DROP TABLE TYPE_BIGINT"))

	    (clsql:execute-command
	     "CREATE TABLE TYPE_TABLE (t_int integer, t_float double precision, t_str VARCHAR(30))")

	    (dotimes (i 11)
	      (let* ((test-int (- i 5))
		     (test-flt (transform-float-1 test-int)))
		(clsql:execute-command
		 (format nil "INSERT INTO TYPE_TABLE VALUES (~a,~a,'~a')"
			 test-int
			 (clsql-sys:number-to-sql-string test-flt)
			 (clsql-sys:number-to-sql-string test-flt)
			 ))))))
  (:cleanup "DROP TABLE TYPE_TABLE"))

(def-dataset *ds-bigint*
  (:setup (lambda ()
	    (ignore-errors (clsql:execute-command "DROP TABLE TYPE_BIGINT"))
	    (clsql:execute-command "CREATE TABLE TYPE_BIGINT (T_INT integer, T_BIGINT BIGINT)")
	    (dotimes (i 11)
	      (clsql:execute-command
	       (format nil "INSERT INTO TYPE_BIGINT VALUES (~a,~a)"
		       (- i 5)
		       (transform-bigint-1 (- i 5)))))))
  (:cleanup "DROP TABLE TYPE_BIGINT"))

;;;; Testing functions

(defun transform-float-1 (i)
  (coerce (* i (abs (/ i 2)) (expt 10 (* 2 i))) 'double-float))

(defun transform-bigint-1 (i)
  (* i (expt 10 (* 3 (abs i)))))

(defun parse-double (num-str)
  (let ((*read-default-float-format* 'double-float))
    (coerce (read-from-string num-str) 'double-float)))

(defun double-float-equal (a b)
  (if (zerop a)
      (if (zerop b)
	  t
	  nil)
      (let ((diff (abs (/ (- a b) a))))
	(if (> diff (* 10 double-float-epsilon))
	    nil
	    t))))

(def-dataset *ds-bigtext*
  (:setup "CREATE TABLE testbigtext(a varchar(7500))")
  (:cleanup "DROP TABLE testbigtext"))
