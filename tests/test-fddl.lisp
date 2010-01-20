;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:     test-fddl.lisp
;;;; Authors:  Marcus Pearce <m.t.pearce@city.ac.uk> and Kevin Rosenberg
;;;; Created:  30/03/2004
;;;; Updated:  $Id$
;;;;
;;;; Tests for the CLSQL Functional Data Definition Language
;;;; (FDDL).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(def-dataset *ds-fddl*
  (:setup ("CREATE TABLE ALPHA (A integer, B integer, C varchar (30), d date, f float)"
	   "CREATE TABLE BRAVO (jack integer, jill integer)"))
  (:sqldata "ALPHA" "A,B,C,d,f"
	    "1,1,'asdf','2010-01-01',3.14"
	    "2,1,'blarg','2012-12-21',0.1")
  (:cleanup "DROP TABLE ALPHA" "DROP TABLE BRAVO"))

(setq *rt-fddl*
      '(

;; list current tables
(deftest :fddl/table/1
    (with-dataset *ds-fddl*
      (sort (mapcar #'string-downcase
		    (clsql:list-tables :owner *test-database-user*))
	    #'string<))
  ("alpha" "bravo"))

;; create a table, test for its existence, drop it and test again
(deftest :fddl/table/2
    (progn (clsql:create-table  [foo]
			       '(([id] integer)
				 ([height] float)
				 ([name] (string 24))
				 ([comments] longchar)))
	   (values
	    (clsql:table-exists-p [foo] :owner *test-database-user*)
	    (progn
	      (clsql:drop-table [foo] :if-does-not-exist :ignore)
	      (clsql:table-exists-p [foo] :owner *test-database-user*))))
  t nil)

;; create a table, list its attributes and drop it
(deftest :fddl/table/3
    (apply #'values
	   (progn (clsql:create-table  [foo]
				      '(([id] integer)
					([height] float)
					([name] (char 255))
					([comments] longchar)))
		  (prog1
		      (sort (mapcar #'string-downcase
				    (clsql:list-attributes [foo]))
			    #'string<)
		    (clsql:drop-table [foo] :if-does-not-exist :ignore))))
  "comments" "height" "id" "name")

(deftest :fddl/table/4
    (values
     (clsql:table-exists-p "MyMixedCase")
     (progn
       (clsql:create-table "MyMixedCase" '(([a] integer)))
       (clsql:table-exists-p "MyMixedCase"))
     (progn
       (clsql:drop-table "MyMixedCase")
       (clsql:table-exists-p "MyMixedCase")))
  nil t nil)

(deftest :fddl/table/5
    (prog1
	(progn
	  (clsql:create-table "MyMixedCase" '(([a] integer)))
	  (clsql:execute-command "insert into \"MyMixedCase\" values (5)")
	   (clsql:insert-records :into "MyMixedCase" :values '(6))
	   (clsql:select [a] :from "MyMixedCase" :order-by '((a :asc))))
      (clsql:drop-table "MyMixedCase"))
  ((5) (6)))

(deftest :fddl/table/6
    (values
     (clsql:table-exists-p [foo])
     (progn
       (let ((*backend-warning-behavior*
	      (if (member *test-database-type*
			  '(:postgresql :postgresql-socket))
		  :ignore
		  :warn)))
	 (case *test-database-underlying-type*
	   (:mssql (clsql:create-table [foo]
				       '(([bar] integer :not-null :primary-key)
					 ([baz] string :not-null :unique))))
	   (t (clsql:create-table [foo]
				  '(([bar] integer :not-null :unique :primary-key)
				    ([baz] string :not-null :unique))))))
       (clsql:table-exists-p [foo]))
     (progn
       (clsql:drop-table [foo])
       (clsql:table-exists-p [foo])))
  nil t nil)

(deftest :fddl/table/7
    (values
     (clsql:table-exists-p [foo])
     (progn
       (let ((*backend-warning-behavior*
	      (if (member *test-database-type*
			  '(:postgresql :postgresql-socket))
		  :ignore
		  :warn)))
	 (clsql:create-table [foo] '(([bar] integer :not-null)
				     ([baz] string :not-null))
			     :constraints '("UNIQUE (bar,baz)"
					    "PRIMARY KEY (bar)")))
       (clsql:table-exists-p [foo]))
     (progn
       (clsql:drop-table [foo])
       (clsql:table-exists-p [foo])))
  nil t nil)

(deftest :fddl/attributes/1
    (apply #'values
     (with-dataset *ds-fddl*
       (sort
	(mapcar #'string-downcase
		(clsql:list-attributes [alpha] :owner *test-database-user*))
	#'string<)))
  "a" "b" "c" "d" "f")

(deftest :fddl/attributes/2
    (with-dataset *ds-fddl*
      (apply #'values
	     (sort
	      (mapcar #'(lambda (a) (string-downcase (car a)))
		      (clsql:list-attribute-types [alpha]
						  :owner *test-database-user*))
	      #'string<)))
  "a" "b" "c" "d" "f")

;; Attribute types are vendor specific so need to test a range
(deftest :fddl/attributes/3
    (with-dataset *ds-fddl*
      (and (member (clsql:attribute-type [a] [alpha]) '(:int :integer :int4 :number)) t))
  t)

(deftest :fddl/attributes/4
    (with-dataset *ds-fddl*
      (multiple-value-bind (type length scale nullable)
	  (clsql:attribute-type [c] [alpha])
	(values (clsql-sys:in type :varchar :varchar2) length scale nullable)))
  t 30 nil 1)

(deftest :fddl/attributes/5
    (with-dataset *ds-fddl*
      (and (member (clsql:attribute-type [d] [alpha]) '(:datetime :timestamp :date)) t))
  t)

(deftest :fddl/attributes/6
    (with-dataset *ds-fddl*
      (and (member (clsql:attribute-type [f] [alpha]) '(:float :float8 :number)) t))
  t)

(deftest :fddl/attributes/7
    (with-dataset *ds-bigint*
      (and (member (clsql:attribute-type [t_bigint] [TYPE_BIGINT]) '(:bigint :int8)) t))
  t)


;; create a view, test for existence, drop it and test again
(deftest :fddl/view/1
    (with-dataset *ds-fddl*
    (progn (clsql:create-view [v1]
			      :as [select [a] [b] [c]
					  :from [alpha]
					  :where [= [a] 1]])
	   (values
	    (clsql:view-exists-p [v1] :owner *test-database-user*)
	    (progn
	      (clsql:drop-view [v1] :if-does-not-exist :ignore)
	      (clsql:view-exists-p [v1] :owner *test-database-user*)))))
  t nil)

  ;; create a view, list its attributes and drop it
(deftest :fddl/view/2
      (with-dataset *ds-fddl*
	(progn (clsql:create-view [v1]
			      :as [select [a] [b] [c]
					  :from [alpha]
					  :where [= [a] 1]])
	     (unwind-protect
		  (sort (mapcar #'string-downcase
				(clsql:list-attributes [v1]))
			#'string<)
	       (clsql:drop-view [v1] :if-does-not-exist :ignore))))
    ("a" "b" "c"))

  ;; create a view, select stuff from it and drop it
(deftest :fddl/view/3
    (with-dataset *ds-fddl*
      (progn
	(clsql:create-view [v1]
			   :as [select [a] [b] [c]
				       :from [alpha]
				       :where [= [a] 1]])
	(unwind-protect
	     (let ((result
		    (list
		     ;; Shouldn't exist
		     (clsql:select [a] [b] [c]
				   :from [v1]
				   :where [= [a] -1])
		     ;; Should exist
		     (car (clsql:select [a] [b] [c]
					:from [v1]
					:where [= [a] 1])))))

	       (apply #'values result))
	  (clsql:drop-view [v1] :if-does-not-exist :ignore))))
  nil (1 1 "asdf"))

(deftest :fddl/view/4
    (with-dataset *ds-fddl*
      (progn
	(clsql:create-view [v1]
			   :column-list '([x] [y] [z])
			   :as [select [a] [b] [c]
				       :from [alpha]
				       :where [= [a] 1]])
	(unwind-protect
	     (let ((result
		    (list
		     ;; Shouldn't exist
		     (clsql:select [x] [y] [z]
				   :from [v1]
				   :where [= [x] -1])
		     ;; Should exist
		     (car (clsql:select [x] [y] [z]
					:from [v1]
					:where [= [x] 1])))))

	       (apply #'values result))
	  (clsql:drop-view [v1] :if-does-not-exist :ignore))))
  nil (1 1 "asdf"))

;; create an index, test for existence, drop it and test again
(deftest :fddl/index/1
    (with-dataset *ds-fddl*
      (progn (clsql:create-index [bar] :on [alpha] :attributes
				 '([a] [b] [c]) :unique t)
	     (values
	       (clsql:index-exists-p [bar] :owner *test-database-user*)
	       (progn
		 (clsql:drop-index [bar] :on [alpha]
				   :if-does-not-exist :ignore)
		 (clsql:index-exists-p [bar] :owner *test-database-user*)))))
  t nil)

;; create indexes with names as strings, symbols and in square brackets
(deftest :fddl/index/2
    (with-dataset *ds-fddl*
      (let ((names '("foo" foo [foo]))
	    (result '()))
	(dolist (name names)
	  (clsql:create-index name :on [alpha] :attributes '([a]))
	  (push (clsql:index-exists-p name :owner *test-database-user*) result)
	  (clsql:drop-index name :on [alpha] :if-does-not-exist :ignore))
	(apply #'values result)))
  t t t)

;; test list-indexes with keyword :ON
(deftest :fddl/index/3
    (progn
      (clsql:create-table [i3test] '(([a] (string 10))
				     ([b] integer)))
      (clsql:create-index [foo] :on [i3test] :attributes
       '([b]) :unique nil)
      (clsql:create-index [bar] :on [i3test] :attributes
       '([a]) :unique t)
      (values
       (clsql:table-exists-p [i3test])
       (clsql:index-exists-p [foo])
       (clsql:index-exists-p [bar])
       (sort
	(mapcar
	 #'string-downcase
	 (clsql:list-indexes :on [i3test] :owner *test-database-user*))
	#'string-lessp)
       (progn
	 (clsql:drop-index [bar] :on [i3test])
	 (clsql:drop-index [foo] :on [i3test])
	 (clsql:drop-table [i3test])
	 t)))
  t t t ("bar" "foo") t)

;; create an sequence, test for existence, drop it and test again
(deftest :fddl/sequence/1
    (progn (clsql:create-sequence [foo])
	   (values
	    (clsql:sequence-exists-p [foo] :owner *test-database-user*)
	    (progn
	      (clsql:drop-sequence [foo] :if-does-not-exist :ignore)
	      (clsql:sequence-exists-p [foo] :owner *test-database-user*))))
  t nil)

;; create and increment a sequence
(deftest :fddl/sequence/2
    (let ((val1 nil))
      (clsql:create-sequence [foo])
      (setf val1 (clsql:sequence-next [foo]))
      (prog1
	  (< val1 (clsql:sequence-next [foo]))
	(clsql:drop-sequence [foo] :if-does-not-exist :ignore)))
  t)

;; explicitly set the value of a sequence
(deftest :fddl/sequence/3
    (progn
      (clsql:create-sequence [foo])
      (clsql:set-sequence-position [foo] 5)
      (prog1
	  (clsql:sequence-next [foo])
	(clsql:drop-sequence [foo] :if-does-not-exist :ignore)))
  6)



(deftest :fddl/owner/1
    (with-dataset *ds-fddl*
      (and
       ;; user tables are an improper subset of all tables
       (= (length (intersection (clsql:list-tables :owner nil)
				(clsql:list-tables :owner :all)
				:test #'string=))
	  (length (clsql:list-tables :owner nil)))
       ;; user tables are a proper subset of all tables
       (> (length (clsql:list-tables :owner :all))
	  (length (clsql:list-tables :owner nil)))))
  t)

(deftest :fddl/cache-table-queries/1
    (with-dataset *ds-fddl*
      (list
       (gethash "ALPHA" (clsql-sys::attribute-cache clsql:*default-database*))
       (progn
	 (clsql:cache-table-queries "ALPHA" :action t)
	 (gethash "ALPHA" (clsql-sys::attribute-cache clsql:*default-database*)))
       (progn
	 (clsql:list-attribute-types "ALPHA")
	 (not
	  (null
	   (cadr
	    (gethash "ALPHA"
		     (clsql-sys::attribute-cache clsql:*default-database*))))))
       (progn
	 (clsql:cache-table-queries "ALPHA" :action :flush)
	 (gethash "ALPHA" (clsql-sys::attribute-cache clsql:*default-database*)))))
  (nil (t nil) t (t nil)))

  ))

#.(clsql:restore-sql-reader-syntax-state)
