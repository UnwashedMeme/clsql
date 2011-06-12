;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-oodml.lisp
;;;; Created: 01/04/2004
;;;;
;;;; Tests for the CLSQL Object Oriented Data Definition Language
;;;; (OODML).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(setq *rt-oodml*
      '(

(deftest :oodml/select/1
    (with-dataset *ds-employees*
      (mapcar #'(lambda (e) (slot-value e 'last-name))
	      (clsql:select 'employee :order-by [last-name] :flatp t :caching nil)))
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
	      "Stalin" "Trotsky" "Yeltsin"))

(deftest :oodml/select/2
    (with-dataset *ds-employees*
      (mapcar #'(lambda (e) (slot-value e 'name))
	      (clsql:select 'company :flatp t :caching nil)))
  ("Widgets Inc."))

(deftest :oodml/select/3
    (with-dataset *ds-employees*
      (mapcar #'(lambda (e) (slot-value e 'ecompanyid))
	      (clsql:select 'employee
			    :where [and [= [slot-value 'employee 'ecompanyid]
					   [slot-value 'company 'companyid]]
					[= [slot-value 'company 'name]
					   "Widgets Inc."]]
			    :flatp t
			    :caching nil)))
  (1 1 1 1 1 1 1 1 1 1))

(deftest :oodml/select/4
    (with-dataset *ds-employees*
      (mapcar #'(lambda (e)
		  (concatenate 'string (slot-value e 'first-name)
			       " "
			       (slot-value e 'last-name)))
	      (clsql:select 'employee :where [= [slot-value 'employee 'first-name]
						"Vladimir"]
			    :flatp t
			    :order-by [last-name]
			    :caching nil)))
  ("Vladimir Lenin" "Vladimir Putin"))

(deftest :oodml/select/5
    (with-dataset *ds-employees*
      (length (clsql:select 'employee :where [married] :flatp t :caching nil)))
  3)

(deftest :oodml/select/6
    (with-dataset *ds-employees*
      (let ((a (caar (clsql:select 'address :where [= 1 [addressid]] :caching nil))))
	(values
	  (slot-value a 'street-number)
	  (slot-value a 'street-name)
	  (slot-value a 'city)
	  (slot-value a 'postal-code))))
  10 "Park Place" "Leningrad" 123)

(deftest :oodml/select/7
    (with-dataset *ds-employees*
      (let ((a (caar (clsql:select 'address :where [= 2 [addressid]] :caching nil))))
	(values
	  (slot-value a 'street-number)
	  (slot-value a 'street-name)
	  (slot-value a 'city)
	  (slot-value a 'postal-code))))
  nil "" "no city" 0)

(deftest :oodml/select/8
    (with-dataset *ds-employees*
      (mapcar #'(lambda (e) (slot-value e 'married))
	      (clsql:select 'employee :flatp t :order-by [emplid] :caching nil)))
  (t t t nil nil nil nil nil nil nil))

(deftest :oodml/select/9
    (with-dataset *ds-employees*
      (mapcar #'(lambda (pair)
		  (list
		   (typep (car pair) 'address)
		   (typep (second pair) 'employee-address)
		   (slot-value (car pair) 'addressid)
		   (slot-value (second pair) 'aaddressid)
		   (slot-value (second pair) 'aemplid)))
	      (employee-addresses employee1)))
  ((t t 1 1 1) (t t 2 2 1)))

(deftest :oodml/select/10
    (with-dataset *ds-employees*
      (mapcar #'(lambda (pair)
		  (list
		   (typep (car pair) 'address)
		   (typep (second pair) 'employee-address)
		   (slot-value (car pair) 'addressid)
		   (slot-value (second pair) 'aaddressid)
		   (slot-value (second pair) 'aemplid)))
	      (employee-addresses employee2)))
  ((t t 2 2 2)))

(deftest :oodml/select/11
    (with-dataset *ds-employees*
      (values (mapcar #'(lambda (x) (slot-value x 'emplid))
		      (clsql:select 'employee :order-by '(([emplid] :asc))
				    :flatp t))
	      (mapcar #'(lambda (x) (slot-value x 'emplid))
		      (clsql:select 'employee :order-by '(([emplid] :desc))
				    :flatp t))))
  (1 2 3 4 5 6 7 8 9 10)
  (10 9 8 7 6 5 4 3 2 1))

;; test retrieval of node, derived nodes etc
(deftest :oodml/select/12
    (with-dataset *ds-nodes*
      (length (clsql:select 'node :where [not [null [node-id]]] :flatp t :caching nil)))
  11)

(deftest :oodml/select/13
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'node :where [= 1 [node-id]] :flatp t :caching nil))))
	(values
	  (slot-value a 'node-id)
	  (slot-value a 'title))))
  1 "Bare node")

(deftest :oodml/select/14
    (with-dataset *ds-nodes*
      (length (clsql:select 'setting :where [not [null [setting-id]]] :flatp t :caching nil)))
  4)

(deftest :oodml/select/15
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'setting :where [= 3 [setting-id]] :flatp t :caching nil))))
	(values
	  (slot-value a 'node-id)
	  (slot-value a 'setting-id)
	  (slot-value a 'title)
	  (slot-value a 'vars))))
  3 3 "Setting2" "var 2")

(deftest :oodml/select/16
    (with-dataset *ds-nodes*
      (length (clsql:select 'user :where [not [null [user-id]]] :flatp t :caching nil)))
  2)

(deftest :oodml/select/17
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'user :where [= 4 [user-id]] :flatp t :caching nil))))
	(values
	  (slot-value a 'node-id)
	  (slot-value a 'user-id)
	  (slot-value a 'title)
	  (slot-value a 'nick))))
  4 4 "user-1" "first user")

(deftest :oodml/select/18
    (with-dataset *ds-nodes*
      (length (clsql:select 'theme :where [not [null [theme-id]]] :flatp t :caching nil)))
  2)

(deftest :oodml/select/19
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'theme :where [= 6 [theme-id]] :flatp t :caching nil))))
	(slot-value a 'theme-id)))
  6)

(deftest :oodml/select/20
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'theme :where [= 7 [theme-id]] :flatp t :caching nil))))
	(values
	  (slot-value a 'node-id)
	  (slot-value a 'theme-id)
	  (slot-value a 'title)
	  (slot-value a 'vars)
	  (slot-value a 'doc)
	  )))
  7 7 "theme-2"
  nil "second theme")

;; Some tests to check weird subclassed nodes (node without own table, or subclassed of same)
(deftest :oodml/select/21
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'location :where [= [title] "location-1"] :flatp t :caching nil))))
	(values
	  (slot-value a 'node-id)
	  (slot-value a 'title))))
  8 "location-1")

(deftest :oodml/select/22
    (with-dataset *ds-nodes*
      (let ((a (car (clsql:select 'subloc :where [not [null [subloc-id]]] :flatp t :caching nil))))
	(values
	  (slot-value a 'node-id)
	  (slot-value a 'subloc-id)
	  (slot-value a 'title)
	  (slot-value a 'loc))))
  10 10 "subloc-1" "a subloc")

(deftest :oodml/select/23
    (with-dataset *ds-artists*
      (length (clsql:select 'artist :flatp t :caching nil)))
  0)



;; test retrieval is deferred
(deftest :oodm/retrieval/1
    (with-dataset *ds-employees*
      (every #'(lambda (e) (not (slot-boundp e 'company)))
	     (select 'employee :flatp t :caching nil)))
  t)

(deftest :oodm/retrieval/2
    (with-dataset *ds-employees*
      (every #'(lambda (e) (not (slot-boundp e 'address)))
	     (select 'deferred-employee-address :flatp t :caching nil)))
  t)

;; :retrieval :immediate should be boundp before accessed
(deftest :oodm/retrieval/3
    (with-dataset *ds-employees*
      (every #'(lambda (ea) (slot-boundp ea 'address))
	     (select 'employee-address :flatp t :caching nil)))
  t)

(deftest :oodm/retrieval/4
    (with-dataset *ds-employees*
      (mapcar #'(lambda (ea) (typep (slot-value ea 'address) 'address))
	      (select 'employee-address :flatp t :caching nil)))
  (t t t t t))

(deftest :oodm/retrieval/5
    (with-dataset *ds-employees*
      (mapcar #'(lambda (ea) (typep (slot-value ea 'address) 'address))
	      (select 'deferred-employee-address :flatp t :caching nil)))
  (t t t t t))

(deftest :oodm/retrieval/6
    (with-dataset *ds-employees*
      (every #'(lambda (ea) (slot-boundp (slot-value ea 'address) 'addressid))
	     (select 'employee-address :flatp t :caching nil)))
  t)

(deftest :oodm/retrieval/7
    (with-dataset *ds-employees*
      (every #'(lambda (ea) (slot-boundp (slot-value ea 'address) 'addressid))
	     (select 'deferred-employee-address :flatp t :caching nil)))
  t)

(deftest :oodm/retrieval/8
    (with-dataset *ds-employees*
      (mapcar #'(lambda (ea) (slot-value (slot-value ea 'address) 'street-number))
	      (select 'employee-address :flatp t :order-by [aaddressid] :caching nil)))
  (10 10 nil nil nil))

(deftest :oodm/retrieval/9
    (with-dataset *ds-employees*
      (mapcar #'(lambda (ea) (slot-value (slot-value ea 'address) 'street-number))
	      (select 'deferred-employee-address :flatp t :order-by [aaddressid] :caching nil)))
  (10 10 nil nil nil))

;; tests update-records-from-instance
(deftest :oodml/update-records/1
    (with-dataset *ds-employees*
      (values
	(progn
	  (let ((lenin (car (clsql:select 'employee
					  :where [= 1 [slot-value 'employee 'emplid]]
					  :flatp t
					  :caching nil))))
	    (format nil "~a ~a: ~a"
		(first-name lenin)
		(last-name lenin)
		(employee-email lenin))))
	(progn
	  (setf (slot-value employee1 'first-name) "Dimitriy"
		(slot-value employee1 'last-name) "Ivanovich"
		(slot-value employee1 'email) "ivanovich@soviet.org")
	  (clsql:update-records-from-instance employee1)
	  (let ((lenin (car (clsql:select 'employee
					  :where [= 1 [slot-value 'employee 'emplid]]
					  :flatp t
					  :caching nil))))
	    (format nil "~a ~a: ~a"
		(first-name lenin)
		(last-name lenin)
		(employee-email lenin))))))
  "Vladimir Lenin: lenin@soviet.org"
  "Dimitriy Ivanovich: ivanovich@soviet.org")

;; tests update-record-from-slot
(deftest :oodml/update-records/2
    (with-dataset *ds-employees*
      ;(start-sql-recording :type :both)
      (values
	(employee-email
	 (car (clsql:select 'employee
			    :where [= 1 [slot-value 'employee 'emplid]]
			    :flatp t
			    :caching nil)))
 	(progn
 	  (setf (slot-value employee1 'email) "lenin-nospam@soviet.org")
 	  (clsql:update-record-from-slot employee1 'email)
	  (employee-email
	   (car (clsql:select 'employee
			      :where [= 1 [slot-value 'employee 'emplid]]
			      :flatp t
			      :caching nil))))))
  "lenin@soviet.org" "lenin-nospam@soviet.org")

;; tests update-record-from-slots
(deftest :oodml/update-records/3
    (with-dataset *ds-employees*
      (values
	(let ((lenin (car (clsql:select 'employee
					:where [= 1 [slot-value 'employee 'emplid]]
					:flatp t
					:caching nil))))
	  (format nil "~a ~a: ~a"
		  (first-name lenin)
		  (last-name lenin)
		  (employee-email lenin)))
	(progn
	  (setf (slot-value employee1 'first-name) "Dimitriy"
		(slot-value employee1 'last-name) "Ivanovich"
		(slot-value employee1 'email) "ivanovich@soviet.org")
	  (clsql:update-record-from-slots employee1 '(first-name last-name email))
	  (let ((lenin (car (clsql:select 'employee
					  :where [= 1 [slot-value 'employee 'emplid]]
					  :flatp t
					  :caching nil))))
	    (format nil "~a ~a: ~a"
		    (first-name lenin)
		    (last-name lenin)
		    (employee-email lenin))))))
  "Vladimir Lenin: lenin@soviet.org"
  "Dimitriy Ivanovich: ivanovich@soviet.org")

(deftest :oodml/update-records/4
    (with-dataset *ds-nodes*
      (flet ((print-fresh-node ()
	       (let ((base (car (clsql:select 'node
					      :where [= 1 [slot-value 'node 'node-id]]
					      :flatp t :caching nil))))
		 (format nil "~a ~a"
			 (slot-value base 'node-id)
			 (slot-value base 'title)))))
	(values
	  (print-fresh-node)
	  (let ((base (car (clsql:select 'node
					 :where [= 1 [slot-value 'node 'node-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value base 'title) "Altered title")
	    (clsql:update-records-from-instance base)
	    (print-fresh-node)))))
  "1 Bare node"
  "1 Altered title")

(deftest :oodml/update-records/4-slots ;just like 4, but use slots fns.
    (with-dataset *ds-nodes*
      (flet ((print-fresh-setting ()
	       (let ((node (car (clsql:select 'setting
					      :where [= 3 [slot-value 'setting 'setting-id]]
					      :flatp t :caching nil))))
		 (format nil "~a ~a ~a"
			 (slot-value node 'setting-id)
			 (slot-value node 'title)
			 (slot-value node 'vars)))))
	(values
	  (print-fresh-setting)
	  (let ((node (car (clsql:select 'setting
					 :where [= 3 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title")
	    (setf (slot-value node 'vars) "Altered vars")
	    (clsql-sys:update-record-from-slot node 'title)
	    (clsql-sys:update-record-from-slot node 'vars)
	    (print-fresh-setting))
	  (let ((node (car (clsql:select 'setting
					 :where [= 3 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Setting2")
	    (setf (slot-value node 'vars) "var 2")
	    (clsql:update-records-from-instance node)
	    (clsql-sys:update-record-from-slots node '(vars title))
	    (print-fresh-setting)))))
  "3 Setting2 var 2"
  "3 Altered title Altered vars"
  "3 Setting2 var 2")

(deftest :oodml/update-records/5
    (with-dataset *ds-nodes*
      (flet ((print-fresh-setting ()
	       (let ((node (car (clsql:select 'setting
					      :where [= 3 [slot-value 'setting 'setting-id]]
					      :flatp t :caching nil))))
		 (format nil "~a ~a ~a"
			 (slot-value node 'setting-id)
			 (slot-value node 'title)
			 (slot-value node 'vars)))))
	(values
	  (print-fresh-setting)
	  (let ((node (car (clsql:select 'setting
					 :where [= 3 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title")
	    (setf (slot-value node 'vars) "Altered vars")
	    (clsql:update-records-from-instance node)
	    (print-fresh-setting)))))
  "3 Setting2 var 2"
  "3 Altered title Altered vars")

(deftest :oodml/update-records/5-slots
    (with-dataset *ds-nodes*
      (flet ((print-fresh-setting ()
	       (let ((node (car (clsql:select 'setting
					      :where [= 3 [slot-value 'setting 'setting-id]]
					      :flatp t :caching nil))))
		 (format nil "~a ~a ~a"
			 (slot-value node 'setting-id)
			 (slot-value node 'title)
			 (slot-value node 'vars)))))
	(values
	  (print-fresh-setting)
	  (let ((node (car (clsql:select 'setting
					 :where [= 3 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title")
	    (setf (slot-value node 'vars) "Altered vars")
	    (clsql-sys:update-record-from-slot node 'title)
	    (clsql-sys:update-record-from-slot node 'vars)
	    (print-fresh-setting))
	  (let ((node (car (clsql:select 'setting
					 :where [= 3 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Setting2")
	    (setf (slot-value node 'vars) "var 2")
	    (clsql-sys:update-record-from-slots node '(title vars))
	    (print-fresh-setting)))))
  "3 Setting2 var 2"
  "3 Altered title Altered vars"
  "3 Setting2 var 2")

(deftest :oodml/update-records/6
    (with-dataset *ds-nodes*
      (flet ((print-fresh-setting ()
	       (let ((node (car (clsql:select 'setting
					      :where [= 7 [slot-value 'setting 'setting-id]]
					      :flatp t :caching nil))))
		 (format nil "~a ~a ~a"
			 (slot-value node 'setting-id)
			 (slot-value node 'title)
			 (or (slot-value node 'vars) "NIL")))))
	(values
	  (print-fresh-setting)
	  (let ((node (car (clsql:select 'setting
					 :where [= 7 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title")
	    (setf (slot-value node 'vars) "Altered vars")
	    (clsql:update-records-from-instance node)
	    (print-fresh-setting))
	  (let ((node (car (clsql:select 'setting
					 :where [= 7 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "theme-2")
	    (setf (slot-value node 'vars) nil)
	    (clsql:update-records-from-instance node)
	    (print-fresh-setting)))))
  "7 theme-2 NIL"
  "7 Altered title Altered vars"
  "7 theme-2 NIL")

(deftest :oodml/update-records/7
    (with-dataset *ds-nodes*
      (flet ((print-fresh-user ()
	       "requery to get what the db has, and print out."
	       (let ((node (car (clsql:select 'user
					      :where [= 5 [slot-value 'user 'user-id]]
					      :flatp t :caching nil))))
		 (format nil "~a ~a ~a"
			 (slot-value node 'user-id)
			 (slot-value node 'title)
			 (slot-value node 'nick)))))
	(values
	  (print-fresh-user)
	  (let ((node (car (clsql:select 'user
					 :where [= 5 [slot-value 'user 'user-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title")
	    (setf (slot-value node 'nick) "Altered nick")
	    (clsql:update-records-from-instance node)
	    (print-fresh-user)))))
  "5 user-2 second user"
  "5 Altered title Altered nick")

(deftest :oodml/update-records/8
    (with-dataset *ds-nodes*
      (flet ((print-fresh-theme ()
	       (let ((node (car (clsql:select 'theme
					      :where [= 6 [slot-value 'theme 'theme-id]]
					      :flatp t :caching nil))))
		 (with-slots (node-id setting-id theme-id title vars doc) node
		   (format nil "~a ~a ~a ~a ~a ~a"
			   node-id setting-id theme-id
			   title (or vars "NIL") doc)))))
	(values
	  (print-fresh-theme)
	  (let ((node (car (clsql:select 'setting
					 :where [= 6 [slot-value 'setting 'setting-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title")
	    (setf (slot-value node 'vars) nil)
	    (clsql:update-records-from-instance node)
	    (print-fresh-theme))
	  (let ((node (car (clsql:select 'theme
					 :where [= 6 [slot-value 'theme 'theme-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "Altered title again")
	    (setf (slot-value node 'doc) "altered doc")
	    (clsql:update-records-from-instance node)
	    (print-fresh-theme))
	  (let ((node (car (clsql:select 'theme
					 :where [= 6 [slot-value 'theme 'theme-id]]
					 :flatp t :caching nil))))
	    (setf (slot-value node 'title) "theme-1")
	    (setf (slot-value node 'vars) "empty")
	    (setf (slot-value node 'doc) "first theme")
	    (clsql:update-records-from-instance node)
	    (print-fresh-theme)))))
  "6 6 6 theme-1 empty first theme"
  "6 6 6 Altered title NIL first theme"
  "6 6 6 Altered title again NIL altered doc"
  "6 6 6 theme-1 empty first theme")

(deftest :oodml/update-records/9
    (with-dataset *ds-nodes*
      (flet ((print-fresh-subloc ()
	       (let ((sl (car (clsql:select 'subloc
					    :where [= 10 [slot-value 'subloc 'subloc-id]]
					    :flatp t :caching nil))))
		 (unless sl
		   (error "Couldn't find expected sublocation"))
		 (format nil "~a ~a ~a"
			 (slot-value sl 'subloc-id)
			 (slot-value sl 'title)
			 (slot-value sl 'loc)))))
	(values
	  (print-fresh-subloc)
	  (let ((sl (car (clsql:select 'subloc
				       :where [= 10 [slot-value 'subloc 'subloc-id]]
				       :flatp t :caching nil))))
	    (setf (slot-value sl 'title) "Altered subloc title")
	    (setf (slot-value sl 'loc) "Altered loc")
	    (clsql:update-records-from-instance sl)
	    (print-fresh-subloc)))))
  "10 subloc-1 a subloc"
  "10 Altered subloc title Altered loc")

(deftest :oodml/update-records/9-slots ;like 9, but use slots fns.
    (with-dataset *ds-nodes*
      (flet ((print-fresh-subloc ()
	       (let ((sl (car (clsql:select 'subloc
					    :where [= 10 [slot-value 'subloc 'subloc-id]]
					    :flatp t :caching nil))))
		 (unless sl
		   (error "In psfl: found no sublocation with id = 10"))
		 (format nil "~a ~a ~a"
			 (slot-value sl 'subloc-id)
			 (slot-value sl 'title)
			 (slot-value sl 'loc)))))
	(values
	  (print-fresh-subloc)
	  (let ((sl (car (clsql:select 'subloc
				       :where [= 10 [slot-value 'subloc 'subloc-id]]
				       :flatp t :caching nil))))
	    (unless sl
	      (error "Select for modification: Found no sublocation with id = 10"))
	    (setf (slot-value sl 'title) "Altered subloc title")
	    (setf (slot-value sl 'loc) "Altered loc")
	    (clsql:update-record-from-slot sl 'title)
	    (clsql:update-record-from-slot sl 'loc)
	    (print-fresh-subloc))
	  (let ((sl (car (clsql:select 'subloc
				       :where [= 10 [slot-value 'subloc 'subloc-id]]
				       :flatp t :caching nil))))
	    (unless sl
	      (error "Select for next modification: Found no sublocation with id = 10"))
	    (setf (slot-value sl 'title) "subloc-1")
	    (setf (slot-value sl 'loc) "a subloc")
	    (clsql:update-record-from-slots sl '(title loc))
	    (print-fresh-subloc)))))
  "10 subloc-1 a subloc"
  "10 Altered subloc title Altered loc"
  "10 subloc-1 a subloc")

;; Verify that we can set a float to nil and then read it back
;; (was failing in Postgresql at somepoint)
(deftest :oodml/update-records/10
    (with-dataset *ds-employees*
      (let ((emp (first (clsql:select 'employee :where [= [emplid] 1] :flatp t))))
	(setf (height emp) nil)
	(clsql-sys:update-record-from-slot emp 'height)
	(values
	  (clsql:select [height] :from [employee] :where [= [emplid] 1])
	  (progn
	    (setf (height emp) 42.0)
	    (clsql-sys:update-record-from-slot emp 'height)
	    (clsql:select [height] :from [employee] :where [= [emplid] 1]))
	  (progn
	    (setf (height emp) 24.13d0)
	    (clsql-sys:update-record-from-slot emp 'height)
	    (clsql:select [height] :from [employee] :where [= [emplid] 1])))))
  ((nil))
  ((42.0d0))
  ((24.13d0)))

(deftest :oodml/update-records/11
    (with-dataset *ds-artists*
      (clsql:update-records-from-instance artist1)
      (list (name artist1) (artist_id artist1)))
  ("Mogwai" 1))

(deftest :oodml/update-records/12
    (with-dataset *ds-artists*
      (clsql:update-records-from-instance artist1)
      (list (name artist1) (genre artist1)))
  ("Mogwai" "Unknown"))

;; tests update-instance-from-records
(deftest :oodml/update-instance/1
    (with-dataset *ds-employees*
      (values
	(format nil "~a ~a: ~a"
		(slot-value employee1 'first-name)
		(slot-value employee1 'last-name)
		(slot-value employee1 'email))
	(progn
	  (clsql:update-records [employee]
				:av-pairs '(([first-name] "Ivan")
					    ([last-name] "Petrov")
					    ([email] "petrov@soviet.org"))
				:where [= [emplid] 1])
	  (clsql:update-instance-from-records employee1)
	  (format nil "~a ~a: ~a"
		(slot-value employee1 'first-name)
		(slot-value employee1 'last-name)
		(slot-value employee1 'email)))))
  "Vladimir Lenin: lenin@soviet.org"
  "Ivan Petrov: petrov@soviet.org")

;; tests update-slot-from-record
(deftest :oodml/update-instance/2
    (with-dataset *ds-employees*
      (values
	(slot-value employee1 'email)
	(progn
	  (clsql:update-records [employee]
				:av-pairs '(([email] "lenin-nospam@soviet.org"))
				:where [= [emplid] 1])
	  (clsql:update-slot-from-record employee1 'email)
	  (slot-value employee1 'email))))
  "lenin@soviet.org" "lenin-nospam@soviet.org")

;; tests normalizedp update-instance-from-records
(deftest :oodml/update-instance/3
    (with-dataset *ds-nodes*
      (values
	(with-output-to-string (out)
	  (format out "~a ~a ~a ~a"
		  (slot-value theme2 'theme-id)
		  (slot-value theme2 'title)
		  (or (slot-value theme2 'vars) "NIL")
		  (slot-value theme2 'doc)))
	(progn
	  (clsql:update-records [node] :av-pairs '(([title] "Altered title"))
				:where [= [node-id] 7])
	  (clsql:update-records [setting] :av-pairs '(([vars] "Altered vars"))
				:where [= [setting-id] 7])
	  (clsql:update-records [theme] :av-pairs '(([doc] "Altered doc"))
				:where [= [theme-id] 7])
	  (clsql:update-instance-from-records theme2)
	  (with-output-to-string (out)
	    (format out "~a ~a ~a ~a"
		    (slot-value theme2 'theme-id)
		    (slot-value theme2 'title)
		    (slot-value theme2 'vars)
		    (slot-value theme2 'doc))))))
  "7 theme-2 NIL second theme"
  "7 Altered title Altered vars Altered doc")

(deftest :oodml/update-instance/4
    (with-dataset *ds-nodes*
      (values
	(progn
	  (setf loc2 (car (clsql:select 'location
					:where [= [node-id] 9]
					:flatp t :caching nil)))
	  (format nil "~a ~a"
		  (slot-value loc2 'node-id)
		  (slot-value loc2 'title)))
	(progn
	  (clsql:update-records [node] :av-pairs '(([title] "Altered title"))
				:where [= [node-id] 9])
	  (clsql:update-instance-from-records loc2)
	  (format nil "~a ~a"
		  (slot-value loc2 'node-id)
		  (slot-value loc2 'title)))))
  "9 location-2"
  "9 Altered title")

(deftest :oodml/update-instance/5
    (with-dataset *ds-nodes*
      (values
	(format nil "~a ~a ~a"
		(slot-value subloc2 'subloc-id)
		(slot-value subloc2 'title)
		(slot-value subloc2 'loc))
	(progn
	  (clsql:update-records [node] :av-pairs '(([title] "Altered title"))
				:where [= [node-id] 11])
	  (clsql:update-records [subloc] :av-pairs '(([loc] "Altered loc"))
				:where [= [subloc-id] 11])
	  (clsql:update-instance-from-records subloc2)
	  (format nil "~a ~a ~a"
		  (slot-value subloc2 'subloc-id)
		  (slot-value subloc2 'title)
		  (slot-value subloc2 'loc)))))
  "11 subloc-2 second subloc"
  "11 Altered title Altered loc")

;; tests update-slot-from-record with normalizedp stuff
(deftest :oodml/update-instance/6
    (with-dataset *ds-nodes*
      (values
	(slot-value theme1 'doc)
	(slot-value theme1 'vars)
	(progn
	  (clsql:update-records [theme]
				:av-pairs '(([doc] "altered doc"))
				:where [= [theme-id] 6])
	  (clsql:update-slot-from-record theme1 'doc)
	  (slot-value theme1 'doc))
	(progn
	  (clsql:update-records [setting]
				:av-pairs '(([vars] "altered vars"))
				:where [= [setting-id] 6])
	  (clsql:update-slot-from-record theme1 'vars)
	  (slot-value theme1 'vars))))
  "first theme" "empty"
  "altered doc" "altered vars")

(deftest :oodml/update-instance/7
    (flet ((print-loc (l)
	     (format nil "~a: ~a"
		     (slot-value l 'node-id) (slot-value l 'title)))
	   (print-subloc (sl)
	     (format nil "~a: ~a"
		     (slot-value sl 'node-id) (slot-value sl 'loc))))
	(with-dataset *ds-nodes*
      (values
	(print-loc loc2)
	(print-subloc subloc2)
	(progn
	  (clsql:update-records [node]
				:av-pairs '(([title] "altered title"))
				:where [= [node-id] 9])
	  (clsql:update-slot-from-record loc2 'title)
	  (print-loc loc2))
	(progn
	  (clsql:update-records [subloc]
				:av-pairs '(([loc] "altered loc"))
				:where [= [subloc-id] 11])
	  (clsql:update-slot-from-record subloc2 'loc)
	  (print-subloc subloc2)))))
  "9: location-2" "11: second subloc"
  "9: altered title" "11: altered loc")

(deftest :oodml/do-query/1
    (with-dataset *ds-employees*
      (let ((result '()))
	(clsql:do-query ((e) [select 'employee :order-by [emplid]])
	  (push (slot-value e 'last-name) result))
	result))
  ("Putin" "Yeltsin" "Gorbachev" "Chernenko" "Andropov" "Brezhnev" "Kruschev"
	   "Trotsky" "Stalin" "Lenin"))

(deftest :oodml/do-query/2
    (with-dataset *ds-employees*
      (let ((result '()))
	(clsql:do-query ((e c) [select 'employee 'company
				       :where [= [slot-value 'employee 'last-name]
						 "Lenin"]])
	  (push (list (slot-value e 'last-name) (slot-value c 'name))
		result))
	result))
  (("Lenin" "Widgets Inc.")))

(deftest :oodml/map-query/1
    (with-dataset *ds-employees*
      (clsql:map-query 'list #'last-name [select 'employee :order-by [emplid]]))
  ("Lenin" "Stalin" "Trotsky" "Kruschev" "Brezhnev" "Andropov" "Chernenko"
	   "Gorbachev" "Yeltsin" "Putin"))

(deftest :oodml/map-query/2
    (with-dataset *ds-employees*
      (clsql:map-query 'list #'(lambda (e c) (list (slot-value e 'last-name)
						   (slot-value c 'name)))
		       [select 'employee 'company :where [= [slot-value 'employee 'last-name]
							    "Lenin"]]))
  (("Lenin" "Widgets Inc.")))

(deftest :oodml/iteration/3
    (with-dataset *ds-employees*
      (loop for (e) being the records in
	    [select 'employee :where [< [emplid] 4] :order-by [emplid]]
	    collect (slot-value e 'last-name)))
  ("Lenin" "Stalin" "Trotsky"))


(deftest :oodml/cache/1
    (with-dataset *ds-employees*
      (progn
	(setf (clsql-sys:record-caches *default-database*) nil)
	(let ((employees (select 'employee)))
	  (every #'(lambda (a b) (eq a b))
		 employees (select 'employee)))))
  t)

(deftest :oodml/cache/2
    (with-dataset *ds-employees*
      (let ((employees (select 'employee)))
	(equal employees (select 'employee :flatp t))))
  nil)

(deftest :oodml/refresh/1
    (with-dataset *ds-employees*
      (let ((addresses (select 'address)))
	(equal addresses (select 'address :refresh t))))
  t)

(deftest :oodml/refresh/2
    (with-dataset *ds-employees*
      (let* ((addresses (select 'address :order-by [addressid] :flatp t :refresh t))
	     (city (slot-value (car addresses) 'city)))
	(clsql:update-records [addr]
			      :av-pairs '((city_field "A new city"))
			      :where [= [addressid] (slot-value (car addresses) 'addressid)])
	(let* ((new-addresses (select 'address :order-by [addressid] :refresh t :flatp t))
	       (new-city (slot-value (car addresses) 'city))
	       )
	  (clsql:update-records [addr]
				:av-pairs `((city_field ,city))
				:where [= [addressid] (slot-value (car addresses) 'addressid)])
	  (values (equal addresses new-addresses)
		  city
		  new-city))))
  t "Leningrad" "A new city")

(deftest :oodml/refresh/3
    (with-dataset *ds-employees*
      (let* ((addresses (select 'address :order-by [addressid] :flatp t)))
	(values
	  (equal addresses (select 'address :refresh t :flatp t))
	  (equal addresses (select 'address :flatp t)))))
  nil nil)

(deftest :oodml/refresh/4
    (with-dataset *ds-employees*
      (let* ((addresses (select 'address :order-by [addressid] :flatp t :refresh t))
	     (*db-auto-sync* t))
	(make-instance 'address :addressid 1000 :city "A new address city")
	(let ((new-addresses (select 'address :order-by [addressid] :flatp t :refresh t)))
	  (delete-records :from [addr] :where [= [addressid] 1000])
	  (values
	    (length addresses)
	    (length new-addresses)
	    (eq (first addresses) (first new-addresses))
	    (eq (second addresses) (second new-addresses))))))
  2 3 t t)


(deftest :oodml/uoj/1
    (with-dataset *ds-employees*
      (progn
	(let* ((dea-list (select 'deferred-employee-address :caching nil :order-by ["ea_join" aaddressid]
				 :flatp t))
	       (dea-list-copy (copy-seq dea-list))
	       (initially-unbound (every #'(lambda (dea) (not (slot-boundp dea 'address))) dea-list)))
	  (update-objects-joins dea-list)
	  (values
	    initially-unbound
	    (equal dea-list dea-list-copy)
	    (every #'(lambda (dea) (slot-boundp dea 'address)) dea-list)
	    (every #'(lambda (dea) (typep (slot-value dea 'address) 'address)) dea-list)
	    (mapcar #'(lambda (dea) (slot-value (slot-value dea 'address) 'addressid)) dea-list)))))
  t t t t (1 1 2 2 2))

;; update-object-joins needs to be fixed for multiple keys
#+ignore
(deftest :oodml/uoj/2
    (progn
      (clsql:update-objects-joins (list company1))
      (mapcar #'(lambda (e)
		  (slot-value e 'ecompanyid))
	      (company-employees company1)))
  (1 1 1 1 1 1 1 1 1 1))

(deftest :oodml/big/1
    (with-dataset *ds-big*
      (let ((objs (clsql:select 'big :order-by [i] :flatp t)))
	(values
	  (length objs)
	  (do ((i 0 (1+ i))
	       (max (expt 2 60))
	       (rest objs (cdr rest)))
	      ((= i (length objs)) t)
	    (let ((obj (car rest))
		  (index (1+ i)))
	      (unless (and (eql (slot-value obj 'i) index)
			   (eql (slot-value obj 'bi) (truncate max index)))
		(print index)
		(describe obj)
		(return nil)))))))
  555 t)

(deftest :oodml/db-auto-sync/1
    (with-dataset *ds-employees*
      (values
	(progn
	  (make-instance 'employee :emplid 20 :groupid 1
			 :last-name "Ivanovich")
	  (select [last-name] :from [employee] :where [= [emplid] 20]
		  :flatp t :field-names nil))
	(let ((*db-auto-sync* t))
	  (make-instance 'employee :emplid 20 :groupid 1
			 :last-name "Ivanovich")
	  (prog1 (select [last-name] :from [employee] :flatp t
			 :field-names nil
			 :where [= [emplid] 20])
	    (delete-records :from [employee] :where [= [emplid] 20])))))
  nil ("Ivanovich"))

(deftest :oodml/db-auto-sync/2
    (with-dataset *ds-employees*
      (values
	(let ((instance (make-instance 'employee :emplid 20 :groupid 1
				       :last-name "Ivanovich")))
	  (setf (slot-value instance 'last-name) "Bulgakov")
	  (select [last-name] :from [employee] :where [= [emplid] 20]
		  :flatp t :field-names nil))
	(let* ((*db-auto-sync* t)
	       (instance (make-instance 'employee :emplid 20 :groupid 1
					:last-name "Ivanovich")))
	  (setf (slot-value instance 'last-name) "Bulgakov")
	  (prog1 (select [last-name] :from [employee] :flatp t
			 :field-names nil
			 :where [= [emplid] 20])
	    (delete-records :from [employee] :where [= [emplid] 20])))))
  nil ("Bulgakov"))

(deftest :oodml/db-auto-sync/3
    (with-dataset *ds-nodes*
      (values
	(progn
	  (make-instance 'theme :title "test-theme" :vars "test-vars"
			 :doc "test-doc")
	  (select [node-id] :from [node] :where [= [title] "test-theme"]
		  :flatp t :field-names nil))
	(let ((*db-auto-sync* t))
	  (make-instance 'theme :title "test-theme" :vars "test-vars"
			 :doc "test-doc")
	  (prog1 (select [title] :from [node] :where [= [title] "test-theme"]
			 :flatp t :field-names nil)
	    (delete-records :from [node] :where [= [title] "test-theme"])
	    (delete-records :from [setting] :where [= [vars] "test-vars"])
	    (delete-records :from [theme] :where [= [doc] "test-doc"])))))
  nil ("test-theme"))

(deftest :oodml/db-auto-sync/4
    (with-dataset *ds-nodes*
      (values
	(let ((inst (make-instance 'theme
				   :title "test-theme" :vars "test-vars"
				   :doc "test-doc"))
              (*print-circle* nil))
	  (setf (slot-value inst 'title) "alternate-test-theme")
	  (format nil "~a ~a ~a ~a"
		  (or (select [title] :from [node]
                              :where [= [title] "test-theme"]
                              :flatp t :field-names nil) "NIL")
		  (or (select [vars] :from [setting]
                              :where [= [vars] "test-vars"]
                              :flatp t :field-names nil) "NIL")
		  (or (select [doc] :from [theme]
                              :where [= [doc] "test-doc"]
                              :flatp t :field-names nil) "NIL")
		  (or (select [title] :from [node]
                              :where [= [title] "alternate-test-theme"]
                              :flatp t :field-names nil) "NIL")))
	(let* ((*db-auto-sync* t)
	       (inst (make-instance 'theme
				    :title "test-theme" :vars "test-vars"
				    :doc "test-doc")))
	  (setf (slot-value inst 'title) "alternate-test-theme")
	  (prog1
	      (format nil "~a ~a ~a ~a"
		      (or (select [title] :from [node]
                                  :where [= [title] "test-theme"]
                                  :flatp t :field-names nil) "NIL")
		      (or (select [vars] :from [setting]
                                  :where [= [vars] "test-vars"]
                                  :flatp t :field-names nil) "NIL")
		      (or (select [doc] :from [theme]
                                  :where [= [doc] "test-doc"]
                                  :flatp t :field-names nil) "NIL")
		      (or (select [title] :from [node]
                                  :where [= [title] "alternate-test-theme"]
                                  :flatp t :field-names nil) "NIL"))
	    (delete-records :from [node] :where [= [title] "alternate-test-theme"])
	    (delete-records :from [setting] :where [= [vars] "test-vars"])
	    (delete-records :from [theme] :where [= [doc] "test-doc"])))))
  "NIL NIL NIL NIL"
  "NIL (test-vars) (test-doc) (alternate-test-theme)")

(deftest :oodml/setf-slot-value/1
    (with-dataset *ds-employees*
      (let* ((*db-auto-sync* t)
	     (instance (make-instance 'employee :emplid 20 :groupid 1)))
	(prog1
	    (setf
	     (slot-value instance 'first-name) "Mikhail"
	     (slot-value instance 'last-name) "Bulgakov")
	  (delete-records :from [employee] :where [= [emplid] 20]))))
  "Bulgakov")

(deftest :oodml/float/1
    (with-dataset *ds-employees*
      (let* ((emp1 (car (select 'employee
				:where [= [slot-value 'employee 'emplid]
					  1]
				:flatp t
				:caching nil)))
	     (height (slot-value emp1 'height)))
	(prog1
	    (progn
	      (setf (slot-value emp1 'height) 1.0E0)
	      (clsql:update-record-from-slot emp1 'height)
	      (= (car (clsql:select [height] :from [employee]
				    :where [= [emplid] 1]
				    :flatp t
				    :field-names nil))
		 1))
	  (setf (slot-value emp1 'height) height)
	  (clsql:update-record-from-slot emp1 'height))))
  t)

(deftest :oodml/float/2
    (with-dataset *ds-employees*
      (let* ((emp1 (car (select 'employee
				:where [= [slot-value 'employee 'emplid]
					  1]
				:flatp t
				:caching nil)))
	     (height (slot-value emp1 'height)))
	(prog1
	    (progn
	      (setf (slot-value emp1 'height) 1.0S0)
	      (clsql:update-record-from-slot emp1 'height)
	      (= (car (clsql:select [height] :from [employee]
				    :where [= [emplid] 1]
				    :flatp t
				    :field-names nil))
		 1))
	  (setf (slot-value emp1 'height) height)
	  (clsql:update-record-from-slot emp1 'height))))
  t)

(deftest :oodml/float/3
    (with-dataset *ds-employees*
      (let* ((emp1 (car (select 'employee
				:where [= [slot-value 'employee 'emplid]
					  1]
				:flatp t
				:caching nil)))
	     (height (slot-value emp1 'height)))
	(prog1
	    (progn
	      (setf (slot-value emp1 'height) 1.0F0)
	      (clsql:update-record-from-slot emp1 'height)
	      (= (car (clsql:select [height] :from [employee]
				    :where [= [emplid] 1]
				    :flatp t
				    :field-names nil))
		 1))
	  (setf (slot-value emp1 'height) height)
	  (clsql:update-record-from-slot emp1 'height))))
  t)

(deftest :oodml/float/4
    (with-dataset *ds-employees*
      (let* ((emp1 (car (select 'employee
				:where [= [slot-value 'employee 'emplid]
					  1]
				:flatp t
				:caching nil)))
	     (height (slot-value emp1 'height)))
	(prog1
	    (progn
	      (setf (slot-value emp1 'height) 1.0D0)
	      (clsql:update-record-from-slot emp1 'height)
	      (= (car (clsql:select [height] :from [employee]
				    :where [= [emplid] 1]
				    :flatp t
				    :field-names nil))
		 1))
	  (setf (slot-value emp1 'height) height)
	  (clsql:update-record-from-slot emp1 'height))))
  t)

(deftest :oodml/float/5
    (with-dataset *ds-employees*
      (let* ((emp1 (car (select 'employee
				:where [= [slot-value 'employee 'emplid]
					  1]
				:flatp t
				:caching nil)))
	     (height (slot-value emp1 'height)))
	(prog1
	    (progn
	      (setf (slot-value emp1 'height) 1.0L0)
	      (clsql:update-record-from-slot emp1 'height)
	      (= (car (clsql:select [height] :from [employee]
				    :where [= [emplid] 1]
				    :flatp t
				    :field-names nil))
		 1))
	  (setf (slot-value emp1 'height) height)
	  (clsql:update-record-from-slot emp1 'height))))
  t)
))



#.(clsql:restore-sql-reader-syntax-state)
