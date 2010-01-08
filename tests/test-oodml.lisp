;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-oodml.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>
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
            (mapcar #'(lambda (e) (slot-value e 'last-name))
             (clsql:select 'employee :order-by [last-name] :flatp t :caching nil))
          ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
           "Stalin" "Trotsky" "Yeltsin"))

        (deftest :oodml/select/2
            (mapcar #'(lambda (e) (slot-value e 'name))
             (clsql:select 'company :flatp t :caching nil))
          ("Widgets Inc."))

        (deftest :oodml/select/3
            (mapcar #'(lambda (e) (slot-value e 'ecompanyid))
             (clsql:select 'employee
                           :where [and [= [slot-value 'employee 'ecompanyid]
                                          [slot-value 'company 'companyid]]
                                       [= [slot-value 'company 'name]
                                          "Widgets Inc."]]
                           :flatp t
                           :caching nil))
          (1 1 1 1 1 1 1 1 1 1))

        (deftest :oodml/select/4
            (mapcar #'(lambda (e)
                        (concatenate 'string (slot-value e 'first-name)
                                     " "
                                     (slot-value e 'last-name)))
             (clsql:select 'employee :where [= [slot-value 'employee 'first-name]
                                               "Vladimir"]
                           :flatp t
                           :order-by [last-name]
                           :caching nil))
          ("Vladimir Lenin" "Vladimir Putin"))

        (deftest :oodml/select/5
            (length (clsql:select 'employee :where [married] :flatp t :caching nil))
          3)

        (deftest :oodml/select/6
            (let ((a (caar (clsql:select 'address :where [= 1 [addressid]] :caching nil))))
              (values
               (slot-value a 'street-number)
               (slot-value a 'street-name)
               (slot-value a 'city)
               (slot-value a 'postal-code)))
          10 "Park Place" "Leningrad" 123)

        (deftest :oodml/select/7
            (let ((a (caar (clsql:select 'address :where [= 2 [addressid]] :caching nil))))
              (values
               (slot-value a 'street-number)
               (slot-value a 'street-name)
               (slot-value a 'city)
               (slot-value a 'postal-code)))
          nil "" "no city" 0)

        (deftest :oodml/select/8
            (mapcar #'(lambda (e) (slot-value e 'married))
             (clsql:select 'employee :flatp t :order-by [emplid] :caching nil))
          (t t t nil nil nil nil nil nil nil))

        (deftest :oodml/select/9
            (mapcar #'(lambda (pair)
                        (list
                         (typep (car pair) 'address)
                         (typep (second pair) 'employee-address)
                         (slot-value (car pair) 'addressid)
                         (slot-value (second pair) 'aaddressid)
                         (slot-value (second pair) 'aemplid)))
             (employee-addresses employee1))
          ((t t 1 1 1) (t t 2 2 1)))

        (deftest :oodml/select/10
            (mapcar #'(lambda (pair)
                        (list
                         (typep (car pair) 'address)
                         (typep (second pair) 'employee-address)
                         (slot-value (car pair) 'addressid)
                         (slot-value (second pair) 'aaddressid)
                         (slot-value (second pair) 'aemplid)))
             (employee-addresses employee2))
          ((t t 2 2 2)))

        (deftest :oodml/select/11
         (values (mapcar #'(lambda (x) (slot-value x 'emplid))
                         (clsql:select 'employee :order-by '(([emplid] :asc))
                                       :flatp t))
          (mapcar #'(lambda (x) (slot-value x 'emplid))
           (clsql:select 'employee :order-by '(([emplid] :desc))
                         :flatp t)))
         (1 2 3 4 5 6 7 8 9 10)
         (10 9 8 7 6 5 4 3 2 1))

        ;; test retrieval of node, derived nodes etc
        (deftest :oodml/select/12
            (length (clsql:select 'node :where [node-id] :flatp t :caching nil))
          11)

        (deftest :oodml/select/13
            (let ((a (car (clsql:select 'node :where [= 1 [node-id]] :flatp t :caching nil))))
              (values
               (slot-value a 'node-id)
               (slot-value a 'title)))
          1 "Bare node")

        (deftest :oodml/select/14
            (length (clsql:select 'setting :where [setting-id] :flatp t :caching nil))
          4)

        (deftest :oodml/select/15
            (let ((a (car (clsql:select 'setting :where [= 3 [setting-id]] :flatp t :caching nil))))
              (values
               (slot-value a 'node-id)
               (slot-value a 'setting-id)
               (slot-value a 'title)
               (slot-value a 'vars)))
          3 3 "Setting2" "var 2")

        (deftest :oodml/select/16
            (length (clsql:select 'user :where [user-id] :flatp t :caching nil))
          2)

        (deftest :oodml/select/17
            (let ((a (car (clsql:select 'user :where [= 4 [user-id]] :flatp t :caching nil))))
              (values
               (slot-value a 'node-id)
               (slot-value a 'user-id)
               (slot-value a 'title)
               (slot-value a 'nick)))
          4 4 "user-1" "first user")

        (deftest :oodml/select/18
            (length (clsql:select 'theme :where [theme-id] :flatp t :caching nil))
          2)

        (deftest :oodml/select/19
         (let ((a (car (clsql:select 'theme :where [= 6 [theme-id]] :flatp t :caching nil))))
           (slot-value a 'theme-id))
         6)

        (deftest :oodml/select/20
            (let ((a (car (clsql:select 'theme :where [= 7 [theme-id]] :flatp t :caching nil))))
              (values
               (slot-value a 'node-id)
               (slot-value a 'theme-id)
               (slot-value a 'title)
               (slot-value a 'vars)
               (slot-value a 'doc)
               ))
         7 7 "theme-2"
         nil "second theme")

		;; Some tests to check weird subclassed nodes (node without own table, or subclassed of same)
        (deftest :oodml/select/21
            (let ((a (car (clsql:select 'location :where [= [title] "location-1"] :flatp t :caching nil))))
              (values
               (slot-value a 'node-id)
               (slot-value a 'title)))
          8 "location-1")

        (deftest :oodml/select/22
            (let ((a (car (clsql:select 'subloc :where [subloc-id] :flatp t :caching nil))))
              (values
               (slot-value a 'node-id)
               (slot-value a 'subloc-id)
               (slot-value a 'title)
               (slot-value a 'loc)))
          10 10 "subloc-1" "a subloc")

        ;; test retrieval is deferred
        (deftest :oodm/retrieval/1
            (every #'(lambda (e) (not (slot-boundp e 'company)))
             (select 'employee :flatp t :caching nil))
          t)

        (deftest :oodm/retrieval/2
            (every #'(lambda (e) (not (slot-boundp e 'address)))
             (select 'deferred-employee-address :flatp t :caching nil))
          t)

        ;; :retrieval :immediate should be boundp before accessed
        (deftest :oodm/retrieval/3
            (every #'(lambda (ea) (slot-boundp ea 'address))
             (select 'employee-address :flatp t :caching nil))
          t)

        (deftest :oodm/retrieval/4
            (mapcar #'(lambda (ea) (typep (slot-value ea 'address) 'address))
             (select 'employee-address :flatp t :caching nil))
          (t t t t t))

        (deftest :oodm/retrieval/5
            (mapcar #'(lambda (ea) (typep (slot-value ea 'address) 'address))
             (select 'deferred-employee-address :flatp t :caching nil))
          (t t t t t))

        (deftest :oodm/retrieval/6
            (every #'(lambda (ea) (slot-boundp (slot-value ea 'address) 'addressid))
             (select 'employee-address :flatp t :caching nil))
          t)

        (deftest :oodm/retrieval/7
            (every #'(lambda (ea) (slot-boundp (slot-value ea 'address) 'addressid))
             (select 'deferred-employee-address :flatp t :caching nil))
          t)

        (deftest :oodm/retrieval/8
            (mapcar #'(lambda (ea) (slot-value (slot-value ea 'address) 'street-number))
             (select 'employee-address :flatp t :order-by [aaddressid] :caching nil))
          (10 10 nil nil nil))

        (deftest :oodm/retrieval/9
            (mapcar #'(lambda (ea) (slot-value (slot-value ea 'address) 'street-number))
             (select 'deferred-employee-address :flatp t :order-by [aaddressid] :caching nil))
          (10 10 nil nil nil))

        ;; tests update-records-from-instance
        (deftest :oodml/update-records/1
            (values
             (progn
               (let ((lenin (car (clsql:select 'employee
                                               :where [= [slot-value 'employee 'emplid]
                                                         1]
                                               :flatp t
                                               :caching nil))))
                 (concatenate 'string
                              (first-name lenin)
                              " "
                              (last-name lenin)
                              ": "
                              (employee-email lenin))))
             (progn
               (setf (slot-value employee1 'first-name) "Dimitriy"
                     (slot-value employee1 'last-name) "Ivanovich"
                     (slot-value employee1 'email) "ivanovich@soviet.org")
               (clsql:update-records-from-instance employee1)
               (let ((lenin (car (clsql:select 'employee
                                               :where [= [slot-value 'employee 'emplid]
                                                         1]
                                               :flatp t
                                               :caching nil))))
                 (concatenate 'string
                              (first-name lenin)
                              " "
                              (last-name lenin)
                              ": "
                              (employee-email lenin))))
             (progn
               (setf (slot-value employee1 'first-name) "Vladimir"
                     (slot-value employee1 'last-name) "Lenin"
                     (slot-value employee1 'email) "lenin@soviet.org")
               (clsql:update-records-from-instance employee1)
               (let ((lenin (car (clsql:select 'employee
                                               :where [= [slot-value 'employee 'emplid]
                                                         1]
                                               :flatp t
                                               :caching nil))))
                 (concatenate 'string
                              (first-name lenin)
                              " "
                              (last-name lenin)
                              ": "
                              (employee-email lenin)))))
          "Vladimir Lenin: lenin@soviet.org"
          "Dimitriy Ivanovich: ivanovich@soviet.org"
          "Vladimir Lenin: lenin@soviet.org")

        ;; tests update-record-from-slot
        (deftest :oodml/update-records/2
            (values
             (employee-email
              (car (clsql:select 'employee
                                 :where [= [slot-value 'employee 'emplid] 1]
                                 :flatp t
                                 :caching nil)))
             (progn
               (setf (slot-value employee1 'email) "lenin-nospam@soviet.org")
               (clsql:update-record-from-slot employee1 'email)
               (employee-email
                (car (clsql:select 'employee
                                   :where [= [slot-value 'employee 'emplid] 1]
                                   :flatp t
                                   :caching nil))))
             (progn
               (setf (slot-value employee1 'email) "lenin@soviet.org")
               (clsql:update-record-from-slot employee1 'email)
               (employee-email
                (car (clsql:select 'employee
                                   :where [= [slot-value 'employee 'emplid] 1]
                                   :flatp t
                                   :caching nil)))))
          "lenin@soviet.org" "lenin-nospam@soviet.org" "lenin@soviet.org")

        ;; tests update-record-from-slots
        (deftest :oodml/update-records/3
            (values
             (let ((lenin (car (clsql:select 'employee
                                             :where [= [slot-value 'employee 'emplid]
                                                       1]
                                             :flatp t
                                             :caching nil))))
               (concatenate 'string
                            (first-name lenin)
                            " "
                            (last-name lenin)
                            ": "
                            (employee-email lenin)))
             (progn
               (setf (slot-value employee1 'first-name) "Dimitriy"
                     (slot-value employee1 'last-name) "Ivanovich"
                     (slot-value employee1 'email) "ivanovich@soviet.org")
               (clsql:update-record-from-slots employee1 '(first-name last-name email))
               (let ((lenin (car (clsql:select 'employee
                                               :where [= [slot-value 'employee 'emplid]
                                                         1]
                                               :flatp t
                                               :caching nil))))
                 (concatenate 'string
                              (first-name lenin)
                              " "
                              (last-name lenin)
                              ": "
                              (employee-email lenin))))
             (progn
               (setf (slot-value employee1 'first-name) "Vladimir"
                     (slot-value employee1 'last-name) "Lenin"
                     (slot-value employee1 'email) "lenin@soviet.org")
               (clsql:update-record-from-slots employee1 '(first-name last-name email))
               (let ((lenin (car (clsql:select 'employee
                                               :where [= [slot-value 'employee 'emplid]
                                                         1]
                                               :flatp t
                                               :caching nil))))
                 (concatenate 'string
                              (first-name lenin)
                              " "
                              (last-name lenin)
                              ": "
                              (employee-email lenin)))))
          "Vladimir Lenin: lenin@soviet.org"
          "Dimitriy Ivanovich: ivanovich@soviet.org"
          "Vladimir Lenin: lenin@soviet.org")

        (deftest :oodml/update-records/4
	  (flet ((print-fresh-node ()
		   (let ((base (car (clsql:select 'node
				      :where [= [slot-value 'node 'node-id]
						1]
				      :flatp t
				      :caching nil))))
		     (format nil "~a ~a"
			     (slot-value base 'node-id)
			     (slot-value base 'title)))))
	    (values
	      (print-fresh-node)
	      (let ((base (car (clsql:select 'node
				 :where [= [slot-value 'node 'node-id]
					   1]
				 :flatp t
				 :caching nil))))
		(setf (slot-value base 'title) "Altered title")
		(clsql:update-records-from-instance base)
		(print-fresh-node))
	      (let ((base (car (clsql:select 'node
				 :where [= [slot-value 'node 'node-id]
					   1]
				 :flatp t
				 :caching nil))))
		(setf (slot-value base 'title) "Bare node")
		(clsql:update-records-from-instance base)
		(print-fresh-node))))
          "1 Bare node"
          "1 Altered title"
          "1 Bare node")

        (deftest :oodml/update-records/5
	  (flet ((print-fresh-setting ()
		   (let ((node (car (clsql:select 'setting
				      :where [= [slot-value 'setting 'setting-id]
						3]
				      :flatp t
				      :caching nil))))
		     (format nil "~a ~a ~a"
			     (slot-value node 'setting-id)
			     (slot-value node 'title)
			     (slot-value node 'vars)))))
	    (values
	      (print-fresh-setting)
	      (let ((node (car (clsql:select 'setting
				 :where [= [slot-value 'setting 'setting-id]
					   3]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "Altered title")
		(setf (slot-value node 'vars) "Altered vars")
		(clsql:update-records-from-instance node)
		(print-fresh-setting))
	      (let ((node (car (clsql:select 'setting
				 :where [= [slot-value 'setting 'setting-id]
					   3]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "Setting2")
		(setf (slot-value node 'vars) "var 2")
		(clsql:update-records-from-instance node)
		(print-fresh-setting))))
          "3 Setting2 var 2"
          "3 Altered title Altered vars"
          "3 Setting2 var 2")

        (deftest :oodml/update-records/6
	  (flet ((print-fresh-setting ()
		   (let ((node (car (clsql:select 'setting
				      :where [= [slot-value 'setting 'setting-id]
						7]
				      :flatp t
				      :caching nil))))
		     (format nil "~a ~a ~a"
			     (slot-value node 'setting-id)
			     (slot-value node 'title)
			     (slot-value node 'vars)))
		   ))
	    (values
	      (print-fresh-setting)
	      (let ((node (car (clsql:select 'setting
				 :where [= [slot-value 'setting 'setting-id]
					   7]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "Altered title")
		(setf (slot-value node 'vars) "Altered vars")
		(clsql:update-records-from-instance node)
		(print-fresh-setting))
	      (let ((node (car (clsql:select 'setting
				 :where [= [slot-value 'setting 'setting-id]
					   7]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "theme-2")
		(setf (slot-value node 'vars) nil)
		(clsql:update-records-from-instance node)
		(print-fresh-setting))))
          "7 theme-2 NIL"
          "7 Altered title Altered vars"
          "7 theme-2 NIL")

        (deftest :oodml/update-records/7
         (flet ((print-fresh-user ()
		  "requery to get what the db has, and print out."
		  (let ((node (car (clsql:select 'user
				     :where [= [slot-value 'user 'user-id]
					       5]
				     :flatp t
				     :caching nil))))
		    (format nil "~a ~a ~a"
			    (slot-value node 'user-id)
			    (slot-value node 'title)
			    (slot-value node 'nick)))))
	   (values
	     (print-fresh-user)
	     (let ((node (car (clsql:select 'user
				:where [= [slot-value 'user 'user-id]
					  5]
				:flatp t
				:caching nil))))
	       (setf (slot-value node 'title) "Altered title")
	       (setf (slot-value node 'nick) "Altered nick")
	       (clsql:update-records-from-instance node)
	       (print-fresh-user))
	     (let ((node (car (clsql:select 'user
				:where [= [slot-value 'user 'user-id]
					  5]
				:flatp t
				:caching nil))))
	       (setf (slot-value node 'title) "user-2")
	       (setf (slot-value node 'nick) "second user")
	       (clsql:update-records-from-instance node)
	       (print-fresh-user))))
          "5 user-2 second user"
          "5 Altered title Altered nick"
          "5 user-2 second user")

        (deftest :oodml/update-records/8
	  (flet ((print-fresh-theme ()
		   (let ((node (car (clsql:select 'theme
				      :where [= [slot-value 'theme 'theme-id]
						6]
				      :flatp t
				      :caching nil))))
		     (format nil "~a ~a ~a ~a ~a ~a"
			     (slot-value node 'node-id)
			     (slot-value node 'setting-id)
			     (slot-value node 'theme-id)
			     (slot-value node 'title)
			     (slot-value node 'vars)
			     (slot-value node 'doc)))))
	    (values
	      (print-fresh-theme)
	      (let ((node (car (clsql:select 'setting
				 :where [= [slot-value 'setting 'setting-id]
					   6]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "Altered title")
		(setf (slot-value node 'vars) nil)
		(clsql:update-records-from-instance node)
		(print-fresh-theme))
	      (let ((node (car (clsql:select 'theme
				 :where [= [slot-value 'theme 'theme-id]
					   6]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "Altered title again")
		(setf (slot-value node 'doc) "altered doc")
		(clsql:update-records-from-instance node)
		(print-fresh-theme))
	      (let ((node (car (clsql:select 'theme
				 :where [= [slot-value 'theme 'theme-id]
					   6]
				 :flatp t
				 :caching nil))))
		(setf (slot-value node 'title) "theme-1")
		(setf (slot-value node 'vars) "empty")
		(setf (slot-value node 'doc) "first theme")
		(clsql:update-records-from-instance node)
		(print-fresh-theme))))
          "6 6 6 theme-1 empty first theme"
	  "6 6 6 Altered title NIL first theme"
          "6 6 6 Altered title again NIL altered doc"
          "6 6 6 theme-1 empty first theme")

	(deftest :oodml/update-records/9
	  (flet ((print-fresh-subloc ()
		   (let ((sl (car (clsql:select 'subloc
				    :where [= [slot-value 'subloc 'subloc-id]
					      10]
				    :flatp t
				    :caching nil))))
		     (format nil "~a ~a ~a"
			     (slot-value sl 'subloc-id)
			     (slot-value sl 'title)
			     (slot-value sl 'loc)))))
	    (values
	      (print-fresh-subloc)
	      (let ((sl (car (clsql:select 'subloc
			       :where [= [slot-value 'subloc 'subloc-id]
					 10]
			       :flatp t
			       :caching nil))))
		(setf (slot-value sl 'title) "Altered subloc title")
		(setf (slot-value sl 'loc) "Altered loc")
		(clsql:update-records-from-instance sl)
		(print-fresh-subloc))
	      (let ((sl (car (clsql:select 'subloc
			       :where [= [slot-value 'subloc 'subloc-id]
					 10]
			       :flatp t
			       :caching nil))))
		(setf (slot-value sl 'title) "subloc-1")
		(setf (slot-value sl 'loc) "a subloc")
		(clsql:update-records-from-instance sl)
		(print-fresh-subloc))))
          "10 subloc-1 a subloc"
          "10 Altered subloc title Altered loc"
          "10 subloc-1 a subloc")

        ;; tests update-instance-from-records
        (deftest :oodml/update-instance/1
            (values
             (concatenate 'string
                          (slot-value employee1 'first-name)
                          " "
                          (slot-value employee1 'last-name)
                          ": "
                          (slot-value employee1 'email))
             (progn
               (clsql:update-records [employee]
                                     :av-pairs '(([first-name] "Ivan")
                                                 ([last-name] "Petrov")
                                                 ([email] "petrov@soviet.org"))
                                     :where [= [emplid] 1])
               (clsql:update-instance-from-records employee1)
               (concatenate 'string
                            (slot-value employee1 'first-name)
                            " "
                            (slot-value employee1 'last-name)
                            ": "
                            (slot-value employee1 'email)))
             (progn
               (clsql:update-records [employee]
                                     :av-pairs '(([first-name] "Vladimir")
                                                 ([last-name] "Lenin")
                                                 ([email] "lenin@soviet.org"))
                                     :where [= [emplid] 1])
               (clsql:update-instance-from-records employee1)
               (concatenate 'string
                            (slot-value employee1 'first-name)
                            " "
                            (slot-value employee1 'last-name)
                            ": "
                            (slot-value employee1 'email))))
          "Vladimir Lenin: lenin@soviet.org"
          "Ivan Petrov: petrov@soviet.org"
          "Vladimir Lenin: lenin@soviet.org")

        ;; tests update-slot-from-record
        (deftest :oodml/update-instance/2
            (values
             (slot-value employee1 'email)
             (progn
               (clsql:update-records [employee]
                                     :av-pairs '(([email] "lenin-nospam@soviet.org"))
                                     :where [= [emplid] 1])
               (clsql:update-slot-from-record employee1 'email)
               (slot-value employee1 'email))
             (progn
               (clsql:update-records [employee]
                                     :av-pairs '(([email] "lenin@soviet.org"))
                                     :where [= [emplid] 1])
               (clsql:update-slot-from-record employee1 'email)
               (slot-value employee1 'email)))
          "lenin@soviet.org" "lenin-nospam@soviet.org" "lenin@soviet.org")

        ;; tests normalisedp update-instance-from-records
        (deftest :oodml/update-instance/3
            (values
             (with-output-to-string (out)
               (format out "~a ~a ~a ~a"
                       (slot-value theme2 'theme-id)
                       (slot-value theme2 'title)
                       (slot-value theme2 'vars)
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
                         (slot-value theme2 'doc))))
             (progn
               (clsql:update-records [node] :av-pairs '(([title] "theme-2"))
                                     :where [= [node-id] 7])
               (clsql:update-records [setting] :av-pairs '(([vars] nil))
                                     :where [= [setting-id] 7])
               (clsql:update-records [theme] :av-pairs '(([doc] "second theme"))
                                     :where [= [theme-id] 7])
               (clsql:update-instance-from-records theme2)
               (with-output-to-string (out)
                 (format out "~a ~a ~a ~a"
                         (slot-value theme2 'theme-id)
                         (slot-value theme2 'title)
                         (slot-value theme2 'vars)
                         (slot-value theme2 'doc)))))
          "7 theme-2 NIL second theme"
          "7 Altered title Altered vars Altered doc"
          "7 theme-2 NIL second theme")

        (deftest :oodml/update-instance/4
		   (values
			(progn
			  (setf loc2 (car (clsql:select 'location
											:where [= [node-id] 9]
											:flatp t :caching nil)))
			  (with-output-to-string (out)
				(format out "~a ~a"
						(slot-value loc2 'node-id)
						(slot-value loc2 'title))))
			(progn
			  (clsql:update-records [node] :av-pairs '(([title] "Altered title"))
									:where [= [node-id] 9])
			  (clsql:update-instance-from-records loc2)
			  (with-output-to-string (out)
				(format out "~a ~a"
						(slot-value loc2 'node-id)
						(slot-value loc2 'title))))
			(progn
			  (clsql:update-records [node] :av-pairs '(([title] "location-2"))
									:where [= [node-id] 9])
			  (clsql:update-instance-from-records loc2)
			  (with-output-to-string (out)
				(format out "~a ~a"
						(slot-value loc2 'node-id)
						(slot-value loc2 'title)))))
          "9 location-2"
          "9 Altered title"
          "9 location-2")

        (deftest :oodml/update-instance/5
            (values
             (with-output-to-string (out)
               (format out "~a ~a ~a"
                       (slot-value subloc2 'subloc-id)
                       (slot-value subloc2 'title)
                       (slot-value subloc2 'loc)))
             (progn
               (clsql:update-records [node] :av-pairs '(([title] "Altered title"))
                                     :where [= [node-id] 11])
               (clsql:update-records [subloc] :av-pairs '(([loc] "Altered loc"))
                                     :where [= [subloc-id] 11])
               (clsql:update-instance-from-records subloc2)
               (with-output-to-string (out)
                 (format out "~a ~a ~a"
                         (slot-value subloc2 'subloc-id)
                         (slot-value subloc2 'title)
                         (slot-value subloc2 'loc))))
             (progn
               (clsql:update-records [node] :av-pairs '(([title] "subloc-2"))
                                     :where [= [node-id] 11])
               (clsql:update-records [subloc] :av-pairs '(([loc] "second subloc"))
                                     :where [= [subloc-id] 11])
               (clsql:update-instance-from-records subloc2)
               (with-output-to-string (out)
                 (format out "~a ~a ~a"
                         (slot-value subloc2 'subloc-id)
                         (slot-value subloc2 'title)
                         (slot-value subloc2 'loc)))))
          "11 subloc-2 second subloc"
          "11 Altered title Altered loc"
          "11 subloc-2 second subloc")

        ;; tests update-slot-from-record with normalisedp stuff
        (deftest :oodml/update-instance/6
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
               (slot-value theme1 'vars))
             (progn
               (clsql:update-records [theme]
                                     :av-pairs '(([doc] "first theme"))
                                     :where [= [theme-id] 6])
               (clsql:update-slot-from-record theme1 'doc)
               (slot-value theme1 'doc))
             (progn
               (clsql:update-records [setting]
                                     :av-pairs '(([vars] "empty"))
                                     :where [= [setting-id] 6])
               (clsql:update-slot-from-record theme1 'vars)
               (slot-value theme1 'vars)))
         "first theme" "empty"
         "altered doc" "altered vars"
         "first theme" "empty")

        (deftest :oodml/update-instance/7
            (values
             (slot-value loc2 'title)
             (slot-value subloc2 'loc)
             (progn
               (clsql:update-records [node]
                                     :av-pairs '(([title] "altered title"))
                                     :where [= [node-id] 9])
               (clsql:update-slot-from-record loc2 'title)
               (slot-value loc2 'title))
             (progn
               (clsql:update-records [subloc]
                                     :av-pairs '(([loc] "altered loc"))
                                     :where [= [subloc-id] 11])
               (clsql:update-slot-from-record subloc2 'loc)
               (slot-value subloc2 'loc))
             (progn
               (clsql:update-records [node]
                                     :av-pairs '(([title] "location-2"))
                                     :where [= [node-id] 9])
               (clsql:update-slot-from-record loc2 'title)
               (slot-value loc2 'title))
             (progn
               (clsql:update-records [subloc]
                                     :av-pairs '(([loc] "second subloc"))
                                     :where [= [subloc-id] 11])
               (clsql:update-slot-from-record subloc2 'loc)
               (slot-value subloc2 'loc)))
         "location-2" "second subloc"
		 "altered title" "altered loc"
         "location-2" "second subloc")

        (deftest :oodml/do-query/1
            (let ((result '()))
              (clsql:do-query ((e) [select 'employee :order-by [emplid]])
                (push (slot-value e 'last-name) result))
              result)
          ("Putin" "Yeltsin" "Gorbachev" "Chernenko" "Andropov" "Brezhnev" "Kruschev"
           "Trotsky" "Stalin" "Lenin"))

        (deftest :oodml/do-query/2
            (let ((result '()))
              (clsql:do-query ((e c) [select 'employee 'company
                                             :where [= [slot-value 'employee 'last-name]
                                                       "Lenin"]])
                (push (list (slot-value e 'last-name) (slot-value c 'name))
                      result))
              result)
          (("Lenin" "Widgets Inc.")))

        (deftest :oodml/map-query/1
            (clsql:map-query 'list #'last-name [select 'employee :order-by [emplid]])
          ("Lenin" "Stalin" "Trotsky" "Kruschev" "Brezhnev" "Andropov" "Chernenko"
           "Gorbachev" "Yeltsin" "Putin"))

        (deftest :oodml/map-query/2
            (clsql:map-query 'list #'(lambda (e c) (list (slot-value e 'last-name)
                                                         (slot-value c 'name)))
             [select 'employee 'company :where [= [slot-value 'employee 'last-name]
                                                  "Lenin"]])
          (("Lenin" "Widgets Inc.")))

        (deftest :oodml/iteration/3
            (loop for (e) being the records in
             [select 'employee :where [< [emplid] 4] :order-by [emplid]]
             collect (slot-value e 'last-name))
          ("Lenin" "Stalin" "Trotsky"))


      (deftest :oodml/cache/1
          (progn
            (setf (clsql-sys:record-caches *default-database*) nil)
            (let ((employees (select 'employee)))
              (every #'(lambda (a b) (eq a b))
                     employees (select 'employee))))
        t)

        (deftest :oodml/cache/2
            (let ((employees (select 'employee)))
              (equal employees (select 'employee :flatp t)))
          nil)

        (deftest :oodml/refresh/1
            (let ((addresses (select 'address)))
              (equal addresses (select 'address :refresh t)))
          t)

        (deftest :oodml/refresh/2
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
                        new-city)))
          t "Leningrad" "A new city")

        (deftest :oodml/refresh/3
            (let* ((addresses (select 'address :order-by [addressid] :flatp t)))
              (values
               (equal addresses (select 'address :refresh t :flatp t))
               (equal addresses (select 'address :flatp t))))
          nil nil)

        (deftest :oodml/refresh/4
            (let* ((addresses (select 'address :order-by [addressid] :flatp t :refresh t))
                   (*db-auto-sync* t))
              (make-instance 'address :addressid 1000 :city "A new address city")
              (let ((new-addresses (select 'address :order-by [addressid] :flatp t :refresh t)))
                (delete-records :from [addr] :where [= [addressid] 1000])
                (values
                 (length addresses)
                 (length new-addresses)
                 (eq (first addresses) (first new-addresses))
                 (eq (second addresses) (second new-addresses)))))
          2 3 t t)


        (deftest :oodml/uoj/1
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
                 (mapcar #'(lambda (dea) (slot-value (slot-value dea 'address) 'addressid)) dea-list))))
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
                     (return nil))))))
          555 t)

        (deftest :oodml/db-auto-sync/1
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
                 (delete-records :from [employee] :where [= [emplid] 20]))))
          nil ("Ivanovich"))

        (deftest :oodml/db-auto-sync/2
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
                 (delete-records :from [employee] :where [= [emplid] 20]))))
          nil ("Bulgakov"))

        (deftest :oodml/db-auto-sync/3
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
                  (delete-records :from [theme] :where [= [doc] "test-doc"]))))
          nil ("test-theme"))

        (deftest :oodml/db-auto-sync/4
            (values
              (let ((inst (make-instance 'theme
                                         :title "test-theme" :vars "test-vars"
                                         :doc "test-doc")))
                (setf (slot-value inst 'title) "alternate-test-theme")
                (with-output-to-string (out)
                  (format out "~a ~a ~a ~a"
                          (select [title] :from [node]
                                  :where [= [title] "test-theme"]
                                  :flatp t :field-names nil)
                          (select [vars] :from [setting]
                                  :where [= [vars] "test-vars"]
                                  :flatp t :field-names nil)
                          (select [doc] :from [theme]
                                  :where [= [doc] "test-doc"]
                                  :flatp t :field-names nil)
                          (select [title] :from [node]
                                  :where [= [title] "alternate-test-theme"]
                                  :flatp t :field-names nil))))
             (let* ((*db-auto-sync* t)
                    (inst (make-instance 'theme
                                         :title "test-theme" :vars "test-vars"
                                         :doc "test-doc")))
                (setf (slot-value inst 'title) "alternate-test-theme")
                (prog1
                (with-output-to-string (out)
                  (format out "~a ~a ~a ~a"
                          (select [title] :from [node]
                                  :where [= [title] "test-theme"]
                                  :flatp t :field-names nil)
                          (select [vars] :from [setting]
                                  :where [= [vars] "test-vars"]
                                  :flatp t :field-names nil)
                          (select [doc] :from [theme]
                                  :where [= [doc] "test-doc"]
                                  :flatp t :field-names nil)
                          (select [title] :from [node]
                                  :where [= [title] "alternate-test-theme"]
                                  :flatp t :field-names nil)))
                  (delete-records :from [node] :where [= [title] "alternate-test-theme"])
                  (delete-records :from [setting] :where [= [vars] "test-vars"])
                  (delete-records :from [theme] :where [= [doc] "test-doc"]))))
         "NIL NIL NIL NIL"
         "NIL (test-vars) (test-doc) (alternate-test-theme)")

        (deftest :oodml/setf-slot-value/1
            (let* ((*db-auto-sync* t)
                   (instance (make-instance 'employee :emplid 20 :groupid 1)))
              (prog1
                  (setf
                   (slot-value instance 'first-name) "Mikhail"
                   (slot-value instance 'last-name) "Bulgakov")
                (delete-records :from [employee] :where [= [emplid] 20])))
          "Bulgakov")

        (deftest :oodml/float/1
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
                (clsql:update-record-from-slot emp1 'height)))
         t)

        (deftest :oodml/float/2
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
                (clsql:update-record-from-slot emp1 'height)))
         t)

        (deftest :oodml/float/3
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
                (clsql:update-record-from-slot emp1 'height)))
          t)

        (deftest :oodml/float/4
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
                (clsql:update-record-from-slot emp1 'height)))
         t)

        (deftest :oodml/float/5
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
                (clsql:update-record-from-slot emp1 'height)))
         t)))



#.(clsql:restore-sql-reader-syntax-state)
