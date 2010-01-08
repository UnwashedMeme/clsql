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

(setq *rt-fddl*
      '(

;; list current tables
(deftest :fddl/table/1
    (sort (mapcar #'string-downcase
                  (clsql:list-tables :owner *test-database-user*))
     #'string<)
  ("addr" "big" "company" "ea_join" "employee" "node" "setting"
   "subloc" "theme" "type_bigint" "type_table" "user"))

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
           (sort
            (mapcar #'string-downcase
                    (clsql:list-attributes [employee]
                                           :owner *test-database-user*))
            #'string<))
  "bd_utime" "birthday" "ecompanyid" "email" "emplid" "first_name" "groupid" "height"
  "last_name" "managerid" "married")

(deftest :fddl/attributes/2
    (apply #'values
           (sort
            (mapcar #'(lambda (a) (string-downcase (car a)))
                    (clsql:list-attribute-types [employee]
                                               :owner *test-database-user*))
            #'string<))
  "bd_utime" "birthday" "ecompanyid" "email" "emplid" "first_name" "groupid" "height"
  "last_name" "managerid" "married")

;; Attribute types are vendor specific so need to test a range
(deftest :fddl/attributes/3
    (and (member (clsql:attribute-type [emplid] [employee]) '(:int :integer :int4 :number)) t)
  t)

(deftest :fddl/attributes/4
    (multiple-value-bind (type length scale nullable)
        (clsql:attribute-type [first-name] [employee])
      (values (clsql-sys:in type :varchar :varchar2) length scale nullable))
  t 30 nil 1)

(deftest :fddl/attributes/5
    (and (member (clsql:attribute-type [birthday] [employee]) '(:datetime :timestamp :date)) t)
  t)

(deftest :fddl/attributes/6
    (and (member (clsql:attribute-type [height] [employee]) '(:float :float8 :number)) t)
  t)

(deftest :fddl/attributes/7
    (and (member (clsql:attribute-type [bd_utime] [employee]) '(:bigint :int8 :char)) t)
  t)


;; create a view, test for existence, drop it and test again
(deftest :fddl/view/1
    (progn (clsql:create-view [lenins-group]
                              :as [select [first-name] [last-name] [email]
                                          :from [employee]
                                          :where [= [managerid] 1]])
           (values
            (clsql:view-exists-p [lenins-group] :owner *test-database-user*)
            (progn
              (clsql:drop-view [lenins-group] :if-does-not-exist :ignore)
              (clsql:view-exists-p [lenins-group] :owner *test-database-user*))))
  t nil)

  ;; create a view, list its attributes and drop it
(when (clsql-sys:db-type-has-views? *test-database-underlying-type*)
  (deftest :fddl/view/2
      (progn (clsql:create-view [lenins-group]
                                :as [select [first-name] [last-name] [email]
                                            :from [employee]
                                            :where [= [managerid] 1]])
             (prog1
                 (sort (mapcar #'string-downcase
                               (clsql:list-attributes [lenins-group]))
                       #'string<)
               (clsql:drop-view [lenins-group] :if-does-not-exist :ignore)))
    ("email" "first_name" "last_name")))

  ;; create a view, select stuff from it and drop it
(deftest :fddl/view/3
    (progn (clsql:create-view [lenins-group]
                              :as [select [first-name] [last-name] [email]
                                          :from [employee]
                                          :where [= [managerid] 1]])
           (let ((result
                  (list
                   ;; Shouldn't exist
                   (clsql:select [first-name] [last-name] [email]
                                 :from [lenins-group]
                                 :where [= [last-name] "Lenin"])
                   ;; Should exist
                   (car (clsql:select [first-name] [last-name] [email]
                                      :from [lenins-group]
                                      :where [= [last-name] "Stalin"])))))
             (clsql:drop-view [lenins-group] :if-does-not-exist :ignore)
             (apply #'values result)))
  nil ("Josef" "Stalin" "stalin@soviet.org"))

(deftest :fddl/view/4
    (progn (clsql:create-view [lenins-group]
                              :column-list '([forename] [surname] [email])
                              :as [select [first-name] [last-name] [email]
                                          :from [employee]
                                          :where [= [managerid] 1]])
           (let ((result
                  (list
                   ;; Shouldn't exist
                   (clsql:select [forename] [surname] [email]
                                 :from [lenins-group]
                                 :where [= [surname] "Lenin"])
                   ;; Should exist
                   (car (clsql:select [forename] [surname] [email]
                                      :from [lenins-group]
                                      :where [= [surname] "Stalin"])))))
             (clsql:drop-view [lenins-group] :if-does-not-exist :ignore)
             (apply #'values result)))
  nil ("Josef" "Stalin" "stalin@soviet.org"))

;; create an index, test for existence, drop it and test again
(deftest :fddl/index/1
    (progn (clsql:create-index [bar] :on [employee] :attributes
                              '([first-name] [last-name] [email]) :unique t)
           (values
            (clsql:index-exists-p [bar] :owner *test-database-user*)
            (progn
              (clsql:drop-index [bar] :on [employee]
                                :if-does-not-exist :ignore)
              (clsql:index-exists-p [bar] :owner *test-database-user*))))
  t nil)

;; create indexes with names as strings, symbols and in square brackets
(deftest :fddl/index/2
    (let ((names '("foo" foo [foo]))
          (result '()))
      (dolist (name names)
        (clsql:create-index name :on [employee] :attributes '([last-name]))
        (push (clsql:index-exists-p name :owner *test-database-user*) result)
        (clsql:drop-index name :on [employee] :if-does-not-exist :ignore))
      (apply #'values result))
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

(deftest :fddl/big/1
    (let ((rows (clsql:select [*] :from [big] :order-by [i] :field-names nil)))
      (values
       (length rows)
       (do ((i 0 (1+ i))
            (max (expt 2 60))
            (rest rows (cdr rest)))
           ((= i (length rows)) t)
         (let ((index (1+ i))
               (int (first (car rest)))
               (bigint (second (car rest))))
           (when (and (or (eq *test-database-type* :oracle)
                          (and (eq *test-database-type* :odbc)
                               (eq *test-database-underlying-type* :postgresql)))
                      (stringp bigint))
             (setf bigint (parse-integer bigint)))
           (unless (and (eql int index)
                        (eql bigint (truncate max index)))
             (return nil))))))
  555 t)

(deftest :fddl/owner/1
    (and
     ;; user tables are an improper subset of all tables
     (= (length (intersection (clsql:list-tables :owner nil)
                              (clsql:list-tables :owner :all)
                              :test #'string=))
        (length (clsql:list-tables :owner nil)))
     ;; user tables are a proper subset of all tables
     (> (length (clsql:list-tables :owner :all))
        (length (clsql:list-tables :owner nil))))
  t)

(deftest :fddl/cache-table-queries/1
 (list
  (gethash "EMPLOYEE" (clsql-sys::attribute-cache clsql:*default-database*))
  (progn
    (clsql:cache-table-queries "EMPLOYEE" :action t)
    (gethash "EMPLOYEE" (clsql-sys::attribute-cache clsql:*default-database*)))
  (progn
    (clsql:list-attribute-types "EMPLOYEE")
    (not
     (null
      (cadr
       (gethash "EMPLOYEE"
                (clsql-sys::attribute-cache clsql:*default-database*))))))
  (progn
    (clsql:cache-table-queries "EMPLOYEE" :action :flush)
    (gethash "EMPLOYEE" (clsql-sys::attribute-cache clsql:*default-database*))))
 (nil (t nil) t (t nil)))

  ))

#.(clsql:restore-sql-reader-syntax-state)
