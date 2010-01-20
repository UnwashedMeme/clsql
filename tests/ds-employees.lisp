(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)
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



(defun initialize-ds-employees ()
  ;;  (start-sql-recording :type :both)
  (let ((*backend-warning-behavior*
         (if (member *test-database-type* '(:postgresql :postgresql-socket))
             :ignore
	     :warn)))
    (mapc #'clsql:create-view-from-class
	  '(employee company address employee-address)))
    

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
                                           :addressid 2)))

  ;; sleep to ensure birthdays are no longer at current time
  ;(sleep 1) ;want to find the test that depends on it, put the sleep there.

  #||
  ;; Lenin manages everyone		;
  (clsql:add-to-relation employee2 'manager employee1)
  (clsql:add-to-relation employee3 'manager employee1)
  (clsql:add-to-relation employee4 'manager employee1)
  (clsql:add-to-relation employee5 'manager employee1)
  (clsql:add-to-relation employee6 'manager employee1)
  (clsql:add-to-relation employee7 'manager employee1)
  (clsql:add-to-relation employee8 'manager employee1)
  (clsql:add-to-relation employee9 'manager employee1)
  (clsql:add-to-relation employee10 'manager employee1)
  ;; Everyone works for Widgets Inc.	;
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
  ;; Lenin is president of Widgets Inc.	;
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


 (def-dataset *ds-employees*
   (:setup initialize-ds-employees)
   (:cleanup (lambda ()
	       (mapc #'clsql-sys:drop-view-from-class
		     '(employee company address employee-address))
	       (ignore-errors
		 (clsql-sys:execute-command "DROP TABLE ea_join")))))

#.(clsql:restore-sql-reader-syntax-state)

