(in-package #:clsql-tests)

(clsql-sys:file-enable-sql-reader-syntax)

(defparameter basenode nil)
(defparameter derivednode1 nil)
(defparameter derivednode2 nil)
(defparameter node nil)
(defparameter setting1 nil)
(defparameter setting2 nil)
(defparameter user1 nil)
(defparameter user2 nil)
(defparameter theme1 nil)
(defparameter theme2 nil)
(defparameter loc1 nil)
(defparameter loc2 nil)
(defparameter subloc1 nil)
(defparameter subloc2 nil)


;; classes for testing the normalizedp stuff
(def-view-class node ()
  ((node-id :accessor node-id :initarg :node-id
            :type integer :db-kind :key
            :db-constraints (:not-null :auto-increment))
   (title :accessor title :initarg :title :type (varchar 240))
   (createtime :accessor createtime :initarg :createtime :type wall-time
               :db-constraints (:not-null) :initform (get-time))
   (modifiedtime :accessor modifiedtime :initarg :modifiedtime :type wall-time
                 :initform (make-time :year 1900 :month 1 :day 1))))

(def-view-class setting (node)
  ((setting-id :accessor setting-id :initarg :setting-id
               :type integer :db-kind :key :db-constraints (:not-null :auto-increment))
   (vars :accessor vars :initarg :vars :type (varchar 240)))
  (:normalizedp t))

(def-view-class user (node)
  ((user-id :accessor user-id :initarg :user-id
            :type integer :db-kind :key :db-constraints (:not-null :auto-increment))
   (nick :accessor nick :initarg :nick :type (varchar 64)))
  (:base-table "nodeuser")
  (:normalizedp t))

(def-view-class theme (setting)
  ((theme-id :accessor theme-id :initarg :theme-id
             :type integer :db-kind :key :db-constraints (:not-null :auto-increment))
   (doc :accessor doc :initarg :doc :type (varchar 240)))
  (:normalizedp t))

;; A class that uses only a superclass db table
(def-view-class location (node)
  ()
  (:base-table node)
  (:normalizedp t))

(def-view-class subloc (location)
  ((subloc-id :accessor subloc-id :initarg :subloc-id
	      :type integer :db-kind :key :db-constraints (:not-null :auto-increment))
   (loc :accessor loc :initarg :loc :type (varchar 64)))
  (:normalizedp t))



(defun initialize-ds-nodes ()
  ;;  (start-sql-recording :type :both)
  (let ((*backend-warning-behavior*
         (if (member *test-database-type* '(:postgresql :postgresql-socket))
             :ignore
	     :warn)))
    (mapc #'clsql:create-view-from-class
	  '(node setting user theme location subloc)))

  (setq *test-start-utime* (get-universal-time))
  (let* ((*db-auto-sync* t))
    (setf  node (make-instance 'node
                              :title "Bare node")
          setting1 (make-instance 'setting
                                  :title "Setting1"
                                  :vars "var 1")
          setting2 (make-instance 'setting
                                  :title "Setting2"
                                  :vars "var 2")
          user1 (make-instance 'user
                               :title "user-1"
                               :nick "first user")
          user2 (make-instance 'user
                               :title "user-2"
                               :nick "second user")
          theme1 (make-instance 'theme
                                :title "theme-1"
                                :vars "empty"
                                :doc "first theme")
          theme2 (make-instance 'theme
                                :title "theme-2"
                                :doc "second theme")
	  loc1 (make-instance 'location
                              :title "location-1")
	  loc2 (make-instance 'location
                              :title "location-2")
	  subloc1 (make-instance 'subloc
				 :title "subloc-1"
				 :loc "a subloc")
	  subloc2 (make-instance 'subloc
				 :title "subloc-2"
				 :loc "second subloc"))))




 (def-dataset *ds-nodes*
   (:setup initialize-ds-nodes)
   (:cleanup (lambda ()
	       (mapc #'clsql-sys:drop-view-from-class
		     '(node setting user theme location subloc))
	       (ignore-errors
		 (clsql-sys:execute-command "DROP TABLE nodeuser")
		 (mapc #'clsql-sys:drop-sequence
		       '(node_node_id_seq setting_setting_id_seq subloc_subloc_id_seq
			 theme_theme_id_seq nodeuser_user_id_seq)
		       )))))

