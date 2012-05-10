

(eval-when (load eval compile)
  (require 'clsql)
  (clsql:push-library-path "/opt/local/lib/mysql5/mysql/")
  (require 'clsql-mysql))

(use-package :clsql)

(defparameter *connection-spec* '(nil "test" "test" "password"))
(defparameter *database-type* :mysql)

(def-view-class bunny ()
  ((bunny-id :accessor bunny-id :initarg :bunny-id
            :type integer :db-kind :key :db-constraints (:not-null))
   (nick :accessor nick :initarg :nick :type (varchar 64))))


(defun test-probe-database ()
  (probe-database *connection-spec* :database-type *database-type*))

(defun make-test-table ()
  (with-database  (db *connection-spec* :database-type *database-type* :if-exists :old)
    (when (not (table-exists-p 'usr :database db))
      (create-view-from-class 'bunny :database db))))

(defun add-bunny (&optional n)
   (with-database  (db *connection-spec* :database-type *database-type* :if-exists :old)
     (let ((bunny1 (make-instance 'bunny :bunny-id n :nick (format nil "bunny~A" n))))
       (update-records-from-instance bunny1 :database db))))

#.(clsql:restore-sql-reader-syntax-state)
#.(clsql:locally-enable-sql-reader-syntax)
(defun get-bunny (n &key (pool nil) (recording nil))
  (with-database  (db *connection-spec* :database-type *database-type* :if-exists :old :pool pool)
    (when recording (clsql:start-sql-recording :database db))
    ;(describe db)
    (clsql:query
     (clsql:sql-operation 
      'select 'bunny
      :where [= [bunny bunny-id] n]
      :flatp t)
     :database db)))

(defun get-bunny-raw (n)
  (with-database  (db *connection-spec* :database-type *database-type* :if-exists :old)
    (clsql:query (format nil "select * from BUNNY where BUNNY_ID=~D" n) :database db)))

#.(clsql:locally-disable-sql-reader-syntax)


(defparameter *bunnyN* nil)

;; test function that binds *bunnyN* to bunny #n, and launches a bunch
;; of competing threads that modifies NICK to bunny thread-number-NNN by using the
;; DATABASE-DESC mechanism.  Prints new values of BUNNYN's nick
;; in a loop so we can see the results of each thread.
(defun test-mod-in-thread (n &key (nthreads 20)  (pool nil) (recording nil))
  (ignore-errors (make-test-table))
  (ignore-errors (add-bunny n))
  (setf *bunnyN* (first (get-bunny n :pool pool :recording recording)))
  ;; recording may have been left ON
  (when (not recording)
    (clsql:stop-sql-recording  :database (clsql-sys::view-database *bunnyN*)))
  (format t "START: BunnyN name is ~A~%" (nick *bunnyN*))
  (loop for i below nthreads
	do
     (let ((ii i))
       (sb-thread:make-thread 
	(lambda ()
	  (clsql:with-clsql-thread  
	    (sleep (random 1.0)) ;; scramble order in which threads modify object
	    (setf (nick *bunnyN*) (format nil "bunny~D [thread-number-~D]" n ii))
	    (clsql:update-records-from-instance *bunnyN*))))))
  (loop for i below 20
	for bunnyn = (first (get-bunny n :pool pool))
	do
     (sleep 0.05)
     (format t "iteration ~D: BunnyN name is ~A~%" i (nick bunnyN))))