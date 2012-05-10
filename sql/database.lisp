;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; Base database functions
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)


;; lock anything with a cache-lock method
(defmacro with-database-cache-locked ((thing desc) &body body)
  `(with-process-lock ((cache-lock ,thing) ,desc) ,@body))





;; Perform an action with the database locked. These should be short
;; actions, not queries.  Allow for NIL database 
(defmacro with-database-locked ((database 
				 &optional (desc "with-database-locked")) 
				&body body)
  (let ((body-func (gensym "with-database-locked-body"))
	(db (gensym "db")))
    `(flet ((,body-func () ,@body))
       (let ((,db ,database))
	 (if (not ,db) ;; allow for NIL database 
	     (,body-func)
	     (with-process-lock ((database-lock ,db) ,desc)
	       (,body-func)))))))
  
;; Given a database, try to set DATABASE-THREAD to the current thread, to mark
;; this database as in use by this thread.  If it is in use, return NIL.
;; Force keywords allows database to be stolen from another thread, but it is 
;; very bad practice.  Returns the old owner thread as the 2nd value (can be NIL),
;; and whether this thread owned it before as the 3rd value
(defun acquire-database-for-thread (database &key (force nil) (allow-ownerless t) (allow-dead-owner t))
  (with-database-locked (database "acquire-database")
    (let ((old-thread (database-thread database)))
      (when force (setf (database-thread database) nil)) ;; BAD PRACTICE except for crash-closes
      (cond 
	;; do not acquire a database in a transaction
	((and (transaction database) (= (transaction-level database) 1)  
	      (not force)) ;; unless force in effect
	 (values nil old-thread nil))
	;; if owned by current thread, we're done
	((and 
	  (database-thread database)
	  (current-thread-p (database-thread database)))
	 (values database old-thread t))
	;; if this owned by a live thread that is not this thead, can't acquire
	((and 
	  (database-thread database)
	  (thread-alive-p (database-thread database)))
	 (values nil old-thread nil)) ;; can't acquire this datbase because it is in use
	;; the thread has no owner, and we allow grabbing an ownerless thread. This should
	;; be fine as long as there is no chance that the thread is an idle pool thread left in 
	;; userspace. 
	((and
	  (not (database-thread database))
	  allow-ownerless)
	 (setf (database-thread database) (current-thread))
	 (values database nil nil))
	;; otherwise, the thread is dead, so we can steal its database, if allowed.
	;; this is probably safe, because idle threads in pools have had their owner threads 
	;; stripped.
	((and 
	  (not (thread-alive-p (database-thread database)))
	  allow-dead-owner)
	 (setf (database-thread database) (current-thread))
	 (values database old-thread nil))))))

;; Release a database by setting THREAD to NIL
(defun release-database-for-thread (database)
  (with-database-locked (database "release-database")
    (setf (database-thread database) nil)))

;; does the current thread own the database.
(defun current-thread-owns-database-p (database)
  (with-database-locked (database "current-thread-owns-database-p")
    (current-thread-p (database-thread database))))

;; current thread or no thread owns database
(defun other-thread-doesnt-own-database-p (database)
  (with-database-locked (database "current-thread-owns-database-p")
    (or (not (database-thread database))
	(current-thread-p (database-thread database)))))

;; check that the current thread owns the database, or error
(defun current-thread-owns-database-or-error (database &optional (where "<unknown-location>"))
  (unless (current-thread-owns-database-p database)
    (signal-wrong-thread-database-error database where)))
	    



(defvar *connect-if-exists* :error
  "Default value for the if-exists keyword argument in calls to
CONNECT. Meaningful values are :new, :warn-new, :error, :warn-old
and :old.")

(defvar *connected-databases-lock* (make-process-lock "*connected-databases-lock*"))
(defvar *connected-databases* nil
  "List of active database objects.")

(defmacro with-connected-databases-lock (desc &body body)
    `(with-process-lock (*connected-databases-lock* ,desc) 
       ,@body))

(defun connected-databases ()
  "Returns the list of active database objects."
  *connected-databases*)

(defvar *default-database* nil
  "Specifies the default database to be used.")

(defun is-database-open (database)
  (eql (database-state database) :open))

(defun find-database (database &key (errorp t) (db-type nil) 
		      (allow-owned-by-other-thread nil)
		      (acquire-for-thread t))
  "Returns the connected databases of type DB-TYPE whose names
match the string DATABASE. If DATABASE is a database object, it
is returned. If DB-TYPE is nil all databases matching the string
DATABASE are considered.  If no matching databases are found and
ERRORP is nil then nil is returned. If ERRORP is nil and one or
more matching databases are found, then the most recently
connected database is returned as a first value and the number of
matching databases is returned as a second value. If no, or more
than one, matching databases are found and ERRORP is true, an
error is signalled.

If the ACQUIRE-FOR-THREAD keyword is set, as is the default, then
reserve the database for this thread.  If this is NIL, return any
database owned by any thread (or no thread) and don't reserve it.

If ALLOWED-OWNED-BY-OTHER-THREAD is T, then allow acquiring
a database owned by another thread.  This is generally BAD PRACTICE,
and is NIL by default."
  (etypecase database
    
    ;; if this database can't be acquired and acquire-for-thread=T, then don't return it
    (database
     (if (or (not acquire-for-thread) 
	     (acquire-database-for-thread 
	      database 
	      :force allow-owned-by-other-thread))
	 (values database 1)
	 (values nil 0)))
       
    (string
     (let* ((count 0) ;; how many match the spec
	    (matching-db nil)) ;; the matching db (possibly acquired)
       ;;
       (with-connected-databases-lock "find-database"
	 (dolist (db (connected-databases))
	   (when (and (string= (database-name db) database)
		      (if db-type
			  (equal (database-type db) db-type)
			t))
	     (incf count)
	     (when (and 
		    ;; return just one database
		    (not matching-db) 
		    ;; acquire if desired, and force acquisition if
		    ;; allowing other threads to own it beforehand
		    (or (not acquire-for-thread)
			(acquire-database-for-thread 
			 db
			 :force allow-owned-by-other-thread)))
	       (setf matching-db db)))))
	 ;;
	 (if (or (not errorp) (= count 1))
           (values matching-db count)
	   (progn
	     ;; because this is a CERROR, we do not un-acquire the database
	     ;; as in commented out next line.  Is this OK?
	     ;; (when matching-db (release-database-for-thread matching-db))
	     (cerror "Return nil."
		     'sql-database-error
		     :message
		     (format nil "There exists ~A database called ~A."
			     (if (zerop count) "no" "more than one")
			     database))))))
    ;;
    (null
     (error "FIND-DATABASE: A database must be specified rather than NIL."))))


(defun connect (connection-spec
                &key (if-exists *connect-if-exists*)
                (make-default t)
                (pool nil)
                (database-type *default-database-type*)
                (encoding nil))
  "Connects to a database of the supplied DATABASE-TYPE which
defaults to *DEFAULT-DATABASE-TYPE*, using the type-specific
connection specification CONNECTION-SPEC. The value of IF-EXISTS,
which defaults to *CONNECT-IF-EXISTS*, determines what happens if
a connection to the database specified by CONNECTION-SPEC is
already established.  A value of :new means create a new
connection.  A value of :warn-new means warn the user and create
a new connect.  A value of :warn-old means warn the user and use
the old connection.  A value of :error means fail, notifying the
user.  A value of :old means return the old connection.
MAKE-DEFAULT is t by default which means that *DEFAULT-DATABASE*
is set to the new connection, otherwise *DEFAULT-DATABASE* is not
changed. If POOL is t the connection will be taken from the
general pool, if POOL is a CONN-POOL object the connection will
be taken from this pool."

  (unless database-type
    (error 'sql-database-error :message "Must specify a database-type."))

  (when (stringp connection-spec)
    (setq connection-spec (string-to-list-connection-spec connection-spec)))

  (unless (member database-type *loaded-database-types*)
    (asdf:operate 'asdf:load-op (ensure-keyword
                                 (concatenate 'string
                                              (symbol-name '#:clsql-)
                                              (symbol-name database-type)))
                  :verbose nil))

  (if pool
      (let ((conn (acquire-from-pool connection-spec database-type pool encoding)))
        (when make-default (setq *default-database* conn))
        conn)
      (let* ((db-name (database-name-from-spec connection-spec database-type))
             (old-db (unless (eq if-exists :new)
		       ;; find a database, and claim it for this thread
                       (find-database db-name :db-type database-type
                                      :errorp nil :acquire-for-thread t)))
             (result-db nil))
        (if old-db
            (ecase if-exists
              (:warn-new
               (setq result-db
                     (database-connect connection-spec database-type))
               (warn 'sql-warning
                     :message
                     (format nil
                             "Created new connection ~A to database ~A~%, although there is an existing connection (~A)."
                             result-db (database-name result-db) old-db)))
              (:error
               (restart-case
                 (error 'sql-connection-error
                          :message
                          (format nil "There is an existing connection ~A to database ~A."
                          old-db
                          (database-name old-db)))
                 (create-new ()
                   :report "Create a new connection."
                   (setq result-db (database-connect connection-spec database-type))
		   (acquire-database-for-thread result-db))
                 (use-old ()
                   :report "Use the existing connection."
                   (setq result-db old-db))))
              (:warn-old
               (setq result-db old-db)
               (warn 'sql-warning
                     :message
                     (format nil
                             "Using existing connection ~A to database ~A."
                             old-db
                             (database-name old-db))))
              (:old
               (setq result-db old-db)))
            (setq result-db
                  (database-connect connection-spec database-type)))
        (when result-db
          (setf (slot-value result-db 'state) :open)
          (with-connected-databases-lock "connect" (pushnew result-db *connected-databases*))
          (when make-default (setq *default-database* result-db))
          (setf (encoding result-db) encoding)
	  ;; double check that this thread owns the database
	  (current-thread-owns-database-or-error result-db) 
          result-db))))


(defun disconnect (&key (database *default-database*) (error nil)
		   (allow-non-owner-thread-to-disconnect nil))

  "Closes the connection to DATABASE and resets
*DEFAULT-DATABASE* if that database was disconnected. If DATABASE
is a database instance, this object is closed. If DATABASE is a
string, then a connected database whose name matches DATABASE is
sought in the list of connected databases. If no matching
database is found and ERROR and DATABASE are both non-nil an
error is signaled, otherwise nil is returned. If the database is
from a pool it will be released to this pool.

A database may be closed only by the thread that owns it, unless the keyword
ALLOW-NON-OWNER-THREAD-TO-DISCONNECT is true.  This is probably BAD PRACTICE
if finding a database by name."
  (declare (optimize debug))

  #+ignore
  (progn ;; debugging code to see explain closes
    (format t "===========================================~%")
    (format t "Closing database ~A with backtrace ~%~A~%~%"
	    database
	    (subseq (sb-debug:backtrace-as-list) 0 7))
    (format t "===========================================~%")
    (force-output))

  (let ((database 
	 (find-database 
	  database 
	  :acquire-for-thread t ;; grab it if we're closing it
	  ;; if the non-owner is allowed to do this disconnect, then
	  ;; find-database is allowed to return a thread owned by someone else
	  :allow-owned-by-other-thread  allow-non-owner-thread-to-disconnect 
	  :errorp (and database error))))
    ;; the database is now owned by this thread, and should be free
    ;; from inteference by other threads until we release it
    (when database
      ;;
      ;; unless specifically allowed, only owner thread can disconnect database
      (when (and (not (current-thread-owns-database-p database))
		 (not allow-non-owner-thread-to-disconnect))
	(error 
	 'sql-database-error 
	 :database database
	 :message "Attempting to DISCONNECT a database from a non-owner thread 
without setting :ALLOW-OTHER-THREAD-TO-DISCONNECT to true."))
      ;;
      (if (conn-pool database)
          (with-process-lock ((conn-pool-lock (conn-pool database)) "Delete from pool")
            (when (release-to-pool database)
	      ;;
	      (with-connected-databases-lock "Remove database from *connected-databases*"
		(setf *connected-databases* (delete database *connected-databases*))
		(when (eq database *default-database*)
		  (setf *default-database* (car *connected-databases*))))
              t))
          (when (database-disconnect database)
	    ;;
            (with-connected-databases-lock "Remove database from *connected-databases*"
	      (setf *connected-databases* (delete database *connected-databases*))
	      (when (eq database *default-database*)
		(setf *default-database* (car *connected-databases*))))
	    ;;
            (setf (slot-value database 'state) :closed)
            t))
      ;; unmark the database as being owned this thread at very end. Now other threads
      ;; can do things with it.
       (release-database-for-thread database))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro check-connection-spec (connection-spec database-type template)
  "Check the connection specification against the provided template,
and signal an sql-user-error if they don't match. This function
is called by database backends."
  `(handler-case
    (destructuring-bind ,template ,connection-spec
      (declare (ignore ,@(remove '&optional template)))
      t)
    (error ()
     (error 'sql-user-error
      :message
      (format nil
              "The connection specification ~A~%is invalid for database type ~A.~%The connection specification must conform to ~A"
              ,connection-spec
              ,database-type
              (quote ,template))))))

(defun reconnect (&key (database *default-database*) (error nil) (force t))
  "Reconnects DATABASE which defaults to *DEFAULT-DATABASE* to
the underlying database management system. On success, t is
returned and the variable *DEFAULT-DATABASE* is set to the newly
reconnected database. If DATABASE is a database instance, this
object is closed. If DATABASE is a string, then a connected
database whose name matches DATABASE is sought in the list of
connected databases. If no matching database is found and ERROR
and DATABASE are both non-nil an error is signaled, otherwise nil
is returned. When the current database connection cannot be
closed, if FORCE is non-nil, as it is by default, the connection
is closed and errors are suppressed. If force is nil and the
database connection cannot be closed, an error is signalled."
  (let ((db (etypecase database
              (database database)
              ((or string list)
               (let ((db (find-database database :errorp nil)))
                 (when (null db)
                   (if (and database error)
                       (error 'sql-connection-error
                              :message
                              (format nil "Unable to find database with connection-spec ~A." database))
                       (return-from reconnect nil)))
                 db)))))

    (when (is-database-open db)
      (if force
          (ignore-errors (disconnect :database db))
          (disconnect :database db :error nil)))

    (connect (connection-spec db) :encoding (encoding db))))


(defun status (&optional full)
  "Prints information about the currently connected databases to
*STANDARD-OUTPUT*. The argument FULL is nil by default and a
value of t means that more detailed information about each
database is printed."
  (flet ((get-data ()
           (let ((data '()))
	     (with-connected-databases-lock "status"
	       (dolist (db (connected-databases) data)
		 (push
		  (append
		   (list (if (equal db *default-database*) "*" "")
			 (database-name db)
			 (string-downcase (string (database-type db)))
			 (cond ((and (command-recording-stream db)
				     (result-recording-stream db))
				"Both")
			       ((command-recording-stream db) "Commands")
			       ((result-recording-stream db) "Results")
			       (t "nil")))
		   (when full
		     (list
		      (if (conn-pool db) "t" "nil")
		      (format nil "~A" (length (database-list-tables db)))
		      (format nil "~A" (length (database-list-views db))))))
		  data)))))
         (compute-sizes (data)
           (mapcar #'(lambda (x) (apply #'max (mapcar #'length x)))
                   (apply #'mapcar (cons #'list data))))
         (print-separator (size)
           (format t "~&~A" (make-string size :initial-element #\-))))
    (format t "~&CLSQL STATUS: ~A~%" (iso-timestring (get-time)))
    (let ((data (get-data)))
      (when data
        (let* ((titles (if full
                           (list "" "DATABASE" "TYPE" "RECORDING" "POOLED"
                                 "TABLES" "VIEWS")
                           (list "" "DATABASE" "TYPE" "RECORDING")))
               (sizes (compute-sizes (cons titles data)))
               (total-size (+ (apply #'+ sizes) (* 2 (1- (length titles)))))
               (control-string (format nil "~~&~~{~{~~~AA  ~}~~}" sizes)))
          (print-separator total-size)
          (format t control-string titles)
          (print-separator total-size)
          (dolist (d data) (format t control-string d))
          (print-separator total-size))))
    (values)))

(defun create-database (connection-spec &key (database-type *default-database-type*))
  "This function creates a database in the database system specified
by DATABASE-TYPE."
  (when (stringp connection-spec)
    (setq connection-spec (string-to-list-connection-spec connection-spec)))
  (database-create connection-spec database-type))

(defun probe-database (connection-spec &key (database-type *default-database-type*))
  "This function tests for the existence of a database in the database
system specified by DATABASE-TYPE."
  (when (stringp connection-spec)
    (setq connection-spec (string-to-list-connection-spec connection-spec)))
  (database-probe connection-spec database-type))

(defun destroy-database (connection-spec &key (database-type *default-database-type*))
  "This function destroys a database in the database system specified
by DATABASE-TYPE."
  (when (stringp connection-spec)
    (setq connection-spec (string-to-list-connection-spec connection-spec)))
  (database-destroy connection-spec database-type))

(defun list-databases (connection-spec &key (database-type *default-database-type*))
  "This function returns a list of databases existing in the database
system specified by DATABASE-TYPE."
  (when (stringp connection-spec)
    (setq connection-spec (string-to-list-connection-spec connection-spec)))
  (database-list connection-spec database-type))

(defun encoding (db)
  (when (typep db 'database)
    (slot-value db 'encoding)))

(defun (setf encoding) (encoding db)
  (when (typep db 'database)
    (setf (slot-value db 'encoding) encoding)
    (when (eql (slot-value db 'state) :open)
      (case (database-type db)
        ;; FIXME: If database object is open then
        ;; send command to SQL engine specifying the character
        ;; encoding for the database
        (:mysql
         )
        ((:postgresql :postgresql-socket)
         )))))





(defmacro with-database ((db-var connection-spec
                                 &key make-default pool
                                 (if-exists *connect-if-exists*)
                                 (database-type *default-database-type*)
                                 (encoding nil))
                                 &body body)
  "Evaluate the body in an environment, where DB-VAR is bound to the
database connection given by CONNECTION-SPEC and CONNECT-ARGS.  The
connection is automatically closed or released to the pool on exit
from the body. MAKE-DEFAULT has a default value of NIL."
  `(let ((,db-var (connect ,connection-spec
                           :database-type ,database-type
                           :if-exists ,if-exists
                           :pool ,pool
                           :make-default ,make-default
                           :encoding ,encoding)))
     (unwind-protect
      (let ((,db-var ,db-var))
        (progn ,@body))
       (progn
	 (disconnect :database ,db-var)))))




(defmacro with-default-database ((database) &rest body)
  "Perform BODY with DATABASE bound as *default-database*."
  `(progv '(*default-database*)
       (list ,database)
     ,@body))




;; given a database desc (like one in a view-db slot), get a database for it
;; that is valid for use in this thread.  Return it to pool as specified in
;; the database-desc.   
(defun get-database-for-database-desc (db-desc)
  (let ((db (connect 
	     (connection-spec db-desc)
	     :pool (conn-pool db-desc) 
	     :make-default nil
	     ;; make it use a NEW connection in non-pooled mode,
	     ;; because we definitely will be closing it afterwards.
	     ;; We could avoid this with more hackery to verify that
	     ;; we just got a database that was already in use by this
	     ;; thread, so in this special case we don't close it
	     ;; after borrowing it.  But is it worth the extra
	     ;; complexity?  If one cares about too many connections,
	     ;; then one should use pools.
	     :if-exists :new
	     :database-type (database-type db-desc))))
    ;; final check that this thread owns the database
    (current-thread-owns-database-or-error db) 
    ;; and if the conn-pools match - should always be true
    (when (not (eq (conn-pool db) (conn-pool db-desc)))
      (error 
       'sql-database-error 
       :database db
       :message  "The conn-pool in database does not match the one in database-desc.  This should not happen."))
    db))



;; this is used a oodml.lisp, for turning a DATABASE-DESC in the
;; VIEW-DATABASE slot into a database properly acquired for this
;; thread.  In non-pooled mode, it will open and close a new
;; connection (database).  It would be possible to make it reuse the
;; old connection using a special variable to store the current database,
;; and reusing it if possible.  
(defmacro with-database-for-database-desc ((db-var db-desc) &body body)
  "Evaluate the BODY binding DB-VAR to a database valid for database-desc DB-DESC,
temporarily swapping out the database-desc of the database for DB-DESC,
because the caches might be different even though they refer to the same database."
  (let ((db-desc-this     (gensym "db-desc-this"))
	(db-desc-original (gensym "db-desc-orig")))
    `(let* ((,db-desc-this ,db-desc)
	    (,db-desc-original nil) ;; holder for original db-desc in database
	    (,db-var (get-database-for-database-desc ,db-desc-this)))
       (unwind-protect
	    (progn
	      ;; save the old db-desc in the database we got, because it has 
	      ;; its own caches
	      (setf ,db-desc-original (database-desc ,db-var))
	      ;; and replace it
	      (setf (database-desc ,db-var) ,db-desc-this)
	      ;; run the body
	      (progn ,@body))
	 ;; reset the old database-desc in the unwind-protect
	 (progn
	   (setf (database-desc ,db-var) ,db-desc-original)
	   ;; return to pool or close, giving up thread's ownership
	   (disconnect :database ,db-var))))))


;; helper function for macro below - double check arguments
(defun %check-args-for-with-database-for-db-or-db-desc (db-desc db where)
  (when (not
	 (and 
	  (or (not db-desc) (typep db-desc 'database-desc))
	  (or (not db) (typep db 'database))))
    (error
     (format nil "In macro with-database-for-db-or-db-desc called from ~A,
db should have type (OR NULL DATABASE) and is ~A and
db-desc should have type (OR NULL DATBASE-DESC) and is ~A"
	     where (type-of db) (type-of db-desc)))))

(defmacro with-database-for-db-or-db-desc 
    ((db-var db-desc db &optional (where "unknown-location")) &body body)
"Macro used in oodml.lisp to get a suitable database object, using the
DATABASE-DESC in the VIEW-DATABASE slot of database objects.  Evaluate
body, trying to use a DATABASE-DESC (db-desc) or DATABASE (db)
according to these rules:

1. if (not db-desc) then there is no view-database slot, so use database

2. if (and db-desc database) then check if database is compatible, and
if so use it, so we don't have to open another database connection.
This is the same behaviour as favoring VIEW-DATABASE over supplied
database.

3. if (not database) then get a database for the db-desc
"

(let ((%db-desc     (gensym "db-desc"))
      (%db          (gensym "db"))
      (body-func    (gensym "with-database-for-db-or-db-desc-body")))
  `(let ((,%db-desc ,db-desc)
	 (,%db ,db))
     (%check-args-for-with-database-for-db-or-db-desc ,%db-desc ,%db ,where)
     (flet ((,body-func (,db-var) 
	      (current-thread-owns-database-or-error ,db-var ,where)
	      ,@body))
       (cond 
	 ;; case 1 - use the database, because no db-desc
	 ((and (not ,%db-desc)  ,%db)
	  ;; no desc, so we must use the provided database - if it is not
	  ;; appropriate for this thread, an error will result 
	  (,body-func ,%db))
	 ;; case 2 - db-desc matches db, so we borrow the database
	 ;; if it is owned by this thread
	 ((and ,%db ,%db-desc
	       (current-thread-owns-database-p ,%db)
	       ;; what about transaction state?
	       (equalp (connection-spec ,%db) (connection-spec ,%db-desc))
	       (equalp (database-type ,%db) (database-type ,%db-desc)))
	  (,body-func ,%db))
	 ;;
	 ;; case 3 - use db-desc to find a valid database, because no
	 ;; database was given or this databae is used by some other thread
	 (,%db-desc
	  (with-database-for-database-desc (,db-var ,%db-desc)
	    (,body-func ,db-var)))
	 ;; otherwise, nothing was provided - this should never happen
	 (t
	  (error  
	   (format nil "with-database-for-db-or-db-desc given null database-desc and database at <~A>" ,where))))))))

