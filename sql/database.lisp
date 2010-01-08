;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id$
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


(defvar *connect-if-exists* :error
  "Default value for the if-exists keyword argument in calls to
CONNECT. Meaningful values are :new, :warn-new, :error, :warn-old
and :old.")

(defvar *connected-databases* nil
  "List of active database objects.")

(defun connected-databases ()
  "Returns the list of active database objects."
  *connected-databases*)

(defvar *default-database* nil
  "Specifies the default database to be used.")

(defun is-database-open (database)
  (eql (database-state database) :open))

(defun find-database (database &key (errorp t) (db-type nil))
  "Returns the connected databases of type DB-TYPE whose names
match the string DATABASE. If DATABASE is a database object, it
is returned. If DB-TYPE is nil all databases matching the string
DATABASE are considered.  If no matching databases are found and
ERRORP is nil then nil is returned. If ERRORP is nil and one or
more matching databases are found, then the most recently
connected database is returned as a first value and the number of
matching databases is returned as a second value. If no, or more
than one, matching databases are found and ERRORP is true, an
error is signalled."
  (etypecase database
    (database
     (values database 1))
    (string
     (let* ((matches (remove-if
                      #'(lambda (db)
                          (not (and (string= (database-name db) database)
                                    (if db-type
                                        (equal (database-type db) db-type)
                                        t))))
                      (connected-databases)))
            (count (length matches)))
       (if (or (not errorp) (= count 1))
           (values (car matches) count)
           (cerror "Return nil."
                   'sql-database-error
                   :message
                   (format nil "There exists ~A database called ~A."
                           (if (zerop count) "no" "more than one")
                           database)))))
    (null
     (error "A database must be specified rather than NIL."))))


(defun connect (connection-spec
                &key (if-exists *connect-if-exists*)
                (make-default t)
                (pool nil)
                (database-type *default-database-type*))
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
      (let ((conn (acquire-from-pool connection-spec database-type pool)))
        (when make-default (setq *default-database* conn))
        conn)
      (let* ((db-name (database-name-from-spec connection-spec database-type))
             (old-db (unless (eq if-exists :new)
                       (find-database db-name :db-type database-type
                                      :errorp nil)))
             (result nil))
        (if old-db
            (ecase if-exists
              (:warn-new
               (setq result
                     (database-connect connection-spec database-type))
               (warn 'sql-warning
                     :message
                     (format nil
                             "Created new connection ~A to database ~A~%, although there is an existing connection (~A)."
                             result (database-name result) old-db)))
              (:error
               (restart-case
                   (error 'sql-connection-error
                          :message
                          (format nil "There is an existing connection ~A to database ~A."
                          old-db
                          (database-name old-db)))
                 (create-new ()
                   :report "Create a new connection."
                   (setq result
                         (database-connect connection-spec database-type)))
                 (use-old ()
                   :report "Use the existing connection."
                   (setq result old-db))))
              (:warn-old
               (setq result old-db)
               (warn 'sql-warning
                     :message
                     (format nil
                             "Using existing connection ~A to database ~A."
                             old-db
                             (database-name old-db))))
              (:old
               (setq result old-db)))
            (setq result
                  (database-connect connection-spec database-type)))
        (when result
          (setf (slot-value result 'state) :open)
          (pushnew result *connected-databases*)
          (when make-default (setq *default-database* result))
          result))))


(defun disconnect (&key (database *default-database*) (error nil))

  "Closes the connection to DATABASE and resets
*DEFAULT-DATABASE* if that database was disconnected. If DATABASE
is a database instance, this object is closed. If DATABASE is a
string, then a connected database whose name matches DATABASE is
sought in the list of connected databases. If no matching
database is found and ERROR and DATABASE are both non-nil an
error is signaled, otherwise nil is returned. If the database is
from a pool it will be released to this pool."
  (let ((database (find-database database :errorp (and database error))))
    (when database
      (if (conn-pool database)
          (with-process-lock ((conn-pool-lock (conn-pool database)) "Delete from pool")
            (when (release-to-pool database)
              (setf *connected-databases* (delete database *connected-databases*))
              (when (eq database *default-database*)
                (setf *default-database* (car *connected-databases*)))
              t))
          (when (database-disconnect database)
            (setf *connected-databases* (delete database *connected-databases*))
            (when (eq database *default-database*)
              (setf *default-database* (car *connected-databases*)))
            (setf (slot-value database 'state) :closed)
            t)))))


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

    (connect (connection-spec db))))


(defun status (&optional full)
  "Prints information about the currently connected databases to
*STANDARD-OUTPUT*. The argument FULL is nil by default and a
value of t means that more detailed information about each
database is printed."
  (flet ((get-data ()
           (let ((data '()))
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
                data))))
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

(defmacro with-database ((db-var connection-spec
                                 &key make-default pool
                                 (if-exists *connect-if-exists*)
                                 (database-type *default-database-type*))
                                 &body body)
  "Evaluate the body in an environment, where DB-VAR is bound to the
database connection given by CONNECTION-SPEC and CONNECT-ARGS.  The
connection is automatically closed or released to the pool on exit
from the body. MAKE-DEFAULT has a default value of NIL."
  `(let ((,db-var (connect ,connection-spec
                           :database-type ,database-type
                           :if-exists ,if-exists
                           :pool ,pool
                           :make-default ,make-default)))
     (unwind-protect
      (let ((,db-var ,db-var))
        (progn ,@body))
       (disconnect :database ,db-var))))

(defmacro with-default-database ((database) &rest body)
  "Perform BODY with DATABASE bound as *default-database*."
  `(progv '(*default-database*)
       (list ,database)
     ,@body))

