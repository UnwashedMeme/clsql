;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; CLSQL broadcast streams which can be used to monitor the
;;;; flow of commands to, and results from, a database.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

;; lock the recorder's lock - a recorder may be shared by several databases
;; and threads - this works on either a datbase or a database-desc
(defmacro with-database-recording-locked (database/database-desc &body body)
  (let ((%db (gensym "datbase")))
    `(let ((,%db ,database/database-desc))
       (with-process-lock ((recorder-lock ,%db)
			   "with-database-recording-locked")
	 ,@body))))


(defun start-sql-recording (&key (type :commands) (database *default-database*))
  "Starts recording of SQL commands sent to and/or results
returned from DATABASE which defaults to *DEFAULT-DATABASE*. The
SQL is output on one or more broadcast streams, initially just
*STANDARD-OUTPUT*, and the functions ADD-SQL-STREAM and
DELETE-SQL-STREAM may be used to add or delete command or result
recording streams. The default value of TYPE is :commands which
means that SQL commands sent to DATABASE are recorded. If TYPE
is :results then SQL results returned from DATABASE are
recorded. Both commands and results may be recorded by passing
TYPE value of :both.

A DATABASE-DESC may be used for DATABASE.   If the DATBASE-RECORDER
is shared, recording is turned on for all DATABASE-DESCs that use it.
"
  (with-database-recording-locked database
    (when (or (eq type :both) (eq type :commands))
      (setf (command-recording-stream database)
	    (make-broadcast-stream *standard-output*)))
    (when (or (eq type :both) (eq type :results))
      (setf (result-recording-stream database)
	    (make-broadcast-stream *standard-output*)))
    (values)))

(defun stop-sql-recording (&key (type :commands) (database *default-database*))
  "Stops recording of SQL commands sent to and/or results
returned from DATABASE which defaults to *DEFAULT-DATABASE*. The
default value of TYPE is :commands which means that SQL commands
sent to DATABASE will no longer be recorded. If TYPE is :results
then SQL results returned from DATABASE will no longer be
recorded. Recording may be stopped for both commands and results
by passing TYPE value of :both.

A DATABASE-DESC may be used for DATABASE.   If the DATBASE-RECORDER
is shared, recording is turned on for all DATABASE-DESCs that use it.
"
  (with-database-recording-locked database 
    (when (or (eq type :both) (eq type :commands))
      (setf (command-recording-stream database) nil))
    (when (or (eq type :both) (eq type :results))
      (setf (result-recording-stream database) nil))
    (values)))

(defun sql-recording-p (&key (type :commands) (database *default-database*))
  "Predicate to test whether the SQL recording specified by TYPE
is currently enabled for DATABASE which defaults to *DEFAULT-DATABASE*.
TYPE may be one of :commands, :results, :both or :either, defaulting to
:commands, otherwise nil is returned."
  (with-database-recording-locked database 
    (when (or (and (eq type :commands)
		   (command-recording-stream database))
	      (and (eq type :results)
                 (result-recording-stream database))
	      (and (eq type :both)
		   (result-recording-stream database)
		   (command-recording-stream database))
	      (and (eq type :either)
		   (or (result-recording-stream database)
		       (command-recording-stream database))))
      t)))

(defun add-sql-stream (stream &key (type :commands)
                              (database *default-database*))
  "Adds the supplied stream STREAM (or T for *standard-output*)
as a component of the recording broadcast stream for the SQL
recording type specified by TYPE on DATABASE which defaults to
*DEFAULT-DATABASE*. TYPE must be one of :commands, :results,
or :both, defaulting to :commands, depending on whether the
stream is to be added for recording SQL commands, results or
both."
  (with-database-recording-locked database 
    (when (or (eq type :both) (eq type :commands))
      (unless (member stream
		      (list-sql-streams :type :commands :database database))
	(setf (command-recording-stream database)
	      (apply #'make-broadcast-stream
		     (cons stream (list-sql-streams :type :commands
						    :database database))))))
    (when (or (eq type :both) (eq type :results))
      (unless (member stream (list-sql-streams :type :results :database database))
	(setf (result-recording-stream database)
	      (apply #'make-broadcast-stream
		     (cons stream (list-sql-streams :type :results
						    :database database))))))
    stream))

(defun delete-sql-stream (stream &key (type :commands)
                                 (database *default-database*))
 "Removes the supplied stream STREAM from the recording broadcast
stream for the SQL recording type specified by TYPE on DATABASE
which defaults to *DEFAULT-DATABASE*. TYPE must be one
of :commands, :results, or :both, defaulting to :commands,
depending on whether the stream is to be added for recording SQL
commands, results or both."
  (with-database-recording-locked database 
    (when (or (eq type :both) (eq type :commands))
      (setf (command-recording-stream database)
	    (apply #'make-broadcast-stream
		   (remove stream (list-sql-streams :type :commands
						    :database database)))))
    (when (or (eq type :both) (eq type :results))
      (setf (result-recording-stream database)
	    (apply #'make-broadcast-stream
		   (remove stream (list-sql-streams :type :results
						    :database database)))))
    stream))
  
(defun list-sql-streams (&key (type :commands) (database *default-database*))
  "Returns the list of component streams for the broadcast stream
recording SQL commands sent to and/or results returned from
DATABASE which defaults to *DEFAULT-DATABASE*. TYPE must be one
of :commands, :results, or :both, defaulting to :commands, and
determines whether the listed streams contain those recording SQL
commands, results or both."
  (with-database-recording-locked database 
    (let ((crs (command-recording-stream database))
	  (rrs (result-recording-stream database)))
      (cond
	((eq type :commands)
	 (when crs (broadcast-stream-streams crs)))
	((eq type :results)
	 (when rrs (broadcast-stream-streams rrs)))
	((eq type :both)
	 (append (when crs (broadcast-stream-streams crs))
		 (when rrs (broadcast-stream-streams rrs))))
	(t
	 (error "Unknown recording type. ~A" type))))))
  
(defun sql-stream (&key (type :commands) (database *default-database*))
  "Returns the broadcast stream used for recording SQL commands
sent to or results returned from DATABASE which defaults to
*DEFAULT-DATABASE*. TYPE must be one of :commands or :results,
defaulting to :commands, and determines whether the stream
returned is that used for recording SQL commands or results."
  (with-database-recording-locked database 
    (cond
      ((eq type :commands)
       (command-recording-stream database))
      ((eq type :results)
       (result-recording-stream database))
      (t
       (error "Unknown recording type. ~A" type)))))

(defun record-sql-command (expr database)
  (when database
    (with-database-recording-locked database 
      (let ((command-recording-stream (command-recording-stream database)))
	(when command-recording-stream
	  (format command-recording-stream "~&;; ~A ~A => ~A~%"
		  (iso-timestring (get-time))
		  (database-name database)
		  expr))))))

(defun record-sql-result (res database)
  (when database
    (with-database-recording-locked database 
      (let ((result-recording-stream (result-recording-stream database)))
	(when result-recording-stream
	  (format result-recording-stream "~&;; ~A ~A <= ~A~%"
		  (iso-timestring (get-time))
		  (database-name database)
		  res))))))



