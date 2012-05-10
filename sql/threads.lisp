;; this file is placed into the public domain and may be incorporated into
;; any other software with no stipulations.  This notice may be removed.
;;
;;
;;  Some databases require per-thread initialization and cleanup.  The methods
;;    (database-init-thread database-type)
;;       and 
;;    (database-cleanup-thread database-type) 
;;  will do this.
;; 
;;  The function (clsql:init-thread) and (clsql:cleanup-thread) will
;;  run the method for all loaded databases.
;;
;;  The macro 
;;    (with-clsql-database (:per-thread-pool T-or-NIL) ...)
;;  will initialize and cleanup all loaded databases using their methods.
;;


(in-package clsql-sys)





(defgeneric database-init-thread (database-type)
  (:documentation 
   "Perform required start-of-thread initialization for database
of type DATABASE-TYPE"))

(defgeneric database-cleanup-thread (database-type)
  (:documentation 
   "Perform required end-of-thread cleanup for database
of type DATABASE-TYPE"))


;; by default, a database type has no per-thread inits or cleanups, so undefined
;; methods for databases will lead to nothing being done
(defmethod database-init-thread (database-type) t)
(defmethod database-cleanup-thread (database-type) t)



(defun init-thread ()
  "Perform start-of-thread initializatons for all loaded databases."
  (dolist (database-type clsql-sys:*loaded-database-types*)
    (database-init-thread database-type)))

(defun cleanup-thread ()
  "Perform end-of-thread initializatons for all loaded databases."
  (dolist (database-type clsql-sys:*loaded-database-types*)
    (database-cleanup-thread database-type)))


(defmacro with-clsql-thread (&body body)
  "A macro to wrap actions inside threads that use databases, to ensure that
all necessarly per-thread initializations and cleanups are performed around
any database access within a thread.
Example: 
  
  (some-lisp:launch-thread
     (lambda ()
       (clsql:with-clsql-thread
          (things-to-do-in-thread))))"

  `(unwind-protect
	(progn 
	  (init-thread)
	  ,@body)
     (progn 
       (cleanup-thread))))