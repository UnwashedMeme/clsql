;;;; Proposed new file in clsql-tests to enable abstracting datasets for reuse.
;;;;
;;;; The core is def-datset and with-dataset that respectively define a set,
;;;; and enable one for a dynamic scope. Datasets will normally be setup and
;;;; torn down in the scope of one test, which may impose a computation
;;;; overhead, but enables simpler tests by not worrying about side-effects
;;;; between tests.
;;;;
;;;; In general datasets should be database agnostic, but because the code
;;;; is only run in the scope of a test, if a test is excluded for a backend
;;;; or some other reason then it is never run hence doesn't cause problems.

(in-package #:clsql-tests)

(defparameter *dataset-debug-on-error* nil
  "If we get an error while loading or cleaning up the dataset,
should we debug (T) or just print and quit.")

(defun generic-error (e)
  (when (and *dataset-debug-on-error*
	     *debugger-hook*)
    (invoke-debugger e))
  (fresh-line *error-output*)
  (princ e *error-output*)
  (throw 'quit-dataset e))

(defmacro def-dataset (name &body body)
  "Define a dataset"
  ;;probably just shove this into a param, perhaps a marginal
  ;; bit of processing first.
  `(defparameter ,name ',body))

(defmacro with-dataset (name &body body)
  "Use a dataset in a dynamic scope, e.g. a single test.
1. Before the body:
  * :setup is run
  * :data is loaded
2. Body
3. :cleanup always happens"
  `(catch 'quit-dataset
     (unwind-protect
	  (progn 
	    (restart-case (%dataset-init ,name)
	      (retry-dataset-init ()
		:report ,(format nil "Retry dataset('~a) init: (with any dataset changes)"
				(symbol-name name))
		(%dataset-init ,name))
	      (skip-this-test ()
		:report "FAIL and run the next test"
		(throw 'quit-dataset :data-set-failure)))
	    ,@body)
       (%dataset-cleanup ,name))))


(defun %dataset-dispatch (arg)
  "For use with def-dataset and with-dataset, tries to DWIM."
  (etypecase arg
    (string (clsql-sys:execute-command arg))  ;treat it as a sql command.
    ((or function symbol) (funcall arg))       ;run functions
    (list
       (case (first arg)
	 ((function lambda) (%dataset-dispatch (eval arg))) ;#' forms, lambdas
	 (progn (mapc #'%dataset-dispatch (rest arg)))    ; (progn "asdf" "ff")
	 (ignore-errors (ignore-errors (mapc #'%dataset-dispatch (rest arg))))
	 (t (mapc #'%dataset-dispatch arg)))    ;otherwise implicit progn
       )))

(defun %dataset-init (name)
  "Run initialization code and fill database for given dataset."
  	;;find items that looks like '(:setup ...),
	;; dispatch the rest.
	(let ((setup (rest (find :setup name :key #'first)))
	      (sqldata (rest (find :sqldata name :key #'first)))
	      (objdata (rest (find :objdata name :key #'first))))
	  (when setup
	    (%dataset-dispatch setup))
	  (when sqldata
	    ;;run raw sql insert statements
	    (destructuring-bind (table-name columns &rest values-list) sqldata
	      (dolist (v values-list)
		(clsql-sys:execute-command
		 (format nil
			 "INSERT INTO ~a (~a) VALUES (~a)"
			 table-name columns v)))))
	  (when objdata
	    ;;presumed to be view-class objects, force them to insert.
	    (dolist (o objdata)
	      (setf (slot-value o 'clsql-sys::view-database) nil)
	      (clsql-sys:update-records-from-instance o)))))

(defun %dataset-cleanup (name)
  "Run cleanup code associated with the given dataset."
  (restart-case 
      (handler-bind ((error #'generic-error))
	(let ((cleanup (rest (find :cleanup name :key #'first))))
	  (when cleanup
	    (%dataset-dispatch cleanup))))
    (retry-dataset-cleanup ()
      :report "Retry dataset cleanup (with any dataset changes)"
      (%dataset-cleanup name))
    (skip-cleanup () nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Test Code

;;incomplete example taken from test-init
;; (def-dataset *employees*
;;   (:setup "CREATE TABLE employee
;; (
;;   emplid integer NOT NULL,
;;   groupid integer NOT NULL,
;;   first_name character varying(30),
;;   last_name character varying(30),
;;   email character varying(100),
;;   ecompanyid integer,
;;   managerid integer,
;;   height double,
;;   married boolean,
;;   birthday timestamp without time zone,
;;   bd_utime bigint,
;;   CONSTRAINT employeepk PRIMARY KEY (emplid, groupid),
;;   CONSTRAINT employee_emplid_key UNIQUE (emplid)
;; )
;; ")
;;   ;;alternatively setup can still be done as
;;   ;;(:setup #'(lambda () (create-view-from-class ...)))
;;   (:sqldata "employees" "emplid,groupid,married,height,first_name,last_name"
;; 	    "1,1,false,1.5,'Napolean', 'Bonaparte'"
;; 	    (format nil "1,1,true,~a,'Vladimir','Lenin'" (1+ (random 1.00))))
;;   (:cleanup "DROP TABLE EMPLOYEES"))

