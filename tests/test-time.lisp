;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (c) 2000, 2001 onShore Development, Inc.
;;;
;;; Test time functions (time.lisp)

(in-package #:clsql-tests)
(clsql-sys:file-enable-sql-reader-syntax)

(def-view-class datetest ()
  ((id :column "id"
                :type integer
                :db-kind :key
                :db-constraints (:not-null :unique)
	        :accessor id :initarg :id
                :initform nil
	        :db-type "int4")
   (testtimetz :column "testtimetz"
                :type clsql-sys:wall-time
                :db-kind :base
                :db-constraints nil
	        :accessor testtimetz :initarg :testtimetz
                :initform nil
	        :db-type "timestamp with time zone")
   (testtime :column "testtime"
             :type clsql-sys:wall-time
             :db-kind :base
	     :db-constraints nil
	     :accessor testtime :initarg :testtime
             :initform nil
	     :db-type "timestamp without time zone")))

(def-dataset *ds-datetest*
  (:setup (lambda () (clsql-sys:create-view-from-class 'datetest)))
  (:cleanup "DROP TABLE datetest"))


(def-dataset *cross-platform-datetest*
  (:setup (lambda () (create-table [datetest]
				   '(([testtime] wall-time)))))
  (:cleanup (lambda ()
	      (drop-table [datetest]))))


(setq *rt-time*
      '(

;; we use parse timestring a lot through here verifying other things
;; start off just checking that.
(deftest :time/iso-parse/0
    (let* ((time1 (parse-timestring "2010-01-23")))
      (decode-time time1))
  0 0 0 0 23 1 2010 6)

(deftest :time/iso-parse/1
    (let* ((time1 (parse-timestring "2010-01-23T14:56:32Z")))
      (decode-time time1))
  0 32 56 14 23 1 2010 6)

(deftest :time/iso-parse/2
    (let* ((time1 (parse-timestring "2008-02-29 12:46:32")))
      (decode-time time1))
  0 32 46 12 29 2 2008 5)

(deftest :time/iso-parse/3
    (let* ((time1 (parse-timestring "2010-01-23 14:56:32.44")))
      (decode-time time1))
  440000 32 56 14 23 1 2010 6)

(deftest :time/iso-parse/4
    (let* ((time1 (parse-timestring "2010-01-23 14:56:32.0044")))
      (decode-time time1))
  4400 32 56 14 23 1 2010 6)

(deftest :time/iso-parse/5
    (let* ((time1 (parse-timestring "2010-01-23 14:56:32.000003")))
      (decode-time time1))
  3 32 56 14 23 1 2010 6)

(deftest :time/print-parse/1
    ;;make sure when we print and parse we get the same time.
    (let* ((time (clsql-sys:make-time :year 2010 :month 1 :day 4
				      :hour 14 :minute 15 :second 44))
	   (string-time (iso-timestring time))
	   (time2 (parse-timestring string-time)))
      (decode-time time2))
  0 44 15 14 4 1 2010 1)

(deftest :time/print-parse/2
    ;;make sure when we print and parse we get the same time.
    (let* ((time (clsql-sys:make-time :year 2010 :month 1 :day 4
				      :hour 14 :minute 15 :second 44 :usec 3))
	   (string-time (iso-timestring time))
	   (time2 (parse-timestring string-time)))
      (decode-time time2))
  3 44 15 14 4 1 2010 1)


;; relations of intervals
(deftest :time/1
    (let* ((time-1 (clsql:parse-timestring "2002-01-01 10:00:00"))
           (time-2 (clsql:parse-timestring "2002-01-01 11:00:00"))
           (time-3 (clsql:parse-timestring "2002-01-01 12:00:00"))
           (time-4 (clsql:parse-timestring "2002-01-01 13:00:00"))
           (interval-1 (clsql:make-interval :start time-1 :end time-2))
           (interval-2 (clsql:make-interval :start time-2 :end time-3))
           (interval-3 (clsql:make-interval :start time-3 :end time-4))
           (interval-4 (clsql:make-interval :start time-1 :end time-3))
           (interval-5 (clsql:make-interval :start time-2 :end time-4))
           (interval-6 (clsql:make-interval :start time-1 :end time-4)))
      (flet ((my-assert (number relation i1 i2)
               (declare (ignore number))
               (let ((found-relation (clsql:interval-relation i1 i2)))
                 (equal relation found-relation))))
        (and
         (my-assert 1 :contains interval-1 interval-1)
         (my-assert 2 :precedes interval-1 interval-2)
         (my-assert 3 :precedes interval-1 interval-3)
         (my-assert 4 :contained interval-1 interval-4)
         (my-assert 5 :precedes interval-1 interval-5)
         (my-assert 6 :contained interval-1 interval-6)
         (my-assert 7 :follows interval-2 interval-1)
         (my-assert 8 :contains interval-2 interval-2)
         (my-assert 9 :precedes interval-2 interval-3)
         (my-assert 10 :contained interval-2 interval-4)
         (my-assert 11 :contained interval-2 interval-5)
         (my-assert 12 :contained interval-2 interval-6)
         (my-assert 13 :follows interval-3 interval-1)
         (my-assert 14 :follows interval-3 interval-2)
         (my-assert 15 :contains interval-3 interval-3)
         (my-assert 16 :follows interval-3 interval-4)
         (my-assert 17 :contained interval-3 interval-5)
         (my-assert 18 :contained interval-3 interval-6)
         (my-assert 19 :contains interval-4 interval-1)
         (my-assert 20 :contains interval-4 interval-2)
         (my-assert 21 :precedes interval-4 interval-3)
         (my-assert 22 :contains interval-4 interval-4)
         (my-assert 23 :overlaps interval-4 interval-5)
         (my-assert 24 :contained interval-4 interval-6)
         (my-assert 25 :follows interval-5 interval-1)
         (my-assert 26 :contains interval-5 interval-2)
         (my-assert 27 :contains interval-5 interval-3)
         (my-assert 28 :overlaps interval-5 interval-4)
         (my-assert 29 :contains interval-5 interval-5)
         (my-assert 30 :contained interval-5 interval-6)
         (my-assert 31 :contains interval-6 interval-1)
         (my-assert 32 :contains interval-6 interval-2)
         (my-assert 33 :contains interval-6 interval-3)
         (my-assert 34 :contains interval-6 interval-4)
         (my-assert 35 :contains interval-6 interval-5)
         (my-assert 36 :contains interval-6 interval-6))))
  t)

;; adjacent intervals in list
(deftest :time/2
  (let* ((interval-list nil)
         (time-1 (clsql:parse-timestring "2002-01-01 10:00:00"))
         (time-3 (clsql:parse-timestring "2002-01-01 12:00:00"))
         (time-4 (clsql:parse-timestring "2002-01-01 13:00:00")))
    (setf interval-list
          (clsql:interval-push interval-list (clsql:make-interval :start time-1 :end time-3
                                                      :type :open)))
    (setf interval-list
          (clsql:interval-push interval-list (clsql:make-interval :start time-3 :end time-4
                                                      :type :open)))
    (clsql:interval-relation (car interval-list) (cadr interval-list)))
  :precedes)

;; nested intervals in list
(deftest :time/3
    (let* ((interval-list nil)
           (time-1 (clsql:parse-timestring "2002-01-01 10:00:00"))
           (time-2 (clsql:parse-timestring "2002-01-01 11:00:00"))
           (time-3 (clsql:parse-timestring "2002-01-01 12:00:00"))
           (time-4 (clsql:parse-timestring "2002-01-01 13:00:00")))
      (setf interval-list
            (clsql:interval-push interval-list (clsql:make-interval :start time-1
                                                        :end time-4
                                                        :type :open)))
      (setf interval-list
            (clsql:interval-push interval-list (clsql:make-interval :start time-2
                                                        :end time-3
                                                        :type :closed)))
      (let* ((interval (car interval-list))
             (interval-contained
              (when interval (car (clsql:interval-contained interval)))))
        (when (and interval interval-contained)
          (and (clsql:time= (clsql:interval-start interval) time-1)
               (clsql:time= (clsql:interval-end interval) time-4)
               (eq (clsql:interval-type interval) :open)
               (clsql:time= (clsql:interval-start interval-contained) time-2)
               (clsql:time= (clsql:interval-end interval-contained) time-3)
               (eq (clsql:interval-type interval-contained) :closed)))))
  t)

;; interval-edit - nonoverlapping
(deftest :time/4
    (let* ((interval-list nil)
           (time-1 (clsql:parse-timestring "2002-01-01 10:00:00"))
           (time-2 (clsql:parse-timestring "2002-01-01 11:00:00"))
           (time-3 (clsql:parse-timestring "2002-01-01 12:00:00"))
           (time-4 (clsql:parse-timestring "2002-01-01 13:00:00")))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-1 :end time-2 :type :open)))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-3 :end time-4 :type :closed)))
      (setf interval-list (clsql:interval-edit interval-list time-1 time-1 time-3))
      ;; should be time-3 not time-2
      (clsql:time= (clsql:interval-end (car interval-list)) time-3))
  t)

;; interval-edit - overlapping
(deftest :time/5
    (let* ((interval-list nil)
           (time-1 (clsql:parse-timestring "2002-01-01 10:00:00"))
           (time-2 (clsql:parse-timestring "2002-01-01 11:00:00"))
           (time-3 (clsql:parse-timestring "2002-01-01 12:00:00"))
           (time-4 (clsql:parse-timestring "2002-01-01 13:00:00")))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-1 :end time-2 :type :open)))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-2 :end time-4 :type :closed)))
      (let ((pass t))
        (handler-case
            (progn
              (setf interval-list
                    (clsql:interval-edit interval-list time-1 time-1 time-3))
              (setf pass nil))
          (error nil))
        pass))
  t)

;; interval-edit - nested intervals in list
(deftest :time/6
    (let* ((interval-list nil)
           (time-1 (clsql:parse-timestring "2002-01-01 10:00:00"))
           (time-2 (clsql:parse-timestring "2002-01-01 11:00:00"))
           (time-3 (clsql:parse-timestring "2002-01-01 12:00:00"))
           (time-4 (clsql:parse-timestring "2002-01-01 13:00:00"))
           (time-5 (clsql:parse-timestring "2002-01-01 14:00:00"))
           (time-6 (clsql:parse-timestring "2002-01-01 15:00:00")))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-1 :end time-6 :type :open)))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-2 :end time-3 :type :closed)))
      (setf interval-list (clsql:interval-push interval-list (clsql:make-interval :start time-4 :end time-5 :type :closed)))
      (setf interval-list (clsql:interval-edit interval-list time-1 time-1 time-4))
      ;; should be time-4 not time-6
      (clsql:time= (clsql:interval-end (car interval-list)) time-4))
  t)

;; Test the boundaries of Local Time with granularity of 1 year
(deftest :time/7
    (let ((sec-in-year (* 60 60 24 365))
          (year (clsql:time-element (clsql:make-time) :year)))
      (dotimes (n 50 n)
        (let ((date (clsql:make-time :second (* n sec-in-year))))
          (unless (= (+ year n)
                     (clsql:time-element date :year))
            (return n)))))
  50)

;; Test db-timestring
(deftest :time/9
    (flet ((grab-year (dbstring)
             (parse-integer (subseq dbstring 1 5))))
      (let ((second-in-year (* 60 60 24 365)))
        (dotimes (n 2000 n)
          (let* ((second (* -1 n second-in-year))
                 (date (clsql:make-time :year 2525 :second second)))
            (unless
                (= (grab-year (clsql:db-timestring date))
                   (clsql:time-element date :year))
              (return n))))))
  2000)

;; Conversion between MJD and Gregorian
(deftest :time/10
    (dotimes (base 10000 base)
      (unless (= (apply #'clsql:gregorian-to-mjd (clsql:mjd-to-gregorian base))
                 base)
        (return base)))
  10000)

;; Clsql:Roll by minutes: +90
(deftest :time/11
    (let ((now (clsql:get-time)))
      (clsql:time= (clsql:time+ now (clsql:make-duration :minute 90))
             (clsql:roll now :minute 90)))
  t)

;;Clsql:Roll by minutes: +900
(deftest :time/12
    (let ((now (clsql:get-time)))
      (clsql:time= (clsql:time+ now (clsql:make-duration :minute 900))
             (clsql:roll now :minute 900)))
  t)


;; Clsql:Roll by minutes: +900
(deftest :time/13
    (let* ((now (clsql:get-time))
           (add-time (clsql:time+ now (clsql:make-duration :minute 9000)))
           (roll-time (clsql:roll now :minute 9000)))
      (clsql:time= add-time roll-time))
  t)



;;; The cross platform dataset uses the 'timestamp' column type which is
;;; in sql-92, for all that means.

(deftest :time/cross-platform/no-usec/no-tz
    (with-dataset *cross-platform-datetest*
      (let ((time (parse-timestring "2008-09-09T14:37:29")))
	(clsql-sys:insert-records :into [datetest]
				  :attributes '([testtime])
				  :values (list time))
	(let ((testtime
	       (first (clsql:select [testtime]
				    :from [datetest] :flatp t
				    :where [= [testtime] time] ))))
	  (format-time nil (parse-timestring testtime) :format :iso)
	  )))
  #.(format-time nil (parse-timestring "2008-09-09T14:37:29") :format :iso))

(deftest :time/cross-platform/no-usec/tz
    (with-dataset *cross-platform-datetest*
      (let ((time (parse-timestring "2008-09-09T14:37:29-04:00")))
	(clsql-sys:insert-records :into [datetest]
				  :attributes '([testtime])
				  :values (list time))
	(let ((testtime
	       (first (clsql:select [testtime]
				    :from [datetest] :flatp t
				    :where [= [testtime] time] ))))
	  (format-time nil (parse-timestring testtime) :format :iso)
	  )))
  #.(format-time nil (parse-timestring "2008-09-09T14:37:29-04:00") :format :iso))

;;;This test gets at the databases that only support miliseconds,
;;; not microseconds.
(deftest :time/cross-platform/msec
    (with-dataset *cross-platform-datetest*
      (let ((time (parse-timestring "2008-09-09T14:37:29.423")))
	(clsql-sys:insert-records :into [datetest]
				  :attributes '([testtime])
				  :values (list time))
	(let ((testtime
	       (first (clsql:select [testtime]
				    :from [datetest] :flatp t
				    :where [= [testtime] time] ))))
	  (format-time nil (parse-timestring testtime) :format :iso)
	  )))
  #.(format-time nil (parse-timestring "2008-09-09T14:37:29.423") :format :iso))

(deftest :time/cross-platform/usec/no-tz
    (with-dataset *cross-platform-datetest*
      (let ((time (parse-timestring "2008-09-09T14:37:29.000213")))
	(clsql-sys:insert-records :into [datetest]
				  :attributes '([testtime])
				  :values (list time))
	(let ((testtime
	       (first (clsql:select [testtime]
				    :from [datetest] :flatp t
				    :where [= [testtime] time] ))))
	  (format-time nil (parse-timestring testtime) :format :iso)
	  )))
  #.(format-time nil (parse-timestring "2008-09-09T14:37:29.000213") :format :iso))

(deftest :time/cross-platform/usec/tz
    (with-dataset *cross-platform-datetest*
      (let ((time (parse-timestring "2008-09-09T14:37:29.000213-04:00")))
	(clsql-sys:insert-records :into [datetest]
				  :attributes '([testtime])
				  :values (list time))
	(let ((testtime
	       (first (clsql:select [testtime]
				    :from [datetest]
				    :limit 1 :flatp t
				    :where [= [testtime] time] ))))
	  (format-time nil (parse-timestring testtime) :format :iso)
	  )))
  #.(format-time nil (parse-timestring "2008-09-09T14:37:29.000213-04:00") :format :iso))




;;; All odbc databases use local times exclusively (they do not send timezone info)
;;; Postgresql can use timezones, except when being used over odbc.  This test when
;;; run through both postgres socket and postgres odbc should test a fairly
;;; broad swath of available problem space
;;;
;;; Things the following tests try to prove correct
;;;  * Reading and writing usec and usec-less times
;;;  * reading and writing timezones (Z=utc) when appropriate (eg: postgresql-socket)
;;;  * reading and writing localtimes when appropriate (eg: ODBC)
;;;  * reading and writing through both the oodml and fdml layers



(deftest :time/pg/fdml/usec
  (with-dataset *ds-datetest*
    (let ((time (parse-timestring "2008-09-09T14:37:29.000213-04:00")))
      (clsql-sys:insert-records :into [datetest]
				:attributes '([testtimetz] [testtime] [id])
				:values (list time time 1))
      (destructuring-bind (testtimetz testtime)
	  (first (clsql:select [testtimetz] [testtime]
			       :from [datetest]
			       :limit 1 :flatp t
			       :where [= [testtime] time] ))
	(values (iso-timestring (parse-timestring testtime))
		(iso-timestring (parse-timestring testtimetz))))))
  #.(iso-timestring (parse-timestring "2008-09-09T14:37:29.000213-04:00"))
  #.(iso-timestring (parse-timestring "2008-09-09T14:37:29.000213-04:00")))

(deftest :time/pg/oodml/no-usec
  (with-dataset *ds-datetest*
    (let ((time (parse-timestring "2008-09-09T14:37:29-04:00")))
      (clsql-sys:update-records-from-instance
       (make-instance 'datetest :testtimetz time :testtime time :id 1))
      (let ((o (first (clsql:select
			  'datetest
			:limit 1 :flatp t
			:where [= [testtime] time] ))))
	(assert o (o) "o shouldnt be null here (we should have just inserted)")
	(update-records-from-instance o)
	(update-instance-from-records o)
	(values (iso-timestring (testtime o))
		(iso-timestring (testtimetz o))))))
  #.(iso-timestring (parse-timestring "2008-09-09T14:37:29-04:00"))
  #.(iso-timestring (parse-timestring "2008-09-09T14:37:29-04:00")))

(deftest :time/pg/oodml/usec
    (with-dataset *ds-datetest*
      (let ((time (parse-timestring "2008-09-09T14:37:29.000278-04:00")))
	(clsql-sys:update-records-from-instance
	 (make-instance 'datetest :testtimetz time :testtime time :id 1))
	(let ((o (first (clsql:select
			 'datetest
			 :limit 1 :flatp t
			 :where [= [testtime] time] ))))
	  (assert o (o) "o shouldnt be null here (we should have just inserted)")
	  (update-records-from-instance o)
	  (update-instance-from-records o)
	  (values (iso-timestring (testtime o))
		  (iso-timestring (testtimetz o)))
	  )))
    #.(iso-timestring (parse-timestring "2008-09-09T14:37:29.000278-04:00"))
    #.(iso-timestring (parse-timestring "2008-09-09T14:37:29.000278-04:00")))

(deftest :time/historic-datetimes
  (with-dataset *cross-platform-datetest*
    (let ((time (parse-timestring "1800-09-09T14:37:29")))
      (clsql-sys:insert-records :into [datetest]
                                :attributes '([testtime])
                                :values (list time))
      (let ((testtime
              (first (clsql:select [testtime]
                       :from [datetest] :flatp t
                       :where [= [testtime] time] ))))
        (format-time nil (parse-timestring testtime) :format :iso)
        )))
  #.(format-time nil (parse-timestring "1800-09-09T14:37:29") :format :iso))

))





