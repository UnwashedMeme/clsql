;;; -*- Mode: Lisp -*-
;;; $Id$
;;;
;;; Copyright (c) 2000, 2001 onShore Development, Inc.
;;;
;;; Test time functions (time.lisp)

(in-package #:clsql-tests)

(setq *rt-time*
      '(

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




(def-view-class datetest ()
  ((testtime :COLUMN "testtime" :TYPE
	     clsql-sys:wall-time :DB-KIND :BASE
	     :DB-CONSTRAINTS COMMON-LISP:NIL
	     :ACCESSOR testtime :INITARG
	     :testtime :INITFORM COMMON-LISP:NIL
	     :DB-TYPE "timestamptz")))



(deftest :time-test-retrieving/saving/retrieving
    (when (or (eq :postgresql *test-database-underlying-type*)
	      (eq :postgresql-socket *test-database-underlying-type*))
      #.(clsql-sys:locally-enable-sql-reader-syntax)
      (unless (clsql-sys:table-exists-p "datetest")
	(clsql-sys:create-view-from-class 'datetest))
      (clsql-sys:delete-records :from 'datetest)
      (let ((time (parse-timestring "2008-09-09T14:37:29.622278-04:00")))
	(clsql-sys:update-records-from-instance
	 (make-instance 'datetest :testtime time))
	(let ((o (first (clsql:select
			    'datetest
			  :limit 1 :flatp T
			  :where [= [testtime] time] ))))
	  (assert o (o) "o shouldnt be null here (we should have just inserted)")
	  (update-records-from-instance o)
	  (update-instance-from-records o)
	  (assert (time= (testtime o) time) (time o) "Time of o: ~a should be time:~a" (testtime o) time)
	  #.(clsql-sys:locally-disable-sql-reader-syntax)
	  )
	)
      nil
      )
  nil)

))
