;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; A variety of structures and function for creating and
;;;; manipulating dates, times, durations and intervals for
;;;; CLSQL.
;;;;
;;;; This file was originally part of ODCL and is Copyright (c) 2002 -
;;;; 2003 onShore Development, Inc.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

;; ------------------------------------------------------------
;; Months

(defvar *month-keywords*
  '(:january :february :march :april :may :june :july :august :september
    :october :november :december))

(defvar *month-names*
  '("" "January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))

(defun month-name (month-index)
  (nth month-index *month-names*))

(defun ordinal-month (month-keyword)
  "Return the zero-based month number for the given MONTH keyword."
  (position month-keyword *month-keywords*))


;; ------------------------------------------------------------
;; Days

(defvar *day-keywords*
  '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))

(defvar *day-names*
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun day-name (day-index)
  (nth day-index *day-names*))

(defun ordinal-day (day-keyword)
  "Return the zero-based day number for the given DAY keyword."
  (position day-keyword *day-keywords*))


;; ------------------------------------------------------------
;; time classes: wall-time, duration

(eval-when (:compile-toplevel :load-toplevel)

(defstruct (wall-time (:conc-name time-)
                      (:constructor %make-wall-time)
                      (:print-function %print-wall-time))
  (mjd 0 :type fixnum)
  (second 0 :type fixnum)
  (usec 0 :type fixnum))

(defun %print-wall-time (time stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<WALL-TIME: ~a>" (format-time nil time))
      (format-time stream time :format :pretty)))

(defstruct (duration (:constructor %make-duration)
                     (:print-function %print-duration))
  (year 0 :type fixnum)
  (month 0 :type fixnum)
  (day 0 :type fixnum)
  (hour 0 :type fixnum)
  (second 0 :type fixnum)
  (minute 0 :type fixnum)
  (usec 0 :type fixnum))

(defun %print-duration (duration stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<DURATION: ~a>"
              (format-duration nil duration :precision :second))
      (format-duration stream duration :precision :second)))

(defstruct (date (:constructor %make-date)
                 (:print-function %print-date))
  (mjd 0 :type fixnum))

(defun %print-date (date stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<DATE: ~a>" (format-date nil date))
      (format-date stream date :format :pretty)))

);eval-when

(defun duration-timestring (duration)
  (let ((second (duration-second duration))
        (minute (duration-minute duration))
        (hour (duration-hour duration))
        (day (duration-day duration))
        (month (duration-month duration))
        (year (duration-year duration)))
    (format nil "P~dY~dM~dD~dH~dM~dS" year month day hour minute second)))


;; ------------------------------------------------------------
;; Constructors

(defun make-time (&key (year 0) (month 1) (day 1) (hour 0) (minute 0)
                       (second 0) (usec 0) (offset 0))
  (let ((mjd (gregorian-to-mjd month day year))
        (sec (+ (* hour 60 60)
                (* minute 60)
                second (- offset))))
    (multiple-value-bind (day-add raw-sec)
        (floor sec (* 60 60 24))
      (%make-wall-time :mjd (+ mjd day-add) :second raw-sec :usec usec))))

(defun make-date (&key (year 0) (month 1) (day 1) (hour 0) (minute 0)
                       (second 0) (usec 0) (offset 0))
  (time->date (make-time :year year :month month :day day :hour hour
                         :minute minute :second second :usec usec :offset offset)))

(defun copy-time (time)
  (%make-wall-time :mjd (time-mjd time)
                   :second (time-second time)))

(defun utime->time (utime)
  "Return a pair: (GREGORIAN DAY . TIME-OF-DAY)"
  (multiple-value-bind (second minute hour day mon year)
      (decode-universal-time utime)
    (make-time :year year :month mon :day day :hour hour :minute minute
               :second second)))

(defun date->time (date)
  "Returns a walltime for the given date"
  (%make-wall-time :mjd (date-mjd date)))

(defun time->date (time)
  "Returns a date for the given wall time (obvious loss in resolution)"
  (%make-date :mjd (time-mjd time)))

(defun get-time ()
  "Return a pair: (GREGORIAN DAY . TIME-OF-DAY)"
  (utime->time (get-universal-time)))

(defun get-date ()
  "Returns a date for today"
  (time->date (get-time)))

(defun make-duration (&key (year 0) (month 0) (day 0) (hour 0) (minute 0)
                      (second 0) (usec 0))
  (multiple-value-bind (second-add usec-1000000)
      (floor usec 1000000)
    (multiple-value-bind (minute-add second-60)
        (floor (+ second second-add) 60)
      (multiple-value-bind (hour-add minute-60)
          (floor (+ minute minute-add) 60)
        (multiple-value-bind (day-add hour-24)
            (floor (+ hour hour-add) 24)
          (%make-duration :year year :month month :day (+ day day-add)
                          :hour hour-24
                          :minute minute-60
                          :second second-60
                          :usec usec-1000000))))))


;; ------------------------------------------------------------
;; Accessors

(defun time-hms (time)
  (multiple-value-bind (hourminute second)
      (floor (time-second time) 60)
    (multiple-value-bind (hour minute)
        (floor hourminute 60)
      (values hour minute second))))

(defun time-ymd (time)
  (destructuring-bind (month day year)
      (mjd-to-gregorian (time-mjd time))
    (values year month day)))

(defun time-dow (time)
  "Return the 0 indexed Day of the week starting with Sunday"
  (mod (+ 3 (time-mjd time)) 7))

(defun decode-time (time)
  "returns the decoded time as multiple values: usec, second, minute, hour,
  day, month, year, integer day-of-week"
  (multiple-value-bind (year month day)
      (time-ymd time)
    (multiple-value-bind (hour minute second)
        (time-hms time)
      (values (time-usec time) second minute hour day month year (mod (+ (time-mjd time) 3) 7)))))

(defun date-ymd (date)
  (time-ymd (date->time date)))

(defun date-dow (date)
  (time-dow (date->time date)))

(defun decode-date (date)
  "returns the decoded date as multiple values: day month year integer day-of-week"
  (multiple-value-bind (year month day)
      (time-ymd (date->time date))
    (values day month year (date-dow date))))

;; duration specific
(defun duration-reduce (duration precision &optional round)
  (ecase precision
    (:usec
     (+ (duration-usec duration)
        (* (duration-reduce duration :second) 1000000)))
    (:second
     (+ (if round
            (floor (duration-usec duration) 500000)
            0)
        (duration-second duration)
        (* (duration-reduce duration :minute) 60)))
    (:minute
     (+ (if round
            (floor (duration-second duration) 30)
            0)
        (duration-minute duration)
        (* (duration-reduce duration :hour) 60)))
    (:hour
     (+ (if round
            (floor (duration-minute duration) 30)
            0)
        (duration-hour duration)
        (* (duration-reduce duration :day) 24)))
    (:day
     (+ (if round
            (floor (duration-hour duration) 12)
            0)
        (duration-day duration)))))


;; ------------------------------------------------------------
;; Arithemetic and comparators

(defun duration= (duration-a duration-b)
  (= (duration-reduce duration-a :usec)
     (duration-reduce duration-b :usec)))

(defun duration< (duration-a duration-b)
  (< (duration-reduce duration-a :usec)
     (duration-reduce duration-b :usec)))

(defun duration<= (duration-a duration-b)
  (<= (duration-reduce duration-a :usec)
      (duration-reduce duration-b :usec)))

(defun duration>= (x y)
  (duration<= y x))

(defun duration> (x y)
  (duration< y x))

(defun %time< (x y)
  (let ((mjd-x (time-mjd x))
        (mjd-y (time-mjd y)))
    (if (/= mjd-x mjd-y)
        (< mjd-x mjd-y)
        (if (/= (time-second x) (time-second y))
            (< (time-second x) (time-second y))
            (< (time-usec x) (time-usec y))))))

(defun %time>= (x y)
  (if (/= (time-mjd x) (time-mjd y))
      (>= (time-mjd x) (time-mjd y))
      (if (/= (time-second x) (time-second y))
          (>= (time-second x) (time-second y))
          (>= (time-usec x) (time-usec y)))))

(defun %time<= (x y)
  (if (/= (time-mjd x) (time-mjd y))
      (<= (time-mjd x) (time-mjd y))
      (if (/= (time-second x) (time-second y))
          (<= (time-second x) (time-second y))
          (<= (time-usec x) (time-usec y)))))

(defun %time> (x y)
  (if (/= (time-mjd x) (time-mjd y))
      (> (time-mjd x) (time-mjd y))
      (if (/= (time-second x) (time-second y))
          (> (time-second x) (time-second y))
          (> (time-usec x) (time-usec y)))))

(defun %time= (x y)
  (and (= (time-mjd x) (time-mjd y))
       (= (time-second x) (time-second y))
       (= (time-usec x) (time-usec y))))

(defun time= (number &rest more-numbers)
  "Returns T if all of its arguments are numerically equal, NIL otherwise."
  (do ((nlist more-numbers (cdr nlist)))
      ((atom nlist) t)
     (declare (list nlist))
     (if (not (%time= (car nlist) number)) (return nil))))

(defun time/= (number &rest more-numbers)
  "Returns T if no two of its arguments are numerically equal, NIL otherwise."
  (do* ((head number (car nlist))
        (nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (unless (do* ((nl nlist (cdr nl)))
                  ((atom nl) t)
               (declare (list nl))
               (if (%time= head (car nl)) (return nil)))
       (return nil))))

(defun time< (number &rest more-numbers)
  "Returns T if its arguments are in strictly increasing order, NIL otherwise."
  (do* ((n number (car nlist))
        (nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (%time< n (car nlist))) (return nil))))

(defun time> (number &rest more-numbers)
  "Returns T if its arguments are in strictly decreasing order, NIL otherwise."
  (do* ((n number (car nlist))
        (nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (%time> n (car nlist))) (return nil))))

(defun time<= (number &rest more-numbers)
  "Returns T if arguments are in strictly non-decreasing order, NIL otherwise."
  (do* ((n number (car nlist))
        (nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (%time<= n (car nlist))) (return nil))))

(defun time>= (number &rest more-numbers)
  "Returns T if arguments are in strictly non-increasing order, NIL otherwise."
  (do* ((n number (car nlist))
        (nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (list nlist))
     (if (not (%time>= n (car nlist))) (return nil))))

(defun time-max (number &rest more-numbers)
  "Returns the greatest of its arguments."
  (do ((nlist more-numbers (cdr nlist))
       (result number))
      ((null nlist) (return result))
     (declare (list nlist))
     (if (%time> (car nlist) result) (setf result (car nlist)))))

(defun time-min (number &rest more-numbers)
  "Returns the least of its arguments."
  (do ((nlist more-numbers (cdr nlist))
       (result number))
      ((null nlist) (return result))
     (declare (list nlist))
     (if (%time< (car nlist) result) (setf result (car nlist)))))

(defun time-compare (time-a time-b)
  (let ((mjd-a (time-mjd time-a))
        (mjd-b (time-mjd time-b))
        (sec-a (time-second time-a))
        (sec-b (time-second time-b))
        (usec-a (time-usec time-a))
        (usec-b (time-usec time-b)))
    (if (= mjd-a mjd-b)
        (if (= sec-a sec-b)
            (if (= usec-a usec-b)
                :equal
                (if (< usec-a usec-b)
                    :less-than
                    :greater-than))
            (if (< sec-a sec-b)
                :less-than
                :greater-than))
        (if (< mjd-a mjd-b)
            :less-than
            :greater-than))))

; now the same for dates
(eval-when (:compile-toplevel :load-toplevel)
(defun replace-string (string1 search-string replace-string &key (test #'string=))
  "Search within string1 for search-string, replace with replace-string, non-destructively."
  (let ((replace-string-length (length replace-string))
        (search-string-length  (length search-string)))
    (labels ((sub-replace-string (current-string position)
               (let ((found-position (search search-string current-string :test test :start2 position)))
                 (if (null found-position)
                     current-string
                     (sub-replace-string (concatenate 'string
                                                      (subseq current-string 0 found-position)
                                                      replace-string
                                                      (subseq current-string (+ found-position search-string-length)))
                                         (+ position replace-string-length))))))
      (sub-replace-string string1 0))))
);eval-when

(defmacro wrap-time-for-date (time-func &key (result-func))
  (let ((date-func (intern (replace-string (symbol-name time-func)
                                           (symbol-name-default-case "TIME")
                                           (symbol-name-default-case "DATE")))))
    `(defun ,date-func (number &rest more-numbers)
      (let ((result (apply #',time-func (mapcar #'date->time (cons number more-numbers)))))
        ,(if result-func
             `(funcall #',result-func result)
             'result)))))

(wrap-time-for-date time=)
(wrap-time-for-date time/=)
(wrap-time-for-date time<)
(wrap-time-for-date time>)
(wrap-time-for-date time<=)
(wrap-time-for-date time>=)
(wrap-time-for-date time-max :result-func time->date)
(wrap-time-for-date time-min :result-func time->date)

(defun date-compare (date-a date-b)
  (time-compare (date->time date-a) (date->time date-b)))

;; ------------------------------------------------------------
;; Formatting and output
(defun db-timestring (time &key stream)
  "return the string to store the given time in the database"
  (if stream
      (progn (write-char #\' stream) (iso-timestring time :stream stream) (write-char #\' stream))
      (concatenate 'string "'" (iso-timestring time) "'")))

(defun iso-timestring (time &key stream)
  (multiple-value-bind (usec sec min hour day month year dow)
      (decode-time time)
    (declare (ignore dow))
    (flet ((fmt (stream)
	     (when (< year 1000) (princ #\0 stream))
	     (when (< year 100) (princ #\0 stream))
	     (when (< year 10) (princ #\0 stream))
	     (princ year stream)
	     (princ #\- stream)
	     (when (< month 10) (princ #\0 stream))
	     (princ month stream)
	     (princ #\- stream)
	     (when (< day 10) (princ #\0 stream))
	     (princ day stream)
	     (princ #\T stream) ;strict ISO says T here isn't optional.
	     (when (< hour 10) (princ #\0 stream))
	     (princ hour stream)
	     (princ #\: stream)
	     (when (< min 10) (princ #\0 stream))
	     (princ min stream)
	     (princ #\: stream)
	     (when (< sec 10) (princ #\0 stream))
	     (princ sec stream)
	     (when (and usec (plusp usec))
	       ;; we dont do this because different dbs support differnt precision levels
	       (princ #\. stream)
	       (loop for i from 5 downto 0
		     for x10 = (expt 10 i)
		     do (multiple-value-bind (quo rem)
			    (floor (/ usec x10))
			  (setf usec (- usec (* quo x10)))
			  (princ quo stream)
			  (when (= rem 0) (return)))))
	     nil))
      (if stream
	  (fmt stream)
	  (with-output-to-string (stream)
	    (fmt stream))))))

(defun db-datestring (date)
  (db-timestring (date->time date)))
(defun iso-datestring (date)
  (iso-timestring (date->time date)))


;; ------------------------------------------------------------
;; Intervals

(defstruct interval
  (start nil)
  (end nil)
  (name nil)
  (contained nil)
  (type nil)
  (data nil))

;; fix : should also return :contains / :contained

(defun interval-relation (x y)
  "Compare the relationship of node x to node y. Returns either
:contained :contains :follows :overlaps or :precedes."
  (let ((xst  (interval-start x))
        (xend (interval-end x))
        (yst  (interval-start y))
        (yend (interval-end y)))
    (case (time-compare xst yst)
      (:equal
       (case (time-compare xend yend)
         (:less-than
          :contained)
         ((:equal :greater-than)
          :contains)))
      (:greater-than
       (case (time-compare xst yend)
         ((:equal :greater-than)
          :follows)
         (:less-than
          (case (time-compare xend yend)
            ((:less-than :equal)
             :contained)
            ((:greater-than)
             :overlaps)))))
      (:less-than
       (case (time-compare xend yst)
         ((:equal :less-than)
          :precedes)
         (:greater-than
          (case (time-compare xend yend)
            (:less-than
             :overlaps)
            ((:equal :greater-than)
             :contains))))))))

;; ------------------------------------------------------------
;; interval lists

(defun sort-interval-list (list)
  (sort list (lambda (x y)
               (case (interval-relation x y)
                 ((:precedes :contains) t)
                 ((:follows :overlaps :contained) nil)))))

;; interval push will return its list of intervals in strict order.
(defun interval-push (interval-list interval &optional container-rule)
  (declare (ignore container-rule))
  (let ((sorted-list (sort-interval-list interval-list)))
    (dotimes (x (length sorted-list))
      (let ((elt (nth x sorted-list)))
        (case (interval-relation elt interval)
          (:follows
           (return-from interval-push (insert-at-index x sorted-list interval)))
          (:contains
           (return-from interval-push
             (replace-at-index x sorted-list
                               (make-interval :start (interval-start elt)
                                              :end (interval-end elt)
                                              :type (interval-type elt)
                                              :contained (interval-push (interval-contained elt) interval)
                                              :data (interval-data elt)))))
          ((:overlaps :contained)
           (error "Overlap")))))
    (append sorted-list (list interval))))

;; interval lists

(defun interval-match (list time)
  "Return the index of the first interval in list containing time"
  ;; this depends on ordering of intervals!
  (let ((list (sort-interval-list list)))
    (dotimes (x (length list))
      (let ((elt (nth x list)))
        (when (and (time<= (interval-start elt) time)
                   (time< time (interval-end elt)))
          (return-from interval-match x))))))

(defun interval-clear (list time)
  (dotimes (x (length list))
    (let ((elt (nth x list)))
      (when (and (time<= (interval-start elt) time)
                 (time< time (interval-end elt)))
        (if (interval-match (interval-contained elt) time)
            (return-from interval-clear
              (replace-at-index x list
                                (make-interval :start (interval-start elt)
                                               :end (interval-end elt)
                                               :type (interval-type elt)
                                               :contained (interval-clear (interval-contained elt) time)
                                               :data (interval-data elt))))
            (return-from interval-clear
              (delete-at-index x list)))))))

(defun interval-edit (list time start end &optional tag)
  "Attempts to modify the most deeply nested interval in list which
begins at time.  If no changes are made, returns nil."
  ;; function required sorted interval list
  (let ((list (sort-interval-list list)))
    (if (null list) nil
      (dotimes (x (length list))
        (let ((elt (nth x list)))
          (when (and (time<= (interval-start elt) time)
                     (time< time (interval-end elt)))
            (or (interval-edit (interval-contained elt) time start end tag)
                (cond ((and (< 0 x)
                            (time< start (interval-end (nth (1- x) list))))
                       (error "Overlap of previous interval"))
                      ((and (< x (1- (length list)))
                            (time< (interval-start (nth (1+ x) list)) end))
                       (error "~S ~S ~S ~S Overlap of next interval" x (length list) (interval-start (nth (1+ x) list)) end ))
                      ((time= (interval-start elt) time)
                       (return-from interval-edit
                         (replace-at-index x list
                                           (make-interval :start start
                                                          :end end
                                                          :type (interval-type elt)
                                                          :contained (restrict-intervals (interval-contained elt) start end)
                                                          :data (or tag (interval-data elt))))))))))))))

(defun restrict-intervals (list start end &aux newlist)
  (let ((test-interval (make-interval :start start :end end)))
    (dolist (elt list)
      (when (equal :contained
                   (interval-relation elt test-interval))
        (push elt newlist)))
    (nreverse newlist)))

;;; utils from odcl/list.lisp

(defun replace-at-index (idx list elt)
  (cond ((= idx 0)
         (cons elt (cdr list)))
        ((= idx (1- (length list)))
         (append (butlast list) (list elt)))
        (t
         (append (subseq list 0 idx)
                 (list elt)
                 (subseq list (1+ idx))))))

(defun insert-at-index (idx list elt)
  (cond ((= idx 0)
         (cons elt list))
        ((= idx (1- (length list)))
         (append list (list elt)))
        (t
         (append (subseq list 0 idx)
                 (list elt)
                 (subseq list idx)))))

(defun delete-at-index (idx list)
  (cond ((= idx 0)
         (cdr list))
        ((= idx (1- (length list)))
         (butlast list))
        (t
         (append (subseq list 0 idx)
                 (subseq list (1+ idx))))))


;; ------------------------------------------------------------
;; return MJD for Gregorian date

(defun gregorian-to-mjd (month day year)
  (let ((b 0)
        (month-adj month)
        (year-adj (if (< year 0)
                      (+ year 1)
                      year))
        d
        c)
    (when (< month 3)
      (incf month-adj 12)
      (decf year-adj))
    (unless (or (< year 1582)
                (and (= year 1582)
                     (or (< month 10)
                         (and (= month 10)
                              (< day 15)))))
      (let ((a (floor (/ year-adj 100))))
        (setf b (+ (- 2 a) (floor (/ a 4))))))
    (if (< year-adj 0)
        (setf c (floor (- (* 365.25d0 year-adj) 679006.75d0)))
        (setf c (floor (- (* 365.25d0 year-adj) 679006d0))))
    (setf d (floor (* 30.6001 (+ 1 month-adj))))
    ;; (cmsg "b ~s c ~s d ~s day ~s" b c d day)
    (+ b c d day)))

;; convert MJD to Gregorian date

(defun mjd-to-gregorian (mjd)
  (let (z r g a b c year month day)
    (setf z (floor (+ mjd 678882)))
    (setf r (- (+ mjd 678882) z))
    (setf g (- z .25))
    (setf a (floor (/ g 36524.25)))
    (setf b (- a (floor (/ a 4))))
    (setf year (floor (/ (+ b g) 365.25)))
    (setf c (- (+ b z) (floor (* 365.25 year))))
    (setf month (truncate (/ (+ (* 5 c) 456) 153)))
    (setf day (+ (- c (truncate (/ (- (* 153 month) 457) 5))) r))
    (when (> month 12)
      (incf year)
      (decf month 12))
    (list month day year)))

(defun duration+ (time &rest durations)
  "Add each DURATION to TIME, returning a new wall-time value."
  (let ((year   (duration-year time))
        (month  (duration-month time))
        (day    (duration-day time))
        (hour   (duration-hour time))
        (minute (duration-minute time))
        (second (duration-second time))
        (usec   (duration-usec time)))
    (dolist (duration durations)
      (incf year    (duration-year duration))
      (incf month   (duration-month duration))
      (incf day     (duration-day duration))
      (incf hour    (duration-hour duration))
      (incf minute  (duration-minute duration))
      (incf second  (duration-second duration))
      (incf usec    (duration-usec duration)))
    (make-duration :year year :month month :day day :hour hour :minute minute
                   :second second :usec usec)))

(defun duration- (duration &rest durations)
    "Subtract each DURATION from TIME, returning a new duration value."
  (let ((year   (duration-year duration))
        (month  (duration-month duration))
        (day    (duration-day duration))
        (hour   (duration-hour duration))
        (minute (duration-minute duration))
        (second (duration-second duration))
        (usec   (duration-usec duration)))
    (dolist (duration durations)
      (decf year    (duration-year duration))
      (decf month   (duration-month duration))
      (decf day     (duration-day duration))
      (decf hour    (duration-hour duration))
      (decf minute  (duration-minute duration))
      (decf second  (duration-second duration))
      (decf usec    (duration-usec duration)))
    (make-duration :year year :month month :day day :hour hour :minute minute
                   :second second :usec usec)))

;; Date + Duration

(defun time+ (time &rest durations)
  "Add each DURATION to TIME, returning a new wall-time value."
  (let ((new-time (copy-time time)))
    (dolist (duration durations)
      (roll new-time
            :year (duration-year duration)
            :month (duration-month duration)
            :day (duration-day duration)
            :hour (duration-hour duration)
            :minute (duration-minute duration)
            :second (duration-second duration)
            :usec (duration-usec duration)
            :destructive t))
    new-time))

(defun date+ (date &rest durations)
  "Add each DURATION to DATE, returning a new date value.
Note that (barring daylight saving time) 12h + 12h will result in a new day, but doing
it as separate calculations will not, as the time is chopped to a date before being returned."
  (time->date (apply #'time+ (cons (date->time date) durations))))

(defun time- (time &rest durations)
  "Subtract each DURATION from TIME, returning a new wall-time value."
  (let ((new-time (copy-time time)))
    (dolist (duration durations)
      (roll new-time
            :year (- (duration-year duration))
            :month (- (duration-month duration))
            :day (- (duration-day duration))
            :hour (- (duration-hour duration))
            :minute (- (duration-minute duration))
            :second (- (duration-second duration))
            :usec (- (duration-usec duration))
            :destructive t))
    new-time))

(defun date- (date &rest durations)
  "Subtract each DURATION to DATE, returning a new date value.
Note that (barring daylight saving time) 12h + 12h will result in a new day, but doing
it as separate calculations will not, as the time is chopped to a date before being returned."
  (time->date (apply #'time- (cons (date->time date) durations))))

(defun time-difference (time1 time2)
  "Returns a DURATION representing the difference between TIME1 and
TIME2."
  (flet ((do-diff (time1 time2)

  (let (day-diff sec-diff)
    (setf day-diff (- (time-mjd time2)
                      (time-mjd time1)))
    (if (> day-diff 0)
        (progn (decf day-diff)
               (setf sec-diff (+ (time-second time2)
                                 (- (* 60 60 24)
                                    (time-second time1)))))
      (setf sec-diff (- (time-second time2)
                        (time-second time1))))
     (make-duration :day day-diff
                   :second sec-diff))))
    (if (time< time1 time2)
        (do-diff time1 time2)
      (do-diff time2 time1))))

(defun date-difference (date1 date2)
  "Returns a DURATION representing the difference between TIME1 and
TIME2."
  (time-difference (date->time date1) (date->time date2)))

(defun format-date (stream date &key format
                    (date-separator "-")
                    (internal-separator " "))
  "produces on stream the datestring corresponding to the date
with the given options"
  (format-time stream (date->time date)
               :format format
               :date-separator date-separator
               :internal-separator internal-separator))

(defun format-time (stream time &key format
                    (date-separator "-")
                    (time-separator ":")
                    (internal-separator " "))
  "produces on stream the timestring corresponding to the wall-time
with the given options"
  (let ((*print-circle* nil))
    (multiple-value-bind (usec second minute hour day month year dow)
        (decode-time time)
      (case format
        (:pretty
	   (format stream "~A ~A, ~A ~D, ~D"
		   (pretty-time hour minute)
		   (day-name dow)
		   (month-name month)
		   day
		   year))
        (:short-pretty
	   (format stream "~A, ~D/~D/~D"
		   (pretty-time hour minute)
		   month day year))
        ((:iso :iso8601) (iso-timestring time :stream stream))
        (t (format stream "~2,'0D~A~2,'0D~A~2,'0D~A~2,'0D~A~2,'0D~A~2,'0D.~6,'0D"
		   year date-separator month date-separator day
		   internal-separator hour time-separator minute time-separator
		   second usec)
	   )))))

(defun pretty-time (hour minute)
  (cond
   ((eq hour 0)
    (format nil "12:~2,'0D AM" minute))
   ((eq hour 12)
    (format nil "12:~2,'0D PM" minute))
   ((< hour 12)
    (format nil "~D:~2,'0D AM" hour minute))
   ((and (> hour 12) (< hour 24))
    (format nil "~D:~2,'0D PM" (- hour 12) minute))
   (t
    (error "pretty-time got bad hour"))))

(defun leap-days-in-days (days)
  ;; return the number of leap days between Mar 1 2000 and
  ;; (Mar 1 2000) + days, where days can be negative
  (if (< days 0)
      (ceiling (/ (- days) (* 365 4)))
      (floor (/ days (* 365 4)))))

(defun current-year ()
  (third (mjd-to-gregorian (time-mjd (get-time)))))

(defun current-month ()
  (first (mjd-to-gregorian (time-mjd (get-time)))))

(defun current-day ()
  (second (mjd-to-gregorian (time-mjd (get-time)))))

(defun parse-date-time (string)
  "parses date like 08/08/01, 8.8.2001, eg"
  (when (> (length string) 1)
    (let ((m (current-month))
          (d (current-day))
          (y (current-year)))
      (let ((integers (mapcar #'parse-integer (hork-integers string))))
        (case (length integers)
          (1
           (setf y (car integers)))
          (2
           (setf m (car integers))
           (setf y (cadr integers)))
          (3
           (setf m (car integers))
           (setf d (cadr integers))
           (setf y (caddr integers)))
          (t
           (return-from parse-date-time))))
      (when (< y 100)
        (incf y 2000))
      (make-time :year y :month m :day d))))

(defun hork-integers (input)
  (let ((output '())
        (start 0))
    (dotimes (x (length input))
      (unless (<= 48 (char-code (aref input x)) 57)
        (push (subseq input start x) output)
        (setf start (1+ x))))
    (nreverse (push (subseq input start) output))))

(defun merged-time (day time-of-day)
  (%make-wall-time :mjd (time-mjd day)
                   :second (time-second time-of-day)))

(defun time-meridian (hours)
  (cond ((= hours 0)
         (values 12 "AM"))
        ((= hours 12)
         (values 12 "PM"))
        ((< 12 hours)
         (values (- hours 12) "PM"))
        (t
         (values hours "AM"))))

(defgeneric to-string (val &rest keys)
  )

(defmethod to-string ((time wall-time) &rest keys)
  (destructuring-bind (&key (style :daytime) &allow-other-keys)
      keys
    (print-date time style)))

(defun print-date (time &optional (style :daytime))
  (multiple-value-bind (usec second minute hour day month year dow)
      (decode-time time)
    (declare (ignore usec second))
    (multiple-value-bind (hours meridian)
        (time-meridian hour)
      (ecase style
        (:time-of-day
         ;; 2:00 PM
         (format nil "~d:~2,'0d ~a" hours minute meridian))
        (:long-day
         ;; October 11th, 2000
         (format nil "~a ~d, ~d" (month-name month) day year))
        (:month
         ;; October
         (month-name month))
        (:month-year
         ;; October 2000
         (format nil "~a ~d" (month-name month) year))
        (:full
         ;; 11:08 AM, November 22, 2002
         (format nil "~d:~2,'0d ~a, ~a ~d, ~d"
                 hours minute meridian (month-name month) day year))
        (:full+weekday
         ;; 11:09 AM Friday, November 22, 2002
         (format nil "~d:~2,'0d ~a ~a, ~a ~d, ~d"
                 hours minute meridian (nth dow *day-names*)
                 (month-name month) day year))
        (:daytime
         ;; 11:09 AM, 11/22/2002
         (format-time nil time :format :short-pretty))
        (:day
         ;; 11/22/2002
         (format nil "~d/~d/~d" month day year))))))

(defun time-element (time element)
  (multiple-value-bind (usec second minute hour day month year dow)
      (decode-time time)
    (declare (ignore usec))
    (ecase element
      (:seconds
       second)
      (:minutes
       minute)
      (:hours
       hour)
      (:day-of-month
       day)
      (:integer-day-of-week
       dow)
      (:day-of-week
       (nth dow *day-keywords*))
      (:month
       month)
      (:year
       year))))

(defun date-element (date element)
  (time-element (date->time date) element))

(defun format-duration (stream duration &key (precision :minute))
  (let ((second (duration-second duration))
        (minute (duration-minute duration))
        (hour (duration-hour duration))
        (day (duration-day duration))
        (month (duration-month duration))
        (year (duration-year duration))
        (return (null stream))
        (stream (or stream (make-string-output-stream))))
    (ecase precision
      (:day
       (setf hour 0 second 0 minute 0))
      (:hour
       (setf second 0 minute 0))
      (:minute
       (setf second 0))
      (:second
       t))
    (if (= 0 year month day hour minute)
        (format stream "0 minutes")
        (let ((sent? nil))
          (when (< 0 year)
            (format stream "~d year~p" year year)
            (setf sent? t))
          (when (< 0 month)
            (when sent?
              (write-char #\Space stream))
            (format stream "~d month~p" month month)
            (setf sent? t))
          (when (< 0 day)
            (when sent?
              (write-char #\Space stream))
            (format stream "~d day~p" day day)
            (setf sent? t))
          (when (< 0 hour)
            (when sent?
              (write-char #\Space stream))
            (format stream "~d hour~p" hour hour)
            (setf sent? t))
          (when (< 0 minute)
            (when sent?
              (write-char #\Space stream))
            (format stream "~d min~p" minute minute)
            (setf sent? t))
          (when (< 0 second)
            (when sent?
              (write-char #\Space stream))
            (format stream "~d sec~p" second second))))
    (when return
      (get-output-stream-string stream))))

(defgeneric midnight (self))
(defmethod midnight ((self wall-time))
  "truncate hours, minutes and seconds"
  (%make-wall-time :mjd (time-mjd self)))

(defun roll (date &key (year 0) (month 0) (day 0) (second 0) (hour 0)
             (minute 0) (usec 0) (destructive nil))
  (unless (= 0 year month)
    (multiple-value-bind (year-orig month-orig day-orig)
        (time-ymd date)
      (multiple-value-bind (new-year new-month)
         (floor (+ month month-orig (* 12 (+ year year-orig))) 12)
       (let ((new-date (make-time :year new-year
                                  :month new-month
                                  :day day-orig
                                  :second (time-second date)
                                  :usec usec)))
         (if destructive
             (setf (time-mjd date) (time-mjd new-date))
             (setq date new-date))))))
  (let ((mjd (time-mjd date))
        (sec (time-second date)))
    (multiple-value-bind (sec-new usec-new)
        (floor (+ usec (time-usec date)
                  (* 1000000
                     (+ sec second
                        (* 60 minute)
                        (* 60 60 hour))))
               1000000)
      (multiple-value-bind (mjd-new sec-new)
          (floor sec-new (* 60 60 24))
        (if destructive
            (progn
              (setf (time-mjd date) (+ mjd mjd-new day)
                    (time-second date) sec-new
                    (time-usec date) usec-new)
              date)
            (%make-wall-time :mjd (+ mjd mjd-new day)
                             :second sec-new
                             :usec usec-new))))))

(defun roll-to (date size position)
  (ecase size
    (:month
     (ecase position
       (:beginning
        (roll date :day (+ 1
                           (- (time-element date :day-of-month)))))
       (:end
        (roll date :day (+ (days-in-month (time-element date :month)
                                          (time-element date :year))
                           (- (time-element date :day-of-month)))))))))

(defun week-containing (time)
  (let* ((midn (midnight time))
         (dow (time-element midn :integer-day-of-week)))
    (list (roll midn :day (- dow))
          (roll midn :day (- 7 dow)))))

(defun leap-year? (year)
  "t if YEAR is a leap yeap in the Gregorian calendar"
  (and (= 0 (mod year 4))
       (or (not (= 0 (mod year 100)))
           (= 0 (mod year 400)))))

(defun valid-month-p (month)
  "t if MONTH exists in the Gregorian calendar"
  (<= 1 month 12))

(defun valid-gregorian-date-p (date)
  "t if DATE (year month day) exists in the Gregorian calendar"
  (let ((max-day (days-in-month (nth 1 date) (nth 0 date))))
    (<= 1 (nth 2 date) max-day)))

(defun days-in-month (month year &key (careful t))
  "the number of days in MONTH of YEAR, observing Gregorian leap year
rules"
  (declare (type fixnum month year))
  (when careful
    (check-type month (satisfies valid-month-p)
                "between 1 (January) and 12 (December)"))
  (if (eql month 2)                     ; feb
      (if (leap-year? year)
          29 28)
      (let ((even (mod (1- month) 2)))
        (if (< month 8)                 ; aug
            (- 31 even)
            (+ 30 even)))))

(defun day-of-year (year month day &key (careful t))
  "the day number within the year of the date DATE.  For example,
1987 1 1 returns 1"
  (declare (type fixnum year month day))
  (when careful
    (let ((date (list year month day)))
    (check-type date (satisfies valid-gregorian-date-p)
                "a valid Gregorian date")))
  (let ((doy (+ day (* 31 (1- month)))))
    (declare (type fixnum doy))
    (when (< 2 month)
      (setf doy (- doy (floor (+ 23 (* 4 month)) 10)))
      (when (leap-year? year)
        (incf doy)))
    doy))

(defun parse-yearstring (string)
  (let ((year (or (parse-integer-insensitively string)
                  (extract-roman string))))
    (when (and year (< 1500 year 2500))
      (make-time :year year))))

(defun parse-integer-insensitively (string)
  (let ((start (position-if #'digit-char-p string))
        (end   (position-if #'digit-char-p string :from-end t)))
    (when (and start end)
      (parse-integer (subseq string start (1+ end)) :junk-allowed t))))

(defvar *roman-digits*
  '((#\M . 1000)
    (#\D . 500)
    (#\C . 100)
    (#\L . 50)
    (#\X . 10)
    (#\V . 5)
    (#\I . 1)))

(defun extract-roman (string &aux parse)
  (dotimes (x (length string))
    (let ((val (cdr (assoc (aref string x) *roman-digits*))))
      (when (and val parse (< (car parse) val))
        (push (- (pop parse)) parse))
      (push val parse)))
  (apply #'+ parse))


;; ------------------------------------------------------------
;; Parsing iso-8601 timestrings

(define-condition iso-8601-syntax-error (sql-user-error)
  ((bad-component;; year, month whatever
    :initarg :bad-component
    :reader bad-component))
  (:report (lambda (c stream)
             (format stream "Bad component: ~A " (bad-component c)))))

(defun parse-timestring (timestring &key (start 0) end junk-allowed)
  "parse a timestring and return the corresponding wall-time.  If the
timestring starts with P, read a duration; otherwise read an ISO 8601
formatted date string."
  (declare (ignore junk-allowed))
  (etypecase timestring
    (wall-time timestring)
    (date (date->time timestring))
    (string
     (let ((string (subseq timestring start end)))
       (if (char= (aref string 0) #\P)
           (parse-iso-8601-duration string)
           (parse-iso-8601-time string))))))

(defun parse-datestring (datestring &key (start 0) end junk-allowed)
  "parse a ISO 8601 timestring and return the corresponding date.
Will throw a hissy fit if the date string is a duration. Will ignore any precision beyond day (hour/min/sec/usec)."
  (etypecase datestring
    (date datestring)
    (wall-time (time->date datestring))
    (string
     (let ((parsed-value
             (parse-timestring
              datestring :start start :end end :junk-allowed junk-allowed)))
       (etypecase parsed-value
         (date parsed-value)
         (wall-time (time->date parsed-value)))))))


(defvar *iso-8601-duration-delimiters*
  '((#\Y . :years)
    (#\D . :days)
    (#\H . :hours)
    (#\M . :months/minutes)
    (#\S . :seconds)))

(defun iso-8601-delimiter (elt)
  (cdr (assoc elt *iso-8601-duration-delimiters*)))

(defun iso-8601-duration-subseq (string end)
  (let* ((pos (position-if #'iso-8601-delimiter string :end end :from-end t))
         (pos2 (when pos
                 (position-if-not #'digit-char-p string :end pos :from-end t)))
         (number (when pos2
                   (parse-integer
                    (subseq string (1+ pos2) pos) :junk-allowed t))))
    (when number
      (values number
              (1+ pos)
              (1+ pos2)
              (iso-8601-delimiter (aref string pos))))))

(defun parse-iso-8601-duration (string)
  "return a wall-time from a duration string"
  (block parse
    (let ((years 0)
          (months 0)
          (days 0)
          (secs 0)
          (hours 0)
          (minutes 0)
          (index (length string))
          (months/minutes nil))
      (loop
       (multiple-value-bind (duration end next-index duration-type)
           (iso-8601-duration-subseq string index)
         (declare (ignore end))
         (case duration-type
           (:years
            (incf years duration))
           (:months/minutes
            (if months/minutes
                (incf months duration)
                (progn
                  (setq months/minutes t)
                  (incf minutes duration))))
           (:days
            (setq months/minutes t)
            (incf days duration))
           (:hours
            (setq months/minutes t)
            (incf hours duration))
           (:seconds
            (incf secs duration))
           (t
            (return-from parse
              (make-duration
               :year years :month months :day days :hour hours
               :minute minutes :second secs))))
         (setf index next-index))))))

;; e.g. 2000-11-11 00:00:00-06

(defun parse-iso-8601-time (string)
  "return the wall-time corresponding to the given ISO 8601 datestring"
  (multiple-value-bind (year month day hour minute second usec offset)
      (syntax-parse-iso-8601 string)
    (make-time :year year
               :month month
               :day day
               :hour hour
               :minute minute
               :second second
               :usec usec
               :offset offset)))


(defun syntax-parse-iso-8601 (string)
  ;; use strlen to determine if fractional seconds are present in timestamp
  (let ((strlen (length string))
        year month day hour minute second usec gmt-sec-offset)
    (handler-case
        (progn
          (setf year           (parse-integer string :start 0 :end 4)
                month          (parse-integer string :start 5 :end 7)
                day            (parse-integer string :start 8 :end 10)
                hour           (if (<= 13 strlen)
                                   (parse-integer string :start 11 :end 13)
                                   0)
                minute         (if (<= 16 strlen)
                                   (parse-integer string :start 14 :end 16)
                                   0)
                second         (if (<= 19 strlen)
                                   (parse-integer string :start 17 :end 19)
                                   0))
          (cond
            ((and (> strlen 19)
                  (or (char= #\, (char string 19))
                      (char= #\. (char string 19))))
             (multiple-value-bind (parsed-usec usec-end)
                 (parse-integer string :start 20 :junk-allowed t)
               (let ((parsed-usec (and parsed-usec
				       (floor (* parsed-usec (expt 10 (+ 6 (- usec-end) 20)))))))
		 (setf usec          (or parsed-usec 0)
		       gmt-sec-offset (if (<= (+ 3 usec-end)  strlen)
					  (let ((skip-to (or (position #\+ string :start 19)
							     (position #\- string :start 19))))
					    (if skip-to
						(* 60 60
						   (parse-integer string :start skip-to
								  :end (+ skip-to 3)))
						0))
					  0)))))
            (t
             (setf usec           0
                   gmt-sec-offset (if (<= 22  strlen)
                                      (let ((skip-to (or (position #\+ string :start 19)
                                                         (position #\- string :start 19))))
                                        (if skip-to
                                            (* 60 60
                                               (parse-integer string :start skip-to
                                                              :end (+ skip-to 3)))
                                            0))
                                      0))))
          (unless (< 0 year)
            (error 'iso-8601-syntax-error
                   :bad-component '(year . 0)))
          (unless (< 0 month)
            (error 'iso-8601-syntax-error
                   :bad-component '(month . 0)))
          (unless (< 0 day)
            (error 'iso-8601-syntax-error
                   :bad-component '(month . 0)))
          (values year month day hour minute second usec gmt-sec-offset))
      (simple-error ()
        (error 'iso-8601-syntax-error
               :bad-component
               (car (find-if (lambda (pair) (null (cdr pair)))
                             `((year . ,year) (month . ,month)
                               (day . ,day) (hour . ,hour)
                               (minute . ,minute) (second . ,second)
                               (usec . ,usec)
                               (timezone . ,gmt-sec-offset)))))))))
