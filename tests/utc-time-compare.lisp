(in-package :clsql-tests)

;;;; This file contains a test ensuring that our utc conversion goes the same
;;;; though UTC as it would through local-time, the values should match in each pairing
;;;; This is left as a manual test, because it requires local-time and nothing else does
;;;;

(ql:quickload :local-time)
(local-time:reread-timezone-repository)

(defun %localtime-timestamp-offset (time)
  (when (time-is-utc? time)
    (return-from %localtime-timestamp-offset 0))
  (multiple-value-bind (tusec tsec tmin thour tday tmonth tyear)
      (decode-time time)
    (multiple-value-bind (_nsec _sec _min _hour _day _month _year
                          _day-of-week _daylight-saving-time-p
                          offset)
        (local-time:decode-timestamp
         (local-time:encode-timestamp
          (* 1000 (or tusec 0)) tsec tmin thour tday tmonth tyear))
      (declare (ignore _nsec _sec _min _hour _day _month _year
                       _day-of-week _daylight-saving-time-p))
      offset)))

(list
 (let* ((clsql-sys::*default-timezone* -1)
        (clsql-sys::*default-timezone-is-dst?* t)
        (local-time:*default-timezone*
          (local-time:find-timezone-by-location-name "Europe/Berlin"))
        (ts (parse-timestring "2017-07-01T08:10:00")))
   (list (multiple-value-list (clsql-sys::%universal-ts-offset ts))
         (multiple-value-list  (clsql-sys::%localtime-timestamp-offset ts))))

 (let* ((clsql-sys::*default-timezone* -1)
        (clsql-sys::*default-timezone-is-dst?* nil)
        (local-time:*default-timezone*
          (local-time:find-timezone-by-location-name "Europe/Berlin"))
        (ts (parse-timestring "2017-12-01T08:10:00")))
   (list (multiple-value-list (clsql-sys::%universal-ts-offset ts))
         (multiple-value-list  (clsql-sys::%localtime-timestamp-offset ts))))

 (let* ((clsql-sys::*default-timezone* 5)
        (clsql-sys::*default-timezone-is-dst?* t)
        (local-time:*default-timezone*
          (local-time:find-timezone-by-location-name "America/New_York"))
        (ts (parse-timestring "2017-07-01T08:10:00")))
   (list (multiple-value-list (clsql-sys::%universal-ts-offset ts))
         (multiple-value-list  (clsql-sys::%localtime-timestamp-offset ts))))

 (let* ((clsql-sys::*default-timezone* 5)
        (clsql-sys::*default-timezone-is-dst?* nil)
        (local-time:*default-timezone*
          (local-time:find-timezone-by-location-name "America/New_York"))
        (ts (parse-timestring "2017-12-01T08:10:00")))
   (list (multiple-value-list (clsql-sys::%universal-ts-offset ts))
         (multiple-value-list  (clsql-sys::%localtime-timestamp-offset ts))))
 )
