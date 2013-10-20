;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-uffi.cl
;;;; Purpose:       Common functions for interfaces using UFFI
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-uffi)


(defun canonicalize-type-list (types auto-list)
  "Ensure a field type list meets expectations"
  (declare (optimize (speed 3) (safety 0)))
  (do ((i 0 (1+ i))
       (new-types '())
       (length-types (length types))
       (length-auto-list (length auto-list)))
      ((= i length-auto-list)
       (nreverse new-types))
    (declare (fixnum length-types length-auto-list i))
    (if (>= i length-types)
        (push t new-types) ;; types is shorter than num-fields
        (push
         (case (nth i types)
           (:int
            (case (nth i auto-list)
              (:int32
               :int32)
              (:int64
               :int64)
              (t
               t)))
           (:double
            (case (nth i auto-list)
              (:double
               :double)
              (t
               t)))
           (:int32
            (if (eq :int32 (nth i auto-list))
                :int32
                t))
           (:int64
            (if (eq :int64 (nth i auto-list))
                :int64
              t))
           (:blob
            :blob)
           (:uint
            :uint)
           (t
            t))
         new-types))))

(uffi:def-function "atoi"
    ((str (* :unsigned-char)))
  :returning :int)

(uffi:def-function ("strtoul" c-strtoul)
    ((str (* :unsigned-char))
     (endptr (* :unsigned-char))
     (radix :int))
  :returning :unsigned-long)

#-windows
(uffi:def-function ("strtoull" c-strtoull)
    ((str (* :unsigned-char))
     (endptr (* :unsigned-char))
     (radix :int))
  :returning :unsigned-long-long)

#+windows
(uffi:def-function ("_strtoui64" c-strtoull)
    ((str (* :unsigned-char))
     (endptr (* :unsigned-char))
     (radix :int))
  :returning :unsigned-long-long)

(uffi:def-function "atol"
    ((str (* :unsigned-char)))
  :returning :long)

(uffi:def-function "atof"
    ((str (* :unsigned-char)))
  :returning :double)

(uffi:def-function "atol64"
    ((str (* :unsigned-char))
     (high32 (* :unsigned-int)))
  :module "clsql-uffi"
  :returning :unsigned-int)

(uffi:def-constant +2^32+ 4294967296)
(uffi:def-constant +2^64+ 18446744073709551616)
(uffi:def-constant +2^32-1+ (1- +2^32+))

(defmacro make-64-bit-integer (high32 low32)
  `(if (zerop (ldb (byte 1 31) ,high32))
       (+ ,low32 (ash ,high32 32))
     (- (+ ,low32 (ash ,high32 32)) +2^64+)))

;; From high to low ints
(defmacro make-128-bit-integer (a b c d)
  `(+ ,d (ash ,c 32) (ash ,b 64) (ash ,a 96)))

(defmacro split-64-bit-integer (int64)
  `(values (ash ,int64 -32) (logand ,int64 +2^32-1+)))

(uffi:def-type char-ptr-def (* :unsigned-char))

(defun strtoul (char-ptr)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (type char-ptr-def char-ptr))
  (c-strtoul char-ptr uffi:+null-cstring-pointer+ 10))

(defun strtoull (char-ptr)
  (declare (optimize (speed 3) (safety 0) (space 0))
           (type char-ptr-def char-ptr))
  (c-strtoull char-ptr uffi:+null-cstring-pointer+ 10))

(defun convert-raw-field (char-ptr type &key length encoding)
 (declare (optimize (speed 3) (safety 0) (space 0))
          (type char-ptr-def char-ptr))
 (cond
   ((uffi:null-pointer-p char-ptr)
    nil)
   (t
    (case type
      (:double
       (atof char-ptr))
      (:int
       (atol char-ptr))
      (:int32
       (atoi char-ptr))
      (:uint32
       (strtoul char-ptr))
      (:uint
       (strtoul char-ptr))
      ((:int64 :uint64)
       (strtoull char-ptr)
       #|(uffi:with-foreign-object (high32-ptr :unsigned-int)
         (let ((low32 (atol64 char-ptr high32-ptr))
               (high32 (uffi:deref-pointer high32-ptr :unsigned-int)))
           (if (zerop high32)
               low32
               (make-64-bit-integer high32 low32))))|#
)
      (:blob
       (if length
           (uffi:convert-from-foreign-usb8 char-ptr length)
           (error "Can't return blob since length is not specified.")))
      (t
       (if encoding
           (if length
               (uffi:convert-from-foreign-string char-ptr
                                                 :null-terminated-p nil
                                                 :length length
                                                 :encoding encoding)
               (uffi:convert-from-foreign-string char-ptr
                                                 :null-terminated-p t
                                                 :encoding encoding))
           (if length
               (uffi:convert-from-foreign-string char-ptr
                                                 :null-terminated-p nil
                                                 :length length)
               (uffi:convert-from-foreign-string char-ptr
                                                 :null-terminated-p t))))))))
