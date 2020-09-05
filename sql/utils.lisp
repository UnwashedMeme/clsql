;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:         utils.lisp
;;;; Purpose:      SQL utility functions
;;;; Programmer:   Kevin M. Rosenberg
;;;; Date Started: Mar 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defmacro defaulting (&rest place-value-plist)
  "for places and values (as an &rest plist)
   if place-n is null set it to value-n"
  `(progn
    ,@(loop for (place value . rest) on place-value-plist by #'cddr
            collect `(unless ,place (setf ,place ,value)))))

(defmacro pop-n (place &optional (n 1))
  "pops n items off of a list in place and returns their values in a new list

   if n > the length of the list in place, then we return the full list,
     setting the place to nil"
  `(loop repeat ,n
    while ,place
    collect (pop ,place)))

(defun %get-int (v)
  (etypecase v
    (string (parse-integer v :junk-allowed t))
    (integer v)
    (number (truncate v))))

(defun dequote (it)
  (if (and (listp it) (eql (first it) 'quote))
      (second it)
      it))

(defvar +whitespace-chars+
  '(#\space #\tab #\newline #\return
    ;; Tested: sbcl unicode, allegrocl, openmcl,clisp use #\no-break_space
    ;; lispworks uses #\no-break-space
    ;; sbcl non-unicode doesn't support no break space
    ;; AllegroCL 8-bit strings don't fail on reading #\no-break_space,
    ;; but can't represent such a character
    ;; CMUCL errors when trying to read #\no-break_space
    #+(and lispworks unicode) #\no-break-space
    #+(or (and sbcl sb-unicode) (and allegro ics) (and clisp i18n)
       (and openmcl openmcl-unicode-strings))
    #\no-break_space
    )
  "List of whitespace characters for this lisp implementation.")

(defun number-to-sql-string (num)
  (etypecase num
    (integer
     (princ-to-string num))
    (rational
     (float-to-sql-string (coerce num 'double-float)))
    (number
     (float-to-sql-string num))))

(defun float-to-sql-string (num)
  "Convert exponent character for SQL"
  (let ((str (write-to-string num :readably t)))
    (declare (type string str))
    (cond
     ((find #\f str)
      (substitute #\e #\f str))
     ((find #\d str)
      (substitute #\e #\d str))
     ((find #\l str)
      (substitute #\e #\l str))
     ((find #\s str)
      (substitute #\e #\S str))
     ((find #\F str)
      (substitute #\e #\F str))
     ((find #\D str)
      (substitute #\e #\D str))
     ((find #\L str)
      (substitute #\e #\L str))
     ((find #\S str)
      (substitute #\e #\S str))
     (t
      str))))

(defun sql-escape (identifier)
  "Change hyphens to underscores, ensure string"
  (let ((unescaped (etypecase identifier
                     (symbol (symbol-name identifier))
                     (string identifier))))
    (substitute #\_ #\- unescaped)))

#+lispworks
(defvar +lw-has-without-preemption+
  #-(or lispworks5 lispworks4) nil
  #+(or lispworks5 lispworks4) t)
#+lispworks
(defvar +lw-global-lock+
  (unless +lw-has-without-preemption+
    (mp:make-lock :name "CLSQL" :important-p nil :safep t :recursivep nil
                  :sharing t)))

(defmacro without-interrupts (&body body)
  #+allegro `(mp:without-scheduling ,@body)
  #+clisp `(progn ,@body)
  #+cmu `(system:without-interrupts ,@body)
  #+lispworks
  (if +lw-has-without-preemption+
      `(mp:without-preemption ,@body)
      `(mp:with-exclusive-lock (+lw-global-lock+)
         ,@body))
  #+openmcl `(ccl:without-interrupts ,@body)
  #+sbcl `(sb-sys::without-interrupts ,@body))

(defun make-process-lock (name)
  #+allegro (mp:make-process-lock :name name)
  #+cmu (mp:make-lock name)
  #+lispworks (mp:make-lock :name name)
  #+openmcl (ccl:make-lock name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #+scl (thread:make-lock name)
  #-(or allegro cmu lispworks openmcl sb-thread scl) (declare (ignore name))
  #-(or allegro cmu lispworks openmcl sb-thread scl) nil)

(defmacro with-process-lock ((lock desc) &body body)
  #+(or cmu allegro lispworks openmcl sb-thread)
  (declare (ignore desc))
  #+(or allegro cmu lispworks openmcl sb-thread)
  (let ((l (gensym)))
    `(let ((,l ,lock))
      #+allegro (mp:with-process-lock (,l) ,@body)
      #+cmu (mp:with-lock-held (,l) ,@body)
      #+openmcl (ccl:with-lock-grabbed (,l) ,@body)
      #+lispworks (mp:with-lock (,l) ,@body)
      #+sb-thread (sb-thread:with-recursive-lock (,l) ,@body)
      ))
  #+scl `(thread:with-lock-held (,lock ,desc) ,@body)
  #-(or cmu allegro lispworks openmcl sb-thread scl) (declare
                                                      (ignore lock desc))
  #-(or cmu allegro lispworks openmcl sb-thread scl) `(progn ,@body))

(defun sql-escape-quotes (s)
  "Escape quotes for SQL string writing"
  (substitute-string-for-char s #\' "''"))

(defun substitute-string-for-char (procstr match-char subst-str)
  "Substitutes a string for a single matching character of a string"
  (when procstr
    (locally
        (declare (type string procstr))
      (let ((pos (position match-char procstr)))
        (if pos
            (concatenate 'string
                         (subseq procstr 0 pos) subst-str
                         (substitute-string-for-char
                          (subseq procstr (1+ pos)) match-char subst-str))
            procstr)))))


(defun position-char (char string start max)
  "From KMRCL."
  (declare (optimize (speed 3) (safety 0) (space 0))
           (fixnum start max) (simple-string string))
  (do* ((i start (1+ i)))
       ((= i max) nil)
    (declare (fixnum i))
    (when (char= char (schar string i)) (return i))))

(defun delimited-string-to-list (string &optional (separator #\space)
                                                  skip-terminal)
  "Split a string with delimiter, from KMRCL."
  (declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))
           (type string string)
           (type character separator))
  (do* ((len (length string))
        (output '())
        (pos 0)
        (end (position-char separator string pos len)
             (position-char separator string pos len)))
       ((null end)
        (if (< pos len)
            (push (subseq string pos) output)
            (when (or (not skip-terminal) (zerop len))
              (push "" output)))
        (nreverse output))
    (declare (type fixnum pos len)
             (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

(defun string-to-list-connection-spec (str)
  (declare (type string str))
  (let ((at-pos (position-char #\@ str 0 (length str))))
    (cond
      ((and at-pos (> (length str) at-pos))
       ;; Connection spec is SQL*NET format
       (cons (subseq str (1+ at-pos))
             (delimited-string-to-list (subseq str 0 at-pos) #\/)))
      (t
       (delimited-string-to-list str #\/)))))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:excl.osi)
    (require 'osi)))

(defun command-output (control-string &rest args)
  ;; Concatenates output and error since Lispworks combines
  ;; these, thus CLSQL can't depend upon separate results
  (multiple-value-bind (output error status)
      (apply #'%command-output control-string args)
    (values
     (concatenate 'string (if output output "")
                  (if error error ""))
     status)))

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))
      (do ((line (read-line in nil eof)
                 (read-line in nil eof)))
          ((eq line eof))
        (format out "~A~%" line)))))

;; From KMRCL
(defun %command-output (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell,
returns (VALUES string-output error-output exit-status)"
  (let ((command (apply #'format nil control-string args)))
    #+sbcl
    (let* ((process (sb-ext:run-program
                    "/bin/sh"
                    (list "-c" command)
                    :input nil :output :stream :error :stream))
           (output (read-stream-to-string (sb-impl::process-output process)))
           (error (read-stream-to-string (sb-impl::process-error process))))
      (close (sb-impl::process-output process))
      (close (sb-impl::process-error process))
      (values
       output
       error
       (sb-impl::process-exit-code process)))


    #+(or cmu scl)
    (let* ((process (ext:run-program
                     "/bin/sh"
                     (list "-c" command)
                     :input nil :output :stream :error :stream))
           (output (read-stream-to-string (ext::process-output process)))
           (error (read-stream-to-string (ext::process-error process))))
      (close (ext::process-output process))
      (close (ext::process-error process))

      (values
       output
       error
       (ext::process-exit-code process)))

    #+allegro
    (multiple-value-bind (output error status)
        (excl.osi:command-output command :whole t)
      (values output error status))

    #+lispworks
    ;; BUG: Lispworks combines output and error streams
    (let ((output (make-string-output-stream)))
      (unwind-protect
          (let ((status
                 (system:call-system-showing-output
                  command
                  :shell-type "/bin/sh"
                  :output-stream output)))
            (values (get-output-stream-string output) nil status))
        (close output)))

    #+clisp
    ;; BUG: CLisp doesn't allow output to user-specified stream
    (values
     nil
     nil
     (ext:run-shell-command  command :output :terminal :wait t))

    #+openmcl
    (let* ((process (ccl:run-program
                     "/bin/sh"
                     (list "-c" command)
                     :input nil :output :stream :error :stream
                     :wait t))
           (output (read-stream-to-string (ccl::external-process-output-stream process)))
           (error (read-stream-to-string (ccl::external-process-error-stream process))))
      (close (ccl::external-process-output-stream process))
      (close (ccl::external-process-error-stream process))
      (values output
              error
              (nth-value 1 (ccl::external-process-status process))))

    #-(or openmcl clisp lispworks allegro scl cmu sbcl)
    (error "COMMAND-OUTPUT not implemented for this Lisp")

    ))


;; From KMRCL
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

;; From KMRCL
(defun substitute-char-string (procstr match-char subst-str)
  "Substitutes a string for a single matching character of a string"
  (substitute-chars-strings procstr (list (cons match-char subst-str))))

(defun replaced-string-length (str repl-alist)
  (declare (simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
    (do* ((i 0 (1+ i))
          (orig-len (length str))
          (new-len orig-len))
         ((= i orig-len) new-len)
      (declare (fixnum i orig-len new-len))
      (let* ((c (char str i))
             (match (assoc c repl-alist :test #'char=)))
        (declare (character c))
        (when match
          (incf new-len (1- (length
                             (the simple-string (cdr match)))))))))


(defun substitute-chars-strings (str repl-alist)
  "Replace all instances of a chars with a string. repl-alist is an assoc
list of characters and replacement strings."
  (declare (simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((orig-len (length str))
        (new-string (make-string (replaced-string-length str repl-alist)))
        (spos 0 (1+ spos))
        (dpos 0))
      ((>= spos orig-len)
       new-string)
    (declare (fixnum spos dpos) (simple-string new-string))
    (let* ((c (char str spos))
           (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (if match
          (let* ((subst (cdr match))
                 (len (length subst)))
            (declare (fixnum len)
                     (simple-string subst))
            (dotimes (j len)
              (declare (fixnum j))
              (setf (char new-string dpos) (char subst j))
              (incf dpos)))
        (progn
          (setf (char new-string dpos) c)
          (incf dpos))))))


(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+lispworks (lw:environment-variable (string var))
  #+ccl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (char= #\a (schar (symbol-name '#:a) 0))
    (pushnew :clsql-lowercase-reader *features*)))

(defun symbol-name-default-case (str)
  #-clsql-lowercase-reader
  (string-upcase str)
  #+clsql-lowercase-reader
  (string-downcase str))

(defun convert-to-db-default-case (str database)
  (if database
      (case (db-type-default-case (database-underlying-type database))
        (:upper (string-upcase str))
        (:lower (string-downcase str))
        (t str))
    ;; Default CommonSQL behavior is to upcase strings
    (string-upcase str)))

(defun ensure-keyword (name)
  "Returns keyword for a name."
  (etypecase name
    (keyword name)
    (string (nth-value 0 (intern (symbol-name-default-case name) :keyword)))
    (symbol (nth-value 0 (intern (symbol-name name) :keyword)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :clsql-lowercase-reader cl:*features*)))

(defun replace-all (string part replacement &key (test #'char=) stream)
  "Returns a new string in which all the occurences of the part
is replaced with replacement. [FROM http://cl-cookbook.sourceforge.net/strings.html#manip]"
  (let ((out (or stream (make-string-output-stream))))
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
		   :start old-pos
		   :end (or pos (length string)))
	  when pos do (write-string replacement out)
	    while pos)
    (unless stream
      (get-output-stream-string out))))

(defun read-decimal-value (string)
  (let* ((comma 0)
         (dot 0)
         (last))
    (loop for c across string
          do (case c
               (#\. (incf dot) (setf last 'dot))
               (#\, (incf comma) (setf last 'comma))))
    (let* ((bag (if (and (eql last 'dot) (eql dot 1))
                    ".0123456789+-"
                    ",0123456789+-"))
           (clean (with-output-to-string (out)
                    (loop for c across string
                          do (when (find c bag :test #'char=)
                               (write-char c out))))))
      (if (and (eql last 'dot) (eql dot 1))
          (decimals:parse-decimal-number clean)
          (decimals:parse-decimal-number
           clean :decimal-separator #\,)))))


(defun filter-plist (plist &rest keys-to-remove)
  "Returns a copy of the given plist with indicated key-value pairs
removed. keys are searched with #'MEMBER"
  (declare (dynamic-extent keys-to-remove))
  (when plist
    (loop for (k v . rest) = plist then rest
          unless (member k keys-to-remove)
            collect k and collect v
          while rest)))

(defmacro make-weak-hash-table (&rest args)
  "Creates a weak hash table for use in a cache."
  `(progn

    ;;NB: These are generally used for caches that may not have an alternate
    ;;clearing mechanism. If you are on an implementation that doesn't support
    ;;weak hash tables then you're memory may accumulate.

    #-(or sbcl allegro clisp lispworks ccl)
    (warn "UNSAFE! use of weak hash on implementation without support. (see clsql/sql/utils.lisp to add)")

    (make-hash-table
      #+allegro   :values    #+allegro :weak
      #+clisp     :weak      #+clisp :value
      #+lispworks :weak-kind #+lispworks :value
      #+sbcl :weakness #+sbcl :value
      #+ccl :weak #+ccl :value
      ,@args)
    ))

(defun to-slot-name (slot)
  "try to turn what we got representing the slot into a slot name"
  (etypecase slot
    (symbol slot)
    (slot-definition (slot-definition-name slot))))

(defun to-class (it)
  (etypecase it
    (class it)
    (symbol (find-class it))
    (standard-object (class-of it))))

(defun to-class-name (o)
  (etypecase o
    (symbol o)
    (standard-class (class-name o))
    (standard-object (class-name (class-of o)))))

(defun easy-slot-value (obj slot)
  "like slot-value except it accepts slot-names or defs
   and returns nil when the slot is unbound"
  (let ((n (to-slot-name slot)))
    (when (and obj (slot-boundp obj n))
      (slot-value obj n))))

(defun (setf easy-slot-value) (new obj slot)
  "like slot-value except it accepts slot-names or defs"
  (setf (slot-value obj (to-slot-name slot)) new))

(defun delist-if-single (x)
  "if this is a single item in a list return it"
  (if (and (listp x) (= 1 (length x)))
      (first x)
      x))
