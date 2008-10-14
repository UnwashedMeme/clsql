;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     postgresql-socket-api.lisp
;;;; Purpose:  Low-level PostgreSQL interface using sockets
;;;; Authors:  Kevin M. Rosenberg based on original code by Pierre R. Mai
;;;; Created:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2004 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:postgresql-socket)

(defparameter +postgres-external-format+ :utf8)

(uffi:def-enum pgsql-ftype
    ((:bytea 17)
     (:int2 21)
     (:int4 23)
     (:int8 20)
     (:float4 700)
     (:float8 701)))

(defmethod clsql-sys:database-type-library-loaded ((database-type
                                          (eql :postgresql-socket)))
  "T if foreign library was able to be loaded successfully. Always true for
socket interface"
  t)

(defmethod clsql-sys:database-type-load-foreign ((database-type (eql :postgresql-socket)))
  t)


;;; Message I/O stuff

(defmacro define-message-constants (description &rest clauses)
  (assert (evenp (length clauses)))
  (loop with seen-characters = nil
        for (name char) on clauses by #'cddr
        for char-code = (char-code char)
        for doc-string = (format nil "~A (~:C): ~A" description char name)
        if (member char seen-characters)
        do (error "Duplicate message type ~@C for group ~A" char description)
        else
        collect
        `(defconstant ,name ,char-code ,doc-string)
        into result-clauses
        and do (push char seen-characters)
      finally
        (return `(progn ,@result-clauses))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(define-message-constants "Backend Message Constants"
  +ascii-row-message+ #\D
  +authentication-message+ #\R
  +backend-key-message+ #\K
  +binary-row-message+ #\B
  +completed-response-message+ #\C
  +copy-in-response-message+ #\G
  +copy-out-response-message+ #\H
  +cursor-response-message+ #\P
  +empty-query-response-message+ #\I
  +error-response-message+ #\E
  +function-response-message+ #\V
  +notice-response-message+ #\N
  +notification-response-message+ #\A
  +ready-for-query-message+ #\Z
  +row-description-message+ #\T))

#+scl
(declaim (inline read-byte write-byte))

(defun send-socket-value-int32 (socket value)
  (declare (type stream socket)
           (type (unsigned-byte 32) value))
  (write-byte (ldb (byte 8 24) value) socket)
  (write-byte (ldb (byte 8 16) value) socket)
  (write-byte (ldb (byte 8 8) value) socket)
  (write-byte (ldb (byte 8 0) value) socket)
  nil)

(defun send-socket-value-int16 (socket value)
  (declare (type stream socket)
           (type (unsigned-byte 16) value))
  (write-byte (ldb (byte 8 8) value) socket)
  (write-byte (ldb (byte 8 0) value) socket)
  nil)

(defun send-socket-value-int8 (socket value)
  (declare (type stream socket)
           (type (unsigned-byte 8) value))
  (write-byte (ldb (byte 8 0) value) socket)
  nil)

(defun send-socket-value-char-code (socket value)
  (declare (type stream socket)
           (type character value))
  (write-byte (ldb (byte 8 0) (char-code value)) socket)
  nil)

(defun send-socket-value-string (socket value)
  (declare (type stream socket)
           (type string value))
  #-sb-unicode
  (loop for char across value
        for code = (char-code char)
        do (write-byte code socket)
        finally (write-byte 0 socket))
  #+sb-unicode
  (write-sequence (sb-ext:string-to-octets value
					   :external-format +postgres-external-format+
					   :null-terminate t)
		  socket)
  nil)

(defun send-socket-value-limstring (socket value limit)
  (declare (type stream socket)
           (type string value)
           (type fixnum limit))
  (let ((length (length value)))
    (dotimes (i (min length limit))
      (let ((code (char-code (char value i))))
        (write-byte code socket)))
    (dotimes (i (- limit length))
      (write-byte 0 socket)))
  nil)


(defun read-socket-value-int32 (socket)
  (declare (type stream socket))
  (declare (optimize (speed 3)))
  (let ((result 0))
    (declare (type (unsigned-byte 32) result))
    (setf (ldb (byte 8 24) result) (read-byte socket))
    (setf (ldb (byte 8 16) result) (read-byte socket))
    (setf (ldb (byte 8 8) result) (read-byte socket))
    (setf (ldb (byte 8 0) result) (read-byte socket))
    result))

(defun read-socket-value-int16 (socket)
  (declare (type stream socket))
  (let ((result 0))
    (declare (type (unsigned-byte 16) result))
    (setf (ldb (byte 8 8) result) (read-byte socket))
    (setf (ldb (byte 8 0) result) (read-byte socket))
    result))

(defun read-socket-value-int8 (socket)
  (declare (type stream socket))
  (read-byte socket))

(defun read-socket-value-string (socket)
  (declare (type stream socket))
  #-sb-unicode
  (with-output-to-string (out)
    (loop for code = (read-byte socket)
          until (zerop code)
          do (write-char (code-char code) out)))
  #+sb-unicode
  (let ((bytes (make-array 64
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop for code = (read-byte socket)
          until (zerop code)
          do (vector-push-extend code bytes))
    (sb-ext:octets-to-string bytes
			     :external-format +postgres-external-format+)))


(defmacro define-message-sender (name (&rest args) &rest clauses)
  (let ((socket-var (gensym))
        (body nil))
    (dolist (clause clauses)
      (let* ((type (first clause))
             (fn (intern (concatenate 'string (symbol-name '#:send-socket-value-)
                                      (symbol-name type)))))
        (push `(,fn ,socket-var ,@(rest clause)) body)))
    `(defun ,name (,socket-var ,@args)
       ,@(nreverse body))))

(define-message-sender send-startup-message
    (database user &optional (command-line "") (backend-tty ""))
  (int32 296)                           ; Length
  (int32 #x00020000)                    ; Version 2.0
  (limstring database 64)
  (limstring user 32)
  (limstring command-line 64)
  (limstring "" 64)     ; Unused
  (limstring backend-tty 64))

(define-message-sender send-terminate-message ()
  (char-code #\X))

(define-message-sender send-unencrypted-password-message (password)
  (int32 (+ 5 (length password)))
  (string password))

(define-message-sender send-query-message (query)
  (char-code #\Q)
  (string query))

(define-message-sender send-encrypted-password-message (crypted-password)
  (int32 (+ 5 (length crypted-password)))
  (string crypted-password))

(define-message-sender send-cancel-request (pid key)
  (int32 16)                            ; Length
  (int32 80877102)                      ; Magic
  (int32 pid)
  (int32 key))


(defun read-socket-sequence (stream length &optional (allow-wide t))
  (declare (stream stream)
           (optimize (speed 3) (safety 0)))
  #-sb-unicode
  (let ((result (make-string length)))
    (dotimes (i length result)
      (declare (fixnum i))
      (setf (char result i) (code-char (read-byte stream)))))
  #+sb-unicode
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
    (read-sequence bytes stream)
    (if allow-wide
        (sb-ext:octets-to-string bytes :external-format +postgres-external-format+)
        (map 'string #'code-char bytes))))

;;; Support for encrypted password transmission

#-scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *crypt-library-loaded* nil)

  (unless *crypt-library-loaded*
    (uffi:load-foreign-library
     (uffi:find-foreign-library "libcrypt"
                           '(#+(or 64bit x86-64) "/usr/lib64/"
                             "/usr/lib/" "/usr/local/lib/" "/lib/"))
     :supporting-libraries '("c"))
    (setq *crypt-library-loaded* t)))

(in-package :postgresql-socket)

(uffi:def-function ("crypt" crypt)
    ((key :cstring)
     (salt :cstring))
  :returning :cstring)

(defun crypt-password (password salt)
  "Encrypt a password for transmission to a PostgreSQL server."
  (uffi:with-cstring (password-cstring password)
    (uffi:with-cstring (salt-cstring salt)
      (uffi:convert-from-cstring
       (crypt password-cstring salt-cstring)))))


;;;; Condition hierarchy

(define-condition postgresql-condition (condition)
  ((connection :initarg :connection :reader postgresql-condition-connection)
   (message :initarg :message :reader postgresql-condition-message))
  (:report
   (lambda (c stream)
     (format stream "~@<~A occurred on connection ~A. ~:@_Reason: ~A~:@>"
             (type-of c)
             (postgresql-condition-connection c)
             (postgresql-condition-message c)))))

(define-condition postgresql-error (error postgresql-condition)
  ())

(define-condition postgresql-fatal-error (postgresql-error)
  ())

(define-condition postgresql-login-error (postgresql-fatal-error)
  ())

(define-condition postgresql-warning (warning postgresql-condition)
  ())

(define-condition postgresql-notification (postgresql-condition)
  ()
  (:report
   (lambda (c stream)
     (format stream "~@<Asynchronous notification on connection ~A: ~:@_~A~:@>"
             (postgresql-condition-connection c)
             (postgresql-condition-message c)))))

;;; Structures

(defstruct postgresql-connection
  host
  port
  database
  user
  password
  options
  tty
  socket
  pid
  key)

(defstruct postgresql-cursor
  connection
  name
  fields)

;;; Socket stuff

(defconstant +postgresql-server-default-port+ 5432
  "Default port of PostgreSQL server.")

(defvar *postgresql-server-socket-timeout* 60
  "Timeout in seconds for reads from the PostgreSQL server.")

#+(or cmu scl)
(defun open-postgresql-socket (host port)
  (etypecase host
    (pathname
     ;; Directory to unix-domain socket
     (ext:connect-to-unix-socket
      (namestring
       (make-pathname :name ".s.PGSQL" :type (princ-to-string port)
                      :defaults host))))
    (string
     (ext:connect-to-inet-socket host port))))

#+sbcl
(defun open-postgresql-socket (host port)
  (etypecase host
    (pathname
     ;; Directory to unix-domain socket
     (let ((sock (make-instance 'sb-bsd-sockets:local-socket
                                :type :stream)))
       (sb-bsd-sockets:socket-connect
        sock
        (namestring
         (make-pathname :name ".s.PGSQL" :type (princ-to-string port)
                        :defaults host)))
       sock))
    (string
     (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream
                                :protocol :tcp)))
       (sb-bsd-sockets:socket-connect
        sock
        (sb-bsd-sockets:host-ent-address
         (sb-bsd-sockets:get-host-by-name host))
        port)
       sock))))

#+(or cmu scl)
(defun open-postgresql-socket-stream (host port)
  (system:make-fd-stream
   (open-postgresql-socket host port)
   :input t :output t :element-type '(unsigned-byte 8)
   :buffering :none
   :timeout *postgresql-server-socket-timeout*))


#+sbcl
(defun open-postgresql-socket-stream (host port)
  (sb-bsd-sockets:socket-make-stream
   (open-postgresql-socket host port) :input t :output t
   :element-type '(unsigned-byte 8)))


#+allegro
(defun open-postgresql-socket-stream (host port)
  (etypecase host
    (pathname
     (let ((path (namestring
                  (make-pathname :name ".s.PGSQL" :type (princ-to-string port)
                                 :defaults host))))
       (socket:make-socket :type :stream :address-family :file
                           :connect :active
                           :remote-filename path :local-filename path)))
    (string
     (socket:with-pending-connect
         (mp:with-timeout (*postgresql-server-socket-timeout* (error "connect failed"))
           (socket:make-socket :type :stream :address-family :internet
                               :remote-port port :remote-host host
                               :connect :active :nodelay t))))))

#+openmcl
(defun open-postgresql-socket-stream (host port)
  (etypecase host
    (pathname
     (let ((path (namestring
                  (make-pathname :name ".s.PGSQL" :type (princ-to-string port)
                                 :defaults host))))
       (ccl:make-socket :type :stream :address-family :file
                        :connect :active
                        :remote-filename path :local-filename path)))
    (string
     (ccl:make-socket :type :stream :address-family :internet
                      :remote-port port :remote-host host
                      :connect :active :nodelay t))))

#+lispworks
(defun open-postgresql-socket-stream (host port)
  (etypecase host
    (pathname
     (error "File sockets not supported on Lispworks."))
    (string
     (comm:open-tcp-stream host port :direction :io :element-type '(unsigned-byte 8)
                           :read-timeout *postgresql-server-socket-timeout*))
    ))


#+clisp
(defun open-postgresql-socket-stream (host port)
  (etypecase host
    (pathname
     (error "Not supported"))
    (string
     (socket:socket-connect
      port host
      :element-type '(unsigned-byte 8)
      :timeout *postgresql-server-socket-timeout*))))


;;; Interface Functions

(defun open-postgresql-connection (&key (host (cmucl-compat:required-argument))
                                        (port +postgresql-server-default-port+)
                                        (database (cmucl-compat:required-argument))
                                        (user (cmucl-compat:required-argument))
                                        options tty password)
  "Open a connection to a PostgreSQL server with the given parameters.
Note that host, database and user arguments must be supplied.

If host is a pathname, it is assumed to name a directory containing
the local unix-domain sockets of the server, with port selecting which
of those sockets to open.  If host is a string, it is assumed to be
the name of the host running the PostgreSQL server.  In that case a
TCP connection to the given port on that host is opened in order to
communicate with the server.  In either case the port argument
defaults to `+postgresql-server-default-port+'.

Password is the clear-text password to be passed in the authentication
phase to the server.  Depending on the server set-up, it is either
passed in the clear, or encrypted via crypt and a server-supplied
salt.  In that case the alien function specified by `*crypt-library*'
and `*crypt-function-name*' is used for encryption.

Note that all the arguments (including the clear-text password
argument) are stored in the `postgresql-connection' structure, in
order to facilitate automatic reconnection in case of communication
troubles."
  (reopen-postgresql-connection
   (make-postgresql-connection :host host :port port
                               :options (or options "") :tty (or tty "")
                               :database database :user user
                               :password (or password ""))))

(defun encrypt-md5 (plaintext salt)
  (string-downcase
   (format nil "~{~2,'0X~}"
           (coerce (md5sum-sequence (concatenate 'string plaintext salt)) 'list))))

(defun reopen-postgresql-connection (connection)
  "Reopen the given PostgreSQL connection.  Closes any existing
connection, if it is still open."
  (when (postgresql-connection-open-p connection)
    (close-postgresql-connection connection))
  (let ((socket (open-postgresql-socket-stream
                  (postgresql-connection-host connection)
                  (postgresql-connection-port connection))))
    (unwind-protect
         (progn
           (setf (postgresql-connection-socket connection) socket)
           (send-startup-message socket
                                 (postgresql-connection-database connection)
                                 (postgresql-connection-user connection)
                                 (postgresql-connection-options connection)
                                 (postgresql-connection-tty connection))
           (force-output socket)
           (loop
               (case (read-socket-value-int8 socket)
                 (#.+authentication-message+
                  (case (read-socket-value-int32 socket)
                    (0 (return))
                    ((1 2)
                     (error 'postgresql-login-error
                            :connection connection
                            :message
                            "Postmaster expects unsupported Kerberos authentication."))
                    (3
                     (send-unencrypted-password-message
                      socket
                      (postgresql-connection-password connection))
                      (force-output socket))
                    (4
                     (let ((salt (read-socket-sequence socket 2 nil)))
                       (send-encrypted-password-message
                        socket
                        (crypt-password
                         (postgresql-connection-password connection) salt)))
                     (force-output socket))
                    (5
                     (let ((salt (read-socket-sequence socket 4 nil)))
                       (let* ((pwd2 (encrypt-md5 (postgresql-connection-password connection)
                                                 (postgresql-connection-user connection)))
                              (pwd (encrypt-md5 pwd2 salt)))
                         (send-encrypted-password-message
                          socket
                          (concatenate 'string "md5" pwd))))
                     (force-output socket))
                    (t
                     (error 'postgresql-login-error
                            :connection connection
                            :message
                            "Postmaster expects unknown authentication method."))))
                 (#.+error-response-message+
                  (let ((message (read-socket-value-string socket)))
                    (error 'postgresql-login-error
                           :connection connection :message message)))
                 (t
                  (error 'postgresql-login-error
                         :connection connection
                         :message
                         "Received garbled message from Postmaster"))))
           ;; Start backend communication
           (force-output socket)
           (loop
               (case (read-socket-value-int8 socket)
                 (#.+backend-key-message+
                  (setf (postgresql-connection-pid connection)
                        (read-socket-value-int32 socket)
                        (postgresql-connection-key connection)
                        (read-socket-value-int32 socket)))
                 (#.+ready-for-query-message+
                  (setq socket nil)
                  (return connection))
                 (#.+error-response-message+
                  (let ((message (read-socket-value-string socket)))
                    (error 'postgresql-login-error
                           :connection connection
                           :message message)))
                 (#.+notice-response-message+
                  (let ((message (read-socket-value-string socket)))
                    (warn 'postgresql-warning :connection connection
                          :message message)))
                 (t
                  (error 'postgresql-login-error
                         :connection connection
                         :message
                         "Received garbled message from Postmaster")))))
      (when socket
        (close socket)))))

(defun close-postgresql-connection (connection &optional abort)
  (unless abort
    (ignore-errors
      (send-terminate-message (postgresql-connection-socket connection))))
  (close (postgresql-connection-socket connection)))

(defun postgresql-connection-open-p (connection)
  (let ((socket (postgresql-connection-socket connection)))
    (and socket (streamp socket) (open-stream-p socket))))

(defun ensure-open-postgresql-connection (connection)
  (unless (postgresql-connection-open-p connection)
    (reopen-postgresql-connection connection)))

(defun process-async-messages (connection)
  (assert (postgresql-connection-open-p connection))
  ;; Process any asnychronous messages
  (loop with socket = (postgresql-connection-socket connection)
        while (listen socket)
        do
        (case (read-socket-value-int8 socket)
          (#.+ready-for-query-message+)
          (#.+notice-response-message+
           (let ((message (read-socket-value-string socket)))
             (warn 'postgresql-warning :connection connection
                   :message message)))
          (#.+notification-response-message+
           (let ((pid (read-socket-value-int32 socket))
                 (message (read-socket-value-string socket)))
             (when (= pid (postgresql-connection-pid connection))
               (signal 'postgresql-notification :connection connection
                       :message message))))
          (t
           (close-postgresql-connection connection)
           (error 'postgresql-fatal-error :connection connection
                  :message "Received garbled message from backend")))))

(defun start-query-execution (connection query)
  (ensure-open-postgresql-connection connection)
  (process-async-messages connection)
  (send-query-message (postgresql-connection-socket connection) query)
  (force-output (postgresql-connection-socket connection)))

(defun wait-for-query-results (connection)
  (assert (postgresql-connection-open-p connection))
  (let ((socket (postgresql-connection-socket connection))
        (cursor-name nil)
        (error nil))
    (loop
        (case (read-socket-value-int8 socket)
          (#.+completed-response-message+
           (return (values :completed (read-socket-value-string socket))))
          (#.+cursor-response-message+
           (setq cursor-name (read-socket-value-string socket)))
          (#.+row-description-message+
           (let* ((count (read-socket-value-int16 socket))
                  (fields
                   (loop repeat count
                     collect
                     (list
                      (read-socket-value-string socket)
                      (read-socket-value-int32 socket)
                      (read-socket-value-int16 socket)
                      (read-socket-value-int32 socket)))))
             (return
               (values :cursor
                       (make-postgresql-cursor :connection connection
                                               :name cursor-name
                                               :fields fields)))))
          (#.+copy-in-response-message+
           (return :copy-in))
          (#.+copy-out-response-message+
           (return :copy-out))
          (#.+ready-for-query-message+
           (when error
             (error error))
           (return nil))
          (#.+error-response-message+
           (let ((message (read-socket-value-string socket)))
             (setq error
                   (make-condition 'postgresql-error
                                   :connection connection :message message))))
          (#.+notice-response-message+
           (let ((message (read-socket-value-string socket)))
             (unless (eq :ignore clsql-sys:*backend-warning-behavior*)
               (warn 'postgresql-warning
                     :connection connection :message message))))
          (#.+notification-response-message+
           (let ((pid (read-socket-value-int32 socket))
                 (message (read-socket-value-string socket)))
             (when (= pid (postgresql-connection-pid connection))
               (signal 'postgresql-notification :connection connection
                       :message message))))
          (t
           (close-postgresql-connection connection)
           (error 'postgresql-fatal-error :connection connection
                  :message "Received garbled message from backend"))))))

(defun read-null-bit-vector (socket count)
  (let ((result (make-array count :element-type 'bit)))
    (dotimes (offset (ceiling count 8))
      (loop with byte = (read-byte socket)
            for index from (* offset 8) below (min count (* (1+ offset) 8))
            for weight downfrom 7
            do (setf (aref result index) (ldb (byte 1 weight) byte))))
    result))


(defun read-field (socket type)
  (let ((length (- (read-socket-value-int32 socket) 4)))
    (case type
      ((:int32 :int64)
       (read-integer-from-socket socket length))
      (:double
       (read-double-from-socket socket length))
      (t
       (read-socket-sequence socket length)))))

(uffi:def-constant +char-code-zero+ (char-code #\0))
(uffi:def-constant +char-code-minus+ (char-code #\-))
(uffi:def-constant +char-code-plus+ (char-code #\+))
(uffi:def-constant +char-code-period+ (char-code #\.))
(uffi:def-constant +char-code-lower-e+ (char-code #\e))
(uffi:def-constant +char-code-upper-e+ (char-code #\E))

(defun read-integer-from-socket (socket length)
  (declare (fixnum length))
  (if (zerop length)
      nil
    (let ((val 0)
          (first-char (read-byte socket))
          (minusp nil))
      (declare (fixnum first-char))
      (decf length) ;; read first char
      (cond
       ((= first-char +char-code-minus+)
        (setq minusp t))
       ((= first-char +char-code-plus+)
        )               ;; nothing to do
       (t
        (setq val (- first-char +char-code-zero+))))

      (dotimes (i length)
        (declare (fixnum i))
        (setq val (+
                   (* 10 val)
                   (- (read-byte socket) +char-code-zero+))))
      (if minusp
          (- val)
        val))))

(defmacro ascii-digit (int)
  (let ((offset (gensym)))
    `(let ((,offset (- ,int +char-code-zero+)))
      (declare (fixnum ,int ,offset))
      (if (and (>= ,offset 0)
               (< ,offset 10))
          ,offset
          nil))))

(defun read-double-from-socket (socket length)
  (declare (fixnum length))
  (let ((before-decimal 0)
        (after-decimal 0)
        (decimal-count 0)
        (exponent 0)
        (decimalp nil)
        (minusp nil)
        (result nil)
        (char (read-byte socket)))
    (declare (fixnum char exponent decimal-count))
    (decf length) ;; already read first character
    (cond
      ((= char +char-code-minus+)
       (setq minusp t))
      ((= char +char-code-plus+)
       )
      ((= char +char-code-period+)
       (setq decimalp t))
      (t
       (setq before-decimal (ascii-digit char))
       (unless before-decimal
         (error "Unexpected value"))))

    (block loop
      (dotimes (i length)
        (setq char (read-byte socket))
        ;;      (format t "~&len:~D, i:~D, char:~D, minusp:~A, decimalp:~A" length i char minusp decimalp)
        (let ((weight (ascii-digit char)))
          (cond
           ((and weight (not decimalp)) ;; before decimal point
            (setq before-decimal (+ weight (* 10 before-decimal))))
           ((and weight decimalp) ;; after decimal point
            (setq after-decimal (+ weight (* 10 after-decimal)))
            (incf decimal-count))
           ((and (= char +char-code-period+))
            (setq decimalp t))
           ((or (= char +char-code-lower-e+)          ;; E is for exponent
                (= char +char-code-upper-e+))
            (setq exponent (read-integer-from-socket socket (- length i 1)))
            (setq exponent (or exponent 0))
            (return-from loop))
          (t
           (break "Unexpected value"))
          )
        )))
    (setq result (* (+ (coerce before-decimal 'double-float)
                       (* after-decimal
                          (expt 10 (- decimal-count))))
                    (expt 10 exponent)))
    (if minusp
        (- result)
        result)))


#+ignore
(defun read-double-from-socket (socket length)
  (let ((result (make-string length)))
    (read-socket-sequence result socket)
    (let ((*read-default-float-format* 'double-float))
      (read-from-string result))))

(defun read-cursor-row (cursor types)
  (let* ((connection (postgresql-cursor-connection cursor))
         (socket (postgresql-connection-socket connection))
         (fields (postgresql-cursor-fields cursor)))
    (assert (postgresql-connection-open-p connection))
    (loop
        (let ((code (read-socket-value-int8 socket)))
          (case code
            (#.+ascii-row-message+
             (return
               (loop with count = (length fields)
                     with null-vector = (read-null-bit-vector socket count)
                     repeat count
                     for null-bit across null-vector
                     for i from 0
                     for null-p = (zerop null-bit)
                     if null-p
                     collect nil
                     else
                     collect
                     (read-field socket (nth i types)))))
            (#.+binary-row-message+
             (error "NYI"))
            (#.+completed-response-message+
             (return (values nil (read-socket-value-string socket))))
            (#.+error-response-message+
             (let ((message (read-socket-value-string socket)))
               (error 'postgresql-error
                      :connection connection :message message)))
            (#.+notice-response-message+
             (let ((message (read-socket-value-string socket)))
               (warn 'postgresql-warning
                     :connection connection :message message)))
            (#.+notification-response-message+
             (let ((pid (read-socket-value-int32 socket))
                   (message (read-socket-value-string socket)))
               (when (= pid (postgresql-connection-pid connection))
                 (signal 'postgresql-notification :connection connection
                         :message message))))
            (t
             (close-postgresql-connection connection)
             (error 'postgresql-fatal-error :connection connection
                    :message "Received garbled message from backend")))))))

(defun map-into-indexed (result-seq func seq)
  (dotimes (i (length seq))
    (declare (fixnum i))
    (setf (elt result-seq i)
          (funcall func (elt seq i) i)))
  result-seq)

(defun copy-cursor-row (cursor sequence types)
  (let* ((connection (postgresql-cursor-connection cursor))
         (socket (postgresql-connection-socket connection))
         (fields (postgresql-cursor-fields cursor)))
    (assert (= (length fields) (length sequence)))
    (loop
        (let ((code (read-socket-value-int8 socket)))
          (case code
            (#.+ascii-row-message+
             (return
               #+ignore
               (let* ((count (length sequence))
                      (null-vector (read-null-bit-vector socket count)))
                 (dotimes (i count)
                   (declare (fixnum i))
                   (if (zerop (elt null-vector i))
                       (setf (elt sequence i) nil)
                       (let ((value (read-field socket (nth i types))))
                         (setf (elt sequence i) value)))))
               (map-into-indexed
                sequence
                #'(lambda (null-bit i)
                    (if (zerop null-bit)
                        nil
                        (read-field socket (nth i types))))
                (read-null-bit-vector socket (length sequence)))))
            (#.+binary-row-message+
             (error "NYI"))
            (#.+completed-response-message+
             (return (values nil (read-socket-value-string socket))))
            (#.+error-response-message+
             (let ((message (read-socket-value-string socket)))
               (error 'postgresql-error
                      :connection connection :message message)))
            (#.+notice-response-message+
             (let ((message (read-socket-value-string socket)))
               (warn 'postgresql-warning
                     :connection connection :message message)))
            (#.+notification-response-message+
             (let ((pid (read-socket-value-int32 socket))
                   (message (read-socket-value-string socket)))
               (when (= pid (postgresql-connection-pid connection))
                 (signal 'postgresql-notification :connection connection
                         :message message))))
            (t
             (close-postgresql-connection connection)
             (error 'postgresql-fatal-error :connection connection
                    :message "Received garbled message from backend")))))))

(defun skip-cursor-row (cursor)
  (let* ((connection (postgresql-cursor-connection cursor))
         (socket (postgresql-connection-socket connection))
         (fields (postgresql-cursor-fields cursor)))
    (loop
        (let ((code (read-socket-value-int8 socket)))
          (case code
            (#.+ascii-row-message+
             (loop for null-bit across
                   (read-null-bit-vector socket (length fields))
                   do
                   (unless (zerop null-bit)
                     (let* ((length (read-socket-value-int32 socket)))
                       (loop repeat (- length 4) do (read-byte socket)))))
             (return t))
            (#.+binary-row-message+
             (error "NYI"))
            (#.+completed-response-message+
             (return (values nil (read-socket-value-string socket))))
            (#.+error-response-message+
             (let ((message (read-socket-value-string socket)))
               (error 'postgresql-error
                      :connection connection :message message)))
            (#.+notice-response-message+
             (let ((message (read-socket-value-string socket)))
               (warn 'postgresql-warning
                     :connection connection :message message)))
            (#.+notification-response-message+
             (let ((pid (read-socket-value-int32 socket))
                   (message (read-socket-value-string socket)))
               (when (= pid (postgresql-connection-pid connection))
                 (signal 'postgresql-notification :connection connection
                         :message message))))
            (t
             (close-postgresql-connection connection)
             (error 'postgresql-fatal-error :connection connection
                    :message "Received garbled message from backend")))))))

(defun run-query (connection query &optional (result-types nil))
  (start-query-execution connection query)
  (multiple-value-bind (status cursor)
      (wait-for-query-results connection)
    (assert (eq status :cursor))
    (loop for row = (read-cursor-row cursor result-types)
          while row
          collect row
          finally
          (wait-for-query-results connection))))

#+scl
(declaim (ext:maybe-inline read-byte write-byte))
