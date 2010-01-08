;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-sql.lisp
;;;; Purpose:       High-level MySQL interface using UFFI
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002-2009 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-mysql
    (:use #:common-lisp #:clsql-sys #:mysql #:clsql-uffi)
    (:export #:mysql-database)
    (:documentation "This is the CLSQL interface to MySQL."))

(in-package #:clsql-mysql)

;; if we have :sb-unicode, UFFI will treat :cstring as a UTF-8 string
(defun expression-length (query-expression)
  (length #+sb-unicode (sb-ext:string-to-octets query-expression
                                                :external-format :utf8)
          #-sb-unicode query-expression))

;;; Field conversion functions

(defun result-field-names (res-ptr)
  (let ((names '()))
    (mysql-field-seek res-ptr 0)
    (loop
       (let ((field (mysql-fetch-field res-ptr)))
         (when (uffi:null-pointer-p field) (return))
         (push (uffi:convert-from-cstring (clsql-mysql-field-name field)) names)))
    (nreverse names)))

(defun make-type-list-for-auto (res-ptr)
  (let ((new-types '()))
    (mysql-field-seek res-ptr 0)
    (loop
       (let ((field (mysql-fetch-field res-ptr)))
         (when (uffi:null-pointer-p field) (return))
         (let* ((flags (clsql-mysql-field-flags field))
                (unsigned (plusp (logand flags 32)))
                (type (clsql-mysql-field-type field)))
           (push
            (case type
              ((#.mysql-field-types#tiny
                #.mysql-field-types#short
                #.mysql-field-types#int24)
               (if unsigned
                   :uint32
                   :int32))
              (#.mysql-field-types#long
               (if unsigned
                   :uint
                   :int))
              (#.mysql-field-types#longlong
               (if unsigned
                   :uint64
                   :int64))
              ((#.mysql-field-types#double
                #.mysql-field-types#float
                #.mysql-field-types#decimal)
               :double)
              (otherwise
               t))
            new-types))))
    (nreverse new-types)))

(defun canonicalize-types (types res-ptr)
  (when types
    (let ((auto-list (make-type-list-for-auto res-ptr)))
      (cond
        ((listp types)
         (canonicalize-type-list types auto-list))
        ((eq types :auto)
         auto-list)
        (t
         nil)))))

(defmethod database-initialize-database-type ((database-type (eql :mysql)))
  t)

;;(uffi:def-type mysql-mysql-ptr-def (* mysql-mysql))
;;(uffi:def-type mysql-mysql-res-ptr-def (* mysql-mysql-res))
(uffi:def-type mysql-mysql-ptr-def mysql-mysql)
(uffi:def-type mysql-mysql-res-ptr-def mysql-mysql-res)
(uffi:def-type mysql-row-def mysql-row)

(defclass mysql-database (database)
  ((mysql-ptr :accessor database-mysql-ptr :initarg :mysql-ptr
              :type mysql-mysql-ptr-def)
   (server-info :accessor database-server-info :initarg :server-info
              :type string)))

(defmethod database-type ((database mysql-database))
  :mysql)

(defmethod database-name-from-spec (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port))
  (destructuring-bind (host db user password &optional port) connection-spec
    (declare (ignore password))
    (concatenate 'string
                 (etypecase host
                   (null "localhost")
                   (pathname (namestring host))
                   (string host))
                 (if port
                     (concatenate 'string
                                  ":"
                                  (etypecase port
                                    (integer (write-to-string port))
                                    (string port)))
                     "")
                 "/" db "/" user)))

(defmethod database-connect (connection-spec (database-type (eql :mysql)))
  (check-connection-spec connection-spec database-type
                         (host db user password &optional port))
  (destructuring-bind (host db user password &optional port) connection-spec
    (let ((mysql-ptr (mysql-init (uffi:make-null-pointer 'mysql-mysql)))
          (socket nil))
      (if (uffi:null-pointer-p mysql-ptr)
          (error 'sql-connection-error
                 :database-type database-type
                 :connection-spec connection-spec
                 :error-id (mysql-errno mysql-ptr)
                 :message (mysql-error-string mysql-ptr))
        (uffi:with-cstrings ((host-native host)
                            (user-native user)
                            (password-native password)
                            (db-native db)
                            (socket-native socket))
          (let ((error-occurred nil))
            (unwind-protect
                (if (uffi:null-pointer-p
                     (mysql-real-connect
                      mysql-ptr host-native user-native password-native
                      db-native
                      (etypecase port
                        (null 0)
                        (integer port)
                        (string (parse-integer port)))
                      socket-native 0))
                    (progn
                      (setq error-occurred t)
                      (error 'sql-connection-error
                             :database-type database-type
                             :connection-spec connection-spec
                             :error-id (mysql-errno mysql-ptr)
                             :message (mysql-error-string mysql-ptr)))
                    (let* ((db
                            (make-instance 'mysql-database
                                           :name (database-name-from-spec connection-spec
                                                                          database-type)
                                           :database-type :mysql
                                           :connection-spec connection-spec
                                           :server-info (uffi:convert-from-cstring
                                                         (mysql:mysql-get-server-info mysql-ptr))
                                           :mysql-ptr mysql-ptr))
                           (cmd "SET SESSION sql_mode='ANSI'"))
                      (uffi:with-cstring (cmd-cs cmd)
                        (if (zerop (mysql-real-query mysql-ptr cmd-cs (expression-length cmd)))
                            db
                            (progn
                              (warn "Error setting ANSI mode for MySQL.")
                              db)))))
              (when error-occurred (mysql-close mysql-ptr)))))))))


(defmethod database-disconnect ((database mysql-database))
  (mysql-close (database-mysql-ptr database))
  (setf (database-mysql-ptr database) nil)
  t)


(defmethod database-query (query-expression (database mysql-database)
                           result-types field-names)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((mysql-ptr (database-mysql-ptr database)))
    (uffi:with-cstring (query-native query-expression)
      (if (zerop (mysql-real-query mysql-ptr query-native
                                   (expression-length query-expression)))
          (let ((res-ptr (mysql-use-result mysql-ptr)))
            (if res-ptr
                (unwind-protect
                     (let ((num-fields (mysql-num-fields res-ptr)))
                       (declare (fixnum num-fields))
                       (setq result-types (canonicalize-types
                                    result-types res-ptr))
                       (values
                        (loop for row = (mysql-fetch-row res-ptr)
                              for lengths = (mysql-fetch-lengths res-ptr)
                              until (uffi:null-pointer-p row)
                              collect
                              (do* ((rlist (make-list num-fields))
                                    (i 0 (1+ i))
                                    (pos rlist (cdr pos)))
                                   ((= i num-fields) rlist)
                                (declare (fixnum i))
                                (setf (car pos)
                                      (convert-raw-field
                                       (uffi:deref-array row '(:array
                                                               (* :unsigned-char))
                                                         i)
                                       result-types i
                                       (uffi:deref-array lengths '(:array :unsigned-long)
                                                         i)))))
                        (when field-names
                          (result-field-names res-ptr))))
                  (mysql-free-result res-ptr))
                (error 'sql-database-data-error
                       :database database
                       :expression query-expression
                       :error-id (mysql-errno mysql-ptr)
                       :message (mysql-error-string mysql-ptr))))
          (error 'sql-database-data-error
                 :database database
                 :expression query-expression
                 :error-id (mysql-errno mysql-ptr)
                 :message (mysql-error-string mysql-ptr))))))

(defmethod database-execute-command (sql-expression (database mysql-database))
  (uffi:with-cstring (sql-native sql-expression)
    (let ((mysql-ptr (database-mysql-ptr database)))
      (declare (type mysql-mysql-ptr-def mysql-ptr))
      (if (zerop (mysql-real-query mysql-ptr sql-native
                                   (expression-length sql-expression)))
          t
        (error 'sql-database-data-error
               :database database
               :expression sql-expression
               :error-id (mysql-errno mysql-ptr)
               :message (mysql-error-string mysql-ptr))))))


(defstruct mysql-result-set
  (res-ptr (uffi:make-null-pointer 'mysql-mysql-res) :type mysql-mysql-res-ptr-def)
  (types nil :type list)
  (num-fields 0 :type fixnum)
  (full-set nil :type boolean))


(defmethod database-query-result-set ((query-expression string)
                                      (database mysql-database)
                                      &key full-set result-types)
  (uffi:with-cstring (query-native query-expression)
    (let ((mysql-ptr (database-mysql-ptr database)))
     (declare (type mysql-mysql-ptr-def mysql-ptr))
      (if (zerop (mysql-real-query mysql-ptr query-native
                                   (expression-length query-expression)))
          (let ((res-ptr (if full-set
                             (mysql-store-result mysql-ptr)
                           (mysql-use-result mysql-ptr))))
            (declare (type mysql-mysql-res-ptr-def res-ptr))
            (if (not (uffi:null-pointer-p res-ptr))
                (let* ((num-fields (mysql-num-fields res-ptr))
                       (result-set (make-mysql-result-set
                                    :res-ptr res-ptr
                                    :num-fields num-fields
                                    :full-set full-set
                                    :types
                                    (canonicalize-types
                                     result-types res-ptr))))
                  (if full-set
                      (values result-set
                              num-fields
                              (mysql-num-rows res-ptr))
                      (values result-set
                              num-fields)))
                (error 'sql-database-data-error
                     :database database
                     :expression query-expression
                     :error-id (mysql-errno mysql-ptr)
                     :message (mysql-error-string mysql-ptr))))
        (error 'sql-database-data-error
               :database database
               :expression query-expression
               :error-id (mysql-errno mysql-ptr)
               :message (mysql-error-string mysql-ptr))))))

(defmethod database-dump-result-set (result-set (database mysql-database))
  (mysql-free-result (mysql-result-set-res-ptr result-set))
  t)


(defmethod database-store-next-row (result-set (database mysql-database) list)
  (let* ((res-ptr (mysql-result-set-res-ptr result-set))
         (row (mysql-fetch-row res-ptr))
         (lengths (mysql-fetch-lengths res-ptr))
         (types (mysql-result-set-types result-set)))
    (declare (type mysql-mysql-res-ptr-def res-ptr)
             (type mysql-row-def row))
    (unless (uffi:null-pointer-p row)
      (loop for i from 0 below (mysql-result-set-num-fields result-set)
            for rest on list
            do
            (setf (car rest)
                  (convert-raw-field
                   (uffi:deref-array row '(:array (* :unsigned-char)) i)
                   types
                   i
                   (uffi:deref-array lengths '(:array :unsigned-long) i))))
      list)))


;; Table and attribute introspection

(defmethod database-list-tables ((database mysql-database) &key (owner nil))
  (declare (ignore owner))
  (cond
   ((eql #\5 (char (database-server-info database) 0))
    (loop for (name type) in (database-query "SHOW FULL TABLES" database nil nil)
          when (and (string-equal type "base table")
                    (not (and (>= (length name) 11)
                              (string-equal (subseq name 0 11) "_CLSQL_SEQ_"))))
          collect name))
   (t
    (remove-if #'(lambda (s)
                   (and (>= (length s) 11)
                        (string-equal (subseq s 0 11) "_CLSQL_SEQ_")))
               (mapcar #'car (database-query "SHOW TABLES" database nil nil))))))

(defmethod database-list-views ((database mysql-database)
                                &key (owner nil))
  (declare (ignore owner))
  (cond
   ((eql #\5 (char (database-server-info database) 0))
    (loop for (name type) in (database-query "SHOW FULL TABLES" database nil nil)
          when (string-equal type "view")
          collect name))
     (t
      nil)))

(defmethod database-list-indexes ((database mysql-database)
                                  &key (owner nil))
  (let ((result '()))
    (dolist (table (database-list-tables database :owner owner) result)
      (setq result
        (append (database-list-table-indexes table database :owner owner)
                result)))))

(defmethod database-list-table-indexes (table (database mysql-database)
                                        &key (owner nil))
  (declare (ignore owner))
  (do ((results nil)
       (rows (database-query
              (format nil "SHOW INDEX FROM ~A" table)
              database nil nil)
             (cdr rows)))
      ((null rows) (nreverse results))
    (let ((col (nth 2 (car rows))))
      (unless (find col results :test #'string-equal)
        (push col results)))))

(defmethod database-list-attributes ((table string) (database mysql-database)
                                     &key (owner nil))
  (declare (ignore owner))
  (mapcar #'car
          (database-query
           (format nil "SHOW COLUMNS FROM ~A" table)
           database nil nil)))

(defmethod database-attribute-type (attribute (table string)
                                    (database mysql-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (let ((row (car (database-query
                   (format nil
                           "SHOW COLUMNS FROM ~A LIKE '~A'" table attribute)
                   database nil nil))))
    (let* ((raw-type (second row))
           (null (third row))
           (start-length (position #\( raw-type))
           (type (if start-length
                     (subseq raw-type 0 start-length)
                     raw-type))
           (length (when start-length
                     (parse-integer (subseq raw-type (1+ start-length))
                                    :junk-allowed t))))
      (when type
        (values (ensure-keyword type) length nil (if (string-equal null "YES") 1 0))))))

;;; Sequence functions

(defun %sequence-name-to-table (sequence-name)
  (concatenate 'string "_CLSQL_SEQ_" (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name)
  (and (>= (length table-name) 11)
       (string-equal (subseq table-name 0 11) "_CLSQL_SEQ_")
       (subseq table-name 11)))

(defmethod database-create-sequence (sequence-name
                                     (database mysql-database))
  (let ((table-name (%sequence-name-to-table sequence-name)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
                  " (id int NOT NULL PRIMARY KEY AUTO_INCREMENT)")
     database)
    (database-execute-command
     (concatenate 'string "INSERT INTO " table-name
                  " VALUES (-1)")
     database)))

(defmethod database-drop-sequence (sequence-name
                                   (database mysql-database))
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table sequence-name))
   database))

(defmethod database-list-sequences ((database mysql-database)
                                    &key (owner nil))
  (declare (ignore owner))
  (mapcan #'(lambda (s)
              (let ((sn (%table-name-to-sequence-name (car s))))
                (and sn (list sn))))
          (database-query "SHOW TABLES" database nil nil)))

(defmethod database-set-sequence-position (sequence-name
                                           (position integer)
                                           (database mysql-database))
  (database-execute-command
   (format nil "UPDATE ~A SET id=~A" (%sequence-name-to-table sequence-name)
           position)
   database)
  (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database)))

(defmethod database-sequence-next (sequence-name (database mysql-database))
  (without-interrupts
   (database-execute-command
    (concatenate 'string "UPDATE " (%sequence-name-to-table sequence-name)
                 " SET id=LAST_INSERT_ID(id+1)")
    database)
   (mysql:mysql-insert-id (clsql-mysql::database-mysql-ptr database))))

(defmethod database-sequence-last (sequence-name (database mysql-database))
  (without-interrupts
    (caar (database-query
           (concatenate 'string "SELECT id from "
                        (%sequence-name-to-table sequence-name))
           database :auto nil))))

(defmethod database-create (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
           (database-execute-command (format nil "create database ~A" name) database)
        (database-disconnect database)))))

(defmethod database-destroy (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password) connection-spec
    (let ((database (database-connect (list host "" user password)
                                      type)))
      (setf (slot-value database 'clsql-sys::state) :open)
      (unwind-protect
          (database-execute-command (format nil "drop database ~A" name) database)
        (database-disconnect database)))))

(defmethod database-probe (connection-spec (type (eql :mysql)))
  (when (find (second connection-spec) (database-list connection-spec type)
              :test #'string-equal)
    t))

(defmethod database-list (connection-spec (type (eql :mysql)))
  (destructuring-bind (host name user password &optional port) connection-spec
    (let ((database (database-connect (list host (or name "mysql")
                                            user password port) type)))
      (unwind-protect
           (progn
             (setf (slot-value database 'clsql-sys::state) :open)
             (mapcar #'car (database-query "show databases" database :auto nil)))
        (progn
          (database-disconnect database)
          (setf (slot-value database 'clsql-sys::state) :closed))))))


;;; Prepared statements

(defclass mysql-stmt ()
  ((database :initarg :database :reader database)
   (stmt :initarg :stmt :accessor stmt)
   (input-bind :initarg :input-bind :reader input-bind)
   (output-bind :initarg :output-bind :reader output-bind)
   (types :initarg :types :reader types)
   (result-set :initarg :result-set :reader result-set)
   (num-fields :initarg :num-fields :reader num-fields)
   (field-names :initarg :field-names :accessor stmt-field-names)
   (length-ptr :initarg :length-ptr :reader length-ptr)
   (is-null-ptr :initarg :is-null-ptr :reader is-null-ptr)
   (result-types :initarg :result-types :reader result-types)))

(defun clsql-type->mysql-type (type)
  (cond
    ((in type :null) mysql-field-types#null)
    ((in type :int :integer) mysql-field-types#long)
    ((in type :short) mysql-field-types#short)
    ((in type :bigint) mysql-field-types#longlong)
    ((in type :float :double :number) mysql-field-types#double)
    ((and (consp type) (in (car type) :char :string :varchar)) mysql-field-types#var-string)
    ((or (eq type :blob) (and (consp type) (in (car type) :blob))) mysql-field-types#var-string)
    (t
       (error 'sql-user-error
              :message
              (format nil "Unknown clsql type ~A." type)))))

#+mysql-client-v4.1
(defmethod database-prepare (sql-stmt types (database mysql-database) result-types field-names)
  (let* ((mysql-ptr (database-mysql-ptr database))
         (stmt (mysql-stmt-init mysql-ptr)))
    (when (uffi:null-pointer-p stmt)
      (error 'sql-database-error
             :error-id (mysql-errno mysql-ptr)
             :message (mysql-error-string mysql-ptr)))

    (uffi:with-cstring (native-query sql-stmt)
      (unless (zerop (mysql-stmt-prepare stmt native-query (expression-length sql-stmt)))
        (mysql-stmt-close stmt)
        (error 'sql-database-error
               :error-id (mysql-errno mysql-ptr)
               :message (mysql-error-string mysql-ptr))))

    (unless (= (mysql-stmt-param-count stmt) (length types))
      (mysql-stmt-close stmt)
      (error 'sql-database-error
             :message
             (format nil "Mysql param count (~D) does not match number of types (~D)"
                     (mysql-stmt-param-count stmt) (length types))))

    (let ((rs (mysql-stmt-result-metadata stmt)))
      (when (uffi:null-pointer-p rs)
        (warn "mysql_stmt_result_metadata returned NULL")
        #+nil
        (mysql-stmt-close stmt)
        #+nil
        (error 'sql-database-error
               :message "mysql_stmt_result_metadata returned NULL"))

      (let ((input-bind (uffi:allocate-foreign-object 'mysql-bind (length types)))
            (mysql-types (mapcar 'clsql-type->mysql-type types))
            field-vec num-fields is-null-ptr output-bind length-ptr)

        (print 'a)
        (dotimes (i (length types))
          (let* ((binding (uffi:deref-array input-bind '(:array mysql-bind) i)))
            (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer-type)
              (nth i mysql-types))
            (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer-length) 0)))

        (print 'b)
        (unless (uffi:null-pointer-p rs)
          (setq field-vec (mysql-fetch-fields rs)
                num-fields (mysql-num-fields rs)
                is-null-ptr (uffi:allocate-foreign-object :byte num-fields)
                output-bind (uffi:allocate-foreign-object 'mysql-bind num-fields)
                length-ptr (uffi:allocate-foreign-object :unsigned-long num-fields))
          (dotimes (i num-fields)
            (declare (fixnum i))
            (let* ((field (uffi:deref-array field-vec '(:array mysql-field) i))
                   (type (uffi:get-slot-value field 'mysql-field 'type))
                   (binding (uffi:deref-array output-bind '(:array mysql-bind) i)))
              (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer-type) type)

              (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer-length) 0)
              #+need-to-allocate-foreign-object-for-this
              (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::is-null)
                (+ i (uffi:pointer-address is-null-ptr)))
              #+need-to-allocate-foreign-object-for-this
              (setf (uffi:get-slot-value binding 'mysql-bind 'length)
                (+ (* i 8) (uffi:pointer-address length-ptr)))

              (case type
                ((#.mysql-field-types#var-string #.mysql-field-types#string
                  #.mysql-field-types#tiny-blob #.mysql-field-types#blob
                  #.mysql-field-types#medium-blob #.mysql-field-types#long-blob)
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer-length) 1024)
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   (uffi:allocate-foreign-object :unsigned-char 1024)))
                (#.mysql-field-types#tiny
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   (uffi:allocate-foreign-object :byte)))
                (#.mysql-field-types#short
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   (uffi:allocate-foreign-object :short)))
                (#.mysql-field-types#long
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   ;; segfaults if supply :int on amd64
                   (uffi:allocate-foreign-object :long)))
                #+64bit
                (#.mysql-field-types#longlong
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   (uffi:allocate-foreign-object :long)))
                (#.mysql-field-types#float
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   (uffi:allocate-foreign-object :float)))
                (#.mysql-field-types#double
                 (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer)
                   (uffi:allocate-foreign-object :double)))
                ((#.mysql-field-types#time #.mysql-field-types#date
                  #.mysql-field-types#datetime #.mysql-field-types#timestamp)
                 (uffi:allocate-foreign-object 'mysql-time))
                (t
                 (error "mysql type ~D not supported." type)))))

          (unless (zerop (mysql-stmt-bind-result stmt output-bind))
            (mysql-stmt-close stmt)
            (error 'sql-database-error
                   :error-id (mysql-stmt-errno stmt)
                   :message  (uffi:convert-from-cstring
                              (mysql-stmt-error stmt)))))

        (make-instance 'mysql-stmt
          :database database
          :stmt stmt
          :num-fields num-fields
          :input-bind input-bind
          :output-bind output-bind
          :result-set rs
          :result-types result-types
          :length-ptr length-ptr
          :is-null-ptr is-null-ptr
          :types mysql-types
          :field-names field-names)))))

#+mysql-client-v4.1
(defmethod database-bind-parameter ((stmt mysql-stmt) position value)
  ;; FIXME: will need to allocate bind structure. This should probably be
  ;; done in C since the API is not mature and may change
  (let ((binding (uffi:deref-array (input-bind stmt) '(:array mysql-bind) (1- position)))
        (type (nth (1- position) (types stmt))))
    (setf (uffi:get-slot-value binding 'mysql-bind 'length) 0)
    (cond
     ((null value)
      (when (is-null-ptr stmt)
        (setf (uffi:deref-array (is-null-ptr stmt) '(:array :byte) (1- position)) 1)))
     (t
      (when (is-null-ptr stmt)
        (setf (uffi:deref-array (is-null-ptr stmt) '(:array :byte) (1- position)) 0))
      (case type
        (#.mysql-field-types#long
         (setf (uffi:get-slot-value binding 'mysql-bind 'mysql::buffer) value))
        (t
         (warn "Unknown input bind type ~D." type))
        )))))

#+mysql-client-v4.1
(defmethod database-run-prepared ((stmt mysql-stmt))
  (print 'a1)
  (when (input-bind stmt)
    (unless (zerop (mysql-stmt-bind-param (stmt stmt) (input-bind stmt)))
      (error 'sql-database-error
             :error-id (mysql-stmt-errno (stmt stmt))
             :message  (uffi:convert-from-cstring
                        (mysql-stmt-error (stmt stmt))))))
  (print 'a2)
  (unless (zerop (mysql-stmt-execute (stmt stmt)))
    (error 'sql-database-error
           :error-id (mysql-stmt-errno (stmt stmt))
           :message  (uffi:convert-from-cstring
                      (mysql-stmt-error (stmt stmt)))))
  (print 'a3)
  (unless (zerop (mysql-stmt-store-result (stmt stmt)))
    (error 'sql-database-error
           :error-id (mysql-stmt-errno (stmt stmt))
           :message  (uffi:convert-from-cstring
                      (mysql-stmt-error (stmt stmt)))))
  (database-fetch-prepared-rows stmt))

#+mysql-client-v4.1
(defun database-fetch-prepared-rows (stmt)
  (do ((rc (mysql-stmt-fetch (stmt stmt)) (mysql-stmt-fetch (stmt stmt)))
       (num-fields (num-fields stmt))
       (rows '()))
      ((not (zerop rc)) (nreverse rows))
    (push
     (loop for i from 0 below num-fields
           collect
           (let ((is-null
                  (not (zerop (uffi:ensure-char-integer
                               (uffi:deref-array (is-null-ptr stmt) '(:array :byte) i))))))
             (unless is-null
               (let* ((bind (uffi:deref-array (output-bind stmt) '(:array mysql-bind) i))
                      (type (uffi:get-slot-value bind 'mysql-bind 'mysql::buffer-type))
                      (buffer (uffi:get-slot-value bind 'mysql-bind 'mysql::buffer)))
                 (case type
                   ((#.mysql-field-types#var-string #.mysql-field-types#string
                     #.mysql-field-types#tiny-blob #.mysql-field-types#blob
                     #.mysql-field-types#medium-blob #.mysql-field-types#long-blob)
                    (uffi:convert-from-foreign-string buffer))
                    (#.mysql-field-types#tiny
                     (uffi:ensure-char-integer
                      (uffi:deref-pointer buffer :byte)))
                    (#.mysql-field-types#short
                     (uffi:deref-pointer buffer :short))
                    (#.mysql-field-types#long
                     (uffi:deref-pointer buffer :int))
                    #+64bit
                    (#.mysql-field-types#longlong
                     (uffi:deref-pointer buffer :long))
                    (#.mysql-field-types#float
                     (uffi:deref-pointer buffer :float))
                    (#.mysql-field-types#double
                     (uffi:deref-pointer buffer :double))
                   ((#.mysql-field-types#time #.mysql-field-types#date
                                              #.mysql-field-types#datetime #.mysql-field-types#timestamp)
                    (let ((year (uffi:get-slot-value buffer 'mysql-time 'mysql::year))
                          (month (uffi:get-slot-value buffer 'mysql-time 'mysql::month))
                          (day (uffi:get-slot-value buffer 'mysql-time 'mysql::day))
                          (hour (uffi:get-slot-value buffer 'mysql-time 'mysql::hour))
                          (minute (uffi:get-slot-value buffer 'mysql-time 'mysql::minute))
                  (second (uffi:get-slot-value buffer 'mysql-time 'mysql::second)))
                      (db-timestring
                       (make-time :year year :month month :day day :hour hour
                                  :minute minute :second second))))
                   (t
                    (list type)))))))
     rows)))




#+mysql-client-v4.1
(defmethod database-free-prepared ((stmt mysql-stmt))
  (with-slots (stmt) stmt
    (mysql-stmt-close stmt))
  )


;;; Database capabilities

(defmethod db-type-use-column-on-drop-index? ((db-type (eql :mysql)))
  t)

(defmethod db-type-has-views? ((db-type (eql :mysql)))
  #+mysql-client-v5 t
  #-mysql-client-v5 nil)

(defmethod db-type-has-subqueries? ((db-type (eql :mysql)))
  #+(or mysql-client-v4.1 mysql-client-v5) t
  #-(or mysql-client-v4.1 mysql-client-v5) nil)

(defmethod db-type-has-boolean-where? ((db-type (eql :mysql)))
  #+(or mysql-client-v4.1 mysql-client-v5) t
  #-(or mysql-client-v4.1 mysql-client-v5) nil)

(defmethod db-type-has-union? ((db-type (eql :mysql)))
  (not (eql (schar mysql::*mysql-client-info* 0) #\3)))

(defmethod db-type-transaction-capable? ((db-type (eql :mysql)) database)
  (let ((tuple (car (database-query "SHOW VARIABLES LIKE 'HAVE_INNODB'" database :auto nil))))
    (and tuple (string-equal "YES" (second tuple)))))

(defmethod db-type-has-prepared-stmt? ((db-type (eql :mysql)))
  #+(or mysql-client-v4.1 mysql-client-v5) t
  #-(or mysql-client-v4.1 mysql-client-v5) nil)

(when (clsql-sys:database-type-library-loaded :mysql)
  (clsql-sys:initialize-database-type :database-type :mysql))
