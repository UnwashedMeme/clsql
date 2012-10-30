;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for SQL interface
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

;;;; This file makes the required package definitions for CLSQL's
;;;; core packages.

(eval-when (:compile-toplevel :load-toplevel :execute)
 #+sbcl
  (if (find-package 'sb-mop)
      (pushnew :clsql-sbcl-mop cl:*features*)
      (pushnew :clsql-sbcl-pcl cl:*features*))

  #+cmu
  (if (eq (symbol-package 'pcl:find-class)
          (find-package 'common-lisp))
      (pushnew :clsql-cmucl-mop cl:*features*)
      (pushnew :clsql-cmucl-pcl cl:*features*)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:clsql-sys
    (:use #:common-lisp
          #+clsql-sbcl-mop #:sb-mop
          #+clsql-cmucl-mop #:mop
          #+allegro #:mop
          #+clisp #:clos
          #+lispworks #:clos
          #+scl #:clos
          #+openmcl #:openmcl-mop)

    #+allegro
    (:shadowing-import-from
     #:excl)
    #+clisp
    (:shadowing-import-from
     #:clos)
    #+lispworks
    (:shadowing-import-from
     #:clos)
    #+clsql-sbcl-mop
    (:shadowing-import-from
     #:sb-pcl
     #:generic-function-lambda-list)
    #+clsql-sbcl-pcl
    (:shadowing-import-from
     #:sb-pcl
     #:name
     #:class-direct-slots
     #:class-of #:class-name #:class-slots #:find-class
     #:slot-boundp
     #:standard-class
     #:slot-definition-name #:finalize-inheritance
     #:standard-direct-slot-definition
     #:standard-effective-slot-definition #:validate-superclass
     #:direct-slot-definition-class #:compute-effective-slot-definition
     #:effective-slot-definition-class
     #:slot-value-using-class
     #:class-prototype #:generic-function-method-class #:intern-eql-specializer
     #:make-method-lambda #:generic-function-lambda-list
     #:class-precedence-list #:slot-definition-type
     #:class-direct-superclasses
     #:compute-class-precedence-list)
    #+clsql-cmucl-mop
    (:shadowing-import-from
     #:pcl
     #:generic-function-lambda-list)
    #+clsql-cmucl-pcl
    (:shadowing-import-from
     #:pcl
     #:class-direct-slots
     #:name
     #:class-of  #:class-name #:class-slots #:find-class #:standard-class
     #:slot-boundp
     #:slot-definition-name #:finalize-inheritance
     #:standard-direct-slot-definition #:standard-effective-slot-definition
     #:validate-superclass #:direct-slot-definition-class
     #:effective-slot-definition-class
     #:compute-effective-slot-definition
     #:slot-value-using-class
     #:class-prototype #:generic-function-method-class #:intern-eql-specializer
     #:make-method-lambda #:generic-function-lambda-list
     #:class-precedence-list #:slot-definition-type
     #:class-direct-superclasses
     #:compute-class-precedence-list)
    #+scl
    (:shadowing-import-from
     #:clos
     #:class-prototype  ;; note: make-method-lambda is not fbound
     )

    (:export
     ;; "Private" exports for use by interface packages
     #:check-connection-spec
     #:database-initialize-database-type
     #:database-type-load-foreign
     #:database-connect
     #:database-disconnect
     #:database-reconnect
     #:database-query
     #:database-execute-command
     #:database-create-sequence
     #:database-drop-sequence
     #:database-sequence-next
     #:database-set-sequence-position
     #:database-query-result-set
     #:database-dump-result-set
     #:database-store-next-row
     #:database-list-tables
     #:database-list-tables-and-sequences
     #:database-table-exists-p
     #:database-list-views
     #:database-view-exists-p
     #:database-list-indexes
     #:database-list-table-indexes
     #:database-index-exists-p
     #:database-list-sequences
     #:database-sequence-last
     #:database-sequence-exists-p
     #:database-last-auto-increment-id
     #:database-list-attributes
     #:database-attribute-type
     #:database-type-library-loaded
     #:database-create
     #:database-destroy
     #:database-probe
     #:database-list
     #:database-acquire-from-conn-pool
     #:database-release-to-conn-pool

     #:db-backend-has-create/destroy-db?
     #:db-type-has-views?
     #:db-type-has-bigint?
     #:db-type-has-union?
     #:db-type-has-prepared-stmt?
     #:db-type-has-subqueries?
     #:db-type-has-boolean-where?
     #:db-type-transaction-capable?
     #:db-type-has-fancy-math?
     #:db-type-default-case
     #:db-type-use-column-on-drop-index?
     #:db-type-use-fully-qualified-column-on-drop-index?
     #:db-type-has-intersect?
     #:db-type-has-except?
     #:db-type-has-auto-increment?
     #:database-underlying-type
     #:database-get-type-specifier
     #:read-sql-value
     #:database-output-sql-as-type
     #:*loaded-database-types*
     #:reload-database-types
     #:is-database-open
     #:*db-pool-max-free-connections*

     ;; Large objects
     #:database-create-large-object
     #:database-write-large-object
     #:database-read-large-object
     #:database-delete-large-object
     #:create-large-object
     #:write-large-object
     #:read-large-object
     #:delete-large-object

     ;; Prepared statments
     #:database-prepare
     #:database-run-prepared
     #:database-bind-parameter
     #:database-free-prepared

     ;; accessors for database class
     #:name
     #:connection-spec
     #:transaction
     #:transaction-level
     #:conn-pool
     #:command-recording-stream
     #:result-recording-stream
     #:record-caches
     #:view-classes
     #:database-state
     #:attribute-cache
     #:database-autocommit
     #:encoding

     ;; utils.lisp
     #:without-interrupts
     #:make-process-lock
     #:with-process-lock
     #:command-output
     #:symbol-name-default-case
     #:convert-to-db-default-case
     #:ensure-keyword
     #:getenv
     #:number-to-sql-string
     #:float-to-sql-string
     #:sql-escape-quotes
     #:sql-escape
     #:in

     ;; Command-object.lisp
     #:expression
     #:parameters
     #:prepare-sql-parameter
     #:prepared-name
     #:has-been-prepared
     #:command-object
     #:reset-command-object

     ;; Generic backends
     #:generic-postgresql-database
     #:generic-odbc-database

         ;; Subclasses of sql-expresssion (expressions.lisp)
         #:sql-function-exp
         #:sql-value-exp
         #:sql-set-exp
         #:sql-query-modifier-exp
         #:sql-relational-exp
         #:sql-upcase-like
         #:sql-assignment-exp
         #:sql-typecast-exp
         #:sql-between-exp
         #:sql-ident
         #:sql-ident-attribute
         #:sql-ident-table

     .

     ;; Shared exports for re-export by CLSQL package.
     #1=(

         ;; foreign library loading
         #:*foreign-library-search-paths*
         #:push-library-path

         ;; Condition system (conditions.lisp)
         #:sql-user-error
         #:sql-database-error
         #:sql-database-data-error
         #:sql-connection-error
         #:sql-temporary-error
         #:sql-timeout-error
         #:sql-fatal-error
         #:sql-error-error-id
         #:sql-error-secondary-error-id
         #:sql-error-database-message
         ;; CLSQL Extensions
         #:sql-condition
         #:sql-error
         #:sql-warning
         #:sql-database-warning
         #:sql-error-database
         #:sql-error-database-type
         #:sql-error-connection-spec
         #:sql-error-expression
         #:sql-warning-database
         #:sql-user-error-message
         #:*backend-warning-behavior*

         ;; Connection/initialisation (base-classes.lisp, database.lisp,
         ;;   initialize.lisp)
         #:*default-database-type*
         #:*default-database*
         #:*initialized-database-types*
         #:initialize-database-type
         #:connect
         #:disconnect
         #:*connect-if-exists*
         #:connected-databases
         #:database
         #:database-name
         #:reconnect
         #:find-database
         #:status
         ;; CLSQL Extensions
         #:database-name-from-spec
         #:database-type
         #:with-database
         #:with-default-database
         #:disconnect-pooled
         #:list-databases
         #:create-database
         #:destroy-database
         #:probe-database

         ;; I/O Recording (recording.lisp)
         #:add-sql-stream
         #:delete-sql-stream
         #:list-sql-streams
         #:sql-recording-p
         #:sql-stream
         #:start-sql-recording
         #:stop-sql-recording

         ;; FDDL (fddl.lisp)
         #:create-table
         #:drop-table
         #:list-tables
         #:table-exists-p
         #:list-attributes
         #:attribute-type
         #:list-attribute-types
         #:create-view
         #:drop-view
         #:create-index
         #:drop-index
         ;; CLSQL Extensions
         #:truncate-database
         #:list-views
         #:view-exists-p
         #:list-indexes
         #:index-exists-p
         #:create-sequence
         #:drop-sequence
         #:list-sequences
         #:sequence-exists-p
         #:sequence-next
         #:sequence-last
         #:set-sequence-position
         #:*old-sequence-names*

         ;; FDML (fdml.lisp)
         #:select
         #:cache-table-queries
         #:*cache-table-queries-default*
         #:delete-records
         #:insert-records
         #:update-records
         #:execute-command
         #:query
         #:print-query
         #:do-query
         #:map-query
         #:loop
         ;; CLSQL Extensions
         #:prepare-sql
         #:bind-parameter
         #:run-prepared-sql
         #:free-prepared-sql

         ;; Transaction handling (transaction.lisp)
         #:with-transaction
         #:commit
         #:rollback
         ;; CLSQL Extensions
         #:add-transaction-commit-hook
         #:add-transaction-rollback-hook
         #:start-transaction
         #:in-transaction-p
         #:set-autocommit

         ;; OODDL (ooddl.lisp)
         #:standard-db-object
         #:def-view-class
         #:create-view-from-class
         #:drop-view-from-class
         #:list-classes
         #:universal-time
         ;; CLSQL Extensions
         #:view-table
         #:bigint
         #:varchar
         #:generalized-boolean
         #:mediumint
         #:smallint
         #:tinyint
         #:*default-string-length*

         ;; OODML (oodml.lisp)
         #:instance-refreshed
         #:update-objects-joins
         #:*default-update-objects-max-len*
         #:*default-caching*
         #:update-slot-from-record
         #:update-instance-from-records
         #:update-records-from-instance
         #:update-record-from-slot
         #:update-record-from-slots
         #:delete-instance-records
         ;; CLSQL Extensions
         #:*db-auto-sync*
         #:write-instance-to-stream
         #:read-instance-from-stream

         ;; Symbolic SQL Syntax (syntax.lisp)
         #:sql
         #:sql-expression
         #:sql-operation
         #:sql-operator
         #:disable-sql-reader-syntax
         #:enable-sql-reader-syntax
         #:locally-disable-sql-reader-syntax
         #:locally-enable-sql-reader-syntax
         #:restore-sql-reader-syntax-state
	 #:file-enable-sql-reader-syntax

         ;; SQL operations (operations.lisp)
         #:sql-query
         #:sql-object-query
         #:sql-any
         #:sql-some
         #:sql-all
         #:sql-not
         #:sql-union
         #:sql-intersect
         #:sql-minus
         #:sql-except
         #:sql-order-by
         #:sql-null
         #:sql-*
         #:sql-+
         #:sql-/
         #:sql--
         #:sql-like
         #:sql-and
         #:sql-or
         #:sql-in
         #:sql-substr
         #:sql-concat-op
         #:sql-=
         #:sql-<
         #:sql->
         #:sql->=
         #:sql-<=
         #:sql-<>
         #:sql-count
         #:sql-max
         #:sql-min
         #:sql-avg
         #:sql-sum
         #:sql-function
         #:sql-between
         #:sql-distinct
         #:sql-nvl
         #:sql-slot-value
         #:sql-userenv
         ;; CLSQL Extensions
         #:sql-concat
         #:sql-substring
         #:sql-limit
         #:sql-group-by
         #:sql-having
         #:sql-not-null
         #:sql-exists
         #:sql-uplike
         #:sql-is
         #:sql-==
         #:sql-the
         #:sql-coalesce
         #:sql-view-class

         ;; Time handling (time.lisp)
         #:bad-component
         #:current-day
         #:current-month
         #:current-year
         #:day-duration
         #:db-timestring
         #:db-datestring
         #:decode-duration
         #:decode-time
         #:decode-date
         #:duration
         #:duration+
         #:duration<
         #:duration<=
         #:duration=
         #:duration>
         #:duration>=
         #:duration-day
         #:duration-hour
         #:duration-minute
         #:duration-month
         #:duration-second
         #:duration-year
         #:duration-reduce
         #:duration-timestring
         #:extract-roman
         #:format-duration
         #:format-time
         #:format-date
         #:get-time
         #:get-date
         #:utime->time
         #:interval-clear
         #:interval-contained
         #:interval-data
         #:interval-edit
         #:interval-end
         #:interval-match
         #:interval-push
         #:interval-relation
         #:interval-start
         #:interval-type
         #:make-duration
         #:make-interval
         #:make-time
         #:make-date
         #:merged-time
         #:midnight
         #:month-name
         #:parse-date-time
         #:parse-timestring
         #:parse-datestring
         #:parse-yearstring
         #:print-date
         #:roll
         #:roll-to
         #:time
         #:time+
         #:time-
         #:time-by-adding-duration
         #:time-compare
         #:time-difference
         #:time-dow
         #:time-element
         #:time-max
         #:time-min
         #:time-mjd
         #:time-msec
         #:time-p
         #:time-sec
         #:time-well-formed
         #:time-ymd
         #:time<
         #:time<=
         #:time=
         #:time>
         #:time>=
         #:date
         #:date+
         #:date-
         #:date-difference
         #:date-compare
         #:date-dow
         #:date-element
         #:date-max
         #:date-min
         #:date-mjd
         #:date-p
         #:date-ymd
         #:date<
         #:date<=
         #:date=
         #:date>
         #:date>=
         #:timezone
         #:universal-time
         #:wall-time
         #:wall-timestring
         #:week-containing
         #:gregorian-to-mjd
         #:mjd-to-gregorian
         #:iso-timestring
         ))
    (:documentation "This is the INTERNAL SQL-Interface package of CLSQL."))


(defpackage #:clsql
  (:use #:common-lisp)
  (:import-from #:clsql-sys . #1#)
  (:export . #1#)
  (:documentation "This is the user package with CLSQL symbols."))

(defpackage #:clsql-user
  (:use #:common-lisp)
  (:import-from #:clsql-sys . #1#)
  (:export . #1#)
  (:documentation "This is the user package with CLSQL symbols."))

  ;; This is from USQL's pcl-patch
  #+(or clsql-sbcl-pcl clsql-cmucl-pcl)
  (progn
    ;; Note that this will no longer required for cmucl as of version 19a.
    (in-package #+cmu :pcl #+sbcl :sb-pcl)
    (defmacro pv-binding1 ((pv calls pv-table-symbol pv-parameters slot-vars)
                           &body body)
      `(pv-env (,pv ,calls ,pv-table-symbol ,pv-parameters)
        (let (,@(mapcar #'(lambda (slot-var p) `(,slot-var (get-slots-or-nil ,p)))
                        slot-vars pv-parameters))
          ,@(mapcar #'(lambda (slot-var) `(declare (ignorable ,slot-var))) slot-vars)
          ,@body))))

;; see http://thread.gmane.org/gmane.lisp.lispworks.general/681
#+lispworks
(setf *packages-for-warn-on-redefinition*
      (delete "SQL" *packages-for-warn-on-redefinition* :test 'string=))

  #+sbcl
  (if (find-package 'sb-mop)
      (setq cl:*features* (delete :clsql-sbcl-mop cl:*features*))
      (setq cl:*features* (delete :clsql-sbcl-pcl cl:*features*)))

  #+cmu
  (if (find-package 'mop)
      (setq cl:*features* (delete :clsql-cmucl-mop cl:*features*))
      (setq cl:*features* (delete :clsql-cmucl-pcl cl:*features*)))

) ;eval-when

