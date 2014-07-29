;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-package.cl
;;;; Purpose:       Package definition for low-level MySQL interface
;;;; Programmers:   Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:mysql
    (:use #:common-lisp #:clsql-uffi)
    (:export
     #:database-library-loaded

     #:mysql-socket
     #:mysql-book
     #:mysql-byte
     #:mysql-net-type
     #:mysql-net-type#tcp-ip
     #:mysql-net-type#socket
     #:mysql-net-type#named-pipe
     #:mysql-net
     #:mysql-used-mem
     #:mysql-mem-root
     #:mysql-field-types
     #:mysql-field-types#decimal
     #:mysql-field-types#tiny
     #:mysql-field-types#short
     #:mysql-field-types#long
     #:mysql-field-types#float
     #:mysql-field-types#double
     #:mysql-field-types#null
     #:mysql-field-types#timestamp
     #:mysql-field-types#longlong
     #:mysql-field-types#int24
     #:mysql-field-types#date
     #:mysql-field-types#time
     #:mysql-field-types#datetime
     #:mysql-field-types#year
     #:mysql-field-types#newdate
     #:mysql-field-types#enum
     #:mysql-field-types#tiny-blob
     #:mysql-field-types#medium-blob
     #:mysql-field-types#long-blob
     #:mysql-field-types#blob
     #:mysql-field-types#var-string
     #:mysql-field-types#string
     #:mysql-field
     #:mysql-row
     #:mysql-field-offset
     #:mysql-row-offset
     #:mysql-field-vector
     #:mysql-data
     #:mysql-options
     #:mysql-mysql-option
     #:mysql-mysql-option#connect-timeout
     #:mysql-mysql-option#compress
     #:mysql-mysql-option#named-pipe
     #:mysql-mysql-option#init-command
     #:mysql-mysql-option#read-default-file
     #:mysql-mysql-option#read-default-group
     #:mysql-status
     #:mysql-status#ready
     #:mysql-status#get-ready
     #:mysql-status#use-result
     #:mysql-mysql
     #:mysql-mysql-res

     ;; functions
     #:mysql-init
     #:mysql-connect
     #:mysql-real-connect
     #:mysql-close
     #:mysql-select-db
     #:mysql-query
     #:mysql-real-query
     #:mysql-create-db
     #:mysql-drop-db
     #:mysql-shutdown
     #:mysql-dump-debug-info
     #:mysql-refresh
     #:mysql-kill
     #:mysql-ping
     #:mysql-stat
     #:mysql-get-server-info
     #:mysql-get-client-info
     #:mysql-get-host-info
     #:mysql-get-proto-info
     #:mysql-list-dbs
     #:mysql-list-tables
     #:mysql-list-fields
     #:mysql-list-processes
     #:mysql-store-result
     #:mysql-use-result
     #:mysql-options
     #:mysql-free-result
     #:mysql-next-result
     #:mysql-row-seek
     #:mysql-field-seek
     #:mysql-fetch-row
     #:mysql-fetch-lengths
     #:mysql-fetch-field
     #:mysql-fetch-fields
     #:mysql-fetch-field-direct
     #:mysql-escape-string
     #:mysql-debug
     #:mysql-num-rows
     #:mysql-num-fields
     #:mysql-affected-rows
     #:mysql-insert-id
     #:mysql-eof
     #:mysql-error
     #:mysql-error-string
     #:mysql-errno
     #:mysql-info
     #:mysql-info-string
     #:mysql-data-seek

     #:mysql-time
     #:mysql-bind
     #:mysql-stmt-param-count
     #:mysql-stmt-prepare
     #:mysql-stmt-execute
     #:mysql-stmt-store-result
     #:mysql-stmt-init
     #:mysql-stmt-close-result
     #:mysql-stmt-free-result
     #:mysql-stmt
     #:mysql-stmt-result-metadata
     #:mysql-stmt-fetch
     #:mysql-stmt-bind-param
     #:mysql-stmt-bind-result
     #:mysql-stmt-close
     #:mysql-stmt-errno
     #:mysql-stmt-error

     #:make-64-bit-integer
     #:clsql-mysql-field-name
     #:clsql-mysql-field-type
     #:clsql-mysql-field-flags

     #:+mysql-option-parameter-map+
     )
    (:documentation "This is the low-level interface MySQL."))
