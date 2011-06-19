;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: odbc -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     odbc-ff-interface.lisp
;;;; Purpose:  Function definitions for UFFI interface to ODBC
;;;; Author:   Kevin M. Rosenberg
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Kevin M. Rosenberg
;;;; and Copyright (C) Paul Meurer 1999 - 2001. All rights reserved.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:odbc)

(def-foreign-type sql-handle :pointer-void)
(def-foreign-type sql-handle-ptr (* sql-handle))
(def-foreign-type string-ptr (* :unsigned-char))
(def-type long-ptr-type (* #.$ODBC-LONG-TYPE))

;; odbc v3
(def-function "SQLAllocHandle"
    ((handle-type :short)
     (input-handle sql-handle)
     (*phenv sql-handle-ptr))
  :module "odbc"
  :returning :short)

;; odbc v3
(def-function "SQLFreeHandle"
    ((handle-type :short)
     (input-handle sql-handle))
  :module "odbc"
  :returning :short)

;; odbc v3
(def-function "SQLCloseCursor"
    ((input-handle sql-handle))
  :module "odbc"
  :returning :short)

;; deprecated
(def-function "SQLAllocEnv"
    ((*phenv sql-handle-ptr)    ; HENV   FAR *phenv
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

;; deprecated
(def-function "SQLAllocConnect"
    ((henv sql-handle)          ; HENV        henv
     (*phdbc sql-handle-ptr)    ; HDBC   FAR *phdbc
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLConnect"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (*szDSN :cstring)        ; UCHAR  FAR *szDSN
     (cbDSN :short)             ; SWORD       cbDSN
     (*szUID :cstring)        ; UCHAR  FAR *szUID
     (cbUID :short)             ; SWORD       cbUID
     (*szAuthStr :cstring)    ; UCHAR  FAR *szAuthStr
     (cbAuthStr :short)         ; SWORD       cbAuthStr
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLDriverConnect"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (hwnd sql-handle)          ; SQLHWND     hwnd
     (*szConnStrIn :cstring)    ; UCHAR  FAR *szConnStrIn
     (cbConnStrIn :short)       ; SWORD       cbConnStrIn
     (*szConnStrOut string-ptr) ; UCHAR  FAR *szConnStrOut
     (cbConnStrOutMax :short)   ; SWORD       cbConnStrOutMax
     (*pcbConnStrOut :pointer-void)      ; SWORD  FAR *pcbConnStrOut
     (fDriverCompletion :short) ; UWORD       fDriverCompletion
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLDisconnect"
    ((hdbc sql-handle))         ; HDBC        hdbc
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

;; deprecated
(def-function "SQLAllocStmt"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (*phstmt sql-handle-ptr)   ; HSTMT  FAR *phstmt
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLGetInfo"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fInfoType :short)         ; UWORD       fInfoType
     (rgbInfoValue :pointer-void)        ; PTR         rgbInfoValue
     (cbInfoValueMax :short)    ; SWORD       cbInfoValueMax
     (*pcbInfoValue :pointer-void)       ; SWORD  FAR *pcbInfoValue
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLPrepare"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr :cstring)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :int)           ; SDWORD      cbSqlStr
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLExecute"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLExecDirect"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlStr :cstring)     ; UCHAR  FAR *szSqlStr
     (cbSqlStr :int)           ; SDWORD      cbSqlStr
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLFreeStmt"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (fOption :short))          ; UWORD       fOption
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

  (def-function "SQLCancel"
      ((hstmt sql-handle)         ; HSTMT       hstmt
       )
    :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLError"
    ((henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (hstmt sql-handle)         ; HSTMT       hstmt
     (*szSqlState string-ptr)   ; UCHAR  FAR *szSqlState
     (*pfNativeError (* :int))      ; SDWORD FAR *pfNativeError
     (*szErrorMsg string-ptr)   ; UCHAR  FAR *szErrorMsg
     (cbErrorMsgMax :short)     ; SWORD       cbErrorMsgMax
     (*pcbErrorMsg (* :short))        ; SWORD  FAR *pcbErrorMsg
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLNumResultCols"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*pccol (* :short))              ; SWORD  FAR *pccol
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLRowCount"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*pcrow (* :int))              ; SDWORD FAR *pcrow
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLDescribeCol"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (*szColName string-ptr)    ; UCHAR  FAR *szColName
     (cbColNameMax :short)      ; SWORD       cbColNameMax
     (*pcbColName (* :short))         ; SWORD  FAR *pcbColName
     (*pfSqlType (* :short))          ; SWORD  FAR *pfSqlType
     (*pcbColDef (* #.$ODBC-ULONG-TYPE))          ; UDWORD FAR *pcbColDef
     (*pibScale (* :short))           ; SWORD  FAR *pibScale
     (*pfNullable (* :short))         ; SWORD  FAR *pfNullable
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLColAttributes"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fDescType :short)         ; UWORD       fDescType
     (rgbDesc string-ptr)             ; PTR         rgbDesc
     (cbDescMax :short)         ; SWORD       cbDescMax
     (*pcbDesc (* :short))            ; SWORD  FAR *pcbDesc
     (*pfDesc (* :int))             ; SDWORD FAR *pfDesc
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLColumns"
    ((hstmt sql-handle)             ; HSTMT       hstmt
     (*szTableQualifier :cstring) ; UCHAR  FAR *szTableQualifier
     (cbTableQualifier :short)      ; SWORD       cbTableQualifier
     (*szTableOwner :cstring)     ; UCHAR  FAR *szTableOwner
     (cbTableOwner :short)          ; SWORD       cbTableOwner
     (*szTableName :cstring)      ; UCHAR  FAR *szTableName
     (cbTableName :short)           ; SWORD       cbTableName
     (*szColumnName :cstring)     ; UCHAR  FAR *szColumnName
     (cbColumnName :short)          ; SWORD       cbColumnName
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLBindCol"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValueMax :int)         ; SDWORD      cbValueMax
     (*pcbValue (* :int))           ; SDWORD FAR *pcbValue
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLFetch"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLTransact"
    ((henv sql-handle)          ; HENV        henv
     (hdbc sql-handle)          ; HDBC        hdbc
     (fType :short)             ; UWORD       fType ($SQL_COMMIT or $SQL_ROLLBACK)
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

;; ODBC 2.0
(def-function "SQLDescribeParam"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (*pfSqlType (* :short))          ; SWORD  FAR *pfSqlType
     (*pcbColDef (* :unsigned-int))          ; UDWORD FAR *pcbColDef
     (*pibScale (* :short))           ; SWORD  FAR *pibScale
     (*pfNullable (* :short))         ; SWORD  FAR *pfNullable
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

;; ODBC 2.0
(def-function "SQLBindParameter"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (ipar :short)              ; UWORD       ipar
     (fParamType :short)        ; SWORD       fParamType
     (fCType :short)            ; SWORD       fCType
     (fSqlType :short)          ; SWORD       fSqlType
     (cbColDef :int)           ; UDWORD      cbColDef
     (ibScale :short)           ; SWORD       ibScale
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValueMax :int)         ; SDWORD      cbValueMax
     (*pcbValue :pointer-void)           ; SDWORD FAR *pcbValue
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

;; level 1
(def-function "SQLGetData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (icol :short)              ; UWORD       icol
     (fCType :short)            ; SWORD       fCType
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValueMax :int)         ; SDWORD      cbValueMax
     (*pcbValue :pointer-void)           ; SDWORD FAR *pcbValue
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLParamData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (*prgbValue :pointer-void)          ; PTR    FAR *prgbValue
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLPutData"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (rgbValue :pointer-void)            ; PTR         rgbValue
     (cbValue :int)            ; SDWORD      cbValue
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLGetConnectOption"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (pvParam :pointer-void)             ; PTR         pvParam
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLSetConnectOption"
    ((hdbc sql-handle)          ; HDBC        hdbc
     (fOption :short)           ; UWORD       fOption
     (vParam :int)             ; UDWORD      vParam
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLSetPos"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (irow :short)              ; UWORD       irow
     (fOption :short)           ; UWORD       fOption
     (fLock :short)             ; UWORD       fLock
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

                                        ; level 2
(def-function "SQLExtendedFetch"
    ((hstmt sql-handle)         ; HSTMT       hstmt
     (fFetchType :short)        ; UWORD       fFetchType
     (irow :int)               ; SDWORD      irow
     (*pcrow :pointer-void)              ; UDWORD FAR *pcrow
     (*rgfRowStatus :pointer-void)       ; UWORD  FAR *rgfRowStatus
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLDataSources"
    ((henv sql-handle)          ; HENV        henv
     (fDirection :short)
     (*szDSN string-ptr)        ; UCHAR  FAR *szDSN
     (cbDSNMax :short)          ; SWORD       cbDSNMax
     (*pcbDSN (* :short))             ; SWORD      *pcbDSN
     (*szDescription string-ptr) ; UCHAR     *szDescription
     (cbDescriptionMax :short)  ; SWORD       cbDescriptionMax
     (*pcbDescription (* :short))     ; SWORD      *pcbDescription
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API

(def-function "SQLFreeEnv"
    ((henv sql-handle)          ; HSTMT       hstmt
     )
  :module "odbc"
  :returning :short)              ; RETCODE_SQL_API


;;; foreign type definitions

;;(defmacro %sql-len-data-at-exec (length)
;;  `(- $SQL_LEN_DATA_AT_EXEC_OFFSET ,length))


(def-struct sql-c-time
    (hour   :short)
  (minute :short)
  (second :short))

(def-struct sql-c-date
    (year  :short)
  (month :short)
  (day   :short))

(def-struct sql-c-timestamp
    (year     :short)
  (month    :short)
  (day      :short)
  (hour     :short)
  (minute   :short)
  (second   :short)
  (fraction :int))

;;; Added by KMR

(def-function "SQLSetEnvAttr"
    ((henv sql-handle)          ; HENV        henv
     (attr :int)
     (*value :pointer-void)
     (szLength :int))
  :module "odbc"
  :returning :short)

(def-function "SQLGetEnvAttr"
    ((henv sql-handle)          ; HENV        henv
     (attr :int)
     (*value :pointer-void)
     (szLength :int)
     (string-length-ptr (* :int)))
  :module "odbc"
  :returning :short)

(def-function "SQLTables"
    ((hstmt :pointer-void)
     (catalog-name :pointer-void)
     (catalog-name-length :short)
     (schema-name :pointer-void)
     (schema-name-length :short)
     (table-name :pointer-void)
     (table-name-length :short)
     (table-type-name :pointer-void)
     (table-type-name-length :short))
  :module "odbc"
  :returning :short)


(def-function "SQLStatistics"
    ((hstmt :pointer-void)
     (catalog-name :pointer-void)
     (catalog-name-length :short)
     (schema-name :pointer-void)
     (schema-name-length :short)
     (table-name :cstring)
     (table-name-length :short)
     (unique :short)
     (reserved :short))
  :module "odbc"
  :returning :short)


