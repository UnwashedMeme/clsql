/****************************************************************************
 * FILE IDENTIFICATION
 *
 *   Name:          clsql-mysql.c
 *   Purpose:       Helper functions for mysql.cl to handle 64-bit parts of API
 *   Programmer:    Kevin M. Rosenberg
 *   Date Started:  Mar 2002
 *
 * $Id$
 *
 * This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
 *
 * CLSQL users are granted the rights to distribute and use this software
 * as governed by the terms of the Lisp Lesser GNU Public License
 * (http://opensource.franz.com/preamble.html), also known as the LLGPL.
 ***************************************************************************/

#if defined(WIN32)||defined(WIN64)
#include <windows.h>

BOOL WINAPI DllEntryPoint(HINSTANCE hinstdll, DWORD fdwReason,
                          LPVOID lpvReserved)
{
        return 1;
}

#define DLLEXPORT __declspec(dllexport)

#else
#define DLLEXPORT
#endif


#include <mysql.h>

/* Need to assemble a 64-bit integer to send to MySQL */
DLLEXPORT
void
clsql_mysql_data_seek (MYSQL_RES* res, unsigned int offset_high32,
                       unsigned int offset_low32)
{
  my_ulonglong offset;

  offset = offset_high32;
  offset = offset << 32;
  offset += offset_low32;

  mysql_data_seek (res, offset);
}

/* The following functions are used to return 64-bit integers to Lisp.
   They return the 32-bit low part and store in upper 32-bits in a
   located sent via a pointer */

static const unsigned int bitmask_32bits = 0xFFFFFFFF;
#define lower_32bits(int64) ((unsigned int) int64 & bitmask_32bits)
#define upper_32bits(int64) ((unsigned int) (int64 >> 32))

DLLEXPORT
unsigned int
clsql_mysql_num_rows (MYSQL_RES* res, unsigned int* pHigh32)
{
  my_ulonglong nRows = mysql_num_rows (res);
  *pHigh32 = upper_32bits(nRows);
  return lower_32bits(nRows);
}

DLLEXPORT
unsigned int
clsql_mysql_affected_rows (MYSQL* res, unsigned int* pHigh32)
{
  my_ulonglong nAffected = mysql_affected_rows (res);
  *pHigh32 = upper_32bits(nAffected);
  return lower_32bits(nAffected);
}

DLLEXPORT
unsigned int
clsql_mysql_insert_id (MYSQL* mysql, unsigned int* pHigh32)
{
  my_ulonglong insert_id = mysql_insert_id (mysql);
  *pHigh32 = upper_32bits(insert_id);
  return lower_32bits(insert_id);
}


/* Accessor functions to hide the differences across MySQL versions */

DLLEXPORT
unsigned int
clsql_mysql_field_flags (MYSQL_FIELD* field)
{
  return field->flags;
}


DLLEXPORT
unsigned int
clsql_mysql_field_type (MYSQL_FIELD* field)
{
  return field->type;
}

DLLEXPORT
char*
clsql_mysql_field_name (MYSQL_FIELD* field)
{
  return field->name;
}

DLLEXPORT
unsigned long
clsql_mysql_field_length (MYSQL_FIELD* field)
{
  return field->length;
}

DLLEXPORT
unsigned long
clsql_mysql_field_max_length (MYSQL_FIELD* field)
{
  return field->max_length;
}


#if MYSQL_VERSION_ID >= 40102
#include <stdlib.h>

DLLEXPORT
MYSQL_BIND*
allocate_bind (unsigned int n)
{
  return (MYSQL_BIND*) malloc (n * sizeof(MYSQL_BIND));
}

DLLEXPORT
void
bind_param (MYSQL_BIND bind[], unsigned int n, unsigned long length, unsigned short is_null,
           void* buffer, unsigned short buffer_type, unsigned long buffer_length)
{
  *bind[n].length = length;
  *bind[n].is_null = is_null;
  bind[n].buffer = buffer;
  bind[n].buffer_type = buffer_type;
  bind[n].buffer_length = buffer_length;
}


DLLEXPORT
unsigned int
clsql_mysql_stmt_affected_rows (MYSQL_STMT* stmt, unsigned int* pHigh32)
{
  my_ulonglong nAffected = mysql_stmt_affected_rows (stmt);
  *pHigh32 = upper_32bits(nAffected);
  return lower_32bits(nAffected);
}

#endif

