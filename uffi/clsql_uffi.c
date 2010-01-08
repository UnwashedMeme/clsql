/****************************************************************************
 * FILE IDENTIFICATION
 *
 *   Name:          clsql-uffi.c
 *   Purpose:       Helper functions for common interfaces using UFFI
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


const unsigned int bitmask_32bits = 0xFFFFFFFF;
#define lower_32bits(int64) ((unsigned int) int64 & bitmask_32bits)
#define upper_32bits(int64) ((unsigned int) (int64 >> 32))

/* Reads a 64-bit integer string, returns result as two 32-bit integers */

DLLEXPORT
unsigned int
atol64 (const unsigned char* str, unsigned int* pHigh32)
{
#if defined(WIN32)||defined(WIN64)
  __int64 result = 0;
#else
  long long result = 0;
#endif
  int minus = 0;
  int first_char = *str;
  if (first_char == '+')
    ++str;
  else if (first_char == '-') {
    minus = 1;
    ++str;
  }

  while (*str) {
    int i = *str - '0';
    if (i < 0 || i > 9) /* Non-numeric character -- quit */
      break;
    result = i + (10 * result);
    str++;
  }
  if (minus)
    result = -result;

  *pHigh32 = upper_32bits(result);
  return lower_32bits(result);
}





