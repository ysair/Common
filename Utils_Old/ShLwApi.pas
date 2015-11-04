{******************************************************************}
{                                                       	   }
{       Borland Delphi Runtime Library                  	   }
{       Shell Ligtweight Utilities interface unit                  }
{ 								   }
{ Portions created by Microsoft are 				   }
{ Copyright (C) 1995-1999 Microsoft Corporation. 		   }
{ All Rights Reserved. 						   }
{ 								   }
{ The original file is: shlwapi.h, released 2 March 1999. 	   }
{ The original Pascal code is: ShLwApi.pas, released 1 Dec 1999.   }
{ The initial developer of the Pascal code is Marcel van Brakel    }
{ (brakelm@chello.nl).                      			   }
{ 								   }
{ Portions created by Marcel van Brakel are			   }
{ Copyright (C) 1999 Marcel van Brakel.				   }
{ 								   }
{ Obtained through:                               	           }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{								   }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{								   }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html 	                   }
{                                                                  }
{ Software distributed under the License is distributed on an 	   }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License. 			   }
{ 								   }
{******************************************************************}

unit ShLwApi;

{$WEAKPACKAGEUNIT}

interface

{$HPPEMIT ''}
{$HPPEMIT '#include <ShLwApi.h>'}
{$HPPEMIT ''}

uses
  Windows, ActiveX, ShlObj;

{$INCLUDE SHLWAPI.INC}

// Users of this header may define any number of these constants to avoid
// the definitions of each functional group.
//
//    NO_SHLWAPI_STRFCNS    String functions
//    NO_SHLWAPI_PATH       Path functions
//    NO_SHLWAPI_REG        Registry functions
//    NO_SHLWAPI_STREAM     Stream functions
//    NO_SHLWAPI_GDI        GDI helper functions

type
  TREFIID = TGUID;

{$IFNDEF NO_SHLWAPI_STRFCNS}
// String Routines

function StrChrA(lpStart: PAnsiChar; wMatch: WORD): PAnsiChar; stdcall;
{$EXTERNALSYM StrChrA}
function StrChrW(lpStart: PWideChar; wMatch: WORD): PWideChar; stdcall;
{$EXTERNALSYM StrChrW}
function StrChr(lpStart: PChar; wMatch: WORD): PChar; stdcall;
{$EXTERNALSYM StrChr}
function StrChrIA(lpStart: PAnsiChar; wMatch: WORD): PAnsiChar; stdcall;
{$EXTERNALSYM StrChrIA}
function StrChrIW(lpStart: PWideChar; wMatch: WORD): PWideChar; stdcall;
{$EXTERNALSYM StrChrIW}
function StrChrI(lpStart: PChar; wMatch: WORD): PChar; stdcall;
{$EXTERNALSYM StrChrI}
function StrCmpNA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNA}
function StrCmpNW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNW}
function StrCmpN(lpStr1, lpStr2: PChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpN}
function StrCmpNIA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNA}
function StrCmpNIW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpNW}
function StrCmpNI(lpStr1, lpStr2: PChar; nChar: Integer): Integer; stdcall;
{$EXTERNALSYM StrCmpN}
function StrCSpnA(lpStr_, lpSet: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnA}
function StrCSpnW(lpStr_, lpSet: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnW}
function StrCSpn(lpStr_, lpSet: PChar): Integer; stdcall;
{$EXTERNALSYM StrCSpn}
function StrCSpnIA(lpStr1, lpSet: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnIA}
function StrCSpnIW(lpStr1, lpSet: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnIW}
function StrCSpnI(lpStr1, lpSet: PChar): Integer; stdcall;
{$EXTERNALSYM StrCSpnI}
function StrDupA(lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrDupA}
function StrDupW(lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrDupW}
function StrDup(lpSrch: PChar): PChar; stdcall;
{$EXTERNALSYM StrDup}
function StrFormatByteSizeA(dw: DWORD; szBuf: PAnsiChar; uiBufSize: UINT): PAnsiChar; stdcall;
{$EXTERNALSYM StrFormatByteSizeA}
function StrFormatByteSizeW(qdw: LONGLONG; szBuf: PWideChar; uiBufSize: UINT): PWideChar; stdcall;
{$EXTERNALSYM StrFormatByteSizeW}
function StrFormatByteSize(dw: DWORD; szBuf: PChar; uiBufSize: UINT): PChar; stdcall;
{$EXTERNALSYM StrFormatByteSize}

{$IFDEF _WIN32_IE_5}

function StrFormatKBSizeA(qdw: LONGLONG; szBuf: PAnsiChar; uiBufSize: UINT): PAnsiChar; stdcall;
{$EXTERNALSYM StrFormatKBSizeA}
function StrFormatKBSizeW(qdw: LONGLONG; szBuf: PWideChar; uiBufSize: UINT): PWideChar; stdcall;
{$EXTERNALSYM StrFormatKBSizeW}
function StrFormatKBSize(qdw: LONGLONG; szBuf: PChar; uiBufSize: UINT): PChar; stdcall;
{$EXTERNALSYM StrFormatKBSize}

{$ENDIF}

function StrFromTimeIntervalA(pszOut: PAnsiChar; cchMax: UINT; dwTimeMS: DWORD;
  digits: Integer): Integer; stdcall;
{$EXTERNALSYM StrFromTimeIntervalA}
function StrFromTimeIntervalW(pszOut: PWideChar; cchMax: UINT; dwTimeMS: DWORD;
  digits: Integer): Integer; stdcall;
{$EXTERNALSYM StrFromTimeIntervalW}
function StrFromTimeInterval(pszOut: PChar; cchMax: UINT; dwTimeMS: DWORD;
  digits: Integer): Integer; stdcall;
{$EXTERNALSYM StrFromTimeInterval}
function StrIsIntlEqualA(fCaseSens: BOOL; lpString1, lpString2: PAnsiChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM StrIsIntlEqualA}
function StrIsIntlEqualW(fCaseSens: BOOL; lpString1, lpString2: PWideChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM StrIsIntlEqualW}
function StrIsIntlEqual(fCaseSens: BOOL; lpString1, lpString2: PChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM StrIsIntlEqual}
function StrNCatA(psz1: PAnsiChar; psz2: PAnsiChar; cchMax: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM StrNCatA}
function StrNCatW(psz1: PWideChar; psz2: PWideChar; cchMax: Integer): PWideChar; stdcall;
{$EXTERNALSYM StrNCatW}
function StrNCat(psz1: PChar; psz2: PChar; cchMax: Integer): PChar; stdcall;
{$EXTERNALSYM StrNCat}
function StrPBrkA(psz, pszSet: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrPBrkA}
function StrPBrkW(psz, pszSet: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrPBrkW}
function StrPBrk(psz, pszSet: PChar): PChar; stdcall;
{$EXTERNALSYM StrPBrk}
function StrRChrA(lpStart, lpEnd: PAnsiChar; wMatch: WORD): PAnsiChar; stdcall;
{$EXTERNALSYM StrRChrA}
function StrRChrW(lpStart, lpEnd: PWideChar; wMatch: WORD): PWideChar; stdcall;
{$EXTERNALSYM StrRChrW}
function StrRChr(lpStart, lpEnd: PChar; wMatch: WORD): PChar; stdcall;
{$EXTERNALSYM StrRChr}
function StrRChrIA(lpStart, lpEnd: PAnsiChar; wMatch: WORD): PAnsiChar; stdcall;
{$EXTERNALSYM StrRChrIA}
function StrRChrIW(lpStart, lpEnd: PWideChar; wMatch: WORD): PWideChar; stdcall;
{$EXTERNALSYM StrRChrIW}
function StrRChrI(lpStart, lpEnd: PChar; wMatch: WORD): PChar; stdcall;
{$EXTERNALSYM StrRChrI}
function StrRStrIA(lpSource, lpLast, lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrRStrIA}
function StrRStrIW(lpSource, lpLast, lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrRStrIW}
function StrRStrI(lpSource, lpLast, lpSrch: PChar): PChar; stdcall;
{$EXTERNALSYM StrRStrI}
function StrSpnA(psz, pszSet: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrSpnA}
function StrSpnW(psz, pszSet: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrSpnW}
function StrSpn(psz, pszSet: PChar): Integer; stdcall;
{$EXTERNALSYM StrSpn}
function StrStrA(lpFirst, lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrStrA}
function StrStrW(lpFirst, lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrStrW}
function StrStr(lpFirst, lpSrch: PChar): PChar; stdcall;
{$EXTERNALSYM StrStr}
function StrStrIA(lpFirst, lpSrch: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM StrStrIA}
function StrStrIW(lpFirst, lpSrch: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrStrIW}
function StrStrI(lpFirst, lpSrch: PChar): PChar; stdcall;
{$EXTERNALSYM StrStrI}
function StrToIntA(lpSrch: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM StrToIntA}
function StrToIntW(lpSrch: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrToIntW}
function StrToInt(lpSrch: PChar): Integer; stdcall;
{$EXTERNALSYM StrToInt}
function StrToIntExA(pszString: PAnsiChar; dwFlags: DWORD; var piRet: Integer): BOOL; stdcall;
{$EXTERNALSYM StrToIntExA}
function StrToIntExW(pszString: PWideChar; dwFlags: DWORD; var piRet: Integer): BOOL; stdcall;
{$EXTERNALSYM StrToIntExW}
function StrToIntEx(pszString: PChar; dwFlags: DWORD; var piRet: Integer): BOOL; stdcall;
{$EXTERNALSYM StrToIntEx}
function StrTrimA(psz: PAnsiChar; pszTrimChars: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM StrTrimA}
function StrTrimW(psz: PWideChar; pszTrimChars: PWideChar): BOOL; stdcall;
{$EXTERNALSYM StrTrimW}
function StrTrim(psz: PChar; pszTrimChars: PChar): BOOL; stdcall;
{$EXTERNALSYM StrTrim}

function ChrCmpIA(w1, w2: WORD): BOOL; stdcall;
{$EXTERNALSYM ChrCmpIA}
function ChrCmpIW(w1, w2: WORD): BOOL; stdcall;
{$EXTERNALSYM ChrCmpIW}
function ChrCmpI(w1, w2: WORD): BOOL; stdcall;
{$EXTERNALSYM ChrCmpI}

function StrIntlEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNA}
function StrIntlEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNW}
function StrIntlEqN(s1, s2: PChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqN}

function StrIntlEqNIA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNIA}
function StrIntlEqNIW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNIW}
function StrIntlEqNI(s1, s2: PChar; nChar: Integer): BOOL;
{$EXTERNALSYM StrIntlEqNI}

{$IFDEF _WIN32_IE_5}

function StrCatBuffA(pszDest, pszSrc: PAnsiChar; cchDestBuffSize: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM StrCatBuffA}
function StrCatBuffW(pszDest, pszSrc: PWideChar; cchDestBuffSize: Integer): PWideChar; stdcall;
{$EXTERNALSYM StrCatBuffW}
function StrCatBuff(pszDest, pszSrc: PChar; cchDestBuffSize: Integer): PChar; stdcall;
{$EXTERNALSYM StrCatBuff}

{$ENDIF}

function StrCpyNXA(psz1: PAnsiChar; psz2: PAnsiChar; cchMax: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM StrCpyNXA}
function StrCpyNXW(psz1: PWideChar; psz2: PWideChar; cchMax: Integer): PWideChar; stdcall;
{$EXTERNALSYM StrCpyNXW}
function StrCpyNX(psz1: PChar; psz2: PChar; cchMax: Integer): PChar; stdcall;
{$EXTERNALSYM StrCpyNX}

{$IFDEF _WIN32_IE_5}

(*
LWSTDAPI_(int)      wvnsprintfA(LPSTR lpOut, int cchLimitIn, LPCSTR lpFmt, va_list arglist);
LWSTDAPI_(int)      wvnsprintfW(LPWSTR lpOut, int cchLimitIn, LPCWSTR lpFmt, va_list arglist);
LWSTDAPIV_(int)     wnsprintfA(LPSTR lpOut, int cchLimitIn, LPCSTR lpFmt, ...);
LWSTDAPIV_(int)     wnsprintfW(LPWSTR lpOut, int cchLimitIn, LPCWSTR lpFmt, ...);
*)

function StrRetToStrA(PAnsiChar: PStrRet; const pidl: PItemIdList; out ppsz: PAnsiChar): HRESULT; stdcall;
{$EXTERNALSYM StrRetToStrA}
function StrRetToStrW(PWideChar: PStrRet; const pidl: PItemIdList; out ppsz: PWideChar): HRESULT; stdcall;
{$EXTERNALSYM StrRetToStrW}
function StrRetToStr(PChar: PStrRet; const pidl: PItemIdList; out ppsz: PChar): HRESULT; stdcall;
{$EXTERNALSYM StrRetToStr}

function StrRetToBufA(PAnsiChar: PStrRet; const pidl: PItemIdList; pszBuf: PAnsiChar; cchBuf: UINT): HRESULT; stdcall;
{$EXTERNALSYM StrRetToBufA}
function StrRetToBufW(PWideChar: PStrRet; const pidl: PItemIdList; pszBuf: PWideChar; cchBuf: UINT): HRESULT; stdcall;
{$EXTERNALSYM StrRetToBufW}
function StrRetToBuf(PChar: PStrRet; const pidl: PItemIdList; pszBuf: PChar; cchBuf: UINT): HRESULT; stdcall;
{$EXTERNALSYM StrRetToBuf}

// helper to duplicate a string using the task allocator

function SHStrDupA(psz: PAnsiChar; out ppwsz: PWideChar): HRESULT; stdcall;
{$EXTERNALSYM SHStrDupA}
function SHStrDupW(psz: PWideChar; out ppwsz: PWideChar): HRESULT; stdcall;
{$EXTERNALSYM SHStrDupW}
function SHStrDup(psz: PChar; out ppwsz: PWideChar): HRESULT; stdcall;
{$EXTERNALSYM SHStrDup}

// Backward compatible to NT's non-standard naming (strictly
// for comctl32)

function IntlStrEqWorkerA(fCaseSens: BOOL; lpString1, lpString2: PAnsiChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM IntlStrEqWorkerA}
function IntlStrEqWorkerW(fCaseSens: BOOL; lpString1, lpString2: PWideChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM IntlStrEqWorkerW}
function IntlStrEqWorker(fCaseSens: BOOL; lpString1, lpString2: PChar; nChar: Integer): BOOL; stdcall;
{$EXTERNALSYM IntlStrEqWorker}

function IntlStrEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqNA}
function IntlStrEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqNW}
function IntlStrEqN(s1, s2: PChar; nChar: Integer): BOOL;
{$EXTERNALSYM IntlStrEqN}

{$ENDIF}

const
  SZ_CONTENTTYPE_HTMLA = 'text/html';
  {$EXTERNALSYM SZ_CONTENTTYPE_HTMLA}
  SZ_CONTENTTYPE_HTMLW = 'text/html';
  {$EXTERNALSYM SZ_CONTENTTYPE_HTMLW}
  SZ_CONTENTTYPE_HTML = SZ_CONTENTTYPE_HTMLA;
  SZ_CONTENTTYPE_CDFA  = 'application/x-cdf';
  {$EXTERNALSYM SZ_CONTENTTYPE_CDFA}
  SZ_CONTENTTYPE_CDFW  = 'application/x-cdf';
  {$EXTERNALSYM SZ_CONTENTTYPE_CDFW}
  SZ_CONTENTTYPE_CDF = SZ_CONTENTTYPE_CDFA;

function PathIsHTMLFileA(pszPath: PAnsiChar): BOOL;
{$EXTERNALSYM PathIsHTMLFile}
function PathIsHTMLFileW(pszPath: PWideChar): BOOL;
{$EXTERNALSYM PathIsHTMLFile}
function PathIsHTMLFile(pszPath: PChar): BOOL;
{$EXTERNALSYM PathIsHTMLFile}

// Flags for StrToIntEx

const
  STIF_DEFAULT      = $00000000;
  {$EXTERNALSYM STIF_DEFAULT}
  STIF_SUPPORT_HEX  = $00000001;
  {$EXTERNALSYM STIF_SUPPORT_HEX}

function StrCatA(psz1, psz2: PAnsiChar): PAnsiChar;
{$EXTERNALSYM StrCatA}
function StrCatW(psz1, psz2: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrCatW}
{$IFDEF UNICODE}
function StrCat(psz1, psz2: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM StrCat}
{$ELSE}
function StrCat(psz1, psz2: PAnsiChar): PAnsiChar;
{$EXTERNALSYM StrCat}
{$ENDIF}

function StrCmpA(psz1, psz2: PAnsiChar): Integer;
{$EXTERNALSYM StrCmpA}
function StrCmpW(psz1, psz2: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCmpW}
{$IFDEF UNICODE}
function StrCmp(psz1, psz2: PWideChar): Integer; stdcall;
{$EXTERNALSYM StrCmp}
{$ELSE}
function StrCmp(psz1, psz2: PAnsiChar): Integer;
{$EXTERNALSYM StrCmp}
{$ENDIF}

function StrCmpIA(psz1, psz2: PAnsiChar): Integer;
{$EXTERNALSYM StrCmpIA}
function StrCmpIW(psz1, psz2: PWideChar): Integer;
{$EXTERNALSYM StrCmpIW}
{$IFDEF UNICODE}
function StrCmpI(psz1, psz2: PWideChar): Integer;
{$EXTERNALSYM StrCmpI}
{$ELSE}
function StrCmpI(psz1, psz2: PAnsiChar): Integer;
{$EXTERNALSYM StrCmpI}
{$ENDIF}

function StrCpyA(psz1, psz2: PAnsiChar): PAnsiChar;
{$EXTERNALSYM StrCpyA}
function StrCpyW(psz1, psz2: PWideChar): PWideChar;
{$EXTERNALSYM StrCpyW}
{$IFDEF UNICODE}
function StrCpy(psz1, psz2: PWideChar): PWideChar;
{$EXTERNALSYM StrCpy}
{$ELSE}
function StrCpy(psz1, psz2: PAnsiChar): PAnsiChar;
{$EXTERNALSYM StrCpy}
{$ENDIF}

function StrCpyNA(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
{$EXTERNALSYM StrCpyNA}
function StrCpyNW(psz1, psz2: PWideChar; cchMax: Integer): PWideChar;
{$EXTERNALSYM StrCpyNW}
{$IFDEF UNICODE}
function StrCpyN(psz1, psz2: PWideChar; cchMax: Integer): PWideChar;
{$EXTERNALSYM StrCpyN}
{$ELSE}
function StrCpyN(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
{$EXTERNALSYM StrCpyN}
{$ENDIF}

function StrToLongA(lpSrch: PAnsiChar): Integer;
{$EXTERNALSYM StrToLongA}
function StrToLongW(lpSrch: PWideChar): Integer;
{$EXTERNALSYM StrToLongW}
function StrToLong(lpSrch: PChar): Integer;
{$EXTERNALSYM StrToLong}
function StrNCmpA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer;
{$EXTERNALSYM StrNCmpA}
function StrNCmpW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer;
{$EXTERNALSYM StrNCmpW}
function StrNCmp(lpStr1, lpStr2: PChar; nChar: Integer): Integer;
{$EXTERNALSYM StrNCmp}
function StrNCmpIA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer;
{$EXTERNALSYM StrNCmpIA}
function StrNCmpIW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer;
{$EXTERNALSYM StrNCmpIW}
function StrNCmpI(lpStr1, lpStr2: PChar; nChar: Integer): Integer;
{$EXTERNALSYM StrNCmpI}
function StrNCpyA(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
{$EXTERNALSYM StrNCpyA}
function StrNCpyW(psz1, psz2: PWideChar; cchMax: Integer): PWideChar;
{$EXTERNALSYM StrNCpyW}
function StrNCpy(psz1, psz2: PChar; cchMax: Integer): PChar;
{$EXTERNALSYM StrNCpy}
function StrCatNA(psz1: PAnsiChar; psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
{$EXTERNALSYM StrCatNA}
function StrCatNW(psz1: PWideChar; psz2: PWideChar; cchMax: Integer): PWideChar;
{$EXTERNALSYM StrCatNW}
function StrCatN(psz1: PChar; psz2: PChar; cchMax: Integer): PChar;
{$EXTERNALSYM StrCatN}
{$ENDIF} // NO_SHLWAPI_STRFCNS

{$IFNDEF NO_SHLWAPI_PATH}
// Path Routines

function PathAddBackslashA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathAddBackslashA}
function PathAddBackslashW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathAddBackslashW}
function PathAddBackslash(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathAddBackslash}
function PathAddExtensionA(pszPath: PAnsiChar; pszExt: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathAddExtensionA}
function PathAddExtensionW(pszPath: PWideChar; pszExt: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathAddExtensionW}
function PathAddExtension(pszPath: PChar; pszExt: PChar): BOOL; stdcall;
{$EXTERNALSYM PathAddExtension}
function PathAppendA(pszPath: PAnsiChar; pMore: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathAppendA}
function PathAppendW(pszPath: PWideChar; pMore: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathAppendW}
function PathAppend(pszPath: PChar; pMore: PChar): BOOL; stdcall;
{$EXTERNALSYM PathAppend}
function PathBuildRootA(szRoot: PAnsiChar; iDrive: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM PathBuildRootA}
function PathBuildRootW(szRoot: PWideChar; iDrive: Integer): PWideChar; stdcall;
{$EXTERNALSYM PathBuildRootW}
function PathBuildRoot(szRoot: PChar; iDrive: Integer): PChar; stdcall;
{$EXTERNALSYM PathBuildRoot}
function PathCanonicalizeA(pszBuf: PAnsiChar; pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathCanonicalizeA}
function PathCanonicalizeW(pszBuf: PWideChar; pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathCanonicalizeW}
function PathCanonicalize(pszBuf: PChar; pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathCanonicalize}
function PathCombineA(szDest: PAnsiChar; lpszDir, lpszFile: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathCombineA}
function PathCombineW(szDest: PWideChar; lpszDir, lpszFile: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathCombineW}
function PathCombine(szDest: PChar; lpszDir, lpszFile: PChar): PChar; stdcall;
{$EXTERNALSYM PathCombine}
function PathCompactPathA(hDC: HDC; pszPath: PAnsiChar; dx: UINT): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathA}
function PathCompactPathW(hDC: HDC; pszPath: PWideChar; dx: UINT): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathW}
function PathCompactPath(hDC: HDC; pszPath: PChar; dx: UINT): BOOL; stdcall;
{$EXTERNALSYM PathCompactPath}
function PathCompactPathExA(pszOut: PAnsiChar; pszSrc: PAnsiChar; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathExA}
function PathCompactPathExW(pszOut: PWideChar; pszSrc: PWideChar; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathExW}
function PathCompactPathEx(pszOut: PChar; pszSrc: PChar; cchMax: UINT; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathCompactPathEx}
function PathCommonPrefixA(pszFile1, pszFile2: PAnsiChar; achPath: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM PathCommonPrefixA}
function PathCommonPrefixW(pszFile1, pszFile2: PWideChar; achPath: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathCommonPrefixW}
function PathCommonPrefix(pszFile1, pszFile2: PChar; achPath: PChar): Integer; stdcall;
{$EXTERNALSYM PathCommonPrefix}
function PathFileExistsA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathFileExistsA}
function PathFileExistsW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathFileExistsW}
function PathFileExists(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathFileExists}
function PathFindExtensionA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindExtensionA}
function PathFindExtensionW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathFindExtensionW}
function PathFindExtension(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathFindExtension}
function PathFindFileNameA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindFileNameA}
function PathFindFileNameW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathFindFileNameW}
function PathFindFileName(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathFindFileName}
function PathFindNextComponentA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindNextComponentA}
function PathFindNextComponentW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathFindNextComponentW}
function PathFindNextComponent(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathFindNextComponent}
function PathFindOnPathA(pszPath: PAnsiChar; var ppszOtherDirs: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathFindOnPathA}
function PathFindOnPathW(pszPath: PWideChar; var ppszOtherDirs: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathFindOnPathW}
function PathFindOnPath(pszPath: PChar; var ppszOtherDirs: PChar): BOOL; stdcall;
{$EXTERNALSYM PathFindOnPath}
function PathGetArgsA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathGetArgsA}
function PathGetArgsW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathGetArgsW}
function PathGetArgs(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathGetArgs}

{$IFDEF _WIN32_IE_5}

{TODO apszSuffix is a pointer to an array of LPCTSTR's!}

function PathFindSuffixArrayA(pszPath: PAnsiChar; const apszSuffix: PAnsiChar;
  iArraySize: Integer): PAnsiChar; stdcall;
{$EXTERNALSYM PathFindSuffixArrayA}
function PathFindSuffixArrayW(pszPath: PWideChar; const apszSuffix: PWideChar;
  iArraySize: Integer): PWideChar; stdcall;
{$EXTERNALSYM PathFindSuffixArrayW}
function PathFindSuffixArray(pszPath: PChar; const apszSuffix: PChar;
  iArraySize: Integer): PChar; stdcall;
{$EXTERNALSYM PathFindSuffixArray}
function PathIsLFNFileSpecA(lpName: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsLFNFileSpecA}
function PathIsLFNFileSpecW(lpName: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsLFNFileSpecW}
function PathIsLFNFileSpec(lpName: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsLFNFileSpec}

{$ENDIF}

function PathGetCharTypeA(ch: AnsiChar): UINT; stdcall;
{$EXTERNALSYM PathGetCharTypeA}
function PathGetCharTypeW(ch: WideChar): UINT; stdcall;
{$EXTERNALSYM PathGetCharTypeW}
function PathGetCharType(ch: Char): UINT; stdcall;
{$EXTERNALSYM PathGetCharType}

// Return flags for PathGetCharType

const
  GCT_INVALID   = $0000;
  {$EXTERNALSYM GCT_INVALID}
  GCT_LFNCHAR   = $0001;
  {$EXTERNALSYM GCT_LFNCHAR}
  GCT_SHORTCHAR = $0002;
  {$EXTERNALSYM GCT_SHORTCHAR}
  GCT_WILD      = $0004;
  {$EXTERNALSYM GCT_WILD}
  GCT_SEPARATOR = $0008;
  {$EXTERNALSYM GCT_SEPARATOR}

function PathGetDriveNumberA(pszPath: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM PathGetDriveNumberA}
function PathGetDriveNumberW(pszPath: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathGetDriveNumberW}
function PathGetDriveNumber(pszPath: PChar): Integer; stdcall;
{$EXTERNALSYM PathGetDriveNumber}
function PathIsDirectoryA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryA}
function PathIsDirectoryW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryW}
function PathIsDirectory(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectory}

{$IFDEF _WIN32_IE_5}

function PathIsDirectoryEmptyA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryEmptyA}
function PathIsDirectoryEmptyW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryEmptyW}
function PathIsDirectoryEmpty(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsDirectoryEmpty}

{$ENDIF}

function PathIsFileSpecA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsFileSpecA}
function PathIsFileSpecW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsFileSpecW}
function PathIsFileSpec(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsFileSpec}
function PathIsPrefixA(pszPrefix, pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsPrefixA}
function PathIsPrefixW(pszPrefix, pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsPrefixW}
function PathIsPrefix(pszPrefix, pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsPrefix}
function PathIsRelativeA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRelativeA}
function PathIsRelativeW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRelativeW}
function PathIsRelative(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRelative}
function PathIsRootA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRootA}
function PathIsRootW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRootW}
function PathIsRoot(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsRoot}
function PathIsSameRootA(pszPath1, pszPath2: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsSameRootA}
function PathIsSameRootW(pszPath1, pszPath2: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsSameRootW}
function PathIsSameRoot(pszPath1, pszPath2: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsSameRoot}
function PathIsUNCA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCA}
function PathIsUNCW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCW}
function PathIsUNC(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNC}

{$IFDEF _WIN32_IE_5}

function PathIsNetworkPathA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsNetworkPathA}
function PathIsNetworkPathW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsNetworkPathW}
function PathIsNetworkPath(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsNetworkPath}

{$ENDIF}

function PathIsUNCServerA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerA}
function PathIsUNCServerW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerW}
function PathIsUNCServer(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServer}
function PathIsUNCServerShareA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerShareA}
function PathIsUNCServerShareW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerShareW}
function PathIsUNCServerShare(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsUNCServerShare}
function PathIsContentTypeA(pszPath, pszContentType: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsContentTypeA}
function PathIsContentTypeW(pszPath, pszContentType: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsContentTypeW}
function PathIsContentType(pszPath, pszContentType: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsContentType}
function PathIsURLA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathIsURLA}
function PathIsURLW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathIsURLW}
function PathIsURL(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathIsURL}
function PathMakePrettyA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathMakePrettyA}
function PathMakePrettyW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathMakePrettyW}
function PathMakePretty(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathMakePretty}
function PathMatchSpecA(pszFile, pszSpec: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathMatchSpecA}
function PathMatchSpecW(pszFile, pszSpec: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathMatchSpecW}
function PathMatchSpec(pszFile, pszSpec: PChar): BOOL; stdcall;
{$EXTERNALSYM PathMatchSpec}
function PathParseIconLocationA(pszIconFile: PAnsiChar): Integer; stdcall;
{$EXTERNALSYM PathParseIconLocationA}
function PathParseIconLocationW(pszIconFile: PWideChar): Integer; stdcall;
{$EXTERNALSYM PathParseIconLocationW}
function PathParseIconLocation(pszIconFile: PChar): Integer; stdcall;
{$EXTERNALSYM PathParseIconLocation}
procedure PathQuoteSpacesA(lpsz: PAnsiChar); stdcall;
{$EXTERNALSYM PathQuoteSpacesA}
procedure PathQuoteSpacesW(lpsz: PWideChar); stdcall;
{$EXTERNALSYM PathQuoteSpacesW}
procedure PathQuoteSpaces(lpsz: PChar); stdcall;
{$EXTERNALSYM PathQuoteSpaces}
function PathRelativePathToA(pszPath: PAnsiChar; pszFrom: PAnsiChar; dwAttrFrom: DWORD;
  pszTo: PAnsiChar; dwAttrTo: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathRelativePathToA}
function PathRelativePathToW(pszPath: PWideChar; pszFrom: PWideChar; dwAttrFrom: DWORD;
  pszTo: PWideChar; dwAttrTo: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathRelativePathToW}
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAttrTo: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathRelativePathTo}
procedure PathRemoveArgsA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathRemoveArgsA}
procedure PathRemoveArgsW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathRemoveArgsW}
procedure PathRemoveArgs(pszPath: PChar); stdcall;
{$EXTERNALSYM PathRemoveArgs}
function PathRemoveBackslashA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathRemoveBackslashA}
function PathRemoveBackslashW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathRemoveBackslashW}
function PathRemoveBackslash(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathRemoveBackslash}
procedure PathRemoveBlanksA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathRemoveBlanksA}
procedure PathRemoveBlanksW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathRemoveBlanksW}
procedure PathRemoveBlanks(pszPath: PChar); stdcall;
{$EXTERNALSYM PathRemoveBlanks}
procedure PathRemoveExtensionA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathRemoveExtensionA}
procedure PathRemoveExtensionW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathRemoveExtensionW}
procedure PathRemoveExtension(pszPath: PChar); stdcall;
{$EXTERNALSYM PathRemoveExtension}
function PathRemoveFileSpecA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathRemoveFileSpecA}
function PathRemoveFileSpecW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathRemoveFileSpecW}
function PathRemoveFileSpec(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathRemoveFileSpec}
function PathRenameExtensionA(pszPath: PAnsiChar; pszExt: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathRenameExtensionA}
function PathRenameExtensionW(pszPath: PWideChar; pszExt: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathRenameExtensionW}
function PathRenameExtension(pszPath: PChar; pszExt: PChar): BOOL; stdcall;
{$EXTERNALSYM PathRenameExtension}
function PathSearchAndQualifyA(pszPath: PAnsiChar; pszBuf: PAnsiChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathSearchAndQualifyA}
function PathSearchAndQualifyW(pszPath: PWideChar; pszBuf: PWideChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathSearchAndQualifyW}
function PathSearchAndQualify(pszPath: PChar; pszBuf: PChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathSearchAndQualify}
procedure PathSetDlgItemPathA(hDlg: HWND; id: Integer; pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathSetDlgItemPathA}
procedure PathSetDlgItemPathW(hDlg: HWND; id: Integer; pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathSetDlgItemPathW}
procedure PathSetDlgItemPath(hDlg: HWND; id: Integer; pszPath: PChar); stdcall;
{$EXTERNALSYM PathSetDlgItemPath}
function PathSkipRootA(pszPath: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM PathSkipRootA}
function PathSkipRootW(pszPath: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM PathSkipRootW}
function PathSkipRoot(pszPath: PChar): PChar; stdcall;
{$EXTERNALSYM PathSkipRoot}
procedure PathStripPathA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathStripPathA}
procedure PathStripPathW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathStripPathW}
procedure PathStripPath(pszPath: PChar); stdcall;
{$EXTERNALSYM PathStripPath}
function PathStripToRootA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathStripToRootA}
function PathStripToRootW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathStripToRootW}
function PathStripToRoot(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathStripToRoot}
procedure PathUnquoteSpacesA(lpsz: PAnsiChar); stdcall;
{$EXTERNALSYM PathUnquoteSpacesA}
procedure PathUnquoteSpacesW(lpsz: PWideChar); stdcall;
{$EXTERNALSYM PathUnquoteSpacesW}
procedure PathUnquoteSpaces(lpsz: PChar); stdcall;
{$EXTERNALSYM PathUnquoteSpaces}
function PathMakeSystemFolderA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathMakeSystemFolderA}
function PathMakeSystemFolderW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathMakeSystemFolderW}
function PathMakeSystemFolder(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathMakeSystemFolder}
function PathUnmakeSystemFolderA(pszPath: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM PathUnmakeSystemFolderA}
function PathUnmakeSystemFolderW(pszPath: PWideChar): BOOL; stdcall;
{$EXTERNALSYM PathUnmakeSystemFolderW}
function PathUnmakeSystemFolder(pszPath: PChar): BOOL; stdcall;
{$EXTERNALSYM PathUnmakeSystemFolder}
function PathIsSystemFolderA(pszPath: PAnsiChar; dwAttrb: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSystemFolderA}
function PathIsSystemFolderW(pszPath: PWideChar; dwAttrb: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSystemFolderW}
function PathIsSystemFolder(pszPath: PChar; dwAttrb: DWORD): BOOL; stdcall;
{$EXTERNALSYM PathIsSystemFolder}

{$IFDEF _WIN32_IE_5}

procedure PathUndecorateA(pszPath: PAnsiChar); stdcall;
{$EXTERNALSYM PathUndecorateA}
procedure PathUndecorateW(pszPath: PWideChar); stdcall;
{$EXTERNALSYM PathUndecorateW}
procedure PathUndecorate(pszPath: PChar); stdcall;
{$EXTERNALSYM PathUndecorate}
function PathUnExpandEnvStringsA(pszPath: PAnsiChar; pszBuf: PAnsiChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathUnExpandEnvStringsA}
function PathUnExpandEnvStringsW(pszPath: PWideChar; pszBuf: PWideChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathUnExpandEnvStringsW}
function PathUnExpandEnvStrings(pszPath: PChar; pszBuf: PChar; cchBuf: UINT): BOOL; stdcall;
{$EXTERNALSYM PathUnExpandEnvStrings}

const
  URL_SCHEME_INVALID     = -1;
  {$EXTERNALSYM URL_SCHEME_INVALID}
  URL_SCHEME_UNKNOWN     =  0;
  {$EXTERNALSYM URL_SCHEME_UNKNOWN}
  URL_SCHEME_FTP         =  1;
  {$EXTERNALSYM URL_SCHEME_FTP}
  URL_SCHEME_HTTP        =  2;
  {$EXTERNALSYM URL_SCHEME_HTTP}
  URL_SCHEME_GOPHER      =  3;
  {$EXTERNALSYM URL_SCHEME_GOPHER}
  URL_SCHEME_MAILTO      =  4;
  {$EXTERNALSYM URL_SCHEME_MAILTO}
  URL_SCHEME_NEWS        =  5;
  {$EXTERNALSYM URL_SCHEME_NEWS}
  URL_SCHEME_NNTP        =  6;
  {$EXTERNALSYM URL_SCHEME_NNTP}
  URL_SCHEME_TELNET      =  7;
  {$EXTERNALSYM URL_SCHEME_TELNET}
  URL_SCHEME_WAIS        =  8;
  {$EXTERNALSYM URL_SCHEME_WAIS}
  URL_SCHEME_FILE        =  9;
  {$EXTERNALSYM URL_SCHEME_FILE}
  URL_SCHEME_MK          =  10;
  {$EXTERNALSYM URL_SCHEME_MK}
  URL_SCHEME_HTTPS       =  11;
  {$EXTERNALSYM URL_SCHEME_HTTPS}
  URL_SCHEME_SHELL       =  12;
  {$EXTERNALSYM URL_SCHEME_SHELL}
  URL_SCHEME_SNEWS       =  13;
  {$EXTERNALSYM URL_SCHEME_SNEWS}
  URL_SCHEME_LOCAL       =  14;
  {$EXTERNALSYM URL_SCHEME_LOCAL}
  URL_SCHEME_JAVASCRIPT  =  15;
  {$EXTERNALSYM URL_SCHEME_JAVASCRIPT}
  URL_SCHEME_VBSCRIPT    =  16;
  {$EXTERNALSYM URL_SCHEME_VBSCRIPT}
  URL_SCHEME_ABOUT       =  17;
  {$EXTERNALSYM URL_SCHEME_ABOUT}
  URL_SCHEME_RES         =  18;
  {$EXTERNALSYM URL_SCHEME_RES}
  URL_SCHEME_MAXVALUE    =  19;
  {$EXTERNALSYM URL_SCHEME_MAXVALUE}

type
  URL_SCHEME = Integer;
  {$EXTERNALSYM URL_SCHEME}

const
  URL_PART_NONE       = 0;
  {$EXTERNALSYM URL_PART_NONE}
  URL_PART_SCHEME     = 1;
  {$EXTERNALSYM URL_PART_SCHEME}
  URL_PART_HOSTNAME   = 2;
  {$EXTERNALSYM URL_PART_HOSTNAME}
  URL_PART_USERNAME   = 3;
  {$EXTERNALSYM URL_PART_USERNAME}
  URL_PART_PASSWORD   = 4;
  {$EXTERNALSYM URL_PART_PASSWORD}
  URL_PART_PORT       = 5;
  {$EXTERNALSYM URL_PART_PORT}
  URL_PART_QUERY      = 6;
  {$EXTERNALSYM URL_PART_QUERY}

type
  URL_PART = DWORD;
  {$EXTERNALSYM URL_PART}

const
  URLIS_URL        = 0;
  {$EXTERNALSYM URLIS_URL}
  URLIS_OPAQUE     = 1;
  {$EXTERNALSYM URLIS_OPAQUE }
  URLIS_NOHISTORY  = 2;
  {$EXTERNALSYM URLIS_NOHISTORY}
  URLIS_FILEURL    = 3;
  {$EXTERNALSYM URLIS_FILEURL}
  URLIS_APPLIABLE  = 4;
  {$EXTERNALSYM URLIS_APPLIABLE}
  URLIS_DIRECTORY  = 5;
  {$EXTERNALSYM URLIS_DIRECTORY}
  URLIS_HASQUERY   = 6;
  {$EXTERNALSYM URLIS_HASQUERY}

type
  TUrlIs = DWORD;

const
  URL_UNESCAPE                   = $10000000;
  {$EXTERNALSYM URL_UNESCAPE}
  URL_ESCAPE_UNSAFE              = $20000000;
  {$EXTERNALSYM URL_ESCAPE_UNSAFE}
  URL_PLUGGABLE_PROTOCOL         = $40000000;
  {$EXTERNALSYM URL_PLUGGABLE_PROTOCOL}
  URL_WININET_COMPATIBILITY      = DWORD($80000000);
  {$EXTERNALSYM URL_WININET_COMPATIBILITY}
  URL_DONT_ESCAPE_EXTRA_INFO     = $02000000;
  {$EXTERNALSYM URL_DONT_ESCAPE_EXTRA_INFO}
  URL_DONT_UNESCAPE_EXTRA_INFO   = URL_DONT_ESCAPE_EXTRA_INFO;
  {$EXTERNALSYM URL_DONT_UNESCAPE_EXTRA_INFO}
  URL_BROWSER_MODE               = URL_DONT_ESCAPE_EXTRA_INFO;
  {$EXTERNALSYM URL_BROWSER_MODE}
  URL_ESCAPE_SPACES_ONLY         = $04000000;
  {$EXTERNALSYM URL_ESCAPE_SPACES_ONLY}
  URL_DONT_SIMPLIFY              = $08000000;
  {$EXTERNALSYM URL_DONT_SIMPLIFY}
  URL_NO_META                    = URL_DONT_SIMPLIFY;
  {$EXTERNALSYM URL_NO_META}
  URL_UNESCAPE_INPLACE           = $00100000;
  {$EXTERNALSYM URL_UNESCAPE_INPLACE}
  URL_CONVERT_IF_DOSPATH         = $00200000;
  {$EXTERNALSYM URL_CONVERT_IF_DOSPATH}
  URL_UNESCAPE_HIGH_ANSI_ONLY    = $00400000;
  {$EXTERNALSYM URL_UNESCAPE_HIGH_ANSI_ONLY}
  URL_INTERNAL_PATH              = $00800000; // Will escape #'s in paths
  {$EXTERNALSYM URL_INTERNAL_PATH}
  URL_FILE_USE_PATHURL           = $00010000;
  {$EXTERNALSYM URL_FILE_USE_PATHURL}
  URL_ESCAPE_PERCENT             = $00001000;
  {$EXTERNALSYM URL_ESCAPE_PERCENT}
  URL_ESCAPE_SEGMENT_ONLY        = $00002000; // Treat the entire URL param as one URL segment.
  {$EXTERNALSYM URL_ESCAPE_SEGMENT_ONLY}

  URL_PARTFLAG_KEEPSCHEME        = $00000001;
  {$EXTERNALSYM URL_PARTFLAG_KEEPSCHEME}

  URL_APPLY_DEFAULT              = $00000001;
  {$EXTERNALSYM URL_APPLY_DEFAULT}
  URL_APPLY_GUESSSCHEME          = $00000002;
  {$EXTERNALSYM URL_APPLY_GUESSSCHEME}
  URL_APPLY_GUESSFILE            = $00000004;
  {$EXTERNALSYM URL_APPLY_GUESSFILE}
  URL_APPLY_FORCEAPPLY           = $00000008;
  {$EXTERNALSYM URL_APPLY_FORCEAPPLY}

function UrlCompareA(psz1, psz2: PAnsiChar; fIgnoreSlash: BOOL): Integer; stdcall;
{$EXTERNALSYM UrlCompareA}
function UrlCompareW(psz1, psz2: PWideChar; fIgnoreSlash: BOOL): Integer; stdcall;
{$EXTERNALSYM UrlCompareW}
function UrlCompare(psz1, psz2: PChar; fIgnoreSlash: BOOL): Integer; stdcall;
{$EXTERNALSYM UrlCompare}
function UrlCombineA(pszBase, pszRelative: PAnsiChar; pszCombined: PAnsiChar;
  out pcchCombined: DWORD; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCombineA}
function UrlCombineW(pszBase, pszRelative: PWideChar; pszCombined: PWideChar;
  out pcchCombined: DWORD; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCombineW}
function UrlCombine(pszBase, pszRelative: PChar; pszCombined: PChar;
  out pcchCombined: DWORD; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCombine}
function UrlCanonicalizeA(pszUrl: PAnsiChar; pszCanonicalized: PAnsiChar;
  pcchCanonicalized: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCanonicalizeA}
function UrlCanonicalizeW(pszUrl: PWideChar; pszCanonicalized: PWideChar;
  pcchCanonicalized: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCanonicalizeW}
function UrlCanonicalize(pszUrl: PChar; pszCanonicalized: PChar;
  pcchCanonicalized: LPDWORD; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCanonicalize}
function UrlIsOpaqueA(pszURL: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsOpaqueA}
function UrlIsOpaqueW(pszURL: PWideChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsOpaqueW}
function UrlIsOpaque(pszURL: PChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsOpaque}
function UrlIsNoHistoryA(pszURL: PAnsiChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsNoHistoryA}
function UrlIsNoHistoryW(pszURL: PWideChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsNoHistoryW}
function UrlIsNoHistory(pszURL: PChar): BOOL; stdcall;
{$EXTERNALSYM UrlIsNoHistory}
function UrlIsFileUrlA(pszURL: PAnsiChar): BOOL;
{$EXTERNALSYM UrlIsFileUrlA}
function UrlIsFileUrlW(pszURL: PWideChar): BOOL;
{$EXTERNALSYM UrlIsFileUrlW}
function UrlIsFileUrl(pszURL: PChar): BOOL;
{$EXTERNALSYM UrlIsFileUrl}
function UrlIsA(pszUrl: PAnsiChar; UrlIs: TUrlIs): BOOL; stdcall;
{$EXTERNALSYM UrlIsA}
function UrlIsW(pszUrl: PWideChar; UrlIs: TUrlIs): BOOL; stdcall;
{$EXTERNALSYM UrlIsW}
function UrlIs(pszUrl: PChar; UrlIs: TUrlIs): BOOL; stdcall;
{$EXTERNALSYM UrlIs}
function UrlGetLocationA(psz1: PAnsiChar): PAnsiChar; stdcall;
{$EXTERNALSYM UrlGetLocationA}
function UrlGetLocationW(psz1: PWideChar): PWideChar; stdcall;
{$EXTERNALSYM UrlGetLocationW}
function UrlGetLocation(psz1: PChar): PChar; stdcall;
{$EXTERNALSYM UrlGetLocation}
function UrlUnescapeA(pszUrl, pszUnescaped: PAnsiChar; pcchUnescaped: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlUnescapeA}
function UrlUnescapeW(pszUrl, pszUnescaped: PWideChar; pcchUnescaped: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlUnescapeW}
function UrlUnescape(pszUrl, pszUnescaped: PChar; pcchUnescaped: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlUnescape}
function UrlEscapeA(pszUrl: PAnsiChar; pszEscaped: PAnsiChar; pcchEscaped: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlEscapeA}
function UrlEscapeW(pszUrl: PWideChar; pszEscaped: PWideChar; pcchEscaped: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlEscapeW}
function UrlEscape(pszUrl: PChar; pszEscaped: PChar; pcchEscaped: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlEscape}
function UrlCreateFromPathA(pszPath: PAnsiChar; pszUrl: PAnsiChar; pcchUrl: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCreateFromPathA}
function UrlCreateFromPathW(pszPath: PWideChar; pszUrl: PWideChar; pcchUrl: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCreateFromPathW}
function UrlCreateFromPath(pszPath: PChar; pszUrl: PChar; pcchUrl: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlCreateFromPath}
function PathCreateFromUrlA(pszUrl: PAnsiChar; pszPath: PAnsiChar; pcchPath: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM PathCreateFromUrlA}
function PathCreateFromUrlW(pszUrl: PWideChar; pszPath: PWideChar; pcchPath: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM PathCreateFromUrlW}
function PathCreateFromUrl(pszUrl: PChar; pszPath: PChar; pcchPath: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM PathCreateFromUrl}
function UrlHashA(pszUrl: PAnsiChar; pbHash: PBYTE; cbHash: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlHashA}
function UrlHashW(pszUrl: PWideChar; pbHash: PBYTE; cbHash: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlHashW}
function UrlHash(pszUrl: PChar; pbHash: PBYTE; cbHash: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlHash}
function UrlGetPartA(pszIn: PAnsiChar; pszOut: PAnsiChar; pcchOut: LPDWORD; dwPart,
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlGetPartA}
function UrlGetPartW(pszIn: PWideChar; pszOut: PWideChar; pcchOut: LPDWORD; dwPart,
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlGetPartW}
function UrlGetPart(pszIn: PChar; pszOut: PChar; pcchOut: LPDWORD; dwPart,
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlGetPart}
function UrlApplySchemeA(pszIn: PAnsiChar; pszOut: PAnsiChar; pcchOut: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlApplySchemeA}
function UrlApplySchemeW(pszIn: PWideChar; pszOut: PWideChar; pcchOut: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlApplySchemeW}
function UrlApplyScheme(pszIn: PChar; pszOut: PChar; pcchOut: LPDWORD;
  dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlApplyScheme}
function HashData(pbData: PBYTE; cbData: DWORD; pbHash: PBYTE; cbHash: DWORD): HRESULT; stdcall;
{$EXTERNALSYM HashData}

function UrlEscapeSpacesA(pszUrl: PAnsiChar; pszEscaped: PAnsiChar; pcchEscaped: LPDWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlEscapeSpacesA}
function UrlEscapeSpacesW(pszUrl: PWideChar; pszEscaped: PWideChar; pcchEscaped: LPDWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlEscapeSpacesW}
function UrlEscapeSpaces(pszUrl: PChar; pszEscaped: PChar; pcchEscaped: LPDWORD): HRESULT; stdcall;
{$EXTERNALSYM UrlEscapeSpaces}
function UrlUnescapeInPlaceA(pszUrl: PAnsiChar; dwFlags: DWORD): HRESULT;
{$EXTERNALSYM UrlUnescapeInPlaceA}
function UrlUnescapeInPlaceW(pszUrl: PWideChar; dwFlags: DWORD): HRESULT;
{$EXTERNALSYM UrlUnescapeInPlaceW}
function UrlUnescapeInPlace(pszUrl: PChar; dwFlags: DWORD): HRESULT;
{$EXTERNALSYM UrlUnescapeInPlace}

{$ENDIF} //  IE 5
{$ENDIF} //  NO_SHLWAPI_PATH

{$IFNDEF NO_SHLWAPI_REG}
// Registry Routines

// SHDeleteEmptyKey mimics RegDeleteKey as it behaves on NT.
// SHDeleteKey mimics RegDeleteKey as it behaves on Win95.

function SHDeleteEmptyKeyA(hKey: HKEY; pszSubKey: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteEmptyKeyA}
function SHDeleteEmptyKeyW(hKey: HKEY; pszSubKey: PWideChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteEmptyKeyW}
function SHDeleteEmptyKey(hKey: HKEY; pszSubKey: PChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteEmptyKey}
function SHDeleteKeyA(hKey: HKEY; pszSubKey: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteKeyA}
function SHDeleteKeyW(hKey: HKEY; pszSubKey: PWideChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteKeyW}
function SHDeleteKey(hKey: HKEY; pszSubKey: PChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteKey}

// These functions open the key, get/set/delete the value, then close
// the key.

function SHDeleteValueA(hKey: HKEY; pszSubKey, pszValue: PAnsiChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteValueA}
function SHDeleteValueW(hKey: HKEY; pszSubKey, pszValue: PWideChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteValueW}
function SHDeleteValue(hKey: HKEY; pszSubKey, pszValue: PChar): DWORD; stdcall;
{$EXTERNALSYM SHDeleteValue}
function SHGetValueA(hKey: HKEY; pszSubKey, pszValue: PAnsiChar; pdwType: LPDWORD;
  pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHGetValueA}
function SHGetValueW(hKey: HKEY; pszSubKey, pszValue: PWideChar; pdwType: LPDWORD;
  pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHGetValueW}
function SHGetValue(hKey: HKEY; pszSubKey, pszValue: PChar; pdwType: LPDWORD;
  pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHGetValue}
function SHSetValueA(hKey: HKEY; pszSubKey, pszValue: PAnsiChar; dwType: DWORD;
  pvData: Pointer; cbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHSetValueA}
function SHSetValueW(hKey: HKEY; pszSubKey, pszValue: PWideChar; dwType: DWORD;
  pvData: Pointer; cbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHSetValueW}
function SHSetValue(hKey: HKEY; pszSubKey, pszValue: PChar; dwType: DWORD;
  pvData: Pointer; cbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHSetValue}

// These functions work just like RegQueryValueEx, except if the
// data type is REG_EXPAND_SZ, then these will go ahead and expand
// out the string.  *pdwType will always be massaged to REG_SZ
// if this happens.  REG_SZ values are also guaranteed to be null
// terminated.

function SHQueryValueExA(hKey: HKEY; pszValue: PAnsiChar; pdwReserved, pdwType: LPDWORD;
  pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHQueryValueExA}
function SHQueryValueExW(hKey: HKEY; pszValue: PWideChar; pdwReserved, pdwType: LPDWORD;
  pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHQueryValueExW}
function SHQueryValueEx(hKey: HKEY; pszValue: PChar; pdwReserved, pdwType: LPDWORD;
  pvData: Pointer; var pcbData: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHQueryValueEx}

// Enumeration functions support.

function SHEnumKeyExA(hKey: HKEY; dwIndex: DWORD; pszName: PAnsiChar; var pcchName: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumKeyExA}
function SHEnumKeyExW(hKey: HKEY; dwIndex: DWORD; pszName: PWideChar; var pcchName: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumKeyExW}
function SHEnumKeyEx(hKey: HKEY; dwIndex: DWORD; pszName: PChar; var pcchName: DWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumKeyEx}
function SHEnumValueA(hKey: HKEY; dwIndex: DWORD; pszValueName: PAnsiChar;
  var pcchValueName: DWORD; pdwType: LPDWORD; pvData: Pointer; pcbData: LPDWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumValueA}
function SHEnumValueW(hKey: HKEY; dwIndex: DWORD; pszValueName: PWideChar;
  var pcchValueName: DWORD; pdwType: LPDWORD; pvData: Pointer; pcbData: LPDWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumValueW}
function SHEnumValue(hKey: HKEY; dwIndex: DWORD; pszValueName: PChar;
  var pcchValueName: DWORD; pdwType: LPDWORD; pvData: Pointer; pcbData: LPDWORD): Longint; stdcall;
{$EXTERNALSYM SHEnumValue}
function SHQueryInfoKeyA(hKey: HKEY; pcSubKeys, pcchMaxSubKeyLen, pcValues,
  pcchMaxValueNameLen: LPDWORD): Longint; stdcall;
{$EXTERNALSYM SHQueryInfoKeyA}
function SHQueryInfoKeyW(hKey: HKEY; pcSubKeys, pcchMaxSubKeyLen, pcValues,
  pcchMaxValueNameLen: LPDWORD): Longint; stdcall;
{$EXTERNALSYM SHQueryInfoKeyW}
function SHQueryInfoKey(hKey: HKEY; pcSubKeys, pcchMaxSubKeyLen, pcValues,
  pcchMaxValueNameLen: LPDWORD): Longint; stdcall;
{$EXTERNALSYM SHQueryInfoKey}

{$IFDEF _WIN32_IE_5}

// recursive key copy
function SHCopyKeyA(hkeySrc: HKEY; szSrcSubKey: PAnsiChar; hkeyDest: HKEY;
  fReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHCopyKeyA}
function SHCopyKeyW(hkeySrc: HKEY; szSrcSubKey: PWideChar; hkeyDest: HKEY;
  fReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHCopyKeyW}
function SHCopyKey(hkeySrc: HKEY; szSrcSubKey: PChar; hkeyDest: HKEY;
  fReserved: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHCopyKey}

// Getting and setting file system paths with environment variables

function SHRegGetPathA(hKey: HKEY; pcszSubKey, pcszValue: PAnsiChar; pszPath: PAnsiChar;
  dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegGetPathA}
function SHRegGetPathW(hKey: HKEY; pcszSubKey, pcszValue: PWideChar; pszPath: PWideChar;
  dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegGetPathW}
function SHRegGetPath(hKey: HKEY; pcszSubKey, pcszValue: PChar; pszPath: PChar;
  dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegGetPath}
function SHRegSetPathA(hKey: HKEY; pcszSubKey, pcszValue, pcszPath: PAnsiChar;
  dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegSetPathA}
function SHRegSetPathW(hKey: HKEY; pcszSubKey, pcszValue, pcszPath: PWideChar;
  dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegSetPathW}
function SHRegSetPath(hKey: HKEY; pcszSubKey, pcszValue, pcszPath: PChar;
  dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM SHRegSetPath}

{$ENDIF}

//////////////////////////////////////////////
// User Specific Registry Access Functions
//////////////////////////////////////////////

// Type definitions.

const
  SHREGDEL_DEFAULT    = $00000000;       // Delete's HKCU, or HKLM if HKCU is not found.
  {$EXTERNALSYM SHREGDEL_DEFAULT}
  SHREGDEL_HKCU       = $00000001;       // Delete HKCU only
  {$EXTERNALSYM SHREGDEL_HKCU}
  SHREGDEL_HKLM       = $00000010;       // Delete HKLM only.
  {$EXTERNALSYM SHREGDEL_HKLM}
  SHREGDEL_BOTH       = $00000011;       // Delete both HKCU and HKLM.
  {$EXTERNALSYM SHREGDEL_BOTH}

  SHREGENUM_DEFAULT   = $00000000;       // Enumerates HKCU or HKLM if not found.
  {$EXTERNALSYM SHREGENUM_DEFAULT}
  SHREGENUM_HKCU      = $00000001;       // Enumerates HKCU only
  {$EXTERNALSYM SHREGENUM_HKCU}
  SHREGENUM_HKLM      = $00000010;       // Enumerates HKLM only.
  {$EXTERNALSYM SHREGENUM_HKLM}
  SHREGENUM_BOTH      = $00000011;       // Enumerates both HKCU and HKLM without duplicates.
  {$EXTERNALSYM SHREGENUM_BOTH}          // This option is NYI.

  SHREGSET_HKCU       = $00000001;       // Write to HKCU if empty.
  {$EXTERNALSYM SHREGSET_HKCU}
  SHREGSET_FORCE_HKCU = $00000002;       // Write to HKCU.
  {$EXTERNALSYM SHREGSET_FORCE_HKCU}
  SHREGSET_HKLM       = $00000004;       // Write to HKLM if empty.
  {$EXTERNALSYM SHREGSET_HKLM}
  SHREGSET_FORCE_HKLM = $00000008;       // Write to HKLM.
  {$EXTERNALSYM SHREGSET_FORCE_HKLM}
  SHREGSET_DEFAULT    = SHREGSET_FORCE_HKCU or SHREGSET_HKLM; // Default is SHREGSET_FORCE_HKCU | SHREGSET_HKLM.
  {$EXTERNALSYM SHREGSET_DEFAULT}

type
  TSHRegDelFlags = DWORD;
  TSHRegEnumFlags = DWORD;

  HUSKEY = THandle;                      // HUSKEY is a Handle to a User Specific KEY.
  {$EXTERNALSYM HUSKEY}
  PHUSKEY = ^HUSKEY;
  {$EXTERNALSYM PHUSKEY}

function SHRegCreateUSKeyA(pszPath: PAnsiChar; samDesired: REGSAM; hRelativeUSKey: HUSKEY;
  phNewUSKey: PHUSKEY; dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegCreateUSKeyA}
function SHRegCreateUSKeyW(pszPath: PWideChar; samDesired: REGSAM; hRelativeUSKey: HUSKEY;
  phNewUSKey: PHUSKEY; dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegCreateUSKeyW}
function SHRegCreateUSKey(pszPath: PChar; samDesired: REGSAM; hRelativeUSKey: HUSKEY;
  phNewUSKey: PHUSKEY; dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegCreateUSKey}
function SHRegOpenUSKeyA(pszPath: PAnsiChar; samDesired: REGSAM; hRelativeUSKey: HUSKEY;
  phNewUSKey: PHUSKEY; fIgnoreHKCU: BOOL): Longint; stdcall;
{$EXTERNALSYM SHRegOpenUSKeyA}
function SHRegOpenUSKeyW(pszPath: PWideChar; samDesired: REGSAM; hRelativeUSKey: HUSKEY;
  phNewUSKey: PHUSKEY; fIgnoreHKCU: BOOL): Longint; stdcall;
{$EXTERNALSYM SHRegOpenUSKeyW}
function SHRegOpenUSKey(pszPath: PChar; samDesired: REGSAM; hRelativeUSKey: HUSKEY;
  phNewUSKey: PHUSKEY; fIgnoreHKCU: BOOL): Longint; stdcall;
{$EXTERNALSYM SHRegOpenUSKey}
function SHRegQueryUSValueA(hUSKey: HUSKEY; pszValue: PAnsiChar; pdwType: LPDWORD;
  pvData: Pointer; pcbData: LPDWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegQueryUSValueA}
function SHRegQueryUSValueW(hUSKey: HUSKEY; pszValue: PWideChar; pdwType: LPDWORD;
  pvData: Pointer; pcbData: LPDWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegQueryUSValueW}
function SHRegQueryUSValue(hUSKey: HUSKEY; pszValue: PChar; pdwType: LPDWORD;
  pvData: Pointer; pcbData: LPDWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegQueryUSValue}
function SHRegWriteUSValueA(hUSKey: HUSKEY; pszValue: PAnsiChar; dwType: DWORD;
  pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegWriteUSValueA}
function SHRegWriteUSValueW(hUSKey: HUSKEY; pszValue: PWideChar; dwType: DWORD;
  pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegWriteUSValueW}
function SHRegWriteUSValue(hUSKey: HUSKEY; pszValue: PChar; dwType: DWORD;
  pvData: Pointer; cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegWriteUSValue}
function SHRegDeleteUSValueA(hUSKey: HUSKEY; pszValue: PAnsiChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteUSValueA}
function SHRegDeleteUSValueW(hUSKey: HUSKEY; pszValue: PWideChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteUSValueW}
function SHRegDeleteUSValue(hUSKey: HUSKEY; pszValue: PChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteUSValue}
function SHRegDeleteEmptyUSKeyA(hUSKey: HUSKEY; pwzSubKey: PAnsiChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteEmptyUSKeyA}
function SHRegDeleteEmptyUSKeyW(hUSKey: HUSKEY; pwzSubKey: PWideChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteEmptyUSKeyW}
function SHRegDeleteEmptyUSKey(hUSKey: HUSKEY; pwzSubKey: PChar; delRegFlags: TSHRegDelFlags): Longint; stdcall;
{$EXTERNALSYM SHRegDeleteEmptyUSKey}
function SHRegEnumUSKeyA(hUSKey: HUSKEY; dwIndex: DWORD; pszName: PAnsiChar;
  pcchName: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSKeyA}
function SHRegEnumUSKeyW(hUSKey: HUSKEY; dwIndex: DWORD; pszName: PWideChar;
  pcchName: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSKeyW}
function SHRegEnumUSKey(hUSKey: HUSKEY; dwIndex: DWORD; pszName: PChar;
  pcchName: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSKey}
function SHRegEnumUSValueA(hUSKey: HUSKEY; dwIndex: DWORD; pszValueName: PAnsiChar;
  pcchValueName, pdwType: LPDWORD; pvData: Pointer; pcbData: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSValueA}
function SHRegEnumUSValueW(hUSKey: HUSKEY; dwIndex: DWORD; pszValueName: PWideChar;
  pcchValueName, pdwType: LPDWORD; pvData: Pointer; pcbData: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSValueW}
function SHRegEnumUSValue(hUSKey: HUSKEY; dwIndex: DWORD; pszValueName: PChar;
  pcchValueName, pdwType: LPDWORD; pvData: Pointer; pcbData: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegEnumUSValue}
function SHRegQueryInfoUSKeyA(hUSKey: HUSKEY; pcSubKeys, pcchMaxSubKeyLen,
  pcValues, pcchMaxValueNameLen: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegQueryInfoUSKeyA}
function SHRegQueryInfoUSKeyW(hUSKey: HUSKEY; pcSubKeys, pcchMaxSubKeyLen,
  pcValues, pcchMaxValueNameLen: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegQueryInfoUSKeyW}
function SHRegQueryInfoUSKey(hUSKey: HUSKEY; pcSubKeys, pcchMaxSubKeyLen,
  pcValues, pcchMaxValueNameLen: LPDWORD; enumRegFlags: TSHRegEnumFlags): Longint; stdcall;
{$EXTERNALSYM SHRegQueryInfoUSKey}
function SHRegCloseUSKey(hUSKey: HUSKEY): Longint; stdcall;
{$EXTERNALSYM SHRegCloseUSKey}

// These calls are equal to an SHRegOpenUSKey, SHRegQueryUSValue, and then a SHRegCloseUSKey.

function SHRegGetUSValueA(pszSubKey, pszValue: PAnsiChar; pdwType: LPDWORD; pvData: Pointer;
  var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegGetUSValueA}
function SHRegGetUSValueW(pszSubKey, pszValue: PWideChar; pdwType: LPDWORD; pvData: Pointer;
  var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegGetUSValueW}
function SHRegGetUSValue(pszSubKey, pszValue: PChar; pdwType: LPDWORD; pvData: Pointer;
  var pcbData: DWORD; fIgnoreHKCU: BOOL; pvDefaultData: Pointer; dwDefaultDataSize: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegGetUSValue}
function SHRegSetUSValueA(pszSubKey, pszValue: PAnsiChar; dwType: DWORD; pvData: Pointer;
  cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegSetUSValueA}
function SHRegSetUSValueW(pszSubKey, pszValue: PWideChar; dwType: DWORD; pvData: Pointer;
  cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegSetUSValueW}
function SHRegSetUSValue(pszSubKey, pszValue: PChar; dwType: DWORD; pvData: Pointer;
  cbData, dwFlags: DWORD): Longint; stdcall;
{$EXTERNALSYM SHRegSetUSValue}


function SHRegGetBoolUSValueA(pszSubKey, pszValue: PAnsiChar; fIgnoreHKCU, fDefault: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHRegGetBoolUSValueA}
function SHRegGetBoolUSValueW(pszSubKey, pszValue: PWideChar; fIgnoreHKCU, fDefault: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHRegGetBoolUSValueW}
function SHRegGetBoolUSValue(pszSubKey, pszValue: PChar; fIgnoreHKCU, fDefault: BOOL): BOOL; stdcall;
{$EXTERNALSYM SHRegGetBoolUSValue}

{$IFDEF _WIN32_IE_5}

function SHRegGetIntW(hk: HKEY; pwzKey: LPCWSTR; iDefault: Integer): Integer; stdcall;
{$EXTERNALSYM SHRegGetIntW}

// TODO ShRegGetIntA does not exist, ShRegGetInt is typedef to ShRegGetIntW()

//  Association APIs
//
//  these APIs are to assist in accessing the data in HKCR
//  getting the Command strings and exe paths
//  for different verbs and extensions are simplified this way

const
  ASSOCF_INIT_NOREMAPCLSID           = $00000001;  //  do not remap clsids to progids
  {$EXTERNALSYM ASSOCF_INIT_NOREMAPCLSID}
  ASSOCF_INIT_BYEXENAME              = $00000002;  //  executable is being passed in
  {$EXTERNALSYM ASSOCF_INIT_BYEXENAME}
  ASSOCF_OPEN_BYEXENAME              = $00000002;  //  executable is being passed in
  {$EXTERNALSYM ASSOCF_OPEN_BYEXENAME}
  ASSOCF_INIT_DEFAULTTOSTAR          = $00000004;  //  treat "*" as the BaseClass
  {$EXTERNALSYM ASSOCF_INIT_DEFAULTTOSTAR}
  ASSOCF_INIT_DEFAULTTOFOLDER        = $00000008;  //  treat "Folder" as the BaseClass
  {$EXTERNALSYM ASSOCF_INIT_DEFAULTTOFOLDER}
  ASSOCF_NOUSERSETTINGS              = $00000010;  //  dont use HKCU
  {$EXTERNALSYM ASSOCF_NOUSERSETTINGS}
  ASSOCF_NOTRUNCATE                  = $00000020;  //  dont truncate the return string
  {$EXTERNALSYM ASSOCF_NOTRUNCATE}
  ASSOCF_VERIFY                      = $00000040;  //  verify data is accurate (DISK HITS)
  {$EXTERNALSYM ASSOCF_VERIFY}
  ASSOCF_REMAPRUNDLL                 = $00000080;  //  actually gets info about rundlls target if applicable
  {$EXTERNALSYM ASSOCF_REMAPRUNDLL}
  ASSOCF_NOFIXUPS                    = $00000100;  //  attempt to fix errors if found
  {$EXTERNALSYM ASSOCF_NOFIXUPS}
  ASSOCF_IGNOREBASECLASS             = $00000200;  //  dont recurse into the baseclass
  {$EXTERNALSYM ASSOCF_IGNOREBASECLASS}

type
  ASSOCF = DWORD;
  {$EXTERNALSYM ASSOCF}

const
  ASSOCSTR_COMMAND          = 1;  //  shell\verb\command string
  {$EXTERNALSYM ASSOCSTR_COMMAND}
  ASSOCSTR_EXECUTABLE       = 2;  //  the executable part of command string
  {$EXTERNALSYM ASSOCSTR_EXECUTABLE}
  ASSOCSTR_FRIENDLYDOCNAME  = 3;  //  friendly name of the document type
  {$EXTERNALSYM ASSOCSTR_FRIENDLYDOCNAME}
  ASSOCSTR_FRIENDLYAPPNAME  = 4;  //  friendly name of executable
  {$EXTERNALSYM ASSOCSTR_FRIENDLYAPPNAME}
  ASSOCSTR_NOOPEN           = 5;  //  noopen value
  {$EXTERNALSYM ASSOCSTR_NOOPEN}
  ASSOCSTR_SHELLNEWVALUE    = 6;  //  query values under the shellnew key
  {$EXTERNALSYM ASSOCSTR_SHELLNEWVALUE}
  ASSOCSTR_DDECOMMAND       = 7;  //  template for DDE commands
  {$EXTERNALSYM ASSOCSTR_DDECOMMAND}
  ASSOCSTR_DDEIFEXEC        = 8;  //  DDECOMMAND to use if just create a process
  {$EXTERNALSYM ASSOCSTR_DDEIFEXEC}
  ASSOCSTR_DDEAPPLICATION   = 9;  //  Application name in DDE broadcast
  {$EXTERNALSYM ASSOCSTR_DDEAPPLICATION}
  ASSOCSTR_DDETOPIC         = 10; //  Topic Name in DDE broadcast
  {$EXTERNALSYM ASSOCSTR_DDETOPIC}
  ASSOCSTR_INFOTIP          = 11; //  info tip for an item, or list of properties to create info tip from
  {$EXTERNALSYM ASSOCSTR_INFOTIP}
  ASSOCSTR_MAX              = 12; //  last item in enum...
  {$EXTERNALSYM ASSOCSTR_MAX}

type
  ASSOCSTR = DWORD;
  {$EXTERNALSYM ASSOCSTR}

const
  ASSOCKEY_SHELLEXECCLASS = 1;  //  the key that should be passed to ShellExec(hkeyClass)
  {$EXTERNALSYM ASSOCKEY_SHELLEXECCLASS}
  ASSOCKEY_APP            = 2;  //  the "Application" key for the association
  {$EXTERNALSYM ASSOCKEY_APP}
  ASSOCKEY_CLASS          = 3;  //  the progid or class key
  {$EXTERNALSYM ASSOCKEY_CLASS}
  ASSOCKEY_BASECLASS      = 4;  //  the BaseClass key
  {$EXTERNALSYM ASSOCKEY_BASECLASS}
  ASSOCKEY_MAX            = 5;  //  last item in enum...
  {$EXTERNALSYM ASSOCKEY_MAX}

type
  ASSOCKEY = DWORD;
  {$EXTERNALSYM ASSOCKEY}

const
  ASSOCDATA_MSIDESCRIPTOR     = 1;   //  Component Descriptor to pass to MSI APIs
  {$EXTERNALSYM ASSOCDATA_MSIDESCRIPTOR}
  ASSOCDATA_NOACTIVATEHANDLER = 2;   //  restrict attempts to activate window
  {$EXTERNALSYM ASSOCDATA_NOACTIVATEHANDLER}
  ASSOCDATA_QUERYCLASSSTORE   = 3;   //  should check with the NT Class Store
  {$EXTERNALSYM ASSOCDATA_QUERYCLASSSTORE}
  ASSOCDATA_HASPERUSERASSOC   = 4;   //  defaults to user specified association
  {$EXTERNALSYM ASSOCDATA_HASPERUSERASSOC}
  ASSOCDATA_MAX               = 5;
  {$EXTERNALSYM ASSOCDATA_MAX}

type
  ASSOCDATA = DWORD;
  {$EXTERNALSYM ASSOCDATA}

const
  ASSOCENUM_NONE = 0;
  {$EXTERNALSYM ASSOCENUM_NONE}

type
  ASSOCENUM = DWORD;
  {$EXTERNALSYM ASSOCENUM}

const
  IID_IQueryAssociations: TGUID = (D1: $c46ca590; D2: $3c3f; D3: $11d2; D4: ($be, $e6, $00, $00, $f8, $05, $ca, $57));
  {$EXTERNALSYM IID_IQueryAssociations}
  CLSID_QueryAssociations: TGUID = (D1: $c46ca590; D2: $3c3f; D3: $11d2; D4: ($be, $e6, $00, $00, $f8, $05, $ca, $57));
  {$EXTERNALSYM CLSID_QueryAssociations}
  SID_IQueryAssociations = '{c46ca590-3c3f-11d2-bee6-0000f805ca57}';

type
  IQueryAssociation = interface (IUnknown)
  [SID_IQueryAssociations]
    function Init(flags: ASSOCF; pszAssoc: LPCWSTR; hkProgid: HKEY; hwnd: HWND): HRESULT; stdcall;
    function GetString(flags: ASSOCF; str: ASSOCSTR; pszExtra: LPCWSTR; pszOut: LPWSTR; out pcchOut: DWORD): HRESULT; stdcall;
    function GetKey(flags: ASSOCF; key: ASSOCKEY; pszExtra: LPCWSTR; out phkeyOut: HKEY): HRESULT; stdcall;
    function GetData(flags: ASSOCF; data: ASSOCDATA; pszExtra: LPCWSTR; pvOut: Pointer; out pcbOut: DWORD): HRESULT; stdcall;
    function GetEnum(flags: ASSOCF; assocenum: ASSOCENUM; pszExtra: LPCWSTR; riid: TREFIID; ppvOut: Pointer): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IQueryAssociation}

function AssocCreate(clsid: TCLSID; riid: TREFIID; ppv: Pointer): HRESULT; stdcall;
{$EXTERNALSYM AssocCreate}

function AssocQueryStringA(flags: ASSOCF; str: ASSOCSTR; pszAssoc, pszExtra: PAnsiChar;
  pszOut: PAnsiChar; pcchOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryStringA}
function AssocQueryStringW(flags: ASSOCF; str: ASSOCSTR; pszAssoc, pszExtra: PWideChar;
  pszOut: PWideChar; pcchOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryStringW}
function AssocQueryString(flags: ASSOCF; str: ASSOCSTR; pszAssoc, pszExtra: PChar;
  pszOut: PChar; pcchOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryString}
function AssocQueryStringByKeyA(flags: ASSOCF; str: ASSOCSTR; hkAssoc: HKEY;
  pszExtra: PAnsiChar; pszOut: PAnsiChar; pcchOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryStringByKeyA}
function AssocQueryStringByKeyW(flags: ASSOCF; str: ASSOCSTR; hkAssoc: HKEY;
  pszExtra: PWideChar; pszOut: PWideChar; pcchOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryStringByKeyW}
function AssocQueryStringByKey(flags: ASSOCF; str: ASSOCSTR; hkAssoc: HKEY;
  pszExtra: PChar; pszOut: PChar; pcchOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryStringByKey}
function AssocQueryKeyA(flags: ASSOCF; key: ASSOCKEY; pszAssoc, pszExtra: PAnsiChar;
  out phkeyOut: HKEY): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryKeyA}
function AssocQueryKeyW(flags: ASSOCF; key: ASSOCKEY; pszAssoc, pszExtra: PWideChar;
  out phkeyOut: HKEY): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryKeyW}
function AssocQueryKey(flags: ASSOCF; key: ASSOCKEY; pszAssoc, pszExtra: PChar;
  out phkeyOut: HKEY): HRESULT; stdcall;
{$EXTERNALSYM AssocQueryKey}

{$ENDIF} //  IE 5
{$ENDIF} //  NO_SHLWAPI_REG

{$IFNDEF NO_SHLWAPI_STREAM}
// Stream Routines

function SHOpenRegStreamA(hkey: HKEY; pszSubkey, pszValue: PAnsiChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStreamA}
function SHOpenRegStreamW(hkey: HKEY; pszSubkey, pszValue: PWideChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStreamW}
function SHOpenRegStream(hkey: HKEY; pszSubkey, pszValue: PChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream}

{$IFDEF _WIN32_IE_5}

function SHOpenRegStream2A(hkey: HKEY; pszSubkey, pszValue: PAnsiChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream2A}
function SHOpenRegStream2W(hkey: HKEY; pszSubkey, pszValue: PWideChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream2W}
function SHOpenRegStream2(hkey: HKEY; pszSubkey, pszValue: PChar; grfMode: DWORD): IStream; stdcall;
{$EXTERNALSYM SHOpenRegStream2}

function SHCreateStreamOnFileA(pszFile: PAnsiChar; grfMode: DWORD; out ppstm: IStream): HRESULT; stdcall;
{$EXTERNALSYM SHCreateStreamOnFileA}
function SHCreateStreamOnFileW(pszFile: PWideChar; grfMode: DWORD; out ppstm: IStream): HRESULT; stdcall;
{$EXTERNALSYM SHCreateStreamOnFileW}
function SHCreateStreamOnFile(pszFile: PChar; grfMode: DWORD; out ppstm: IStream): HRESULT; stdcall;
{$EXTERNALSYM SHCreateStreamOnFile}

{$ENDIF} // IE 5
{$ENDIF} // NO_SHLWAPI_STREAM

{$IFDEF _WIN32_IE_5}

// SHAutoComplete
//      hwndEdit - HWND of editbox, ComboBox or ComboBoxEx.
//      dwFlags - Flags to indicate what to AutoAppend or AutoSuggest for the editbox.
//
// WARNING:
//    Caller needs to have called CoInitialize() or OleInitialize()
//    and cannot call CoUninit/OleUninit until after
//    WM_DESTROY on hwndEdit.
//
//  dwFlags values:

const
  SHACF_DEFAULT                  = $00000000;  // Currently (SHACF_FILESYSTEM | SHACF_URLALL)
  {$EXTERNALSYM SHACF_DEFAULT }
  SHACF_FILESYSTEM               = $00000001;  // This includes the File System as well as the rest of the shell (Desktop\My Computer\Control Panel\)
  {$EXTERNALSYM SHACF_FILESYSTEM }
  SHACF_URLHISTORY               = $00000002;  // URLs in the User's History
  {$EXTERNALSYM SHACF_URLHISTORY }
  SHACF_URLMRU                   = $00000004;  // URLs in the User's Recently Used list.
  {$EXTERNALSYM SHACF_URLMRU }
  SHACF_USETAB                   = $00000008;  // URLs in the User's Recently Used list.
  {$EXTERNALSYM SHACF_USETAB}
  SHACF_FILESYS_ONLY             = $00000010;  // Don't AutoComplete non-File System items.
  {$EXTERNALSYM SHACF_FILESYS_ONLY}

  SHACF_URLALL                   = SHACF_URLHISTORY or SHACF_URLMRU;
  {$EXTERNALSYM SHACF_URLALL }
  SHACF_AUTOSUGGEST_FORCE_ON     = $10000000;  // Ignore the registry default and force the feature on.
  {$EXTERNALSYM SHACF_AUTOSUGGEST_FORCE_ON }
  SHACF_AUTOSUGGEST_FORCE_OFF    = $20000000;  // Ignore the registry default and force the feature off.
  {$EXTERNALSYM SHACF_AUTOSUGGEST_FORCE_OFF }
  SHACF_AUTOAPPEND_FORCE_ON      = $40000000;  // Ignore the registry default and force the feature on. (Also know as AutoComplete)
  {$EXTERNALSYM SHACF_AUTOAPPEND_FORCE_ON }
  SHACF_AUTOAPPEND_FORCE_OFF     = DWORD($80000000);  // Ignore the registry default and force the feature off. (Also know as AutoComplete)
  {$EXTERNALSYM SHACF_AUTOAPPEND_FORCE_OFF}

function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM SHAutoComplete}

procedure SHSetThreadRef(punk: IUnknown); stdcall;
{$EXTERNALSYM SHSetThreadRef}
procedure SHGetThreadRef(out ppunk: IUnknown); stdcall;
{$EXTERNALSYM SHGetThreadRef}

function SHSkipJunction(pbc: IBindCtx; const pclsid: TCLSID): BOOL; stdcall;
{$EXTERNALSYM SHSkipJunction}

{$ENDIF} // _WIN32_IE >= $0500

{$IFDEF _WIN32_IE_5}

const
  CTF_INSIST         = $00000001;     // SHCreateThread() dwFlags - call pfnThreadProc synchronously if CreateThread() fails
  {$EXTERNALSYM CTF_INSIST}
  CTF_THREAD_REF     = $00000002;     // hold a reference to the creating thread
  {$EXTERNALSYM CTF_THREAD_REF}
  CTF_PROCESS_REF    = $00000004;     // hold a reference to the creating process
  {$EXTERNALSYM CTF_PROCESS_REF}
  CTF_COINIT         = $00000008;     // init COM for the created thread
  {$EXTERNALSYM CTF_COINIT}

function SHCreateThread(pfnThreadProc: TThreadStartRoutine; pData: Pointer;
  dwFlags: DWORD; pfnCallback: TThreadStartRoutine): BOOL; stdcall;
{$EXTERNALSYM SHCreateThread}

{$ENDIF} // IE 5

{$IFNDEF NO_SHLWAPI_GDI}
// GDI helper functions

function SHCreateShellPalette(hdc: HDC): HPALETTE; stdcall;
{$EXTERNALSYM SHCreateShellPalette}

{$IFDEF _WIN32_IE_5}
procedure ColorRGBToHLS(clrRGB: TColorRef; out pwHue, pwLuminance, pwSaturation: WORD); stdcall;
{$EXTERNALSYM ColorRGBToHLS}
function ColorHLSToRGB(wHue, wLuminance, wSaturation: WORD): TColorRef; stdcall;
{$EXTERNALSYM ColorHLSToRGB}
function ColorAdjustLuma(clrRGB: TColorRef; n: Integer; fScale: Boolean): TColorRef; stdcall;
{$EXTERNALSYM ColorAdjustLuma}
{$ENDIF} // _WIN32_IE >= $0500
{$ENDIF} // NO_SHLWAPI_GDI

// DllGetVersion

type
  PDllVersionInfo = ^TDllVersionInfo;
  _DLLVERSIONINFO = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD; // Major version
    dwMinorVersion: DWORD; // Minor version
    dwBuildNumber: DWORD;  // Build Number
    dwPlatformId: DWORD;   // DLLVER_PLATFORM_*
  end;
  {$EXTERNALSYM _DLLVERSIONINFO}
  TDllVersionInfo = _DLLVERSIONINFO;

const
  DLLVER_PLATFORM_WINDOWS = $00000001; // Windows 95
  {$EXTERNALSYM DLLVER_PLATFORM_WINDOWS}
  DLLVER_PLATFORM_NT      = $00000002; // Windows NT
  {$EXTERNALSYM DLLVER_PLATFORM_NT}

{$IFDEF _WIN32_IE_501}

type
  PDllVersionInfo2 = ^TDllVersionInfo2;
  _DLLVERSIONINFO2 = packed record
    info1: TDllVersionInfo;
    dwFlags: DWORD;                         // No flags currently defined
    ullVersion: Int64;                      // Encoded as:
                                            // Major 0xFFFF 0000 0000 0000
                                            // Minor 0x0000 FFFF 0000 0000
                                            // Build 0x0000 0000 FFFF 0000
                                            // QFE   0x0000 0000 0000 FFFF
  end;
  {$EXTERNALSYM _DLLVERSIONINFO2}
  DLLVERSIONINFO2 = _DLLVERSIONINFO2;
  {$EXTERNALSYM DLLVERSIONINFO2}
  TDllVersionInfo2 = DLLVERSIONINFO2;

const
  DLLVER_MAJOR_MASK = Int64($FFFF000000000000);
  {$EXTERNALSYM DLLVER_MAJOR_MASK}
  DLLVER_MINOR_MASK = Int64($0000FFFF00000000);
  {$EXTERNALSYM DLLVER_MINOR_MASK}
  DLLVER_BUILD_MASK = Int64($00000000FFFF0000);
  {$EXTERNALSYM DLLVER_BUILD_MASK}
  DLLVER_QFE_MASK   = Int64($000000000000FFFF);
  {$EXTERNALSYM DLLVER_QFE_MASK}

{$ENDIF}

function MAKEDLLVERULL(Major, Minor, Build, Qfe: Word): Int64;
{$EXTERNALSYM MAKEDLLVERULL}

// The caller should always GetProcAddress('DllGetVersion'), not
// implicitly link to it.

type
  DllGetVersionProc = function (var pdvi: TDllVersionInfo): HRESULT; stdcall;
  {$EXTERNALSYM DLLGETVERSIONPROC}

// DllInstall (to be implemented by self-installing DLLs)
// STDAPI DllInstall(BOOL bInstall, LPCWSTR pszCmdLine);
// procedure DllInstall(bInstall: BOOL; pszCmdLine: LPCWSTR); stdcall;

implementation

const
  shlwapi32 = 'shlwapi.dll';

{$IFNDEF NO_SHLWAPI_STRFCNS}
function StrChrA; external shlwapi32 name 'StrChrA';
function StrChrW; external shlwapi32 name 'StrChrW';
function StrChr; external shlwapi32 name 'StrChrA';
function StrChrIA; external shlwapi32 name 'StrChrIA';
function StrChrIW; external shlwapi32 name 'StrChrIW';
function StrChrI; external shlwapi32 name 'StrChrIA';
function StrCmpNA; external shlwapi32 name 'StrCmpNA';
function StrCmpNW; external shlwapi32 name 'StrCmpNW';
function StrCmpN; external shlwapi32 name 'StrCmpNA';
function StrCmpNIA; external shlwapi32 name 'StrCmpNIA';
function StrCmpNIW; external shlwapi32 name 'StrCmpNIW';
function StrCmpNI; external shlwapi32 name 'StrCmpNIA';
function StrCSpnA; external shlwapi32 name 'StrCSpnA';
function StrCSpnW; external shlwapi32 name 'StrCSpnW';
function StrCSpn; external shlwapi32 name 'StrCSpnA';
function StrCSpnIA; external shlwapi32 name 'StrCSpnIA';
function StrCSpnIW; external shlwapi32 name 'StrCSpnIW';
function StrCSpnI; external shlwapi32 name 'StrCSpnIA';
function StrDupA; external shlwapi32 name 'StrDupA';
function StrDupW; external shlwapi32 name 'StrDupW';
function StrDup; external shlwapi32 name 'StrDupA';
function StrFormatByteSizeA; external shlwapi32 name 'StrFormatByteSizeA';
function StrFormatByteSizeW; external shlwapi32 name 'StrFormatByteSizeW';
function StrFormatByteSize; external shlwapi32 name 'StrFormatByteSizeA';

{$IFDEF _WIN32_IE_5}

function StrFormatKBSizeA; external shlwapi32 name 'StrFormatKBSizeA';
function StrFormatKBSizeW; external shlwapi32 name 'StrFormatKBSizeW';
function StrFormatKBSize; external shlwapi32 name 'StrFormatKBSizeA';

{$ENDIF}

function StrFromTimeIntervalA; external shlwapi32 name 'StrFromTimeIntervalA';
function StrFromTimeIntervalW; external shlwapi32 name 'StrFromTimeIntervalW';
function StrFromTimeInterval; external shlwapi32 name 'StrFromTimeIntervalA';
function StrIsIntlEqualA; external shlwapi32 name 'StrIsIntlEqualA';
function StrIsIntlEqualW; external shlwapi32 name 'StrIsIntlEqualW';
function StrIsIntlEqual; external shlwapi32 name 'StrIsIntlEqualA';
function StrNCatA; external shlwapi32 name 'StrNCatA';
function StrNCatW; external shlwapi32 name 'StrNCatW';
function StrNCat; external shlwapi32 name 'StrNCatA';
function StrPBrkA; external shlwapi32 name 'StrPBrkA';
function StrPBrkW; external shlwapi32 name 'StrPBrkW';
function StrPBrk; external shlwapi32 name 'StrPBrkA';
function StrRChrA; external shlwapi32 name 'StrRChrA';
function StrRChrW; external shlwapi32 name 'StrRChrW';
function StrRChr; external shlwapi32 name 'StrRChrA';
function StrRChrIA; external shlwapi32 name 'StrRChrIA';
function StrRChrIW; external shlwapi32 name 'StrRChrIW';
function StrRChrI; external shlwapi32 name 'StrRChrIA';
function StrRStrIA; external shlwapi32 name 'StrRStrIA';
function StrRStrIW; external shlwapi32 name 'StrRStrIW';
function StrRStrI; external shlwapi32 name 'StrRStrIA';
function StrSpnA; external shlwapi32 name 'StrSpnA';
function StrSpnW; external shlwapi32 name 'StrSpnW';
function StrSpn; external shlwapi32 name 'StrSpnA';
function StrStrA; external shlwapi32 name 'StrStrA';
function StrStrW; external shlwapi32 name 'StrStrW';
function StrStr; external shlwapi32 name 'StrStrA';
function StrStrIA; external shlwapi32 name 'StrStrIA';
function StrStrIW; external shlwapi32 name 'StrStrIW';
function StrStrI; external shlwapi32 name 'StrStrIA';
function StrToIntA; external shlwapi32 name 'StrToIntA';
function StrToIntW; external shlwapi32 name 'StrToIntW';
function StrToInt; external shlwapi32 name 'StrToIntA';
function StrToIntExA; external shlwapi32 name 'StrToIntExA';
function StrToIntExW; external shlwapi32 name 'StrToIntExW';
function StrToIntEx; external shlwapi32 name 'StrToIntExA';
function StrTrimA; external shlwapi32 name 'StrTrimA';
function StrTrimW; external shlwapi32 name 'StrTrimW';
function StrTrim; external shlwapi32 name 'StrTrimA';

function ChrCmpIA; external shlwapi32 name 'StrCmpIA';
function ChrCmpIW; external shlwapi32 name 'StrCmpIW';
function ChrCmpI; external shlwapi32 name 'StrCmpIA';

function StrIntlEqNA; external shlwapi32 name 'StrIntlEqNA';
function StrIntlEqNW; external shlwapi32 name 'StrIntlEqNW';
function StrIntlEqN; external shlwapi32 name 'StrIntlEqNA';

function StrIntlEqNIA; external shlwapi32 name 'StrIntlEqNIA';
function StrIntlEqNIW; external shlwapi32 name 'StrIntlEqNIW';
function StrIntlEqNI; external shlwapi32 name 'StrIntlEqNIA';

{$IFDEF _WIN32_IE_5}

function StrCatBuffA; external shlwapi32 name 'StrCatBuffA';
function StrCatBuffW; external shlwapi32 name 'StrCatBuffW';
function StrCatBuff; external shlwapi32 name 'StrCatBuffA';

{$ENDIF}

function StrCpyNXA; external shlwapi32 name 'StrCpyNXA';
function StrCpyNXW; external shlwapi32 name 'StrCpyNXW';
function StrCpyNX; external shlwapi32 name 'StrCpyNXA';

{$IFDEF _WIN32_IE_5}

(*
LWSTDAPI_(int)      wvnsprintfA(LPSTR lpOut, int cchLimitIn, LPCSTR lpFmt, va_list arglist);
LWSTDAPI_(int)      wvnsprintfW(LPWSTR lpOut, int cchLimitIn, LPCWSTR lpFmt, va_list arglist);
LWSTDAPIV_(int)     wnsprintfA(LPSTR lpOut, int cchLimitIn, LPCSTR lpFmt, ...);
LWSTDAPIV_(int)     wnsprintfW(LPWSTR lpOut, int cchLimitIn, LPCWSTR lpFmt, ...);
*)

function StrRetToStrA; external shlwapi32 name 'StrRetToStrA';
function StrRetToStrW; external shlwapi32 name 'StrRetToStrW';
function StrRetToStr; external shlwapi32 name 'StrRetToStrA';

function StrRetToBufA; external shlwapi32 name 'StrRetToBufA';
function StrRetToBufW; external shlwapi32 name 'StrRetToBufW';
function StrRetToBuf; external shlwapi32 name 'StrRetToBufA';

function SHStrDupA; external shlwapi32 name 'SHStrDupA';
function SHStrDupW; external shlwapi32 name 'SHStrDupW';
function SHStrDup; external shlwapi32 name 'SHStrDupA';

function IntlStrEqWorkerA; external shlwapi32 name 'IntlStrEqWorkerA';
function IntlStrEqWorkerW; external shlwapi32 name 'IntlStrEqWorkerW';
function IntlStrEqWorker; external shlwapi32 name 'IntlStrEqWorkerA';


function IntlStrEqNA(s1, s2: PAnsiChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorkerA(True, s1, s2, nChar)
end;

function IntlStrEqNW(s1, s2: PWideChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorkerW(True, s1, s2, nChar)
end;

function IntlStrEqN(s1, s2: PChar; nChar: Integer): BOOL;
begin
  Result := IntlStrEqWorker(True, s1, s2, nChar)
end;

{$ENDIF} // IE 5

function PathIsHTMLFileA(pszPath: PAnsiChar): BOOL;
begin
  Result := PathIsContentTypeA(pszPath, SZ_CONTENTTYPE_HTMLA)
end;

function PathIsHTMLFileW(pszPath: PWideChar): BOOL;
begin
  Result := PathIsContentTypeW(pszPath, SZ_CONTENTTYPE_HTMLW)
end;

function PathIsHTMLFile(pszPath: PChar): BOOL;
begin
  Result := PathIsContentType(pszPath, SZ_CONTENTTYPE_HTML)
end;

function StrCatA(psz1, psz2: PAnsiChar): PAnsiChar;
begin
  Result := lstrcatA(psz1, psz2);
end;

function StrCatW; external shlwapi32 name 'StrCatW';

{$IFDEF UNICODE}
function StrCat; external shlwapi32 name 'StrCatW';
{$ELSE}
function StrCat(psz1, psz2: PAnsiChar): PAnsiChar;
begin
  Result := lstrcat(psz1, psz2);
end;
{$ENDIF}

function StrCmpA(psz1, psz2: PAnsiChar): Integer;
begin
  Result := lstrcmpA(psz1, psz2);
end;

function StrCmpW; external shlwapi32 name 'StrCmpW';

{$IFDEF UNICODE}
function StrCmp; external shlwapi32 name 'StrCmpW';
{$ELSE}
function StrCmp(psz1, psz2: PAnsiChar): Integer;
begin
  Result := lstrcmp(psz1, psz2);
end;
{$ENDIF}

function StrCmpIA(psz1, psz2: PAnsiChar): Integer;
begin
  Result := lstrcmpiA(psz1, psz2);
end;

function StrCmpIW; external shlwapi32 name 'StrCmpW';
{$IFDEF UNICODE}
function StrCmpI; external shlwapi32 name 'StrCmpW';
{$ELSE}
function StrCmpI(psz1, psz2: PAnsiChar): Integer;
begin
  Result := lstrcmpi(psz1, psz2);
end;
{$ENDIF}

function StrCpyA(psz1, psz2: PAnsiChar): PAnsiChar;
begin
  Result := lstrcpyA(psz1, psz2);
end;
function StrCpyW; external shlwapi32 name 'StrCpyW';
{$IFDEF UNICODE}
function StrCpy; external shlwapi32 name 'StrCpyW';
{$ELSE}
function StrCpy(psz1, psz2: PAnsiChar): PAnsiChar;
begin
  Result := lstrcpy(psz1, psz2);
end;
{$ENDIF}

function StrCpyNA(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
begin
  Result := lstrcpynA(psz1, psz2, cchMax);
end;
function StrCpyNW; external shlwapi32 name 'StrCpyNW';
{$IFDEF UNICODE}
function StrCpyN; external shlwapi32 name 'StrCpyNW';
{$ELSE}
function StrCpyN(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
begin
  Result := lstrcpyn(psz1, psz2, cchMax);
end;
{$ENDIF}

function StrToLongA(lpSrch: PAnsiChar): Integer;
begin
  Result := StrToIntA(lpSrch);
end;

function StrToLongW(lpSrch: PWideChar): Integer;
begin
  Result := StrToIntW(lpSrch);
end;

function StrToLong(lpSrch: PChar): Integer;
begin
  Result := StrToInt(lpSrch);
end;

function StrNCmpA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer;
begin
  Result := StrCmpNA(lpStr1, lpStr2, nChar);
end;

function StrNCmpW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer;
begin
  Result := StrCmpNW(lpStr1, lpStr2, nChar);
end;

function StrNCmp(lpStr1, lpStr2: PChar; nChar: Integer): Integer;
begin
  Result := StrCmpN(lpStr1, lpStr2, nChar);
end;

function StrNCmpIA(lpStr1, lpStr2: PAnsiChar; nChar: Integer): Integer;
begin
  Result := StrCmpNIA(lpStr1, lpStr2, nChar);
end;

function StrNCmpIW(lpStr1, lpStr2: PWideChar; nChar: Integer): Integer;
begin
  Result := StrCmpNIW(lpStr1, lpStr2, nChar);
end;

function StrNCmpI(lpStr1, lpStr2: PChar; nChar: Integer): Integer;
begin
  Result := StrCmpNI(lpStr1, lpStr2, nChar);
end;

function StrNCpyA(psz1, psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
begin
  Result := StrCpyNA(psz1, psz2, cchMax);
end;

function StrNCpyW(psz1, psz2: PWideChar; cchMax: Integer): PWideChar;
begin
  Result := StrCpyNW(psz1, psz2, cchMax);
end;

function StrNCpy(psz1, psz2: PChar; cchMax: Integer): PChar;
begin
  Result := StrCpyN(psz1, psz2, cchMax);
end;

function StrCatNA(psz1: PAnsiChar; psz2: PAnsiChar; cchMax: Integer): PAnsiChar;
begin
  Result := StrNCatA(psz1, psz2, cchMax);
end;

function StrCatNW(psz1: PWideChar; psz2: PWideChar; cchMax: Integer): PWideChar;
begin
  Result := StrNCatW(psz1, psz2, cchMax);
end;

function StrCatN(psz1: PChar; psz2: PChar; cchMax: Integer): PChar;
begin
  Result := StrNCat(psz1, psz2, cchMax);
end;

{$ENDIF} // NO_SHLWAPI_STRFCNS
{$IFNDEF NO_SHLWAPI_PATH}
function PathAddBackslashA; external shlwapi32 name 'PathAddBackslashA';
function PathAddBackslashW; external shlwapi32 name 'PathAddBackslashW';
function PathAddBackslash; external shlwapi32 name 'PathAddBackslashA';
function PathAddExtensionA; external shlwapi32 name 'PathAddExtensionA';
function PathAddExtensionW; external shlwapi32 name 'PathAddExtensionW';
function PathAddExtension; external shlwapi32 name 'PathAddExtensionA';
function PathAppendA; external shlwapi32 name 'PathAppendA';
function PathAppendW; external shlwapi32 name 'PathAppendW';
function PathAppend; external shlwapi32 name 'PathAppendA';
function PathBuildRootA; external shlwapi32 name 'PathBuildRootA';
function PathBuildRootW; external shlwapi32 name 'PathBuildRootW';
function PathBuildRoot; external shlwapi32 name 'PathBuildRootA';
function PathCanonicalizeA; external shlwapi32 name 'PathCanonicalizeA';
function PathCanonicalizeW; external shlwapi32 name 'PathCanonicalizeW';
function PathCanonicalize; external shlwapi32 name 'PathCanonicalizeA';
function PathCombineA; external shlwapi32 name 'PathCombineA';
function PathCombineW; external shlwapi32 name 'PathCombineW';
function PathCombine; external shlwapi32 name 'PathCombineA';
function PathCompactPathA; external shlwapi32 name 'PathCompactPathA';
function PathCompactPathW; external shlwapi32 name 'PathCompactPathW';
function PathCompactPath; external shlwapi32 name 'PathCompactPathA';
function PathCompactPathExA; external shlwapi32 name 'PathCompactPathExA';
function PathCompactPathExW; external shlwapi32 name 'PathCompactPathExW';
function PathCompactPathEx; external shlwapi32 name 'PathCompactPathExA';
function PathCommonPrefixA; external shlwapi32 name 'PathCommonPrefixA';
function PathCommonPrefixW; external shlwapi32 name 'PathCommonPrefixW';
function PathCommonPrefix; external shlwapi32 name 'PathCommonPrefixA';
function PathFileExistsA; external shlwapi32 name 'PathFileExistsA';
function PathFileExistsW; external shlwapi32 name 'PathFileExistsW';
function PathFileExists; external shlwapi32 name 'PathFileExistsA';
function PathFindExtensionA; external shlwapi32 name 'PathFindExtensionA';
function PathFindExtensionW; external shlwapi32 name 'PathFindExtensionW';
function PathFindExtension; external shlwapi32 name 'PathFindExtensionA';
function PathFindFileNameA; external shlwapi32 name 'PathFindFileNameA';
function PathFindFileNameW; external shlwapi32 name 'PathFindFileNameW';
function PathFindFileName; external shlwapi32 name 'PathFindFileNameA';
function PathFindNextComponentA; external shlwapi32 name 'PathFindNextComponentA';
function PathFindNextComponentW; external shlwapi32 name 'PathFindNextComponentW';
function PathFindNextComponent; external shlwapi32 name 'PathFindNextComponentA';
function PathFindOnPathA; external shlwapi32 name 'PathFindOnPathA';
function PathFindOnPathW; external shlwapi32 name 'PathFindOnPathW';
function PathFindOnPath; external shlwapi32 name 'PathFindOnPathA';
function PathGetArgsA; external shlwapi32 name 'PathGetArgsA';
function PathGetArgsW; external shlwapi32 name 'PathGetArgsW';
function PathGetArgs; external shlwapi32 name 'PathGetArgsA';

{$IFDEF _WIN32_IE_5}

function PathFindSuffixArrayA; external shlwapi32 name 'PathFindSuffixArrayA';
function PathFindSuffixArrayW; external shlwapi32 name 'PathFindSuffixArrayW';
function PathFindSuffixArray; external shlwapi32 name 'PathFindSuffixArrayA';
function PathIsLFNFileSpecA; external shlwapi32 name 'PathIsLFNFileSpecA';
function PathIsLFNFileSpecW; external shlwapi32 name 'PathIsLFNFileSpecW';
function PathIsLFNFileSpec; external shlwapi32 name 'PathIsLFNFileSpecA';

{$ENDIF}

function PathGetCharTypeA; external shlwapi32 name 'PathGetCharTypeA';
function PathGetCharTypeW; external shlwapi32 name 'PathGetCharTypeW';
function PathGetCharType; external shlwapi32 name 'PathGetCharTypeA';

function PathGetDriveNumberA; external shlwapi32 name 'PathGetDriveNumberA';
function PathGetDriveNumberW; external shlwapi32 name 'PathGetDriveNumberW';
function PathGetDriveNumber; external shlwapi32 name 'PathGetDriveNumberA';
function PathIsDirectoryA; external shlwapi32 name 'PathIsDirectoryA';
function PathIsDirectoryW; external shlwapi32 name 'PathIsDirectoryW';
function PathIsDirectory; external shlwapi32 name 'PathIsDirectoryA';

{$IFDEF _WIN32_IE_5}

function PathIsDirectoryEmptyA; external shlwapi32 name 'PathIsDirectoryEmptyA';
function PathIsDirectoryEmptyW; external shlwapi32 name 'PathIsDirectoryEmptyW';
function PathIsDirectoryEmpty; external shlwapi32 name 'PathIsDirectoryEmptyA';

{$ENDIF}

function PathIsFileSpecA; external shlwapi32 name 'PathIsFileSpecA';
function PathIsFileSpecW; external shlwapi32 name 'PathIsFileSpecW';
function PathIsFileSpec; external shlwapi32 name 'PathIsFileSpecA';
function PathIsPrefixA; external shlwapi32 name 'PathIsPrefixA';
function PathIsPrefixW; external shlwapi32 name 'PathIsPrefixW';
function PathIsPrefix; external shlwapi32 name 'PathIsPrefixA';
function PathIsRelativeA; external shlwapi32 name 'PathIsRelativeA';
function PathIsRelativeW; external shlwapi32 name 'PathIsRelativeW';
function PathIsRelative; external shlwapi32 name 'PathIsRelativeA';
function PathIsRootA; external shlwapi32 name 'PathIsRootA';
function PathIsRootW; external shlwapi32 name 'PathIsRootW';
function PathIsRoot; external shlwapi32 name 'PathIsRootA';
function PathIsSameRootA; external shlwapi32 name 'PathIsSameRootA';
function PathIsSameRootW; external shlwapi32 name 'PathIsSameRootW';
function PathIsSameRoot; external shlwapi32 name 'PathIsSameRootA';
function PathIsUNCA; external shlwapi32 name 'PathIsUNCA';
function PathIsUNCW; external shlwapi32 name 'PathIsUNCW';
function PathIsUNC; external shlwapi32 name 'PathIsUNCA';

{$IFDEF _WIN32_IE_5}

function PathIsNetworkPathA; external shlwapi32 name 'PathIsNetworkPathA';
function PathIsNetworkPathW; external shlwapi32 name 'PathIsNetworkPathW';
function PathIsNetworkPath; external shlwapi32 name 'PathIsNetworkPathA';

{$ENDIF}

function PathIsUNCServerA; external shlwapi32 name 'PathIsUNCServerA';
function PathIsUNCServerW; external shlwapi32 name 'PathIsUNCServerW';
function PathIsUNCServer; external shlwapi32 name 'PathIsUNCServerA';
function PathIsUNCServerShareA; external shlwapi32 name 'PathIsUNCServerShareA';
function PathIsUNCServerShareW; external shlwapi32 name 'PathIsUNCServerShareW';
function PathIsUNCServerShare; external shlwapi32 name 'PathIsUNCServerShareA';
function PathIsContentTypeA; external shlwapi32 name 'PathIsContentTypeA';
function PathIsContentTypeW; external shlwapi32 name 'PathIsContentTypeW';
function PathIsContentType; external shlwapi32 name 'PathIsContentTypeA';
function PathIsURLA; external shlwapi32 name 'PathIsURLA';
function PathIsURLW; external shlwapi32 name 'PathIsURLW';
function PathIsURL; external shlwapi32 name 'PathIsURLA';
function PathMakePrettyA; external shlwapi32 name 'PathMakePrettyA';
function PathMakePrettyW; external shlwapi32 name 'PathMakePrettyW';
function PathMakePretty; external shlwapi32 name 'PathMakePrettyA';
function PathMatchSpecA; external shlwapi32 name 'PathMatchSpecA';
function PathMatchSpecW; external shlwapi32 name 'PathMatchSpecW';
function PathMatchSpec; external shlwapi32 name 'PathMatchSpecA';
function PathParseIconLocationA; external shlwapi32 name 'PathParseIconLocationA';
function PathParseIconLocationW; external shlwapi32 name 'PathParseIconLocationW';
function PathParseIconLocation; external shlwapi32 name 'PathParseIconLocationA';
procedure PathQuoteSpacesA; external shlwapi32 name 'PathQuoteSpacesA';
procedure PathQuoteSpacesW; external shlwapi32 name 'PathQuoteSpacesW';
procedure PathQuoteSpaces; external shlwapi32 name 'PathQuoteSpacesA';
function PathRelativePathToA; external shlwapi32 name 'PathRelativePathToA';
function PathRelativePathToW; external shlwapi32 name 'PathRelativePathToW';
function PathRelativePathTo; external shlwapi32 name 'PathRelativePathToA';
procedure PathRemoveArgsA; external shlwapi32 name 'PathRemoveArgsA';
procedure PathRemoveArgsW; external shlwapi32 name 'PathRemoveArgsW';
procedure PathRemoveArgs; external shlwapi32 name 'PathRemoveArgsA';
function PathRemoveBackslashA; external shlwapi32 name 'PathRemoveBackslashA';
function PathRemoveBackslashW; external shlwapi32 name 'PathRemoveBackslashW';
function PathRemoveBackslash; external shlwapi32 name 'PathRemoveBackslashA';
procedure PathRemoveBlanksA; external shlwapi32 name 'PathRemoveBlanksA';
procedure PathRemoveBlanksW; external shlwapi32 name 'PathRemoveBlanksW';
procedure PathRemoveBlanks; external shlwapi32 name 'PathRemoveBlanksA';
procedure PathRemoveExtensionA; external shlwapi32 name 'PathRemoveExtensionA';
procedure PathRemoveExtensionW; external shlwapi32 name 'PathRemoveExtensionW';
procedure PathRemoveExtension; external shlwapi32 name 'PathRemoveExtensionA';
function PathRemoveFileSpecA; external shlwapi32 name 'PathRemoveFileSpecA';
function PathRemoveFileSpecW; external shlwapi32 name 'PathRemoveFileSpecW';
function PathRemoveFileSpec; external shlwapi32 name 'PathRemoveFileSpecA';
function PathRenameExtensionA; external shlwapi32 name 'PathRenameExtensionA';
function PathRenameExtensionW; external shlwapi32 name 'PathRenameExtensionW';
function PathRenameExtension; external shlwapi32 name 'PathRenameExtensionA';
function PathSearchAndQualifyA; external shlwapi32 name 'PathSearchAndQualifyA';
function PathSearchAndQualifyW; external shlwapi32 name 'PathSearchAndQualifyW';
function PathSearchAndQualify; external shlwapi32 name 'PathSearchAndQualifyA';
procedure PathSetDlgItemPathA; external shlwapi32 name 'PathSetDlgItemPathA';
procedure PathSetDlgItemPathW; external shlwapi32 name 'PathSetDlgItemPathW';
procedure PathSetDlgItemPath; external shlwapi32 name 'PathSetDlgItemPathA';
function PathSkipRootA; external shlwapi32 name 'PathSkipRootA';
function PathSkipRootW; external shlwapi32 name 'PathSkipRootW';
function PathSkipRoot; external shlwapi32 name 'PathSkipRootA';
procedure PathStripPathA; external shlwapi32 name 'PathStripPathA';
procedure PathStripPathW; external shlwapi32 name 'PathStripPathW';
procedure PathStripPath; external shlwapi32 name 'PathStripPathA';
function PathStripToRootA; external shlwapi32 name 'PathStripToRootA';
function PathStripToRootW; external shlwapi32 name 'PathStripToRootW';
function PathStripToRoot; external shlwapi32 name 'PathStripToRootA';
procedure PathUnquoteSpacesA; external shlwapi32 name 'PathUnquoteSpacesA';
procedure PathUnquoteSpacesW; external shlwapi32 name 'PathUnquoteSpacesW';
procedure PathUnquoteSpaces; external shlwapi32 name 'PathUnquoteSpacesA';
function PathMakeSystemFolderA; external shlwapi32 name 'PathMakeSystemFolderA';
function PathMakeSystemFolderW; external shlwapi32 name 'PathMakeSystemFolderW';
function PathMakeSystemFolder; external shlwapi32 name 'PathMakeSystemFolderA';
function PathUnmakeSystemFolderA; external shlwapi32 name 'PathUnmakeSystemFolderA';
function PathUnmakeSystemFolderW; external shlwapi32 name 'PathUnmakeSystemFolderW';
function PathUnmakeSystemFolder; external shlwapi32 name 'PathUnmakeSystemFolderA';
function PathIsSystemFolderA; external shlwapi32 name 'PathIsSystemFolderA';
function PathIsSystemFolderW; external shlwapi32 name 'PathIsSystemFolderW';
function PathIsSystemFolder; external shlwapi32 name 'PathIsSystemFolderA';

{$IFDEF _WIN32_IE_5}

procedure PathUndecorateA; external shlwapi32 name 'PathUndecorateA';
procedure PathUndecorateW; external shlwapi32 name 'PathUndecorateW';
procedure PathUndecorate; external shlwapi32 name 'PathUndecorateA';
function PathUnExpandEnvStringsA; external shlwapi32 name 'PathUnExpandEnvStringsA';
function PathUnExpandEnvStringsW; external shlwapi32 name 'PathUnExpandEnvStringsW';
function PathUnExpandEnvStrings; external shlwapi32 name 'PathUnExpandEnvStringsA';

function UrlCompareA; external shlwapi32 name 'UrlCompareA';
function UrlCompareW; external shlwapi32 name 'UrlCompareW';
function UrlCompare; external shlwapi32 name 'UrlCompareA';
function UrlCombineA; external shlwapi32 name 'UrlCombineA';
function UrlCombineW; external shlwapi32 name 'UrlCombineW';
function UrlCombine; external shlwapi32 name 'UrlCombineA';
function UrlCanonicalizeA; external shlwapi32 name 'UrlCanonicalizeA';
function UrlCanonicalizeW; external shlwapi32 name 'UrlCanonicalizeW';
function UrlCanonicalize; external shlwapi32 name 'UrlCanonicalizeA';
function UrlIsOpaqueA; external shlwapi32 name 'UrlIsOpaqueA';
function UrlIsOpaqueW; external shlwapi32 name 'UrlIsOpaqueW';
function UrlIsOpaque; external shlwapi32 name 'UrlIsOpaqueA';
function UrlIsNoHistoryA; external shlwapi32 name 'UrlIsNoHistoryA';
function UrlIsNoHistoryW; external shlwapi32 name 'UrlIsNoHistoryW';
function UrlIsNoHistory; external shlwapi32 name 'UrlIsNoHistoryA';

function UrlIsFileUrlA(pszURL: PAnsiChar): BOOL;
begin
  Result := UrlIsA(pszURL, URLIS_FILEURL)
end;
function UrlIsFileUrlW(pszURL: PWideChar): BOOL;
begin
  Result := UrlIsW(pszURL, URLIS_FILEURL)
end;
function UrlIsFileUrl(pszURL: PChar): BOOL;
begin
  Result := UrlIs(pszURL, URLIS_FILEURL)
end;

function UrlIsA; external shlwapi32 name 'UrlIsA';
function UrlIsW; external shlwapi32 name 'UrlIsW';
function UrlIs; external shlwapi32 name 'UrlIsA';
function UrlGetLocationA; external shlwapi32 name 'UrlGetLocationA';
function UrlGetLocationW; external shlwapi32 name 'UrlGetLocationW';
function UrlGetLocation; external shlwapi32 name 'UrlGetLocationA';
function UrlUnescapeA; external shlwapi32 name 'UrlUnescapeA';
function UrlUnescapeW; external shlwapi32 name 'UrlUnescapeW';
function UrlUnescape; external shlwapi32 name 'UrlUnescapeA';
function UrlEscapeA; external shlwapi32 name 'UrlEscapeA';
function UrlEscapeW; external shlwapi32 name 'UrlEscapeW';
function UrlEscape; external shlwapi32 name 'UrlEscapeA';
function UrlCreateFromPathA; external shlwapi32 name 'UrlCreateFromPathA';
function UrlCreateFromPathW; external shlwapi32 name 'UrlCreateFromPathW';
function UrlCreateFromPath; external shlwapi32 name 'UrlCreateFromPathA';
function PathCreateFromUrlA; external shlwapi32 name 'PathCreateFromUrlA';
function PathCreateFromUrlW; external shlwapi32 name 'PathCreateFromUrlW';
function PathCreateFromUrl; external shlwapi32 name 'PathCreateFromUrlA';
function UrlHashA; external shlwapi32 name 'UrlHashA';
function UrlHashW; external shlwapi32 name 'UrlHashW';
function UrlHash; external shlwapi32 name 'UrlHashA';
function UrlGetPartA; external shlwapi32 name 'UrlGetPartA';
function UrlGetPartW; external shlwapi32 name 'UrlGetPartW';
function UrlGetPart; external shlwapi32 name 'UrlGetPartA';
function UrlApplySchemeA; external shlwapi32 name 'UrlApplySchemeA';
function UrlApplySchemeW; external shlwapi32 name 'UrlApplySchemeW';
function UrlApplyScheme; external shlwapi32 name 'UrlApplySchemeA';
function HashData; external shlwapi32 name 'HashData';


function UrlEscapeSpacesA(pszUrl: PAnsiChar; pszEscaped: PAnsiChar; pcchEscaped: LPDWORD): HRESULT; stdcall;
begin
  Result := UrlCanonicalizeA(pszUrl, pszEscaped, pcchEscaped, URL_ESCAPE_SPACES_ONLY or URL_DONT_ESCAPE_EXTRA_INFO);
end;

function UrlEscapeSpacesW(pszUrl: PWideChar; pszEscaped: PWideChar; pcchEscaped: LPDWORD): HRESULT; stdcall;
begin
  Result := UrlCanonicalizeW(pszUrl, pszEscaped, pcchEscaped, URL_ESCAPE_SPACES_ONLY or URL_DONT_ESCAPE_EXTRA_INFO);
end;

function UrlEscapeSpaces(pszUrl: PChar; pszEscaped: PChar; pcchEscaped: LPDWORD): HRESULT; stdcall;
begin
  Result := UrlCanonicalize(pszUrl, pszEscaped, pcchEscaped, URL_ESCAPE_SPACES_ONLY or URL_DONT_ESCAPE_EXTRA_INFO);
end;

function UrlUnescapeInPlaceA(pszUrl: PAnsiChar; dwFlags: DWORD): HRESULT;
begin
  Result := UrlUnescapeA(pszUrl, nil, nil, dwFlags or URL_UNESCAPE_INPLACE);
end;

function UrlUnescapeInPlaceW(pszUrl: PWideChar; dwFlags: DWORD): HRESULT;
begin
  Result := UrlUnescapeW(pszUrl, nil, nil, dwFlags or URL_UNESCAPE_INPLACE);
end;

function UrlUnescapeInPlace(pszUrl: PChar; dwFlags: DWORD): HRESULT;
begin
  Result := UrlUnescape(pszUrl, nil, nil, dwFlags or URL_UNESCAPE_INPLACE);
end;

{$ENDIF} //  IE 5
{$ENDIF} //  NO_SHLWAPI_PATH

{$IFNDEF NO_SHLWAPI_REG}
function SHDeleteEmptyKeyA; external shlwapi32 name 'SHDeleteEmptyKeyAA';
function SHDeleteEmptyKeyW; external shlwapi32 name 'SHDeleteEmptyKeyWW';
function SHDeleteEmptyKey; external shlwapi32 name 'SHDeleteEmptyKeyA';
function SHDeleteKeyA; external shlwapi32 name 'SHDeleteKeyA';
function SHDeleteKeyW; external shlwapi32 name 'SHDeleteKeyW';
function SHDeleteKey; external shlwapi32 name 'SHDeleteKeyA';

function SHDeleteValueA; external shlwapi32 name 'SHDeleteValueA';
function SHDeleteValueW; external shlwapi32 name 'SHDeleteValueW';
function SHDeleteValue; external shlwapi32 name 'SHDeleteValueA';
function SHGetValueA; external shlwapi32 name 'SHGetValueA';
function SHGetValueW; external shlwapi32 name 'SHGetValueW';
function SHGetValue; external shlwapi32 name 'SHGetValueA';
function SHSetValueA; external shlwapi32 name 'SHSetValueA';
function SHSetValueW; external shlwapi32 name 'SHSetValueW';
function SHSetValue; external shlwapi32 name 'SHSetValueA';

function SHQueryValueExA; external shlwapi32 name 'SHQueryValueExA';
function SHQueryValueExW; external shlwapi32 name 'SHQueryValueExW';
function SHQueryValueEx; external shlwapi32 name 'SHQueryValueExA';

function SHEnumKeyExA; external shlwapi32 name 'SHEnumKeyExA';
function SHEnumKeyExW; external shlwapi32 name 'SHEnumKeyExW';
function SHEnumKeyEx; external shlwapi32 name 'SHEnumKeyExA';
function SHEnumValueA; external shlwapi32 name 'SHEnumValueA';
function SHEnumValueW; external shlwapi32 name 'SHEnumValueW';
function SHEnumValue; external shlwapi32 name 'SHEnumValueA';
function SHQueryInfoKeyA; external shlwapi32 name 'SHQueryInfoKeyA';
function SHQueryInfoKeyW; external shlwapi32 name 'SHQueryInfoKeyW';
function SHQueryInfoKey; external shlwapi32 name 'SHQueryInfoKeyA';

{$IFDEF _WIN32_IE_5}

function SHCopyKeyA; external shlwapi32 name 'SHCopyKeyA';
function SHCopyKeyW; external shlwapi32 name 'SHCopyKeyW';
function SHCopyKey; external shlwapi32 name 'SHCopyKeyA';

function SHRegGetPathA; external shlwapi32 name 'SHRegGetPathA';
function SHRegGetPathW; external shlwapi32 name 'SHRegGetPathW';
function SHRegGetPath; external shlwapi32 name 'SHRegGetPathA';
function SHRegSetPathA; external shlwapi32 name 'SHRegSetPathA';
function SHRegSetPathW; external shlwapi32 name 'SHRegSetPathW';
function SHRegSetPath; external shlwapi32 name 'SHRegSetPathA';

{$ENDIF}

function SHRegCreateUSKeyA; external shlwapi32 name 'SHRegCreateUSKeyA';
function SHRegCreateUSKeyW; external shlwapi32 name 'SHRegCreateUSKeyW';
function SHRegCreateUSKey; external shlwapi32 name 'SHRegCreateUSKeyA';
function SHRegOpenUSKeyA; external shlwapi32 name 'SHRegOpenUSKeyA';
function SHRegOpenUSKeyW; external shlwapi32 name 'SHRegOpenUSKeyW';
function SHRegOpenUSKey; external shlwapi32 name 'SHRegOpenUSKeyA';
function SHRegQueryUSValueA; external shlwapi32 name 'SHRegQueryUSValueA';
function SHRegQueryUSValueW; external shlwapi32 name 'SHRegQueryUSValueW';
function SHRegQueryUSValue; external shlwapi32 name 'SHRegQueryUSValueA';
function SHRegWriteUSValueA; external shlwapi32 name 'SHRegWriteUSValueA';
function SHRegWriteUSValueW; external shlwapi32 name 'SHRegWriteUSValueW';
function SHRegWriteUSValue; external shlwapi32 name 'SHRegWriteUSValueA';
function SHRegDeleteUSValueA; external shlwapi32 name 'SHRegDeleteUSValueA';
function SHRegDeleteUSValueW; external shlwapi32 name 'SHRegDeleteUSValueW';
function SHRegDeleteUSValue; external shlwapi32 name 'SHRegDeleteUSValueA';
function SHRegDeleteEmptyUSKeyA; external shlwapi32 name 'SHRegDeleteEmptyUSKeyA';
function SHRegDeleteEmptyUSKeyW; external shlwapi32 name 'SHRegDeleteEmptyUSKeyW';
function SHRegDeleteEmptyUSKey; external shlwapi32 name 'SHRegDeleteEmptyUSKeyA';
function SHRegEnumUSKeyA; external shlwapi32 name 'SHRegEnumUSKeyA';
function SHRegEnumUSKeyW; external shlwapi32 name 'SHRegEnumUSKeyW';
function SHRegEnumUSKey; external shlwapi32 name 'SHRegEnumUSKeyA';
function SHRegEnumUSValueA; external shlwapi32 name 'SHRegEnumUSValueA';
function SHRegEnumUSValueW; external shlwapi32 name 'SHRegEnumUSValueW';
function SHRegEnumUSValue; external shlwapi32 name 'SHRegEnumUSValueA';
function SHRegQueryInfoUSKeyA; external shlwapi32 name 'SHRegQueryInfoUSKeyA';
function SHRegQueryInfoUSKeyW; external shlwapi32 name 'SHRegQueryInfoUSKeyW';
function SHRegQueryInfoUSKey; external shlwapi32 name 'SHRegQueryInfoUSKeyA';
function SHRegCloseUSKey; external shlwapi32 name 'SHRegCloseUSKey';

function SHRegGetUSValueA; external shlwapi32 name 'SHRegGetUSValueA';
function SHRegGetUSValueW; external shlwapi32 name 'SHRegGetUSValueW';
function SHRegGetUSValue; external shlwapi32 name 'SHRegGetUSValueA';
function SHRegSetUSValueA; external shlwapi32 name 'SHRegSetUSValueA';
function SHRegSetUSValueW; external shlwapi32 name 'SHRegSetUSValueW';
function SHRegSetUSValue; external shlwapi32 name 'SHRegSetUSValueA';

function SHRegGetBoolUSValueA; external shlwapi32 name 'SHRegGetBoolUSValueA';
function SHRegGetBoolUSValueW; external shlwapi32 name 'SHRegGetBoolUSValueW';
function SHRegGetBoolUSValue; external shlwapi32 name 'SHRegGetBoolUSValueA';

{$IFDEF _WIN32_IE_5}

function SHRegGetIntW; external shlwapi32 name 'SHRegGetIntW';

function AssocCreate; external shlwapi32 name 'AssocCreate';

function AssocQueryStringA; external shlwapi32 name 'AssocQueryStringA';
function AssocQueryStringW; external shlwapi32 name 'AssocQueryStringW';
function AssocQueryString; external shlwapi32 name 'AssocQueryStringA';
function AssocQueryStringByKeyA; external shlwapi32 name 'AssocQueryStringByKeyA';
function AssocQueryStringByKeyW; external shlwapi32 name 'AssocQueryStringByKeyW';
function AssocQueryStringByKey; external shlwapi32 name 'AssocQueryStringByKeyA';
function AssocQueryKeyA; external shlwapi32 name 'AssocQueryKeyA';
function AssocQueryKeyW; external shlwapi32 name 'AssocQueryKeyW';
function AssocQueryKey; external shlwapi32 name 'AssocQueryKeyA';

{$ENDIF} //  IE 5
{$ENDIF} //  NO_SHLWAPI_REG

{$IFNDEF NO_SHLWAPI_STREAM}
function SHOpenRegStreamA; external shlwapi32 name 'SHOpenRegStreamA';
function SHOpenRegStreamW; external shlwapi32 name 'SHOpenRegStreamW';
function SHOpenRegStream; external shlwapi32 name 'SHOpenRegStreamA';

{$IFDEF _WIN32_IE_5}

function SHOpenRegStream2A; external shlwapi32 name 'SHOpenRegStream2A';
function SHOpenRegStream2W; external shlwapi32 name 'SHOpenRegStream2W';
function SHOpenRegStream2; external shlwapi32 name 'SHOpenRegStream2A';

function SHCreateStreamOnFileA; external shlwapi32 name 'SHCreateStreamOnFileA';
function SHCreateStreamOnFileW; external shlwapi32 name 'SHCreateStreamOnFileW';
function SHCreateStreamOnFile; external shlwapi32 name 'SHCreateStreamOnFileA';

{$ENDIF} // IE 5
{$ENDIF} // NO_SHLWAPI_STREAM

{$IFDEF _WIN32_IE_5}
function SHAutoComplete; external shlwapi32 name 'SHAutoComplete';

procedure SHSetThreadRef; external shlwapi32 name 'SHSetThreadRef';
procedure SHGetThreadRef; external shlwapi32 name 'SHGetThreadRef';

function SHSkipJunction; external shlwapi32 name 'SHSkipJunction';

{$ENDIF}

{$IFDEF _WIN32_IE_5}

function SHCreateThread; external shlwapi32 name 'SHCreateThread';

{$ENDIF} // IE 5

{$IFNDEF NO_SHLWAPI_GDI}
function SHCreateShellPalette; external shlwapi32 name 'SHCreateShellPalette';

{$IFDEF _WIN32_IE_5}
procedure ColorRGBToHLS; external shlwapi32 name 'ColorRGBToHLS';
function ColorHLSToRGB; external shlwapi32 name 'ColorHLSToRGB';
function ColorAdjustLuma; external shlwapi32 name 'ColorAdjustLuma';
{$ENDIF} // _WIN32_IE >= $0500
{$ENDIF} // NO_SHLWAPI_GDI

function MAKEDLLVERULL(Major, Minor, Build, Qfe: Word): Int64;
begin
  Result := (Int64(Major) shl 48) or (Minor shl 32) or (Build shl 16) or Qfe;
end;

end.
