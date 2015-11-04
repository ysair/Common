unit uWinFunc;

interface

uses
  Windows, SysUtils, ActiveX, ShlObj, ShellAPI;

function  FileTimeToDateTime(Fd : TFileTime):TDateTime;
function  DateTimeToFileTime(Dt : TDateTime):TFileTime;
function  GetExeFileName: string;
function  GetFileCreateTime(const AFileName  : string):TDateTime;
function  GetFileModifyTime(const AFileName  : string):TDateTime;
function  GetFileSize(const FileName: string): Int64;
function  GetGUID: string;
function  GetInstanceFileName: string;
function  GetInstancePath: string;
function  GetLastErrorText: string;
function  GetTempFileName(const Dir  : string): string; overload;
function  GetTempFileName: string; overload;
function  SetFileCreateTime(FileName : string; FileTime : TDateTime) : boolean;
function  SetFileModifyTime(FileName : string; FileTime : TDateTime) : boolean;
function  ShortNameToLongName(const FileName: string): string;
function  LongNameToShortName(const FileName: string): string;
function  GetWindowsDirectory : string;
function  ExpandEnvironmentStrings(const AStr : string):string;
function  CurrentUser_IsAdmin: Boolean;
function  UAC_IsAdmin: Boolean;
procedure UAC_ButtonAddIcon(const AHandle : HWND);
function  AppRunAs(const AExeFile : string; const AParam : string): boolean;

implementation

var
  _Kernel32Handle: HMODULE = HMODULE(0);
  _GetLongPathName: function (lpszShortPath: PChar; lpszLongPath: PChar;
      cchBuffer: DWORD): DWORD; stdcall;

const
  DefTempFilePrefix = '~tp';
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;

function Kernel32Handle: HMODULE;
begin
  if _Kernel32Handle = HMODULE(0) then
    _Kernel32Handle := LoadLibrary(kernel32);
  Result := _Kernel32Handle;
end;

function GetLastErrorText: string;
begin
  Result  :=  SysErrorMessage(GetLastError);
end;

function GetInstanceFileName: string;
var
  pPath : array[0..MAX_PATH] of char;
begin
  GetModuleFileName(HInstance, pPath, MAX_PATH);
  Result  :=  pPath;//ShortNameToLongName(pPath);
end;

function  GetInstancePath: string;
begin
  Result  :=  ExtractFilePath(GetInstanceFileName);
end;

function GetExeFileName: string;
var
  pPath : array[0..MAX_PATH] of char;
begin
  GetModuleFileName(0, pPath, MAX_PATH);
  Result  :=  pPath;
end;

function GetGUID: string;
begin
  Result := Win.GetGUID;
end;

function GetFileCreateTime(const AFileName  : string):TDateTime;
var
  FileTime: TFileTime;
  hFile: THandle;
begin
  Result := 0;
  FileTime.dwLowDateTime := 0;
  FileTime.dwHighDateTime := 0;
  hFile := FileOpen(AFileName, fmShareDenyNone);
  if hFile <> INVALID_HANDLE_VALUE then
  try
    if Windows.GetFileTime(hFile, @FileTime, nil, nil) then
      result  :=  FileTimeToDateTime(FileTime);
  finally
    FileClose(hFile);
  end;
end;

function GetFileModifyTime(const AFileName  : string):TDateTime;
var
  FileTime: TFileTime;
  hFile: THandle;
begin
  Result := 0;
  FileTime.dwLowDateTime := 0;
  FileTime.dwHighDateTime := 0;
  hFile := FileOpen(AFileName, fmShareDenyNone);
  if hFile <> INVALID_HANDLE_VALUE then
  try
    if Windows.GetFileTime(hFile, nil, nil, @FileTime) then
      result  :=  FileTimeToDateTime(FileTime);
  finally
    FileClose(hFile);
  end;
end;

function SetFileCreateTime(FileName : string; FileTime : TDateTime) : Boolean;
var
  FileHnd  : Integer;
  fTime : TFileTime;
begin
  Result  :=  False;
  fTime :=  DateTimeToFileTime(FileTime);
  FileHnd := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if FileHnd <> Integer(INVALID_HANDLE_VALUE) then
  try
    Result  :=  windows.SetFileTime(FileHnd, @fTime, nil, nil);
  finally
    FileClose(FileHnd);
  end;
end;

function SetFileModifyTime(FileName : string; FileTime : TDateTime) : boolean;
var
  FileHnd  : Integer;
  fTime : TFileTime;
begin
  Result  :=  False;
  fTime :=  DateTimeToFileTime(FileTime);
  FileHnd := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if FileHnd <> Integer(INVALID_HANDLE_VALUE) then
  try
    Result  :=  windows.SetFileTime(FileHnd, nil, nil, @fTime);
  finally
    FileClose(FileHnd);
  end;
end;

function FileTimeToDateTime(Fd  : TFileTime):TDateTime;
var
  Tct:_SystemTime;
  Temp:_FileTime;
begin
  FileTimeToLocalFileTime(Fd,Temp);
  FileTimeToSystemTime(Temp, Tct);
  result  :=  SystemTimeToDateTime(Tct);
end;

function DateTimeToFileTime(Dt : TDateTime):TFileTime;
var
  Tct   : _SystemTime;
  Temp  : _FileTime;
begin
  DateTimeToSystemTime(dt, Tct);
  SystemTimeToFileTime(Tct, Temp);
  LocalFileTimeToFileTime(Temp, Result);
end;

function GetFileSize(const FileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    SysUtils.FindClose(SearchRec);
  end
  else Result := -1;
end;

function GetTempFileName: string;
//取得临时文件名
var
  SPath, SFile : array [0..MAX_PATH] of char;
begin
  windows.GetTempPath(MAX_PATH, SPath);
  windows.GetTempFileName(SPath, PChar(DefTempFilePrefix), 0, SFile);
  result := SFile;
  //SysUtils.DeleteFile(result);
end;

function GetTempFileName(const Dir: string): string;
var
  sFile : array[0..MAX_PATH] of char;
begin
  SysUtils.ForceDirectories(Dir);
  windows.GetTempFileName(PChar(Dir), PChar(DefTempFilePrefix), 0, sFile);
  result  :=  sFile;
end;

function ShellGetLongPathName(const Path: string): string;
var
  PIDL: PItemIDList;
  Desktop: IShellFolder;
  AnsiName: string;
  WideName: array [0..MAX_PATH] of WideChar;
  Eaten, Attr: ULONG;
begin
  Result := Path;
  if Path <> '' then
  begin
    if Succeeded(SHGetDesktopFolder(Desktop)) then
    begin
      MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PAnsiChar(AnsiString(Path)), -1, WideName, MAX_PATH);
      if Succeeded(Desktop.ParseDisplayName(0, nil, WideName, Eaten, PIDL, Attr)) then
      try
        SetLength(AnsiName, MAX_PATH);
        if SHGetPathFromIDList(PIDL, PChar(AnsiName)) then
          SetLength(AnsiName, StrLen(PChar(AnsiName)));
        Result := AnsiName;
      finally
        CoTaskMemFree(PIDL);
      end;
    end;
  end;
end;

// 短文件名转长文件名  by cnPack
function ShortNameToLongName(const FileName: string): string;
const
{$IFDEF DELPHI2009_UP}
  SCnGetLongPathName = 'GetLongPathNameW';
{$ELSE}
  SCnGetLongPathName = 'GetLongPathNameA';
{$ENDIF}
begin
  Result := FileName;
  if not Assigned(_GetLongPathName) then
    _GetLongPathName := GetProcAddress(Kernel32Handle, SCnGetLongPathName);
  if Assigned(_GetLongPathName) then
  begin
    SetLength(Result, MAX_PATH);
    SetLength(Result, _GetLongPathName(PChar(FileName), PChar(Result), MAX_PATH));
  end
  else
  begin
    Result := ShellGetLongPathName(FileName);
  end;
end;

// 长文件名转短文件名  by cnPack
function LongNameToShortName(const FileName: string): string;
var
  Buf: PChar;
  BufSize: Integer;
begin
  BufSize := GetShortPathName(PChar(FileName), nil, 0) + 1;
  GetMem(Buf, BufSize);
  try
    GetShortPathName(PChar(FileName), Buf, BufSize);
    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

function  GetWindowsDirectory : string;
begin
  Result := Win.GetWindowsDirectory;
end;

function ExpandEnvironmentStrings(const AStr : string):string;
var
  buf : Pointer;
  size : DWORD;
begin
  size :=  windows.ExpandEnvironmentStrings(PChar(AStr), nil, 0);
  GetMem(buf, size);
  try
    windows.ExpandEnvironmentStrings(PChar(AStr), Buf, size);
    Result  :=  StrPas(PChar(buf));
  finally
    FreeMem(buf, Size);
  end;
end;

function CurrentUser_IsAdmin: Boolean;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
    hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
        hAccessToken);
  end;
  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
      ptgGroups, 1024, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, psidAdministrators);
{$R-}
      for x := 0 to ptgGroups.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          Break;
        end;
{$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

function UAC_IsAdmin: Boolean;
const
  TokenElevationType = 18;
  TokenElevation = 20;
  TokenElevationTypeDefault = 1;
  TokenElevationTypeFull = 2;
  TokenElevationTypeLimited = 3;
var
  token: Cardinal;
  Elevation: DWord;
  dwSize: Cardinal;
begin
  result  :=  false;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
  try
    if GetTokenInformation(token, TTokenInformationClass(TokenElevation), @Elevation, SizeOf(Elevation), dwSize) then
    begin
      result  := Elevation <> 0;
    end;
  finally
    CloseHandle(token);
  end;
end;

procedure UAC_ButtonAddIcon(const AHandle : HWND);
const
  BCM_SETSHIELD = $160C;
begin
  SendMessage(AHandle, BCM_SETSHIELD, 0, 1);
end;

function AppRunAs(const AExeFile : string; const AParam : string): boolean;
var
  si: SHELLEXECUTEINFO;
begin
  zeromemory(@si, sizeof(SHELLEXECUTEINFO));
  si.cbSize := sizeof(SHELLEXECUTEINFO);
  si.lpVerb := 'runas';
  si.lpFile := PChar(AExeFile);
  si.nShow := SW_SHOW;
  si.fMask := SEE_MASK_NOCLOSEPROCESS;
  si.lpParameters := PChar(AParam);
  result := ShellExecuteEx(@si);
end;

end.
