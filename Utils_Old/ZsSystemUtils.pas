unit ZsSystemUtils;

interface

uses
  Types, Windows, Messages, ShellAPI, TlHelp32, fcWindows;

  // ========== win 7 下拽拉文件用到的 ==============
var
  ChangeWindowMessageFilter: function(msg: Cardinal; dwFlag: Word): BOOL;
stdcall;

const
  WM_COPYGLOBALDATA = 73;
  MSGFLT_ADD = 1;
  // ================================================

procedure Delay(lMilliSeconds: Dword);

function PressKey(const AKey: LongInt): Boolean;

function PressShift: Boolean;

function PressCtrl: Boolean;

function GetDefaultExploreProgram: String;

// 这个函数起什么作用我不清楚,你能否说明一下?
procedure ResetScreenCursor;

Function ReadRegString(MainKey: HKey; SubKey, ValName: String): String;

Procedure WriteRegString(MainKey: HKey; SubKey, ValName: String; const Data: String);

Procedure DeleteRegValue(MainKey: HKey; SubKey, ValName: String);

function GetLocalComputerName: string;

function GetErrorText(const ACode: Integer): string;

function GetLastErrorText: string;

function GetWinVersion: TWinVersion;

procedure SetWin7VistaDragFiles;

// xp下，当前的用户是否有系统管理员权限
function WinXp_IsAdmin: Boolean;

// 判断当前的windows的用户是否有提升到管理员权限
function Vista_HasAdminAccess: Boolean; // 只能Vista win7下使用

function ShellExecuteWithAdmin(AAppName: string; AParams: string;
  const ShowCmd: Integer = SW_SHOW): Boolean;

function InstalledDirNeedUAC: Boolean;

function IsUACEnable: Boolean;

function DirectoryIsNeeddUAC(ADir: string; const ACheckUACEnable: Boolean = true): Boolean;
// ADir 用ExtractFilePath返回的，以“\”结尾

function GetSystemFolder(AFolderType: TSystemFolderType): String;

function GetSystemFolderDir(mFolder: Integer): string;

function GetWindowsTempDirectory: string;

function GetRandomFileName(const InitDir: String): String;

implementation

uses
  Shlobj, Forms, Registry, Sysutils, ZsStringUtils, ZsRegistryUtils;

function GetLocalComputerName: string;
var
  computername: pchar;
  size: Cardinal;
begin
  size := MAX_COMPUTERNAME_LENGTH + 1;
  getmem(computername, size);
  try
    if getcomputername(computername, size) then
      result := strpas(computername)
    else
      result := '';
  finally
    freemem(computername);
  end;
end;

function GetWinVersion: TWinVersion;
var
  osVerInfo: TOSVersionInfo;
  majorVersion, minorVersion: Integer;
begin
  result := wvUnknown;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    minorVersion := osVerInfo.dwMinorVersion;
    majorVersion := osVerInfo.dwMajorVersion;
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
        begin
          if majorVersion <= 4 then
            result := wvWinNT
          else if (majorVersion = 5) and (minorVersion = 0) then
            result := wvWin2000
          else if (majorVersion = 5) and (minorVersion = 1) then
            result := wvWinXP
          else if (majorVersion = 6) and (minorVersion = 0) then
            result := wvVista
          else if (majorVersion = 6) and (minorVersion = 1) then
            result := wvWin7
          else if majorVersion = 7 then
            result := wvWin7Later;
        end;
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          if (majorVersion = 4) and (minorVersion = 0) then
            result := wvWin95
          else if (majorVersion = 4) and (minorVersion = 10) then
          begin
            if osVerInfo.szCSDVersion[1] = 'A' then
              result := wvWin98SE
            else
              result := wvWin98;
          end
          else if (majorVersion = 4) and (minorVersion = 90) then
            result := wvWinME
          else
            result := wvUnknown;
        end;
    end;
  end;
end;

procedure SetWin7VistaDragFiles;
begin
  if GetWinVersion in [wvWin7, wvVista, wvWin7Later] then
  begin
    @ChangeWindowMessageFilter := GetProcAddress(LoadLibrary('user32.dll'),
      'ChangeWindowMessageFilter');
    if @ChangeWindowMessageFilter <> nil then // je?li mamy adres, oznacza, ?e program uruchomiono pod systemem Widndows 7 lub pod Vist?
      try
        ChangeWindowMessageFilter(WM_COPYGLOBALDATA, MSGFLT_ADD);
        ChangeWindowMessageFilter(WM_DROPFILES, MSGFLT_ADD);
      except
      end;
  end;
end;

// ------------------------------------------------------------------------------
Function ReadRegString(MainKey: HKey; SubKey, ValName: String): String;
// NB default value is read if subkey isent ended with a backslash
Var
  Key: HKey;
  C: Array [0 .. 1023] of Char;
  D: Cardinal; // value type
  D2: Cardinal; // buffer size
Begin
  // asm int 3 end; //trap
  result := '';

  if RegOpenKeyEx(MainKey, pchar(SubKey), 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    try
      C := '';
      D2 := SizeOf(C);
      if (RegQueryValueEx(Key, pchar(ValName), Nil, @D, @C, @D2) = ERROR_SUCCESS) and
        ((D = REG_EXPAND_SZ) or (D = REG_SZ)) then
        result := C
      else
        result := '';
    finally
      RegCloseKey(Key);
    end;
  end;
End;

// ------------------------------------------------------------------------------
const
  KeyVal: Integer = KEY_WRITE or KEY_EXECUTE or KEY_QUERY_VALUE;

  // ------------------------------------------------------------------------------
Procedure WriteRegString(MainKey: HKey; SubKey, ValName: String; const Data: String);
Var
  Key: HKey;
  D: Cardinal;
Begin
  // asm int 3 end; //trap
  if RegCreateKeyEx(MainKey, pchar(SubKey), 0, Nil, REG_OPTION_NON_VOLATILE, KeyVal, Nil, Key, @D)
    = ERROR_SUCCESS then
  begin
    try
      RegSetValueEx(Key, pchar(ValName), 0, REG_SZ, pchar(Data), Length(Data));
    finally
      RegCloseKey(Key);
    end;
  end
End;

// ------------------------------------------------------------------------------
Procedure DeleteRegValue(MainKey: HKey; SubKey, ValName: String);
Var
  Key: HKey;
Begin
  // asm int 3 end; //trap
  if RegOpenKeyEx(MainKey, pchar(SubKey), 0, KeyVal, Key) = ERROR_SUCCESS then
  begin
    try
      RegDeleteValue(Key, pchar(ValName));
    finally
      RegCloseKey(Key);
    end;
  end
End;

procedure Delay(lMilliSeconds: Dword);
var
  MyEvent: Thandle;
begin
  MyEvent := CreateEvent(nil, true, False, nil);
  waitforSingleObject(MyEvent, lMilliSeconds);
  CloseHandle(MyEvent);
end;

function PressKey(const AKey: LongInt): Boolean;
begin
  result := GetKeyState(AKey) < 0;
end;

function PressShift: Boolean;
begin
  result := PressKey(vk_Shift);
end;

function PressCtrl: Boolean;
begin
  result := PressKey(VK_CONTROL)
end;

function GetDefaultExploreProgram: String;
begin
  Result := Win.GetDefaultExploreProgram;
end;

procedure ResetScreenCursor;
var
  p: TPoint;
begin
  GetCursorPos(p);
  Mouse_Event(MOUSEEVENTF_LEFTUP, p.X, p.Y, 0, 0);
end;

function GetLastErrorText: string;
var
  dwSize: Dword;
  lpszTemp: pchar;
begin
  dwSize := 512;
  lpszTemp := nil;
  try
    getmem(lpszTemp, dwSize);
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, GetLastError,
      LANG_NEUTRAL, lpszTemp, dwSize, nil);
  finally
    result := strpas(lpszTemp);
    freemem(lpszTemp);
  end;
end;

function GetErrorText(const ACode: Integer): string;
var
  dwSize: Dword;
  lpszTemp: pchar;
begin
  dwSize := 512;
  lpszTemp := nil;
  try
    getmem(lpszTemp, dwSize);
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ACode,
      LANG_NEUTRAL, lpszTemp, dwSize, nil);
  finally
    result := strpas(lpszTemp);
    freemem(lpszTemp);
  end;
end;


// 判断当前的windows的用户是否有提升到管理员权限
{$R-}

function Vista_HasAdminAccess: Boolean; // 只能Vista win7下使用
const
  TokenElevationType = 18;
  TokenElevation = 20;
  TokenElevationTypeDefault = 1;
  TokenElevationTypeFull = 2;
  TokenElevationTypeLimited = 3;
var
  token: Cardinal;
  Elevation: Dword;
  dwSize: Cardinal;
begin
  result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
    try
      if GetTokenInformation(token, TTokenInformationClass(TokenElevation), @Elevation,
        SizeOf(Elevation), dwSize) then
      begin
        result := Elevation <> 0;
      end;
    finally
      CloseHandle(token);
    end;
end;
{$R+}

// xp下，当前的用户是否有系统管理员权限
function WinXp_IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: Thandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: Dword;
  psidAdministrators: PSID;
  X: Integer;
  bSuccess: BOOL;
begin
  result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, true, hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  end;
  if bSuccess then
  begin
    getmem(ptgGroups, 1024);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID,
        DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
{$R-}
      for X := 0 to ptgGroups.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups.Groups[X].Sid) then
        begin
          result := true;
          Break;
        end;
{$R+}
      FreeSid(psidAdministrators);
    end;
    freemem(ptgGroups);
  end;
end;

function ShellExecuteWithAdmin(AAppName: string; AParams: string;
  const ShowCmd: Integer = SW_SHOW): Boolean;
var
  si: SHELLEXECUTEINFO;
begin
  if (GetWinVersion in [wvWin7, wvVista, wvWin7Later]) and (not Vista_HasAdminAccess) then
  begin
    zeromemory(@si, SizeOf(SHELLEXECUTEINFO));
    si.cbSize := SizeOf(SHELLEXECUTEINFO);
    si.lpVerb := 'runas';
    si.lpFile := pchar(AAppName);
    si.nShow := ShowCmd;
    si.lpParameters := pchar(AParams);
    si.fMask := SEE_MASK_NOCLOSEPROCESS;
    result := ShellExecuteEx(@si);
  end
  else
    result := ShellExecute(0, 'open', pchar(AAppName), pchar(AParams), nil, ShowCmd) >= 32
end;

function IsUACEnable: Boolean;
begin
  Result := Win.IsUACEnabled;
end;

function DirectoryIsNeeddUAC(ADir: string; const ACheckUACEnable: Boolean = true): Boolean;
begin
  Result := Win.DirectoryNeedUAC(ADir, ACheckUACEnable);
end;

function InstalledDirNeedUAC: Boolean;
begin
  result := Win.InstalledDirNeedUAC;
end;

function GetSystemFolderDir(mFolder: Integer): string;
begin
  Result := Win.GetSystemFolderDir(mFolder);
end;

function GetSystemFolder(AFolderType: TSystemFolderType): String;
begin
  Result := Win.GetSystemFolder(AFolderType);
end;

function GetWindowsTempDirectory: string;
begin
  Result := Win.GetTempDirectory;
end;

function GetRandomFileName(const InitDir: String): String;
begin
  Result := Win.GetRandomFileName(InitDir);
end;

end.
