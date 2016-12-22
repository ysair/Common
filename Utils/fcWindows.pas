unit fcWindows;

interface

uses
  Classes, SysUtils, Windows, StrUtils, ShellAPI, ShlObj, CommCtrl, ActiveX,
  Forms, TLHelp32, Messages;

type
  {
    GetVersionEx 函数已经废弃，即使win10，也只能获得6.2
  }
  // Windows functions
  TSystemFolderType = (spAppData, spStartUp, spDesktop, spSendTo, spPrograms, spStartMenu,
    spQuickLanuch, spSystem32, spMyDocument, spProgramFiles, spSkin, spDmResource);

  Win = record
  type
    TWinVersion = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME, wvWin2000, wvWinXP,
      wvVista, wvWin7, wvWin8OrLater);

    class function GetInstanceFileName: string; static;
    class function GetInstancePath: string; static;
    class function GetLastErrorText: string; static;

    /// <summary>
    /// 获取Windows的临时目录，最后没有分隔符。
    /// </summary>
    class function GetTempDirectory: string; static;

    /// <summary>
    /// 获取Windows的临时目录，最后有分隔符。
    /// </summary>
    class function GetTempPath: string; static;

    /// <summary>
    /// 根据时间获取一个随机文件名
    /// </summary>
    /// <param name="ADir">指定目录</param>
    /// <returns>System.string</returns>
    class function GetRandomFileName(const ADir: String): string;static;

    /// <summary>
    /// 获取一个Windows的临时目录下的临时文件路径
    /// </summary>
    class function GetTempFileName: string; overload; static;

    /// <summary>
    /// 获取指定目录下的临时文件路径
    ///
    /// </summary>
    /// <param name="ADir">指定目录</param>
    /// <returns>string：临时文件路径</returns>
    class function GetTempFileName(const ADir: string): string; overload; static;

    /// <summary>
    /// 获取指定目录下，指定后缀名的临时文件路径
    /// </summary>
    /// <param name="ADir">指定目录</param>
    /// <param name="AFileExt">指定后缀名</param>
    /// <returns>string：临时文件路径</returns>
    class function GetTempFileName(const ADir: string; const AFileExt: string): string; overload;
      static;

    /// <summary>
    /// 获取Windows路径
    /// </summary>
    class function GetWindowsDirectory: string; static;

    /// <summary>
    /// 获取主线程ID
    /// </summary>
    /// <returns>DWORD：主线程ID</returns>
    class function GetMainThreadID: DWORD; static;

    /// <summary>
    /// 将消息对主线程中所有窗口进行广播
    /// </summary>
    /// <param name="Msg">消息</param>
    /// <param name="wParam">参数wParam</param>
    /// <param name="lParam">参数lParam</param>
    class procedure BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam); static;

    class function IsWindow(const AHandle: HWND): Boolean; static;
    class function GetWinVersion: TWinVersion; static;
    class function GetWinVerIsVistaOrLater: Boolean; static;
    class function GetWinVerIsWin8OrLater: Boolean; static;

    class function GetUserIdleTime: DWORD; static;
    class function GetExeNameByProcessID(PID: DWORD): String; static;
    class function InFullScreen: Boolean; static;
    class function GetInputDesktopName: string; static; // 获取当前桌面名称
    class function InLogonDesktop: Boolean; static;   // 是否在登录界面
    class function InScreenDesktop: Boolean; static;  // 是否在屏保界面
    class function InDefaultDesktop: Boolean; static; // 是否在默认桌面


    class function ForegroundWindow(const HWND: HWND): Boolean; static;

    /// <summary>
    /// 获取系统路径
    /// </summary>
    /// <param name="AFolder">CLIDL代号，表示特殊的目录，shlObj.pas的第2017-2152行有相关定义</param>
    ///  CSIDL_DESKTOP                       = $0000;  桌面
    ///  CSIDL_INTERNET                      = $0001;  Internet Explorer (icon on desktop)
    ///  CSIDL_PROGRAMS                      = $0002;  程序组(Start Menu\Programs) D:\Documents and Settings\Administrator\「开始」菜单\程序
    ///  CSIDL_CONTROLS                      = $0003;  控制面板
    ///  CSIDL_PRINTERS                      = $0004;  打印机
    ///  CSIDL_PERSONAL                      = $0005;  我的文档
    ///  CSIDL_FAVORITES                     = $0006;  收藏夹
    ///  CSIDL_STARTUP                       = $0007;  启动
    ///  CSIDL_RECENT                        = $0008;  最近文档
    ///  CSIDL_SENDTO                        = $0009;  发送到
    ///  CSIDL_BITBUCKET                     = $000a;  回收站
    ///  CSIDL_STARTMENU                     = $000b;  开始菜单
    ///  CSIDL_DESKTOPDIRECTORY              = $0010;  桌面目录
    ///  CSIDL_DRIVES                        = $0011;  我的电脑
    ///  CSIDL_NETWORK                       = $0012;  网上邻居
    ///  CSIDL_NETHOOD                       = $0013;  网上邻居目录
    ///  CSIDL_FONTS                         = $0014;  字体
    ///  CSIDL_COMMON_STARTMENU              = $0016;  //公用的开始菜单
    ///  CSIDL_COMMON_PROGRAMS               = $0017;
    ///  CSIDL_COMMON_STARTUP                = $0018;
    ///  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;
    ///  CSIDL_APPDATA                       = $001a; //D:\Documents and Settings\Administrator\Application Data
    ///  CSIDL_COMMON_FAVORITES          = $001f;
    ///  CSIDL_INTERNET_CACHE            = $0020;  D:\Documents and Settings\Administrator\Local Settings\Temporary Internet Files
    ///  CSIDL_COOKIES                   = $0021;   Cookies文件夹
    ///  CSIDL_HISTORY                   = $0022;   历史文件夹
    ///  CSIDL_COMMON_APPDATA            = $0023;
    ///  CSIDL_WINDOWS                   = $0024;   D:\WINNT
    ///  CSIDL_SYSTEM                    = $0025;   D:\WINNT\system32
    ///  CSIDL_PROGRAM_FILES             = $0026    D:\Program Files
    ///  CSIDL_MYPICTURES                = $0027    D:\Documents and Settings\Administrator\My Documents\My Pictures
    ///  CSIDL_PROFILE                   = $0028    D:\Documents and Settings\Administrator
    /// <returns>字符串：系统路径</returns>
    class function GetSystemFolderDir(AFolder: Integer): string; static;

    /// <summary>
    /// 延迟指定时间
    /// </summary>
    /// <param name="lMilliSeconds">微秒级时间</param>
    class procedure Delay(lMilliSeconds: DWORD); static;

    class function EnableDebugPrivilege: Boolean; static;


    /// <summary>
    /// 获取GUID字符串，如果调用失败，会返回空值
    /// </summary>
    /// <returns>GUID</returns>
    class function GetGUID: string; static;

    /// <summary>
    /// 判断给定目录是否需要UAC权限。
    // 判断方法：如果在以下的几个位置，则需要UAC。
    // 系统跟目录：
    // CSIDL_WINDOWS                   = $0024;   C:\WINNT
    // CSIDL_SYSTEM                    = $0025;   C:\WINNT\system32
    // CSIDL_PROGRAM_FILES             = $0026    C:\Program Files
    /// </summary>
    /// <param name="ADir">目录路径</param>
    /// <param name="ACheckUACEnabled">是否检查UAC开启状态</param>
    /// <returns>该目录是否需要UAC权限</returns>
    class function DirectoryNeedUAC(ADir: string; const ACheckUACEnabled: Boolean = true): Boolean; static;

    /// <summary>
    /// 当前程序所在目录是否需要UAC权限
    /// </summary>
    class function InstalledDirNeedUAC: Boolean; static;

    /// <summary>
    /// 当前系统的UAC是否开启
    /// </summary>
    class function IsUACEnabled: Boolean; static;

    /// <summary>
    /// 根据指定目录类型，获取系统路径。
    /// </summary>
    /// <param name="AFolderType">系统目录类型</param>
    /// <returns>System.string</returns>
    class function GetSystemFolder(AFolderType: TSystemFolderType): string; static;

    class function GetDefaultExploreProgram: String; static;

    /// 判断一个dll是否有效
    /// 注意：如果这个dll与其他dll相互影响，函数可能会返回无效
    class function DllIsValid(ADllFileName: string): Boolean; static;

    class function Vista_HasAdminAccess: Boolean; static;  ////判断当前的windows的用户是否有提升到管理员权限, 只能Vista win7下使用， 其他的系统返回false

    /// 创建一个进程，并且等待运行结束
    ///  function: 秒， 超时时间
    class function CreateProcessAndWaitFinished(ACmdLine: string; const ATimeOut: DWORD=MAXWORD; const AShowWindow : Word = SW_HIDE): Boolean; static;

    /// 判断句柄是否允许访问,比如设为父窗口句柄
    class function HandleCanAccess(const AHandle : HWND):Boolean; static;

    //以系统管理员的权限运行AAppName。 会自动判断当前的进程是否有系统管理员权限。
    class function ShellExecuteWithAdmin(AAppName: string; AParams: string; const ShowCmd: Integer= SW_SHOW): Boolean; static;

    //Vista权限下，对不同权限进程消息互通处理
    class procedure ChangeWindowMessageFilter_Comm(AMsg: Cardinal); static;
    class procedure ChangeWindowMessageFilter_DragFiles; static;

    // 获得IE的版本， 不是从注册表读取，而是直接从文件读取版本号。 因为注册表老不准，总被修改。
    class function  GetIEVersion: string; static;
    class function  GetIEMainVersion: Integer; static;

    //判断一个目录是否有写权限
    //因为可能受到一些杀毒软件的影响，获取目录属性可能不准，直接用创建文件的方法来判断
    class function  DirectoryHasWriteAccess(ADir: string): Boolean; static;

  end;

  {屏幕相关的}
  Scrn = class
    class function DPIRatio: Single; static;    //DPI

    {把一个数字转为当前dpi显示的大小
     例如：
      如果当前dpi是96， 按1:1返回
      如果当前dpi是150%（96*150%=144），则返回1:1.5
      200%放大显示时，返回2倍
     注意：使用这个函数的基准是按96dpi计算的
    }
    class function ToCurrDpi(AInSize: Integer): Integer; static;
  end;

implementation

uses
  Registry, fcStr, fcRegistry, fcFile;

var
  HLib_PSAPI: HWND;
  EnumProcessModules: function(hProcess: THandle; lphModule: LPDWORD; cb: DWORD; var lpcbNeeded: DWORD): bool; StdCall;
  GetModuleFileNameEx : function(hProcess: HINST; hModule: HINST; lpFilename: PChar; nSize: DWORD): DWORD; stdcall;
  GetProcessImageFileName : function(hProcess: THandle; lpFilename: PChar; nSize: DWORD): DWORD; stdcall;
  ChangeWindowMessageFilter: function (msg: Cardinal; dwFlag : Word): BOOL; stdcall ;

{ Win }

function __BroadcastMessageEnumProc(AHandle: HWND; AParam: lParam): Boolean; stdcall;
var
  lpMsg: PMessage;
begin
  if (AParam <> 0) and Win.IsWindow(AHandle) then
  begin
    lpMsg := Pointer(AParam);
    PostMessage(AHandle, lpMsg.Msg, lpMsg.wParam, lpMsg.lParam);
  end;
  Result := True;
end;

class procedure Win.BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam);
var
  mtid: DWORD;
  lpMsg: PMessage;
begin
  mtid := GetMainThreadID;
  new(lpMsg);
  try
    lpMsg.Msg := Msg;
    lpMsg.wParam := wParam;
    lpMsg.lParam := lParam;
    EnumThreadWindows(mtid, @__BroadcastMessageEnumProc, Integer(lpMsg));
  finally
    Dispose(lpMsg);
  end;
end;

class function Win.IsWindow(const AHandle: HWND): Boolean;
var
  Style: DWORD;
begin
  if not Windows.IsWindow(AHandle) then
    Exit(False);
  Style := GetWindowLong(AHandle, GWL_STYLE);
  Result := (Style and WS_SYSMENU = WS_SYSMENU) or (Style and WS_POPUP = WS_POPUP);
end;

class function Win.ShellExecuteWithAdmin(AAppName, AParams: string;
  const ShowCmd: Integer): Boolean;
var
  si : SHELLEXECUTEINFO;
begin
  if GetWinVerIsVistaOrLater and (not Vista_HasAdminAccess) then
  begin
    zeromemory(@si, sizeof(SHELLEXECUTEINFO));
    si.cbSize :=  sizeof(SHELLEXECUTEINFO);
    si.lpVerb :=  'runas';
    si.lpFile :=  pchar(AAppName);
    si.nShow  :=  ShowCmd;
    si.lpParameters := pchar(AParams);
    si.fMask  :=  SEE_MASK_NOCLOSEPROCESS;
    Result := ShellExecuteEx(@si);
  end
  else
    Result := ShellExecute(0, 'open', PChar(AAppName), PChar(AParams), nil, ShowCmd)>=32
end;


{$R-}class function Win.Vista_HasAdminAccess: Boolean;
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
  Result  :=  false;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
  try
    if GetTokenInformation(token, TTokenInformationClass(TokenElevation), @Elevation, SizeOf(Elevation), dwSize) then
    begin
      result  := Elevation <> 0;
    end;
  finally
    CloseHandle(token);
  end;
end; {$R+}

class function Win.GetIEMainVersion: Integer;
begin
  Result := StrToIntDef(fcStr.Str.GetTokenBeforeChar(Win.GetIEVersion, '.'), 0);
end;

class function Win.GetIEVersion: string;
var
  l_File: string;
begin
  l_File := Win.GetSystemFolder(spProgramFiles) + '\Internet Explorer\iexplore.exe';
  if FileExists(l_File) then
    Exit(fcFile.FileVersion.GetFileVersion(l_File));

  Result := fcRegistry.Reg.SH_GetKeyValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Internet Explorer', 'svcVersion');
  if Result='' then
    Result := fcRegistry.Reg.SH_GetKeyValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Internet Explorer', 'Version');
end;

class function Win.GetInputDesktopName: string;
var
  desk: HDESK;
  sname: array [0 .. MAX_PATH - 1] of char;
  len: DWORD;
begin // 'Screen-saver'
  Result := '';
  desk := OpenInputDesktop(DESKTOP_READOBJECTS, False, STANDARD_RIGHTS_READ);
  if desk = 0 then
    Exit
  else
  begin
    ZeroMemory(@sname[0], SizeOf(sname));
    GetUserObjectInformation(desk, UOI_NAME, @sname[0], SizeOf(sname), len);
    if len = 0 then
      Exit;
    Result := WideCharToString(sname);
    CloseDesktop(desk);
  end;
end;

class function Win.GetInstanceFileName: string;
var
  pPath: array [0 .. MAX_PATH] of char;
begin
  GetModuleFileName(HInstance, pPath, MAX_PATH);
  Result := pPath; // ShortNameToLongName(pPath);
end;

class function Win.GetInstancePath: string;
begin
  Result := ExtractFilePath(GetInstanceFileName);
end;

class function Win.GetLastErrorText: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

class function Win.GetMainThreadID: DWORD;
var
  PID: DWORD;
  te32: THREADENTRY32;
  snap: THandle;
begin
  Result := 0;
  PID := GetCurrentProcessId;
  ZeroMemory(@te32, SizeOf(THREADENTRY32));
  te32.dwSize := SizeOf(THREADENTRY32);
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, PID);
  if snap <> 0 then
  begin
    if Thread32First(snap, te32) then
    begin
      repeat
        if te32.th32OwnerProcessID = PID then
          Exit(te32.th32ThreadID);
      until not(Thread32Next(snap, te32));
    end;
  end;
end;

class function Win.GetRandomFileName(const ADir: String): String;
var
  sFileName: String;
begin
  ForceDirectories(ADir);
  repeat
    sFileName := ADir + '\' + FormatDateTime('yyyymmddhhnnsszzz', now);
    Sleep(1);
  until not FileExists(sFileName);
  result := sFileName;
end;

class function Win.GetTempPath: string;
begin
  Result := GetTempDirectory + '\';
end;

class function Win.GetTempFileName: string;
begin
  Result := GetTempFileName(GetTempPath);
end;

class function Win.GetTempFileName(const ADir: string): string;
var
  sFile: array [0 .. MAX_PATH] of char;
begin
  SysUtils.ForceDirectories(ADir);
  Windows.GetTempFileName(PChar(ADir), 'tmp', 0, sFile);
  Result := sFile;
  Windows.DeleteFile(sFile);
end;

class function Win.GetTempDirectory: string;
var
  Dir: array [0 .. MAX_PATH] of char;
begin
  if Boolean(Windows.GetTempPath(MAX_PATH, Dir)) then
    Result := strpas(Dir)
  else
  begin
    Windows.GetWindowsDirectory(Dir, MAX_PATH);
    Result := strpas(Dir) + '\temp';
    ForceDirectories(Result);
  end;

  if Result <> '' then
    if Result[Length(Result)] = '\' then
      Result := copy(Result, 1, Length(Result) - 1);

  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

class function Win.GetTempFileName(const ADir, AFileExt: string): string;
begin
  Result := ChangeFileExt(GetTempFileName(ADir), AFileExt);
end;

class function Win.GetWindowsDirectory: string;
var
  sPath: array [0 .. MAX_PATH] of char;
begin
  Windows.GetWindowsDirectory(sPath, MAX_PATH);
  Result := sPath;
end;

class procedure Win.ChangeWindowMessageFilter_Comm(AMsg: Cardinal);
const
  MSGFLT_ADD = 1;
begin
  if GetWinVerIsVistaOrLater then
  begin

    @ChangeWindowMessageFilter :=GetProcAddress(LoadLibrary('user32.dll'),'ChangeWindowMessageFilter');
    if @ChangeWindowMessageFilter<>nil then //je?li mamy adres, oznacza, ?e program uruchomiono pod systemem Widndows 7 lub pod Vist?
    try
      ChangeWindowMessageFilter(AMsg , MSGFLT_ADD);
    except
    end;
  end;
end;

class procedure Win.ChangeWindowMessageFilter_DragFiles;
const
  WM_COPYGLOBALDATA = 73;
begin
  ChangeWindowMessageFilter_Comm(WM_COPYGLOBALDATA);
  ChangeWindowMessageFilter_Comm(WM_DROPFILES);
  ChangeWindowMessageFilter_Comm(WM_COPYDATA);
end;

class function Win.CreateProcessAndWaitFinished(ACmdLine: string;
  const ATimeOut: DWORD; const AShowWindow : Word): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  SI.wShowWindow := AShowWindow;
  Result := CreateProcess(nil,
                          PChar(ACmdLine),
                          nil,
                          nil,
                          True,
                          0,
                          nil,
                          nil,
                          SI,
                          PI);
  try
    WaitForSingleObject(PI.hProcess, ATimeOut * 1000);
  finally
    // Close all remaining handles
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

class procedure Win.Delay(lMilliSeconds: DWORD);
var
  l_Event: THandle;
begin
  l_Event := CreateEvent(nil, True, False, nil);
  waitforSingleObject(l_Event, lMilliSeconds);
  CloseHandle(l_Event);
end;

class function Win.EnableDebugPrivilege: Boolean;
var
  hToken: THandle;
  tp: TTokenPrivileges;
  rl: Cardinal;
begin
  Result := False;
  OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken);
  if LookupPrivilegeValue(nil, 'SeDebugPrivilege', tp.Privileges[0].Luid) then
  begin
    tp.PrivilegeCount := 1;
    tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    Result := AdjustTokenPrivileges(hToken, False, tp, SizeOf(tp), nil, rl);
  end;
end;

class function Win.ForegroundWindow(const HWND: HWND): Boolean;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(HWND) then
    ShowWindow(HWND, SW_RESTORE);
  if GetForegroundWindow = HWND then
    Result := True
  else
  begin // 如果有一個視窗有鍵盤 foucs，Windows 98/2000 不會把另一個視窗切到前景
    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
        ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and (Win32MinorVersion > 0))))
      then
    begin
      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadProcessID(HWND, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(HWND);
        SetForegroundWindow(HWND);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = HWND);
      end;
      if not Result then
      begin
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0), SPIF_SENDCHANGE);
        BringWindowToTop(HWND);
        SetForegroundWindow(HWND);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout),
          SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(HWND);
      SetForegroundWindow(HWND);
    end;
    Result := (GetForegroundWindow = HWND);
  end;
end;

class function Win.GetExeNameByProcessID(PID: DWORD): String;
var
  hProc: HWND;
  hMod: hModule;
  dwSize: DWORD;
  szName: array [0 .. MAX_PATH - 1] of char;
begin
  Result := '';
  hProc := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
  if hProc > 0 then
    try
      if Assigned(GetProcessImageFileName) and (GetProcessImageFileName(hProc, szName,
          MAX_PATH) > 0) then
      begin
        Exit(szName);
      end
      else if Assigned(EnumProcessModules) and Assigned(GetModuleFileNameEx) then
      begin
        if EnumProcessModules(hProc, @hMod, SizeOf(hMod), dwSize) then
        begin
          ZeroMemory(@szName[0], SizeOf(szName));
          GetModuleFileNameEx(hProc, hMod, szName, MAX_PATH);
          Result := szName;
        end;
      end;
    finally
      CloseHandle(hProc);
    end;
end;

class function Win.GetSystemFolderDir(AFolder: Integer): string;
var
  vItemIDList: PItemIDList;
  vBuffer: array [0 .. MAX_PATH] of char;
begin
  SHGetSpecialFolderLocation(0, AFolder, vItemIDList);
  SHGetPathFromIDList(vItemIDList, vBuffer); // 转换成文件系统的路径
  Result := vBuffer;
end;

class function Win.GetUserIdleTime: DWORD;
var
  lii: TLastInputInfo;
begin
  ZeroMemory(@lii, SizeOf(TLastInputInfo));
  lii.cbSize := SizeOf(TLastInputInfo);
  if GetLastInputInfo(lii) then
    Result := GetTickCount - lii.dwTime
  else
    Result := 0;
end;

class function Win.GetWinVerIsVistaOrLater: Boolean;
var
  osVerInfo: TOSVersionInfo;
begin
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  Result := GetVersionEx(osVerInfo)
    and (osVerInfo.dwPlatformId=VER_PLATFORM_WIN32_NT)
    and (osVerInfo.dwMajorVersion>=6);
end;

class function Win.GetWinVerIsWin8OrLater: Boolean;
var
  osVerInfo: TOSVersionInfo;
begin
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  Result := GetVersionEx(osVerInfo)
    and (osVerInfo.dwPlatformId=VER_PLATFORM_WIN32_NT)
    and (
         ( (osVerInfo.dwMajorVersion=6) and (osVerInfo.dwMinorVersion>1))
         or (osVerInfo.dwMajorVersion>6)
        ) ;
end;

class function Win.GetWinVersion: TWinVersion;
var
  osVerInfo: TOSVersionInfo;
  majorVersion, minorVersion: Integer;
begin
  Result := wvUnknown;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    minorVersion := osVerInfo.dwMinorVersion;
    majorVersion := osVerInfo.dwMajorVersion;
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
        begin
          if majorVersion <= 4 then
            Result := wvWinNT
          else if (majorVersion = 5) and (minorVersion = 0) then
            Result := wvWin2000
          else if (majorVersion = 5) and (minorVersion = 1) then
            Result := wvWinXP
          else if (majorVersion = 6) and (minorVersion = 0) then
            Result := wvVista
          else if (majorVersion = 6) and (minorVersion = 1) then
            Result := wvWin7
          else if (majorVersion = 6) and (minorVersion > 1) or (majorVersion>6) then
            Result := wvWin8OrLater;
        end;
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          if (majorVersion = 4) and (minorVersion = 0) then
            Result := wvWin95
          else if (majorVersion = 4) and (minorVersion = 10) then
          begin
            if osVerInfo.szCSDVersion[1] = 'A' then
              Result := wvWin98SE
            else
              Result := wvWin98;
          end
          else if (majorVersion = 4) and (minorVersion = 90) then
            Result := wvWinME
          else
            Result := wvUnknown;
        end;
    end;
  end;
end;

class function Win.HandleCanAccess(const AHandle: HWND): Boolean;
var
  PID: DWORD;
  hProc : THandle;
begin
  Result  :=  IsWindow(AHandle);
  if Result then
    if GetWindowThreadProcessID(AHandle, @PID) > 0 then
    begin
      hProc :=  OpenProcess(PROCESS_ALL_ACCESS, False, PID);
      Result  :=  hProc > 0;
      if Result then
        CloseHandle(hProc);
    end;
end;

class function Win.InDefaultDesktop: Boolean;
var
  sname: string;
begin
  sname := GetInputDesktopName;
  Result := SameText(sname, 'Default');
end;

class function Win.InFullScreen: Boolean;
var
  p: TPoint;
  h1, h2, h3, h4: HWND;
  p1, p2, p3, p4: DWORD;
  sName: string;
begin
  p := Point(1, 1);
  h1 := WindowFromPoint(p);
  p := Point(Screen.Width - 1, 1);
  h2 := WindowFromPoint(p);
  p := Point(1, Screen.Height - 1);
  h3 := WindowFromPoint(p);
  p := Point(Screen.Width - 1, Screen.Height - 1);
  h4 := WindowFromPoint(p);
  GetWindowThreadProcessID(h1, @p1);
  GetWindowThreadProcessID(h2, @p2);
  GetWindowThreadProcessID(h3, @p3);
  GetWindowThreadProcessID(h4, @p4);
  Result := (p1 = p2) and (p1 = p3) and (p1 = p4);
  if Result then
  begin
    sName := ExtractFileName(GetExeNameByProcessID(p1));
    Result := (sName <> '') and not SameText(sName, 'explorer.exe');
  end;
end;

class function Win.InLogonDesktop: Boolean;
var
  sname: string;
begin
  sname := GetInputDesktopName;
  Result := SameText(sname, 'winlogon') or (sname = '') and
    (GetLastError = ERROR_ACCESS_DENIED);
end;

class function Win.InScreenDesktop: Boolean;
var
  sname: string;
begin
  sname := GetInputDesktopName;
  Result := SameText(sname, 'Screen-saver') or SameText(sname, 'Screen saver');
end;

class function Win.GetGUID: string;
var
  id  : TGuid;
begin
  if CoCreateGuid(id) = S_OK then
    result  :=  GuidToString(id)
  else
    result  :=  Str.RandomStr(32);
end;


class function Win.IsUACEnabled: Boolean;
begin
  result := GetWinVerIsVistaOrLater
    and (Reg.ReadKeyValue(HKEY_LOCAL_MACHINE,
        'Software\Microsoft\Windows\CurrentVersion\Policies\System', 'EnableLUA', 0) = 1);
end;

class function Win.DirectoryNeedUAC(ADir: string; const ACheckUACEnabled: Boolean): Boolean;
var
  l_WindowsDir: string;
  l_EspDirs: array [0 .. 2] of string;
  l_Idx: Integer;
begin
  result := False;

  if ACheckUACEnabled then
    if not IsUACEnabled then
      Exit;

  l_EspDirs[0] := UpperCase(GetSystemFolderDir($0024));
  l_EspDirs[1] := UpperCase(GetSystemFolderDir($0025));
  l_EspDirs[2] := UpperCase(GetSystemFolderDir($0026));

  l_WindowsDir := l_EspDirs[0];
  ADir := UpperCase(ADir);

  if (l_WindowsDir = '') or (ADir = '') then
    Exit;

  // 如果在系统跟目录下
  if (Length(ADir) = 3) and (ADir[1] = l_WindowsDir[1]) then
  begin
    result := true;
    Exit;
  end;

  for l_Idx := Low(l_EspDirs) to High(l_EspDirs) do
  begin
    if Pos(l_EspDirs[l_Idx], ADir) = 1 then
    begin
      result := true;
      Exit;
    end;
  end;
end;

class function Win.DllIsValid(ADllFileName: string): Boolean;
begin
  SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result  := LoadLibrary(PChar(ADllFileName)) > 0;
  finally
    SetErrorMode(0);
  end;
end;

class function Win.InstalledDirNeedUAC: Boolean;
begin
  result := DirectoryNeedUAC(ExtractFilePath(Application.ExeName));
end;

class function Win.GetSystemFolder(AFolderType: TSystemFolderType): String;
var
  PText: array [0 .. 255] of Char;
  laengde: Integer;
const
  l_Folder1 = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  l_Folder2 = 'Software\Microsoft\Windows\CurrentVersion';
begin
  result := '';

  if AFolderType in [spAppData, spStartUp, spDesktop, spSendTo, spPrograms, spStartMenu,
    spQuickLanuch, spMyDocument] then
  begin
    case AFolderType of
      spAppData:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'AppData', '');
      spStartUp:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'StartUp', '');
      spDesktop:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'DeskTop', '');
      spSendTo:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'SendTo', '');
      spPrograms:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'Programs', '');
      spStartMenu:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'Start Menu', '');
      spMyDocument:
        result := Reg.ReadKeyValue(HKEY_CURRENT_USER, l_Folder1, 'Personal', '');
    end;
  end
  else if AFolderType = spSkin then
    result := ExtractFilePath(Application.ExeName) + 'Sys\Skins'
  else if AFolderType = spDmResource then
    result := ExtractFilePath(Application.ExeName) + 'Sys\Resource'
  else if AFolderType = spSystem32 then
  begin
    laengde := GetSystemDirectory(PText, MAX_PATH);
    result := copy(PText, 1, laengde);
  end
  else if AFolderType in [spProgramFiles] then
  begin
    case AFolderType of
      spProgramFiles:
        result := Reg.ReadKeyValue(HKEY_LOCAL_MACHINE, l_Folder2, 'ProgramFilesDir', '');
    end;
  end;
end;

class function Win.GetDefaultExploreProgram: String;
var
  RegF: TRegistry;
begin
  RegF := TRegistry.Create;
  try
    RegF.RootKey := HKEY_CLASSES_ROOT;
    RegF.OpenKey('HTTP\shell\open\command', true);

    result := Trim(RegF.ReadString('')); // "C:\Program Files\Internet Explorer\iexplore.exe" -nohome
  finally
    RegF.Free;
  end;

  if result <> '' then
  begin
    if result[1] = '"' then
    begin
      Delete(result, 1, 1);
      result := Str.GetTokenBeforeChar(result, '"', true, False);
    end
    else
    begin
      result := Str.GetTokenBeforeChar(result, ' ', true, False);
    end;
  end;

  if not FileExists(result) then
  begin
    result := 'IEXPLORE.EXE';
  end;
end;


class function Win.DirectoryHasWriteAccess(ADir: string): Boolean;
var
  l_FileName: string;
  l_Handle: Integer;
begin
  if not ForceDirectories(ADir) then
    Exit(False);

  if Str.IsEndWith('\', ADir, False) then
    l_FileName := ADir
  else
    l_FileName := ADir + '\';

  l_FileName := l_FileName + '$'+Str.RandomStr(10) + '.tmp';
  if not FileExists(l_FileName) then
  begin
    l_Handle := FileCreate(l_FileName);
    if l_Handle = -1 then
    begin
      Exit(False);
    end
    else
    begin
      Result := True;
      FileClose(l_Handle);
      DeleteFile(PWideChar(l_FileName));
    end;
  end
  else
    Result := DeleteFile(PWideChar(l_FileName));

end;

procedure InitAPI;
begin
  GetModuleFileNameEx := nil;
  HLib_PSAPI := SafeLoadLibrary('psapi.dll');
  if HLib_PSAPI > 0 then
  begin
    @EnumProcessModules := GetProcAddress(HLib_PSAPI, 'EnumProcessModules');
    @GetModuleFileNameEx := GetProcAddress(HLib_PSAPI, 'GetModuleFileNameExW');
    @GetProcessImageFileName := GetProcAddress(HLib_PSAPI, 'GetProcessImageFileNameW');
  end;
end;

procedure FinalAPI;
begin
  if HLib_PSAPI > 0 then
    FreeLibrary(HLib_PSAPI);
end;

{ Scrn }

class function Scrn.DPIRatio: Single;
begin
  Result := Screen.PixelsPerInch / 96;
end;

class function Scrn.ToCurrDpi(AInSize: Integer): Integer;
begin
  Result := AInSize * Screen.PixelsPerInch div 96;
end;

initialization

InitAPI;

finalization

FinalAPI;

end.
