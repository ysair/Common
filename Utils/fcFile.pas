unit fcFile;

interface

uses
  SysUtils, Windows, Classes, ShellAPI, ShlObj, CommCtrl, ActiveX, Forms,
  Dialogs, Math, StrUtils;

type
  TVersionInfo = record
    CommpanyName: string;
    FileDescription: string;
    FileVersion: string;
    InternalName: string;
    LegalCopyright: string;
    LegalTrademarks: string;
    OriginalFileName: string;
    ProductName: string;
    ProductVersion: string;
    Comments: string;
    VsFixedFileInfo: VS_FIXEDFILEINFO;
    UserDefineValue: string;
  end;

  PSearchFileInfo = ^TSearchFileInfo;
  TSearchFileInfo = record
    FileName: string;
    ModifyTime: TFileTime;
    Size: Integer;
    Tag: Integer;
    HashCode : string;
    ZipSize : Integer;
    ZipHashCode : string;
  end;

  TFileTimeType = (fttCreation, fttLastAccess, fttLastWrite);

  TFindFilesCallBackFunc  = procedure(const ASearchFileInfo : PSearchFileInfo);

  Files = record
    class function RunAs(const AExeFile: string; const AParam: string): boolean; static;

    class function DeleteDirectory(const ADir: string; const AFlag: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_SILENT; const AFormHandle: THandle=0): boolean; static;

    class function CopyFile(const AFrom, ATo: string; const AFlag: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI; const AFormHandle: THandle=0): boolean; static;
    class function CopyFiles(const AFromFileList: TStringList; AToDir: string; const AFlag: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI; const AFormHandle: THandle=0): boolean; static;
    class function MoveFile(const AFrom, ATo: string; const AFlag: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI; const AFormHandle: THandle=0): boolean; static;
    class function DeleteFile(const AFileName: string; const AFlag: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_SILENT; const AFormHandle: THandle=0): boolean; static;
    class function RenameFile(const AFileName, ANewName: string; const AFlag: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_SILENT; const AFormHandle: THandle=0): boolean; static;


    //当要把一个文件保存到一个目录下时，可能该目录下已经存在文件了，
    //则会自动重命名为“文件名(2).jpg” 这样的格式, 标号从2开始
    //返回包含路径的新的文件名
    class function GetASaveFileName(AToDir: string; ADefuleFileName: string): string; static;

    /// <summary>
    /// 打开一个窗口选择一个文件, 兼容Vista
    /// </summary>
    /// <param name="AForm">父窗口</param>
    /// <param name="AFilter">文件过滤条件</param>
    /// <param name="ADefaultDir">默认目录</param>
    /// <returns>选择的文件名</returns>
    class function SelectFile(AForm: TForm; const AFilter: String = ''; const ADefaultDir: String = ''; const ACanMultFile: Boolean=false; const AType: Integer=1): String; static;
    class function OpenFileDlg(AForm: TForm; const AFilterOrFileExt: String): string; static;
    class function SaveFileDlg(AForm: TForm; const AFilterOrFileExt: String): string; static;


    class function IsFileReadOnly(AFileName: String): boolean; static;
    class function SetFileReadOnly(AFileName: String; AValue: boolean): boolean; static;
    class function GetRandomFileName(const ADir: String;  const AFileExt: String = '.tmp'): String; static;
    class function GetContentType(AExtName: string): string; static;
    class function GetFileSize(AFileName: string): Int64; static;
    class function GetFileModifyTime(AFileName: string): TDateTime; static;

    //分别对应文件创建时间，访问时间，修改时间
    class function GetFileDateTime(const FileName: string; FileTimeType: TFileTimeType): TDateTime; static;
    class function SetFileDateTime(const FileName: string; FileTimeType: TFileTimeType; DateTime: TDateTime): Integer; overload; static;
    class procedure SetFileDateTime(const FileName: string; DateTime: TDateTime); overload; static;



    class function FileToString(const AFileName: string; const AEncoding: TEncoding = nil): string; static;
    class function StringToFile(const AFileName: string; AContents: RawByteString; AAppend: boolean = False): boolean; overload; static;
    class function StringToFile(const AFileName: string; AContents: string; const AEncoding: TEncoding = nil): boolean; overload; static;
    class function FileToBytes(const AFileName : string):TBytes; static;
    class function BytesToFile(const ABytes : TBytes; const AFileName : string):Boolean; static;
    class function TextFileAppendLine(const AFileName, ALine: string): Boolean; static;
    class function TextFileAppendLines(const AFileName: string; const ALines: TStrings): Boolean; static;

    { Unicode }
    class function FileIsInUsed(AFileName: string): boolean; static;


    {这个函数会判断系统版本，如果是win7/vista则会采用新型的选择对话框}
    class function SelectDir(AParentHandle: THandle; const ACaption, ALabel: string; var ADirectory: string; const ARoot: string=''): Boolean; overload; static;   {优先使用1，}
    class function SelectDirectory(AParentHandle: THandle; const Caption: string; const Root: WideString;  var Directory: string): Boolean; overload; static; //优先使用2，{使用这个，新的}
    class function SelectDirectory(const Caption: string; const Root: String; out Directory: string; const AHandle: HWND = 0): boolean; overload; static; {不使用这个，因为无法创建目录}

    class function Open(const AFileName: string): boolean; static;
    class function OpenWith(const AFileName: string): boolean; static; //弹出打开为..的对话框并且打开
    class function FindFiles(const AList: TStrings; const APath: String; const AFileName: string = '*.*'; const ASubFolder: boolean = False): DWORD; overload; static;
    class function FindFiles(const AList: TList; const APath: String; const AFileName: string = '*.*'; const ASubFolder: boolean = False; const ACallBackFunc : TFindFilesCallBackFunc = nil): DWORD; overload; static;
    class function FindFileInFolder(const APath: String; const AFileName: string = '*.*'; const ASubFolder: boolean = False): Boolean; static;
    class function FindFolders(const AList: TStrings; const APath: String;const AFolderName: string = '*'; const ASubFolder: boolean = False): DWORD; static;
    class procedure FindTree(const Path: String; CurrLevel, WantLevelCount: Integer; var TreeResult: TStringList); overload; static;
    class procedure FindTree(const Path: String; CurrLevel, WantLevelCount: Integer; var TreeResult: TStringList; var ARunning: boolean); overload; static;

    {用资源管理器打开，并且定位到文件}
    class function LocaltionByExplorer(const AFileName: string): boolean; static;

    /// <summary>
    /// 去掉文件名中一些无效的字符
    /// </summary>
    /// <param name="AFileName">文件名</param>
    /// <returns>过滤后文件名</returns>
    class function GetValidFileName(const AFileName: String): String; static;

    /// <summary>
    /// 显示一个选择窗口，把一个文件拷贝为该选择文件。
    /// </summary>
    /// <param name="AForm">选择窗口的父窗口</param>
    /// <param name="AFromFile">待拷贝文件名</param>
    /// <returns>拷贝目标文件名</returns>
    class function CopyFileTo(AForm: TForm; AFromFile: String; const AFlag: FILEOP_FLAGS = FOF_SILENT): String; overload; static;
    class function CopyFileTo(AFormHandle: THandle; AFromFile: String; const AFlag: FILEOP_FLAGS = FOF_SILENT): String; overload; static;
    class function CopyFilesTo(AFormHandle: THandle; AFromFileList: TStringList; const AFlag: FILEOP_FLAGS = FOF_SILENT): Boolean; static;

    /// <summary>
    /// 创建一个空文件。不使用系统的FileCreate的原因是FileCreate会返回一个Integer变量的Handle，如果这个变量没有释放
    /// 则后面的所有关于这个文件的操作都将失败
    /// </summary>
    /// <param name="AFileName">文件名</param>
    /// <returns>是否创建成功</returns>
    class function FileCreate(const AFileName: string): boolean; static;

    /// <summary>
    /// 创建一个程序的快捷方式
    /// </summary>
    /// <param name="proName">程序路径</param>
    /// <param name="proParam">程序参数</param>
    /// <param name="lnkName">链接路径名</param>
    /// <param name="Description">程序描述</param>
    /// <remarks>
    /// <code>
    /// FShortCutDesktopFileName := PBSystemPath1.DeskTop + '\DreamEdit.lnk';
    /// if not FIleExists(FShortCutDesktopFileName) then
    /// MakeLink(Application.Exename,'',FShortCutDesktopFileName,'DreamEdit');
    /// </code>
    /// </remarks>
    class procedure MakeLink(proName, proParam, lnkName, Description: string);  static;

    {把快捷方式解析成真正的文件名}
    class function FileLinkToTrueFileName(ALnkName: string): string; static;

    /// <summary>
    /// 删除一个目录，包含子目录和其目录下的所有文件
    /// </summary>
    class procedure DeleteDirectory2(const Directory: string); static;

    /// <summary>
    /// 删除目录下所有指定类型文件
    /// </summary>
    /// <param name="Dir"></param>
    /// <param name="Filetype"></param>
    class procedure DeleteFiles(Dir, Filetype: String); static;

    class function StringToFileName(const AStr: string): string; static;

    {把文件放入剪切板，可以在资源管理器中执行粘贴、剪切}{还不支持UNICODE的文件名}
    class function SetFilesToClipboard(AFileList: TStringList; const bCopy: Boolean=true): Boolean; static;
    class function SetFileToClipboard(AFileName: string; const bCopy: Boolean=true): Boolean; static;

    {从剪切板读取文件名}
    class function GetFilesFromClipboard(AFileList: TStringList): Boolean; static;

    class function LocalFileToURL(const AFileName : string):string; static;

    {用于更新文件时，如果覆盖不成功，先把原来的文件改为为.old，然后在复制过去}
    class function UpdateFile_DoCopy(AFromFile, AToFile: string): Boolean; static;
  end;

  FileVersion = record
    class function GetFileVersionInfomation(const FileName: string; var info: TVersionInfo; const UserDefine: string = ''): boolean; static;

    class function GetFileVersion(const AFileName: String): String; static;
    class function CompareVersion(const AOldVersion: string; const ANewVersion: string): Integer; static;
  end;

  FileStr = record
    /// <summary>
    /// 格式化表示文件大小的字符串。
    /// </summary>
    /// <param name="AFileSize"></param>
    /// <param name="ADecimalLenOfM">可以取[1,2,3]，表示当文件大小以M为单位时，小数点后位数</param>
    /// <param name="ADecimalLenOfK">可以取[1,2,3]，表示当文件大小以K为单位时，小数点后位数</param>
    /// <param name="ASpaceCount">数字和“K” "M"之间空格个数
    /// <returns></returns>
    class function FormatFileSize(AFileSize: Int64; const ADecimalLenOfM: Integer = 1; const ADecimalLenOfK: Integer = 1; const ASpaceCount: Integer=1): String; static;
    class function FormatFileSizeExt(AFileSize: Int64): String; static;

    // 系统的 SameFileName函数无法处理长短文件名的问题
    class function IsSameFileName(AFileName1, AFileName2: string): boolean; static;

    class function CompareVersionNumber(AVer1, AVer2: string): Integer; static;

    class function FilterNoEffectChar(FileName: String): String; static;

    /// 返回 不包含扩展名的文件名，
    /// eg:  c:\test1.eml -> test1
    //       test1.eml -> test1
    class function ExtractFileFirstName(AFileName: string): string; static;

    //去掉目录名后面的那个“\”
    class function FilePathToFileDir(AFilePath: string): string; static;


    //判断一个文件名是否 以路径“APath”开始的
    //eg:  AFileName = c:\test1\test2\test3\...\aa.txt
    //     APath = c:\Test1\test2         |  = c:\Test1\test
    //     返回True                       |  返回 false
    class function FileNameIsBeginWithPath(AFileName: string; APath: string): Boolean; static;
  end;

function SearchFileInfoComp(F1, F2: Pointer): Integer;

implementation

uses
  ComObj, fcRegistry, AnsiStrings, MyDlg, FReplace, fcStr, fcWindows;
{$WARN SYMBOL_PLATFORM OFF}

function SearchFileInfoComp(F1, F2: Pointer): Integer;
begin
  if LowerCase(PSearchFileInfo(F1)^.FileName) > LowerCase
    (PSearchFileInfo(F2)^.FileName) then
    Result := 1
  else if LowerCase(PSearchFileInfo(F1)^.FileName) < LowerCase
    (PSearchFileInfo(F2)^.FileName) then
    Result := -1
  else
    Result := 0;
end;

{ Files }

class function Files.FindFiles(const AList: TStrings;
  const APath, AFileName: string; const ASubFolder: boolean): DWORD;
var
  sname: string;
  sPath: String;
  info: SysUtils.TSearchRec;
begin
  if not DirectoryExists(APath) then
    Exit(ERROR_PATH_NOT_FOUND);

  if APath[Length(APath)] <> '\' then
    sPath := APath + '\'
  else
    sPath := APath;
  if AFileName = '' then
    sname := '*.*'
  else
  if ExtractFileExt(AFileName) = AFileName then   // .xml
    sname := '*' + AFileName
  else
    sname := AFileName;

  Result := FindFirst(sPath + sname, faAnyFile and (not faDirectory), info);
  if Result <> INVALID_HANDLE_VALUE then
  begin
    try
      if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
        ((info.Attr and faDirectory) <> faDirectory) then
        AList.Add(sPath + info.Name);
      while 0 = FindNext(info) do
        if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
          ((info.Attr and faDirectory) <> faDirectory) then
          AList.Add(sPath + info.Name);
    finally
      SysUtils.FindClose(info);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
        try
          if (info.Name <> '.') and (info.Name <> '..') and
            ((info.Attr and faDirectory) = faDirectory) then
            FindFiles(AList, sPath + info.Name, sname, ASubFolder);
          while 0 = FindNext(info) do
            if (info.Name <> '.') and (info.Name <> '..') and
              ((info.Attr and faDirectory) = faDirectory) then
              FindFiles(AList, sPath + info.Name, sname, ASubFolder);
        finally
          SysUtils.FindClose(info);
        end;
    Result := NO_ERROR;
  end
  else
    Result := GetLastError;
end;

class function Files.SetFilesToClipboard(AFileList: TStringList; const bCopy: Boolean): Boolean;
  function _GetFileList: AnsiString;
  var
    I: Integer;
  begin
    Result := AnsiString(AFileList[0]);
    for I := 1 to AFileList.Count - 1 do
      Result := Result + #0+ AnsiString(AFileList[I]);

    Result := Result + #0;
  end;

type
  PDropFiles = ^TDropFiles;
  TDropFiles = record
    pFiles: DWORD;
    pt: TPoint;
    fNC: BOOL;
    fWide: BOOL;
  end;
const
  DROPEFFECT_COPY = 1;
  DROPEFFECT_MOVE = 2;
var
  hGblFileList: HGLOBAL;
  pFileListDate: Pbyte;
  HandleDropEffect: UInt;
  hGblDropEffect: HGLOBAL;
  pdwDropEffect: PDWORD;
  l_FileListStr: AnsiString;
begin
  if AFileList.Count = 0 then
    Exit(False);
  l_FileListStr := _GetFileList;

  hGblFileList := GlobalAlloc(GMEM_ZEROINIT or GMEM_MOVEABLE or GMEM_SHARE,
    Length(l_FileListStr) + Sizeof(TDropFiles));
  pFileListDate := GlobalLock(hGblFileList);
  PDropFiles(pFileListDate)^.pFiles := Sizeof(TDropFiles);
  PDropFiles(pFileListDate)^.pt.Y := 0;
  PDropFiles(pFileListDate)^.pt.X := 0;
  PDropFiles(pFileListDate)^.fNC := False;
  PDropFiles(pFileListDate)^.fWide := False;
  inc(pFileListDate, Sizeof(TDropFiles));
  CopyMemory(pFileListDate, PAnsiChar(l_FileListStr), Length(l_FileListStr));
  GlobalUnlock(hGblFileList);
  HandleDropEffect := RegisterClipboardFormat('Preferred DropEffect ');
  hGblDropEffect := GlobalAlloc(GMEM_ZEROINIT or GMEM_MOVEABLE or GMEM_SHARE,
    Sizeof(DWORD));
  pdwDropEffect := GlobalLock(hGblDropEffect);
  if (bCopy) then
    pdwDropEffect^ := DROPEFFECT_COPY
  else
    pdwDropEffect^ := DROPEFFECT_MOVE;
  GlobalUnlock(hGblDropEffect);

  Result := OpenClipboard(0);
  if Result then
  begin
    EmptyClipboard();
    SetClipboardData(HandleDropEffect, hGblDropEffect);
    SetClipboardData(CF_HDROP, hGblFileList);
    CloseClipboard();
  end;
end;

class function Files.SetFileToClipboard(AFileName: string;
  const bCopy: Boolean): Boolean;
var
  l_List: TStringList;
begin
  l_List := TStringList.Create;
  try
    l_List.Add(AFileName);
    Result := Files.SetFilesToClipboard(l_List, bCopy);
  finally
    l_List.Free;
  end;
end;

class function Files.CopyFile(const AFrom, ATo: string;
  const AFlag: FILEOP_FLAGS; const AFormHandle: THandle): boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  ForceDirectories(ExtractFilePath(ATo));
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd := AFormHandle;
    wFunc := FO_COPY;
    pFrom := PChar(AFrom + #0);
    pTo := PChar(ATo + #0);
    fFlags := AFlag;
  end;
  try
    Result := (SHFileOperation(fo) = 0);
  except
    Result := false;
  end;
end;

class function Files.CopyFiles(const AFromFileList: TStringList; AToDir: string;
  const AFlag: FILEOP_FLAGS; const AFormHandle: THandle): boolean;
var
  fo: TSHFILEOPSTRUCT;
  I : Integer;
  l_Str: string;
begin
  if not ForceDirectories(AToDir) then
    Exit(false);

  l_Str := '';
  for I := 0 to AFromFileList.Count - 1 do
    l_Str := l_Str + AFromFileList[I] + #0;

  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd := AFormHandle;
    wFunc := FO_COPY;
    pFrom := PChar(l_Str);
    pTo := PChar(AToDir + #0);
    fFlags := AFlag;
  end;
  try
    Result := (SHFileOperation(fo) = 0);
  except
    Result := false;
  end;
end;

class function Files.CopyFilesTo(AFormHandle: THandle;
  AFromFileList: TStringList; const AFlag: FILEOP_FLAGS): Boolean;
var
  l_ToDir: string;
begin
  Result := false;
  if AFromFileList.Count>1 then
  begin
    if Files.SelectDir(AFormHandle, '', '', l_ToDir) then
      Result := Files.CopyFiles(AFromFileList, l_ToDir, AFlag, AFormHandle);
  end
  else
  if AFromFileList.Count=1 then
  begin
    Result := Files.CopyFileTo(AFormHandle, AFromFileList[0], AFlag)<>'';
  end;
end;

class function Files.CopyFileTo(AFormHandle: THandle;
  AFromFile: String; const AFlag: FILEOP_FLAGS): String;
var
  l_Dlg: TSaveDialog;
  l_Ext: String;
begin
  Result := '';

  l_Dlg := TSaveDialog.Create(nil);
  l_Dlg.FileName := ExtractFileName(AFromFile);

  l_Ext := ExtractFileExt(AFromFile);
  if l_Ext <> '' then
    l_Dlg.Filter := Format('*%s|*%s|*.*|*.*', [l_Ext, l_Ext]);

  if l_Dlg.Execute(AFormHandle) then
  begin
    if (ExtractFileExt(l_Dlg.FileName) = '') and (l_Ext <> '') then
    begin
      l_Dlg.FileName := l_Dlg.FileName + l_Ext;
    end;

    if Files.CopyFile(AFromFile, PChar(l_Dlg.FileName), AFlag) then
      Result := l_Dlg.FileName;
  end;
  l_Dlg.Free;
end;

class function Files.DeleteDirectory(const ADir: string;
  const AFlag: FILEOP_FLAGS; const AFormHandle: THandle): boolean;

  procedure _MyDeleteDirectory(const Directory: String);
  var SearchRec : TSearchRec;
      i : Integer;
  begin
    if not DirectoryExists(Directory) then
      Exit;
    try
      Files.DeleteFiles(Directory, '*.*');

      i := FindFirst(Directory+'\*.*', faDirectory, SearchRec);

      if (i=0) and (SearchRec.Attr and faDirectory > 0)  and
         (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          _MyDeleteDirectory(Directory +'\'+ SearchRec.Name );

      Repeat
        if (i=0) and (SearchRec.Attr and faDirectory > 0)  and
           (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            _MyDeleteDirectory(Directory +'\'+ SearchRec.Name );

        i := FindNext(SearchRec);
      Until i <> 0;

      SysUtils.FindClose(SearchRec);
      RmDir(Directory);
    except
    end;
  end;

var
  fo: TSHFILEOPSTRUCT;
  l_InDir: string;
begin
  l_InDir := ADir;
  while Str.IsEndWith('\', l_InDir, True) do
    Delete(l_InDir, Length(l_InDir), 1);

  if not DirectoryExists(l_InDir) then
    Exit(True);

  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd := AFormHandle;
    wFunc := FO_DELETE;
    pFrom := PChar(l_InDir + #0);
    pTo := #0#0;
    fFlags := AFlag;
  end;
  try
    Result := (SHFileOperation(fo) = 0);
    if DirectoryExists(l_InDir) then
      RmDir(l_InDir);
  except
    Result := false;
  end;

  if DirectoryExists(l_InDir) then
  begin
    _MyDeleteDirectory(l_InDir);
  end;
end;

class function Files.DeleteFile(const AFileName: string;
  const AFlag: FILEOP_FLAGS; const AFormHandle: THandle): boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  if not FileExists((AFileName)) then
    Exit(True);
  if Windows.DeleteFile(PChar(AFileName)) then
    Exit(True);

  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd := AFormHandle;
    wFunc := FO_DELETE;
    pFrom := PChar(AFileName + #0);
    pTo := #0#0;
    fFlags := AFlag;
  end;
  try
    Result := (SHFileOperation(fo) = 0);
  except
    Result := false;
  end;
  if (not Result) and FileExists(AFileName) then
  begin
    Result := RenameFile(AFileName, AFileName + '.temp'+IntToStr(GetTickCount));
  end;
end;

class function Files.FileIsInUsed(AFileName: string): boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  HFileRes := CreateFile(PChar(AFileName), GENERIC_READ or GENERIC_WRITE, 0
    { this is the trick! } , nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

class function Files.FileLinkToTrueFileName(ALnkName: string): string;
var
  link: IShellLink;
  storage: IPersistFile;
  filedata: TWin32FindData;
  buf: Array[0..MAX_PATH] of Char;
  widepath: WideString;
begin
  if not SameText(ExtractFileExt(ALnkName), '.lnk') then
    Exit(ALnkName);

  OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, link));
  OleCheck(link.QueryInterface(IPersistFile, storage));
  widepath := ALnkName;
  Result := '';
  If Succeeded(storage.Load(@widepath[1], STGM_READ)) Then
    If Succeeded(link.Resolve(GetActiveWindow, SLR_NOUPDATE)) Then
      If Succeeded(link.GetPath(buf, sizeof(buf), filedata, SLGP_UNCPRIORITY)) Then
        Result := buf;
  storage := nil;
  link:= nil;
end;

class function Files.FileToString(const AFileName: string; const AEncoding: TEncoding): string;
var
  l_List: TStringList;
begin
  l_List := TStringList.Create;
  try
    try
      if AEncoding = nil then
        l_List.LoadFromFile(AFileName)
      else
        l_List.LoadFromFile(AFileName, AEncoding);
    except
    end;
    Result := l_List.Text;
    if Result<>'' then
      Result := Copy(Result, 1, Length(Result)-2);      //最后是回车换行
  finally
    l_List.Free;
  end;
end;

class function Files.GetValidFileName(const AFileName: String): String;
var
  I: Integer;
  l_Path, l_File: string;
begin
  // '/ ', '\ ', ' < ', '> ', '| ', ': ', '? ', '* ', ' " '
  l_Path := ExtractFilePath(AFileName);
  l_File := ExtractFileName(AFileName);

  Result := '';
  for I := 1 to Length(l_File) do
    if not CharInSet(l_File[I], ['/', '\', '<', '>', '|', ':', '?', '*', '"'])
      then
      Result := Result + l_File[I];

  Result := l_Path + Result;
end;

class function Files.FindFileInFolder(const APath, AFileName: string;
  const ASubFolder: boolean): Boolean;
var
  sname: string;
  sPath: String;
  info: SysUtils.TSearchRec;
begin
  if APath[Length(APath)] <> '\' then
    sPath := APath + '\'
  else
    sPath := APath;
  if AFileName = '' then
    sname := '*.*'
  else
  if ExtractFileExt(AFileName) = AFileName then   // .xml
    sname := '*' + AFileName
  else
    sname := AFileName;
  Result := false;
  if FindFirst(sPath + sname, faAnyFile and (not faDirectory), info)=0 then
  begin
    try
      if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
        ((info.Attr and faDirectory) <> faDirectory)
      then
        Exit(True);
      while 0 = FindNext(info) do
        if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
          ((info.Attr and faDirectory) <> faDirectory)
        then
          Exit(True);
    finally
      SysUtils.FindClose(info);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
        try
          if (info.Name <> '.') and (info.Name <> '..') and
            ((info.Attr and faDirectory) = faDirectory)
          then
            if FindFileInFolder(sPath + info.Name, sname, ASubFolder) then
              Exit(True);

          while 0 = FindNext(info) do
            if (info.Name <> '.') and (info.Name <> '..') and
              ((info.Attr and faDirectory) = faDirectory)
            then
              if FindFileInFolder(sPath + info.Name, sname, ASubFolder) then
                Exit(True);
        finally
          SysUtils.FindClose(info);
        end;
  end;
end;

class function Files.FindFiles(
    const AList: TList;
    const APath: String;
    const AFileName: string = '*.*';
    const ASubFolder: boolean = False;
    const ACallBackFunc : TFindFilesCallBackFunc = nil
    ): DWORD;
var
  sfi: PSearchFileInfo;
  sname: string;
  sPath: String;
  info: SysUtils.TSearchRec;

  procedure AddToList;
  begin
    new(sfi);
    sfi.FileName := sPath + info.Name;
    sfi.ModifyTime := info.FindData.ftLastWriteTime;
    sfi.Size := info.Size;
    sfi.Tag := 0;
    AList.Add(sfi);
    if Assigned(ACallBackFunc) then
      ACallBackFunc(sfi);
  end;

begin
  if APath[Length(APath)] <> '\' then
    sPath := APath + '\'
  else
    sPath := APath;
  if AFileName = '' then
    sname := '*.*'
  else
  if ExtractFileExt(AFileName) = AFileName then   // .xml
    sname := '*' + AFileName
  else
    sname := AFileName;

  Result := FindFirst(sPath + sname, faAnyFile and (not faDirectory), info);
  if Result <> INVALID_HANDLE_VALUE then
  begin
    try
      if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
        ((info.Attr and faDirectory) <> faDirectory) then
        AddToList;
      while 0 = FindNext(info) do
        if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
          ((info.Attr and faDirectory) <> faDirectory) then
          AddToList;
    finally
      SysUtils.FindClose(info);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
        try
          if (info.Name <> '.') and (info.Name <> '..') and
            ((info.Attr and faDirectory) = faDirectory) then
            FindFiles(AList, sPath + info.Name, sname, ASubFolder, ACallBackFunc);
          while 0 = FindNext(info) do
            if (info.Name <> '.') and (info.Name <> '..') and
              ((info.Attr and faDirectory) = faDirectory) then
              FindFiles(AList, sPath + info.Name, sname, ASubFolder, ACallBackFunc);
        finally
          SysUtils.FindClose(info);
        end;
    Result := NO_ERROR;
  end
  else
    Result := GetLastError;
end;

class function Files.FindFolders(const AList: TStrings;
  const APath, AFolderName: string;
  const ASubFolder: boolean): DWORD;
var
  sname: string;
  sPath: String;
  info: TSearchRec;
begin
  if APath[Length(APath)] <> '\' then
    sPath := APath + '\'
  else
    sPath := APath;
  if AFolderName = '' then
    sname := '*.*'
  else
    sname := AFolderName;

  Result := FindFirst(sPath + sname, faDirectory, info);
  if Result <> INVALID_HANDLE_VALUE then
  begin
    try
      if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
        ((info.Attr and faDirectory) = faDirectory) then
        AList.Add(sPath + info.Name);
      while 0 = FindNext(info) do
        if (info.Name <> '') and (info.Name <> '.') and (info.Name <> '..') and
          ((info.Attr and faDirectory) = faDirectory) then
          AList.Add(sPath + info.Name);
    finally
      SysUtils.FindClose(info);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
        try
          if (info.Name <> '.') and (info.Name <> '..') and
            ((info.Attr and faDirectory) = faDirectory) then
            FindFolders(AList, sPath + info.Name, sname, ASubFolder);
          while 0 = FindNext(info) do
            if (info.Name <> '.') and (info.Name <> '..') and
              ((info.Attr and faDirectory) = faDirectory) then
              FindFolders(AList, sPath + info.Name, sname, ASubFolder);
        finally
          SysUtils.FindClose(info);
        end;
    Result := NO_ERROR;
  end
  else
    Result := GetLastError;
end;

class procedure Files.FindTree(const Path: String;
  CurrLevel, WantLevelCount: Integer; var TreeResult: TStringList);
var
  Running: boolean;
begin
  Running := True;
  FindTree(Path, CurrLevel, WantLevelCount, TreeResult, Running);
end;

class procedure Files.FindTree(const Path: String;
  CurrLevel, WantLevelCount: Integer; var TreeResult: TStringList;
  var ARunning: boolean);
  function getSpace(iCount: Integer): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to iCount do
      Result := Result + #9;
  end;

var
  fs: TSearchRec;
  fpath, InPath: String;
  Running: ^boolean;
begin
  Running := @ARunning;
  if CurrLevel >= WantLevelCount then
    Exit;

  InPath := Path;
  while (InPath <> '') and (Path[Length(InPath)] = '\') do
    InPath := Copy(InPath, 1, Length(InPath) - 1);

  if CurrLevel = 0 then
    TreeResult.Add(InPath);

  fpath := InPath + '\*.*';

  if FindFirst(fpath, faDirectory, fs) = 0 then
  begin
    if (fs.Name <> '.') and (fs.Name <> '..') then
      if (fs.Attr and faDirectory) = faDirectory then
      begin
        TreeResult.Add(getSpace(CurrLevel) + InPath + '\' + fs.Name);
        FindTree(InPath + '\' + fs.Name, CurrLevel + 1, WantLevelCount,
          TreeResult);
      end;

    while FindNext(fs) = 0 do
    begin
      if not Running^ then
        Break;

      if (fs.Name <> '.') and (fs.Name <> '..') then
        if (fs.Attr and faDirectory) = faDirectory then
        begin
          TreeResult.Add(getSpace(CurrLevel) + InPath + '\' + fs.Name);
          FindTree(InPath + '\' + fs.Name, CurrLevel + 1, WantLevelCount,
            TreeResult, Running^);
        end;
    end;
  end;

  SysUtils.FindClose(fs);
end;

class function Files.GetASaveFileName(AToDir, ADefuleFileName: string): string;
var
  I: Integer;
  l_FilePart1, l_FilePart2: string;
begin
  if AToDir[Length(AToDir)]<>'\' then
    AToDir := AToDir + '\';
  l_FilePart2 := ExtractFileExt(ADefuleFileName);
  l_FilePart1 := Copy(ADefuleFileName, 1, Length(ADefuleFileName) - Length(l_FilePart2));

  {检查文件名太长，超过255}
  while (l_FilePart1<>'') and (Length(AToDir) + Length(l_FilePart1) + Length(l_FilePart2) > 245) do
    l_FilePart1 := Copy(l_FilePart1, 1, Length(l_FilePart1)-1);
  ADefuleFileName := l_FilePart1 + l_FilePart2;

  for I := 1 to 20000 do
  begin
    if I=1 then
      Result := AToDir + ADefuleFileName
    else
      Result := Format('%s%s (%d)%s', [AToDir, l_FilePart1, I, l_FilePart2]);
    if not FileExists(Result) then
      Exit;
  end;
end;

class function Files.GetContentType(AExtName: string): string;
begin
  Result := fcRegistry.Reg.SH_GetKeyValue(HKEY_Classes_Root, AExtName,
    'Content Type');
end;

class function Files.GetFileDateTime(const FileName: string;
  FileTimeType: TFileTimeType): TDateTime;
//分别对应文件创建时间，访问时间，修改时间
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
  DosDateTime: Integer;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      case FileTimeType of
      fttCreation:
        FileTimeToLocalFileTime(FindData.ftCreationTime, LocalFileTime);
      fttLastAccess:
        FileTimeToLocalFileTime(FindData.ftLastAccessTime, LocalFileTime);
      fttLastWrite:
        FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      end;
      if FileTimeToDosDateTime(LocalFileTime, LongRec(DosDateTime).Hi,
        LongRec(DosDateTime).Lo) then
      begin
        Result := FileDateToDateTime(DosDateTime);
        Exit;
      end;
    end;
  end;
  Result := 0;
end;

class function Files.GetFileModifyTime(AFileName: string): TDateTime;
begin
  if not FileAge(AFileName, Result) then
    Result := 0;
end;

class function Files.GetFilesFromClipboard(AFileList: TStringList): Boolean;
var
  hDrop : UInt;
  l_FileName : array [0..MAX_PATH] of Char;
  i , nCount : integer;
begin
  AFileList.Clear;
  Result := OpenClipboard(0);
  if Result then
  begin
    hDrop := GetClipboardData(CF_HDROP);
    if hDrop=0 then
      Exit(False);
    nCount := DragQueryFile(hDrop , $FFFFFFFF , nil , 0);
    for i:=0 to nCount-1 do begin
      DragQueryFile(hDrop , i , @l_FileName , MAX_PATH);
      AFileList.Add(Trim(string(l_FileName)));
    end;
    CloseClipboard();
  end;
end;

class function Files.GetFileSize(AFileName: string): Int64;
var
  FindResult: Integer;
  SearchRec: TSearchRec;
begin
  FindResult := FindFirst(AFileName, faAnyFile, SearchRec);
  try
    if 0 = FindResult then
      Result := SearchRec.Size
    else
      Result := 0;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

class function Files.GetRandomFileName(const ADir: String;
  const AFileExt: String): String;
var
  l_Count: Integer;
  l_Path: string;
begin
  l_Path := Trim(ADir);
  if (l_Path <> '') and (Copy(l_Path, Length(l_Path), 1) <> '\') then
    l_Path := l_Path + '\';

  Randomize;
  l_Count := 0;
  ForceDirectories(l_Path);
  while True do
  begin
    Result := l_Path + fcStr.Str.RandomStr(12) + AFileExt;
    if not FileExists(Result) then
      Exit;
    Inc(l_Count);
    if l_Count > 20 then
      Exit;
  end;
end;

class function Files.IsFileReadOnly(AFileName: String): boolean;
var
  l_FileProity: DWORD;
begin
  try
    l_FileProity := GetFileAttributes(PChar(AFileName));
    Result := boolean(l_FileProity and FILE_ATTRIBUTE_READONLY);
  except
    Result := False;
  end;
end;

class function Files.LocalFileToURL(const AFileName: string): string;
begin
  Result  :=  StringReplace(AFileName, ' ', '%20', [rfReplaceAll]);
  Result  :=  StringReplace(Result, '\', '/', [rfReplaceAll]);
  Result  :=  'file:///' + Result;
end;

class function Files.LocaltionByExplorer(const AFileName: string): boolean;
var
  l_Explorer: string;
const
  c_Name = 'explorer.exe';
begin
  l_Explorer := fcWindows.Win.GetSystemFolderDir($0024)+'\'+ c_Name;
  if not FileExists(l_Explorer) then
    l_Explorer := c_Name;

  Result := ShellExecute(0, 'open', PChar(l_Explorer), PChar('/select,'+ AFileName), nil,  SW_SHOW) > 32;
end;

class function Files.MoveFile(const AFrom, ATo: string;
  const AFlag: FILEOP_FLAGS; const AFormHandle: THandle): boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd := AFormHandle;
    wFunc := FO_MOVE;
    pFrom := PChar(AFrom + #0);
    pTo := PChar(ATo + #0);
    fFlags := AFlag;
  end;
  try
    Result := (SHFileOperation(fo) = 0);
  except
    Result := false;
  end;
end;

class function Files.Open(const AFileName: string): boolean;
const
  C_FileExt_CanExecute : Array[0..10] of String =
    ('.bat','.cmd','.exe', '.scr','.chm','.hlp','.vbs','.js','.pif', '.reg','.lnk');
begin
  Result := ShellExecute(0, 'open', pchar(AFileName), nil, Pchar(ExtractFilePath(AFileName)),  SW_SHOW) > 32;

  if (not Result)
    and (fcStr.Arr.PosInStrArray(ExtractFileExt(AFileName), C_FileExt_CanExecute, false) =-1)
  then
    Result := Files.OpenWith(AFileName);
end;

class function Files.OpenFileDlg(AForm: TForm; const AFilterOrFileExt: String): string;
begin
  Result := SelectFile(AForm, AFilterOrFileExt, '', false, 1);
end;

class function Files.OpenWith(const AFileName: string): boolean;
begin
  //这样不对：RUNDLL32.EXE shell32, OpenAs_RunDLL "Z:\test 13\MailProxy.rar"
  //应该这样：RUNDLL32.EXE shell32, OpenAs_RunDLL Z:\test 13\MailProxy.rar
  //居然不支持双引号
  Result := ShellExecute(0,
                        'open',
                        'rundll32.exe',
                        PChar(Format('shell32.dll,OpenAs_RunDLL %s', [AFileName])),
                        nil,
                        SW_Show) > 32;

end;

class function Files.SelectFile(AForm: TForm;
  const AFilter, ADefaultDir: String;
  const ACanMultFile: Boolean;
  const AType: Integer
  ): String;
var
  l_OpenDlgVista: TFileOpenDialog;  {vista/win7 and later}
  l_OpenDlgNormal: TOpenDialog;
  l_SaveDlgVista: TFileSaveDialog;
  l_SaveDlgNormal: TSaveDialog;

  l_Handle: THandle;

  //邮件文件(*.eml)|*.eml
  //AFilter传入的可能只是 .txt 这种格式
  function _GetInFilter: string;
  begin
    if (AFilter<>'') and (AFilter[1]='.') then
      Result := '*'+ AFilter
    else
      Result := AFilter;
    if Pos('|', Result)<=0 then
      Result := Result + '|' + Result;
  end;

  procedure _AssignFileFilter(AFileTypeItems: TFileTypeItems);
  var
    I, J: Integer;
    FilterStr: string;
  begin
    FilterStr := _GetInFilter;
    J := 1;
    I := AnsiPos('|', FilterStr);
    while I <> 0 do
      with AFileTypeItems.Add do
      begin
        DisplayName := Copy(FilterStr, J, I - J);
        if not SysLocale.FarEast then
          J := AnsiStrings.PosEx('|', AnsiString(FilterStr), I + 1)
        else
        begin
          J := AnsiPos(AnsiString('|'),
            AnsiString(Copy(FilterStr, I + 1, MAXINT)));
          if J <> 0 then
            J := J + (I + 1) - 1;
        end;
        if J = 0 then
          J := Length(FilterStr) + 1;
        FileMask := Copy(FilterStr, I + 1, J - I - 1);
        Inc(J);

        if not SysLocale.FarEast then
          I := PosEx(AnsiString('|'), AnsiString(FilterStr), J)
        else
        begin
          I := AnsiPos('|', Copy(FilterStr, J, MAXINT));
          if I <> 0 then
            I := I + J - 1;
        end;
      end;
  end;

  procedure _CheckNewFileExt(var AIn: string);
  var
    l_Str, l_Ext: string;
    I: Integer;
  begin
    //用户不会输入扩展名的，所以自动加上
    if (AType = 2) and (not FileExists(AIn)) and (ExtractFileExt(AIn) = '') and (AFilter<>'') then
    begin
      l_Str := _GetInFilter;  //所有文件(*.*)|*.*|Office文档(*.doc,*.xls)|*.doc;*.xls|压缩文档(*.zip,*.rar)|*.zip;*.rar|图片文档|*.jpg;*.gif;*.jpeg;*.bmp;*.png
      l_Ext := '';
      for I := Length(l_Str) downto 1 do
        if l_Str[I]='.' then
          Break
        else
          l_Ext := l_Str[I] + l_Ext;

      AIn := AIn + '.' + l_Ext;
    end;
  end;

begin
  Result := '';
  if AForm <> nil then
    l_Handle := AForm.Handle
  else
    l_Handle := Application.Handle;

  if fcWindows.Win.GetWinVerIsVistaOrLater then
  begin
    if AType = 2 then   {Save Dialg}
    begin
      l_SaveDlgVista := TFileSaveDialog.Create(AForm);
      try
        l_SaveDlgVista.DefaultFolder := ADefaultDir;
        if ACanMultFile then
          l_SaveDlgVista.Options := l_SaveDlgVista.Options + [fdoAllowMultiSelect, fdoFileMustExist];
        _AssignFileFilter(l_SaveDlgVista.FileTypes);
        if l_SaveDlgVista.Execute(l_Handle) then
        begin
          if ACanMultFile then
            Result := l_SaveDlgVista.Files.Text
          else
          begin
            Result := l_SaveDlgVista.FileName;
            _CheckNewFileExt(Result);
          end;
        end;
      finally
        l_SaveDlgVista.Free;
      end;
    end
    else  {Open Dialog}
    begin
      l_OpenDlgVista := TFileOpenDialog.Create(AForm);
      try
        l_OpenDlgVista.DefaultFolder := ADefaultDir;
        if ACanMultFile then
          l_OpenDlgVista.Options := l_OpenDlgVista.Options + [fdoAllowMultiSelect, fdoFileMustExist];
        _AssignFileFilter(l_OpenDlgVista.FileTypes);
        if l_OpenDlgVista.Execute(l_Handle) then
        begin
          if ACanMultFile then
            Result := l_OpenDlgVista.Files.Text
          else
            Result := l_OpenDlgVista.FileName;
        end;
      finally
        l_OpenDlgVista.Free;
      end;
    end;
  end
  else
  begin
    if AType = 2 then   {Save Dialg}
    begin
      l_SaveDlgNormal := TSaveDialog.Create(AForm);
      try
        l_SaveDlgNormal.InitialDir := ADefaultDir;
        l_SaveDlgNormal.Filter := _GetInFilter;
        if ACanMultFile then
          l_SaveDlgNormal.Options := l_SaveDlgNormal.Options + [ofAllowMultiSelect, ofFileMustExist];
        if l_SaveDlgNormal.Execute(l_Handle) then
        begin
          if ACanMultFile then
            Result := l_SaveDlgNormal.Files.Text
          else
          begin
            Result := l_SaveDlgNormal.FileName;
            _CheckNewFileExt(Result);
          end;
        end;
      finally
        l_SaveDlgNormal.Free;
      end;
    end
    else
    begin
      l_OpenDlgNormal := TOpenDialog.Create(AForm);
      try
        l_OpenDlgNormal.InitialDir := ADefaultDir;
        l_OpenDlgNormal.Filter := _GetInFilter;
        if ACanMultFile then
          l_OpenDlgNormal.Options := l_OpenDlgNormal.Options + [ofAllowMultiSelect, ofFileMustExist];
        if l_OpenDlgNormal.Execute(l_Handle) then
        begin
          if ACanMultFile then
            Result := l_OpenDlgNormal.Files.Text
          else
            Result := l_OpenDlgNormal.FileName;
        end;
      finally
        l_OpenDlgNormal.Free;
      end;
    end;
  end;
end;

class function Files.RenameFile(const AFileName, ANewName: string;
  const AFlag: FILEOP_FLAGS; const AFormHandle: THandle): boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd := AFormHandle;
    wFunc := FO_RENAME;
    pFrom := PChar(AFileName + #0);
    pTo := PChar(ANewName + #0);
    fFlags := AFlag;
  end;
  try
    Result := (SHFileOperation(fo) = 0);
  except
    Result := false;
  end;
end;

class function Files.RunAs(const AExeFile, AParam: string): boolean;
var
  si: SHELLEXECUTEINFO;
begin
  zeromemory(@si, SizeOf(SHELLEXECUTEINFO));
  si.cbSize := SizeOf(SHELLEXECUTEINFO);
  si.lpVerb := 'runas';
  si.lpFile := PChar(AExeFile);
  si.nShow := SW_SHOW;
  si.fMask := SEE_MASK_NOCLOSEPROCESS;
  si.lpParameters := PChar(AParam);
  Result := ShellExecuteEx(@si);
end;


class function Files.SelectDirectory(AParentHandle: THandle;
  const Caption: string; const Root: WideString;
  var Directory: string): Boolean;

  function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  begin
    if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
      SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
    result := 0;
  end;

var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(AParentHandle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;

      with BrowseInfo do
      begin
        hwndOwner := AParentHandle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS + 64;  //+64就会出现新建文件夹的对话框
        if Directory <> '' then
        begin
          lpfn := @SelectDirCB;
          lParam := Integer(PChar(Directory));
        end;
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

class function Files.SaveFileDlg(AForm: TForm; const AFilterOrFileExt: String): string;
begin
  Result := SelectFile(AForm, AFilterOrFileExt, '', false, 2);
  if Result <> '' then
    ForceDirectories(ExtractFilePath(Result));
end;

class function Files.SelectDir(AParentHandle: THandle; const ACaption, ALabel: string;
  var ADirectory: string; const ARoot: string): Boolean;
begin
  if fcWindows.Win.GetWinVerIsVistaOrLater then
  begin
    with TFileOpenDialog.Create(nil) do
    begin
      Title := ACaption;
      FileNameLabel := ALabel;
      DefaultFolder := ADirectory;
      Options := Options + [fdoPickFolders, fdoForceFileSystem];
      Result :=  Execute(AParentHandle);
      if Result then
        ADirectory := FileName;

      Free;
    end;
  end
  else
    Result := SelectDirectory(AParentHandle, ACaption, ARoot, ADirectory);
end;

class function Files.SelectDirectory(const Caption, Root: String;
  out Directory: string; const AHandle: HWND): boolean;
  function BrowseCallbackProc(Handle: HWND; uMsg: UINT; lParam: Cardinal;
    lpData: Cardinal): Integer; stdcall;
  var
    dirbuf: array [0 .. MAX_PATH - 1] of Char;
    rc: TRect;
    hc: HWND;
    rco: TRect;
  begin
    Result := 0;
    case uMsg of
      BFFM_INITIALIZED:
        begin
          SendMessage(Handle, BFFM_SETSELECTION, 1, lpData);
          { BrowseCallbackOldWProc  := TFNWndProc(
            SetWindowLong(Handle, GWL_WNDPROC, Longint(@BrowseCallbackNewWProc)));
            SetWindowLong(Handle, GWL_STYLE,
            GetWindowLong(Handle, GWL_STYLE) or WS_SIZEBOX);// }
          if GetWindowRect(Handle, rc) then
          begin
            GetWindowRect(GetDesktopWindow, rco);
            rc.Right := 400;
            rc.Bottom := 500;
            rc.Top := (rco.Bottom - rc.Right) div 2;
            rc.Left := (rco.Right - rc.Right) div 2;
            SetWindowPos(Handle, 0, rc.Left, rc.Top, rc.Right, rc.Bottom, 0);
            // }

            GetWindowRect(Handle, rc);
            hc := FindWindowEx(Handle, 0, 'SysTreeView32', nil);
            if hc > 0 then
            begin
              SetWindowPos(hc, 0, 0, 0, rc.Right - rc.Left - 26,
                rc.Bottom - rc.Top - 140, SWP_NOMOVE or SWP_NOZORDER);
              SetWindowLong(hc, GWL_STYLE,
                GetWindowLong(hc, GWL_STYLE) or TVS_SHOWSELALWAYS);
            end;
            hc := FindWindowEx(Handle, 0, 'Button', nil);
            if hc > 0 then
            begin
              SetWindowPos(hc, 0, rc.Right - rc.Left - 180,
                rc.Bottom - rc.Top - 60, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
              hc := FindWindowEx(Handle, hc, 'Button', nil);
              SetWindowPos(hc, 0, rc.Right - rc.Left - 90,
                rc.Bottom - rc.Top - 60, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
            end;
          end;
        end;
      BFFM_SELCHANGED:
        if SHGetPathFromIDList(PItemIDList(lParam), @dirbuf) then
          SendMessage(Handle, BFFM_SETSTATUSTEXT, 0, LongInt(@dirbuf));
    end;
  end;   //*)

var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
begin
  Result := False;
  Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      with BrowseInfo do
      begin
        if AHandle > 0 then
          hwndOwner := AHandle
        else
          hwndOwner := Screen.ActiveForm.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT ; // 包含 BIF_STATUSTEXT
        lParam := Integer(PChar(Root));
        lpfn := @BrowseCallbackProc; // 回调函数
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result := ItemIDList <> nil;
      if Result then
      begin
        SHGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

class function Files.SetFileDateTime(const FileName: string;
  FileTimeType: TFileTimeType; DateTime: TDateTime): Integer;
var
  Handle: THandle;
  LocalFileTime, FileTime: TFileTime;
  DosDateTime: Integer;
  I : TFileTimeType;
  FileTimes: array[TFileTimeType] of Pointer;
begin
  Result := 0;
  DosDateTime := DateTimeToFileDate(DateTime);
  Handle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    for I := fttCreation to fttLastWrite do
      FileTimes[I] := nil;
    DosDateTimeToFileTime(LongRec(DosDateTime).Hi, LongRec(DosDateTime).Lo, LocalFileTime);
    LocalFileTimeToFileTime(LocalFileTime, FileTime);
    FileTimes[FileTimeType] := @FileTime;
    if SetFileTime(Handle, FileTimes[fttCreation], FileTimes[fttLastAccess], FileTimes[fttLastWrite]) then
      Exit;
  finally
    FileClose(Handle);
  end;
  Result := GetLastError;
end;

class procedure Files.SetFileDateTime(const FileName: string;
  DateTime: TDateTime);
var
  Handle: THandle;
  LocalFileTime, FileTime: TFileTime;
  DosDateTime: Integer;
begin
  DosDateTime := DateTimeToFileDate(DateTime);
  Handle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    DosDateTimeToFileTime(LongRec(DosDateTime).Hi, LongRec(DosDateTime).Lo, LocalFileTime);
    LocalFileTimeToFileTime(LocalFileTime, FileTime);
    if SetFileTime(Handle, @FileTime, @FileTime, @FileTime) then
      Exit;
  finally
    FileClose(Handle);
  end;
end;

class function Files.SetFileReadOnly(AFileName: String;
  AValue: boolean): boolean;
var
  l_FileProity: Cardinal;
begin
  try
    Result := True;
    if not FileExists(AFileName) then
      Exit;

    if IsFileReadOnly(AFileName) <> AValue then
    begin
      l_FileProity := GetFileAttributes(PChar(AFileName));

      if AValue then
        l_FileProity := l_FileProity or FILE_ATTRIBUTE_READONLY
      else
        l_FileProity := FILE_ATTRIBUTE_NORMAL;

      Result := SetFileAttributes(PChar(AFileName), l_FileProity);
    end;
  except
    Result := False;
  end;
end;

class function Files.StringToFile(const AFileName: string;
  AContents: RawByteString; AAppend: boolean): boolean;
var
  fs: TFileStream;
  Len: Int64;
begin
  try
    if FileExists(AFileName) then
      fs := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      fs := TFileStream.Create(AFileName, fmCreate);
    try
      if AAppend then
        fs.Seek(0, soEnd); // faster than .Position := .Size
      Len := Length(AContents);
      if Len > 0 then
        fs.WriteBuffer(AContents[1], Len);
      if not AAppend then
        fs.Size :=  Len;
    finally
      fs.Free;
    end;
    Exit(True);
  except
    Result := False;
  end;
end;

class function Files.StringToFile(const AFileName: string; AContents: string;
  const AEncoding: TEncoding): boolean;
var
  fs: TFileStream;
  l_Buffer, l_Preamble: TBytes;
  l_Encoding: TEncoding;
  l_NewSize : Int64;
begin
  try
    if FileExists(AFileName) then
      fs := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      fs := TFileStream.Create(AFileName, fmCreate);

    try
      if AEncoding = nil then
        l_Encoding := TEncoding.UTF8
      else
        l_Encoding := AEncoding;

      l_Buffer := l_Encoding.GetBytes(AContents);
      l_Preamble := l_Encoding.GetPreamble;
      l_NewSize :=  0;
      if Length(l_Preamble) > 0 then
      begin
        fs.WriteBuffer(l_Preamble[0], Length(l_Preamble));
        Inc(l_NewSize, Length(l_Preamble));
      end;
      fs.WriteBuffer(l_Buffer[0], Length(l_Buffer));
      Inc(l_NewSize, Length(l_Buffer));
      fs.Size :=  l_NewSize;
    finally
      fs.Free;
    end;
    Exit(True);
  except
    Result := False;
  end;
end;

class function Files.StringToFileName(const AStr: string): string;
const
  ReplaceStrs = '\/:*?"<>|';
var
  I: Integer;
begin
  Result := AStr;
  for I := 1 to Length(ReplaceStrs) do
    Result := StringReplace(Result, ReplaceStrs[I], '', [rfReplaceAll]);
end;

class function Files.TextFileAppendLine(const AFileName, ALine: string): Boolean;
var
  l_List: TStringList;
begin
  l_List := TStringList.Create;
  try
    l_List.Add(ALine);
    Result := TextFileAppendLines(AFileName, l_List);
  finally
    l_List.Free;
  end;
end;

class function Files.TextFileAppendLines(const AFileName: string; const ALines: TStrings): Boolean;
var
  l_F: TextFile;
  l_IsExists: Boolean;
  I: Integer;
begin
  l_IsExists := FileExists(AFileName);
  try
    if not l_IsExists then
      if not ForceDirectories(ExtractFilePath(AFileName)) then
        Exit(False);

    AssignFile(l_F, AFileName);
    try
      FileMode := 2;
      if l_IsExists then
        Append(l_F)
      else
        Rewrite(l_F);

      for I := 0 to ALines.Count -1 do
        Writeln(l_F, ALines[I]);

      Result := True;
    finally
      CloseFile(l_F);
    end;
  except
    Result := false;
  end;
end;

class function Files.UpdateFile_DoCopy(AFromFile, AToFile: string): Boolean;
var
  tmpfile: string;
begin
  if not FileExists(AFromFile) then
    Exit(False);

  ForceDirectories(ExtractFilePath(AToFile));
  Result := Files.CopyFile(AFromFile, AToFile);
  if not Result then
  begin
    tmpfile :=  ChangeFileExt(AToFile, '.old');
    if FileExists(tmpfile) then
      Files.DeleteFile(tmpfile);
    if not Files.RenameFile(AToFile, tmpfile) then
      Exit(False);
    if not ForceDirectories(ExtractFilePath(AToFile)) then
      Exit(False);
    if not Files.CopyFile(AFromFile, AToFile) then
      Exit(False);
    Result := True;
  end;
end;

class function Files.FileToBytes(const AFileName: string): TBytes;
var
  ms : TMemoryStream;
begin
  SetLength(Result, 0);
  if not FileExists(AFileName) then Exit;
  try
    ms  :=  TMemoryStream.Create;
    try
      ms.LoadFromFile(AFileName);
      ms.Position :=  0;
      SetLength(Result, ms.Size);
      CopyMemory(@Result[0], ms.Memory, ms.Size);
    finally
      ms.Free;
    end;
  except
  end;
end;

class function Files.BytesToFile(const ABytes: TBytes;
  const AFileName: string): Boolean;
var
  fs: TFileStream;
begin
  try
    if FileExists(AFileName) then
      fs := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      fs := TFileStream.Create(AFileName, fmCreate);

    try
      fs.Write(ABytes[0], Length(ABytes));
      fs.Size :=  Length(ABytes);
    finally
      fs.Free;
    end;
    Result  :=  True;
  except
    Result := False;
  end;
end;

class function Files.CopyFileTo(AForm: TForm; AFromFile: String;
  const AFlag: FILEOP_FLAGS): String;
var
  l_Handle: THandle;
begin
  if Assigned(AForm) then
    l_Handle := AForm.Handle
  else
  if Screen.ActiveForm<>nil then
    l_Handle := Screen.ActiveForm.Handle
  else
    l_Handle := 0;
  Result := Files.CopyFileTo(l_Handle, AFromFile, AFlag);
end;

class function Files.FileCreate(const AFileName: string): boolean;
var
  f: Textfile;
begin
  if not DirectoryExists(ExtractFileDir(AFileName)) then
    ForceDirectories(ExtractFileDir(AFileName));

  if not FileExists(AFileName) then
  begin
    AssignFile(f, AFileName);
    FileMode := 2;
    try
      Rewrite(f);
    except
      CloseFile(f);
      Result := False;
      Exit;
    end;

    write(f, '');
    CloseFile(f);
  end;

  Result := True;
end;

class procedure Files.MakeLink(proName, proParam, lnkName, Description: string);
// proname,lnkname均为全路径名
var
  aObj: IUnknown;
  MyLink: IShellLink;
  MyPFile: IPersistFile;
  WFileName: WideString;
begin
  aObj := CreateComObject(CLSID_ShellLink);
  MyLink := aObj as IShellLink;
  MyPFile := aObj as IPersistFile;

  with MyLink do // 操作IShellLink类
  begin
    // 设置显示窗口的类型
    SetShowCmd(SW_NORMAL);
    { 对DOS程序,一般建议使用SetShowCmd(SW_SHOWMAXIMIZED); }
    // 设置命令行参数
    SetArguments(PChar(proParam));
    // 设置描述
    SetDescription(PChar(Description));
    // 设置程序名称[全路径,如果在系统Path中,可直接为程文件名]
    SetPath(PChar(proName));
    // 设置工作目录
    SetWorkingDirectory(PChar(ExtractFilePath(proName)));
  end;

  // 取.LNK文件的位置
  CreateDir(ExtractFilePath(lnkName));
  // 建立快捷方式: .LNK文件
  WFileName := lnkName; // 将一个String赋给WideString，转换过程由Delphi自动完成
  MyPFile.Save(PWChar(WFileName), False);
end;

class procedure Files.DeleteDirectory2(const Directory: string);
var
  SearchRec: TSearchRec;
  I: Integer;
begin
  if not DirectoryExists(Directory) then
    Exit;

  try
    DeleteFiles(Directory, '*.*');

    I := FindFirst(Directory + '\*.*', faDirectory, SearchRec);

    if (I = 0) and (SearchRec.Attr and faDirectory > 0) and
      (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      DeleteDirectory2(Directory + '\' + SearchRec.Name);

    Repeat
      if (I = 0) and (SearchRec.Attr and faDirectory > 0) and
        (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        DeleteDirectory2(Directory + '\' + SearchRec.Name);

      I := FindNext(SearchRec);
    Until I <> 0;

    SysUtils.FindClose(SearchRec);
{$I-}
    RmDir(Directory);
  except
  end;
end;

class procedure Files.DeleteFiles(Dir, Filetype: String);
var
  SearchRec: TSearchRec;
begin
  while (Dir <> '') and (Dir[Length(Dir)] = '\') do
    Dir := Copy(Dir, 1, Length(Dir) - 1);

  FindFirst(Dir + '\' + Filetype, $00000020, SearchRec);

  if SearchRec.Name = '' then
  begin
    SysUtils.FindClose(SearchRec);
    Exit;
  end;

  DeleteFile(Dir + '\' + SearchRec.Name);

  while FindNext(SearchRec) = 0 do
    DeleteFile(Dir + '\' + SearchRec.Name);

  SysUtils.FindClose(SearchRec);
end;

{ FileVersion }

class function FileVersion.CompareVersion(const AOldVersion,
  ANewVersion: string): Integer;
// 比较版本 格式 0.0.0.0
// 0=相同 -1=旧比新大 -2版本错误 1, 10, 100, 1000=新比旧大

type
  TVerInfoArray = array[0..3] of Integer;

  function _StrToVerArray(const AStr : string; var AVerArr : TVerInfoArray) : Integer;
  var
    i : Integer;
    s : string;
  begin
    s :=  AStr + '.';
    for Result  :=  0 to 3 do
    begin
      i :=  Pos('.', s);
      AVerArr[Result] :=  StrToIntDef(Copy(s, 1, i-1), -1);
      if AVerArr[Result] < 0 then Exit;
      Delete(s, 1, i);
    end;
    Result  :=  4;
  end;

  function _Comp(const AOldVer: TVerInfoArray; const ANewVer: TVerInfoArray;
    const AIndex: Integer): Integer;
  begin
    if AOldVer[AIndex] = ANewVer[AIndex] then
    begin
      if AIndex = 3 then
        Result := 0
      else
        Result := _Comp(AOldVer, ANewVer, AIndex + 1);
    end
    else if AOldVer[AIndex] > ANewVer[AIndex] then
      Result := -1
    else
      Result := StrToInt('1' + LeftStr('000', 3 - AIndex));
  end;

var
  vOld: TVerInfoArray;
  vNew: TVerInfoArray;
begin
  if _StrToVerArray(AOldVersion, vOld) <> 4 then Exit(-2);
  if _StrToVerArray(ANewVersion, vNew) <> 4 then Exit(-2);
  Result := _Comp(vOld, vNew, 0);
end;

class function FileVersion.GetFileVersion(const AFileName: String): String;
var
  info: TVersionInfo;
begin
  Result := '0.0.0.0';
  if not FileExists(AFileName) then
    Exit;
  if GetFileVersionInfomation(AFileName, info) then
    Result := info.FileVersion;

  // 返回的文件版本的格式是： 1, 1, 0, 1014
  Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;

class function FileVersion.GetFileVersionInfomation(const FileName: string;
  var info: TVersionInfo; const UserDefine: string): boolean;
const
  SFInfo = '\StringFileInfo\';
var
  VersionInfo: Pointer;
  InfoSize: DWORD;
  InfoPointer: Pointer;
  Translation: Pointer;
  VersionValue: string;
  unused: DWORD;
begin
  unused := 0;
  Result := False;
  InfoSize := GetFileVersionInfoSize(PChar(FileName), unused);
  if InfoSize > 0 then
  begin
    GetMem(VersionInfo, InfoSize);
    Result := GetFileVersionInfo(PChar(FileName), 0, InfoSize, VersionInfo);
    if Result then
    begin
      VerQueryValue(VersionInfo, '\VarFileInfo\Translation', Translation,
        InfoSize);
      if Assigned(Translation) then
        VersionValue := SFInfo + IntToHex(LoWord(LongInt(Translation^)), 4)
          + IntToHex(HiWord(LongInt(Translation^)), 4) + '\'
      else
        VersionValue := SFInfo;
      VerQueryValue(VersionInfo, PChar(VersionValue + 'CompanyName'),
        InfoPointer, InfoSize);
      info.CommpanyName := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'FileDescription'),
        InfoPointer, InfoSize);
      info.FileDescription := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'FileVersion'),
        InfoPointer, InfoSize);
      info.FileVersion := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'InternalName'),
        InfoPointer, InfoSize);
      info.InternalName := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'LegalCopyright'),
        InfoPointer, InfoSize);
      info.LegalCopyright := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'LegalTrademarks'),
        InfoPointer, InfoSize);
      info.LegalTrademarks := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'OriginalFileName'),
        InfoPointer, InfoSize);
      info.OriginalFileName := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'ProductName'),
        InfoPointer, InfoSize);
      info.ProductName := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'ProductVersion'),
        InfoPointer, InfoSize);
      info.ProductVersion := string(PChar(InfoPointer));
      VerQueryValue(VersionInfo, PChar(VersionValue + 'Comments'), InfoPointer,
        InfoSize);
      info.Comments := string(PChar(InfoPointer));
      if VerQueryValue(VersionInfo, '\', InfoPointer, InfoSize) then
        info.VsFixedFileInfo := TVSFixedFileInfo(InfoPointer^);
      if UserDefine <> '' then
      begin
        if VerQueryValue(VersionInfo, PChar(VersionValue + UserDefine),
          InfoPointer, InfoSize) then
          info.UserDefineValue := string(PChar(InfoPointer));
      end;
    end;
    FreeMem(VersionInfo);
  end;
end;

{ FileStr }

class function FileStr.CompareVersionNumber(AVer1, AVer2: string): Integer;
// 对比文件版本号，
// 1= Aver1 > AVer2;
// -1 =
  procedure _PraseToList(const Str: String; const List: TStringList);
  var
    I: Integer;
    l_Part: String;
  begin
    List.Clear;
    l_Part := '';
    for I := 1 to Length(Str) do
    begin
      if Str[I] = '.' then
      begin
        List.Add(l_Part);
        l_Part := '';
      end
      else
      begin
        l_Part := l_Part + Str[I];
      end;
    end;
    if l_Part <> '' then
      List.Add(l_Part);
  end;

var
  l_List1, l_List2: TStringList;
  I, l_Id1, l_Id2: Integer;
begin
  l_List1 := TStringList.Create;
  l_List2 := TStringList.Create;
  try
    _PraseToList(AVer1, l_List1);
    _PraseToList(AVer2, l_List2);

    for I := 0 to ifThen(l_List1.Count > l_List2.Count, l_List2.Count - 1,
      l_List1.Count - 1) do
    begin
      l_Id1 := StrToIntDef(l_List1[I], 0);
      l_Id2 := StrToIntDef(l_List2[I], 0);

      if l_Id1 <> l_Id2 then
      begin
        Result := ifThen(l_Id1 > l_Id2, 1, -1);
        Exit;
      end;
    end;

    if l_List1.Count = l_List2.Count then
      Result := 0
    else
      Result := ifThen(l_List1.Count > l_List2.Count, 1, -1);
  finally
    l_List1.Free;
    l_List2.Free;
  end;
end;

class function FileStr.ExtractFileFirstName(AFileName: string): string;
var
  l_Ext: string;
begin
  Result := ExtractFileName(AFileName);
  l_Ext := ExtractFileExt(Result);
  if Length(l_Ext)>0 then
    Result := Copy(Result, 1, Length(Result)-Length(l_Ext));
end;

class function FileStr.FileNameIsBeginWithPath(AFileName,
  APath: string): Boolean;
begin
  if APath = '' then Exit(False);

  if Str.IsEndWith('\', APath, True) then
    Result := Str.IsStartWith(APath, AFileName, false)
  else
    Result := Str.IsStartWith(APath+'\', AFileName, false)
end;

class function FileStr.FilePathToFileDir(AFilePath: string): string;
begin
  if (AFilePath<>'')
    and Str.IsEndWith('\', AFilePath, false) then
  begin
    Result := Copy(AFilePath, 1, Length(AFilePath)-1);
  end
  else
    Result := AFilePath;
end;

class function FileStr.FilterNoEffectChar(FileName: String): String;
  function GetANewFileName(ATempPath: String): String;
  var
    l_Idx: Integer;
    l_Ext, l_LastFileName, l_NewFileName: String;
    l_Handle: Integer;
  begin
    l_Ext := ExtractFileExt(FileName);
    l_LastFileName := ATempPath + '1' + l_Ext;

    for l_Idx := 1 to Length(FileName) - Length(l_Ext) do
    begin
      if l_Idx > 100 then
        Break;

      l_NewFileName := ATempPath + Copy(FileName, 1, l_Idx) + l_Ext;
      if FileExists(l_NewFileName) then
        Continue;

      l_Handle := FileCreate(l_NewFileName);
      if l_Handle = -1 then
      begin
        Break;
      end
      else
      begin
        FileClose(l_Handle);
        DeleteFile(PWideChar(l_NewFileName));
        l_LastFileName := l_NewFileName;
      end;
    end;

    Result := ExtractFileName(l_LastFileName);
  end;

var
  I, ii: Integer;
  l_TmpFile, l_TempDir: String;
  l_Find: boolean;
  l_Handle: Integer;
  l_ansifilename: AnsiString;
const
  NoEffectChar = '\/:*?"<>|';
  C_BasicChars =
    'abcdefghijklmnopqrstuvwxyz~!@#$%^&()_+`-={}[];''.,1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ';
begin
  Result := FileName;
  for I := 1 to Length(NoEffectChar) do
  begin
    ii := FastPosNoCase(FileName, NoEffectChar[I], Length(FileName), 1, 1);
    while ii > 0 do
    begin
      Delete(FileName, ii, 1);
      ii := Pos(NoEffectChar[I], FileName);
    end;
  end;

  // 看看有没有特殊字符
  l_Find := False;
  for I := 1 to Length(FileName) do
  begin
    if FastPosNoCase(C_BasicChars, FileName[I], Length(C_BasicChars), 1, 1)
      <= 0 then
    begin
      l_Find := True;
      Break;
    end;
  end;
  if not l_Find then
  begin
    Result := FileName;
    Exit;
  end;

  // 如果有回车换行，则取前面的......
  if FastPosNoCase(FileName, #13, Length(FileName), 1, 1) > 1 then
    FileName := Str.GetTokenBeforeChar(FileName, #13, True, True);
  if FastPosNoCase(FileName, #10, Length(FileName), 1, 1) > 1 then
    FileName := Str.GetTokenBeforeChar(FileName, #10, True, True);

  try
    l_TempDir := Win.GetTempPath + '\DreamMail\';
    if not ForceDirectories(l_TempDir) then
      Exit;

    l_TmpFile := l_TempDir + FileName;
    if FileExists(l_TmpFile) then
      Exit;

    // 尝试在硬盘上创建一个文件，如果创建成功，则表示这个文件是可以用的。
    l_Handle := FileCreate(l_TmpFile);
    if l_Handle = -1 then
    begin
      // 这里引入charset的问题是，当来信的字符集是其他字符集的时候，
      // 比如jp，可能附件名称是乱码，导致了无法打开和保存附件。
      // l_FindLargeChar := false;

      l_ansifilename := AnsiString(FileName);
      I := 1;
      while I < Length(l_ansifilename) do
      begin
        if (l_ansifilename[I] >= #$81) then
        begin
          if l_ansifilename[I + 1] < #$40 then // 文件名肯定错误了。。。。。。
          begin
            l_ansifilename[I] := '_';
            Inc(I);
          end
          else
            Inc(I, 2);
        end
        else
        begin
          Inc(I);
        end;
      end;
      FileName := string(l_ansifilename);

      // 再创建一次，如果不成功，则生成一个默认的文件
      l_TmpFile := l_TempDir + FileName;
      if FileExists(l_TmpFile) then
        Exit;

      l_Handle := FileCreate(l_TmpFile);
      if l_Handle = -1 then
      begin
        FileName := GetANewFileName(l_TempDir);
      end
      else
      begin
        FileClose(l_Handle);
        DeleteFile(PWideChar(l_TmpFile));
      end;
    end
    else
    begin
      FileClose(l_Handle);
      DeleteFile(PWideChar(l_TmpFile));
    end;
  finally
    Result := FileName;
  end;
end;

class function FileStr.FormatFileSize(AFileSize: Int64;
  const ADecimalLenOfM: Integer;
  const ADecimalLenOfK: Integer;
  const ASpaceCount: Integer): String;
  function GetDecimal(AValue: Integer): string;
  begin
    case AValue of
      0: Result := '0';
      2:
        Result := '0.00';
      3:
        Result := '0.000';
    else
      Result := '0.0';
    end;
  end;
  function _GetSpace: string;
  begin
    case ASpaceCount of
      0: Result := '';
      1: Result := ' ';
    else
      Result := ' ';
    end;
  end;
begin
  if AFileSize <= 0 then
    Result := '0'+_GetSpace+'K'
  else if AFileSize <= 1024 then
  begin
    if ADecimalLenOfK=0 then
      Result := '1'+_GetSpace+'K'
    else
      Result := FormatFloat(GetDecimal(ADecimalLenOfK) + _GetSpace+'K', AFileSize / 1024)
  end
  else if AFileSize < 1024 * 1024 then
    Result := FormatFloat(GetDecimal(ADecimalLenOfK) + _GetSpace+'K', AFileSize / 1024)
  else
    Result := FormatFloat(GetDecimal(ADecimalLenOfM) + _GetSpace+'M', AFileSize / (1024 * 1024));
end;

class function FileStr.FormatFileSizeExt(AFileSize: Int64): String;
begin
  if AFileSize <= 0 then
    Result := '0 K'
  else if AFileSize <= 1024 then
     Result := '1 K'
  else
    Result := Format('%0.0n K', [AFileSize / 1024]);
end;

class function FileStr.IsSameFileName(AFileName1, AFileName2: string): boolean;
var
  l_s1, l_s2: string;
begin
  Result := True;
  if SameFileName(AFileName1, AFileName2) then
    Exit;

  l_s1 := StringReplace(AFileName1, '\\', '\', [rfReplaceAll]);
  l_s2 := StringReplace(AFileName2, '\\', '\', [rfReplaceAll]);

  {如果 AFileName1, AFileName2 是网络文件夹， 2个斜杠开头的，则直接比较}
  if fcStr.Str.IsStartWith('\', l_s1, false) or fcStr.Str.IsStartWith('\', l_s2, False) then
    Exit( SameText(l_s1, l_s2) );

  {其他的比较}
  l_s1 := ExtractShortPathName(l_s1);
  l_s2 := ExtractShortPathName(l_s2);
  Result := SameFileName(l_s1, l_s2);
end;

end.
