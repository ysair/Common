unit ZsFileUtils;

interface

uses
  Classes, Forms, Windows, Controls, ShellAPI;

//Tested

type
  TFileTimeType = (fttCreation, fttLastAccess, fttLastWrite);
  //分别对应文件创建时间，访问时间，修改时间
  function GetFileDateTime(const FileName: string; FileTimeType: TFileTimeType): TDateTime;
  function SetFileDateTime(const FileName: string; FileTimeType: TFileTimeType; DateTime: TDateTime): Integer;

function SaveStrToFile(FileName, Str : String; bShowMessage : Boolean = false) : Boolean;

function ZsGetFileSize(FileName: String): Integer;

function ZsGetFileType(ExtName: String) :String;

function IsFileReadOnly(AFileName: String): Boolean;

function SetFileReadOnly(AFileName: String; bReadOnly:Boolean): Boolean;

//判断一个目录是否能修改
function DirectorCanModify(ADir: String): Boolean;

//Untested
procedure MyDeleteDirectory(const Directory: String);
procedure MyDeleteFiles(Dir, Filetype: String);
procedure MyFindFile(const Path, FileType: String; var FileResult: TStringList);
procedure MyFindTree(const Path : String; CurrLevel, WantLevelCount : Integer; var TreeResult : TStringList); overload;
procedure MyFindTree(const Path : String; CurrLevel, WantLevelCount : Integer; var TreeResult : TStringList; var ARunning :boolean ); overload;

function ZsCopyFile(AFromFile, AToFile: String; const AOptions: FILEOP_FLAGS = FOF_SILENT): Boolean;
function ZsMoveFile(AFromFile, AToFile: String; const AOptions: FILEOP_FLAGS = FOF_SILENT): Boolean;

function GetWindowsTempDirectory: string;
function ZsGetWindowsDirectory: string;

function MySelectDirectory(theForm:TForm; const Caption: string;
  const Root: WideString; var Directory: string): Boolean;

function MyCopyFiles(FromFolder, ToFolder, FileType : String): Boolean;

function GetFilesImageToList(ImageList: TImageList; FileName: String;
    IsSmallIcon: Boolean): Boolean; overload;

function GetFilesImageToList(ImageList: TImageList; FileList: TStringList;
    IsSmallIcon: Boolean): Boolean; overload;

function GetRandomFileName(const InitDir:String):String;

function MyFileCreate(fn: string): Boolean;
procedure GetAllFileToList(theDir, sFileType:String; var toList: TStringList);

procedure PlaySoundFile(fileName: string);

function ExecuteFile( fileName: string): Boolean;

procedure MakeLink(proName, proParam, lnkName, Description: string);

function CopyAExistsFileToAntherFile(AForm: TForm; AFromFile: String): String;

function IsFileInUse(fName : string) : Boolean;

function FileContentToString(AFileName: String): String;



implementation

uses
  SysUtils, Math, shlobj, ActiveX, Registry, Graphics, JclFileUtils,
  MMSystem, ComObj, JclStrings, MyDlg, fcFile;


function CopyAExistsFileToAntherFile(AForm: TForm; AFromFile: String): String;
var
  l_Dlg: TMySaveDialog;
  l_Ext: String;
begin
  Result := '';

  l_Dlg := TMySaveDialog.Create(AForm);
  l_Dlg.FileName := ExtractFileName(AFromFile);

  l_Ext := ExtractFileExt(AFromFile);
  if l_Ext <> '' then
    l_Dlg.Filter := Format('*%s|*%s|*.*|*.*', [l_Ext, l_Ext]);

  if l_Dlg.Execute then
  begin
    if (ExtractFileExt(l_Dlg.FileName) = '') and (l_Ext<>'') then
    begin
      l_Dlg.FileName := l_Dlg.FileName + l_Ext;
    end;

    if ZsCopyFile(AFromFile, PChar(l_Dlg.FileName)) then
      Result := l_Dlg.FileName;
  end;
  l_Dlg.Free;
end;

function SaveStrToFile(FileName, Str : String; bShowMessage:boolean) : Boolean;
var
  parentDir : String;
begin
  try
    parentDir := ExtractFileDir(ExpandFileName(FileName));
    ForceDirectories(parentDir);
    if FileExists(FileName) then
      Files.DeleteFile(FileName);

    if not FileExists(FileName) then
    begin
      JclStrings.StringToFile(FileName, Str);
      Result := true;
    end
    else
      Result := false;
  except
    Result := false;
    if bShowMessage then
      MessageBox(0,PChar('Write File Error: ' + FileName),'Error',MB_OK + MB_ICONERROR);
  end;
end;

//------------------------------------------------------------------------------
//得到文件的大小，单位是字节
function ZsGetFileSize(FileName: String): Integer;
var
  FindResult: Integer;
  SearchRec : TSearchRec;
begin
  FindResult := FindFirst(FileName, faAnyFile, SearchRec);
  try
    if 0 = FindResult then
      Result := SearchRec.Size
    else
      Result := 0;
  finally
    SysUtils.FindClose( SearchRec);
  end;
end;

function ZsGetFileType(ExtName: String) :String;
var
  RegF : TRegistry;
begin
  RegF:=TRegistry.Create;
  try
    RegF.RootKey := HKEY_Classes_Root;
    RegF.OpenKey(ExtName,False);
    Result := RegF.ReadString('Content Type');
    if Result = '' then
      Result := RegF.ReadString('');
    RegF.CloseKey;
  finally
    RegF.Free;
  end;
end;

function IsFileReadOnly(AFileName: String): Boolean;
var
  l_FileProity: DWORD;
begin
  try
    l_FileProity := GetFileAttributes(Pchar(AFileName));
    Result := Boolean(l_FileProity and FILE_ATTRIBUTE_READONLY);
  except
    Result := false;
  end;
end;

function SetFileReadOnly(AFileName: String; bReadOnly:Boolean): Boolean;
var
  l_FileProity: DWORD;
begin
  try
    Result := True;
    if not FileExists(AFileName) then
      Exit;
       
    if IsFileReadOnly(AFileName) <> bReadOnly then
    begin
      l_FileProity := 0;
      if bReadOnly then
        Result := SetFileAttributes(Pchar(AFileName),l_FileProity or FILE_ATTRIBUTE_READONLY)
      else
        Result := SetFileAttributes(Pchar(AFileName), l_FileProity or FILE_ATTRIBUTE_NORMAL)
    end;
  except
    Result := false;
  end;
end;

procedure MyDeleteDirectory(const Directory: String);
begin
  Files.DeleteDirectory2(Directory);
end;

//删除一个目录下的所有文件，fileType指明TSearchRec能够识别的文件类型
// MyDeleteFiles('c:\', '*.bak')
procedure MyDeleteFiles(Dir, Filetype: String);
begin
  Files.DeleteFiles(Dir, FileType);
end;

//function  MyDeleteFile(AFileName: String): Boolean;
//begin
//  Result := True;
//  if FileExists(AFileName) then
//  begin
//    Result := DeleteFile(AFileName);
//    if (not Result) then
//    begin
//      if IsFileReadOnly(AFileName) then
//        SetFileReadOnly(AFileName, false);
//      Result := JclFileUtils.FileDelete(AFileName);
//    end;
//  end;
//end;

procedure MyFindFile(const Path, FileType: String; var FileResult: TStringList);
var
  fs    : TsearchRec;
  fpath : String;
begin
  fpath := Path + '\' + FileType;

  if FindFirst(fpath, faAnyFile, fs) = 0 then
  begin
    if (fs.Name<>'.') and (fs.Name<>'..') then
      if (fs.Attr and faDirectory) <> faDirectory then
        FileResult.add( Path + '\' + fs.Name);

    while findnext(fs)=0 do
    begin
      if (fs.Name<>'.')and(fs.Name<>'..') then
        if (fs.Attr and faDirectory) <> faDirectory then
          FileResult.add( Path + '\' + fs.Name);
    end;
  end;

  SysUtils.FindClose(fs);
end;

//  MyFileTree ('c:', 0, 1000, List );
// WantLevelCount是想要得到几层目录，你设置为1000好了。
procedure MyFindTree(const Path : String; CurrLevel, WantLevelCount : Integer;
  var TreeResult : TStringList);
begin
  Files.FindTree(Path, CurrLevel,WantLevelCount,TreeResult);
end;

procedure MyFindTree(const Path : String; CurrLevel, WantLevelCount : Integer;
  var TreeResult : TStringList; var ARunning :boolean );
begin
  Files.FindTree(Path, CurrLevel,WantLevelCount,TreeResult, ARunning);
end;

//------------------------------------------------------------------------------
// 得到windows的temp目录
//  DWORD GetTempPath(
//       DWORD nBufferLength,  // size, in characters, of the buffer
//      LPTSTR lpBuffer   // address of buffer for temp. path
//       );
// 返回结果的最后一个字符不是“\”
function GetWindowsTempDirectory: string;
var dir: array[0..1023] of char;
   resstr: string;
begin
  if Boolean( GetTempPath(1023, dir) ) then
     resstr := StrPas(dir)
  else
  begin
      GetWindowsDirectory(dir, 1023) ;
      resstr := strpas(dir)+'\temp';
      ForceDirectories(resstr);
  end;

  if resstr <> '' then
     if resstr[Length(resstr)] = '\' then
        resstr := copy(resstr, 1, Length(resstr)-1);

  result := resstr;
  if not DirectoryExists( resstr) then
    ForceDirectories (resstr);
end;

function ZsGetWindowsDirectory: string;
var
  l_dir: array[0..1023] of char;
begin
  GetWindowsDirectory(l_dir, 1023);
  Result := StrPas(l_dir);
end;

//------------------------------------------------------------------------------
//选择目录对话框
  function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  begin
    if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
      SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
    result := 0;
  end;

function MySelectDirectory(theForm:TForm; const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
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
        IDesktopFolder.ParseDisplayName(theForm.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;

      with BrowseInfo do
      begin
        hwndOwner := theForm.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS + 64;  //+64就会出现新建文件夹的对话框
        if Directory <> '' then
        begin
          lpfn := SelectDirCB;
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

//------------------------------------------------------------------------------
// MyCopyFile('c:\tss', 'D:\ddd', 'Mail*.*');
function MyCopyFiles(FromFolder, ToFolder, FileType : String): Boolean;
var
   SearchRec : TSearchRec;
begin
  Result := False;
  ForceDirectories(ToFolder); 

  if FindFirst(FromFolder+'\'+ FileType, $00000020  , SearchRec) <> 0 then
  begin
    //没有文件，
    Result := True;
    Exit;
  end;

  if (SearchRec.Name <> '') and ( SearchRec.Attr and faDirectory = 0) and
     (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
  begin
    try
       CopyFile( Pchar(FromFolder + '\' + SearchRec.Name),
                Pchar(ToFolder + '\' + SearchRec.Name),
                False);
    except
      Exit;
    end;
  end;

  while FindNext(SearchRec) = 0 do
  begin
     if (SearchRec.Name <> '') and (SearchRec.Attr and faDirectory = 0) and
       (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
     begin
      try
         CopyFile( Pchar(FromFolder + '\' + SearchRec.Name),
                   Pchar(ToFolder + '\' + SearchRec.Name),
                   False);
      except
        Exit;
      end;
     end;
  end;

  SysUtils.FindClose(SearchRec);

  Result := True;
end;

//------------------------------------------------------------------------------
//把FileList中文件的图标加到ImageList中去
function GetFilesImageToList(ImageList: TImageList; FileName: String;
    IsSmallIcon: Boolean): Boolean;
var
  F_icon: TIcon;
  shinfo:TSHFILEINFO;
  isCreateFile: Boolean;
  Flag: Integer;
begin
  F_icon := TIcon.Create;

  if FileName <> '' then if fileName[1] = '"' then Delete(FileName, 1, 1);
  if FileName <> '' then
    if fileName[Length(fileName)] = '"' then
      FileName := Copy(FileName, 1, Length(fileName) -1);

  isCreateFile := false;
  if not FileExists(fileName) then
  begin
    fileName := GetWindowsTempDirectory + '\' + ExtractFileName(fileName) ;
    if not FileExists ( fileName) then isCreateFile := MyFileCreate(fileName);
    if not FileExists (FileName) then
    begin
      FileName := GetWindowsTempDirectory + '\temp'+FormatDateTime('hhnnsszzz', now)+ExtractFileExt(FileName);
      isCreateFile := MyFileCreate(fileName);
    end;
  end;

  if isSmallIcon then Flag:=(SHGFI_SMALLICON or SHGFI_ICON)
  else Flag:=(SHGFI_LARGEICON or SHGFI_ICON);

  SHGetFileInfo(Pchar(fileName),0,shinfo,Sizeof(shinfo),Flag);

  F_icon.Handle := shinfo.hIcon;
  ImageList.AddIcon(F_icon);   //把ICO装入ImageList中

  if isCreateFile then
    Files.DeleteFile(fileName);

  F_Icon.Free;
  Result := True;
end;

function GetFilesImageToList(ImageList: TImageList; FileList: TStringList;
    IsSmallIcon: Boolean): Boolean;
var i : integer;
begin
  ImageList.Clear;
  if IsSmallIcon then ImageList.Height := 16
  else ImageList.Height := 32;
  ImageList.Width := ImageList.Height;

  for i := 0 to FileList.Count -1 do
    GetFilesImageToList(ImageList, FileList[i], IsSmallIcon);

  result := True;
end;                

function GetRandomFileName(const InitDir:String):String;
var
  sFileName :String;
begin
  ForceDirectories(InitDir ); 
  repeat
    sFileName := InitDir + '\' + FormatDateTime('yyyymmddhhnnsszzz',now);
    Sleep(1);
  until not FileExists(sFileName);
  result := sFileName;
end;


//------------------------------------------------------------------------------
// 不使用系统的FileCreate的原因是FileCreate会返回一个Integer变量的Handle，如果这个变量没有释放
// 则后面的所有关于这个文件的操作都将失败
function MyFileCreate(fn: string): Boolean;
var f : Textfile;
begin
  if not DirectoryExists(ExtractFileDir( fn)) then
    ForceDirectories ( ExtractFileDir( fn));

  if not fileExists( fn) then
  begin
    AssignFile( f, fn);
    FileMode := 2;
    try
      Rewrite(F);
    except
      CloseFile(f);
      Result := false;
      Exit;
    end;

    write(F, '');
    closeFile(F);
  end;

  Result := True;
end;

//------------------------------------------------------------------------------
//找到某些符合类型的文件到List中
procedure GetAllFileToList(theDir, sFileType:String; var toList: TStringList);
var SearchRec: TSearchRec;
begin
  if FindFirst(theDir+'\*.'+sFileType, faAnyFile, SearchRec) = 0 then
  begin
    toList.Add(theDir+'\'+SearchRec.Name);

    while FindNext(SearchRec) = 0 do
      toList.Add(theDir+'\'+SearchRec.Name);

    Sysutils.FindClose(SearchRec);
  end;
end;                                                

//------------------------------------------------------------------------------
procedure PlaySoundFile(fileName: string);
begin
  if not FileExists(fileName) then Exit;
  try
    sndPlaySound(Pchar(FileName), snd_ASync);
  except
  end;
end;

function ExecuteFile( fileName: string): Boolean;
begin
  result := ShellExecute (Application.Handle, 'open', pchar(fileName), nil, Pchar(ExtractFilePath(fileName)),  sw_show) > 32;

  if not Result then
    Result := ShellExecute(Application.Handle,
                          'open',
                          'rundll32.exe',
                          PChar(Format('shell32.dll,OpenAs_RunDLL "%s"', [fileName])),
                          nil,
                          SW_Show) > 32;

(*
    Pchar('rundll32.exe shell32.dll,OpenAs_RunDLL "'+fileName+'"'), nil, nil, sw_Show) > 32;

    ShellExecute(0, 'open', 'rundll32.exe',                            { do not localize}
      PChar(Format('shell32.dll,OpenAs_RunDLL %s', [FFileName])), nil, { do not localize}
      SW_SHOW);
*)
end;

procedure MakeLink(proName, proParam, lnkName, Description: string);
begin
  Files.MakeLink(proName, proParam, lnkName, Description);
end;

function ZsCopyFile(AFromFile, AToFile: String; const AOptions: FILEOP_FLAGS = FOF_SILENT): Boolean;
{from 和 to 实际上是“多个null结束的字符串”，可以带多个文件名同时做。
比如： 'f1.txt'#0'f2.txt'#0'f3.txt'#0#0 可以同时做 f1.txt和f2.txt和f3.txt
3个文件, 最后要多一个0表示全部结束了}
var
  F: TSHFileOpStruct;
begin
  Result := false;
  if not FileExists(AFromFile) then
    Exit;

  F.Wnd   := Application.Handle;
  F.wFunc := fo_COPY;
  F.pfrom := PChar(AFromFile+#0#0);
  f.pto   := PCHAR(AToFile+#0#0);
  f.fFlags:= AOptions;
  Result  := SHFileOperation(F)=0;
end;

function ZsMoveFile(AFromFile, AToFile: String; const AOptions: FILEOP_FLAGS = FOF_SILENT): Boolean;
var
  F: TSHFileOpStruct;
begin
  Result := false;
  if not FileExists(AFromFile) then
    Exit;

    
  F.Wnd   := Application.Handle;
  F.wFunc := fo_Move;
  F.pfrom := PChar(AFromFile+#0#0);
  f.pto   := PCHAR(AToFile+#0#0);
  f.fFlags:= AOptions;
  Result  := SHFileOperation(F)=0;
end;

function DirectorCanModify(ADir: String): Boolean;
var
  l_TmpFile: String;
  l_Handle: Integer;
begin
  Result := false;
  if not ForceDirectories(ADir) then
    Exit;
  l_TmpFile := GetRandomFileName(ADir);
  l_Handle := FileCreate(l_TmpFile);
  if l_Handle = -1 then
    Exit;
    
  Result := True;

  try
    FileClose(l_Handle);
    Files.DeleteFile(l_TmpFile);
  except
  end;
end;

function IsFileInUse(fName : string) : Boolean;
var
  HFileRes : HFILE;
begin
  Result := false;
  if not FileExists(fName) then
    Exit;

  HFileRes := CreateFile(pchar(fName), GENERIC_READ or GENERIC_WRITE,
    0 {this is the trick!}, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end; 


function GetFileDateTime(const FileName: string; FileTimeType: TFileTimeType): TDateTime;
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
  Result := -1;
end;
function SetFileDateTime(const FileName: string; FileTimeType: TFileTimeType; DateTime: TDateTime): Integer;
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
    if SetFileTime(Handle, FileTimes[fttCreation], FileTimes[fttLastAccess],
      FileTimes[fttLastWrite]) then Exit;
  finally
    FileClose(Handle);
  end;
  Result := GetLastError;
end;

function FileContentToString(AFileName: String): String;
var
  l_List: TStringList;
begin
  Result := '';
  if not FileExists(AFileName) then
    Exit;

  l_List := TStringList.Create;
  try
    l_List.LoadFromFile(AFileName);
    Result := l_List.Text;
  finally
    l_List.Free;
  end;
end;



end.
