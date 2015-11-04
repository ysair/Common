//==============================================================================
// Unit Name: uFindFile
// Author   : ysai
// Date     : 2003-11-28 
// Purpose  : 
// History  :
//==============================================================================

unit uFindFile;

interface

uses
  SysUtils,Windows,Classes;

type
  PSearchFileInfo  = ^TSearchFileInfo;
  TSearchFileInfo  = record
    FileName    : string;
    ModifyTime  : TFileTime;
    Size        : Integer;
    Tag         : Integer;
    HashCode    : string;
  end;

  TFindFilesCallBackFunc  = procedure(const ASearchFileInfo : PSearchFileInfo);

function FindFiles(
    const AList       : TStrings;
    const APath       : String;
    const AFileName   : string = '*.*';
    const ASubFolder  : Boolean = False
    ) : DWORD; overload;

function FindFiles(
    const AList       : TList;
    const APath       : String;
    const AFileName   : string = '*.*';
    const ASubFolder  : Boolean = False;
    const ACallBackFunc : TFindFilesCallBackFunc = nil
    ) : DWORD; overload;

function FindFolders(
    const AList       : TStrings;
    const APath       : String;
    const AFolderName : string = '*';
    const ASubFolder  : Boolean = False
    ) : DWORD;

function SearchFileInfoComp(F1, F2: Pointer): Integer;

implementation

{$WARN SYMBOL_PLATFORM OFF}

function SearchFileInfoComp(F1, F2: Pointer): Integer;
begin
  if LowerCase(PSearchFileInfo(F1)^.FileName) >
      LowerCase(PSearchFileInfo(F2)^.FileName) then
    Result  :=  1
  else if LowerCase(PSearchFileInfo(F1)^.FileName) <
      LowerCase(PSearchFileInfo(F2)^.FileName) then
    Result  :=  -1
  else
    Result  :=  0;
end;

function FindFiles(
    const AList       : TStrings;
    const APath       : String;
    const AFileName   : string = '*.*';
    const ASubFolder  : Boolean = False
    ) : DWORD;
var
  sname : string;
  sPath : String;
  info  : TSearchRec;
begin
  if APath[Length(APath)] <> '\' then
    sPath :=  APath + '\'
  else
    sPath :=  APath;
  if AFileName = '' then
    sname :=  '*.*'
  else
    sname :=  AFileName;
  
  Result  :=  FindFirst(sPath + sname, faAnyFile and (not faDirectory),info);
  if Result <> INVALID_HANDLE_VALUE then
  begin
    try
      if (info.Name <> '')
          and (info.Name <> '.')
          and (info.Name <> '..')
          and ((info.Attr and faDirectory) <> faDirectory) then
        AList.Add(sPath + info.Name);
      while 0 = FindNext(info) do
        if (info.Name <> '')
            and (info.Name <> '.')
            and (info.Name <> '..')
            and ((info.Attr and faDirectory) <> faDirectory) then
          AList.Add(sPath + info.Name);
    finally
      FindClose(info.FindHandle);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
      try
          if (info.Name <> '.')
              and (info.Name <> '..')
              and ((info.Attr and faDirectory) = faDirectory) then
            FindFiles(AList, sPath + info.Name, sname, ASubFolder);
          while 0 = FindNext(info) do
            if (info.Name <> '.')
                and (info.Name <> '..')
                and ((info.Attr and faDirectory) = faDirectory) then
              FindFiles(AList, sPath + info.Name, sname, ASubFolder);
      finally
        FindClose(info.FindHandle);
      end;
    Result  :=  NO_ERROR;
  end
  else                      
    Result  :=  GetLastError;
end;

function FindFiles(
    const AList       : TList;
    const APath       : String;
    const AFileName   : string = '*.*';
    const ASubFolder  : Boolean = False;
    const ACallBackFunc : TFindFilesCallBackFunc = nil
    ) : DWORD;
var
  sfi   : PSearchFileInfo;
  sname : string;
  sPath : String;
  info  : TSearchRec;

  procedure AddToList;
  begin
    new(sfi);
    sfi.FileName    :=  sPath + info.Name;
    sfi.ModifyTime  :=  info.FindData.ftLastWriteTime;
    sfi.Size        :=  info.Size;
    sfi.Tag         :=  0;
    AList.Add(sfi);
    if Assigned(ACallBackFunc) then
      ACallBackFunc(sfi);
  end;

begin
  if APath[Length(APath)] <> '\' then
    sPath :=  APath + '\'
  else
    sPath :=  APath;
  if AFileName = '' then
    sname :=  '*.*'
  else
    sname :=  AFileName;

  Result  :=  FindFirst(sPath + sname, faAnyFile and (not faDirectory),info);
  if Result <> INVALID_HANDLE_VALUE then
  begin
    try
      if (info.Name <> '')
          and (info.Name <> '.')
          and (info.Name <> '..')
          and ((info.Attr and faDirectory) <> faDirectory) then
        AddToList;
      while 0 = FindNext(info) do
        if (info.Name <> '')
            and (info.Name <> '.')
            and (info.Name <> '..')
            and ((info.Attr and faDirectory) <> faDirectory) then
          AddToList;
    finally
      FindClose(info.FindHandle);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
      try
          if (info.Name <> '.')
              and (info.Name <> '..')
              and ((info.Attr and faDirectory) = faDirectory) then
            FindFiles(AList, sPath + info.Name, sname, ASubFolder, ACallBackFunc);
          while 0 = FindNext(info) do
            if (info.Name <> '.')
                and (info.Name <> '..')
                and ((info.Attr and faDirectory) = faDirectory) then
              FindFiles(AList, sPath + info.Name, sname, ASubFolder, ACallBackFunc);
      finally
        FindClose(info.FindHandle);
      end;
    Result  :=  NO_ERROR;
  end
  else
    Result  :=  GetLastError;
end;

function FindFolders(
    const AList       : TStrings;
    const APath       : String;
    const AFolderName : string = '*';
    const ASubFolder  : Boolean = False
    ) : DWORD;
var
  sname : string;
  sPath : String;
  info  : TSearchRec;
begin
  if APath[Length(APath)] <> '\' then
    sPath :=  APath + '\'
  else
    sPath :=  APath;
  if AFolderName = '' then
    sname :=  '*.*'
  else
    sname :=  AFolderName;

  Result  :=  FindFirst(sPath + sname, faDirectory, info);
  if Result <> INVALID_HANDLE_VALUE then
  begin
    try
      if (info.Name <> '')
          and (info.Name <> '.')
          and (info.Name <> '..')
          and ((info.Attr and faDirectory) = faDirectory) then
        AList.Add(sPath + info.Name);
      while 0 = FindNext(info) do
        if (info.Name <> '')
            and (info.Name <> '.')
            and (info.Name <> '..')
            and ((info.Attr and faDirectory) = faDirectory) then
          AList.Add(sPath + info.Name);
    finally
      FindClose(info.FindHandle);
    end;

    if ASubFolder then
      if 0 = FindFirst(sPath + '*', faDirectory, info) then
      try
          if (info.Name <> '.')
              and (info.Name <> '..')
              and ((info.Attr and faDirectory) = faDirectory) then
            FindFolders(AList, sPath + info.Name, sname, ASubFolder);
          while 0 = FindNext(info) do
            if (info.Name <> '.')
                and (info.Name <> '..')
                and ((info.Attr and faDirectory) = faDirectory) then
              FindFolders(AList, sPath + info.Name, sname, ASubFolder);
      finally
        FindClose(info.FindHandle);
      end;
    Result  :=  NO_ERROR;
  end
  else
    Result  :=  GetLastError;
end;

end.
