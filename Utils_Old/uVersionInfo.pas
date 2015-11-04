unit uVersionInfo;

interface

uses
  Windows, SysUtils, Classes, Math;

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
    VsFixedFileInfo:VS_FIXEDFILEINFO;
    UserDefineValue:string;
  end;

function GetFileVersionInfomation(
    const FileName: string;
    var   info: TVersionInfo;
    const UserDefine  : string=''
    ): boolean;

function GetFileVersion(const AFileName: String): String;
function CompareVersionNumber(AVer1, AVer2: string): Integer;

implementation

function GetFileVersion(const AFileName: String): String;
var
	info: TVersionInfo;
begin
  Result := '0.0.0.0';
  if not FileExists(AFileName) then 
    Exit;
  if GetFileVersionInfomation(AFileName, info) then 
  	Result := info.FileVersion;
  //返回的文件版本的格式是： 1, 1, 0, 1014
  Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;

function GetFileVersionInfomation(
    const FileName: string;
    var   info: TVersionInfo;
    const UserDefine  : string=''
    ): boolean;
const
  SFInfo= '\StringFileInfo\';
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
  InfoSize := GetFileVersionInfoSize(pchar(FileName), unused);
  if InfoSize > 0 then
  begin
    GetMem(VersionInfo, InfoSize);
    Result := GetFileVersionInfo(pchar(FileName), 0, InfoSize, VersionInfo);
    if Result then
    begin
      VerQueryValue(VersionInfo, '\VarFileInfo\Translation', Translation, InfoSize);
      if Assigned(Translation) then
        VersionValue := SFInfo + IntToHex(LoWord(Longint(Translation^)), 4) +
          IntToHex(HiWord(Longint(Translation^)), 4) + '\'
      else
        VersionValue := SFInfo;
      VerQueryValue(VersionInfo, pchar(VersionValue + 'CompanyName'), InfoPointer, InfoSize);
      info.CommpanyName := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'FileDescription'), InfoPointer, InfoSize);
      info.FileDescription := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'FileVersion'), InfoPointer, InfoSize);
      info.FileVersion := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'InternalName'), InfoPointer, InfoSize);
      info.InternalName := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'LegalCopyright'), InfoPointer, InfoSize);
      info.LegalCopyright := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'LegalTrademarks'), InfoPointer, InfoSize);
      info.LegalTrademarks := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'OriginalFileName'), InfoPointer, InfoSize);
      info.OriginalFileName := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'ProductName'), InfoPointer, InfoSize);
      info.ProductName := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'ProductVersion'), InfoPointer, InfoSize);
      info.ProductVersion := string(pchar(InfoPointer));
      VerQueryValue(VersionInfo, pchar(VersionValue + 'Comments'), InfoPointer, InfoSize);
      info.Comments := string(pchar(InfoPointer));
      if VerQueryValue(VersionInfo, '\', InfoPointer, InfoSize) then
        info.VsFixedFileInfo := TVSFixedFileInfo(InfoPointer^);
      if UserDefine<>'' then
      begin
        if VerQueryValue(VersionInfo,pchar(VersionValue+UserDefine),InfoPointer,InfoSize) then
          info.UserDefineValue:=string(pchar(InfoPointer));
      end;
    end;
    FreeMem(VersionInfo);
  end;
end;

function CompareVersionNumber(AVer1, AVer2: string): Integer;
//对比文件版本号，
// 1= Aver1 > AVer2;
// -1 =
  procedure _PraseToList(const Str:String; const List: TStringList);
  var
    I: Integer;
    l_Part: String;
  begin
    List.Clear;
    l_Part := '';
    for I := 1 to Length(Str) do
    begin
      if Str[I]='.' then
      begin
        List.Add(l_Part );
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
    _PraseToList(AVer1, l_List1 );
    _PraseToList(AVer2, l_List2 );

    for I := 0 to ifThen(l_List1.Count>l_List2.Count, l_List2.Count-1, l_List1.Count-1) do
    begin
      l_Id1 := StrToIntDef(l_List1[I],0);
      l_Id2 := StrToIntDef(l_List2[I],0);

      if l_Id1<> l_Id2 then
      begin
        Result := IfThen(l_Id1>l_Id2, 1, -1);
        Exit;
      end;
    end;

    if l_List1.Count = l_List2.Count then
      Result := 0
    else
      Result := IfThen(l_List1.Count>l_List2.Count,1,-1);
  finally
    l_List1.Free;
    l_List2.Free;
  end;
end;

end.
