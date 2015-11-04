unit uDirSnapshot;

interface

uses
  windows, SysUtils, Classes, StrUtils, fcFile;

type
  TCompareAction =   (
      caAdded,
      caModifyed,
      caRemoved
      );

  TDirSnapshotCompareProc = procedure(Sender : TObject; CompareAction : TCompareAction; FileName : string) of object;
  TDirSnapshotCompareItemProc = procedure(Sender : TObject; CompareAction : TCompareAction; ASelfItem : PSearchFileInfo; ACompareItem : PSearchFileInfo);

  TFileCompareOption = (coRootDir, coSize, coModifyTime, coHash);
  TFileCompareOptions = set of TFileCompareOption;

  TDirSnapshot = class
  private
    FList : TList;
    FDir: string;
    FSubFolder: Boolean;
    FOnCompare: TDirSnapshotCompareProc;
    FCompareOptions: TFileCompareOptions;
    FSaveModifyTime: Boolean;
  private
    procedure SetDir(const Value: string);
    procedure DoCompare(CompareAction : TCompareAction; FileName : string);
    function  CompareFile(const Left, Right : PSearchFileInfo) : Boolean;
  public
    property Dir  : string read FDir Write SetDir;
    property SubFolder : Boolean read FSubFolder Write FSubFolder;
    property OnCompare : TDirSnapshotCompareProc read FOnCompare Write FOnCompare;
    property CompareOptions : TFileCompareOptions read FCompareOptions write FCompareOptions default [coRootDir, coSize, coModifyTime];
    property FileList : TList read FList;
    property SaveModifyTime : Boolean read FSaveModifyTime write FSaveModifyTime;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Camera(const ACallBackFunc : TFindFilesCallBackFunc = nil);
    procedure Clear;
    function  Search(const AFileName  : string) : PSearchFileInfo;
    procedure Compare(const OldSnapshot : TDirSnapshot); overload;
    procedure Compare(const OldSnapshot : TDirSnapshot; const ACompareItemProc : TDirSnapshotCompareItemProc); overload;
    procedure RemoveExists(const OldSnapshot : TDirSnapshot);

    procedure SaveToStream(const AStream : TStream);
    procedure SaveToFile(const AFileName  : string);
    procedure LoadFromFile(const AFileName  : string);
    procedure LoadFromStream(const AStream : TStream);
  end;

implementation

{ TDirSnapshot }

procedure TDirSnapshot.Clear;
var
  i : Integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PSearchFileInfo(FList[i]));
  FList.Clear;
end;

constructor TDirSnapshot.Create;
begin
  inherited;
  FList :=  TList.Create;
  CompareOptions  :=  [coRootDir, coSize, coModifyTime];
end;

destructor TDirSnapshot.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TDirSnapshot.SaveToFile(const AFileName: string);
var
  fs : TFileStream;
begin
  if FileExists(AFileName) then
    DeleteFile(AFileName);
  fs  :=  TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TDirSnapshot.SaveToStream(const AStream: TStream);

  function SearchFileInfoToStr(
      const sfi : PSearchFileInfo;
      const skp : Integer
      ):string;
  begin
    Result  :=  MidStr(sfi.FileName, skp + 1, Length(sfi.FileName))
        + '|' + IntToStr(sfi.Size) + '|';
    if SaveModifyTime then
      Result  :=  Result + IntToStr(sfi.ModifyTime.dwHighDateTime)
        + '|' + IntToStr(sfi.ModifyTime.dwLowDateTime)
    else
      Result  :=  Result + '|';
    if sfi.HashCode <> '' then
      Result  :=  Result + '|' + sfi.HashCode;
    if sfi.ZipHashCode <> '' then
      Result  :=  Result + '|' + IntToStr(sfi.ZipSize) + '|' + sfi.ZipHashCode;
  end;

var
  path: string;
  p   : Integer;
  sfi : PSearchFileInfo;
  i   : Integer;
  lst : TStrings;
begin
  lst :=  TStringList.Create;
  try
    path  :=  Dir;
    p :=  Length(path);
    lst.Add(path);
    for i := 0 to FList.Count - 1 do
    begin
      sfi :=  FList[i];
      lst.Add(SearchFileInfoToStr(sfi, p));
    end;
    lst.SaveToStream(AStream);
  finally
    lst.Free;
  end;
end;

procedure TDirSnapshot.LoadFromFile(const AFileName: string);
var
  fs : TFileStream;
begin
  if FileExists(AFileName) then
  begin
    fs  :=  TFileStream.Create(AFileName, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TDirSnapshot.LoadFromStream(const AStream: TStream);

  function StrToSearchFileInfo(
      const AStr  : string;
      const ADir  : string
      ):PSearchFileInfo;
  var
    lst : TStrings;
  begin
    new(Result);
    try
      Result.Tag  :=  0;
      lst :=  TStringList.Create;
      try
        lst.Delimiter :=  '|';
        lst.DelimitedText :=  AStr;
        if lst.Count < 4 then
          raise Exception.Create('');
        Result.FileName :=  ADir  + lst[0];
        Result.Size     :=  StrToInt(lst[1]);
        Result.ModifyTime.dwHighDateTime  :=  StrToInt64Def(lst[2], 0);
        Result.ModifyTime.dwLowDateTime   :=  StrToInt64Def(lst[3], 0);
        if lst.Count > 4 then
        begin
          Result.HashCode :=  lst[4];
          if lst.Count > 6 then
          begin
            Result.ZipSize  :=  StrToInt(lst[5]);
            Result.ZipHashCode  :=  lst[6];
          end;
        end;
      finally
        lst.Free;
      end;
    except
      Dispose(PSearchFileInfo(Result));
      Result  :=  nil;
    end;
  end;

var
  path: string;
  lst : TStrings;
  i   : Integer;
  sfi : PSearchFileInfo;
begin
  lst :=  TStringList.Create;
  try
    AStream.Position  :=  0;
    lst.LoadFromStream(AStream);
    if lst.Count > 1 then
    begin
      path :=  lst[0];
      if Length(path) > 0 then
      begin
        Clear;
        Dir :=  path;
        for i := 1 to lst.Count - 1 do
        begin
          sfi :=  StrToSearchFileInfo(lst[i], Path);
          if Assigned(sfi) then
            FList.Add(sfi);
        end;
      end;
    end;
  finally
    lst.Free;
  end;
end;

procedure TDirSnapshot.RemoveExists(const OldSnapshot: TDirSnapshot);
var
  i : Integer;
  sfi : PSearchFileInfo;
  fn  : string;
  rootlen : Integer;
begin
  if (CompareOptions = []) or (CompareOptions = [coRootDir]) then
    CompareOptions  :=  [coRootDir, coSize, coModifyTime];
  if (coRootDir in CompareOptions) and (Lowercase(Dir) <> LowerCase(OldSnapshot.Dir)) then
    Exit;
  rootlen :=  Length(Dir);
  for i := FList.Count - 1 downto 0 do
  begin
    fn  :=  MidStr(PSearchFileInfo(FList[i]).FileName, rootlen + 1, MAX_PATH);
    sfi :=  OldSnapshot.Search(OldSnapshot.Dir + fn);
    if Assigned(sfi) then
      if not CompareFile(PSearchFileInfo(FList[i]), sfi) then
      begin
        Dispose(PSearchFileInfo(FList[i]));
        FList.Delete(i);
      end;
  end;
end;

procedure TDirSnapshot.Camera(const ACallBackFunc : TFindFilesCallBackFunc);
begin
  Clear;
  Files.FindFiles(FList, Dir, '', FSubFolder, ACallBackFunc);
  FList.Sort(@SearchFileInfoComp);
end;

function TDirSnapshot.Search(const AFileName: string): PSearchFileInfo;
var
  ibegin  : Integer;
  iend    : Integer;
  p       : Integer;
  sfi     : PSearchFileInfo;
  tmp     : PSearchFileInfo;
begin
  Result  :=  nil;
  if FList.Count = 0 then
    Exit;
  new(tmp);
  try
    tmp.FileName  :=  AFileName;
    ibegin  :=  0;
    iend    :=  FList.Count - 1;
    while iend - ibegin > 3 do
    begin
      p :=  ibegin + (iend - ibegin) div 2;
      sfi :=  FList[p];
      case SearchFileInfoComp(tmp, sfi) of
        -1  : iend  :=  p;
        0   : begin Result  :=  sfi; Exit; end;
        1   : ibegin := p;
      end;
    end;
    for p := ibegin to iend do
    begin
      sfi :=  FList[p];
      if 0 = SearchFileInfoComp(tmp, sfi) then
      begin
        Result  :=  sfi;
        break;
      end;
    end;
  finally
    dispose(PSearchFileInfo(tmp));
  end;
end;

procedure TDirSnapshot.DoCompare(CompareAction: TCompareAction;
  FileName: string);
begin
  if Assigned(FOnCompare) then
    FOnCompare(Self, CompareAction, FileName);
end;

procedure TDirSnapshot.Compare(const OldSnapshot: TDirSnapshot);
var
  i : Integer;
  sfi : PSearchFileInfo;
  fn  : string;
  rootlen : Integer;
  oldrootlen : Integer;
begin
  if (CompareOptions = []) or (CompareOptions = [coRootDir]) then
    CompareOptions  :=  [coRootDir, coSize, coModifyTime];
  if (coRootDir in CompareOptions) and (Lowercase(Dir) <> LowerCase(OldSnapshot.Dir)) then
    Exit;
  rootlen :=  Length(Dir);
  oldrootlen  :=  Length(OldSnapshot.Dir);
  for i := 0 to FList.Count - 1 do
  begin
    fn  :=  MidStr(PSearchFileInfo(FList[i]).FileName, rootlen + 1, MAX_PATH);
    sfi :=  OldSnapshot.Search(OldSnapshot.Dir + fn);
    if Assigned(sfi) then
    begin
      sfi.Tag :=  1;
      if CompareFile(PSearchFileInfo(FList[i]), sfi) then
        DoCompare(caModifyed, fn);
    end
    else
      DoCompare(caAdded, fn);
  end;
  for i := 0 to OldSnapshot.FList.Count - 1 do
    if PSearchFileInfo(OldSnapshot.FList[i]).Tag = 0 then
      DoCompare(caRemoved, MidStr(PSearchFileInfo(OldSnapshot.FList[i]).FileName, oldrootlen + 1, MAX_PATH));
end;

procedure TDirSnapshot.Compare(const OldSnapshot: TDirSnapshot; const ACompareItemProc: TDirSnapshotCompareItemProc);

  procedure _DoCompare(const ACompareAction : TCompareAction; const ASelfItem : PSearchFileInfo; const ACompareItem : PSearchFileInfo);
  begin
    if Assigned(ACompareItemProc) then
      ACompareItemProc(Self, ACompareAction, ASelfItem, ACompareItem);
  end;

var
  i : Integer;
  sfi : PSearchFileInfo;
  fn  : string;
  rootlen : Integer;
begin
  if (CompareOptions = []) or (CompareOptions = [coRootDir]) then
    CompareOptions  :=  [coRootDir, coSize, coModifyTime];
  if (coRootDir in CompareOptions) and (Lowercase(Dir) <> LowerCase(OldSnapshot.Dir)) then
    Exit;
  rootlen :=  Length(Dir);
  for i := 0 to FList.Count - 1 do
  begin
    fn  :=  MidStr(PSearchFileInfo(FList[i]).FileName, rootlen + 1, MAX_PATH);
    sfi :=  OldSnapshot.Search(OldSnapshot.Dir + fn);
    if Assigned(sfi) then
    begin
      sfi.Tag :=  1;
      if CompareFile(PSearchFileInfo(FList[i]), sfi) then
        _DoCompare(caModifyed, FList[i], sfi);
    end
    else
      _DoCompare(caAdded, FList[i], sfi);
  end;
  for i := 0 to OldSnapshot.FList.Count - 1 do
    if PSearchFileInfo(OldSnapshot.FList[i]).Tag = 0 then
      _DoCompare(caRemoved, nil, OldSnapshot.FList[i]);
end;

function TDirSnapshot.CompareFile(const Left, Right: PSearchFileInfo): Boolean;
begin //²»Í¬·µ»Øtrue
  Result  :=  (coSize in CompareOptions) and (Left.Size <> Right.Size)
      or (coModifyTime in CompareOptions) and ((Left.ModifyTime.dwHighDateTime <> Right.ModifyTime.dwHighDateTime) or (Left.ModifyTime.dwLowDateTime <> Right.ModifyTime.dwLowDateTime))
      or (coHash in CompareOptions) and (Left.HashCode <> Right.HashCode);
end;

procedure TDirSnapshot.SetDir(const Value: string);
begin
  FDir := Value;
  if RightStr(FDir, 1) <> '\' then FDir  := FDir + '\';
end;

end.
