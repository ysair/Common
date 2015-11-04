unit uDirSnapshot;

interface

uses
  windows, SysUtils, Classes, StrUtils, uFindFile;

type
  TCompareAction =   (
      caAdded,
      caModifyed,
      caRemoved
      );

  TDirSnapshotCompareProc = procedure(Sender : TObject; CompareAction : TCompareAction; FileName : string) of object;

  TDirSnapshot = class
  private
    FList : TList;
    FDir: string;
    FSubFolder: Boolean;
    FOnCompare: TDirSnapshotCompareProc;
  private
    procedure SetDir(const Value: string);
    procedure DoCompare(CompareAction : TCompareAction; FileName : string);
  public
    property Dir  : string read FDir Write SetDir;
    property SubFolder : Boolean read FSubFolder Write FSubFolder;
    property OnCompare : TDirSnapshotCompareProc read FOnCompare Write FOnCompare;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Camera;
    procedure Clear;
    function  Search(const AFileName  : string) : PSearchFileInfo;
    procedure Compare(const OldSnapshot : TDirSnapshot);

    procedure SaveToFile(const AFileName  : string);
    procedure LoadFromFile(const AFileName  : string);
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
end;

destructor TDirSnapshot.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TDirSnapshot.SaveToFile(const AFileName: string);

  function SearchFileInfoToStr(
      const sfi : PSearchFileInfo;
      const skp : Integer
      ):string;
  begin
    Result  :=  MidStr(sfi.FileName, skp + 1, Length(sfi.FileName))
        + '|' + IntToStr(sfi.Size)
        + '|' + IntToStr(sfi.ModifyTime.dwHighDateTime)
        + '|' + IntToStr(sfi.ModifyTime.dwLowDateTime);
  end;

var
  path: string;
  p   : Integer;
  sfi : PSearchFileInfo;
  i   : Integer;
  f   : TextFile;
begin
  path  :=  Dir;
  p :=  Length(path);
  try
    if FileExists(AFileName) then
      DeleteFile(AFileName);
    AssignFile(f, AFileName);
    try
      ReWrite(f);
      Writeln(f, path);
      for i := 0 to FList.Count - 1 do
      begin
        sfi :=  FList[i];
        Writeln(f, SearchFileInfoToStr(sfi, p));
      end;
    finally
      CloseFile(f);
    end;
  except
  end;
end;

procedure TDirSnapshot.LoadFromFile(const AFileName: string);

  function StrToSearchFileInfo(
      const AStr  : string;
      const ADir  : string
      ):PSearchFileInfo;
  var
    s : string;
    p : Integer;
  begin
    new(Result);
    try
      Result.Tag  :=  0;
      s :=  AStr;
      p :=  Pos('|', s);
      Result.FileName :=  ADir + LeftStr(s, p - 1);
      Delete(s, 1, p);
      p :=  Pos('|', s);
      Result.Size :=  StrToInt(LeftStr(s, p - 1));
      Delete(s, 1, p);
      p :=  Pos('|', s);
      Result.ModifyTime.dwHighDateTime :=  StrToInt64(LeftStr(s, p - 1));
      Delete(s, 1, p);
      Result.ModifyTime.dwLowDateTime  :=  StrToInt64(s);
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
  if FileExists(AFileName) then
  begin
    lst :=  TStringList.Create;
    try
      lst.LoadFromFile(AFileName);
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
end;

procedure TDirSnapshot.Camera;
begin
  Clear;
  FindFiles(FList, Dir, '', FSubFolder);
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
begin
  if Lowercase(Dir) <> LowerCase(OldSnapshot.Dir) then
    Exit;
  for i := 0 to FList.Count - 1 do
  begin
    fn  :=  PSearchFileInfo(FList[i]).FileName;
    sfi :=  OldSnapshot.Search(fn);
    if Assigned(sfi) then
    begin
      sfi.Tag :=  1;
      if (PSearchFileInfo(FList[i]).Size <> sfi.Size)
          or (PSearchFileInfo(FList[i]).ModifyTime.dwHighDateTime <> sfi.ModifyTime.dwHighDateTime)
          or (PSearchFileInfo(FList[i]).ModifyTime.dwLowDateTime <> sfi.ModifyTime.dwLowDateTime)
          then
        DoCompare(caModifyed, fn);
    end
    else
      DoCompare(caAdded, fn);
  end;
  for i := 0 to OldSnapshot.FList.Count - 1 do
    if PSearchFileInfo(OldSnapshot.FList[i]).Tag = 0 then
      DoCompare(caRemoved, PSearchFileInfo(OldSnapshot.FList[i]).FileName);
end;

procedure TDirSnapshot.SetDir(const Value: string);
begin
  FDir := Value;
  if RightStr(FDir, 1) <> '\' then FDir  := FDir + '\';
end;

end.
