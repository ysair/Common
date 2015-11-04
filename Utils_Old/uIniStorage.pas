unit uIniStorage;

interface

uses
  windows, SysUtils, Classes, IniFiles, uStorage;

type
  TIniFileStorage = class(TFileStorage)
  private
    FIniFile  : TIniFile;
  protected
    procedure SetFileName(const Value: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Write(
        const Key : string;
        const Value : string
        ); override;
    procedure Write(
        const Key : string;
        const Value : integer
        ); override;
    procedure Write(
        const Key : string;
        const Value : int64
        ); override;
    procedure Write(
        const Key : string;
        const Value : double
        ); override;
    procedure WriteDateTime(
        const Key : string;
        const Value : TDateTime
        ); override;
    procedure Write(
        const Key : string;
        const Value : boolean
        ); override;

    procedure ReadSections(
        const SectionList : TStrings
        ); override;
    procedure ReadKeys(
        const KeyList : TStrings
        ); override;

    function Read(
        const Key : string
        ): variant; override;
    function ReadString(
        const Key : string
        ): string; override;
    function ReadInteger(
        const Key : string
        ): integer; override;
    function ReadInt64(
        const Key : string
        ): int64; override;
    function ReadFloat(
        const Key : string
        ): double; override;
    function ReadDateTime(
        const Key : string
        ): TDateTime; override;
    function ReadBoolean(
        const Key : string
        ): boolean; override;

    procedure DeleteKey(
        const Key : string
        ); override;
    procedure DeleteSection(
        const Section : string
        ); override;
    procedure Clear; override;
  end;

implementation

{ TIniFileStorage }

constructor TIniFileStorage.Create;
begin
  inherited Create;  
end;

destructor TIniFileStorage.Destroy;
begin
  if assigned(FIniFile) then
    FIniFile.Free;
  inherited;
end;

procedure TIniFileStorage.SetFileName(const Value: string);
begin
  if Value <> FileName then
  begin
    inherited;
    if assigned(FIniFile) then
      FIniFile.Free;
    FIniFile  :=  TIniFile.Create(FileName);
  end;
end;

procedure TIniFileStorage.Write(const Key: string;
  const Value: boolean);
begin
  if assigned(FIniFile) then
    FIniFile.WriteBool(Section, Key, Value);
end;

procedure TIniFileStorage.Write(const Key, Value: string);
begin
  if assigned(FIniFile) then
    FIniFile.WriteString(Section, Key, Value);
end;

procedure TIniFileStorage.Write(const Key: string;
  const Value: integer);
begin
  if assigned(FIniFile) then
    FIniFile.WriteInteger(Section, Key, Value);
end;

procedure TIniFileStorage.Write(const Key: string;
  const Value: int64);
begin
  if assigned(FIniFile) then
    FIniFile.WriteString(Section, Key, IntToStr(Value));
end;
procedure TIniFileStorage.Write(const Key: string;
  const Value: double);
begin
  if assigned(FIniFile) then
    FIniFile.WriteFloat(Section, Key, Value);
end;

procedure TIniFileStorage.WriteDateTime(const Key: string;
  const Value: TDateTime);
begin
  if assigned(FIniFile) then
    FIniFile.WriteDateTime(Section, Key, Value);
end;

procedure TIniFileStorage.ReadSections(const SectionList: TStrings);
begin
  if assigned(FIniFile) and assigned(SectionList) then
    FIniFile.ReadSections(SectionList);
end;

procedure TIniFileStorage.ReadKeys(const KeyList: TStrings);
begin
  if assigned(FIniFile) and assigned(KeyList) then
    FIniFile.ReadSection(Section, KeyList);
end;

function TIniFileStorage.Read(const Key: string): variant;
begin
  result :=  '';
  if assigned(FIniFile) then
    result  :=  FIniFile.ReadString(Section, Key, result);
end;

function TIniFileStorage.ReadBoolean(const Key: string): boolean;
begin
  result :=  false;
  if assigned(FIniFile) then
    result  :=  FIniFile.ReadBool(Section, Key, result);
end;

function TIniFileStorage.ReadDateTime(const Key: string): TDateTime;
begin
  result :=  0;
  if assigned(FIniFile) then
    result  :=  FIniFile.ReadDateTime(Section, Key, result);
end;

function TIniFileStorage.ReadFloat(const Key: string): double;
begin
  result :=  0;
  if assigned(FIniFile) then
    result  :=  FIniFile.ReadFloat(Section, Key, result);
end;

function TIniFileStorage.ReadInteger(const Key: string): integer;
begin
  result :=  0;
  if assigned(FIniFile) then
    result  :=  FIniFile.ReadInteger(Section, Key, result);
end;

function TIniFileStorage.ReadInt64(const Key: string): int64;
begin
  result :=  0;
  if assigned(FIniFile) then
    result  :=  StrToInt64Def(FIniFile.ReadString(Section, Key, ''), result);
end;

function TIniFileStorage.ReadString(const Key: string): string;
begin
  result :=  '';
  if assigned(FIniFile) then
    result  :=  FIniFile.ReadString(Section, Key, result);
end;

procedure TIniFileStorage.DeleteKey(const Key: string);
begin
  if assigned(FIniFile) then
    FIniFile.DeleteKey(Section, Key); 
end;

procedure TIniFileStorage.DeleteSection(const Section: string);
begin
  if assigned(FIniFile) then
    FIniFile.EraseSection(Section); 
end;

procedure TIniFileStorage.Clear;
begin
  if (FileName <> '') and FileExists(FileName) then
    DeleteFile(FileName);
end;

end.
