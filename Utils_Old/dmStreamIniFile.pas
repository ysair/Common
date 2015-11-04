unit dmStreamIniFile;

interface
uses IniFiles, Classes, SysUtils;

type

  TdmStreamIniFile = class(TCustomIniFile)
  private
    FSections: TStringList;
    FEncoding: TEncoding;
    function AddSection(const Section: string): TStrings;
  public
    constructor Create(const AStream: TStream=nil); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(ASteam: TStream);

    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: string); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure SetStrings(List: TStrings);
    procedure WriteString(const Section, Ident, Value: String); override;
    property Encoding: TEncoding read FEncoding write FEncoding;
    procedure UpdateFile; override; //没用用到
  end;

implementation


constructor TdmStreamIniFile.Create(const AStream: TStream);
begin
  FEncoding := TEncoding.Default;
  FSections := THashedStringList.Create;
{$IFDEF LINUX}
  FSections.CaseSensitive := True;
{$ENDIF}
  if AStream <>nil then
    LoadFromStream(AStream);
end;

destructor TdmStreamIniFile.Destroy;
begin
  if FSections <> nil then
    Clear;
  FSections.Free;
  inherited Destroy;
end;

function TdmStreamIniFile.AddSection(const Section: string): TStrings;
begin
  Result := THashedStringList.Create;
  try
    THashedStringList(Result).CaseSensitive := FSections.CaseSensitive;
    FSections.AddObject(Section, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TdmStreamIniFile.Clear;
var
  I: Integer;
begin
  for I := 0 to FSections.Count - 1 do
    TObject(FSections.Objects[I]).Free;
  FSections.Clear;
end;

procedure TdmStreamIniFile.DeleteKey(const Section, Ident: String);
var
  I, J: Integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    J := Strings.IndexOfName(Ident);
    if J >= 0 then
      Strings.Delete(J);
  end;
end;

procedure TdmStreamIniFile.EraseSection(const Section: string);
var
  I: Integer;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    TStrings(FSections.Objects[I]).Free;
    FSections.Delete(I);
  end;
end;

procedure TdmStreamIniFile.GetStrings(List: TStrings);
var
  I, J: Integer;
  Strings: TStrings;
begin
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[I] + ']');
      Strings := TStrings(FSections.Objects[I]);
      for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
      List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TdmStreamIniFile.LoadFromStream(AStream: TStream);
var
  List: TStringList;
//  Bytes: TBytes;
begin
  if AStream.Size >0 then
  begin
  {
    AStream.Position := 0;
    SetLength(Bytes, 2);
    AStream.Read(Bytes[0], 2);
    FEncoding := nil; //Reset the encoding to that read from the file
    TEncoding.GetBufferEncoding(Bytes, FEncoding);
   }
    List := TStringList.Create;
    try
      AStream.Position := 0;
      List.LoadFromStream(AStream, FEncoding);
      SetStrings(List);
    finally
      List.Free;
    end;
  end
  else
    Clear;
end;

procedure TdmStreamIniFile.ReadSection(const Section: string;
  Strings: TStrings);
var
  I, J: Integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TdmStreamIniFile.ReadSections(Strings: TStrings);
begin
  Strings.Assign(FSections);
end;

procedure TdmStreamIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
      Strings.Assign(TStrings(FSections.Objects[I]));
  finally
    Strings.EndUpdate;
  end;
end;

function TdmStreamIniFile.ReadString(const Section, Ident,
  Default: string): string;
var
  I: Integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
    begin
      Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
      Exit;
    end;
  end;
  Result := Default;
end;

procedure TdmStreamIniFile.SaveToStream(ASteam: TStream);
var
  I, J: Integer;
  Strings: TStrings;
  l_Str: AnsiString;
begin
  ASteam.Position := 0;
  ASteam.Size := 0;

  for I := 0 to FSections.Count - 1 do
  begin
    l_Str := '[' + AnsiString(FSections[I]) + ']'#13#10;
    ASteam.Write(l_Str[1], Length(l_Str));
    Strings := TStrings(FSections.Objects[I]);
    for J := 0 to Strings.Count - 1 do
    begin
      l_Str := AnsiString(Strings[J])+#13#10;
      ASteam.Write(l_Str[1], Length(l_Str));
    end;

    l_Str := #13#10;
    ASteam.Write(l_Str[1], Length(l_Str));
  end;
end;

procedure TdmStreamIniFile.SetStrings(List: TStrings);
var
  I, J: Integer;
  S: string;
  Strings: TStrings;
begin
  Clear;
  Strings := nil;
  for I := 0 to List.Count - 1 do
  begin
    S := Trim(List[I]);
    if (S <> '') and (S[1] <> ';') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
      begin
        Delete(S, 1, 1);
        SetLength(S, Length(S)-1);
        Strings := AddSection(Trim(S));
      end
      else
        if Strings <> nil then
        begin
          J := Pos('=', S);
          if J > 0 then // remove spaces before and after '='
            Strings.Add(Trim(Copy(S, 1, J-1)) + '=' + Trim(Copy(S, J+1, MaxInt)) )
          else
            Strings.Add(S);
        end;
  end;
end;

procedure TdmStreamIniFile.UpdateFile;
begin
  inherited;
  ;
end;

procedure TdmStreamIniFile.WriteString(const Section, Ident, Value: String);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStrings(FSections.Objects[I])
  else
    Strings := AddSection(Section);
  S := Ident + '=' + Value;
  I := Strings.IndexOfName(Ident);
  if I >= 0 then
    Strings[I] := S
  else
    Strings.Add(S);
end;



end.
