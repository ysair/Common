unit AnsiTomEncryption;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TActionType = (atEncryption, atDecryption);

  TAnsiTomEncryption = class(TComponent)
  private
    { Private declarations }
    FInputString: AnsiString;
    FOutputString: AnsiString;
    FKeyString: AnsiString;
    FAction: TActionType;
    procedure SetInputString(input: AnsiString);
    procedure SetOutputString(input: AnsiString);
    procedure SetKeyString(input: AnsiString);
    Function EncryptionEngine(Src: AnsiString; Key: AnsiString;
      Encrypt: Boolean): AnsiString;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Procedure Execute;
  published
    { Published declarations }
    property input: AnsiString read FInputString write SetInputString;
    property Output: AnsiString read FOutputString write SetOutputString;
    property Key: AnsiString read FKeyString write SetKeyString;
    property Action : TActionType read FAction write FAction default atEncryption;
  end;


implementation


constructor TAnsiTomEncryption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Action := atEncryption;
end;

Procedure TAnsiTomEncryption.SetOutputString(input: AnsiString);
begin
  if input <> FOutputString then
    FOutputString := input;
end;

Procedure TAnsiTomEncryption.SetKeyString(input: AnsiString);
begin
  if input <> FKeyString Then
    FKeyString := input;
end;

Procedure TAnsiTomEncryption.SetInputString(input: AnsiString);
begin
  if input <> FInputString Then
    FInputString := input;
end;

Function TAnsiTomEncryption.EncryptionEngine(Src: AnsiString; Key: AnsiString;
  Encrypt: Boolean): AnsiString;
var
  KeyLen: Integer;
  KeyPos: Integer;
  offset: Integer;
  dest: AnsiString;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Range: Integer;

begin
  KeyLen := Length(Key);
  if KeyLen = 0 then
    Key := 'DM';
  KeyPos := 0;
  Range := 256;
  if Encrypt then
  begin
    Randomize;
    offset := Random(Range);
    dest := AnsiString( format('%1.2x', [offset]) );
    for SrcPos := 1 to Length(Src) do
    begin
      SrcAsc := (Ord(Src[SrcPos]) + offset) MOD 255;
      if KeyPos < KeyLen then
        KeyPos := KeyPos + 1
      else
        KeyPos := 1;
      SrcAsc := SrcAsc xor Ord(Key[KeyPos]);
      dest := dest + AnsiString( format('%1.2x', [SrcAsc]) );
      offset := SrcAsc;
    end;
  end
  else
  begin
    offset := StrToInt('$' + string(copy(Src, 1, 2)));
    SrcPos := 3;
    repeat
      SrcAsc := StrToInt('$' + string( copy(Src, SrcPos, 2)));
      if KeyPos < KeyLen Then
        KeyPos := KeyPos + 1
      else
        KeyPos := 1;
      TmpSrcAsc := SrcAsc xor Ord(Key[KeyPos]);
      if TmpSrcAsc <= offset then
        TmpSrcAsc := 255 + TmpSrcAsc - offset
      else
        TmpSrcAsc := TmpSrcAsc - offset;
      dest := dest + AnsiString( AnsiChar(TmpSrcAsc) );
      offset := SrcAsc;
      SrcPos := SrcPos + 2;
    until SrcPos >= Length(Src);
  end;
  Result := dest;
end;

procedure TAnsiTomEncryption.Execute;
var
  EncryptionFlag: Boolean;
begin
  if Length(FInputString) = 0 then
  begin
    FOutputString := '';
    exit;
  end;

  if FAction = atEncryption then
    EncryptionFlag := True
  else
    EncryptionFlag := False;

  try
    FOutputString := EncryptionEngine(FInputString, FKeyString, EncryptionFlag);
  except
    FOutputString := FInputString;
  end;
end;

end.
