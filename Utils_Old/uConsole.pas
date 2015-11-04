unit uConsole;

interface

uses
  Classes, Windows;

type
  TConsole = class
  private
    FOutStream: THandleStream;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(const AStr : string);
    procedure WriteLn(const AStr : string = '');
  end;

function Console : TConsole;

implementation

var
  G_Console : TConsole;

function Console : TConsole;
begin
  if not Assigned(G_Console) then
    G_Console :=  TConsole.Create;
  Result  :=  G_Console;
end;

{ TConsole }

constructor TConsole.Create;
var
  hOutput: THandle;
begin
  inherited;
  hOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  FOutStream := THandleStream.Create(hOutput);
end;

destructor TConsole.Destroy;
begin
  FOutStream.Free;
  inherited;
end;

procedure TConsole.Write(const AStr: string);
var
  pStr  : PChar;
  pLen  : Integer;
begin
  pLen  :=  Length(AStr);
  GetMem(pStr, pLen);
  try
    CopyMemory(pStr, @AStr[1], pLen);
    FOutStream.Write(pStr^, pLen);
  finally
    FreeMem(pStr, pLen);
  end;
end;

procedure TConsole.WriteLn(const AStr: string);
begin
  Write(AStr + #13#10);
end;

initialization
finalization
  if Assigned(G_Console) then
    G_Console.Free;

end.
