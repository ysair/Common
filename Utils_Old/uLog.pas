unit uLog;

interface

uses
  windows, SysUtils, Classes;

type
  TLog  = class
  private
  protected
  public
    procedure Write(const AStr  : string); overload; virtual; abstract;
    procedure Write(const AType : integer; AStr : string); overload; virtual; abstract;
    procedure Write(const ATime : TDateTime; const AType : integer; AStr : string); overload; virtual; abstract;
  end;

  TFileLog  = class(TLog)
  private
    FFileName : string;
    FLogFile      : TextFile;
    FLogFileOpened  : boolean;
    FAllowBr: Boolean;
  private
    procedure SetFileName(const Value: string);
  protected
    procedure OpenFile;
    procedure CloseFile;
    procedure WriteLn(const AStr  : string);
  public
    property FileName : string read FFileName write SetFileName;
    property AllowBr  : Boolean read FAllowBr Write FAllowBr;
  public
    destructor Destroy; override;

    procedure Save;
    procedure Close;
    procedure Write(const AStr  : string); override;
    procedure Write(const AType : integer; AStr : string); override;
    procedure Write(const ATime : TDateTime; const AType : integer; AStr : string); override;
  end;

implementation

{ TFileLog }

destructor TFileLog.Destroy;
begin
  CloseFile;
  inherited;
end;

procedure TFileLog.OpenFile;
begin
  if (FFileName <> '') and not FLogFileOpened then
  begin
    AssignFile(FLogFile, FFileName);
    if FileExists(FFileName) then
      Append(FLogFile)
    else
      ReWrite(FLogFIle);
    FLogFileOpened  :=  true;
  end;
end;

procedure TFileLog.CloseFile;
begin
  if FLogFileOpened then
    System.CloseFile(FLogFile);
  FLogFileOpened  :=  false;
  FFileName :=  '';
end;

procedure TFileLog.WriteLn(const AStr: string);
begin
  if FLogFileOpened then
    system.Writeln(FLogFile, AStr);
end;

procedure TFileLog.Save;
begin
  if FLogFileOpened then
    Flush(FLogFile);
end;

procedure TFileLog.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    if FLogFileOpened then
      CloseFile;
  end;
end;

procedure TFileLog.Write(const AStr: string);
begin
  Write(Now, 0, AStr);
end;

procedure TFileLog.Write(const AType: integer; AStr: string);
begin
  Write(Now, AType, AStr);
end;

procedure TFileLog.Write(const ATime: TDateTime; const AType: integer;
  AStr: string);
var
  s : string;
  i : Integer;
begin
  OpenFile;
  s :=  AStr;
  if not FAllowBr then
    for i := 1 to Length(s) do
      if Ord(s[i]) in [10, 13] then
        s[i]  :=  #32;
  s := FormatDateTime('YYYY-MM-DD hh:mm:ss', ATime) +
      Format(' %d %s', [AType, s]);
  Writeln(s);
end;

procedure TFileLog.Close;
begin
  CloseFile;
end;

end.
