// Common Framework
// DataSnap Client·â×°µ¥Ôª
{$I CommonFramework.inc}
unit uCFDSClientClasses;

interface

uses
  SysUtils, Classes, Generics.Collections, Variants, DB, DBClient, Windows,
  DBXCommon, SqlExpr, uCFIntfDef, uCFClasses, uCFConsts;

const
  IID_ICFDSCommand: TGUID = '{7A426F36-0316-4CC4-AAA5-7246F96DD42F}';

type
  TCFDSClient = class;

  ICFDSCommand = interface(ICFCommand)
    ['{7A426F36-0316-4CC4-AAA5-7246F96DD42F}']
    procedure SetDSClient(const ADSClient : TCFDSClient);
    function  GetDSClient : TCFDSClient;
  end;

  //
  TCFDSClient = class(TCFObject)
  private
    FSQLConnection: TSQLConnection;
    FLastErrorMessage: string;
  private
    function GetDBXConnection: TDBXConnection;
    function GetConnected: Boolean;
  public
    property LastErrorMessage: string read FLastErrorMessage;
    property SQLConnection: TSQLConnection read FSQLConnection;
    property DBXConnection: TDBXConnection read GetDBXConnection;
    property Connected: Boolean read GetConnected;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ConnectTo(const AAddress: string; const APort: integer): Boolean;
  end;

  TCFDSClientExecutor = class(TCFCommandExecutor)
  private
    FDSClient: TCFDSClient;
  private
    function GetConnected: Boolean;
  protected
    procedure SetCommand(const Value: ICFCommand); override;
  public
    property DSClient : TCFDSClient read FDSClient;
    property Connected: Boolean read GetConnected;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Stop; override;
  end;

  TCFDSCommand = class(TCFCommand, ICFDSCommand)
  protected
    FDSClient : TCFDSClient;
  protected
    // ICFDSCommand
    procedure SetDSClient(const ADSClient : TCFDSClient);
    function  GetDSClient : TCFDSClient;
  public
    property DSClient : TCFDSClient read GetDSClient write SetDSClient;
  end;

  TCFDSCommand_ConnectTo = class(TCFDSCommand)
  protected
    procedure Execute; override;
    function  GetID : Integer; override;
  end;

implementation

uses
  uCFResource;

{ TCFDSClient }

constructor TCFDSClient.Create;
begin
  inherited;
  FSQLConnection := TSQLConnection.Create(nil);
  FSQLConnection.DriverName :=  'Datasnap';
  FSQLConnection.LoginPrompt := False;
end;

destructor TCFDSClient.Destroy;
begin
  FSQLConnection.Free;
  inherited;
end;

function TCFDSClient.GetConnected: Boolean;
begin
  Result := SQLConnection.Connected;
end;

function TCFDSClient.GetDBXConnection: TDBXConnection;
begin
  Result := SQLConnection.DBXConnection;
end;

function TCFDSClient.ConnectTo(const AAddress: string;
  const APort: integer): Boolean;
begin
  if (SQLConnection.Params.Values['HostName'] <> AAddress) or
    (SQLConnection.Params.Values['Port'] <> IntToStr(APort)) then
    SQLConnection.Connected := False;
  SQLConnection.Params.Values['HostName'] := AAddress;
  SQLConnection.Params.Values['Port'] := IntToStr(APort);
  try
    SQLConnection.Connected := True;
  except
    on E: Exception do
      FLastErrorMessage := E.Message;
  end;
  Result := Connected;
end;

{ TCFDSClientExecutor }

constructor TCFDSClientExecutor.Create;
begin
  inherited Create;
  FDSClient :=  TCFDSClient.Create;
end;

destructor TCFDSClientExecutor.Destroy;
begin
  FDSClient.Free;
  inherited;
end;

function TCFDSClientExecutor.GetConnected: Boolean;
begin
  Result  :=  DSClient.Connected;
end;

procedure TCFDSClientExecutor.SetCommand(const Value: ICFCommand);
var
  ic : ICFDSCommand;
begin
  inherited;
  if Assigned(Command) and (Command.QueryInterface(ICFDSCommand, ic) = S_OK) then
     ic.SetDSClient(DSClient);
end;

procedure TCFDSClientExecutor.Stop;
begin
  //DSClient.SQLConnection.
  inherited;
end;

{ TCFDSCommand }

function TCFDSCommand.GetDSClient: TCFDSClient;
begin
  Result  :=  FDSClient;
end;

procedure TCFDSCommand.SetDSClient(const ADSClient: TCFDSClient);
begin
  FDSClient :=  ADSClient;
end;

{ TCFDSCommand_ConnectTo }

procedure TCFDSCommand_ConnectTo.Execute;
begin
  inherited;
  FResultCode :=  -1;
  if not Assigned(DSClient) then
  begin
    FErrorMessage :=  SEParam;
    Exit;
  end;

  if DSClient.ConnectTo(Params[0], Params[1]) then
    FResultCode :=  CommandResultCode_OK
  else begin
    FErrorMessage :=  DSClient.LastErrorMessage;
  end;
end;

function TCFDSCommand_ConnectTo.GetID: Integer;
begin
  Result  :=  CommandID_ConnectTo;
end;

end.
