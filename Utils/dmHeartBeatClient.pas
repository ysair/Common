unit dmHeartBeatClient;

interface

uses DBXCommon, DBXJSON, Classes, SysUtils, DB, DBXDBReaders;

type
  //设置一个datasnap的heartBeatCommand，如'TDNHeartBeat.HeartBeat'，来建立心跳，
  //目的是用于SqlConnectionPool的维护。
  TDmHeartBeatClient = class
  private
    FCommand: string;
    FDBXConnection: TDBXConnection;
    FHeartBeatCommand: TDBXCommand;
    procedure SetDBXConnection(const Value: TDBXConnection);
  public
    property DBXConnection: TDBXConnection read FDBXConnection write
      SetDBXConnection;

    constructor Create(heartBeatCommand: string);
    destructor Destroy; override;
    procedure HeartBeat;
  end;

implementation

{ THeartBeatClient }

procedure TDmHeartBeatClient.HeartBeat;
begin
  if FDBXConnection = nil then
    raise EInvalidOperation.Create(
      'Connection cannot be nil.  Make sure the connection has been opened.');

  if FHeartBeatCommand = nil then
  begin
    FHeartBeatCommand := FDBXConnection.CreateCommand;
    FHeartBeatCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FHeartBeatCommand.Text := FCommand; // 'TDNHeartBeat.HeartBeat';
    FHeartBeatCommand.Prepare;
  end;
  FHeartBeatCommand.ExecuteUpdate;
end;

procedure TDmHeartBeatClient.SetDBXConnection(const Value: TDBXConnection);
begin
  if FDBXConnection <> Value then
  begin
    FDBXConnection := Value;
    FreeAndNil(FHeartBeatCommand);
  end;
end;

constructor TDmHeartBeatClient.Create(heartBeatCommand: string);
begin
  FCommand := heartBeatCommand;
end;

destructor TDmHeartBeatClient.Destroy;
begin
  FreeAndNil(FHeartBeatCommand);
  inherited;
end;

end.
