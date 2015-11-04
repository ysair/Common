unit fcUdpTime;

interface

uses
  Windows, SysUtils, IdUDPServer, IdUDPClient, IdSocketHandle, uCFClasses, IdGlobal;

type
  TTimeServer = class(TCFObject)
  private
    FServer:TIdUDPServer;
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetStarted: Boolean;
  public
    class procedure OnUDPRead(AThread: TIdUDPListenerThread; const AData: TIdDynByteArray;
      ABinding: TIdSocketHandle);

    property Port:Integer read GetPort write SetPort;
    property Started:Boolean read GetStarted;

    property Server:TIdUDPServer read FServer;

    constructor Create; override;
    destructor Destroy; override;

    function Start:Boolean;
    function Stop:Boolean;
  end;

  TTimeClient = class(TObject)
  private
    FHost:string;
    FPort:Integer;
    FClient:TIdUDPClient;
    FLock: TRTLCriticalSection;
  public
    property Host:string read FHost write FHost;
    property Port:Integer read FPort write FPort;
  public
    constructor Create;
    destructor Destroy; override;

    function GetServerTime(var AValue:TDateTime):Boolean;
  end;

implementation

uses
  fcSystem;


constructor TTimeClient.Create;
begin
  FClient := TIdUDPClient.Create(nil);
  FClient.ReceiveTimeout := 5000;
  FHost := 'localhost';
  FPort := 7401;
  InitializeCriticalSection(FLock);
end;

destructor TTimeClient.Destroy;
begin
  FClient.Free;
  DeleteCriticalSection(FLock);
end;

function TTimeClient.GetServerTime(var AValue:TDateTime):Boolean;
var
  bytes:TBytes;
begin
  EnterCriticalSection(FLock);
  try
    SetLength(bytes, 8);
    if (FClient.Host <> FHost) or (FClient.Port <> FPort) then
    begin
      FClient.Host := FHost;
      FClient.Port := FPort;
      if FClient.Connected then
      try
        FClient.Disconnect;
      except
      end;
    end;

    try
      FClient.Connect;
      FClient.Send('dt', TEncoding.ASCII);
      Result := 8 = FClient.ReceiveBuffer(bytes, 8);
      AValue := FCSysUtils.BytesToDouble(bytes);
    except
      Result := False;
    end;

    if (not Result) and FClient.Connected then
    try
      FClient.Disconnect;
    except
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TTimeServer }

constructor TTimeServer.Create;
begin
  inherited;
  FServer := TIdUDPServer.Create;
  FServer.OnUDPRead := OnUDPRead;
end;

destructor TTimeServer.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

function TTimeServer.GetPort: Integer;
begin
  Result := FServer.Bindings.DefaultPort;
end;

function TTimeServer.GetStarted: Boolean;
begin
  Result := FServer.Active;
end;

class procedure TTimeServer.OnUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdDynByteArray; ABinding: TIdSocketHandle);
var
  buf:TBytes;
begin
  buf := FCSysUtils.DoubleToBytes(Now);
  ABinding.SendTo(ABinding.PeerIP, ABinding.PeerPort, buf, 0, 8);
end;

procedure TTimeServer.SetPort(const Value: Integer);
begin
  if FServer.Bindings.DefaultPort <> Value then
  begin
    FServer.Active := False;
    FServer.Bindings.DefaultPort := Value;
  end;
end;

function TTimeServer.Start: Boolean;
begin
  FServer.Active := True;
  Result := Started;
end;

function TTimeServer.Stop: Boolean;
begin
  FServer.Active := False;
  Result := not Started;
end;

end.
