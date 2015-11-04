unit uCFIndyHttpServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, EncdDecd,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer, IdContext,
  IdHTTPServer,
  uCFIntfDef, uCFClasses;

type
  TCFIndyHttpServer = class(TCFObject)
  private
    FHttp : TIdHttpServer;
  private
    function GetPort: Integer;
    function GetStarted: boolean;
    procedure SetPort(const Value: Integer);
  protected
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        AResponseInfo: TIdHTTPResponseInfo); virtual;
  public
    property Started : boolean read GetStarted;
    property Port : Integer read GetPort write SetPort;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

{ THttpServer }

constructor TCFIndyHttpServer.Create;
begin
  inherited;
  FHttp :=  TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet  :=  OnCommandGet;
end;

destructor TCFIndyHttpServer.Destroy;
begin
  FHttp.Free;
  inherited;
end;

function TCFIndyHttpServer.GetPort: Integer;
begin
  Result  :=  FHttp.DefaultPort;
end;

procedure TCFIndyHttpServer.SetPort(const Value: Integer);
begin
  FHttp.DefaultPort :=  Value;
end;

function TCFIndyHttpServer.GetStarted: boolean;
begin
  Result  :=  FHttp.Active;
end;

procedure TCFIndyHttpServer.Start;
begin
  FHttp.Active  :=  True;
end;

procedure TCFIndyHttpServer.Stop;
begin
  FHttp.Active  :=  False;
end;

procedure TCFIndyHttpServer.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin

end;

end.
