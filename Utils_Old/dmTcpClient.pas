unit dmTcpClient;

interface

uses
  SysUtils, Classes, IdSocks, IdIOHandler, IdIOHandlerSocket,IdGlobal,
  IdBaseComponent, IdComponent, Types, Windows,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdConnectThroughHttpProxy,
  IdCustomTransparentProxy,  IdTCPConnection, IdTCPClient;

type
  PSSLOptions = ^TSSLOptions;
  TSSLOptions = record
    Mode: TIdSSLMode;
  end;
  PSocketProxy = ^TSocketProxy;
  TSocketProxy = record
    sName: String;
    sType: TSocksVersion;
    sHost: String;
    sPort: Integer;
    sUser: String;
    sPass: String;
  end;
  PHttpProxy = ^THttpProxy;
  THttpProxy = record
     hName: String;
     hHost: String;
     hPort: Integer;
     hUser: String;
     hPass: String;
  end;

  { 结构图示：
                 -- SSL    --
    TcpClient <                > --- Socks Proxy ---- Http Proxy
                 -- Normal --
  }
//2005,11,28
//今天算是学到东西了, 总结一下,2 点
// 当 TRapTCPClient 创建的时候, AOwner如果是Application, 可能有以下问题:
//  1. 当 TRapTcpCLient所在的单元/组件释放的时候,但是Application没有释放, 或者反之,是会出错的.
//  2. 今天测试出代理不能用,最后发现原因了... TRapTCPClient 的创建参数是nil
//     而TIdIOHandlerSocket的创建参数是 AOwner....哈哈,看来对这个属主的了解更加深刻了一些了.

  TRapTCPClient = class(TIdTCPClient)
  private
    FNormalSocks: TIdIOHandlerStack;
    FSSLSock: TIdSSLIOHandlerSocketOpenSSL;
    FSocksPorxy: TIdSocksInfo;
    FHttpProxy: TIdConnectThroughHttpProxy;
    FOwnerComponent : TComponent;   //所有的组件的父容器。需要统一的属主，否则代理不能用

    procedure FreeSocketsInfo;
  public
    destructor Destroy; override;

    procedure Set_SSL_Proxy(
          const AUsedSSL: Boolean=false;
          const AUsedSocketProxy: Boolean=false;
          const AUsedHttpProxy: Boolean=false;
          const ASSLInfo: PSSLOptions=nil;
          const ASocketProxyInfo: PSocketProxy=nil;
          const AHttpProxyInfo: PHttpProxy=nil);
  end;

  //使用线程的方式来打开一个Socket，能避免程序进入假死状态
  TSocketOpenThread=class(TThread)
  private
    FSocket:   TRapTCPClient;
    FPRunning: ^boolean;
    FTimeOut:  Integer;
  protected
    procedure Execute;override;
  public
    bOpenError: boolean;
    constructor Create( var ASocket:TRapTCPClient;
                        var ARunning:boolean;
                        const ATimeOut: Integer=20*1000);
    destructor Destroy; override;
  end;


//打开socket连接断口, ATimeOut = -1 的时候表示无限等待直到打开端口或者出错
function SocketOpen(var ATcpClient: TRapTCPClient; var ACanceled:boolean; const ATimeOut: Integer=20*1000): Boolean;

implementation

{ TRapTCPClient }


destructor TRapTCPClient.Destroy;
begin
  if Connected then Disconnect;

  FreeSocketsInfo;

  inherited;
end;

procedure TRapTCPClient.FreeSocketsInfo;
begin
  if FHttpProxy<>nil then FHttpProxy.Free;
  if FSocksPorxy<>nil then FSocksPorxy.Free;
  if FNormalSocks<>nil then FNormalSocks.Free;
  if FSSLSock<>nil then FSSLSock.Free;
  if FOwnerComponent<>nil then  FOwnerComponent.Free;
end;

procedure TRapTCPClient.Set_SSL_Proxy(const AUsedSSL: Boolean;
  const AUsedSocketProxy: Boolean;
  const AUsedHttpProxy: Boolean; const ASSLInfo: PSSLOptions;
  const ASocketProxyInfo: PSocketProxy; const AHttpProxyInfo: PHttpProxy);
var
  l_hs: TIdIOHandlerSocket;
begin
  FreeSocketsInfo;

  FOwnerComponent := TComponent.Create(Self.Owner);

  //设置SSL
  if AUsedSSL and (ASSLInfo<>nil) then
  begin
    FSSLSock := TIdSSLIOHandlerSocketOpenSSL.Create(FOwnerComponent );
    FSSLSock.MaxLineAction := maSplit;
    FSSLSock.SSLOptions.Mode := ASSLInfo.Mode;
    Self.IOHandler := FSSLSock;
    l_hs := FSSLSock;
  end
  else  //普通的sock链接，创建TIdIOHandlerStack的目的可以设置MaxLineAction
  begin
    FNormalSocks := TIdIOHandlerStack.Create(FOwnerComponent );
    FNormalSocks.MaxLineAction := maSplit;
    Self.IOHandler := FNormalSocks;
    l_hs := FNormalSocks;
  end;

  //只要是使用了Socket代理或者Http代理，都必须创建 FSocksPorxy
  if (AUsedSocketProxy and (ASocketProxyInfo<>nil))
    or (AUsedHttpProxy and (AHttpProxyInfo<>nil)) then
  begin
    FSocksPorxy := TIdSocksInfo.Create(FOwnerComponent);
    FSocksPorxy.Version := svNoSocks;
    l_hs.TransparentProxy := FSocksPorxy;

    //设置Socket代理
    if AUsedSocketProxy and (ASocketProxyInfo<>nil) then
    begin
      FSocksPorxy.Version := ASocketProxyInfo.sType;
      case ASocketProxyInfo.sType of
        svNoSocks:;
        svSocks4, svSocks4A :
          begin
            FSocksPorxy.Authentication := saNoAuthentication;
            FSocksPorxy.Host := ASocketProxyInfo.sHost;
            FSocksPorxy.Port := ASocketProxyInfo.sPort;
          end;
        svSocks5 :
          begin
            FSocksPorxy.Authentication := saUsernamePassword;
            FSocksPorxy.Host := ASocketProxyInfo.sHost;
            FSocksPorxy.Port := ASocketProxyInfo.sPort;
            FSocksPorxy.Username := ASocketProxyInfo.sName;
            FSocksPorxy.Password := ASocketProxyInfo.sPass;
          end;
      end;
    end;

    //设置Http代理
    if AUsedHttpProxy and (AHttpProxyInfo<>nil) then
    begin
      FHttpProxy := TIdConnectThroughHttpProxy.Create(FOwnerComponent);
      FSocksPorxy.ChainedProxy := FHttpProxy;
      FHttpProxy.Host := AHttpProxyInfo.hHost;
      FHttpProxy.Port := AHttpProxyInfo.hPort;
      FHttpProxy.Username := AHttpProxyInfo.hUser;
      FHttpProxy.Password := AHttpProxyInfo.hPass;
    end;
  end;
end;

{ TSocketOpenThread }

constructor TSocketOpenThread.Create(var ASocket: TRapTCPClient;
  var ARunning: boolean; const ATimeOut: Integer);
begin
  FSocket := ASocket;
  FPRunning := @ARunning;
  FPRunning^ := true;
  FTimeOut := ATimeOut;

  inherited Create(True);
end;

destructor TSocketOpenThread.Destroy;
begin
  FSocket := nil;
  FPRunning := nil;
  inherited;
end;

procedure TSocketOpenThread.Execute;
begin
  bOpenError := false;
  FreeOnTerminate := false;
  try
    FSocket.ConnectTimeout := FTimeOut;
    FSocket.Connect;
    FPRunning^ := false;
  except
    bOpenError := True;
  end;
end;

function SocketOpen(var ATcpClient: TRapTCPClient; var ACanceled:boolean; const ATimeOut: Integer):Boolean;

  procedure _Delay(AMilliSeconds: Dword);
  var
     l_Event :Thandle;
  begin
     l_Event := CreateEvent(nil,True,False,nil) ;
     waitforSingleObject(l_Event, AMilliSeconds);
     CloseHandle(l_Event);
  end;
var
  l_Thread: TSocketOpenThread;
  l_Running: Boolean;
  l_Start, l_TimeOut: DWord;
begin
  l_Running := false;

  if ATimeOut > 5*1000 then
    l_TimeOut := ATimeOut
  else
    l_TimeOut := 20*1000;

  l_Start := GetTickCount;
  l_Thread := TSocketOpenThread.Create(ATcpClient,l_Running, l_TimeOut);
  try
    l_Thread.Priority := tpLower;
    l_Thread.Resume;

    if ATimeOut > 0 then
    begin
      while l_Running and
            //(not AskToCloseApplication) and
            //(not AskToCloseAppWhenTheadComplete) and
            (GetTickCount - l_Start <= l_TimeOut) and
            (not ACanceled) and
            (not l_Thread.bOpenError) do
      begin
        _Delay(100);
      end;
    end
    else
    begin
      while l_Running and
            //(not AskToCloseApplication) and
            //(not AskToCloseAppWhenTheadComplete) and
            (not ACanceled) and
            (not l_Thread.bOpenError) do
      begin
        _Delay(100);
      end;
    end;

    if l_Running then
    begin
      Result := false;
      TerminateThread(l_Thread.Handle,1);
    end
    else
      Result := true;
  finally
    l_Thread.Free;
  end;
end;

end.
