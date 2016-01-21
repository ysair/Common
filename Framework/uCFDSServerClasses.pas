// Common Framework
// DataSnap Server封装单元
{$I CommonFramework.inc}
unit uCFDSServerClasses;

interface

uses
  SysUtils, Classes, Generics.Collections, Variants, DB, DBClient, Windows,
  DSCommonServer, DSServer, DSTCPServerTransport, DBXCommon, DBCommonTypes,
  DBXTransport,
  uCFIntfDef, uCFClasses, uCFTypedef;

type
  PDSServerClassInfo = ^TDSServerClassInfo;
  TDSServerClassInfo = record
    ServerClass : TDSServerClass;
    PersistentClass: TPersistentClass;
  end;

  //封装DataSnap Server类
  TCFDSServer = class(TCFObject)
  private
    FServer : TDSServer;
    FTCPServerTransport  : TDSTCPServerTransport;
    FServerClassList : TList<PDSServerClassInfo>;
    FOnMessage: TNotifyMessageEvent;
    FOnServerConnect: TDSConnectEvent;
    FOnServerDisconnect: TDSConnectEvent;
    FCompression: Boolean;
  private
    function  GetStarted: boolean;
    function  FindDSServerClassInfo(const APersistentClass: TPersistentClass):PDSServerClassInfo; overload;
    function  FindDSServerClassInfo(const AServerClass: TDSServerClass):PDSServerClassInfo; overload;
    procedure DSServerClassGetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
    procedure ClearServerClassList;
    function  GetTCPPort: Integer;
    procedure SetTCPPort(const Value: Integer);
    function  GetBufferKBSize: Integer;
    procedure SetBufferKBSize(const Value: Integer);
    function  GetMaxThreads: Integer;
    procedure SetMaxThreads(const Value: Integer);
    function  GetPoolSize: Integer;
    procedure SetPoolSize(const Value: Integer);
  protected
    property Server : TDSServer read FServer;
    property TCPServerTransport : TDSTCPServerTransport read FTCPServerTransport;
  protected
    procedure DoMessage(const AMsg : string); virtual;
    procedure DSServerOnConnect(DSConnectEventObject: TDSConnectEventObject); virtual;
    procedure DSServerOnDisconnect(DSConnectEventObject: TDSConnectEventObject); virtual;
    procedure DSServerOnCreateInstance(DSCreateInstanceEventObject: TDSCreateInstanceEventObject); virtual;
    function  DSServerOnTrace(TraceInfo: TDBXTraceInfo): CBRType; virtual;
  public
    property Compression : Boolean read FCompression write FCompression;
    property Started : boolean read GetStarted;
    property TCPPort : Integer read GetTCPPort write SetTCPPort;
    property BufferKBSize : Integer read GetBufferKBSize write SetBufferKBSize;
    property PoolSize : Integer read GetPoolSize write SetPoolSize;
    property MaxThreads : Integer read GetMaxThreads write SetMaxThreads;
    property OnMessage : TNotifyMessageEvent read FOnMessage write FOnMessage;
    property OnServerConnect : TDSConnectEvent read FOnServerConnect write FOnServerConnect;
    property OnServerDisconnect : TDSConnectEvent read FOnServerDisconnect write FOnServerDisconnect;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Start; virtual;
    procedure Stop; virtual;
    procedure RegisterServerClass(const AClass : TPersistentClass);
    procedure UnregisterServerClass(const AClass : TPersistentClass);
  end;

  {$MethodInfo ON}
  TCFServerObject = class(TCFPersistentObject)
  end;
  {$MethodInfo OFF}

  {$IFDEF FRAMEWORK_INCLUDE_DB}
  //支持数据库操作的ServerObject基类
  TCFDBServerObject = class(TCFServerObject)
  protected
    function GetStorage : ICFDataBaseStorage; virtual;
    function TableExists(const ATableName : string):Boolean;
    function ExecSQL(const ASQL : string; out ErrMsg : string):Boolean; virtual;
    function DoQuery(const ASQL : string; out ErrMsg : string; const AKeyField: string = ''): TClientDataSet; overload; virtual;
    function DoQuery(const ASQL : string; var AData : OleVariant; out ErrMsg : string; const AKeyField: string = ''):boolean; overload; virtual;
    function DoUpdate(const ASQL : string; const AData : OleVariant; out ErrMsg : string):boolean; virtual;
    function DoQueryCount(const ASQL : string; out ErrMsg : string): Integer; virtual;
    function DoQueryValue(const ASQL : string; var AValue : Variant; out ErrMsg : string): Boolean; virtual;
    function DoQueryIntValue(const ASQL : string; var AValue : Integer; out ErrMsg : string): Boolean; virtual;
    function DoQueryDoubleValue(const ASQL : string; var AValue : Double; out ErrMsg : string): Boolean; virtual;
    function DoQueryStringValue(const ASQL : string; var AValue : string; out ErrMsg : string): Boolean; virtual;
    function DoQueryList(const ASQL : string; const AList : TStrings; out ErrMsg : string): Boolean; virtual;
  end;

  //表基类
  TCFDBTable = class(TCFDBServerObject)
  private
    class var FTableName: string;
    class var FIDFieldName: string;
  protected
    class property TableName : string read FTableName write FTableName;
    class property IDFieldName : string read FIDFieldName write FIDFieldName;
  protected
    function  GetNewID(out ErrMsg : string) : Integer; virtual;
    function  GetEmpty(var AData : OleVariant; out ErrMsg : string):Boolean; virtual;
    function  GetAll(var AData : OleVariant; out ErrMsg : string):Boolean; virtual;
    function  QueryByID(const AID : Integer; var AData : OleVariant; out ErrMsg : string):Boolean; virtual;
    function  Update(const AData : OleVariant; out ErrMsg : string):Boolean; virtual;
  public
    class constructor Create;
  end;

  //ID生成器
  IDMaker = class(TCFObject)
  private
    class var FDB : TCFDBServerObject;
    class var FDict : TDictionary<string, Integer>;
    class var FCritical: TCFCritical;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create; override;

    class function GetNewID(const ATableName : string;
        const AIDFieldName : string; out ErrMsg : string):Integer;
  end;
  {$ENDIF}

implementation

uses
  uCFGlobal, uCFConsts, uCFResource, fcConvert, IdTCPConnection, IdWinsock2;

type
  TCP_KeepAlive = record
    OnOff: Cardinal;
    KeepAliveTime: Cardinal;
    KeepAliveInterval: Cardinal;
  end;

const
  Filter_Compression  = 'ZLibCompression';

{ TCFDSServer }

constructor TCFDSServer.Create;
begin
  inherited;
  FServerClassList  :=  TList<PDSServerClassInfo>.Create;
  FServer :=  TDSServer.Create(nil);
  FServer.OnConnect     :=  DSServerOnConnect;
  FServer.OnDisconnect  :=  DSServerOnDisconnect;
  FServer.OnTrace       :=  DSServerOnTrace;
  FServer.AutoStart :=  False;
  FTCPServerTransport  :=  TDSTCPServerTransport.Create(nil);
  FTCPServerTransport.Server :=  FServer;
end;

destructor TCFDSServer.Destroy;
begin
  Stop;
  ClearServerClassList;
  FServerClassList.Free;
  FTCPServerTransport.Free;
  FServer.Free;
  inherited;
end;

procedure TCFDSServer.DoMessage(const AMsg: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(self, AMsg);
end;

procedure TCFDSServer.ClearServerClassList;
var
  i : Integer;
begin
  for i := FServerClassList.Count - 1 downto 0 do
  begin
    FServerClassList[i].ServerClass.Free;
    Dispose(FServerClassList[i]);
    FServerClassList.Delete(i);
  end;
end;

function TCFDSServer.GetStarted: boolean;
begin
  Result :=  Server.Started;
end;

function TCFDSServer.GetBufferKBSize: Integer;
begin
  Result  :=  FTCPServerTransport.BufferKBSize;
end;

procedure TCFDSServer.SetBufferKBSize(const Value: Integer);
begin
  FTCPServerTransport.BufferKBSize :=  Value;
end;

function TCFDSServer.GetMaxThreads: Integer;
begin
  Result  :=  FTCPServerTransport.MaxThreads;
end;

procedure TCFDSServer.SetMaxThreads(const Value: Integer);
begin
  FTCPServerTransport.MaxThreads :=  Value;
end;

function TCFDSServer.GetPoolSize: Integer;
begin
  Result  :=  FTCPServerTransport.PoolSize;
end;

procedure TCFDSServer.SetPoolSize(const Value: Integer);
begin
  FTCPServerTransport.PoolSize :=  Value;
end;

function TCFDSServer.GetTCPPort: Integer;
begin
  Result  :=  FTCPServerTransport.Port;
end;

procedure TCFDSServer.DSServerOnConnect(
  DSConnectEventObject: TDSConnectEventObject);
var
  Val: TCP_KeepAlive;
  Ret: DWord;
  ClientConnection: TIdTCPConnection;
begin
  if DSConnectEventObject.ChannelInfo <> nil then
  begin
    DoMessage('【User Connected】');
    DoMessage(#9'[ID]'+IntToStr(DSConnectEventObject.ChannelInfo.Id));
    DoMessage(#9'[Info]' +DSConnectEventObject.ChannelInfo.Info );
    DoMessage(#9'[ServerConnection]' +DSConnectEventObject.ConnectProperties[TDBXPropertyNames.ServerConnection]);
    DoMessage('');

    ClientConnection := TIdTCPConnection(DSConnectEventObject.ChannelInfo.Id);
    Val.OnOff := 1;
    Val.KeepAliveTime := 5000;
    Val.KeepAliveInterval := 3000;
    WSAIoctl(ClientConnection.Socket.Binding.Handle, IOC_IN or IOC_VENDOR or 4,
        @Val, SizeOf(Val), nil, 0, @Ret, nil, nil); //}
    DoMessage('Connect : ' + ClientConnection.Socket.Binding.PeerIP +
        ':' + IntToStr(ClientConnection.Socket.Binding.PeerPort));
  end;
  if Assigned(FOnServerConnect) then
    FOnServerConnect(DSConnectEventObject);
end;

procedure TCFDSServer.DSServerOnDisconnect(
  DSConnectEventObject: TDSConnectEventObject);
var
  ClientConnection: TIdTCPConnection;
begin
  if DSConnectEventObject.ChannelInfo <> nil then
  begin
    ClientConnection := TIdTCPConnection(DSConnectEventObject.ChannelInfo.Id);
    DoMessage('【Disconnect】[ID]'+IntToStr(DSConnectEventObject.ChannelInfo.Id)+
        ClientConnection.Socket.Binding.PeerIP +
        ':' + IntToStr(ClientConnection.Socket.Binding.PeerPort));
  end;
  if Assigned(OnServerDisconnect) then
    OnServerDisconnect(DSConnectEventObject);
end;

procedure TCFDSServer.DSServerOnCreateInstance(
  DSCreateInstanceEventObject: TDSCreateInstanceEventObject);
begin
end;

function TCFDSServer.DSServerOnTrace(TraceInfo: TDBXTraceInfo): CBRType;
begin
  DoMessage('Trace : ' + TraceInfo.Message);
  Result  :=  cbrUSEDEF;
end;

procedure TCFDSServer.SetTCPPort(const Value: Integer);
begin
  FTCPServerTransport.Port :=  Value;
end;

procedure TCFDSServer.Start;
var
  i : Integer;
  bc : Boolean;
begin
  if not Server.Started then
  begin
    if Compression then
    begin
      bc  :=  False;
      for i := 0 to FTCPServerTransport.Filters.Count - 1 do
        if SameText(TTransportFilter(FTCPServerTransport.Filters.Items[i]).Id, Filter_Compression) then
        begin
          bc  :=  True;
          Break;
        end;
      if not bc then
        FTCPServerTransport.Filters.AddFilter(Filter_Compression);
    end;
    Server.Start;
  end;
end;

procedure TCFDSServer.Stop;
begin
  if Server.Started then Server.Stop;
end;

procedure TCFDSServer.RegisterServerClass(const AClass: TPersistentClass);
var
  info :  PDSServerClassInfo;
begin
  info  :=  FindDSServerClassInfo(AClass);
  if Assigned(info) then Exit;
  New(info);
  info.PersistentClass  :=  AClass;
  info.ServerClass  :=  TDSServerClass.Create(nil);
  info.ServerClass.Server :=  Server;
  info.ServerClass.OnGetClass :=  DSServerClassGetClass;
  info.ServerClass.LifeCycle  :=  'Session';
  info.ServerClass.OnCreateInstance :=  DSServerOnCreateInstance;
  FServerClassList.Add(info);
end;

procedure TCFDSServer.UnregisterServerClass(const AClass: TPersistentClass);
var
  info :  PDSServerClassInfo;
begin
  info  :=  FindDSServerClassInfo(AClass);
  if Assigned(info) then
  begin
    FServerClassList.Delete(FServerClassList.IndexOf(info));
    info.ServerClass.Free;
    Dispose(info);
  end;
end;

procedure TCFDSServer.DSServerClassGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
var
  info :  PDSServerClassInfo;
begin
  info  :=  FindDSServerClassInfo(DSServerClass);
  if Assigned(info) then
    PersistentClass :=  info.PersistentClass;
end;

function TCFDSServer.FindDSServerClassInfo(
  const AServerClass: TDSServerClass): PDSServerClassInfo;
var
  i : Integer;
begin
  Result  :=  nil;
  for i := FServerClassList.Count - 1 downto 0 do
    if FServerClassList[i].ServerClass = AServerClass then
    begin
      Result  :=  FServerClassList[i];
      Break;
    end;
end;

function TCFDSServer.FindDSServerClassInfo(
  const APersistentClass: TPersistentClass): PDSServerClassInfo;
var
  i : Integer;
begin
  Result  :=  nil;
  for i := FServerClassList.Count - 1 downto 0 do
    if FServerClassList[i].PersistentClass = APersistentClass then
    begin
      Result  :=  FServerClassList[i];
      Break;
    end;
end;

{$IFDEF FRAMEWORK_INCLUDE_DB}
{ TCFDSServerObject }

function TCFDBServerObject.GetStorage: ICFDataBaseStorage;
begin
  Result  :=  CFGlobal.InterfaceFactory.CreateInterface(IID_ICFDataBaseStorage) as ICFDataBaseStorage;
  if Assigned(Result) then
    Result.OpenConnection;
end;

function TCFDBServerObject.TableExists(const ATableName: string): Boolean;
var
  intf :  ICFDataBaseStorage;
begin
  Result  :=  False;
  intf  :=  GetStorage;
  if Assigned(intf) then
  try
    Result  :=  intf.TableExists(ATableName);
  finally
    intf.ReleaseInstance;
  end;
end;

function TCFDBServerObject.ExecSQL(const ASQL: string;
  out ErrMsg: string): Boolean;
var
  intf :  ICFDataBaseStorage;
begin
  Result  :=  False;
  intf  :=  GetStorage;
  if Assigned(intf) then
  try
    CFGlobal.Log.WriteLog(ltMessage, ASQL);
    Result  :=  intf.ExecSQL(ASQL);
    if not Result then
    begin
      ErrMsg  :=  intf.GetLastErrorMessage;
      CFGlobal.Log.WriteLog(ltError, ErrMsg);
    end;
  finally
    intf.ReleaseInstance;
  end
  else
    ErrMsg  :=  SEFactoryCreateInterfaceFail;
end;

function TCFDBServerObject.DoQuery(const ASQL: string;
  out ErrMsg: string; const AKeyField: string): TClientDataSet;
var
  intf :  ICFDataBaseStorage;
begin
  intf  :=  GetStorage;
  if Assigned(intf) then
  try
    CFGlobal.Log.WriteLog(ltMessage, ASQL);
    Result  :=  intf.Query(ASQL, AKeyField);
    if not Assigned(Result) then
    begin
      ErrMsg  :=  intf.GetLastErrorMessage;
      CFGlobal.Log.WriteLog(ltError, ErrMsg);
    end;
  finally
    intf.ReleaseInstance;
  end
  else begin
    Result  :=  nil;
    ErrMsg  :=  SEFactoryCreateInterfaceFail;
  end;
end;

function TCFDBServerObject.DoQuery(const ASQL: string; var AData: OleVariant;
  out ErrMsg: string; const AKeyField: string): boolean;
var
  ds : TClientDataSet;
begin
  ds  :=  DoQuery(ASQL, ErrMsg, AKeyField);
  if Assigned(ds) then
  try
    AData :=  ds.Data;
    Result  :=  True;
  finally
    ds.Free;
  end
  else
    Result  :=  False;
end;

function TCFDBServerObject.DoQueryCount(const ASQL: string;
  out ErrMsg: string): Integer;
var
  ds : TClientDataSet;
begin
  ds  :=  DoQuery(Format('select count(*) from (%s) as a', [ASQL]), ErrMsg);
  if Assigned(ds) then
  try
    if ds.IsEmpty then
      Result  :=  0
    else
      Result  :=  ds.Fields[0].AsInteger;
  finally
    ds.Free;
  end
  else
    Result  :=  -1;
end;

function TCFDBServerObject.DoQueryDoubleValue(const ASQL: string;
  var AValue: Double; out ErrMsg: string): Boolean;
var
  ds : TClientDataSet;
begin
  Result := False;
  ds  :=  DoQuery(ASQL, ErrMsg);
  if Assigned(ds) then
  try
    if ds.RecordCount > 0 then
    begin
      AValue  :=  ds.Fields[0].AsFloat;
      Result  :=  True;
    end
  finally
    ds.Free;
  end
end;

function TCFDBServerObject.DoQueryValue(const ASQL : string;
    var AValue : Variant; out ErrMsg : string): Boolean;
var
  ds : TClientDataSet;
begin
  Result := False;
  ds  :=  DoQuery(ASQL, ErrMsg);
  if Assigned(ds) then
  try
    if ds.RecordCount > 0 then
    begin
      AValue  :=  ds.Fields[0].Value;
      Result  :=  True;
    end
  finally
    ds.Free;
  end
end;

function TCFDBServerObject.DoQueryList(const ASQL: string;
  const AList: TStrings; out ErrMsg: string): Boolean;
var
  ds : TClientDataSet;
begin
  ds  :=  DoQuery(ASQL, ErrMsg);
  if Assigned(ds) then
  try
    AList.Clear;
    while not ds.Eof do
    begin
      AList.Add(ds.Fields[0].AsString);
      ds.Next;
    end;
    Result  :=  True;
  finally
    ds.Free;
  end
  else
    Result  :=  False;
end;

function TCFDBServerObject.DoQueryStringValue(const ASQL: string;
  var AValue: string; out ErrMsg: string): Boolean;
var
  ds : TClientDataSet;
begin
  Result := False;
  ds  :=  DoQuery(ASQL, ErrMsg);
  if Assigned(ds) then
  try
    if ds.RecordCount > 0 then
    begin
      AValue  :=  ds.Fields[0].AsString;
      Result  :=  True;
    end
  finally
    ds.Free;
  end
end;

function TCFDBServerObject.DoQueryIntValue(const ASQL: string; var AValue: Integer;
  out ErrMsg: string): Boolean;
var
  ds : TClientDataSet;
begin
  Result := False;
  ds  :=  DoQuery(ASQL, ErrMsg);
  if Assigned(ds) then
  try
    if ds.RecordCount > 0 then
    begin
      AValue  :=  ds.Fields[0].AsInteger;
      Result  :=  True;
    end
  finally
    ds.Free;
  end
end;

function TCFDBServerObject.DoUpdate(const ASQL: string; const AData: OleVariant;
  out ErrMsg: string): boolean;
var
  intf :  ICFDataBaseStorage;
  ds : TClientDataSet;
begin
  Result  :=  False;
  intf  :=  GetStorage;
  if Assigned(intf) then
  try
    CFGlobal.Log.WriteLog(ltMessage, ASQL);
    ds  :=  TClientDataSet.Create(nil);
    try
      ds.Data :=  AData;
      ds.CommandText  :=  ASQL;
      Result  :=  intf.Update(ds);
      if not Result then
      begin
        ErrMsg  :=  intf.GetLastErrorMessage;
        CFGlobal.Log.WriteLog(ltError, ErrMsg);
      end;
    finally
      ds.Free;
    end;
  finally
    intf.ReleaseInstance;
  end
  else
    ErrMsg  :=  SEFactoryCreateInterfaceFail;
end;

{ TCFDBTable }

class constructor TCFDBTable.Create;
begin
  FIDFieldName  :=  DB_DefField_ID;
end;

function TCFDBTable.GetNewID(out ErrMsg : string): Integer;
begin
  Result  :=  IDMaker.GetNewID(TableName, IDFieldName, ErrMsg);
end;

function TCFDBTable.GetEmpty(var AData: OleVariant;
  out ErrMsg: string): Boolean;
begin
  Result  :=  DoQuery(Format('select * from %s where 1=2', [TableName]), AData, ErrMsg);
end;

function TCFDBTable.GetAll(var AData: OleVariant; out ErrMsg: string): Boolean;
begin
  Result  :=  DoQuery(Format('select * from %s', [TableName]), AData, ErrMsg);
end;

function TCFDBTable.QueryByID(const AID: Integer; var AData: OleVariant;
  out ErrMsg: string): Boolean;
begin
  Result  :=  DoQuery(Format(
      'select * from %s where %s = %d',
      [TableName
      ,IDFieldName
      ,AID
      ]), AData, ErrMsg);
end;

function TCFDBTable.Update(const AData: OleVariant;
  out ErrMsg: string): Boolean;
begin
  Result  :=  DoUpdate(Format('select * from %s', [TableName]), AData, ErrMsg);
end;

{ IDMaker }

class constructor IDMaker.Create;
begin
  FCritical :=  TCFCritical.Create;
  FDB :=  TCFDBServerObject.Create;
  FDict :=  TDictionary<string, Integer>.Create;
end;

class destructor IDMaker.Destroy;
begin
  FDict.Free;
  FDB.Free;
  FCritical.Free;
end;

constructor IDMaker.Create;
begin
  inherited;
  raise Exception.Create(SECantCreateInstance);
end;

class function IDMaker.GetNewID(const ATableName, AIDFieldName: string;
  out ErrMsg: string): Integer;
var
  id : Integer;
  v : Variant;
begin
  FCritical.Enter;
  try
    if FDict.TryGetValue(ATableName, id) then
    begin
      Inc(id);
      FDict.AddOrSetValue(ATableName, id);
      Exit(id);
    end;
  finally
    FCritical.Leave;
  end;

  if FDB.DoQueryValue(Format('select max(%s) from %s', [AIDFieldName, ATableName]), v, ErrMsg) then
  begin
    FCritical.Enter;
    try
      if not FDict.TryGetValue(ATableName, id) then
        if VarIsNumeric(v) then
          id :=  v
        else
          id :=  0;
      Inc(id);
      FDict.AddOrSetValue(ATableName, id);
      Exit(id);
    finally
      FCritical.Leave;
    end;
  end
  else
    Result  :=  -1;
end;
{$ENDIF}

end.
