//Common Framework
//ZEOS 存储类
{$I CommonFramework.inc}
unit uCFZEOSDBStorage;

interface

uses
  SysUtils, Classes, Windows, ADODB, DB, DBClient, Provider, ActiveX, Messages,
  uCFIntfDef, uCFClasses, uCFDBStorage, uCFConsts,
  ZAbstractRODataset, ZAbstractDataset, ZDataset, ZConnectionGroup,
  ZAbstractConnection, ZConnection;

type
  TCFZEOSDBStorage = class(TCFDBStorage)
  private
    FConnection : TZConnection;
    FQuery : TZQuery;
    FDataSet : TZQuery;
    FProvider : TDataSetProvider;
    FOldConnectionString : string;
  protected
    function  OpenConnection : Boolean; override;
    procedure CloseConnection; override;
    function  GetConnected : boolean; override;
    function  SupportTrans : boolean; override;
    procedure BeginTrans; override;
    procedure CommitTrans; override;
    procedure RollbackTrans; override;
    function  TableExists(const ATableName : string):Boolean; override;
    function  ExecSQL(const ASQL : string):boolean; override;
    function  Query(const ASQL : string; const AKeyField : string) : TClientDataSet; override;
    function  Update(const ADataSet : TClientDataSet):boolean; override;
    procedure OnNotifyMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);override;
    procedure OnProviderUpdateError(Sender: TObject; DataSet: TCustomClientDataSet;
        E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse);
    procedure OnProviderBeforeUpdateRecord(Sender: TObject; SourceDS: TDataSet;
        DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind; var Applied: Boolean);
    procedure OnProviderAfterUpdateRecord(Sender: TObject; SourceDS: TDataSet;
        DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
    function  RefreshCache : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TZEOSDBStorageBuilder = class(TCFPoolInterfaceBuilder)
  protected
    function BuiltInterfaceGUID : TGUID; override;
    function BuiltClass : TClass; override;
  public
    constructor Create; override;
  end;

implementation

uses
  fcDatabase, uCFGlobal;

const
  TAG_KEYFIELD  = $77FF22AA;
  DEF_CacheSize = 1000;

constructor TCFZEOSDBStorage.Create;
{var
  cls : TComponentClass;
  ret : DWORD;//}
begin
  inherited Create;
  CoInitialize(nil);
  {if GetCurrentThreadId = CFGlobal.MainThreadID then
  begin
    FConnection := TADOConnection.Create(nil);
    FQuery := TADOQuery.Create(nil);
    FDataSet := TADODataSet.Create(nil);
    FProvider := TDataSetProvider.Create(nil);
  end
  else begin
    cls  :=  TADOConnection;
    SetLastError(0);
    ret :=  SendMessage(CFGlobal.MessageHandle, CF_MSG_CreateComponent, Integer(cls), 0);
    if GetLastError = 0 then
      FConnection :=  TADOConnection(Pointer(ret))
    else
      FConnection := TADOConnection.Create(nil);

    cls :=  TADOQuery;
    SetLastError(0);
    ret :=  SendMessage(CFGlobal.MessageHandle, CF_MSG_CreateComponent, Integer(cls), 0);
    if GetLastError = 0 then
      FQuery :=  TADOQuery(Pointer(ret))
    else
      FQuery := TADOQuery.Create(nil);

    cls :=  TADODataSet;
    SetLastError(0);
    ret :=  SendMessage(CFGlobal.MessageHandle, CF_MSG_CreateComponent, Integer(cls), 0);
    if GetLastError = 0 then
      FDataSet :=  TADODataSet(Pointer(ret))
    else
      FDataSet := TADODataSet.Create(nil);

    cls :=  TDataSetProvider;
    SetLastError(0);
    ret :=  SendMessage(CFGlobal.MessageHandle, CF_MSG_CreateComponent, Integer(cls), 0);
    if GetLastError = 0 then
      FProvider :=  TDataSetProvider(Pointer(ret))
    else
      FProvider := TDataSetProvider.Create(nil);
  end;//}
  FConnection := TZConnection.Create(nil);
  FQuery := TZQuery.Create(nil);
  FDataSet := TZQuery.Create(nil);
  FProvider := TDataSetProvider.Create(nil);

  FConnection.LoginPrompt :=  False;
  FQuery.Connection :=  FConnection;
  FDataSet.Connection :=  FConnection;
  FProvider.DataSet :=  FDataSet;
  FProvider.OnUpdateError :=  OnProviderUpdateError;
  FProvider.BeforeUpdateRecord  :=  OnProviderBeforeUpdateRecord;
  FProvider.AfterUpdateRecord := OnProviderAfterUpdateRecord;
  FProvider.ResolveToDataSet  :=  True;
  //FProvider.UpdateMode  :=  upWhereKeyOnly;
end;

destructor TCFZEOSDBStorage.Destroy;
begin
  CloseConnection;
  FConnection.Free;
  FQuery.Free;
  FDataSet.Free;
  FProvider.Free;
  inherited;
end;

procedure TCFZEOSDBStorage.OnNotifyMessage(Msg: UINT; wParam: WPARAM;
  lParam: LPARAM);
begin
  inherited;
  case Msg of
    CF_MSG_Config_Changed :
      begin
      end;
  end;
end;

function TCFZEOSDBStorage.OpenConnection: Boolean;

  //线程里打开ADOConnection,在XP下切换输入法程序会死掉
  //原因:ADO自动创建一个ADODB.AsyncEventMessenger窗口,然后会有一个对应的IME窗口,但线程里没有消息循环
  //     XP下输入法切换时会SendMessage给IME窗口并等待返回,IME窗口并不会处理消息,造成死锁
  //这个函数把线程里的IME窗口释放掉,切换输入法时就不会有消息过来了
  procedure _FreeIMEWindow;
  const
    IME_WINDOW_CLASS = 'IME';
    IME_WINDOW_TEXT  = 'Default IME';
  var
    h : HWND;
    pid : DWORD;
    dh : HWND;
  begin
    h :=  FindWindow(IME_WINDOW_CLASS, IME_WINDOW_TEXT);
    while IsWindow(h) do
    begin
      if GetWindowThreadProcessId(h, pid) = GetCurrentThreadId then
        dh  :=  h
      else
        dh  :=  0;
      h :=  FindWindowEx(0, h, IME_WINDOW_CLASS, IME_WINDOW_TEXT);
      if dh <> 0 then
        DestroyWindow(dh);
    end;
  end;

begin
  DoWorking;
  if Connected then
  begin
    if (FConnection.Protocol = GetProvider) and (FOldConnectionString = ConnectionString) then
      Exit(True)
    else
      FConnection.Connected :=  False;
  end;
  try
    FConnection.Protocol  :=  GetProvider;
    FConnection.Database  :=  ConnectionString;
    FOldConnectionString  :=  ConnectionString;
    FConnection.Connected :=  True;
    if GetCurrentThreadId <> CFGlobal.MainThreadID then
      _FreeIMEWindow;
  except on E: Exception do
    FLastErrorMessage :=  e.Message;
  end;
  Result  :=  FConnection.Connected;
end;

procedure TCFZEOSDBStorage.CloseConnection;
begin
  FConnection.Connected :=  False;
end;

function TCFZEOSDBStorage.GetConnected: boolean;
begin
  Result  :=  FConnection.Connected;
end;

function TCFZEOSDBStorage.SupportTrans: boolean;
begin
  Result  :=  True;
end;

procedure TCFZEOSDBStorage.BeginTrans;
begin
  if Connected then
    FConnection.StartTransaction;
end;

procedure TCFZEOSDBStorage.CommitTrans;
begin
  if Connected then
    FConnection.Commit;
end;

function TCFZEOSDBStorage.RefreshCache: Boolean;
begin
  Result  :=  False;
  {if FConnection.Connected and (Pos('MICROSOFT.JET.OLEDB.', UpperCase(ConnectionString)) > 0) then
    Result  :=  Database.Access.RefreshCache(FConnection.ConnectionObject);//}
end;

procedure TCFZEOSDBStorage.RollbackTrans;
begin
  if Connected then
    FConnection.Rollback;
end;

function TCFZEOSDBStorage.TableExists(const ATableName: string): Boolean;
var
  lst : TStrings;
begin
  DoWorking;
  Result  :=  False;
  if not Connected then Exit;
  try
    lst :=  TStringList.Create;
    try
      FConnection.GetTableNames('', lst);
      Result  :=  lst.IndexOf(ATableName) >= 0;
    finally
      lst.Free;
    end;
  except on E: Exception do
    FLastErrorMessage :=  e.Message;
  end;
end;

function TCFZEOSDBStorage.ExecSQL(const ASQL: string): boolean;
begin
  DoWorking;
  Result  :=  False;
  if not Connected then Exit;
  try
    FQuery.SQL.Text  :=  ASQL;
    FQuery.ExecSQL;
    Result  :=  True;
  except on E: Exception do
    FLastErrorMessage :=  e.Message;
  end;
end;

function TCFZEOSDBStorage.Query(const ASQL: string; const AKeyField: string): TClientDataSet;
begin
  DoWorking;
  Result  :=  nil;
  if not Connected then Exit;
  try
    FDataSet.Active  :=  False;
    FDataSet.SQL.Text :=  ASQL;
    FDataSet.Active  :=  True;
    if AKeyField <> '' then
      if Assigned(FDataSet.FindField(AKeyField)) then
        FDataSet.FieldByName(AKeyField).ProviderFlags :=
            FDataSet.FieldByName(AKeyField).ProviderFlags + [pfInKey];

    Result :=  TClientDataSet.Create(nil);
    Result.SetProvider(FProvider);
    Result.Active :=  True;
    Result.CommandText  :=  ASQL;
  except on E: Exception do
    begin
      FLastErrorMessage :=  e.Message;
      if Assigned(Result) then
        FreeAndNil(Result);
    end;
  end;
end;

function TCFZEOSDBStorage.Update(const ADataSet: TClientDataSet): boolean;
var
  i : Integer;//}
begin
  DoWorking;
  Result  :=  False;
  if not Connected then Exit;
  try
    FDataSet.Active  :=  False;
    FDataSet.SQL.Text :=  ADataSet.CommandText;

    FProvider.UpdateMode  :=  upWhereChanged;
    for i := 0 to ADataSet.FieldCount - 1 do
      if pfInKey in ADataSet.Fields[i].ProviderFlags then
      begin
        FProvider.UpdateMode  :=  upWhereKeyOnly;
        Break;
      end;//}
    ADataSet.SetProvider(FProvider);
    Result  :=  ADataSet.ApplyUpdates(0) = 0;
  except on E: Exception do
    FLastErrorMessage :=  e.Message;
  end;
end;

procedure TCFZEOSDBStorage.OnProviderBeforeUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
  var Applied: Boolean);
var
  i : Integer;
begin
  if UpdateKind = ukInsert then
    for i := 0 to DeltaDS.FieldCount - 1 do
      if (pfInKey in DeltaDS.Fields[i].ProviderFlags)
          and (DeltaDS.Fields[i] is TNumericField)
          and (DeltaDS.Fields[i].AsInteger < 0) then
      begin
        DeltaDS.Fields[i].ProviderFlags :=  [];
        DeltaDS.Fields[i].Tag :=  TAG_KEYFIELD;
      end;
end;

procedure TCFZEOSDBStorage.OnProviderAfterUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
var
  i : Integer;
begin
  if UpdateKind = ukInsert then
    for i := 0 to DeltaDS.FieldCount - 1 do
      if (DeltaDS.Fields[i].Tag =  TAG_KEYFIELD)
          and (DeltaDS.Fields[i].ProviderFlags =  [])
          and (DeltaDS.Fields[i] is TNumericField)
          and (DeltaDS.Fields[i].AsInteger < 0) then
      begin
        DeltaDS.Fields[i].ProviderFlags :=  [pfInKey];
        DeltaDS.Fields[i].Tag :=  0;
      end;
end;

procedure TCFZEOSDBStorage.OnProviderUpdateError(Sender: TObject;
  DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
  var Response: TResolverResponse);
begin
  inherited;
  raise E;
end;

{ TCFADODBStorageBuilder }

constructor TZEOSDBStorageBuilder.Create;
begin
  inherited;
  Pool.SetClearPoolInterval(CFGlobal.Config.DataBase.ClearPool_Interval);
end;

function TZEOSDBStorageBuilder.BuiltClass: TClass;
begin
  Result  :=  TCFZEOSDBStorage;
end;

function TZEOSDBStorageBuilder.BuiltInterfaceGUID: TGUID;
begin
  Result  :=  IID_ICFDataBaseStorage;
end;

initialization
  Classes.RegisterClass(TZEOSDBStorageBuilder);

finalization
  Classes.UnRegisterClass(TZEOSDBStorageBuilder);

end.
