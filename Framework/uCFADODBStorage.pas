//Common Framework
//ADODB 存储类
{$I CommonFramework.inc}
unit uCFADODBStorage;

interface

uses
  SysUtils, Classes, Windows, DB, ADODB, DBClient, Provider, ActiveX, Messages,
  uCFIntfDef, uCFClasses, uCFDBStorage, uCFConsts;

type
  TCFADODBStorage = class(TCFDBStorage)
  private
    FConnection : TADOConnection;
    FQuery : TADOQuery;
    FDataSet : TADODataSet;
    FProvider : TDataSetProvider;
    FOldConnectionString : string;
  protected
    function  CreateDataSet : TADODataSet;
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
    function  BaseQuery(const ASQL : string) : TDataSet; override;
    procedure DoLoadConfig; override;
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

  TADODBStorageBuilder = class(TCFPoolInterfaceBuilder)
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

constructor TCFADODBStorage.Create;
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
  FConnection := TADOConnection.Create(nil);
  FQuery := TADOQuery.Create(nil);
  FDataSet  :=  CreateDataSet;
  FProvider := TDataSetProvider.Create(nil);

  FConnection.LoginPrompt :=  False;
  FQuery.Connection :=  FConnection;
  FQuery.CacheSize  :=  DEF_CacheSize;
  FQuery.MarshalOptions :=  moMarshalModifiedOnly;
  FProvider.DataSet :=  FDataSet;
  FProvider.OnUpdateError :=  OnProviderUpdateError;
  FProvider.BeforeUpdateRecord  :=  OnProviderBeforeUpdateRecord;
  FProvider.AfterUpdateRecord := OnProviderAfterUpdateRecord;
  FProvider.ResolveToDataSet  :=  True;
  //FProvider.UpdateMode  :=  upWhereKeyOnly;

  OnNotifyMessage(CF_MSG_Config_Changed, 0, 0);
end;

destructor TCFADODBStorage.Destroy;
begin
  CloseConnection;
  FConnection.Free;
  FQuery.Free;
  FDataSet.Free;
  FProvider.Free;
  inherited;
end;

procedure TCFADODBStorage.DoLoadConfig;
begin
  inherited;
  if Assigned(FConnection) then
    if (CFGlobal.Config.DataBase.Connection_Timeout > 0) then
      FConnection.ConnectionTimeout :=  CFGlobal.Config.DataBase.Connection_Timeout
    else
      FConnection.ConnectionTimeout :=  DB_Connection_Timeout;
  if Assigned(FQuery) then
    if (CFGlobal.Config.DataBase.Command_Timeout > 0) then
      FQuery.CommandTimeout :=  CFGlobal.Config.DataBase.Command_Timeout
    else
      FQuery.CommandTimeout :=  DB_Command_Timeout;
  if Assigned(FDataSet) then
    if (CFGlobal.Config.DataBase.Command_Timeout > 0) then
      FDataSet.CommandTimeout :=  CFGlobal.Config.DataBase.Command_Timeout
    else
      FDataSet.CommandTimeout :=  DB_Command_Timeout;
end;

function TCFADODBStorage.CreateDataSet: TADODataSet;
begin
  Result := TADODataSet.Create(nil);
  Result.Connection :=  FConnection;
  Result.CacheSize  :=  DEF_CacheSize;
  Result.MarshalOptions :=  moMarshalModifiedOnly;
  if CFGlobal.Config.DataBase.Command_Timeout > 0 then
    Result.CommandTimeout :=  CFGlobal.Config.DataBase.Command_Timeout
  else
    Result.CommandTimeout :=  DB_Command_Timeout;
end;

function TCFADODBStorage.OpenConnection: Boolean;

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
    if FOldConnectionString = ConnectionString then
      Exit(True)
    else
      FConnection.Connected :=  False;
  try
    FConnection.ConnectionString  :=  ConnectionString;
    FOldConnectionString  :=  ConnectionString;
    FConnection.Connected :=  True;
    if GetCurrentThreadId <> CFGlobal.MainThreadID then
      _FreeIMEWindow;
  except on E: Exception do
    FLastErrorMessage :=  e.Message;
  end;
  Result  :=  FConnection.Connected;
end;

procedure TCFADODBStorage.CloseConnection;
begin
  FConnection.Connected :=  False;
end;

function TCFADODBStorage.GetConnected: boolean;
begin
  Result  :=  FConnection.Connected;
end;

function TCFADODBStorage.SupportTrans: boolean;
begin
  Result  :=  True;
end;

procedure TCFADODBStorage.BeginTrans;
begin
  if Connected then
    FConnection.BeginTrans;
end;

procedure TCFADODBStorage.CommitTrans;
begin
  if Connected then
    FConnection.CommitTrans;
end;

function TCFADODBStorage.RefreshCache: Boolean;
begin
  Result  :=  False;
  if FConnection.Connected and (Pos('MICROSOFT.JET.OLEDB.', UpperCase(ConnectionString)) > 0) then
    Result  :=  Database.Access.RefreshCache(FConnection.ConnectionObject);
end;

procedure TCFADODBStorage.RollbackTrans;
begin
  if Connected then
    FConnection.RollbackTrans;
end;

function TCFADODBStorage.TableExists(const ATableName: string): Boolean;
var
  lst : TStrings;
begin
  DoWorking;
  Result  :=  False;
  if not Connected then Exit;
  try
    FConnection.Errors.Clear;
    lst :=  TStringList.Create;
    try
      FConnection.GetTableNames(lst, True);
      Result  :=  lst.IndexOf(ATableName) >= 0;
    finally
      lst.Free;
    end;
  except on E: Exception do
    FLastErrorMessage :=  e.Message;
  end;
end;

function TCFADODBStorage.ExecSQL(const ASQL: string): boolean;
begin
  DoWorking;
  Result  :=  False;
  if not Connected then Exit;
  try
    FConnection.Errors.Clear;
    FQuery.SQL.Text  :=  ASQL;
    FQuery.ExecSQL;
    Result  :=  FConnection.Errors.Count = 0;
    if not Result then
      FLastErrorMessage :=  FConnection.Errors.Item[FConnection.Errors.Count - 1].Description;
  except on E: Exception do
    FLastErrorMessage :=  e.Message + #13#10 + ASQL;
  end;
end;

function TCFADODBStorage.Query(const ASQL: string; const AKeyField: string): TClientDataSet;
begin
  DoWorking;
  Result  :=  nil;
  if not Connected then Exit;
  try
    FDataSet.Active  :=  False;
    FDataSet.CommandText :=  ASQL;
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

function TCFADODBStorage.BaseQuery(const ASQL: string): TDataSet;
begin
  DoWorking;
  Result  :=  nil;
  if not Connected then Exit;
  try
    Result  :=  CreateDataSet;
    TAdoDataSet(Result).CommandText :=  ASQL;
    FDataSet.Active  :=  True;
  except on E: Exception do
    begin
      FLastErrorMessage :=  e.Message;
      if Assigned(Result) then
        FreeAndNil(Result);
    end;
  end;
end;

function TCFADODBStorage.Update(const ADataSet: TClientDataSet): boolean;
var
  i : Integer;//}
begin
  DoWorking;
  Result  :=  False;
  if not Connected then Exit;
  try
    FDataSet.Active  :=  False;
    FDataSet.CommandText :=  ADataSet.CommandText;

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

procedure TCFADODBStorage.OnProviderBeforeUpdateRecord(Sender: TObject;
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

procedure TCFADODBStorage.OnProviderAfterUpdateRecord(Sender: TObject;
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

procedure TCFADODBStorage.OnProviderUpdateError(Sender: TObject;
  DataSet: TCustomClientDataSet; E: EUpdateError; UpdateKind: TUpdateKind;
  var Response: TResolverResponse);
begin
  inherited;
  raise E;
end;

{ TCFADODBStorageBuilder }

constructor TADODBStorageBuilder.Create;
begin
  inherited;
  Pool.SetClearPoolInterval(CFGlobal.Config.DataBase.ClearPool_Interval);
end;

function TADODBStorageBuilder.BuiltClass: TClass;
begin
  Result  :=  TCFADODBStorage;
end;

function TADODBStorageBuilder.BuiltInterfaceGUID: TGUID;
begin
  Result  :=  IID_ICFDataBaseStorage;
end;

initialization
  Classes.RegisterClass(TADODBStorageBuilder);

finalization
  Classes.UnRegisterClass(TADODBStorageBuilder);

end.
