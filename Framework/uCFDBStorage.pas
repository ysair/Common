//Common Framework
//DB存储类
{$I CommonFramework.inc}
unit uCFDBStorage;

interface

uses
  SysUtils, Classes, DB, DBClient, ExtCtrls, Windows,
  uCFIntfDef, uCFClasses;

type
  TCFDBStorage = class(TCFPersistentObject, ICFDataBaseStorage, ICFBuiltInterface, ICFAuotRecycle)
  private
    FIdleTimer : TTimer;
  protected
    FProvider : string;
    FLastErrorMessage : string;
    FConnectionString : string;
    FAutoRecycle : Boolean;
    FCanAutoRecycle : Boolean;
    FAutoRecycle_Interval : Integer;
    FDisconnection_Interval : Integer;
    FLastWorkTime : DWORD;
    FWorking : Boolean;
  protected
    //ICFDataBaseStorage
    function  GetLastErrorMessage : string; virtual;
    function  GetProvider : string;                                   //获取Provider
    procedure SetProvider(const AProvider : string);                  //设置Provider
    procedure SetConnectionString(const AConnectStr : string); virtual;
    function  GetConnectionString : string; virtual;
    function  OpenConnection : Boolean; virtual;
    procedure CloseConnection; virtual;
    function  GetConnected : boolean; virtual;
    function  SupportTrans : boolean; virtual;
    procedure BeginTrans; virtual;
    procedure CommitTrans; virtual;
    procedure RollbackTrans; virtual;
    function  TableExists(const ATableName : string):Boolean; virtual;
    function  ExecSQL(const ASQL : string):boolean; virtual;
    function  Query(const ASQL : string; const AKeyField : string) : TClientDataSet; virtual;
    function  Update(const ADataSet : TClientDataSet):boolean; virtual;
    function  RefreshCache : Boolean; virtual;
    function  BaseQuery(const ASQL : string) : TDataSet; virtual;
    procedure DoLoadConfig; virtual;
    //ICFAuotRecycle
    function  GetAutoRecycle : boolean; virtual;
    procedure SetAutoRecycle(const AValue : Boolean); virtual;
    function  GetCanAutoRecycle : Boolean; virtual;
    //ICFInterface
    procedure OnNotifyMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);override;
    //==========================================================================
    procedure DoWorking;
    procedure OnIdleTimer(Sender : TObject); virtual;
  public
    property Builder;
    property Connected : Boolean read GetConnected;
    property Provider : string read GetProvider write SetProvider;
    property ConnectionString : string read GetConnectionString write SetConnectionString;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  uCFGlobal, uCFConsts;

{ TCFDBStorage }

constructor TCFDBStorage.Create;
begin
  inherited Create;
  FCanAutoRecycle :=  True;
  FAutoRecycle  :=  True;
  CFGlobal.Manager.RegisterInterface(Self);
  FIdleTimer  :=  TTimer.Create(nil);
  FIdleTimer.OnTimer  :=  OnIdleTimer;
  FIdleTimer.Interval :=  1000;
  FIdleTimer.Enabled  :=  True;
  OnNotifyMessage(CF_MSG_Config_Changed, 0, 0);
  DoWorking;
end;

destructor TCFDBStorage.Destroy;
begin
  if Assigned(CFGlobal) and Assigned(CFGlobal.Manager) then
    CFGlobal.Manager.UnregisterInterface(Self);
  FIdleTimer.Free;
  inherited;
end;

procedure TCFDBStorage.DoLoadConfig;
var
  pool  : ICFInterfacePool;
begin
  if Assigned(Builder) then
  begin
    pool := Builder.GetPool;
    if Assigned(pool) then
      pool.SetClearPoolInterval(CFGlobal.Config.DataBase.ClearPool_Interval);
  end;
  FAutoRecycle_Interval   :=  CFGlobal.Config.DataBase.AutoRecycle_Interval;
  FDisconnection_Interval :=  CFGlobal.Config.DataBase.Disconnection_Interval;
  ConnectionString  :=  CFGlobal.Config.DataBase.ConnectionString;
  Provider  :=  CFGlobal.Config.DataBase.Provider;
end;

procedure TCFDBStorage.DoWorking;
begin
  FWorking  :=  True;
  FLastWorkTime :=  GetTickCount;
end;

function TCFDBStorage.RefreshCache: Boolean;
begin
  Result  :=  False;
end;

function TCFDBStorage.GetAutoRecycle: boolean;
begin
  Result  :=  FAutoRecycle;
end;

procedure TCFDBStorage.SetAutoRecycle(const AValue: Boolean);
begin
  FAutoRecycle  :=  AValue;
end;

function TCFDBStorage.GetCanAutoRecycle: Boolean;
begin
  Result  :=  FCanAutoRecycle;
end;

procedure TCFDBStorage.OnNotifyMessage(Msg: UINT; wParam: WPARAM;
  lParam: LPARAM);
begin
  case Msg of
    CF_MSG_Config_Changed :
      DoLoadConfig;
    CF_MSG_DB_RefreshCache  :
      if Connected then
        RefreshCache;
  end;
end;

function TCFDBStorage.GetLastErrorMessage: string;
begin
  Result  :=  FLastErrorMessage;
end;

function TCFDBStorage.GetProvider: string;
begin
  Result  :=  FProvider;
end;

procedure TCFDBStorage.OnIdleTimer(Sender: TObject);
var
  idletime : DWORD;
begin
  if FLastWorkTime = 0 then
    Exit;
  idletime  :=  GetTickCount - FLastWorkTime;
  if (FDisconnection_Interval > 0) and (Integer(idletime) > FDisconnection_Interval) then
    CloseConnection;
  if FWorking and (FAutoRecycle_Interval > 0) and (Integer(idletime) > FAutoRecycle_Interval) then
  begin
    FWorking  :=  False;
    ReleaseInstance;
  end;
end;

procedure TCFDBStorage.SetConnectionString(const AConnectStr: string);
begin
  FConnectionString :=  AConnectStr;
end;

procedure TCFDBStorage.SetProvider(const AProvider: string);
begin
  FProvider :=  AProvider;
end;

function TCFDBStorage.GetConnectionString: string;
begin
  Result  :=  FConnectionString;
end;

function TCFDBStorage.OpenConnection: Boolean;
begin
  Result  :=  False;
end;

procedure TCFDBStorage.CloseConnection;
begin

end;

function TCFDBStorage.GetConnected: boolean;
begin
  Result  :=  False;
end;

function TCFDBStorage.SupportTrans: boolean;
begin
  Result  :=  False;
end;

function TCFDBStorage.BaseQuery(const ASQL: string): TDataSet;
begin
  Result  :=  nil;
end;

procedure TCFDBStorage.BeginTrans;
begin

end;

procedure TCFDBStorage.CommitTrans;
begin

end;

procedure TCFDBStorage.RollbackTrans;
begin

end;

function TCFDBStorage.TableExists(const ATableName: string): Boolean;
begin
  Result  :=  False;
end;

function TCFDBStorage.ExecSQL(const ASQL: string): boolean;
begin
  Result  :=  False;
end;

function TCFDBStorage.Query(const ASQL : string; const AKeyField : string) : TClientDataSet;
begin
  Result  :=  nil;
end;

function TCFDBStorage.Update(const ADataSet: TClientDataSet): boolean;
begin
  Result  :=  False;
end;

end.
