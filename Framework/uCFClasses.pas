// Common Framework
// 类定义单元
{$I CommonFramework.inc}
unit uCFClasses;

interface

uses
  SysUtils, Classes, Windows, Generics.Collections, ExtCtrls, Variants
  {$IFDEF FRAMEWORK_INCLUDE_DB} ,DB {$ENDIF}
  {$IFDEF FRAMEWORK_INCLUDE_XML} ,XMLDoc, XMLIntf  {$ENDIF}
  ,uCFConsts, uCFIntfDef, uCFTypeDef, Messages, typinfo;

type
  TCFObjectClass = class of TCFObject;
  TCFPersistentObjectClass = class of TCFPersistentObject;

  // CF框架基类
  // 注意,如果即要用对象方式调用又要用接口方式调用,Create后马上调用一次_AddRef
  // (TCFObject as IInterface)._AddRef;
  TCFObject = class(TInterfacedObject, ICFInterface, ICFBuiltInterface)
  private
    FBuider: ICFInterfaceBuilder;
    FTag: Integer;
  protected
    // ICFInterface
    function  ImplementObject: TObject;
    procedure ReleaseInstance; virtual;
    procedure OnNotifyMessage(Msg: UINT; wParam: wParam; lParam: lParam);  virtual;
    // ICFBuiltInterface
    function  GetBuilder: ICFInterfaceBuilder; virtual;
    procedure SetBuilder(const AIntf: ICFInterfaceBuilder); virtual;
    // TCFObject
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  protected
    property Builder: ICFInterfaceBuilder read GetBuilder;
  public
    property Tag : Integer read FTag write FTag;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BeforeDestruction; override;
  end;

  // CF框架Persistent基类
  TCFPersistentObject = class(TPersistent, IInterface, ICFInterface, ICFBuiltInterface)
  private
    FBuider: ICFInterfaceBuilder;
    FTag: Integer;
  protected
    FRefCount: Integer;
    FDestroying: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // ICFInterface
    function ImplementObject: TObject; virtual;
    procedure ReleaseInstance; virtual;
    procedure OnNotifyMessage(Msg: UINT; wParam: wParam; lParam: lParam); virtual;
    // ICFBuiltInterface
    function GetBuilder: ICFInterfaceBuilder; virtual;
    procedure SetBuilder(const AIntf: ICFInterfaceBuilder); virtual;
    // TCFPersistentObject
    procedure Initializa; virtual;
    procedure Finaliz; virtual;
  protected
    property Builder: ICFInterfaceBuilder read GetBuilder;
  public
    property Tag : Integer read FTag write FTag;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  //能接收消息的类
  TCFHandledObject = class(TCFObject, ICFMessageHandled)
  private
    FHandle: HWND;
    FOnMessage: TWndMethod;
  protected
    function  GetHandle: HWND;
    function  GetMessageEvent: TWndMethod;
    procedure SetMessageEvent(const Value: TWndMethod);
    procedure WndProc(var Message: TMessage); virtual;
  public
    property Handle : HWND read GetHandle;
    property OnMessage : TWndMethod read GetMessageEvent write SetMessageEvent;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TCFCritical = class sealed(TCFObject)
  protected
    FSection: TRTLCriticalSection;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;
  end;

  // 适配器基类
  TCFAdapter = class(TCFObject, ICFAdapter)
  private
    FCaller: TObject;
  protected
    // ICFAdapter
    function GetCaller: TObject; virtual;
    procedure SetCaller(const AObj: TObject); virtual;
    function GetAdapteeClass: TClass; virtual; abstract;
  public
    property Caller: TObject read GetCaller;
    constructor Create(ACaller: TObject); reintroduce; virtual;
  end;

  // 池类
  TCFVirtualPools = class abstract(TCFObject)
  protected
    FClearInterval: Integer;
    FClearType: TPoolsClearType;
    FClearTimer: TTimer;
    FList: TList;
    FCritical: TCFCritical;
    FCapacity: Integer;
    FLastWorkTime: DWORD;
    FUnique: Boolean;
    FOwnsObjects: Boolean;
  protected
    function  GetCount: Integer; virtual;
    function  GetIdleTime: Integer;
    procedure DoWorking;
    procedure OnClearTimer(Sender: TObject); virtual;
    procedure SetClearInterval(const Value: Integer);

    procedure DoPush(Item: Pointer); virtual;
    function  DoPop: Pointer; virtual;
    procedure FreeItem(var Item: Pointer); virtual;
    procedure Clear; virtual;
  public
    property Count: Integer read GetCount;
    property IdleTime: Integer read GetIdleTime;
    property ClearType: TPoolsClearType read FClearType write FClearType;
    property ClearInterval: Integer read FClearInterval write SetClearInterval;
    property Unique: Boolean read FUnique;
    property OwnsObjects: Boolean read FOwnsObjects;
  public
    constructor Create(
        const AOwnsObjects: Boolean = True;
        const Capacity: Integer = 0;
        const AUnique: Boolean = False
        );reintroduce; virtual;
    destructor Destroy; override;
  end;

  // 对象池
  TCFObjectPools = class(TCFVirtualPools, ICFObjectPool)
  protected
    procedure FreeItem(var Item: Pointer); override;
    function  GetClearPoolInterval : Integer;
    procedure SetClearPoolInterval(const AValue : Integer);
    function  GetClearType: TPoolsClearType;
    procedure SetClearType(const Value: TPoolsClearType);
  public
    procedure Push(Obj: TObject);
    function  Pop: TObject;
  end;

  // 接口池
  TCFInterfacePools = class(TCFVirtualPools, ICFInterfacePool)
  protected
    FIntfList : TList<IInterface>;
  protected
    procedure FreeItem(var Item: IInterface); reintroduce;
    function  GetCount: Integer; override;
    function  GetClearPoolInterval : Integer;
    procedure SetClearPoolInterval(const AValue : Integer);
    function  GetClearType: TPoolsClearType;
    procedure SetClearType(const Value: TPoolsClearType);
    procedure Clear; override;
    procedure OnClearTimer(Sender: TObject); override;
  public
    constructor Create(
        const AOwnsObjects: Boolean = True;
        const Capacity: Integer = 0;
        const AUnique: Boolean = False
        );override;
    destructor Destroy; override;

    procedure Push(Intf: IInterface);
    function  Pop: IInterface;
  end;

  // 支持池的构造器基类
  TCFPoolInterfaceBuilder = class(TCFPersistentObject, ICFInterfaceBuilder)
  private
    FPool: ICFInterfacePool;
  protected
    property Pool: ICFInterfacePool read FPool;
  protected
    function  BuiltClass: TClass; virtual; abstract;
    // ICFInterfaceBuilder
    function  SupportPool : Boolean; virtual;
    function  BuiltInterfaceGUID: TGUID; virtual; abstract;
    function  BuildInterface: IInterface; virtual;
    function  GetPool : ICFInterfacePool; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  // 接口工厂类
  TCFInterfaceFactory = class(TCFObject, ICFInterfaceFactory)
  private
    FBuilderList: TList<ICFInterfaceBuilder>;
  protected
    procedure RegisterBuilder(const ABuilder: ICFInterfaceBuilder); virtual;
    procedure UnregisterBuilder(const ABuilder: ICFInterfaceBuilder); virtual;
    procedure ClearBuilders(const AFree : Boolean); virtual;
    function  CreateInterface(const AIntfGUID: TGUID): ICFInterface; virtual;
    procedure FreeInterface(const AIntf: ICFInterface); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  // 配置对象基类
  TCFConfig = class(TCFPersistentObject, ICFPropertiesXMLReader, ICFPropertiesXMLWriter)
  private
    FXMLReader: ICFPropertiesXMLReader;
    FXMLWriter: ICFPropertiesXMLWriter;
  protected
    property XMLReader: ICFPropertiesXMLReader read FXMLReader implements ICFPropertiesXMLReader;
    property XMLWriter: ICFPropertiesXMLWriter read FXMLWriter implements ICFPropertiesXMLWriter;
  public
    constructor Create; override;
  end;

  //线程类
  TCFThread = class(TThread)
  private
    FOnExecute: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure ProcessMessages; virtual;
  public
    property OnExecute : TNotifyEvent read FOnExecute write FOnExecute;
  end;

  // 命令对象
  TCFCommand = class(TCFObject, ICFCommand)
  protected
    FResultCode: Integer;
    FErrorMessage: string;
    FParameter: TVariantArray;
    FTerminated : Boolean;
    FEventTime : DWORD;
  protected
    function  CanExecute: Boolean; virtual;
    procedure Execute; virtual;
    function  GetResultCode: Integer;
    function  GetErrorMessage: string;
    function  GetID: Integer; virtual;
    procedure SetParameters(const AParameter: array of const); virtual;
    function  GetParameterCount : Integer;
    procedure SetParameterCount(const ACount : Integer);
    procedure SetParameter(const AIndex: Integer; const Value: Variant); virtual;
    function  GetParameter(const AIndex: Integer): Variant; virtual;
    procedure Terminate; virtual;
  public
    property Params[const i: Integer]: Variant read GetParameter write SetParameter;
    property ParamCount : Integer read GetParameterCount write SetParameterCount;
    property Terminated : Boolean read FTerminated;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const AParameters: array of const); reintroduce; overload;
    destructor Destroy; override;
  end;

  // 线程执行命令对象
  TCFCommandExecutor = class(TCFHandledObject, ICFCommandExecutor)
  protected
    FIsRunning: Boolean;
    FCommand: ICFCommand;
    FCommandExecuted : Boolean;
    FThread : TCFThread;
    FFreeOnExecuted: Boolean;
    FBeforeExecuteCommand : TInterfaceNotifyEvent;
    FAfterExecuteCommand : TInterfaceNotifyEvent;
  protected
    function  GetCommand: ICFCommand;
    procedure SetCommand(const Value: ICFCommand); virtual;
    function  GetBeforeExecuteCommand : TInterfaceNotifyEvent;
    procedure SetBeforeExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetAfterExecuteCommand : TInterfaceNotifyEvent;
    procedure SetAfterExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetIsRunning: Boolean; virtual;
    function  GetCanExecuteCommand: Boolean; virtual;
    function  CommandCanExecute: Boolean; virtual;
    procedure OnThreadExecute(Sender : TObject);
    procedure DoBeforeExecuteCommand;
    procedure DoAfterExecuteCommand;
    procedure WndProc(var Message: TMessage); override;
  public
    property Command: ICFCommand read GetCommand write SetCommand;
    property CanExecuteCommand: Boolean read GetCanExecuteCommand;
    property BeforeExecuteCommand : TInterfaceNotifyEvent read GetBeforeExecuteCommand write SetBeforeExecuteCommand;
    property AfterExecuteCommand : TInterfaceNotifyEvent read GetAfterExecuteCommand write SetAfterExecuteCommand;
    property IsRunning : Boolean read GetIsRunning;
    property FreeOnExecuted : Boolean read FFreeOnExecuted write FFreeOnExecuted;
  public
    constructor Create; reintroduce; overload;
    constructor Create(
        const ACommand : ICFCommand;
        const AAfterExecuteCommandEvent : TInterfaceNotifyEvent;
        const AFreeOnExecuted : Boolean = False
        ); reintroduce; overload;
    destructor Destroy; override;

    function  ExecuteCommand(const ACommand: ICFCommand): Boolean; virtual;
    function  Start : Boolean; virtual;
    procedure Stop; virtual;
  end;

  // 线程执行命令列表对象
  TCFCommandListExecutor = class(TCFHandledObject, ICFCommandListExecutor)
  protected
    FIsRunning: Boolean;
    FOnExecuted: TNotifyEvent;
    FCommandExecuted : Boolean;
    FThread : TCFThread;
    FFreeOnExecuted: Boolean;
    FCommandList : TList<ICFCommand>;
    FBeforeExecuteCommand : TInterfaceNotifyEvent;
    FAfterExecuteCommand : TInterfaceNotifyEvent;
    FCritical : TCFCritical;
  protected
    function  GetCurrCommand: ICFCommand;
    function  GetOnExecuted: TNotifyEvent;
    procedure SetOnExecuted(const Value: TNotifyEvent);
    function  GetAfterExecuteCommand: TInterfaceNotifyEvent;
    function  GetBeforeExecuteCommand: TInterfaceNotifyEvent;
    procedure SetAfterExecuteCommand(const Value: TInterfaceNotifyEvent);
    procedure SetBeforeExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetIsRunning: Boolean;
    procedure OnThreadExecute(Sender : TObject);
    procedure DoExecuted;
    procedure DoBeforeExecuteCommand;
    procedure DoAfterExecuteCommand;
    procedure WndProc(var Message: TMessage); override;
  public
    property CurrCommand: ICFCommand read GetCurrCommand;
    property OnExecuted : TNotifyEvent read GetOnExecuted write SetOnExecuted;
    property BeforeExecuteCommand : TInterfaceNotifyEvent read GetBeforeExecuteCommand write SetBeforeExecuteCommand;
    property AfterExecuteCommand : TInterfaceNotifyEvent read GetAfterExecuteCommand write SetAfterExecuteCommand;
    property IsRunning : Boolean read GetIsRunning;
    property FreeOnExecuted : Boolean read FFreeOnExecuted write FFreeOnExecuted;
  public
    constructor Create; reintroduce; overload;
    constructor Create(
        const AExecutedEvent : TNotifyEvent;
        const AFreeOnExecuted : Boolean = False
        ); reintroduce; overload;
    destructor Destroy; override;

    function  Start : Boolean;
    procedure Stop;
    procedure AddCommand(const ACommand: ICFCommand);
    function  CommandExists(const ACommandID : Integer):Boolean;
    function GetCommand(const ACommandID: Integer): ICFCommand;
    procedure Clear;
  end;

  // 对象列表类,支持从DataSet生成列表
  TCFObjectList<T : class> = class abstract(TObjectList<T>, ICFInterface{$IFDEF FRAMEWORK_INCLUDE_DB}, ICFPropertiesDataSetReader{$ENDIF}{$IFDEF FRAMEWORK_INCLUDE_XML}, ICFPropertiesXMLReader{$ENDIF})
  protected
    FRefCount: Integer;
    FDestroying: Boolean;
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // ICFInterface
    function ImplementObject: TObject; virtual;
    procedure ReleaseInstance; virtual;
    procedure OnNotifyMessage(Msg: UINT; wParam: wParam; lParam: lParam); virtual;
    {$IFDEF FRAMEWORK_INCLUDE_DB}
    //ICFPropertiesDataSetReader
    function PropertiesReadFromDataSet(const ADataSet : TDataSet) : Boolean;
    {$ENDIF}
    {$IFDEF FRAMEWORK_INCLUDE_XML}
    //ICFPropertiesXMLReader
    function PropertiesReadFromXMLNode(const AIntf : IInterface; const ANodePath : string) : Boolean;
    {$ENDIF}
  public
    constructor Create; reintroduce; virtual;
    function NewItem : T; virtual; abstract;
  end;

  //日志类
  TCFLog = class abstract(TCFObject, ICFLog)
  private
    FOnLog: TNotifyMessageEvent;
  protected
    function GetOnLog: TNotifyMessageEvent;
    procedure SetOnLog(AEvent: TNotifyMessageEvent);
  public
    property OnLog : TNotifyMessageEvent read GetOnLog write SetOnLog;

    procedure WriteLog(const ALog : string); overload; virtual; abstract;
    procedure WriteLog(const AType : TCFLogType; const ALog : string); overload; virtual; abstract;
  end;

  TCFTextFileLog = class(TCFLog, ICFLog)
  private
    FFileName: string;
    FCacheList : TStrings;
    FLastWriteTick : DWORD;
    FLock: TRTLCriticalSection;
  private
    procedure SetFileName(const Value: string);
    procedure Open;
    procedure Close;
    procedure WriteCacheList;
    procedure LockList;
    procedure UnlockList;
  public
    property FileName : string read FFileName write SetFileName;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteLog(const ALog : string); overload; override;
    procedure WriteLog(const AType : TCFLogType; const ALog : string); overload; override;
  end;

  //多线程字符串仓库
  TCFStringRepository = class(TCFObject, ICFStringRepository)
  protected
    FCurrID : Integer;
    FCritical : TCFCritical;
    FStringDict : TDictionary<Integer, string>;
  protected
    procedure Clear;
    function  Add(const AStr : string):Integer;
    function  Get(const AID : Integer):string;
    function  Extract(const AID : Integer):string;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  //线程列表
  TCFThreadList = class(TCFObject)
  private
    FList: TList;
    FLock: TRTLCriticalSection;
    FDuplicates: TDuplicates;
    function GetCount:integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
    function Items(idx: Integer): Pointer;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Count:integer read GetCount;
  end;

implementation

uses
  Consts, uCFXML, uCFResource, fcConvert, fcPlus_VCL, fcWindows, fcRtti, uCFGlobal;

{ TDNObject }

constructor TCFObject.Create;
begin
  inherited;
end;

destructor TCFObject.Destroy;
begin
  inherited;
end;

procedure TCFObject.BeforeDestruction;
begin
  FRefCount := 0;
  inherited;
end;

procedure TCFObject.Initialize;
begin

end;

procedure TCFObject.Finalize;
begin

end;

function TCFObject.ImplementObject: TObject;
begin
  Result := Self;
end;

procedure TCFObject.ReleaseInstance;
var
  ib : ICFBuiltInterface;
  builder : ICFInterfaceBuilder;
  Pool: ICFInterfacePool;
begin
  if QueryInterface(IID_ICFBuiltInterface, ib) = S_OK then
  begin
    builder   :=  ib.GetBuilder;
    if Assigned(builder)  then
    begin
      Pool  :=  builder.GetPool;
      if Assigned(pool) then
      begin
        Pool.Push(Self);
        Exit;
      end;
    end;
  end;

  if FRefCount > 0 then
    _Release
  else
    Free;
end;

procedure TCFObject.OnNotifyMessage(Msg: UINT; wParam: wParam; lParam: lParam);
begin

end;

function TCFObject.GetBuilder: ICFInterfaceBuilder;
begin
  Result := FBuider;
end;

procedure TCFObject.SetBuilder(const AIntf: ICFInterfaceBuilder);
begin
  FBuider := AIntf;
end;

{ TCFPersistentObject }

procedure TCFPersistentObject.AssignTo(Dest: TPersistent);
var
  plist : PPropList;
  plen : Integer;
  i : Integer;
  v:Variant;
begin
  if not Dest.ClassNameIs(ClassName) then
    Exit;

  plen  :=  GetPropList(Dest, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].SetProc) then
        Continue;

      try
        v := GetPropValue(Self, string(plist[i].Name));
        SetPropValue(Dest, plist[i], v);
      except
      end;
    end;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

constructor TCFPersistentObject.Create;
begin
  inherited;
end;

destructor TCFPersistentObject.Destroy;
begin
  FDestroying := True;
  inherited;
end;

function TCFPersistentObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCFPersistentObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TCFPersistentObject._Release: Integer;
begin
  if FRefCount > 0 then
    Result := InterlockedDecrement(FRefCount)
  else
    Result := 0;
  if (Result = 0) and not FDestroying then
  begin
    FDestroying := True;
    Destroy;
  end;
end;

procedure TCFPersistentObject.ReleaseInstance;
var
  ib : ICFBuiltInterface;
  builder : ICFInterfaceBuilder;
  Pool: ICFInterfacePool;
begin
  if QueryInterface(IID_ICFBuiltInterface, ib) = S_OK then
  begin
    builder   :=  ib.GetBuilder;
    if Assigned(builder)  then
    begin
      Pool  :=  builder.GetPool;
      if Assigned(pool) then
      begin
        Pool.Push(Self);
        Exit;
      end;
    end;
  end;
  _Release;
end;

procedure TCFPersistentObject.Initializa;
begin

end;

procedure TCFPersistentObject.Finaliz;
begin

end;

function TCFPersistentObject.ImplementObject: TObject;
begin
  Result := Self;
end;

procedure TCFPersistentObject.OnNotifyMessage(Msg: UINT; wParam: wParam;
  lParam: lParam);
begin

end;

function TCFPersistentObject.GetBuilder: ICFInterfaceBuilder;
begin
  Result := FBuider;
end;

procedure TCFPersistentObject.SetBuilder(const AIntf: ICFInterfaceBuilder);
begin
  FBuider := AIntf;
end;

{ TCFHandledObject }

constructor TCFHandledObject.Create;
begin
  inherited;
  FHandle := AllocateHWnd(WndProc);
end;

destructor TCFHandledObject.Destroy;
begin
  DeallocateHWnd(FHandle);
  inherited;
end;

function TCFHandledObject.GetHandle: HWND;
begin
  Result  :=  FHandle;
end;

function TCFHandledObject.GetMessageEvent: TWndMethod;
begin
  Result  :=  FOnMessage;
end;

procedure TCFHandledObject.SetMessageEvent(const Value: TWndMethod);
begin
  FOnMessage  :=  Value;
end;

procedure TCFHandledObject.WndProc(var Message: TMessage);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Message)
  else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

{ TCFProxy }
{
  constructor TCFProxy.Create(AOwner: TObject);
  begin
  inherited Create;
  FOwner := AOwner;
  end;// }

{ TCFAdapter }

constructor TCFAdapter.Create(ACaller: TObject);
begin
  inherited Create;
  SetCaller(ACaller);
end;

function TCFAdapter.GetCaller: TObject;
begin
  Result := FCaller;
end;

procedure TCFAdapter.SetCaller(const AObj: TObject);
begin
  FCaller := AObj;
end;

{ TCFPoolObjectBuilder }

constructor TCFPoolInterfaceBuilder.Create;
begin
  inherited;
  FPool := TCFInterfacePools.Create(True, 0, True);
  FPool.SetClearType(pctAll);
end;

destructor TCFPoolInterfaceBuilder.Destroy;
begin
  inherited;
end;

function TCFPoolInterfaceBuilder.BuildInterface: IInterface;
var
  Obj: TObject;
  built: ICFBuiltInterface;
  cls: TClass;
  ib : ICFBuiltInterface;
  itmp : IInterface;
begin
  Result := FPool.Pop;
  if not Assigned(Result) then
  begin
    cls := BuiltClass;
    if cls.InheritsFrom(TCFObject) then
      Result := TCFObjectClass(cls).Create
    else if cls.InheritsFrom(TCFPersistentObject) then
      Result := TCFPersistentObjectClass(cls).Create
    else if cls.InheritsFrom(TComponent) then
      Result := TComponentClass(cls).Create(nil)
    else begin
      obj := cls.Create;
      if not obj.GetInterface(IID_IInterface, Result) then
        obj.Free;
    end;
    if Result.QueryInterface(IID_ICFBuiltInterface, ib) = S_OK then
      ib.SetBuilder(Self);

    if Result.QueryInterface(BuiltInterfaceGUID, itmp) = S_OK then
    begin
      itmp  :=  nil;
      if Result.QueryInterface(IID_ICFBuiltInterface, built) = S_OK then
        built.SetBuilder(Self);
    end
    else
      Result  :=  nil;
  end;
end;

function TCFPoolInterfaceBuilder.GetPool: ICFInterfacePool;
begin
  Result  :=  FPool;
end;

function TCFPoolInterfaceBuilder.SupportPool: Boolean;
begin
  Result  :=  True;
end;

{ TCFInterfaceFactory }

constructor TCFInterfaceFactory.Create;
begin
  inherited;
  FBuilderList := TList<ICFInterfaceBuilder>.Create;
end;

destructor TCFInterfaceFactory.Destroy;
begin
  ClearBuilders(True);
  FBuilderList.Free;
  inherited;
end;

procedure TCFInterfaceFactory.ClearBuilders(const AFree : Boolean);
var
  i : Integer;
  icf : ICFInterface;
begin
  for i := FBuilderList.Count - 1 downto 0 do
  begin
    if AFree then
      if FBuilderList[i].QueryInterface(IID_ICFInterface, icf) = S_OK then
      begin
        FBuilderList[i]._AddRef;
        icf.ImplementObject.Free;
      end
      else //}
        FBuilderList[i]._Release;
    //FBuilderList.Items[0] :=  nil;
    //FBuilderList.Delete(0);
  end;// }
  FBuilderList.Clear;
end;

procedure TCFInterfaceFactory.RegisterBuilder
  (const ABuilder: ICFInterfaceBuilder);
var
  i: Integer;
begin
  for i := FBuilderList.Count - 1 downto 0 do
    if IsEqualGUID(ABuilder.BuiltInterfaceGUID,
        FBuilderList[i].BuiltInterfaceGUID) then
      UnregisterBuilder(FBuilderList[i]);
  FBuilderList.Add(ABuilder);
end;

procedure TCFInterfaceFactory.UnregisterBuilder
  (const ABuilder: ICFInterfaceBuilder);
var
  i: Integer;
begin
  for i := FBuilderList.Count - 1 downto 0 do
    if ABuilder = FBuilderList[i] then
    begin
      FBuilderList[i]._Release;
      FBuilderList.Delete(i);
      Break;
    end;
end;

function TCFInterfaceFactory.CreateInterface(const AIntfGUID: TGUID)
  : ICFInterface;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FBuilderList.Count - 1 do
    if IsEqualGUID(AIntfGUID, FBuilderList[i].BuiltInterfaceGUID) then
    begin
      Result := FBuilderList[i].BuildInterface as ICFInterface;
      Break;
    end;
end;

procedure TCFInterfaceFactory.FreeInterface(const AIntf: ICFInterface);
{var
  Pool: ICFInterfacePool;
  ib : ICFBuiltInterface;
  iib : ICFInterfaceBuilder;//}
begin
  AIntf.ReleaseInstance;
  {if AIntf.QueryInterface(IID_ICFBuiltInterface, ib) = S_OK then
  begin
    iib   :=  ib.GetBuilder;
    if Assigned(iib)  then
    begin
      Pool  :=  iib.GetPool;
      if Assigned(pool) then
        Pool.Push(AIntf);
    end;
  end;//}
end;

{ TCFConfig }

constructor TCFConfig.Create;
begin
  inherited;
  FXMLReader := TCFXml2PropertiesAdapter.Create(Self);
  FXMLWriter := TCFProperties2XMLAdapter.Create(Self);
end;

{ TCFCritical }

constructor TCFCritical.Create;
begin
  inherited;
  InitializeCriticalSection(FSection);
end;

destructor TCFCritical.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited;
end;

procedure TCFCritical.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TCFCritical.Leave;
begin
  LeaveCriticalSection(FSection);
end;

{ TCFVirtualPools }

constructor TCFVirtualPools.Create(const AOwnsObjects: Boolean;
  const Capacity: Integer; const AUnique: Boolean);
begin
  inherited Create;
  FCritical := TCFCritical.Create;
  FList := TList.Create;
  FOwnsObjects := AOwnsObjects;
  FUnique := AUnique;
  FCapacity := Capacity;
  DoWorking;
  FClearTimer := TTimer.Create(nil);
  FClearTimer.Interval := 1000;
  FClearTimer.OnTimer := OnClearTimer;
  FClearTimer.Enabled := True; // }
end;

destructor TCFVirtualPools.Destroy;
begin
  FClearTimer.Free;
  Clear;
  FreeAndNil(FList);
  FCritical.Free;
  inherited;
end;

procedure TCFVirtualPools.Clear;
var
  Item: Pointer;
begin
  Item := DoPop;
  while Assigned(Item) do
  begin
    if FOwnsObjects then
      FreeItem(Item);
    Item := DoPop;
  end;
end;

function TCFVirtualPools.DoPop: Pointer;
begin
  Result := nil;
  FCritical.Enter;
  try
    if FList.Count > 0 then
    begin
      Result := FList.Items[0];
      FList.Delete(0);
    end;
    DoWorking;
  finally
    FCritical.Leave;
  end;
end;

procedure TCFVirtualPools.DoPush(Item: Pointer);
begin
  if not Assigned(Item) then
    Exit;
  FCritical.Enter;
  try
    if FUnique and (FList.IndexOf(Item) >= 0) then
      Exit;
    if (FList.Count >= FCapacity) and (FCapacity > 0) then
      Raise Exception.Create(SEPoolsFull)
    else
      FList.Add(Item);
    DoWorking;
  finally
    FCritical.Leave;
  end;
end;

procedure TCFVirtualPools.DoWorking;
begin
  FLastWorkTime := GetTickCount;
end;

procedure TCFVirtualPools.FreeItem(var Item: Pointer);
begin
end;

function TCFVirtualPools.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCFVirtualPools.GetIdleTime: Integer;
begin
  Result := GetTickCount - FLastWorkTime;
end;

procedure TCFVirtualPools.OnClearTimer(Sender: TObject);
var
  Item: Pointer;
begin
  if (FClearInterval <= 0) or (IdleTime <= FClearInterval) then
    Exit;

  case ClearType of
    pctOne:
      begin
        Item := DoPop;
        if Assigned(Item) and FOwnsObjects then
          FreeItem(Item);
      end;
    pctAll:
      Clear;
  end;
end;

procedure TCFVirtualPools.SetClearInterval(const Value: Integer);
begin
  if Value <> 0 then
    FClearInterval := Value;
end;

{ TCFObjectPools }

procedure TCFObjectPools.FreeItem(var Item: Pointer);
begin
  inherited;
  if Assigned(Item) then
  begin
    TObject(Item).Free;
    Item  :=  nil;
  end;
end;

function TCFObjectPools.Pop: TObject;
begin
  Result := DoPop;
end;

procedure TCFObjectPools.Push(Obj: TObject);
begin
  DoPush(Obj);
end;

function TCFObjectPools.GetClearPoolInterval: Integer;
begin
  Result  :=  ClearInterval;
end;

procedure TCFObjectPools.SetClearPoolInterval(const AValue: Integer);
begin
  ClearInterval :=  AValue;
end;

function TCFObjectPools.GetClearType: TPoolsClearType;
begin
  Result  :=  ClearType;
end;

procedure TCFObjectPools.SetClearType(const Value: TPoolsClearType);
begin
  ClearType :=  Value;
end;

{ TCFInterfacePools }

constructor TCFInterfacePools.Create(const AOwnsObjects: Boolean;
  const Capacity: Integer; const AUnique: Boolean);
begin
  inherited Create(AOwnsObjects, Capacity, AUnique);
  FIntfList :=  TList<IInterface>.Create;
end;

destructor TCFInterfacePools.Destroy;
begin
  Clear;
  FreeAndNil(FIntfList);
  inherited;
end;

procedure TCFInterfacePools.FreeItem(var Item: IInterface);
begin
  Item  :=  nil;
end;

function TCFInterfacePools.Pop: IInterface;
begin
  FCritical.Enter;
  try
    if FIntfList.Count > 0 then
    begin
      Result := FIntfList.Items[0];
      FIntfList.Delete(0);
      //CFGlobal.Log.WriteLog(ltMessage, IntToStr(Integer(Result)) + ' poped from pool.');
    end
    else
      Result := nil;
    DoWorking;
  finally
    FCritical.Leave;
  end;
end;

procedure TCFInterfacePools.Push(Intf: IInterface);
begin
  if not Assigned(Intf) then
    Exit;
  FCritical.Enter;
  try
    //CFGlobal.Log.WriteLog(ltMessage, IntToStr(Integer(intf)) + ' pushed in pool.');
    if FUnique and (FIntfList.IndexOf(Intf) >= 0) then
      Exit;
    if (FIntfList.Count >= FCapacity) and (FCapacity > 0) then
      Raise Exception.Create(SEPoolsFull)
    else
      FIntfList.Add(Intf);
    DoWorking;
  finally
    FCritical.Leave;
  end;
end;

function TCFInterfacePools.GetClearPoolInterval: Integer;
begin
  Result  :=  ClearInterval;
end;

procedure TCFInterfacePools.SetClearPoolInterval(const AValue: Integer);
begin
  ClearInterval :=  AValue;
end;

function TCFInterfacePools.GetClearType: TPoolsClearType;
begin
  Result  :=  ClearType;
end;

function TCFInterfacePools.GetCount: Integer;
begin
  Result  :=  FIntfList.Count;
end;

procedure TCFInterfacePools.OnClearTimer(Sender: TObject);
var
  Item: IInterface;
begin
  if (FClearInterval <= 0) or (IdleTime <= FClearInterval) then
    Exit;

  case ClearType of
    pctOne:
      begin
        Item := Pop;
        if Assigned(Item) and OwnsObjects then
          FreeItem(Item);
      end;
    pctAll:
      Clear;
  end;
end;

procedure TCFInterfacePools.Clear;
var
  Item: IInterface;
begin
  if not Assigned(FIntfList) then Exit;
  Item := Pop;
  while Assigned(Item) do
  begin
    if OwnsObjects then
      FreeItem(Item);
    Item := Pop;
  end;
end;

procedure TCFInterfacePools.SetClearType(const Value: TPoolsClearType);
begin
  ClearType :=  Value;
end;

{ TCFThread }

procedure TCFThread.Execute;
begin
  inherited;
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TCFThread.ProcessMessages;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

{ TCFCommand }

constructor TCFCommand.Create(const AParameters: array of const);
begin
  inherited Create;
  FEventTime  :=  Def_CommandEventTime;
  SetParameters(AParameters);
end;

constructor TCFCommand.Create;
begin
  inherited Create;
  FEventTime  :=  Def_CommandEventTime;
end;

destructor TCFCommand.Destroy;
begin
  SetLength(FParameter, 0);
  inherited;
end;

function TCFCommand.CanExecute: Boolean;
begin
  Result := True;
end;

procedure TCFCommand.Execute;
begin
  FResultCode  :=  CommandResultCode_Error;
end;

function TCFCommand.GetErrorMessage: string;
begin
  Result := FErrorMessage;
end;

function TCFCommand.GetID: Integer;
begin
  Result := 0;
end;

function TCFCommand.GetResultCode: Integer;
begin
  Result := FResultCode;
end;

procedure TCFCommand.SetParameter(const AIndex: Integer; const Value: Variant);
begin
  if (AIndex >= Low(FParameter)) and (AIndex <= High(FParameter)) then
    FParameter[AIndex]  :=  Value;
end;

function TCFCommand.GetParameterCount: Integer;
begin
  Result  :=  VarArrayHighBound(FParameter, 1) - VarArrayLowBound(FParameter, 1) + 1;
end;

procedure TCFCommand.SetParameterCount(const ACount: Integer);
begin
  SetLength(FParameter, ACount);
end;

procedure TCFCommand.SetParameters(const AParameter: array of const);
var
  i: Integer;
begin
  SetParameterCount(Length(AParameter));
  for i := Low(FParameter) to High(FParameter) do
    FParameter[i] := Convert.VarRecToVariant(AParameter[i]);
end;

function TCFCommand.GetParameter(const AIndex: Integer): Variant;
begin
  if (AIndex >= Low(FParameter)) and (AIndex <= High(FParameter)) then
    Result := FParameter[AIndex]
  else
    Result := Unassigned;
end;

procedure TCFCommand.Terminate;
begin
  FTerminated :=  True;
end;

{ TCFCommandExecutor }

function TCFCommandExecutor.CommandCanExecute: Boolean;
begin
  Result := Assigned(Command) and Command.CanExecute;
end;

constructor TCFCommandExecutor.Create;
begin
  inherited Create;
  FThread :=  TCFThread.Create(True);
  FThread.OnExecute :=  OnThreadExecute;
  FThread.Start;
end;

constructor TCFCommandExecutor.Create(const ACommand: ICFCommand;
  const AAfterExecuteCommandEvent: TInterfaceNotifyEvent;
  const AFreeOnExecuted : Boolean);
begin
  inherited Create;
  FreeOnExecuted  :=  AFreeOnExecuted;
  FThread :=  TCFThread.Create(True);
  FThread.OnExecute :=  OnThreadExecute;
  //FThread.FreeOnTerminate :=  True;
  SetCommand(ACommand);
  SetAfterExecuteCommand(AAfterExecuteCommandEvent);
  FThread.Start;
end;

destructor TCFCommandExecutor.Destroy;
begin
  Stop;
  SetCommand(nil);
  Start;
  FThread.Terminate;
  FThread.Free;
  inherited;
end;

function TCFCommandExecutor.ExecuteCommand(const ACommand: ICFCommand): Boolean;
begin
  Sleep(1);
  if CanExecuteCommand then
  begin
    Command := ACommand;
    Result  :=  Start;
  end
  else
    Result  :=  False;
end;

function TCFCommandExecutor.Start : Boolean;
begin
  Result := CanExecuteCommand;
  if Result then
  begin
    if FThread.Suspended then
      FThread.Suspended :=  False;
  end;
end;

procedure TCFCommandExecutor.Stop;
begin
  FIsRunning  :=  False;
  if Assigned(FCommand) then
  begin
    FCommand.Terminate;
  end;
  if not FThread.Terminated then
    FThread.Suspended :=  True;
end;

procedure TCFCommandExecutor.WndProc(var Message: TMessage);
begin
  case message.Msg of
    CF_MSG_Free : Free;
    else
      inherited;
  end;
end;

procedure TCFCommandExecutor.DoAfterExecuteCommand;
begin
  if Assigned(FAfterExecuteCommand) then
  try
    FAfterExecuteCommand(FCommand);
  except
  end;
  if FreeOnExecuted then
    PostMessage(Handle, CF_MSG_Free, 0, 0);
end;

procedure TCFCommandExecutor.DoBeforeExecuteCommand;
begin

end;

procedure TCFCommandExecutor.OnThreadExecute(Sender : TObject);
var
  ci: ICFInterface;
begin
  while not TCFThread(Sender).Terminated do
  begin
    if CommandCanExecute and not FCommandExecuted then
    begin
      try
        FIsRunning  :=  True;
        try
          FCommand.Execute;
        except
          on e: Exception do
            if Command.QueryInterface(IID_ICFInterface, ci) = S_OK then
              if ci.ImplementObject is TCFCommand then
                TCFCommand(ci.ImplementObject).FErrorMessage := e.Message;
        end;
      finally
        FIsRunning  :=  False;
        FCommandExecuted  :=  True;
      end;
      if not TCFThread(Sender).Terminated then
        TCFThread(Sender).Synchronize(DoAfterExecuteCommand);
    end;
    Sleep(1);
  end;
end;

function TCFCommandExecutor.GetBeforeExecuteCommand: TInterfaceNotifyEvent;
begin
  Result  :=  FBeforeExecuteCommand;
end;

procedure TCFCommandExecutor.SetBeforeExecuteCommand(
  const Value: TInterfaceNotifyEvent);
begin
  FBeforeExecuteCommand :=  Value;
end;

function TCFCommandExecutor.GetAfterExecuteCommand: TInterfaceNotifyEvent;
begin
  Result :=  FAfterExecuteCommand;
end;

procedure TCFCommandExecutor.SetAfterExecuteCommand(
  const Value: TInterfaceNotifyEvent);
begin
  FAfterExecuteCommand  :=  Value;
end;

function TCFCommandExecutor.GetCanExecuteCommand: Boolean;
begin
  Result := not IsRunning;
end;

function TCFCommandExecutor.GetCommand: ICFCommand;
begin
  Result  :=  FCommand;
end;

function TCFCommandExecutor.GetIsRunning: Boolean;
begin
  Result  :=  FIsRunning;
end;

procedure TCFCommandExecutor.SetCommand(const Value: ICFCommand);
begin
  if not IsRunning then
  begin
    FCommand := Value;
    FCommandExecuted  :=  False;
  end;
end;

{ TCFCommandListExecutor }

constructor TCFCommandListExecutor.Create;
begin
  inherited;
  FCritical :=  TCFCritical.Create;
  FCommandList  :=  TList<ICFCommand>.Create;
  FThread :=  TCFThread.Create(True);
  FThread.OnExecute :=  OnThreadExecute;
  FThread.Start;
end;

constructor TCFCommandListExecutor.Create(
  const AExecutedEvent: TNotifyEvent; const AFreeOnExecuted: Boolean);
begin
  inherited Create;
  FCritical :=  TCFCritical.Create;
  FCommandList  :=  TList<ICFCommand>.Create;
  FreeOnExecuted  :=  AFreeOnExecuted;
  FThread :=  TCFThread.Create(True);
  FThread.OnExecute :=  OnThreadExecute;
  SetOnExecuted(AExecutedEvent);
  FThread.Start;
end;

destructor TCFCommandListExecutor.Destroy;
begin
  Stop;
  Clear;
  Start;
  FThread.Terminate;
  FThread.Free;
  FCommandList.Free;
  FCritical.Free;
  inherited;
end;

procedure TCFCommandListExecutor.Clear;
begin
  FCritical.Enter;
  try
    FCommandList.Clear;
  finally
    FCritical.Leave;
  end;
end;

function TCFCommandListExecutor.CommandExists(
  const ACommandID: Integer): Boolean;
var
  i : Integer;
begin
  for i := FCommandList.Count - 1 downto 0 do
    if FCommandList[i].GetID = ACommandID then
      Exit(True);
  Result  :=  False;
end;

function TCFCommandListExecutor.GetCommand(
  const ACommandID: Integer): ICFCommand;
var
  i : Integer;
begin
  for i := FCommandList.Count - 1 downto 0 do
    if FCommandList[i].GetID = ACommandID then
      Exit(FCommandList[i]);
  Result  :=  nil;
end;

procedure TCFCommandListExecutor.DoBeforeExecuteCommand;
begin
  if Assigned(BeforeExecuteCommand) then
    BeforeExecuteCommand(CurrCommand);
end;

procedure TCFCommandListExecutor.DoAfterExecuteCommand;
begin
  if Assigned(AfterExecuteCommand) then
    AfterExecuteCommand(CurrCommand);
  FCritical.Enter;
  try
    if FCommandList.Count > 0 then
      FCommandList.Delete(0);
  finally
    FCritical.Leave;
  end;
  if FCommandList.Count = 0 then
    DoExecuted;
end;

procedure TCFCommandListExecutor.DoExecuted;
begin
  FIsRunning  :=  False;
  if Assigned(FOnExecuted) then
  try
    FOnExecuted(Self);
  except
  end;
  if FreeOnExecuted then
    PostMessage(Handle, CF_MSG_Free, 0, 0);
end;

procedure TCFCommandListExecutor.AddCommand(const ACommand: ICFCommand);
begin
  FCritical.Enter;
  try
    FCommandList.Add(ACommand);
  finally
    FCritical.Leave;
  end;
end;

function TCFCommandListExecutor.GetAfterExecuteCommand: TInterfaceNotifyEvent;
begin
  Result  :=  FAfterExecuteCommand;
end;

function TCFCommandListExecutor.GetBeforeExecuteCommand: TInterfaceNotifyEvent;
begin
  Result  :=  FBeforeExecuteCommand;
end;

function TCFCommandListExecutor.GetCurrCommand: ICFCommand;
begin
  if FCommandList.Count > 0 then
    Result  :=  FCommandList[0]
  else
    Result  :=  nil;
end;

function TCFCommandListExecutor.GetIsRunning: Boolean;
begin
  Result  :=  FIsRunning;
end;

function TCFCommandListExecutor.GetOnExecuted: TNotifyEvent;
begin
  Result  :=  FOnExecuted;
end;

procedure TCFCommandListExecutor.OnThreadExecute(Sender: TObject);
var
  ci: ICFInterface;
  cmd : ICFCommand;
begin
  while not TCFThread(Sender).Terminated do
  begin
    TCFThread(Sender).ProcessMessages;
    cmd :=  CurrCommand;
    if Assigned(cmd) and cmd.CanExecute then
    begin
      FIsRunning  :=  True;
      TCFThread(Sender).Synchronize(DoBeforeExecuteCommand);
      try
        cmd.Execute;
      except
        on e: Exception do
          if cmd.QueryInterface(IID_ICFInterface, ci) = S_OK then
            if ci.ImplementObject is TCFCommand then
              TCFCommand(ci.ImplementObject).FErrorMessage := e.Message;
      end;
      if not TCFThread(Sender).Terminated then
        if cmd = CurrCommand then
          TCFThread(Sender).Synchronize(DoAfterExecuteCommand);
    end
    else
      FIsRunning  :=  False;
    Sleep(1);
  end;
end;

procedure TCFCommandListExecutor.SetAfterExecuteCommand(
  const Value: TInterfaceNotifyEvent);
begin
  FAfterExecuteCommand  :=  Value;
end;

procedure TCFCommandListExecutor.SetBeforeExecuteCommand(
  const Value: TInterfaceNotifyEvent);
begin
  FAfterExecuteCommand  :=  Value;
end;

procedure TCFCommandListExecutor.SetOnExecuted(
  const Value: TNotifyEvent);
begin
  FOnExecuted :=  Value;
end;

function TCFCommandListExecutor.Start: Boolean;
begin
  if FThread.Suspended then
    FThread.Suspended :=  False;
  Result  :=  True;
end;

procedure TCFCommandListExecutor.Stop;
begin
  FIsRunning  :=  False;
  if Assigned(CurrCommand) then
  begin
    CurrCommand.Terminate;
  end;
  if not FThread.Terminated then
    FThread.Suspended :=  True;
end;

procedure TCFCommandListExecutor.WndProc(var Message: TMessage);
begin
  case message.Msg of
    CF_MSG_Free : Free;
    else
      inherited;
  end;
end;

{ TCFObjectList<T> }

constructor TCFObjectList<T>.Create;
begin
  inherited Create(True);
end;

function TCFObjectList<T>.ImplementObject: TObject;
begin
  Result := Self;
end;

procedure TCFObjectList<T>.OnNotifyMessage(Msg: UINT; wParam: wParam;
  lParam: lParam);
begin
end;

function TCFObjectList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TCFObjectList<T>.ReleaseInstance;
begin
  _Release;
end;

function TCFObjectList<T>._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TCFObjectList<T>._Release: Integer;
begin
  if FRefCount > 0 then
    Result := InterlockedDecrement(FRefCount)
  else
    Result := 0;
  if (Result = 0) and not FDestroying then
  begin
    FDestroying := True;
    Destroy;
  end;
end;

{$IFDEF FRAMEWORK_INCLUDE_DB}
function TCFObjectList<T>.PropertiesReadFromDataSet(
  const ADataSet: TDataSet): Boolean;
var
  bk : TBytes;
  Item : TObject;
  intf : ICFPropertiesDataSetReader;
begin
  Result  :=  False;
  if ADataSet.IsEmpty then Exit;
  ADataSet.DisableControls;
  bk  :=  ADataSet.GetBookmark;
  try
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      Item  :=  NewItem;
      if Item.GetInterface(IID_ICFPropertiesDataSetReader, intf) then
        intf.PropertiesReadFromDataSet(ADataSet)
      else if Item is TPersistent then
        Item.PropertiesReadFromDataSet(ADataSet);
      Add(Item);
      ADataSet.Next;
    end;
  finally
    ADataSet.GotoBookmark(bk);
    ADataSet.EnableControls;
  end;
end;
{$ENDIF}

{$IFDEF FRAMEWORK_INCLUDE_XML}
function TCFObjectList<T>.PropertiesReadFromXMLNode(const AIntf : IInterface; const ANodePath : string) : Boolean;
var
  Node : IXMLNode;
  i : Integer;
  Item : TObject;
  intf : ICFPropertiesXMLReader;
begin
  Result  :=  False;
  if not Assigned(AIntf) then Exit;
  if not AIntf.QueryInterface(IID_IXMLNode, node) = S_OK then Exit;
  Node  :=  TCFXml.FindXMLNode(Node, ANodePath);
  for i := 0 to Node.ChildNodes.Count - 1 do
  begin
    Item := NewItem;
    if Item.GetInterface(IID_ICFPropertiesXMLReader, intf) then
      intf.PropertiesReadFromXMLNode(Node.ChildNodes[i], '')
    else if Item is TPersistent then
      Item.PropertiesReadFromXMLNode(Node.ChildNodes[i]);
    Add(Item);
  end;
end;
{$ENDIF}

{ TCFLog }

function TCFLog.GetOnLog: TNotifyMessageEvent;
begin
  Result  :=  FOnLog;
end;

procedure TCFLog.SetOnLog(AEvent: TNotifyMessageEvent);
begin
  FOnLog  :=  AEvent;
end;

{ TCFTextFileLog }

constructor TCFTextFileLog.Create;
begin
  inherited;
  FCacheList  :=  TStringList.Create;
  InitializeCriticalSection(FLock);
end;

destructor TCFTextFileLog.Destroy;
begin
  Close;
  FCacheList.Free;
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TCFTextFileLog.LockList;
begin
  EnterCriticalSection(FLock);
end;

procedure TCFTextFileLog.Open;
begin
end;

procedure TCFTextFileLog.Close;
begin
  WriteCacheList;
end;

procedure TCFTextFileLog.SetFileName(const Value: string);

  function _Macro(const AStr : string):string;
  begin
    Result  :=  StringReplace(AStr, '[Date]', FormatDateTime('YYYY-MM-DD', Now), [rfReplaceAll]);
  end;

var
  s : string;
begin
  s :=  _Macro(Value);

  if FileName = s then
    Exit;
  if FFileName <> '' then
    Close;

  FFileName := s;
  if FileName = '' then
    Exit;

  if Pos(':', FFileName) = 0 then
    FFileName :=  Win.GetInstancePath + FFileName;

  Open;
end;

procedure TCFTextFileLog.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

procedure TCFTextFileLog.WriteCacheList;
var
  f : TTextWriter;
  i : Integer;
begin
  if FileName = '' then
  begin
    FLastWriteTick  :=  GetTickCount;
    Exit;
  end;

  LockList;
  try
    try
      f :=  TStreamWriter.Create(FileName, True);
      try
        for i := 0 to FCacheList.Count - 1 do
          f.WriteLine(FCacheList[i]);
        FCacheList.Clear;
        FLastWriteTick  :=  GetTickCount;
      finally
        f.Free;
      end;
    except
    end;
  finally
    UnlockList;
  end;
end;

procedure TCFTextFileLog.WriteLog(const ALog: string);
begin
  if Assigned(FOnLog) then
    OnLog(Self, ALog);

  LockList;
  try
    FCacheList.Add(FormatDateTime('YYYY-MM-DD hh:nn:ss', Now) + ' ' + ALog);
  finally
    UnlockList;
  end;

  if (FCacheList.Count > 10) or (GetTickCount - FLastWriteTick > 60 * 1000) then
    WriteCacheList;
end;

procedure TCFTextFileLog.WriteLog(const AType: TCFLogType; const ALog: string);
begin
  WriteLog(IntToStr(Integer(AType)) + ' ' + ALog);
end;

{ TCFStringRepository }

constructor TCFStringRepository.Create;
begin
  inherited;
  FCritical :=  TCFCritical.Create;
  FStringDict :=  TDictionary<Integer, string>.Create;
end;

destructor TCFStringRepository.Destroy;
begin
  FStringDict.Free;
  FCritical.Free;
  inherited;
end;

procedure TCFStringRepository.Clear;
begin
  FStringDict.Clear;
end;

function TCFStringRepository.Add(const AStr: string): Integer;
begin
  FCritical.Enter;
  try
    Inc(FCurrID);
    FStringDict.AddOrSetValue(FCurrID, AStr);
    Result  :=  FCurrID;
  finally
    FCritical.Leave;
  end;
end;

function TCFStringRepository.Get(const AID: Integer): string;
begin
  FCritical.Enter;
  try
    if not FStringDict.TryGetValue(AID, Result) then
      Result  :=  '';
  finally
    FCritical.Leave;
  end;
end;

function TCFStringRepository.Extract(const AID: Integer): string;
begin
  FCritical.Enter;
  try
    if FStringDict.TryGetValue(AID, Result) then
      FStringDict.Remove(AID)
    else
      Result  :=  '';
  finally
    FCritical.Leave;
  end;
end;

{ TCFThreadList }

constructor TCFThreadList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TList.Create;
  FDuplicates := dupIgnore;
end;

destructor TCFThreadList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;

  inherited;
end;

procedure TCFThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      FList.Error(@SDuplicateItem, Integer(Item));
  finally
    UnlockList;
  end;
end;

procedure TCFThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TCFThreadList.LockList: TList;
begin
  try
    try
      EnterCriticalSection(FLock);
    except
    end;
  finally
    Result := FList;
  end;
end;

procedure TCFThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    try
      FList.Remove(Item);
    except
    end;
  finally
    UnlockList;
  end;
end;

procedure TCFThreadList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

function TCFThreadList.GetCount: integer;
begin
  result := FList.Count;
end;

function TCFThreadList.Items(idx: Integer): Pointer;
begin
  if idx <= FList.Count - 1 then
    Result := FList[idx]
  else
    Result := nil;
end;

end.
