//Common Framework
//配置单元
{$I CommonFramework.inc}
unit uCFGlobal;

interface

uses
  SysUtils, Classes, Windows, Messages,
  uCFIntfDef, uCFClasses, uCFController, uCFXML, uCFTypedef;

type
  //数据库配置对象
  TCFDataBaseConfig = class(TCFConfig)
  private
    FConnectionString: string;
    FDisconnection_Interval: Integer;
    FClearPool_Interval: Integer;
    FBuilder: string;
    FAutoRecycle_Interval: Integer;
    FProvider: string;
  public
    constructor Create; override;
  published
    property Builder : string read FBuilder write FBuilder;
    property Provider : string read FProvider write FProvider;
    property ConnectionString : string read FConnectionString write FConnectionString;
    property Disconnection_Interval : Integer read FDisconnection_Interval write FDisconnection_Interval;
    property ClearPool_Interval : Integer read FClearPool_Interval write FClearPool_Interval;
    property AutoRecycle_Interval : Integer read FAutoRecycle_Interval write FAutoRecycle_Interval;
  end;

  //日志配置类
  TCFLogConfig = class(TCFConfig)
  private
    FSaveLog: Boolean;
    FFileName: string;
  published
    property SaveLog : Boolean read FSaveLog write FSaveLog;
    property FileName : string read FFileName write FFileName;
  end;

  //配置集合类
  TCFConfigCollection = class(TCFObject, ICFPropertiesXMLReader)
  private
    FDataBase: TCFDataBaseConfig;
    FLog: TCFLogConfig;
  protected
    function PropertiesReadFromXMLNode(const ANode : IInterface; const ANodePath : string) : Boolean;
  public
    property DataBase : TCFDataBaseConfig read FDataBase;
    property Log : TCFLogConfig read FLog;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  LoadFromXMLFile(const AFileName : string; const ANodePath : string) : boolean;
  end;

  //全局类
  TCFGlobal = class(TObject)
  private
    FManager: ICFManager;
    FInterfaceFactory : ICFInterfaceFactory;
    FConfig: TCFConfigCollection;
    FCritical: TCFCritical;
    FCFLog: TCFLog;
    FMainThreadID: DWORD;
    FMessageObject : ICFMessageHandled;
    FStringRepository: ICFStringRepository;
  protected
    function  GetCFLog: ICFLog;
    function  GetMessageHandle: HWND;
    procedure SetManager(const Value: ICFManager);
    procedure SetInterfaceFactory(const Value: ICFInterfaceFactory);
    procedure CreateBuilder(const AClassName : string);
  public
    property Manager : ICFManager read FManager write SetManager;
    property InterfaceFactory : ICFInterfaceFactory read FInterfaceFactory write SetInterfaceFactory;
    property Config : TCFConfigCollection read FConfig;
    property Critical : TCFCritical read FCritical;
    property Log : ICFLog read GetCFLog;
    property MainThreadID : DWORD read FMainThreadID;
    property MessageHandle : HWND read GetMessageHandle;
    property StringRepository : ICFStringRepository read FStringRepository;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize; reintroduce;
  end;

var
  CFGlobal  : TCFGlobal;

implementation

uses
  uCFConsts, uCFResource;

type
  TSyncMessageObject = class(TCFHandledObject)
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

{ TCFDataBaseConfig }

constructor TCFDataBaseConfig.Create;
begin
  inherited;
  Disconnection_Interval  :=  DB_Disconnection_Interval;
  ClearPool_Interval      :=  DB_ClearPool_Interval;
  AutoRecycle_Interval    :=  DB_AutoRecycle_Interval;
  Builder                 :=  DB_Def_StorageBuilder;
end;

{ TCFConfigCollection }

constructor TCFConfigCollection.Create;
begin
  inherited;
  FDataBase :=  TCFDataBaseConfig.Create;
  FLog := TCFLogConfig.Create;
end;

destructor TCFConfigCollection.Destroy;
begin
  FDataBase.Free;
  FLog.Free;
  inherited;
end;

function TCFConfigCollection.LoadFromXMLFile(
    const AFileName : string; const ANodePath : string) : boolean;
begin
  Result  :=  PropertiesReadFromXMLNode(
      TCFXml.FindXMLNode(
          TCFXml.OpenFile(AFileName),
          ANodePath
          ),
      '');
end;

function TCFConfigCollection.PropertiesReadFromXMLNode(
    const ANode: IInterface; const ANodePath : string): Boolean;
begin
  (DataBase as ICFPropertiesXMLReader).PropertiesReadFromXMLNode(ANode, 'DataBase');
  (Log as ICFPropertiesXMLReader).PropertiesReadFromXMLNode(ANode, 'Log');
  Result  :=  True;
end;

{ TCFGlobal }

constructor TCFGlobal.Create;
begin
  inherited;
  FMainThreadID :=  GetCurrentThreadId;
  FCritical :=  TCFCritical.Create;
  FStringRepository :=  TCFStringRepository.Create;
  SetManager(TCFManager.Create);
  SetInterfaceFactory(TCFInterfaceFactory.Create);
  FConfig :=  TCFConfigCollection.Create;
  FCFLog  :=  TCFTextFileLog.Create;
  (FCFLog as IInterface)._AddRef;
  FMessageObject  :=  TSyncMessageObject.Create;
end;

destructor TCFGlobal.Destroy;
begin
  FreeAndNil(FConfig);
  FreeAndNil(FCFLog);
  FStringRepository :=  nil;
  FCritical.Free;
  FManager  :=  nil;
  FInterfaceFactory :=  nil;
  inherited;
end;

function TCFGlobal.GetCFLog: ICFLog;
begin
  Result  :=  FCFLog as ICFLog;
end;

function TCFGlobal.GetMessageHandle: HWND;
begin
  Result  :=  FMessageObject.GetHandle;
end;

procedure TCFGlobal.Initialize;
begin
  CreateBuilder(Config.DataBase.Builder);
  if Config.Log.SaveLog then
    TCFTextFileLog(FCFLog).FileName :=  Config.Log.FileName;
end;

procedure TCFGlobal.CreateBuilder(const AClassName: string);
var
  cls : TPersistentClass;
  builder : ICFInterfaceBuilder;
  obj : TCFPersistentObject;
begin
  inherited;
  cls :=  GetClass(AClassName);
  if not Assigned(cls) then Exit;
  if not Assigned(cls.GetInterfaceEntry(IID_ICFInterfaceBuilder)) then Exit;
  if not cls.InheritsFrom(TCFPersistentObject) then Exit;
  obj :=  TCFPersistentObjectClass(cls).Create;
  builder :=  obj as ICFInterfaceBuilder;
  InterfaceFactory.RegisterBuilder(builder);
end;

procedure TCFGlobal.SetManager(const Value: ICFManager);
begin
  if Assigned(Value) then
    FManager := Value;
end;

procedure TCFGlobal.SetInterfaceFactory(const Value: ICFInterfaceFactory);
begin
  if Assigned(Value) then
    FInterfaceFactory := Value;
end;

{ TSyncMessageObject }

procedure TSyncMessageObject.WndProc(var Message: TMessage);
var
  cls : TComponentClass;
  obj : TComponent;
begin
  case Message.Msg of
    CF_MSG_CreateComponent  :
      begin
        cls :=  TComponentClass(Pointer(Message.WParam));
        obj :=  cls.Create(nil);
        obj.Tag :=  GetCurrentThreadId;
        Message.Result  :=  Integer(obj);
      end;
    else
      inherited;
  end;
end;

initialization
  CFGlobal  :=  TCFGlobal.Create;

finalization
  FreeAndNil(CFGlobal);

end.
