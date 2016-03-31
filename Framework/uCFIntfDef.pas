//Common Framework
//接口定义单元
{$I CommonFramework.inc}
unit uCFIntfDef;

interface

uses
  SysUtils, Classes, Windows
  {$IFDEF FRAMEWORK_INCLUDE_DB}, DB, DBClient, MidasLib{$ENDIF}
  ,uCFConsts, uCFTypedef;

type
  //基础接口
  ICFInterface = interface
    ['{A53141F4-D12F-4C7E-8F67-13672D585D76}']
    function  ImplementObject : TObject;  //实现接口的对象实例
    procedure ReleaseInstance;            //释放接口/对象
    procedure OnNotifyMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM); //收到通知消息
  end;

  //管理接口
  ICFManager = interface
    ['{79938A11-02E3-4E76-B07A-23E7C5ED65D6}']
    procedure RegisterInterface(const AIntf : ICFInterface);   //注册接口
    procedure UnregisterInterface(const AIntf : ICFInterface); //注销接口
    procedure NotifyMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM); //广播通知消息
  end;

  //消息接口
  ICFMessageHandled = interface
    ['{79ADCEF4-A3D1-4940-B24F-505B5F07C5EE}']
    procedure SetMessageEvent(const Value: TWndMethod);
    function  GetMessageEvent : TWndMethod;
    function  GetHandle : HWND;
  end;

  //XML读接口
  ICFPropertiesXMLReader = interface
    ['{747E22B8-DA28-48D3-A84C-34444325C502}']
    function PropertiesReadFromXMLNode(const AIntf : IInterface; const ANodePath : string) : Boolean;
  end;
  ICFPropertiesXMLTextReader = interface
    ['{B2C40D22-675E-418C-B5F3-604B9DBCE273}']
    function PropertiesReadFromXML(const AXML : string; const ANodePath : string) : boolean;
  end;
  ICFPropertiesXMLFileReader = interface
    ['{986BF783-2ECF-47D5-8E47-56D50548ECFD}']
    function PropertiesReadFromXMLFile(const AFileName : string; const ANodePath : string) : Boolean;
  end;

  //XML写接口
  ICFPropertiesXMLWriter = interface
    ['{02F46CB9-A562-42BE-8192-79AE85F44D11}']
    function PropertiesWriteToXMLNode(const AIntf : IInterface; const ANodePath : string) : Boolean;
  end;
  ICFPropertiesXMLTextWriter = interface
    ['{FC3DD24B-66C9-4CE5-A063-9A7FF0C506E2}']
    function PropertiesWriteToXML(var AXML : string; const ANodePath : string) : Boolean;
  end;
  ICFPropertiesXMLFileWriter = interface
    ['{8F8B0C41-5BAD-4CA0-829F-FB3EE628FDF6}']
    function PropertiesWriteToXMLFile(const AFileName : string; const ANodePath : string) : Boolean;
  end;

  //JSON读接口
  ICFPropertiesJSONReader = interface
    ['{EDBB1E15-EE0C-47CB-84F0-D07FD0C39803}']
    function PropertiesReadFromJSONObject(const AIntf : IInterface; const APath : string) : Boolean;
  end;
  ICFPropertiesJSONTextReader = interface
    ['{6E0805DC-4A19-40CF-B7B3-A282BBA68BAE}']
    function PropertiesReadFromJSON(const AJSON : string; const APath : string) : boolean;
  end;
  ICFPropertiesJSONFileReader = interface
    ['{7C22767A-227E-4612-B8F4-6D6D6991E476}']
    function PropertiesReadFromJSONFile(const AFileName : string; const APath : string) : Boolean;
  end;

  //JSON写接口
  ICFPropertiesJSONWriter = interface
    ['{8DDC3A54-2E96-4E4A-B106-BF933C1356E5}']
    function PropertiesWriteToJSONObject(const AIntf : IInterface; const APath : string) : Boolean;
  end;
  ICFPropertiesJSONTextWriter = interface
    ['{2EE1AFB3-20B1-4F14-9F91-E2CDF574A4EC}']
    function PropertiesWriteToJSON(var AJSON : string; const APath : string) : Boolean;
  end;
  ICFPropertiesJSONFileWriter = interface
    ['{CC2E6AAD-CCCC-487D-B5CB-98061B8A5205}']
    function PropertiesWriteToJSONFile(const AFileName : string; const APath : string) : Boolean;
  end;

  //文件存储接口
  ICFFileStorage = interface
    ['{4AE220F3-06BD-4937-B801-ED0D4D7AF308}']
    function SaveString(const ASection : string; const AKey : string; const AValue : string):boolean;
    function LoadString(const ASection : string; const AKey : string):string;
    function DeleteString(const ASection : string; const AKey : string):boolean;
    function DeleteSection(const ASection : string):boolean;
    function GetSectionKeys(const ASection : string; const AKeyList : TStrings):Integer;
  end;

  {$IFDEF FRAMEWORK_INCLUDE_DB}
  //DataSet读接口
  ICFPropertiesDataSetReader = interface
    ['{CA1B8268-61C0-4916-AA61-178890469B39}']
    function PropertiesReadFromDataSet(const ADataSet : TDataSet) : Boolean;
  end;
  //DataSet写接口
  ICFPropertiesDataSetWriter = interface
    ['{C894B16B-87BB-4A2A-B437-CD3A4DE5A97E}']
    function PropertiesWriteToDataSet(const ADataSet : TDataSet) : Boolean;
  end;

  //数据库存储接口
  ICFDataBaseStorage = interface(ICFInterface)
    ['{75143280-CBC8-4D0B-BD3C-08FD1DB47B47}']
    function  GetLastErrorMessage : string;                           //获取最后的错误信息
    function  GetProvider : string;                                   //获取Provider
    procedure SetProvider(const AProvider : string);                  //设置Provider
    procedure SetConnectionString(const AConnectStr : string);        //设置连接字符串
    function  GetConnectionString : string;                           //获取连接字符串
    function  OpenConnection : Boolean;                               //打开链接
    procedure CloseConnection;                                        //关闭连接
    function  GetConnected : boolean;                                 //获取连接状态
    function  SupportTrans : boolean;                                 //是否支持事务
    procedure BeginTrans;                                             //开始事务
    procedure CommitTrans;                                            //提交事务
    procedure RollbackTrans;                                          //回滚事务
    function  TableExists(const ATableName : string):boolean;         //表是否存在
    function  ExecSQL(const ASQL : string):boolean;                   //执行SQL
    function  Query(const ASQL : string; const AKeyField : string) : TClientDataSet;  //查询数据
    function  Update(const ADataSet : TClientDataSet):boolean;        //更新数据
    function  RefreshCache : Boolean;                                 //刷新缓存
    //
    function  BaseQuery(const ASQL : string) : TDataSet;              //查询原始数据集
  end;
  {$ENDIF}

  //接口池接口
  ICFInterfacePool = interface
    ['{C9780674-665D-4EF5-B5E6-DD96ED58DC0A}']
    //procedure RecycleInterface(const AIntf : ICFInterface);   //回收接口
    function  GetClearPoolInterval : Integer;                 //获取清除池的时间间隔
    procedure SetClearPoolInterval(const AValue : Integer);   //设置清除池的时间间隔
    procedure Push(Intf: IInterface);                         //压栈
    function  Pop: IInterface;                                //出栈
    function  GetClearType: TPoolsClearType;
    procedure SetClearType(const Value: TPoolsClearType);
  end;

  //对象池接口
  ICFObjectPool = interface
    ['{F13A5FA3-9462-4C2B-B5AF-66626060A36F}']
    //procedure RecycleObject(const AObj : TObject);            //回收对象
    function  GetClearPoolInterval : Integer;                 //获取清除池的时间间隔
    procedure SetClearPoolInterval(const AValue : Integer);   //设置清除池的时间间隔
    procedure Push(Obj: TObject);                             //压栈
    function  Pop: TObject;                                   //出栈
    function  GetClearType: TPoolsClearType;
    procedure SetClearType(const Value: TPoolsClearType);
  end;//}

  //接口构造器
  ICFInterfaceBuilder = interface
    ['{DBA589BD-13EB-48E9-8A6D-AFD1E356328E}']
    function BuiltInterfaceGUID : TGUID;            //被构建接口的GUID
    function BuildInterface : IInterface;           //构建接口
    function SupportPool : Boolean;                 //是否支持池
    function GetPool : ICFInterfacePool;            //接口池
  end;

  //对象构造器
  ICFObjectBuilder = interface
    ['{640C8DCA-7D77-4F12-8B19-F368E2BE87DB}']
    function BuiltClass : TClass;                   //被构建对象的类型
    function BuildObject : TObject;                 //构建对象
    function SupportPool : Boolean;                 //是否支持池
    function GetPool : ICFObjectPool;               //对象池
  end;

  //适配器
  ICFAdapter = interface
    ['{AE1C0848-39E1-416E-BBB1-AF71CCFCCA6A}']
    function  GetCaller : TObject;                  //获取调用者
    procedure SetCaller(const AObj : TObject);      //设置调用者
    function  GetAdapteeClass : TClass;             //获取被适配对象的类型
  end;//}

  //被构建接口
  ICFBuiltInterface = interface
    ['{49336D08-5FB8-45B1-97F2-7BE1F1C75BD1}']
    function  GetBuilder : ICFInterfaceBuilder;               //获取构建器
    procedure SetBuilder(const AIntf : ICFInterfaceBuilder);  //设置构建器
  end;

  //自动回收接口
  ICFAuotRecycle = interface
    ['{32E047AA-8D6A-4D36-81AE-980D3D94822E}']
    function  GetAutoRecycle : boolean;
    procedure SetAutoRecycle(const AValue : Boolean);
    function  GetCanAutoRecycle : Boolean;
  end;

  //接口工厂
  ICFInterfaceFactory = interface
    ['{2BABC34E-5DF9-4C45-B521-8118E5104759}']
    function  CreateInterface(const AIntfGUID : TGUID) :  ICFInterface;
    procedure FreeInterface(const AIntf : ICFInterface);
    procedure RegisterBuilder(const ABuilder : ICFInterfaceBuilder);
    procedure UnregisterBuilder(const ABuilder : ICFInterfaceBuilder);
    procedure ClearBuilders(const AFree : Boolean);
  end;

  //命令接口
  ICFCommand  = interface
    ['{26C8EA54-058C-40E5-A7F5-303F4E6B7CEB}']
    function  CanExecute : Boolean;
    procedure Execute;
    function  GetResultCode : Integer;
    function  GetErrorMessage : string;
    function  GetID : Integer;
    procedure SetParameters(const AParameters : array of const);
    function  GetParameterCount : Integer;
    procedure SetParameterCount(const ACount : Integer);
    procedure SetParameter(const AIndex : Integer; const Value: Variant);
    function  GetParameter(const AIndex : Integer) : Variant;
    procedure Terminate;
  end;

  //命令执行接口
  ICFCommandExecutor = interface
    ['{C6B89AB7-3DB0-48E1-9200-E0A46D872735}']
    function  GetCommand: ICFCommand;
    procedure SetCommand(const Value: ICFCommand);
    function  GetBeforeExecuteCommand : TInterfaceNotifyEvent;
    procedure SetBeforeExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetAfterExecuteCommand : TInterfaceNotifyEvent;
    procedure SetAfterExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetIsRunning: Boolean;
    function  GetCanExecuteCommand: Boolean;
    function  CommandCanExecute: Boolean;
    function  ExecuteCommand(const ACommand: ICFCommand): Boolean;
    function  Start : Boolean;
    procedure Stop;
  end;

  //命令列表执行接口
  ICFCommandListExecutor = interface
    ['{4DAE0DE6-CA36-4DAF-B3C8-0478B174908D}']
    function  GetCurrCommand: ICFCommand;
    function  GetBeforeExecuteCommand : TInterfaceNotifyEvent;
    procedure SetBeforeExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetAfterExecuteCommand : TInterfaceNotifyEvent;
    procedure SetAfterExecuteCommand(const Value: TInterfaceNotifyEvent);
    function  GetOnExecuted: TNotifyEvent;
    procedure SetOnExecuted(const Value: TNotifyEvent);
    function  GetIsRunning: Boolean;
    procedure AddCommand(const ACommand: ICFCommand);
    function  CommandExists(const ACommandID : Integer):Boolean;
    procedure Clear;
    function  Start : Boolean;
    procedure Stop;
  end;

  //日志接口
  ICFLog = interface
    ['{FB886BA2-F480-473F-8C5B-75CA4C74E739}']
    procedure WriteLog(const ALog : string); overload;
    procedure WriteLog(const AType : TCFLogType; const ALog : string); overload;
    procedure SetOnLog(AEvent : TNotifyMessageEvent);
    function  GetOnLog : TNotifyMessageEvent;
  end;

  //字符串仓库接口
  ICFStringRepository = interface
    ['{4B1C862B-FB89-4F2D-950C-7939FA6DFED0}']
    procedure Clear;
    function  Add(const AStr : string):Integer;
    function  Get(const AID : Integer):string;
    function  Extract(const AID : Integer):string;
  end;

implementation

end.
