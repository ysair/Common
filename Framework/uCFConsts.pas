//Common Framework
//常量定义单元

unit uCFConsts;

interface

uses
  Messages;

const
  IID_IXMLNode                    : TGUID = '{395950C0-7E5D-11D4-83DA-00C04F60B2DD}';

const
  IID_IInterface                  : TGUID = '{00000000-0000-0000-C000-000000000046}';
  IID_ICFInterface                : TGUID = '{A53141F4-D12F-4C7E-8F67-13672D585D76}';
  IID_ICFManager                  : TGUID = '{79938A11-02E3-4E76-B07A-23E7C5ED65D6}';
  IID_ICFDataBaseStorage          : TGUID = '{75143280-CBC8-4D0B-BD3C-08FD1DB47B47}';
  IID_ICFPropertiesXMLReader      : TGUID = '{747E22B8-DA28-48D3-A84C-34444325C502}';
  IID_ICFPropertiesXMLWriter      : TGUID = '{02F46CB9-A562-42BE-8192-79AE85F44D11}';
  IID_ICFPropertiesXMLTextReader  : TGUID = '{B2C40D22-675E-418C-B5F3-604B9DBCE273}';
  IID_ICFPropertiesXMLTextWriter  : TGUID = '{FC3DD24B-66C9-4CE5-A063-9A7FF0C506E2}';
  IID_ICFPropertiesXMLFileReader  : TGUID = '{986BF783-2ECF-47D5-8E47-56D50548ECFD}';
  IID_ICFPropertiesXMLFileWriter  : TGUID = '{8F8B0C41-5BAD-4CA0-829F-FB3EE628FDF6}';
  IID_ICFPropertiesDataSetReader  : TGUID = '{CA1B8268-61C0-4916-AA61-178890469B39}';
  IID_ICFPropertiesDataSetWriter  : TGUID = '{C894B16B-87BB-4A2A-B437-CD3A4DE5A97E}';
  IID_ICFInterfaceBuilder         : TGUID = '{DBA589BD-13EB-48E9-8A6D-AFD1E356328E}';
  IID_ICFObjectBuilder            : TGUID = '{640C8DCA-7D77-4F12-8B19-F368E2BE87DB}';
  IID_ICFAdapter                  : TGUID = '{AE1C0848-39E1-416E-BBB1-AF71CCFCCA6A}';
  IID_ICFInterfacePool            : TGUID = '{C9780674-665D-4EF5-B5E6-DD96ED58DC0A}';
  IID_ICFObjectPool               : TGUID = '{F13A5FA3-9462-4C2B-B5AF-66626060A36F}';
  IID_ICFAuotRecycle              : TGUID = '{32E047AA-8D6A-4D36-81AE-980D3D94822E}';
  IID_ICFInterfaceFactory         : TGUID = '{2BABC34E-5DF9-4C45-B521-8118E5104759}';
  IID_ICFBuiltInterface           : TGUID = '{49336D08-5FB8-45B1-97F2-7BE1F1C75BD1}';
  IID_ICFCommand                  : TGUID = '{26C8EA54-058C-40E5-A7F5-303F4E6B7CEB}';
  IID_ICFCommandExecutor          : TGUID = '{C6B89AB7-3DB0-48E1-9200-E0A46D872735}';
  IID_ICFCommandListExecutor      : TGUID = '{4DAE0DE6-CA36-4DAF-B3C8-0478B174908D}';
  IID_ICFTreeItem                 : TGUID = '{FA58C801-1DFA-4807-9688-14509FA598C0}';
  IID_ICFLog                      : TGUID = '{FB886BA2-F480-473F-8C5B-75CA4C74E739}';
  IID_ICFMemoryManager            : TGUID = '{502F6EB4-7FFB-4E4A-A51F-E33839DD61AB}';
  IID_ICFMessageHandled           : TGUID = '{79ADCEF4-A3D1-4940-B24F-505B5F07C5EE}';
  IID_ICFFileStorage              : TGUID = '{4AE220F3-06BD-4937-B801-ED0D4D7AF308}';

const //消息定义
  CF_MSG_BASE                     = WM_USER + $F000;   //WM_USER + $F000 到 WM_USER + $FFFF预留
  CF_MSG_Config_Changed           = CF_MSG_BASE + $001;
  CF_MSG_CreateComponent          = CF_MSG_BASE + $021;
  CF_MSG_Free                     = CF_MSG_BASE + $099;
  CF_MSG_DB_BASE                  = CF_MSG_BASE + $100;//WM_USER + $F100 到 WM_USER + $F1FF 数据相关消息
  CF_MSG_DB_RefreshCache          = CF_MSG_DB_BASE + $01;

const //数据库常量
  DB_Disconnection_Interval = 5 * 60 * 1000;  //5分钟 断开连接
  DB_ClearPool_Interval     = 10 * 60 * 1000; //10分钟 清空池
  DB_AutoRecycle_Interval   = 0;//30 * 1000;   //30秒 自动回收
  DB_Connection_Timeout     = 30;      //30秒连接超时
  DB_Command_Timeout        = 5 * 60;  //5分钟命令超时
  DB_Def_StorageBuilder     = 'TADODBStorageBuilder';

  DB_DefField_ID            = 'ID';

const
  DefTempFilePrefix   = 'tmp';

const
  ConnectionString_Access = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;'
                           +'Jet OLEDB:Database Password=%s;';

const
  Def_CommandEventTime          = 1000;

  CommandResultCode_OK          = 0;
  CommandResultCode_Error       = -1;
  CommandResultCode_Terminated  = -2;
  CommandResultCode_More        = 999;

  CommandID_CFBase            = $1000;
  CommandID_CFDBBase          = CommandID_CFBase + $1000;
  //CommandID_ConnectTo         = CommandID_CFDBBase + 1;
  CommandID_CFUser            = $10000;

implementation

end.
