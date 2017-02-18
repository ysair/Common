//Common Framework
//XML操作单元
{$I CommonFramework.inc}
unit uCFXML;

interface

uses
  SysUtils, Classes, XMLDoc, XMLIntf, typinfo, Variants, StrUtils,
  uCFIntfDef, uCFClasses;

type
  //XML解析类
  TCFXml = class(TCFObject)
  public
    class function  ExtractFirstNodeName(var APath : string):string; static;
    class function  CreateDoc(const ARootName : string) : IXMLDocument; static;
    class function  OpenXML(const AXML : string) : IXMLDocument; static;
    class function  OpenFile(const AFileName : string) : IXMLDocument; static;
    class function  OpenStream(const AStream : TStream; AEncodingType: TXMLEncodingType = xetUnknown) : IXMLDocument; static;
    class function  ForcePath(const ANode : IXMLNode; const ANodePath : string) : IXMLNode; static;
    class function  FindXMLNode(const ARoot : IXMLNode; const ANodePath : string) : IXMLNode; overload; static;
    class function  FindXMLNode(const ADoc : IXMLDocument; const ANodePath : string) : IXMLNode; overload; static;
    class function  TryGetNodeValue(const ARoot : IXMLNode; const ANodePath : string; var AValue : OleVariant):boolean; static;
    class function  GetNodeValue(const ARoot : IXMLNode; const ANodePath : string; const ADefValue : string = ''):string; static;
    class function  ReadPropertiesFromXMLNode(const AObj : TObject; const AIntf : IInterface; const ANodePath : string = '') : Boolean; static;
    class function  ReadPropertiesFromXML(const AObj : TObject; const AXML : string; const ANodePath : string) : boolean; static;
    class function  ReadPropertiesFromXMLFile(const AObj : TObject; const AFileName : string; const ANodePath : string) : Boolean; static;
    class function  WritePropertiesToXMLNode(const AObj : TObject; const AIntf : IInterface; const ANodePath : string) : Boolean; static;
    class function  WritePropertiesToXML(const AObj : TObject; const ANodePath : string; var AXML : string) : Boolean; static;
    class function  WritePropertiesToXMLFile(const AObj : TObject; const ANodePath : string; const AFileName : string) : Boolean; static;
  end;

  //从XML读取对象属性适配器
  TCFXml2PropertiesAdapter = class(TCFAdapter, ICFPropertiesXMLReader, ICFPropertiesXMLTextReader, ICFPropertiesXMLFileReader)
  protected
    function  GetAdapteeClass : TClass; override;
    function  PropertiesReadFromXMLNode(const AIntf : IInterface; const ANodePath : string) : Boolean;
    function  PropertiesReadFromXML(const AXML : string; const ANodePath : string) : boolean;
    function  PropertiesReadFromXMLFile(const AFileName : string; const ANodePath : string) : Boolean;
  end;//}

  //将对象属性写入XML的适配器
  TCFProperties2XMLAdapter = class(TCFAdapter, ICFPropertiesXMLWriter, ICFPropertiesXMLTextWriter, ICFPropertiesXMLFileWriter)
  protected
    function  GetAdapteeClass : TClass; override;
    function  PropertiesWriteToXMLNode(const AIntf : IInterface; const ANodePath : string) : Boolean;
    function  PropertiesWriteToXML(var AXML : string; const ANodePath : string) : Boolean;
    function  PropertiesWriteToXMLFile(const AFileName : string; const ANodePath : string) : Boolean;
  end;//}

implementation

const
  IID_IXMLNode : TGUID  = '{395950C0-7E5D-11D4-83DA-00C04F60B2DD}';

{ TCFXMLIOProxy }

class function TCFXml.ExtractFirstNodeName(var APath: string): string;
var
  lst : TStrings;
begin
  lst :=  TStringList.Create;
  try
    ExtractStrings(['/', '\'], [], PChar(APath), lst);
    Result  :=  lst[0];
    APath  :=  '';
    lst.Delete(0);
    while lst.Count > 0 do
    begin
      APath  :=  APath + lst[0] + '\';
      lst.Delete(0);
    end;
  finally
    lst.Free;
  end;
end;

class function TCFXml.CreateDoc(const ARootName : string): IXMLDocument;
begin
  try
    Result  :=  TXMLDocument.Create(nil);
    Result.LoadFromXML('');
    Result.DocumentElement  :=  Result.CreateNode(ARootName);
    Result.Encoding :=  'UTF-8';
  except
    Result  :=  nil;
  end;
end;

class function TCFXml.OpenXML(const AXML: string): IXMLDocument;
//从文本解析XML,返回接口
var
  doc : IXMLDocument;
begin
  doc :=  TXMLDocument.Create(nil);
  try
    doc.LoadFromXML(AXML);
    Result  :=  doc;
  except
    Result  :=  nil;
  end;
end;

class function TCFXml.OpenFile(const AFileName: string): IXMLDocument;
//打开XML文件,返回接口
var
  doc : IXMLDocument;
begin
  Result  :=  nil;
  if FileExists(AFileName) then
  begin
    doc :=  TXMLDocument.Create(nil);
    try
      doc.LoadFromFile(AFileName);
      Result  :=  doc;
    except
    end;
  end;
end;

class function TCFXml.OpenStream(const AStream: TStream; AEncodingType: TXMLEncodingType = xetUnknown): IXMLDocument;
var
  doc : IXMLDocument;
begin
  Result  :=  nil;
  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    doc :=  TXMLDocument.Create(nil);
    try
      doc.LoadFromStream(AStream, AEncodingType);
      Result  :=  doc;
    except
    end;
  end;
end;

class function TCFXml.ForcePath(const ANode: IXMLNode;
  const ANodePath: string): IXMLNode;
var
  node : IXMLNode;
  lst : TStrings;
  i : Integer;
  cnode : IXMLNode;
begin
  Result  :=  nil;
  if not Assigned(ANode) then Exit;
  lst :=  TStringList.Create;
  try
    ExtractStrings(['/', '\'], [], PChar(ANodePath), lst);
    node  :=  ANode;
    for i := 0 to lst.Count - 1 do
    begin
      cnode :=  node.ChildNodes.FindNode(lst[i]);
      if not Assigned(cnode) then
      begin
        cnode :=  node.AddChild(lst[i]);
      end;
      node  :=  cnode;
    end;
    Result  :=  node;
  finally
    lst.Free;
  end;
end;

class function TCFXml.GetNodeValue(const ARoot: IXMLNode; const ANodePath,
  ADefValue: string): string;
var
  node : IXMLNode;
begin
  node  :=  FindXMLNode(ARoot, ANodePath);
  if Assigned(node) then
    Result  :=  VarToStr(node.NodeValue)
  else
    Result  :=  ADefValue;
end;

class function TCFXml.FindXMLNode(const ARoot: IXMLNode;
  const ANodePath: string): IXMLNode;
//查找XMLNode,返回接口
//可以查找多层节点,如 Root\NodeA\NodeB
var
  lst : TStrings;
  i : Integer;
  j : Integer;
  found : Boolean;
begin
  Result  :=  ARoot;
  if not Assigned(Result) then Exit;
  lst :=  TStringList.Create;
  try
    ExtractStrings(['/', '\'], [], PChar(ANodePath), lst);
    for i := 0 to lst.Count - 1 do
    begin //FindNode有bug,有的节点找不到
      found :=  False;
      for j := 0 to Result.ChildNodes.Count - 1 do
        if Result.ChildNodes[j].NodeName = lst[i] then
        begin
          Result  :=  Result.ChildNodes[j];
          found :=  True;
          Break;
        end;
      if not found then
        exit(nil);
    end;
  finally
    lst.Free;
  end;
end;

class function TCFXml.FindXMLNode(const ADoc : IXMLDocument;
    const ANodePath : string) : IXMLNode;
//查找XMLNode,返回接口
//可以查找多层节点,如 Root\NodeA\NodeB
var
  lst : TStrings;
  i : Integer;
  broot : Boolean;
begin
  if not Assigned(ADoc) then Exit;
  if ANodePath = '' then
  begin
    Result  :=  ADoc.DocumentElement;
    Exit;
  end;

  broot :=  CharInSet(ANodePath[1], ['/', '\']);
  if broot then
    Result  :=  ADoc.DocumentElement
  else
    Result  :=  nil;

  lst :=  TStringList.Create;
  try
    ExtractStrings(['/', '\'], [], PChar(ANodePath), lst);
    for i := 0 to lst.Count - 1 do
      if not broot and (i = 0) then
      begin
        if (lst[i] <> '') and not SameText(ADoc.DocumentElement.NodeName, lst[i]) then Exit;
        Result  :=  ADoc.DocumentElement;
      end
      else begin
        Result  :=  Result.ChildNodes.FindNode(lst[i]);
        if not Assigned(Result) then Break;
      end;
  finally
    lst.Free;
  end;
end;

class function TCFXml.ReadPropertiesFromXMLNode(const AObj : TObject;
    const AIntf : IInterface; const ANodePath : string) : Boolean;
//从XMLNode读取对象属性
//支持以下两种读法
//<node Attrib1="value1" Attrib2="value2" />
//<node> <Attrib1>value1</Attrib1> <Attrib2>value2</Attrib2> </node>
var
  node : IXMLNode;
  cnode : IXMLNode;
  plist : PPropList;
  plen : Integer;
  i : Integer;
  sname : string;
  svalue  : string;
  v : Variant;
begin
  Result  :=  False;
  if not Assigned(AIntf) then Exit;
  if not Assigned(AObj) then Exit;
  if AIntf.QueryInterface(IID_IXMLNode, node) <> S_OK then Exit;
  node  :=  FindXMLNode(node, ANodePath);
  if not Assigned(node) then Exit;
  plen  :=  GetPropList(AObj, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].SetProc) then Continue;

      sname :=  string(plist[i].Name);
      v :=  node.Attributes[sname];
      if VarIsNull(v) then
      begin
        cnode :=  node.ChildNodes.FindNode(sname);
        if not Assigned(cnode) then Continue;
        v  :=  cnode.Text;
        if VarToStr(v) = '' then Continue;
      end;
      svalue  :=  VarToStr(v);
      try
        case plist[i].PropType^.Kind of
          tkString, tkLString, tkWString, tkUString :
            SetStrProp(AObj, plist[i], svalue);
          tkInteger :
            SetOrdProp(AObj, plist[i], StrToInt(svalue));
          tkEnumeration :
            SetEnumProp(AObj, plist[i], svalue);
        end;//}
      except
      end;
    end;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
  Result  :=  True;
end;

class function TCFXml.TryGetNodeValue(const ARoot: IXMLNode;
  const ANodePath: string; var AValue: OleVariant): boolean;
var
  node : IXMLNode;
begin
  node  :=  FindXMLNode(ARoot, ANodePath);
  Result  :=  Assigned(node);
  if Result then
    AValue  :=  node.NodeValue;
end;

class function TCFXml.ReadPropertiesFromXML(const AObj: TObject; const AXML,
  ANodePath: string): boolean;
//从XML文本读取对象属性
begin
  Result  :=  ReadPropertiesFromXMLNode(
      AObj,
      FindXMLNode(OpenXML(AXML).DocumentElement, ANodePath)
      );
end;

class function TCFXml.ReadPropertiesFromXMLFile(const AObj: TObject;
  const AFileName, ANodePath: string): Boolean;
//从XML文件读取对象属性
begin
  Result  :=  ReadPropertiesFromXMLNode(
      AObj,
      FindXMLNode(OpenFile(AFileName).DocumentElement, ANodePath)
      );
end;

class function TCFXml.WritePropertiesToXMLNode(const AObj: TObject;
  const AIntf: IInterface; const ANodePath : string): Boolean;
//把对象属性写入XMLNode
var
  node : IXMLNode;
  cnode : IXMLNode;
  plist : PPropList;
  plen : Integer;
  i : Integer;
  sname : string;
  svalue  : string;
begin
  Result  :=  False;
  if not Assigned(AIntf) then Exit;
  if not Assigned(AObj) then Exit;
  if AIntf.QueryInterface(IID_IXMLNode, node) <> S_OK then Exit;
  if ANodePath <> '' then
    node  :=  ForcePath(node, ANodePath);
  if not assigned(node) then Exit;

  plen  :=  GetPropList(AObj, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].GetProc) then Continue;

      sname :=  string(plist[i].Name);
      case plist[i].PropType^.Kind of
        tkString, tkLString, tkWString, tkUString :
          svalue  :=  GetStrProp(AObj, plist[i]);
        tkInteger :
          svalue  :=  IntToStr(GetOrdProp(AObj, plist[i]));
        tkEnumeration :
          svalue  :=  GetEnumProp(AObj, plist[i]);
        else
          svalue  :=  '';
      end;//}

      cnode :=  node.ChildNodes.FindNode(sname);
      if VarIsNull(node.Attributes[sname]) and
          Assigned(cnode) and not cnode.HasChildNodes then
        cnode.Text  :=  svalue
      else
        node.Attributes[sname]  :=  svalue;
    end;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
  Result  :=  True;
end;

class function TCFXml.WritePropertiesToXML(const AObj: TObject;
    const ANodePath: string; var AXML : string): Boolean;
//把对象属性写入XML文本
var
  doc : IXMLDocument;
  root : string;
  path : string;
begin
  path  :=  ANodePath;
  root := ExtractFirstNodeName(path);
  doc :=  CreateDoc(root);
  if Assigned(doc) then
  begin
    Result  :=  WritePropertiesToXMLNode(
        AObj,
        doc.DocumentElement, ANodePath
        );
    doc.SaveToXML(AXML);
  end
  else
    Result  :=  False;
end;

class function TCFXml.WritePropertiesToXMLFile(const AObj: TObject;
  const ANodePath, AFileName: string): Boolean;
//把对象属性写入XML文档
var
  xml : string;
begin
  Result  :=  WritePropertiesToXML(AObj, ANodePath, xml);
  if Result then
  try
    with TStringList.Create do
    try
      Text  :=  xml;
      SaveToFile(AFileName);
    finally
    end;
  except
    Result  :=  False;
  end;
end;

{ TCFXml2PropertiesAdapter }

function TCFXml2PropertiesAdapter.GetAdapteeClass: TClass;
begin
  Result  :=  TCFXml;
end;

function TCFXml2PropertiesAdapter.PropertiesReadFromXMLNode(
  const AIntf: IInterface; const ANodePath : string): Boolean;
begin
  Result  :=  TCFXml.ReadPropertiesFromXMLNode(Caller, AIntf, ANodePath);
end;

function TCFXml2PropertiesAdapter.PropertiesReadFromXML(const AXML,
  ANodePath: string): boolean;
begin
  Result  :=  TCFXml.ReadPropertiesFromXML(Caller, AXML, ANodePath);
end;

function TCFXml2PropertiesAdapter.PropertiesReadFromXMLFile(const AFileName,
  ANodePath: string): Boolean;
begin
  Result  :=  TCFXml.ReadPropertiesFromXMLFile(Caller, AFileName, ANodePath);
end;

{ TCFProperties2XMLAdapter }

function TCFProperties2XMLAdapter.GetAdapteeClass: TClass;
begin
  Result  :=  TCFXml;
end;

function TCFProperties2XMLAdapter.PropertiesWriteToXMLNode(const AIntf: IInterface;
  const ANodePath: string): Boolean;
begin
  Result  :=  TCFXml.WritePropertiesToXMLNode(Caller, AIntf, ANodePath);
end;

function TCFProperties2XMLAdapter.PropertiesWriteToXML(var AXML : string; const ANodePath: string): Boolean;
begin
  Result  :=  TCFXml.WritePropertiesToXML(Caller, ANodePath,  AXML);
end;

function TCFProperties2XMLAdapter.PropertiesWriteToXMLFile(const AFileName,
  ANodePath: string): Boolean;
begin
  Result  :=  TCFXml.WritePropertiesToXMLFile(Caller, AFileName, ANodePath);
end;

end.
