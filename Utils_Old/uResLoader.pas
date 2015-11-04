{===============================================================================
资源加载类,实现从各种类型的资源中加载对象属性
要加载的属性必须在published域并可写
本单元已实现了两个类型的适配器,文本文件及XML文件
//=============================================================================}
unit uResLoader;

interface

uses
  Classes, Sysutils, typinfo, Variants, XMLIntf, XMLDoc;

const
  GUID_ResLoader      : TGUID = '{DAAB825E-83DB-4E14-A42E-26FC302D765D}';
  GUID_ResLoadAdapter : TGUID = '{3CA3FFCF-C4E4-4B23-9525-769672E5DE0B}';

type
  IResLoader = interface(IInterface)
    ['{DAAB825E-83DB-4E14-A42E-26FC302D765D}']
    function LoadFromFile(const AFileName : string; const AType : string = ''; const AObj : TPersistent = nil):Boolean;
    function LoadFromStream(const AStream : TStream; const AType : string; const AObj : TPersistent = nil):Boolean;
  end;

  IResLoadAdapter = interface(IInterface)
    ['{3CA3FFCF-C4E4-4B23-9525-769672E5DE0B}']
    function LoadFromFile(const Obj : TPersistent; const AFileName : string):Boolean;
    function LoadFromStream(const Obj : TPersistent; const AStream : TStream):Boolean;
  end;

  TResLoader = class(TInterfacedPersistent, IResLoader)
  private
    //FResLoadAdapter: IResLoadAdapter;
  protected
    //property ResLoadAdapter : IResLoadAdapter read FResLoadAdapter implements IResLoadAdapter;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadFromFile(const AFileName : string; const AType : string = ''; const AObj : TPersistent = nil):Boolean;
    function LoadFromStream(const AStream : TStream; const AType : string; const AObj : TPersistent = nil):Boolean;
  published
    //属性列表
  end;

  TTextResLoadAdapter = class(TInterfacedObject, IResLoadAdapter)
  private
    procedure SetObjectProp(const AList : TStrings; const AObj : TPersistent);
  protected
    function LoadFromFile(const Obj : TPersistent; const AFileName : string):Boolean;
    function LoadFromStream(const Obj : TPersistent; const AStream : TStream):Boolean;
  public
  end;

  TXMLResLoadAdapter = class(TInterfacedObject, IResLoadAdapter)
  private
    procedure SetObjectProp(const ANode : IXMLNode; const AObj : TPersistent);
  protected
    function LoadFromFile(const Obj : TPersistent; const AFileName : string):Boolean;
    function LoadFromStream(const Obj : TPersistent; const AStream : TStream):Boolean;
  public
  end;

  TResLoadAdapterGroup = class
  private
    FClassList : TList;
  private
    procedure ClearClassList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function RegisterClass(const Adapter : TClass; const AType : string):Boolean;
    function UnRegisterClass(const AType : string):Boolean;
    function GetAdapterClass(const AType : string):TClass;
    function GetAdapterInterface(const AType : string):IResLoadAdapter;
  end;

var
  ResLoadAdapterGroup : TResLoadAdapterGroup;

implementation

type
  PResLoadAdapterClassInfo  = ^TResLoadAdapterClassInfo;
  TResLoadAdapterClassInfo = record
    RClass  : TClass;
    RType   : string;
  end;

{ TResLoader }

constructor TResLoader.Create;
begin
  inherited Create;
  //FResLoadAdapter  :=  CreateResLoadAdapter;
end;

destructor TResLoader.Destroy;
begin
  //FResLoadAdapter  :=  nil;
  inherited;
end;

function TResLoader.LoadFromFile(
    const AFileName : string;
    const AType : string = '';
    const AObj : TPersistent = nil
    ):Boolean;
var
  obj : TPersistent;
  Adapter : IResLoadAdapter;
  ext : string;
begin
  Result  :=  False;
  if AObj = nil then
    obj :=  Self
  else
    obj :=  AObj;
  if AType <> '' then
    ext :=  AType
  else begin
    ext :=  ExtractFileExt(AFileName);
    if (Length(ext) > 0) and (ext[1] = '.') then
      Delete(ext, 1, 1);
  end;
  Adapter :=  ResLoadAdapterGroup.GetAdapterInterface(ext);
  if Assigned(Adapter) then
  try
    Result  :=  Adapter.LoadFromFile(obj, AFileName);
  finally
    Adapter :=  nil;
  end;
end;

function TResLoader.LoadFromStream(
    const AStream : TStream;
    const AType: string;
    const AObj: TPersistent = nil
    ): Boolean;
var
  obj : TPersistent;
  Adapter : IResLoadAdapter;
begin
  Result  :=  False;
  if AObj = nil then
    obj :=  Self
  else
    obj :=  AObj;
  Adapter :=  ResLoadAdapterGroup.GetAdapterInterface(AType);
  if Assigned(Adapter) then
  try
    Result  :=  Adapter.LoadFromStream(obj, AStream);
  finally
    Adapter :=  nil;
  end;
end;

{ TTextResLoadAdapter }

//从文本文件加载,格式   属性=值
procedure TTextResLoadAdapter.SetObjectProp(const AList: TStrings;
  const AObj: TPersistent);
var
  plist : PPropList;
  Count : Integer;
  i : Integer;
  pinfo : PPropInfo;
  sv  : string;
begin
  Count :=  GetPropList(AObj, plist);
  if Count > 0 then
  try
    for i := 0 to Count - 1 do
    begin
      pinfo := plist^[I];
      if pinfo^.PropType^.Kind in [tkString, tkLString, tkWString, tkUString] then
      begin
        sv  :=  AList.Values[pinfo^.Name];
        if sv <> '' then
          if pinfo^.PropType^.Kind in [tkWString, tkUString] then
            SetWideStrProp(AObj, pinfo, sv)
          else
            SetStrProp(AObj, pinfo, sv);
      end;
    end;
  finally
    FreeMem(plist, Count * SizeOf(Pointer));
  end;
end;

function TTextResLoadAdapter.LoadFromFile(
    const Obj: TPersistent;
    const AFileName: string
    ): Boolean;
var
  lst : TStrings;
begin
  Result  :=  False;
  if FileExists(AFileName) then
  try
    lst :=  TStringList.Create;
    try
      lst.LoadFromFile(AFileName);
      SetObjectProp(lst, Obj);
      Result  :=  True;
    finally
      lst.Free;
    end;
  except
  end;
end;

function TTextResLoadAdapter.LoadFromStream(const Obj: TPersistent;
  const AStream: TStream): Boolean;
var
  lst : TStrings;
  s : TStringStream;
begin
  Result  :=  False;
  try
    AStream.Position  :=  0;
    s :=  TStringStream.Create('');
    try
      s.CopyFrom(AStream, AStream.Size);
      lst :=  TStringList.Create;
      try
        lst.Text  :=  s.DataString;
        SetObjectProp(lst, Obj);
        Result  :=  True;
      finally
        lst.Free;
      end;
    finally
      s.Free;
    end;
  except
  end;
end;

{ TXMLResLoadAdapter }

//从XML文件加载,格式 <root><属性>值</属性></root>
procedure TXMLResLoadAdapter.SetObjectProp(
    const ANode: IXMLNode;
    const AObj: TPersistent
    );
var
  plist : PPropList;
  Count : Integer;
  i : Integer;
  pinfo : PPropInfo;
  node  : IXMLNode;
  sv  : string;
begin
  Count :=  GetPropList(AObj, plist);
  if Count > 0 then
  try
    for i := 0 to Count - 1 do
    begin
      pinfo := plist^[I];
      if pinfo^.PropType^.Kind in [tkString, tkLString, tkWString] then
      begin
        node  :=  ANode.ChildNodes.FindNode(pinfo^.Name);
        if Assigned(Node) then
        begin
          sv  :=  VarToStrDef(Node.NodeValue, '');
          if sv <> '' then
            if pinfo^.PropType^.Kind = tkWString then
              SetWideStrProp(AObj, pinfo, sv)
            else
              SetStrProp(AObj, pinfo, sv);
          node  :=  nil;
        end;
      end;
    end;
  finally
    FreeMem(plist, Count * SizeOf(Pointer));
  end;
end;

function TXMLResLoadAdapter.LoadFromFile(
    const Obj: TPersistent;
    const AFileName: string
    ): Boolean;
var
  XML : IXMLDocument;
begin
  Result  :=  False;
  if FileExists(AFileName) then
  try
    XML :=  TXMLDocument.Create(nil);
    try
      XML.LoadFromFile(AFileName);
      XML.Active  :=  True;
      SetObjectProp(XML.DocumentElement, Obj);
    finally
      XML :=  nil;
    end;
    Result  :=  True;
  except
  end;
end;

function TXMLResLoadAdapter.LoadFromStream(const Obj: TPersistent;
  const AStream: TStream): Boolean;
var
  XML : IXMLDocument;
begin
  Result  :=  False;
  try
    AStream.Position  :=  0;
    XML :=  TXMLDocument.Create(nil);
    try
      XML.LoadFromStream(AStream);
      XML.Active  :=  True;
      SetObjectProp(XML.DocumentElement, Obj);
      Result  :=  True;
    finally
      XML :=  nil;
    end;
  except
  end;
end;

{ TResLoadAdapterGroup }

constructor TResLoadAdapterGroup.Create;
begin
  inherited;
  FClassList  :=  TList.Create;
end;

destructor TResLoadAdapterGroup.Destroy;
begin
  ClearClassList;
  FClassList.Free;
  inherited;
end;

procedure TResLoadAdapterGroup.ClearClassList;
var
  i : Integer;
begin
  for i := 0 to FClassList.Count - 1 do
    dispose(PResLoadAdapterClassInfo(FClassList[i]));
end;

function TResLoadAdapterGroup.RegisterClass(const Adapter: TClass;
  const AType: string): Boolean;
var
  p : PResLoadAdapterClassInfo;
  sType : string;
  InterfaceEntry: PInterfaceEntry;
begin
  Result  :=  False;
  InterfaceEntry  :=  Adapter.GetInterfaceEntry(GUID_ResLoadAdapter);
  if Assigned(InterfaceEntry) then
  begin
    sType   :=  LowerCase(AType);
    UnRegisterClass(sType);
    new(p);
    p.RClass  :=  Adapter;
    p.RType   :=  sType;
    FClassList.Add(p);
    Result  :=  True;
  end;
end;

function TResLoadAdapterGroup.UnRegisterClass(const AType: string): Boolean;
var
  i : Integer;
  sType : string;
begin
  Result  :=  False;
  sType   :=  LowerCase(AType);
  for i := 0 to FClassList.Count - 1 do
    if PResLoadAdapterClassInfo(FClassList[i]).RType = sType then
    begin
      dispose(PResLoadAdapterClassInfo(FClassList[i]));
      FClassList.Delete(i);
      break;
    end;
end;

function TResLoadAdapterGroup.GetAdapterClass(
  const AType: string): TClass;
var
  i : Integer;
  sType : string;
begin
  Result  :=  nil;
  sType :=  LowerCase(AType);
  for i := 0 to FClassList.Count - 1 do
    if PResLoadAdapterClassInfo(FClassList[i]).RType = sType then
      Result  :=  PResLoadAdapterClassInfo(FClassList[i]).RClass;
end;

function TResLoadAdapterGroup.GetAdapterInterface(
  const AType: string): IResLoadAdapter;
var
  cls : TClass;
  obj : TObject;
begin
  Result  :=  nil;
  cls :=  GetAdapterClass(AType);
  if Assigned(cls) then
  begin
    obj :=  cls.Create;
    obj.GetInterface(GUID_ResLoadAdapter, Result);
  end;
end;

initialization
  ResLoadAdapterGroup :=  TResLoadAdapterGroup.Create;
  ResLoadAdapterGroup.RegisterClass(TTextResLoadAdapter, 'txt');
  ResLoadAdapterGroup.RegisterClass(TXMLResLoadAdapter,  'xml');

finalization
  ResLoadAdapterGroup.Free;

end.
