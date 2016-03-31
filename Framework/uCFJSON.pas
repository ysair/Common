//Common Framework
//JSON操作单元
{$I CommonFramework.inc}
unit uCFJSON;

interface

uses
  SysUtils, Classes, typinfo, Variants, StrUtils,
  uCFIntfDef, uCFClasses;

type
  //XML解析类
  TCFJSON = class(TCFObject)
  public
    class function  ReadPropertiesFromJSONObject(const AObj : TPersistent; const AIntf : IInterface; const APath : string = '') : Boolean; static;
    class function  ReadPropertiesFromJSON(const AObj : TPersistent; const AJSON : string; const APath : string) : boolean; static;
    class function  ReadPropertiesFromJSONFile(const AObj : TPersistent; const AFileName : string; const APath : string) : Boolean; static;
    class function  WritePropertiesToJSONObject(const AObj : TPersistent; const AIntf : IInterface; const APath : string) : Boolean; static;
    class function  WritePropertiesToJSON(const AObj : TPersistent; const APath : string; var AJSON : string) : Boolean; static;
    class function  WritePropertiesToJSONFile(const AObj : TPersistent; const APath : string; const AFileName : string) : Boolean; static;
  end;

  //从JSON读取对象属性适配器
  TCFJSON2PropertiesAdapter = class(TCFAdapter, ICFPropertiesJSONReader, ICFPropertiesJSONTextReader, ICFPropertiesJSONFileReader)
  protected
    function  GetAdapteeClass : TClass; override;
    function  PropertiesReadFromJSONObject(const AIntf : IInterface; const APath : string) : Boolean;
    function  PropertiesReadFromJSON(const AJSON : string; const APath : string) : boolean;
    function  PropertiesReadFromJSONFile(const AFileName : string; const APath : string) : Boolean;
  end;//}

  //将对象属性写入JSON的适配器
  TCFProperties2JSONAdapter = class(TCFAdapter, ICFPropertiesJSONWriter, ICFPropertiesJSONTextWriter, ICFPropertiesJSONFileWriter)
  protected
    function  GetAdapteeClass : TClass; override;
    function  PropertiesWriteToJSONObject(const AIntf : IInterface; const APath : string) : Boolean;
    function  PropertiesWriteToJSON(var AJSON : string; const APath : string) : Boolean;
    function  PropertiesWriteToJSONFile(const AFileName : string; const APath : string) : Boolean;
  end;//}

implementation

uses
  superobject;

{ TCFJSON }

class function TCFJSON.ReadPropertiesFromJSONObject(const AObj: TPersistent; const AIntf: IInterface;
  const APath: string): Boolean;
var
  iso : ISuperObject;
  plist : PPropList;
  plen : Integer;
  ip : ISuperObject;
  i : Integer;
  sname : string;
begin
  Result  :=  False;
  if not Assigned(AObj) then Exit;
  if not Assigned(AIntf) then Exit;
  if AIntf.QueryInterface(ISuperObject, iso) <> S_OK then Exit;
  try
    if APath <> '' then
    begin
      iso :=  iso[APath];
      if not Assigned(iso) then Exit;
    end;
    if not iso.IsType(stObject) then Exit;

    plen  :=  GetPropList(AObj, plist);
    if plen > 0 then
    try
      for i := 0 to plen - 1 do
      begin
        if not Assigned(plist[i].SetProc) then Continue;

        sname :=  string(plist[i].Name);
        ip  :=  iso.O[sname];
        if not Assigned(ip) then
          Continue;

        try
          case plist[i].PropType^.Kind of
            tkString, tkLString, tkWString, tkUString :
              SetStrProp(AObj, plist[i], ip.AsString);
            tkInteger :
              SetOrdProp(AObj, plist[i], ip.AsInteger);
            tkInt64 :
              SetInt64Prop(AObj, plist[i], ip.AsInteger);
            tkEnumeration :
              SetEnumProp(AObj, plist[i], ip.AsString);
            tkFloat :
              SetFloatProp(AObj, plist[i], ip.AsDouble);
          end;//}
        except
        end;
      end;
    finally
      FreeMem(plist, plen * SizeOf(Pointer));
    end;
    Result  :=  True;
  except on E: Exception do
  end;
end;

class function TCFJSON.ReadPropertiesFromJSON(const AObj: TPersistent; const AJSON, APath: string): boolean;
var
  iso : ISuperObject;
begin
  try
    iso :=  TSuperObject.ParseString(PWideChar(AJSON), False);
    Result  :=  ReadPropertiesFromJSONObject(AObj, iso, APath);
  except on E: Exception do
    Result  :=  False;
  end;
end;

class function TCFJSON.ReadPropertiesFromJSONFile(const AObj: TPersistent; const AFileName, APath: string): Boolean;
var
  iso : ISuperObject;
begin
  try
    iso :=  TSuperObject.ParseFile(AFileName, False);
    Result  :=  ReadPropertiesFromJSONObject(AObj, iso, APath);
  except on E: Exception do
    Result  :=  False;
  end;
end;

class function TCFJSON.WritePropertiesToJSONObject(const AObj: TPersistent; const AIntf: IInterface;
  const APath: string): Boolean;
var
  iso : ISuperObject;
  iobj : ISuperObject;
  plist : PPropList;
  plen : Integer;
  i : Integer;
  sname : string;
begin
  Result  :=  False;
  if not Assigned(AIntf) then Exit;
  if not Assigned(AObj) then Exit;
  if AIntf.QueryInterface(ISuperObject, iso) <> S_OK then Exit;
  if APath = '' then
    iobj  :=  iso
  else
    iobj  :=  TSuperObject.Create();

  plen  :=  GetPropList(AObj, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].GetProc) then Continue;

      sname :=  string(plist[i].Name);
      case plist[i].PropType^.Kind of
        tkString, tkLString, tkWString, tkUString :
          iobj.S[sname] :=  GetStrProp(AObj, plist[i]);
        tkInteger :
          iobj.I[sname] :=  GetOrdProp(AObj, plist[i]);
        tkEnumeration :
          iobj.S[sname] :=  GetEnumProp(AObj, plist[i]);
        tkFloat :
          iobj.D[sname] :=  GetFloatProp(AObj, plist[i]);
        tkInt64 :
          iobj.I[sname] :=  GetInt64Prop(AObj, plist[i]);
      end;//}
    end;
    if APath <> '' then
      iso[APath]  :=  iobj;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
  Result  :=  True;
end;

class function TCFJSON.WritePropertiesToJSON(const AObj: TPersistent; const APath: string; var AJSON: string): Boolean;
var
  iso : ISuperObject;
begin
  iso :=  TSuperObject.Create();
  Result  :=  WritePropertiesToJSONObject(AObj, iso, APath);
  if Result then
    AJSON :=  iso.AsJSon();
end;

class function TCFJSON.WritePropertiesToJSONFile(const AObj: TPersistent; const APath, AFileName: string): Boolean;
var
  iso : ISuperObject;
begin
  iso :=  TSuperObject.Create();
  Result  :=  WritePropertiesToJSONObject(AObj, iso, APath);
  if Result then
  try
    iso.SaveTo(AFileName);
  except
    Result  :=  False;
  end;
end;

{ TCFJSON2PropertiesAdapter }

function TCFJSON2PropertiesAdapter.GetAdapteeClass: TClass;
begin
  Result  :=  TCFJSON;
end;

function TCFJSON2PropertiesAdapter.PropertiesReadFromJSON(const AJSON, APath: string): boolean;
begin
  if Caller is TPersistent then
    Result  :=  TCFJSON.ReadPropertiesFromJSON(TPersistent(Caller), AJSON, APath)
  else
    Result  :=  False;
end;

function TCFJSON2PropertiesAdapter.PropertiesReadFromJSONFile(const AFileName, APath: string): Boolean;
begin
  if Caller is TPersistent then
    Result  :=  TCFJSON.ReadPropertiesFromJSONFile(TPersistent(Caller), AFileName, APath)
  else
    Result  :=  False;
end;

function TCFJSON2PropertiesAdapter.PropertiesReadFromJSONObject(const AIntf: IInterface; const APath: string): Boolean;
begin
  if Caller is TPersistent then
    Result  :=  TCFJSON.ReadPropertiesFromJSONObject(TPersistent(Caller), AIntf, APath)
  else
    Result  :=  False;
end;

{ TCFProperties2JSONAdapter }

function TCFProperties2JSONAdapter.GetAdapteeClass: TClass;
begin
  Result  :=  TCFJSON;
end;

function TCFProperties2JSONAdapter.PropertiesWriteToJSON(var AJSON: string; const APath: string): Boolean;
begin
  if Caller is TPersistent then
    Result  :=  TCFJSON.WritePropertiesToJSON(TPersistent(Caller), APath, AJSON)
  else
    Result  :=  False;
end;

function TCFProperties2JSONAdapter.PropertiesWriteToJSONFile(const AFileName, APath: string): Boolean;
begin
  if Caller is TPersistent then
    Result  :=  TCFJSON.WritePropertiesToJSONFile(TPersistent(Caller), APath, AFileName)
  else
    Result  :=  False;
end;

function TCFProperties2JSONAdapter.PropertiesWriteToJSONObject(const AIntf: IInterface; const APath: string): Boolean;
begin
  if Caller is TPersistent then
    Result  :=  TCFJSON.WritePropertiesToJSONObject(TPersistent(Caller), AIntf, APath)
  else
    Result  :=  False;
end;

end.
