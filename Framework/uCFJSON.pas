//Common Framework
//JSON操作单元
{$I CommonFramework.inc}
unit uCFJSON;

interface

uses
  SysUtils, Classes, typinfo, Variants, StrUtils, Rtti,
  uCFIntfDef, uCFClasses;

type
  //JSON解析类
  TCFJSON = class(TCFObject)
  public
    class function  ReadPropertiesFromJSONObject(const AObj : TObject; const AIntf : IInterface; const APath : string = '') : Boolean; static;
    class function  ReadPropertiesFromJSON(const AObj : TObject; const AJSON : string; const APath : string) : boolean; static;
    class function  ReadPropertiesFromJSONFile(const AObj : TObject; const AFileName : string; const APath : string) : Boolean; static;
    class function  WritePropertiesToJSONObject(const AObj : TObject; const AIntf : IInterface; const APath : string) : Boolean; static;
    class function  WritePropertiesToJSON(const AObj : TObject; const APath : string; var AJSON : string) : Boolean; static;
    class function  WritePropertiesToJSONFile(const AObj : TObject; const APath : string; const AFileName : string) : Boolean; static;
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

type
  TSuperRttiContextEx = class(TSuperRttiContext)
  private
    class function GetFieldName(r: TRttiNamedObject): string;
    class function GetFieldDefault(r: TRttiNamedObject; const obj: ISuperObject): ISuperObject;
  public
    function FromJson(TypeInfo: PTypeInfo; const obj: ISuperObject; var Value: TValue): Boolean; override;
    function ToJson(var value: TValue; const index: ISuperObject): ISuperObject; override;
  end;

{ TCFJSON }

class function TCFJSON.ReadPropertiesFromJSONObject(const AObj: TObject; const AIntf: IInterface;
  const APath: string): Boolean;
var
  iso : ISuperObject;
  ctx : TSuperRttiContextEx;
  v: TValue;
begin
  Result  :=  False;
  if not Assigned(AObj) then Exit;
  if not Assigned(AIntf) then Exit;
  if AIntf.QueryInterface(ISuperObject, iso) <> S_OK then Exit;
  try
    ctx := TSuperRttiContextEx.Create;
    try
      v := AObj;
      if APath <> '' then
        iso :=  iso[APath];
      Result  :=  ctx.FromJson(v.TypeInfo, iso, v);
    finally
      ctx.Free;
    end;

    Result  :=  True;
  except on E: Exception do
  end;
end;

class function TCFJSON.ReadPropertiesFromJSON(const AObj: TObject; const AJSON, APath: string): boolean;
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

class function TCFJSON.ReadPropertiesFromJSONFile(const AObj: TObject; const AFileName, APath: string): Boolean;
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

class function TCFJSON.WritePropertiesToJSONObject(const AObj: TObject; const AIntf: IInterface;
  const APath: string): Boolean;
var
  iso : ISuperObject;
  iobj : ISuperObject;
  v: TValue;
  ctx: TSuperRttiContext;
begin
  Result  :=  False;
  if not Assigned(AIntf) then Exit;
  if not Assigned(AObj) then Exit;
  if AIntf.QueryInterface(ISuperObject, iso) <> S_OK then Exit;

  ctx := TSuperRttiContextEx.Create;
  try
    v := AObj;
    try
      iobj  :=  ctx.ToJson(v, SO);
      if APath <> '' then
        iso[APath]  :=  iobj
      else begin
        iso.Clear(true);
        iso.Merge(iobj, True);
      end;
      Exit(True);
    except
    end;
  finally
    ctx.Free;
  end;
end;

class function TCFJSON.WritePropertiesToJSON(const AObj: TObject; const APath: string; var AJSON: string): Boolean;
var
  iso : ISuperObject;
begin
  iso :=  TSuperObject.Create();
  Result  :=  WritePropertiesToJSONObject(AObj, iso, APath);
  if Result then
    AJSON :=  iso.AsJSon();
end;

class function TCFJSON.WritePropertiesToJSONFile(const AObj: TObject; const APath, AFileName: string): Boolean;
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
  Result  :=  TCFJSON.ReadPropertiesFromJSON(Caller, AJSON, APath);
end;

function TCFJSON2PropertiesAdapter.PropertiesReadFromJSONFile(const AFileName, APath: string): Boolean;
begin
  Result  :=  TCFJSON.ReadPropertiesFromJSONFile(Caller, AFileName, APath);
end;

function TCFJSON2PropertiesAdapter.PropertiesReadFromJSONObject(const AIntf: IInterface; const APath: string): Boolean;
begin
  Result  :=  TCFJSON.ReadPropertiesFromJSONObject(Caller, AIntf, APath);
end;

{ TCFProperties2JSONAdapter }

function TCFProperties2JSONAdapter.GetAdapteeClass: TClass;
begin
  Result  :=  TCFJSON;
end;

function TCFProperties2JSONAdapter.PropertiesWriteToJSON(var AJSON: string; const APath: string): Boolean;
begin
  Result  :=  TCFJSON.WritePropertiesToJSON(Caller, APath, AJSON);
end;

function TCFProperties2JSONAdapter.PropertiesWriteToJSONFile(const AFileName, APath: string): Boolean;
begin
  Result  :=  TCFJSON.WritePropertiesToJSONFile(Caller, APath, AFileName);
end;

function TCFProperties2JSONAdapter.PropertiesWriteToJSONObject(const AIntf: IInterface; const APath: string): Boolean;
begin
  Result  :=  TCFJSON.WritePropertiesToJSONObject(Caller, AIntf, APath);
end;

{ TSuperRttiContextEx }

class function TSuperRttiContextEx.GetFieldName(r: TRttiNamedObject): string;
var
  o: TCustomAttribute;
begin
  for o in r.GetAttributes do
    if o is SOName then
      Exit(SOName(o).Name);
  Result := r.Name;
end;

class function TSuperRttiContextEx.GetFieldDefault(r: TRttiNamedObject; const obj: ISuperObject): ISuperObject;
var
  o: TCustomAttribute;
begin
  if not ObjectIsType(obj, stNull) then Exit(obj);
  for o in r.GetAttributes do
    if o is SODefault then
      Exit(SO(SODefault(o).Name));
  Result := obj;
end;

function TSuperRttiContextEx.FromJson(TypeInfo: PTypeInfo; const obj: ISuperObject; var Value: TValue): Boolean;

  procedure FromClass;
  var
    p: TRttiProperty;
    v: TValue;
    n : string;
  begin
    case ObjectGetType(obj) of
      stObject:
        begin
          Result := True;
          if Value.Kind <> tkClass then
            Value := GetTypeData(TypeInfo).ClassType.Create;
          for p in Context.GetType(Value.AsObject.ClassType).GetProperties do
            if p.PropertyType <> nil then
            begin
              v := p.GetValue(Value.AsObject);
              n :=  GetFieldName(p);
              //只写published属性
              if not Assigned(GetPropInfo(Value.AsObject, n)) then
                Continue;
              Result := FromJson(p.PropertyType.Handle, GetFieldDefault(p, obj.AsObject[n]), v);
              if Result then
              begin
                if p.IsWritable then
                  p.SetValue(Value.AsObject, v);
              end;
            end;
        end;
      stNull:
        begin
          Value := nil;
          Result := True;
        end
    else
      // error
      Value := nil;
      Result := False;
    end;
  end;

begin
  if TypeInfo <> nil then
  begin
    if TypeInfo.Kind = tkClass then
      FromClass
    else
      Result  :=  inherited FromJson(TypeInfo, obj, Value);
  end else
    Result := False;
end;

function TSuperRttiContextEx.ToJson(var value: TValue; const index: ISuperObject): ISuperObject;

  procedure ToClass;
  var
    o: ISuperObject;
    p: TRttiProperty;
    v: TValue;
    n : string;
  begin
    if TValueData(Value).FAsObject <> nil then
    begin
      o := index[IntToStr(NativeInt(Value.AsObject))];
      if o = nil then
      begin
        Result := TSuperObject.Create(stObject);
        index[IntToStr(NativeInt(Value.AsObject))] := Result;
        for p in Context.GetType(Value.AsObject.ClassType).GetProperties do
          if p.PropertyType <> nil then
          begin
            v := p.GetValue(Value.AsObject);
            n :=  GetFieldName(p);
            //只写published属性
            if Assigned(GetPropInfo(Value.AsObject, n)) then
              Result.AsObject[n] := ToJson(v, index);
          end;
      end else
        Result := o;
    end else
      Result := nil;
  end;

begin
  if Value.Kind = tkClass then
    ToClass
  else
    Result  :=  inherited ToJson(Value, Index);
end;

end.
