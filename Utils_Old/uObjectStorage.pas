unit uObjectStorage;

interface

uses
  windows, SysUtils, Classes, uStorage, typinfo;

const
  GUID_StorageInterpreter: TGUID = '{43F64832-2DF3-46A0-8103-73143FB23B35}';

type
  //注意,使用RTTI存取的对象必须从TPersistent继承,需要存取的属性放在published域  
  TStorageInterpreterType = (siInterpreter, siInterface, siRTTI);
  TStorageInterpreterSet = set of TStorageInterpreterType;

  TObjectInterpreter = class;
  TObjectInterpreterClass = class of TObjectInterpreter;

  //实现接口的对象一般从TInterfacedObject继承,如果对象还要有RTTI,就从TInterfacedPersistent继承
  IStorageInterpreter = interface(IInterface)
    ['{43F64832-2DF3-46A0-8103-73143FB23B35}']
    procedure SaveObject(Storage  : TStorage; Section : string);
    procedure LoadObject(Storage  : TStorage; Section : string);
  end;

  TObjectStorage  = class
  private
    FStorage  : TStorage;
    FInterpreterList  : TList;
    FOwnerStorage: Boolean;
  public
    property Storage  : TStorage read FStorage write FStorage;
    property OwnerStorage : Boolean read FOwnerStorage write FOwnerStorage;
  public
    constructor Create;
    destructor Destroy; override;

    function  SaveObject(Obj  : TObject; Section  : string; InterpreterSet : TStorageInterpreterSet = [siInterpreter]) : boolean;
    function  LoadObject(Obj  : TObject; Section  : string; InterpreterSet : TStorageInterpreterSet = [siInterpreter]) : boolean;

    procedure RegisterInterpreter(Interpreter : TObjectInterpreterClass);
    procedure UnregisterInterpreter(Interpreter : TObjectInterpreterClass);
  end;

  TObjectInterpreter = class
  public
    class function  SupportClass(ObjClass : TClass) : boolean; virtual; abstract;
    class procedure SaveObject(Obj  : TObject; Storage  : TStorage; Section : string); virtual; abstract;
    class procedure LoadObject(Obj  : TObject; Storage  : TStorage; Section : string); virtual; abstract;
  end;

function ObjectStorage  : TObjectStorage;

implementation

var
  G_ObjectStorage : TObjectStorage;

function ObjectStorage  : TObjectStorage;
begin
  if not assigned(G_ObjectStorage) then
    G_ObjectStorage :=  TObjectStorage.Create;
  result  :=  G_ObjectStorage;
end;

{ TObjectStorage }

constructor TObjectStorage.Create;
begin
  FInterpreterList  :=  TList.Create;
end;

destructor TObjectStorage.Destroy;
begin
  FInterpreterList.Free;
  if OwnerStorage and Assigned(Storage) then
    Storage.Free;
  inherited;
end;

function TObjectStorage.SaveObject(Obj: TObject; Section: string;
    InterpreterSet : TStorageInterpreterSet) : boolean;

  function InterpreterSave : Boolean;
  var
    i : Integer;
  begin
    Result  :=  False;
    for i := FInterpreterList.Count - 1 downto 0 do
    begin
      if TObjectInterpreterClass(FInterpreterList[i]).SupportClass(Obj.ClassType) then
      begin
        TObjectInterpreterClass(FInterpreterList[i]).SaveObject(Obj, Storage, Section);
        result  :=  true;
        break;
      end;
    end;
  end;

  function InterfaceSave : Boolean;
  var
    intf  : IStorageInterpreter;
  begin
    Result  :=  False;
    if obj.GetInterface(GUID_StorageInterpreter, intf) then
    try
      intf.SaveObject(Storage, Section);
      Result  :=  True;
    finally
      intf  :=  nil;
    end;
  end;

  function RTTISave : Boolean;
  var
    plist : PPropList;
    Count : Integer;
    i : Integer;
    pinfo : PPropInfo;
  begin
    Result  :=  False;
    Count :=  GetPropList(obj, plist);
    if Count > 0 then
    try
      for i := 0 to Count - 1 do
      begin
        pinfo := plist^[I];
        {$WARN IMPLICIT_STRING_CAST OFF}
        case pinfo^.PropType^.Kind of
          tkInteger, tkChar, tkWChar:
            Storage.Write(pinfo^.Name, GetOrdProp(obj, pinfo));
          tkInt64 :
            Storage.Write(pinfo^.Name, GetInt64Prop(obj, pinfo));
          tkFloat   :
            Storage.Write(pinfo^.Name, GetFloatProp(obj, pinfo));
          tkString, tkLString:
            Storage.Write(pinfo^.Name, GetStrProp(obj, pinfo));
          tkWString, tkUString:
            Storage.Write(pinfo^.Name, GetWideStrProp(obj, pinfo));
          tkSet :
            Storage.Write(pinfo^.Name, GetSetProp(obj, pinfo));
          tkEnumeration :
            Storage.Write(pinfo^.Name, GetEnumProp(obj, pinfo));
        end;
        {$WARN IMPLICIT_STRING_CAST ON}
      end;
      Result  :=  True;
    finally
      FreeMem(plist, Count * SizeOf(Pointer));
    end;
  end;

begin
  result  :=  false;
  if not Assigned(Storage) then exit;
  Storage.Section  :=  Section;
  if InterpreterSet = [] then
    Result  :=  InterpreterSave or InterfaceSave or RTTISave
  else begin
    Result  :=  True;
    if siRTTI in InterpreterSet then
      Result  :=  Result and RTTISave;
    if siInterface in InterpreterSet then
      Result  :=  Result and InterfaceSave;
    if siInterpreter in InterpreterSet then
      Result  :=  Result and InterpreterSave;
  end;
end;

function TObjectStorage.LoadObject(Obj: TObject; Section: string;
    InterpreterSet : TStorageInterpreterSet) : boolean;

  function InterpreterLoad : Boolean;
  var
    i : integer;
  begin
    Result  :=  False;
    for i := FInterpreterList.Count - 1 downto 0 do
    begin
      if TObjectInterpreterClass(FInterpreterList[i]).SupportClass(Obj.ClassType) then
      begin
        TObjectInterpreterClass(FInterpreterList[i]).LoadObject(Obj, Storage, Section);
        result  :=  true;
        break;
      end;
    end;
  end;

  function InterfaceLoad : Boolean;
  var
    intf  : IStorageInterpreter;
  begin
    Result  :=  False;
    if obj.GetInterface(GUID_StorageInterpreter, intf) then
    try
      intf.LoadObject(Storage, Section);
      Result  :=  True;
    finally
      intf  :=  nil;
    end;
  end;

  function RTTILoad : Boolean;
  var
    plist : PPropList;
    Count : Integer;
    i : Integer;
    pinfo : PPropInfo;
  begin
    Result  :=  False;
    Count :=  GetPropList(obj, plist);
    if Count > 0 then
    try
      for i := 0 to Count - 1 do
      try
        pinfo := plist^[I];
        if not Assigned(pinfo^.SetProc) then
          Continue;
        {$WARN IMPLICIT_STRING_CAST OFF}
        case pinfo^.PropType^.Kind of
          tkInteger, tkChar, tkWChar:
            SetOrdProp(obj, pinfo, Storage.ReadInteger(pinfo^.Name));
          tkInt64 :
            SetInt64Prop(obj, pinfo, Storage.ReadInt64(pinfo^.Name));
          tkFloat   :
            SetFloatProp(obj, pinfo, Storage.ReadFloat(pinfo^.Name));
          tkString, tkLString :
            SetStrProp(obj, pinfo, Storage.ReadString(pinfo^.Name));
          tkWString, tkUString:
            SetWideStrProp(obj, pinfo, Storage.ReadString(pinfo^.Name));
          tkSet :
            SetSetProp(obj, pinfo, Storage.ReadString(pinfo^.Name));
          tkEnumeration :
            SetEnumProp(obj, pinfo, Storage.ReadString(pinfo^.Name));
        end;
        {$WARN IMPLICIT_STRING_CAST ON}
      except
      end;
      Result  :=  True;
    finally
      FreeMem(plist, Count * SizeOf(Pointer));
    end;
  end;

begin
  result  :=  false;
  if not Assigned(Storage) then exit;
  Storage.Section  :=  Section;
  if InterpreterSet = [] then
    Result  :=  InterpreterLoad or InterfaceLoad or RTTILoad
  else begin
    Result  :=  True;
    if siRTTI in InterpreterSet then
      Result  :=  Result and RTTILoad;
    if siInterface in InterpreterSet then
      Result  :=  Result and InterfaceLoad;
    if siInterpreter in InterpreterSet then
      Result  :=  Result and InterpreterLoad;
  end;
end;

procedure TObjectStorage.RegisterInterpreter(
  Interpreter: TObjectInterpreterClass);
begin
  if FInterpreterList.IndexOf(Interpreter) = -1 then
    FInterpreterList.Add(Interpreter);
end;

procedure TObjectStorage.UnregisterInterpreter(
  Interpreter: TObjectInterpreterClass);
var
  i : integer;
begin
  i :=  FInterpreterList.IndexOf(Interpreter);
  if i >= 0 then
    FInterpreterList.Delete(i);
end;

initialization

finalization
  if Assigned(G_ObjectStorage) then
    G_ObjectStorage.Free;

end.
