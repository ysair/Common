unit fcRTTI;
{$I CommonFramework.inc}

interface

uses
  SysUtils, TypInfo, Variants, Controls
{$IFDEF FRAMEWORK_INCLUDE_XML}, XMLIntf {$ENDIF}
{$IFDEF FRAMEWORK_INCLUDE_DB}, DB {$ENDIF}
  ;

type
  Rtti = record
    class function SetPropValue(const AObj: TObject; const APropInfo: PPropInfo;
      const AValue: Variant): Boolean; overload; static;
    class function SetPropValue(const AObj: TObject; const APropName: string;
      const AValue: Variant): Boolean; overload; static;
    class function GetPropValue(const AObj: TObject; const APropName: string): Variant; static;
    class function GetIntPropValue(const AObj: TObject; const APropName: string): Integer; static;
    class procedure SetControlValue(const AControl: TControl; const AValue: Variant); static;
    class procedure GetControlValue(const AObj: TObject; const AControl: TControl;
      const APropInfo: PPropInfo); static;
    class function  PropertyExists(const AObj : TObject; const APropName : string):Boolean; static;
  end;

  TObjectHelper = class Helper for TObject
  private
    function PropertyReadFromControl(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyWriteToControl(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyIsModifiedInControl(const AControl: TControl;
      const APropInfo: PPropInfo): Boolean;
  public
    procedure PropertiesCopyFrom(const AObj: TObject);overload;
    procedure PropertiesCopyFrom(const AObj: TObject; const AExcept : array of string);overload;
    function PropertiesReadFromVariant(const AValue: Variant): Boolean;
    function PropertiesWriteToVariant: Variant;
    function PropertiesReadFromControls(const AControl: TWinControl): Boolean;
    function PropertiesWriteToControls(const AControl: TWinControl): Boolean;
    function PropertiesIsModifiedInControls(const AControl: TWinControl): Boolean;
    function PropertiesEqual(const AObj : TObject) : Boolean;
    function PropertiesCheckChangedStr(const AObj : TObject) : string;  //把改变的内容输出为字符串标识，如 Name:  abc -> def; 返回是多行的字符串
{$IFDEF FRAMEWORK_INCLUDE_XML}
    function PropertiesReadFromXMLNode(const ANode: IXMLNode): Boolean; overload;
    function PropertiesWriteToXMLNode(const ANode: IXMLNode): Boolean;overload;
    function PropertiesReadFromXMLNode(const ANode: IXMLNode; const AIgnoreProperties: array of string): Boolean;overload;
    function PropertiesWriteToXMLNode(const ANode: IXMLNode; const AIgnoreProperties: array of string): Boolean;overload;
{$ENDIF}
{$IFDEF FRAMEWORK_INCLUDE_DB}
    function PropertiesReadFromDataSet(const ADataSet: TDataSet; const AFieldArray: array of string): Boolean; overload;
    function PropertiesReadFromDataSet(const ADataSet: TDataSet): Boolean; overload;
    function PropertiesWriteToDataSet(const ADataSet: TDataSet; const AFieldArray: array of string): Boolean; overload;
    function PropertiesWriteToDataSet(const ADataSet: TDataSet): Boolean; overload;

{$ENDIF}
  end;

implementation

uses
  fcStr;

const
  ControlRttiPropList: array [0 .. 4] of string = ('Value', 'Date', 'ItemIndex', 'Checked', 'Text');

  { Rtti }

class procedure Rtti.GetControlValue(const AObj: TObject; const AControl: TControl;
  const APropInfo: PPropInfo);
var
  v: Variant;
  i: Integer;
begin
  try
    VarClear(v);
    for i := Low(ControlRttiPropList) to High(ControlRttiPropList) do
      if IsPublishedProp(AControl, ControlRttiPropList[i]) then
      begin
        v := GetPropValue(AControl, ControlRttiPropList[i]);
        Break;
      end;
    if not VarIsClear(v) then
      SetPropValue(AObj, APropInfo, v);
  except
  end;
end;

class function Rtti.GetIntPropValue(const AObj: TObject; const APropName: string): Integer;
var
  pi: PPropInfo;
begin
  pi := GetPropInfo(AObj, APropName);
  if Assigned(pi) then
    Result := TypInfo.GetOrdProp(AObj, pi)
  else
    Result := 0;
end;

class function Rtti.GetPropValue(const AObj: TObject; const APropName: string): Variant;
var
  pi: PPropInfo;
begin
  pi := GetPropInfo(AObj, APropName);
  if Assigned(pi) then
    Result := TypInfo.GetPropValue(AObj, pi)
  else
    Result := NULL;
end;

class function Rtti.PropertyExists(const AObj: TObject;
  const APropName: string): Boolean;
begin
  Result  :=  Assigned(GetPropInfo(AObj, APropName));
end;

class procedure Rtti.SetControlValue(const AControl: TControl; const AValue: Variant);
var
  i: Integer;
begin
  try
    for i := Low(ControlRttiPropList) to High(ControlRttiPropList) do
      if SetPropValue(AControl, GetPropInfo(AControl, ControlRttiPropList[i]), AValue) then
        Break;
  except
  end;
end;

class function Rtti.SetPropValue(const AObj: TObject; const APropName: string;
  const AValue: Variant): Boolean;
var
  pi: PPropInfo;
begin
  pi := GetPropInfo(AObj, APropName);
  if Assigned(pi) then
    Result := SetPropValue(AObj, pi, AValue)
  else
    Result := False;
end;

class function Rtti.SetPropValue(const AObj: TObject; const APropInfo: PPropInfo;
  const AValue: Variant): Boolean;
var
  s: string;
begin
  Result := False;
  if Assigned(APropInfo) and Assigned(APropInfo.SetProc) then
  try
    case APropInfo.PropType^.Kind of
      tkInteger:
        if VarIsNumeric(AValue) then
          SetOrdProp(AObj, APropInfo, AValue)
        else
          SetOrdProp(AObj, APropInfo, 0);
      tkInt64:
        if VarIsNumeric(AValue) then
          SetInt64Prop(AObj, APropInfo, AValue)
        else
          SetInt64Prop(AObj, APropInfo, 0);
      tkFloat:
          SetFloatProp(AObj, APropInfo, AValue) ;
      tkEnumeration:
        if VarIsNull(AValue) then
          SetOrdProp(AObj, APropInfo, 0)
        else
          SetOrdProp(AObj, APropInfo, Integer(AValue));
      tkChar, tkWChar:
        begin
          s := VarToStr(AValue);
          if s = '' then
            s := #0;
          SetOrdProp(AObj, APropInfo, Ord(s[1]));
        end;
      tkSet:
        begin
          s := VarToStr(AValue);
          SetSetProp(AObj, APropInfo, s);
        end
    else
      TypInfo.SetPropValue(AObj, APropInfo, AValue);
    end;
    Result := True;
  except
  end;
end;

{ TObjectHelper }
procedure TObjectHelper.PropertiesCopyFrom(const AObj: TObject;
  const AExcept: array of string);
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  sValue: string;
  iValue: Int64;
  sName: string;
  dValue: Double;
  pInfo: PPropInfo;
begin
  if not Assigned(AObj) then
    Exit;

  dValue := 0;
  // 获取所有published的properties
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      sName := string(plist[i].Name);
      if Arr.PosInStrArray(sName, AExcept, False) >= 0 then
        Continue;

      // 如果无法赋值，忽略
      if not Assigned(plist[i].SetProc) then
        Continue;

      // 查看源对象中是否有相应的property
      pInfo := GetPropInfo(AObj, sName);
      if not Assigned(pInfo) then
        Continue;

      //根据类型赋值
      case plist[i].PropType^.Kind of
        tkString, tkUString:
          begin
            sValue := GetStrProp(AObj, pInfo);
            SetStrProp(Self, plist[i], sValue);
          end;
        tkInteger, tkInt64, tkEnumeration, tkChar, tkWChar, tkPointer:
          begin
            case pInfo.PropType^.Kind of
              tkInt64:
                iValue := GetInt64Prop(AObj, pInfo);
              tkInteger, tkEnumeration, tkChar, tkWChar, tkPointer:
                iValue := GetOrdProp(AObj, pInfo);
              tkString, tkUString:
                iValue := StrToIntDef(GetStrProp(AObj, pInfo), 0);
            else
              Continue;
            end;

            if plist[i].PropType^.Kind = tkInt64 then
              SetInt64Prop(Self, plist[i], iValue)
            else
              SetOrdProp(Self, plist[i], Int64Rec(iValue).Lo);
          end;
        tkFloat:
          begin
            case pInfo.PropType^.Kind of
              tkInteger, tkEnumeration, tkChar, tkWChar:
                dValue := GetOrdProp(AObj, pInfo);
              tkString, tkUString:
                dValue := StrToFloatDef(GetStrProp(AObj, pInfo), 0);
              tkFloat:
                dValue := GetFloatProp(AObj, pInfo);
            else
              Continue;
            end;

            SetFloatProp(Self, plist[i], dValue);
          end;
        tkMethod  :
          SetMethodProp(Self, plist[i], GetMethodProp(AObj, pInfo));
        tkSet :
          begin
            sValue := GetSetProp(AObj, pInfo);
            try
              SetSetProp(Self, plist[i], sValue);
            except
            end;
          end;
      end;
    end;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

procedure TObjectHelper.PropertiesCopyFrom(const AObj: TObject);
begin
  Self.PropertiesCopyFrom(AObj, []);
end;

function TObjectHelper.PropertiesReadFromVariant(const AValue: Variant): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  j: Integer;
begin
  Result := False;
  if not VarIsArray(AValue) then
    Exit;
  if VarArrayHighBound(AValue, 2) - VarArrayLowBound(AValue, 2) < 1 then
    Exit;

  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
      if Assigned(plist[i].SetProc) then
        for j := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
          if SameText(VarToStr(AValue[j, 0]), string(plist[i].Name)) then
          try
            SetOrdProp(Self, plist[i], Integer(AValue[j, 1]));
          except
          end;
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesEqual(const AObj: TObject): Boolean;
//比较对象属性是否相等
//源对象.PropertiesEqual(目标对象)
//如果源对象的每一个属性目标对象都存在,并且值相等,则返回真
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  prop : PPropInfo;
begin
  Result  :=  False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
      if Assigned(plist[i].GetProc) then
      begin
        prop  :=  GetPropInfo(AObj, string(plist[i].Name));
        if not Assigned(prop) or not Assigned(prop.GetProc) then
          Exit;
        if GetPropValue(Self, plist[i]) <> GetPropValue(AObj, prop) then
          Exit;
      end;
    Result  :=  True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesCheckChangedStr(const AObj: TObject): string;
//返回多行字符串，如
// count: 13 -> 14
// name: abc -> def
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  prop : PPropInfo;
  l_v1, l_v2: Variant;
  l_s1, l_s2: string;
begin
  Result  :=  '';
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
      if Assigned(plist[i].GetProc) then
      begin
        prop  :=  GetPropInfo(AObj, string(plist[i].Name));
        if not Assigned(prop) or not Assigned(prop.GetProc) then
          Continue;
        l_v1 := GetPropValue(Self, plist[i]);
        l_v2 := GetPropValue(AObj, prop);
        if l_v1 <> l_v2 then
        begin
          l_s1 := VarToStrDef(l_v1,'');
          l_s2 := VarToStrDef(l_v2,'');
          Result := Result + #13#10 + Format('[%s] %s -> %s',[string(plist[i].Name), l_s1, l_s2]);
        end;
      end;
  finally
    Result := Trim(Result);
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesIsModifiedInControls(const AControl: TWinControl): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
begin
  Result := False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if Assigned(plist[i].SetProc) then
      begin
        if PropertyIsModifiedInControl(AControl, plist[i]) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesReadFromControls(const AControl: TWinControl): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
begin
  Result := False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
      if Assigned(plist[i].SetProc) then
        PropertyReadFromControl(AControl, plist[i]);
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesWriteToControls(const AControl: TWinControl): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
begin
  Result := False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
      if Assigned(plist[i].GetProc) then
        PropertyWriteToControl(AControl, plist[i]);
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesWriteToVariant: Variant;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  Count: Integer;
begin
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    Count := 0;
    for i := 0 to plen - 1 do
      if Assigned(plist[i].GetProc) then
        Inc(Count);

    Result := VarArrayCreate([0, Count - 1, 0, 1], varVariant);
    Count := 0;
    for i := 0 to plen - 1 do
      if Assigned(plist[i].GetProc) then
      try
        Result[Count, 0] := plist[i].Name;
        Result[Count, 1] := GetPropValue(Self, plist[i]);
      finally
        Inc(Count);
      end;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertyIsModifiedInControl(const AControl: TControl;
  const APropInfo: PPropInfo): Boolean;
var
  v: Variant;
  i, j: Integer;
  wincontrol: TWinControl;
begin
  Result := False;
  if SameText(AControl.Name, string(APropInfo.Name)) then
  begin
    try
      VarClear(v);
      for i := Low(ControlRttiPropList) to High(ControlRttiPropList) do
        if IsPublishedProp(AControl, ControlRttiPropList[i]) then
        begin
          v := GetPropValue(AControl, ControlRttiPropList[i]);
          Break;
        end;
      if not VarIsClear(v) then
        Result := GetPropValue(Self, string(APropInfo.Name)) <> v;
    except
    end;
    Exit;
  end;

  if (AControl is TWinControl) then
  begin
    wincontrol := AControl as TWinControl;
    for j := 0 to wincontrol.ControlCount - 1 do
    begin
      if PropertyIsModifiedInControl(wincontrol.Controls[j], APropInfo) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TObjectHelper.PropertyReadFromControl(const AControl: TControl;
  const APropInfo: PPropInfo): Boolean;
var
  j: Integer;
  wincontrol: TWinControl;
begin
  Result := False;
  if SameText(AControl.Name, string(APropInfo.Name)) then
  begin
    Rtti.GetControlValue(Self, AControl, APropInfo);
    Result := True;
    Exit;
  end;

  if (AControl is TWinControl) then
  begin
    wincontrol := AControl as TWinControl;
    for j := 0 to wincontrol.ControlCount - 1 do
    begin
      if PropertyReadFromControl(wincontrol.Controls[j], APropInfo) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TObjectHelper.PropertyWriteToControl(const AControl: TControl;
  const APropInfo: PPropInfo): Boolean;
var
  j: Integer;
  wincontrol: TWinControl;
begin
  Result := False;
  if SameText(AControl.Name, string(APropInfo.Name)) then
  begin
    Rtti.SetControlValue(AControl, TypInfo.GetPropValue(Self, APropInfo));
    Result := True;
    Exit;
  end;

  if (AControl is TWinControl) then
  begin
    wincontrol := AControl as TWinControl;
    for j := 0 to wincontrol.ControlCount - 1 do
    begin
      if PropertyWriteToControl(wincontrol.Controls[j], APropInfo) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;
{$IFDEF FRAMEWORK_INCLUDE_XML}

function TObjectHelper.PropertiesReadFromXMLNode(const ANode: IXMLNode): Boolean;
begin
  Result  :=  PropertiesReadFromXMLNode(ANode, []);
end;

function TObjectHelper.PropertiesReadFromXMLNode(const ANode: IXMLNode; const AIgnoreProperties: array of string): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  sValue: string;
  iValue: Int64;
  cnode: IXMLNode;
  sName: string;
  fvalue : Extended;
begin
  Result := False;
  if not Assigned(ANode) then
    Exit;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].SetProc) then
        Continue;
      sName := string(plist[i].Name);
      if fcStr.Arr.PosInStrArray(sName, AIgnoreProperties, false)>=0 then
        Continue;

      cnode := ANode.ChildNodes.FindNode(sName);
      if Assigned(cnode) or ANode.HasAttribute(sName) then
      begin
        case plist[i].PropType^.Kind of
          tkString, tkUString:
            try
              sValue := GetStrProp(Self, plist[i]);
              if Assigned(cnode) then
                sValue := VarToStrDef(cnode.NodeValue, sValue)
              else
                sValue := VarToStrDef(ANode.Attributes[sName], sValue);
              SetStrProp(Self, plist[i], sValue);
            except
            end;
          tkInteger, tkInt64:
            try
              iValue := GetInt64Prop(Self, plist[i]);
              if Assigned(cnode) then
                iValue := StrToInt64Def(VarToStr(cnode.NodeValue), iValue)
              else
                iValue := StrToInt64Def(VarToStr(ANode.Attributes[sName]), iValue);
              if plist[i].PropType^.Kind = tkInt64 then
                SetInt64Prop(Self, plist[i], iValue)
              else
                SetOrdProp(Self, plist[i], Int64Rec(iValue).Lo);
            except
            end;
          tkEnumeration:
            try
              sValue := GetEnumProp(Self, plist[i]);
              if Assigned(cnode) then
                sValue := VarToStrDef(cnode.NodeValue, sValue)
              else
                sValue := VarToStrDef(ANode.Attributes[sName], sValue);

              if SameText(sValue, 'True') then
                iValue := 1
              else if SameText(sValue, 'False') then
                iValue := 0
              else
                iValue := StrToIntDef(sValue, 0);
              SetOrdProp(Self, plist[i], iValue);
            except
            end;
          tkFloat :
            try
              fvalue  :=  GetFloatProp(Self, plist[i]);
              if Assigned(cnode) then
                fvalue  :=  StrToFloatDef(VarToStrDef(cnode.NodeValue, ''), fvalue)
              else
                fvalue  :=  StrToFloatDef(VarToStrDef(ANode.Attributes[sName], ''), fvalue);
              SetFloatProp(Self, plist[i],  fvalue);//}
            except
            end;
          tkSet :
            try
              if Assigned(cnode) then
                sValue := VarToStrDef(cnode.NodeValue, sValue)
              else
                sValue := VarToStrDef(ANode.Attributes[sName], sValue);
              SetSetProp(Self, plist[i], sValue);
            except
            end;
        end;
      end;
    end;
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesWriteToXMLNode(const ANode: IXMLNode): Boolean;
begin
  Result  :=  PropertiesWriteToXMLNode(ANode, []);
end;

function TObjectHelper.PropertiesWriteToXMLNode(const ANode: IXMLNode; const AIgnoreProperties: array of string): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  sValue: string;
  iValue: Int64;
  cnode: IXMLNode;
  sName: string;
begin
  Result := False;
  if not Assigned(ANode) then
    Exit;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].GetProc) then
        Continue;
      sName := string(plist[i].Name);
      if fcStr.Arr.PosInStrArray(sName, AIgnoreProperties, false)>=0 then
        Continue;

      cnode := ANode.ChildNodes.FindNode(sName);
      case plist[i].PropType^.Kind of
        tkString, tkUString:
          try
            sValue := GetStrProp(Self, plist[i]);
            if Assigned(cnode) then
              cnode.NodeValue := sValue
            else
              ANode.Attributes[sName] := sValue;
          except
          end;
        tkInteger, tkInt64, tkEnumeration:
          try
            if plist[i].PropType^.Kind = tkInt64 then
              iValue := GetInt64Prop(Self, plist[i])
            else
              iValue := GetOrdProp(Self, plist[i]);

            if Assigned(cnode) then
              cnode.NodeValue := iValue
            else
              ANode.Attributes[sName] := iValue;
          except
          end;
        tkFloat :
          try
            if Assigned(cnode) then
              cnode.NodeValue := GetFloatProp(Self, plist[i])
            else
              ANode.Attributes[sName] := GetFloatProp(Self, plist[i]);
          except
          end;
        tkSet :
          try
            sValue  :=  GetSetProp(Self, plist[i]);
            if Assigned(cnode) then
              cnode.NodeValue := sValue
            else
              ANode.Attributes[sName] := sValue;
          except
          end;
      end;
    end;
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;
{$ENDIF}
{$IFDEF FRAMEWORK_INCLUDE_DB}

function TObjectHelper.PropertiesReadFromDataSet(const ADataSet: TDataSet; const AFieldArray: array of string): Boolean;
var
  I: Integer;
  fd: TField;
  pf: PPropInfo;
  sFieldName: string;
begin
  Result := False;
  if ADataSet.IsEmpty then
    Exit;

  for I := Low(AFieldArray) to High(AFieldArray) do
  begin
    sFieldName := AFieldArray[I];
    if (sFieldName<>'') and (sFieldName[1]='[') and (sFieldName[Length(sFieldName)]=']') then
      sFieldName := Copy(sFieldName, 2, Length(sFieldName)-2);

    pf := GetPropInfo(Self, sFieldName);
    fd := ADataSet.FindField(sFieldName);
    if Assigned(pf) and Assigned(fd) then
      Rtti.SetPropValue(Self, pf, fd.Value);
  end;
  Result := True;
end;

function TObjectHelper.PropertiesReadFromDataSet(const ADataSet: TDataSet): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  fd: TField;
begin
  Result := False;
  if ADataSet.IsEmpty then
    Exit;

  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].SetProc) then
        Continue;

      fd := ADataSet.FindField(string(plist[i].Name));
      if Assigned(fd) then
        Rtti.SetPropValue(Self, plist[i], fd.Value);
    end;
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;

function TObjectHelper.PropertiesWriteToDataSet(const ADataSet: TDataSet; const AFieldArray: array of string): Boolean;
var
  I: Integer;
  fd: TField;
  sValue: string;
  iValue: Integer;
  pf: PPropInfo;
  sFieldName: string;
begin
  Result := False;
  if ADataSet.IsEmpty then
    Exit;
  if not(ADataSet.State in dsEditModes) then
    ADataSet.Edit;

  for I := Low(AFieldArray) to High(AFieldArray) do
  begin
    sFieldName := AFieldArray[I];
    if (sFieldName<>'') and (sFieldName[1]='[') and (sFieldName[Length(sFieldName)]=']') then
      sFieldName := Copy(sFieldName, 2, Length(sFieldName)-2);

    pf := GetPropInfo(Self, sFieldName);
    fd := ADataSet.FindField(sFieldName);
    if Assigned(pf) and Assigned(fd) and fd.CanModify then
    begin
      try
        case pf.PropType^.Kind of
          tkInteger:
            fd.AsInteger := GetOrdProp(Self, pf);
          tkInt64:
            fd.AsLargeInt := GetInt64Prop(Self, pf);
          tkFloat:
            fd.AsExtended := GetFloatProp(Self, pf);
          tkSet :
            fd.AsString :=  GetSetProp(Self, pf);
          tkEnumeration:
            begin
              sValue := GetEnumProp(Self, pf);
              if SameText(sValue, 'True') then
                iValue := 1
              else if SameText(sValue, 'False') then
                iValue := 0
              else
                iValue := GetOrdProp(Self, pf);
              fd.Value := iValue;
            end
        else
          fd.Value := GetPropValue(Self, pf);
        end;
      except
      end;
    end;
  end;
  Result := True;
end;

function TObjectHelper.PropertiesWriteToDataSet(const ADataSet: TDataSet): Boolean;
var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  fd: TField;
  sValue: string;
  iValue: Integer;
begin
  Result := False;
  if ADataSet.IsEmpty then
    Exit;
  if not(ADataSet.State in dsEditModes) then
    ADataSet.Edit;

  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
    for i := 0 to plen - 1 do
    begin
      if not Assigned(plist[i].SetProc) then      //???
        Continue;

      fd := ADataSet.FindField(string(plist[i].Name));
      if Assigned(fd) and fd.CanModify then
      try
        case plist[i].PropType^.Kind of
          tkInteger:
            fd.AsInteger := GetOrdProp(Self, plist[i]);
          tkInt64:
            fd.AsLargeInt := GetInt64Prop(Self, plist[i]);
          tkFloat:
            fd.AsExtended := GetFloatProp(Self, plist[i]);
          tkSet :
            fd.AsString :=  GetSetProp(Self, plist[i]);
          tkEnumeration:
            begin
              sValue := GetEnumProp(Self, plist[i]);
              if SameText(sValue, 'True') then
                iValue := 1
              else if SameText(sValue, 'False') then
                iValue := 0
              else
                iValue := GetOrdProp(Self, plist[i]);
              fd.Value := iValue;
            end
        else
          fd.Value := GetPropValue(Self, plist[i]);
        end;
      except
      end;
    end;
    Result := True;
  finally
    FreeMem(plist, plen * SizeOf(Pointer));
  end;
end;
{$ENDIF}

end.
