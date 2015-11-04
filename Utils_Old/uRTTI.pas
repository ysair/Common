unit uRTTI;

interface

uses
  Windows, SysUtils, typinfo;

function GetObjectProperty(
    const AObject: TObject;
    const APropName: string
    ): TObject;
function SetIntegerPropertyIfExists(
    const AObject: TObject;
    const APropName: string;
    const AValue: integer
    ): Boolean;
function SetEnumPropertyIfExists(
    const AObject: TObject;
    const APropName: string;
    const AValue: integer
    ): Boolean;

implementation

//取得对象属性值,如果存在
function GetObjectProperty(
    const AObject: TObject;
    const APropName: string
    ): TObject;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := GetPropInfo(AObject.ClassInfo, APropName);
  if Assigned(PropInfo) and
    (PropInfo^.PropType^.Kind = tkClass) then
    Result := GetObjectProp(AObject, PropInfo);
end;

//给整型属性赋值,如果存在
function SetIntegerPropertyIfExists(
    const AObject: TObject;
    const APropName: string;
    const AValue: integer
    ): Boolean;
var
  PropInfo: PPropInfo;
begin
  Result := False;
  if not Assigned(AObject) then Exit;
  PropInfo := GetPropInfo(AObject.ClassInfo, APropName);
  if Assigned(PropInfo) and
    (PropInfo^.PropType^.Kind = tkInteger) then
  begin
    SetOrdProp(AObject, PropInfo, AValue);
    Result := True;
  end;
end;

//给枚举型属性赋值,如果存在
function SetEnumPropertyIfExists(
    const AObject: TObject;
    const APropName: string;
    const AValue: integer
    ): Boolean;
var
  PropInfo: PPropInfo;
begin
  Result := False;
  if not Assigned(AObject) then Exit;
  PropInfo := GetPropInfo(AObject.ClassInfo, APropName);
  if Assigned(PropInfo) and
    (PropInfo^.PropType^.Kind = tkEnumeration) then
  begin
    SetOrdProp(AObject, PropInfo, AValue);
    Result := True;
  end;
end;

end.

