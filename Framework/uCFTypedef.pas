// Common Framework
// 类型定义单元
{$I CommonFramework.inc}
unit uCFTypedef;

interface

uses
  SysUtils, Classes, Windows;

type
  TPoolsClearType = (pctNone, pctOne, pctAll);

  TVariantArray = array of Variant;

  TCFLogType = (ltNone, ltMessage, ltError);

  TInterfaceNotifyEvent = procedure(Sender: IInterface) of object;
  TNotifyMessageEvent = procedure(Sender : TObject; const AMsg : string) of object;

  TEditAction  =  (eaAdd, eaEdit, eaDelete);

  TFormAction = (faAdd, faEdit, faSelect);

  TIDNames = record
    ID : Integer;
    Name : string;
    class function GetNameFromID(const AID : Integer; const AArr : array of TIDNames) : string; static;
    class function GetIDFromName(const AName : string; const AArr : array of TIDNames) : Integer; static;
  end;

  TStrKeyValues = record
    Key : string;
    Value : string;
    class function GetValueFromKey(const AKey : string; const AArr : array of TStrKeyValues) : string; static;
    class function GetKeyFromValue(const AValue : string; const AArr : array of TStrKeyValues) : string; static;
  end;

implementation

{ TIDNames }

class function TIDNames.GetIDFromName(const AName: string;
  const AArr: array of TIDNames): Integer;
var
  idname : TIDNames;
begin
  for  idname in AArr do
    if SameText(idname.Name, AName) then
      Exit(idname.ID);
  Result  :=  -1;
end;

class function TIDNames.GetNameFromID(const AID: Integer;
  const AArr: array of TIDNames): string;
var
  idname : TIDNames;
begin
  for  idname in AArr do
    if idname.ID = AID then
      Exit(idname.Name);
  Result  :=  '';
end;

{ TStrKeyValues }

class function TStrKeyValues.GetKeyFromValue(const AValue: string;
  const AArr: array of TStrKeyValues): string;
var
  kv : TStrKeyValues;
begin
  for  kv in AArr do
    if SameText(kv.Value, AValue) then
      Exit(kv.Key);
  Result  :=  '';
end;

class function TStrKeyValues.GetValueFromKey(const AKey: string;
  const AArr: array of TStrKeyValues): string;
var
  kv : TStrKeyValues;
begin
  for  kv in AArr do
    if SameText(kv.Key, AKey) then
      Exit(kv.Value);
  Result  :=  '';
end;

end.
