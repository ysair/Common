unit fcXML;

interface
uses
  Classes, SysUtils, XMLIntf, Variants, XMLDoc;

type

  XML = record
    class function ReadAttributesToInt(ANode: IXMLNode; const AAttribName: string; const ADefValue:Integer=0): Integer; static;
    class function ReadAttributesToStr(ANode: IXMLNode; const AAttribName: string; const ADefValue:string=''): string; static; {优先读取同门的子节点，读不到再读取属性}
    class function ReadAttributesToBool(ANode: IXMLNode; const AAttribName: string; const ADefValue:Boolean=false): Boolean; static;
    class function ReadAttributesToInt64(ANode: IXMLNode; const AAttribName: string; const ADefValue:Int64=0): Int64; static;
    class function ReadAttributesToDateTime(ANode: IXMLNode; const AAttribName: string; const ADefValue:TDateTime=0): TDateTime; static;

    class function WriteAttribute(ANode: IXMLNode; const AAttribName: string; AStrValue: string): Boolean; static;
  end;

implementation

{ XML }

class function XML.ReadAttributesToBool(ANode: IXMLNode; const AAttribName: string;
  const ADefValue: Boolean): Boolean;
var
  l_s: string;
begin
  l_s := ReadAttributesToStr(ANode, AAttribName);
  if l_s = '' then
    Result := ADefValue
  else if SameText(l_s, '1') or SameText(l_s, 'true') then
    Result := True
  else
    Result := false;
end;

class function XML.ReadAttributesToDateTime(ANode: IXMLNode; const AAttribName: string;
  const ADefValue: TDateTime): TDateTime;
begin
  Result := StrToDateTimeDef(ReadAttributesToStr(ANode, AAttribName), ADefValue);
end;

class function XML.ReadAttributesToInt(ANode: IXMLNode; const AAttribName: string;
  const ADefValue: Integer): Integer;
begin
  Result := StrToIntDef(ReadAttributesToStr(ANode, AAttribName), ADefValue);
end;

class function XML.ReadAttributesToInt64(ANode: IXMLNode; const AAttribName: string;
  const ADefValue: Int64): Int64;
begin
  Result := StrToInt64Def(ReadAttributesToStr(ANode, AAttribName), ADefValue);
end;

class function XML.ReadAttributesToStr(ANode: IXMLNode; const AAttribName,
  ADefValue: string): string;
var
  l_Node: IXMLNode;
begin
  {优先读取同门的子节点，读不到再读取属性}
  l_Node := ANode.ChildNodes.FindNode(AAttribName);
  if not Assigned(l_Node) then
    l_Node := ANode.AttributeNodes.FindNode(AAttribName);
  if Assigned(l_Node) then
    Result := VarToStrDef(l_Node.NodeValue, ADefValue)
  else
    Result := VarToStrDef(ANode.Attributes[AAttribName], ADefValue);
end;

class function XML.WriteAttribute(ANode: IXMLNode; const AAttribName: string;
  AStrValue: string): Boolean;
var
  l_MultLine: Boolean;
  l_Node: IXMLNode;
begin
  Result := True;
  l_MultLine := Pos(#13, AStrValue) > 0;
  l_Node := ANode.ChildNodes.FindNode(AAttribName);
  if l_MultLine then
    if not Assigned(l_Node) then
      l_Node := ANode.AddChild(AAttribName);

  if Assigned(l_Node) then
    l_Node.NodeValue := AStrValue
  else
    ANode.Attributes[AAttribName] := AStrValue;
end;

end.