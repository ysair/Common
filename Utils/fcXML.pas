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
    class function FindXMLNode(const ARoot : IXMLNode; const ANodePath : string) : IXMLNode; static;
    class function TryGetNodeValue(const ARoot : IXMLNode; const ANodePath : string; var AValue : OleVariant):boolean; static;
    class function GetNodeValue(const ARoot : IXMLNode; const ANodePath : string; const ADefValue : string = ''):string; static;
  end;

implementation


{ XML }

class function XML.FindXMLNode(const ARoot: IXMLNode;
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

class function XML.GetNodeValue(const ARoot: IXMLNode;
  const ANodePath: string; const ADefValue : string): string;
var
  node : IXMLNode;
begin
  node  :=  FindXMLNode(ARoot, ANodePath);
  if Assigned(node) then
    Result  :=  VarToStr(node.NodeValue)
  else
    Result  :=  ADefValue;
end;

class function XML.TryGetNodeValue(const ARoot: IXMLNode;
  const ANodePath: string; var AValue: OleVariant): boolean;
var
  node : IXMLNode;
begin
  node  :=  FindXMLNode(ARoot, ANodePath);
  Result  :=  Assigned(node);
  if Result then
    AValue  :=  node.NodeValue;
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