{-----------------------------------------------------------------------------
 Unit Name: xXMLUtils
 Author:
 Purpose:   IXMLDocument Read/Write Utils
 History:
  2008-11-08: * 函数全面支持大小写名称(默认XMLDoc是区分大小写的)
              + add SetXMLValue function support CaseSensitive
              * 函数全面支持默认空值的判断及返回默认值
  2008-01-17: + create this file
-----------------------------------------------------------------------------
           Copyright(c) XemeSoft 1998-2009
-----------------------------------------------------------------------------}


unit xXMLUtils;

interface

uses
  SysUtils,  XMLDoc, XMLIntf, Variants;


  //取得下级节点的节点名称与传入的名称相等的节点(不区分大小写)
  function GetNodeFromName(sName:string;ANode:IXMLNode):IXMLNode;
  //--- Get XML Value ---
  function GetXMLVal(aNode:IXMLNode; aName:WideString):OleVariant;

  function GetXMLValue(aNode:IXMLNode; aName:WideString; DefValue:WideString=''):WideString;
  function GetXMLIntValue(aNode:IXMLNode; aName:WideString; DefValue:Integer=0):Integer;
  //此函数可以判断XML中是否为默认的值，如果是则返回False
  function GetXMLValueInt(const aNode:IXMLNode; const aName:WideString; var aValue:integer; const DefValue:Integer=0):boolean;
  function GetXMLFloatValue(aNode:IXMLNode; aName:WideString; DefValue:Real=0):Real;
  function GetXMLBoolValue(aNode:IXMLNode; aName:WideString; DefValue:boolean=false):boolean;

  function SetXMLValue(aNode:IXMLNode; aName:WideString; Value:OleVariant):boolean;

  //--- Get XML Attribute ---
  function GetXMLAttr(aNode:IXMLNode;  aName:WideString):OleVariant; overload;
  function GetXMLAttribute(aNode:IXMLNode; aName:WideString; DefValue:WideString=''):WideString; overload;
  function GetXMLIntAttribute(aNode:IXMLNode; aName:WideString; DefValue:integer=0):integer; overload;
  function GetXMLFloatAttribute(aNode:IXMLNode; aName:WideString; DefValue:Real=0):Real; overload;
  function GetXMLBoolAttribute(aNode:IXMLNode; aName:WideString; DefValue:Boolean=false):Boolean; overload;

  function GetXMLAttr(aNode:IXMLNode; aIndex:word):OleVariant; overload;
  function GetXMLAttribute(aNode:IXMLNode; aIndex:word; DefValue:WideString=''):WideString; overload;
  function GetXMLIntAttribute(aNode:IXMLNode; aIndex:word; DefValue:integer=0):integer; overload;
  function GetXMLFloatAttribute(aNode:IXMLNode; aIndex:word; DefValue:Real=0):Real; overload;
  function GetXMLBoolAttribute(aNode:IXMLNode; aIndex:word; DefValue:Boolean=false):Boolean; overload;

var
  XMLDocCaseSensitive:Boolean=false;

implementation

const
  DEF_NULLVALUE=-9999;
  DEF_NULLVALUESTR='-9999';

//判断指定的Node是否有效或者不为空
function isValidNode(ANode:IXMLNode;aName:string):Boolean;
begin
  Result := False;
  if (aNode<>nil) and (aNode.ChildNodes[aName]<>nil) and not VarIsEmpty(aNode.ChildNodes[aName].NodeValue) then
    Result := True;
end;

//取得当前节点的属性与传入的名称相等的值(不区分大小写)
function GetAttrFromName(sName:string;ANode:IXMLNode):OleVariant;
var
  i:integer;
begin
  Result := Null;
  sName := LowerCase(sName);
  if not Assigned(ANode) then Exit;
  for i:= 0 to ANode.AttributeNodes.Count - 1 do
  begin
    if LowerCase(ANode.AttributeNodes[i].NodeName) = sName then
    begin
      Result := ANode.AttributeNodes[i].NodeValue;
      Break;
    end;
  end;
end;

//取得下级节点的节点名称与传入的名称相等的节点(不区分大小写)
function GetNodeFromName(sName:string;ANode:IXMLNode):IXMLNode;
var
  i:Integer;
begin
  sName := LowerCase(sName);
  Result := nil;
  if not Assigned(ANode) then Exit;
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if sName = LowerCase(ANode.ChildNodes[i].NodeName) then
    begin
      Result := ANode.ChildNodes[i];
      Break;
    end;
  end;
end;

function GetXMLVal(aNode:IXMLNode; aName:WideString):OleVariant;
var
  tmpNode:IXMLNode;
begin
  if XMLDocCaseSensitive then
  begin
    if isValidNode(aNode,aName) then
      result:=aNode.ChildNodes[aName].NodeValue
    else
      result:=null;
  end else
  begin
    tmpNode := GetNodeFromName(aName,aNode);
    if (Assigned(tmpNode)) and (not VarIsEmpty(tmpNode.NodeValue)) then
      Result := tmpNode.NodeValue
    else Result := null;
  end;
//  if (VarIsNumeric(Result) or VarIsFloat(Result)) and (Result=DEF_NULLVALUE) then
//    result:=Null
//  else if VarIsStr(Result) and Result=DEF_NULLVALUESTR then
//    result:=Null
end;

function GetXMLValue(aNode:IXMLNode; aName:WideString; DefValue:WideString=''):WideString;
var
  tmpNode:IXMLNode;
begin
  if XMLDocCaseSensitive then
  begin
    if isValidNode(aNode,aName) then
      result:=aNode.ChildNodes[aName].Text
    else
      result:=DefValue;
  end else
  begin
    tmpNode := GetNodeFromName(aName,aNode);
    if (Assigned(tmpNode)) and (not VarIsEmpty(tmpNode.NodeValue)) then
      Result := tmpNode.Text
    else Result := DefValue;
  end;
  if Result=DEF_NULLVALUESTR then Result:=DefValue;
end;

function GetXMLIntValue(aNode:IXMLNode; aName:WideString; DefValue:Integer=0):Integer;
var
  tmpNode:IXMLNode;
begin
  if XMLDocCaseSensitive then
  begin
    if isValidNode(aNode,aName) then
      result:=Integer(aNode.ChildNodes[aName].NodeValue)
    else
      result:=DefValue;
  end else
  begin
    tmpNode := GetNodeFromName(aName,aNode);
    if (Assigned(tmpNode)) and (not VarIsEmpty(tmpNode.NodeValue)) then
      Result := Integer(tmpNode.NodeValue)
    else Result := DefValue;
  end;
  if Result=DEF_NULLVALUE then Result:=DefValue;
end;

function GetXMLValueInt(const aNode:IXMLNode; const aName:WideString; var aValue:integer; const DefValue:Integer=0):boolean; overload;
var
  tmpNode:IXMLNode;
begin
  result:=false;
  aValue:=DefValue;
  if XMLDocCaseSensitive then
  begin
    if isValidNode(aNode,aName) then
    begin
      aValue:=Integer(aNode.ChildNodes[aName].NodeValue);
      if aValue=DEF_NULLVALUE then
        Result:=false
      else
        result:=true;
    end;
  end else
  begin
    tmpNode := GetNodeFromName(aName,aNode);
    if (Assigned(tmpNode)) and (not VarIsEmpty(tmpNode.NodeValue)) then
    begin
      aValue := Integer(tmpNode.NodeValue);
      if aValue=DEF_NULLVALUE then
        Result:=false
      else
        result:=true;
    end
  end;
end;

function GetXMLFloatValue(aNode:IXMLNode; aName:WideString; DefValue:Real=0):Real;
var
  tmpNode:IXMLNode;
begin
  if XMLDocCaseSensitive then
  begin
    if isValidNode(aNode,aName) then
      result:=real(aNode.ChildNodes[aName].NodeValue)
    else
      result:=DefValue;
  end else
  begin
    tmpNode := GetNodeFromName(aName,aNode);
    if (Assigned(tmpNode)) and (not VarIsEmpty(tmpNode.NodeValue)) then
      Result := real(tmpNode.NodeValue)
    else Result := DefValue;
  end;
  if Result=DEF_NULLVALUE then Result:=DefValue;
end;

function GetXMLBoolValue(aNode:IXMLNode; aName:WideString; DefValue:boolean=false):boolean;
var
  tmpNode:IXMLNode;
begin
  if XMLDocCaseSensitive then
  begin
    if isValidNode(aNode,aName) then
      result:=boolean(aNode.ChildNodes[aName].NodeValue)
    else
      result:=DefValue;
  end else
  begin
    tmpNode := GetNodeFromName(aName,aNode);
    if (Assigned(tmpNode)) and (not VarIsEmpty(tmpNode.NodeValue)) then
      Result := boolean(tmpNode.NodeValue)
    else Result := DefValue;
  end;
end;

function GetXMLAttr(aNode:IXMLNode;  aName:WideString):OleVariant;
begin
  if XMLDocCaseSensitive then
  begin
    if (aNode<>nil) and (aNode.Attributes[aName]<>null) then
      result:=string(aNode.Attributes[aName])
    else
      result:=null;
  end
  else
    result := GetAttrFromName(aName,aNode);
end;

function GetXMLAttribute(aNode:IXMLNode; aName:WideString; DefValue:WideString=''):WideString;
var
  tmpVar:OleVariant;
begin
  if XMLDocCaseSensitive then
  begin
    if (aNode<>nil) and (aNode.Attributes[aName]<>null) then
      result:=string(aNode.Attributes[aName])
    else
      result:=DefValue;
  end else
  begin
    tmpVar := GetAttrFromName(aName,aNode);
    if tmpVar <> Null then Result := string(tmpVar)
    else Result := DefValue;
  end;
  if Result=DEF_NULLVALUESTR then Result:=DefValue;
end;

function GetXMLIntAttribute(aNode:IXMLNode; aName:WideString; DefValue:integer=0):integer;
var
  tmpVar:OleVariant;
begin
  if XMLDocCaseSensitive then
  begin
    if (aNode<>nil) and (aNode.Attributes[aName]<>null) then
      result:=Integer(aNode.Attributes[aName])
    else
      result:=DefValue;
  end else
  begin
    tmpVar := GetAttrFromName(aName,aNode);
    if tmpVar <> Null then Result := Integer(tmpVar)
    else Result := DefValue;
  end;
  if Result=DEF_NULLVALUE then Result:=DefValue;
end;

function GetXMLFloatAttribute(aNode:IXMLNode; aName:WideString; DefValue:Real=0):Real;
var
  tmpVar:OleVariant;
begin
  if XMLDocCaseSensitive then
  begin
    if (aNode<>nil) and (aNode.Attributes[aName]<>null) then
      result:=real(aNode.Attributes[aName])
    else
      result:=DefValue;
  end else
  begin
    tmpVar := GetAttrFromName(aName,aNode);
    if tmpVar <> Null then Result := real(tmpVar)
    else Result := DefValue;
  end;
  if Result=DEF_NULLVALUE then Result:=DefValue;
end;

function GetXMLBoolAttribute(aNode:IXMLNode; aName:WideString; DefValue:Boolean=false):Boolean;
var
  tmpVar:OleVariant;
begin
  if XMLDocCaseSensitive then
  begin
    if (aNode<>nil) and (aNode.Attributes[aName]<>null) then
      result:=Boolean(aNode.Attributes[aName])
    else
      result:=DefValue;
  end else
  begin
    tmpVar := GetAttrFromName(aName,aNode);
    if tmpVar <> Null then Result := Boolean(tmpVar)
    else Result := DefValue;
  end;
end;

//通过Index来读取Attribute，所以不存在区分大小写的问题
function GetXMLAttr(aNode:IXMLNode; aIndex:word):OleVariant; 
begin
  if (aNode<>nil) and (aIndex>=0) and (aIndex<aNode.AttributeNodes.Count) then
    result:=aNode.AttributeNodes[aIndex].NodeValue
  else
    result:=null;
end;

function GetXMLAttribute(aNode:IXMLNode; aIndex:word; DefValue:WideString=''):WideString;
begin
  if (aNode<>nil) and (aIndex>=0) and (aIndex<aNode.AttributeNodes.Count) and (aNode.AttributeNodes[aIndex].NodeValue<>null) then
    result:=aNode.AttributeNodes[aIndex].NodeValue
  else
    result:=DefValue;
end;

function GetXMLIntAttribute(aNode:IXMLNode; aIndex:word; DefValue:integer=0):integer;
begin
  if (aNode<>nil) and (aIndex>=0) and (aIndex<aNode.AttributeNodes.Count) and (aNode.AttributeNodes[aIndex].NodeValue<>null) then
    result:=Integer(aNode.AttributeNodes[aIndex].NodeValue)
  else
    result:=DefValue;
end;

function GetXMLFloatAttribute(aNode:IXMLNode; aIndex:word; DefValue:Real=0):Real;
begin
  if (aNode<>nil) and (aIndex>=0) and (aIndex<aNode.AttributeNodes.Count) and (aNode.AttributeNodes[aIndex].NodeValue<>null) then
    result:=Real(aNode.AttributeNodes[aIndex].NodeValue)
  else
    result:=DefValue;
end;

function GetXMLBoolAttribute(aNode:IXMLNode; aIndex:word; DefValue:Boolean=false):Boolean;
begin
  if (aNode<>nil) and (aIndex>=0) and (aIndex<aNode.AttributeNodes.Count) and (aNode.AttributeNodes[aIndex].NodeValue<>null) then
    result:=Boolean(aNode.AttributeNodes[aIndex].NodeValue)
  else
    result:=DefValue;
end;
            
function SetXMLValue(aNode:IXMLNode; aName:WideString; Value:OleVariant):boolean;
var
  n:IXMLNode;
begin
  result:=false;
  if aNode=nil then exit;
  try
    if XMLDocCaseSensitive then
      n:=aNode.ChildNodes.FindNode(aName)
    else
      n:=GetNodeFromName(aName,aNode);
    if n=nil then
      n:=aNode.AddChild(aName);
    n.NodeValue:=Value;
    result:=true;
  except

  end;
end;


end.
