unit fcPlus_DevExpress;

interface

uses
  Windows, Classes, SysUtils, Controls, DB, cxTL, Variants, Math,
  cxDropDownEdit, dxBar, {dxNavBar, dxNavBarStyles,} cxCustomData,
  cxGridTableView, cxGridDBTableView, cxDBTL, dxCore;

type
  TNodeFunc = function (ANode: TcxTreeListNode): Boolean of object;

  TcxComboBoxPropertiesHelper = class helper for TcxComboBoxProperties
  public
    procedure FillItemsFromDataSet(const ADataSet : TDataSet; const AFieldName : string);
  end;

  TdxBarManagerHelper = class helper for TdxBarManager
  public
    procedure RemoveToolButtonShortCut;
    procedure SetLargeButtonAutoGrayScale(const AValue : Boolean);
    procedure SetDefaultPropertyValues;
  end;

  {TdxNavBarHelper = class helper for TdxNavBar
  public
    procedure SetDefaultStylesFont(const AFontName : string;
        const AItemFontSize : Integer = 0; const AGroupFontSize : Integer = 0);
  end;//}

  TcxTreeListHelper = class helper for TcxTreeList
  public
    function CompareTreeNode(ANode1, ANode2: TcxTreeListNode; isNumberColumn: boolean): Integer; overload;
    function CompareTreeNode(ANode1, ANode2: TcxTreeListNode; ANumberColumn: TcxTreeListColumn): Integer; overload;
  end;

  TcxGridTableViewHelper = class helper for TcxGridTableView
  public
    function SaveColumnInfoToStr : string;
    procedure LoadColumnInfoFromStr(const AStr : string);
  end;

  TcxDBTreeListHelper = class helper for TcxDBTreeList
  public
    /// <summary>
    ///  根据是否可见函数对每个节点进行设置。
    ///  每个节点是否可见，决定于该节点是否可见，或者其子节点是否可见。
    /// </summary>
    /// <param name="AFunc"></param>
    /// <returns></returns>
    function SetTreeVisible(AFunc: TNodeFunc; AAllItem : Boolean = False):Boolean;

    /// <summary>
    ///  根据可读可写函数对每个节点进行设置。如果可读，则该节点可见。如果可写，则该节点的tag为1.
    ///  每个节点是否可写，决定于该节点/其父节点是否可写。
    ///  每个节点是否可读，决定于该节点是否可写，或者该节点/其子节点是否可读。
    /// </summary>
    /// <param name="AViewFunc">判断节点是否可读的函数</param>
    /// <param name="AEditFunc">判断节点是否可写的函数</param>
    /// <returns>节点是否可读</returns>
    function SetTreeViewEdit(AViewFunc, AEditFunc: TNodeFunc):Boolean;

  end;

implementation

uses
  fcStr;

function SetNodeVisible(ANode:TcxTreeListNode; AFunc: TNodeFunc):Boolean;
var
  I: Integer;
  curResult:Boolean;
begin
  Result := AFunc(ANode);

  if (not Result) and (ANode.Count > 0) then
  begin
    for I := 0 to ANode.Count - 1 do
    begin
      curResult := SetNodeVisible(ANode.Items[I], AFunc);
      Result := Result or curResult;
    end;
  end;

  ANode.Visible := Result;
end;

function SetNodeViewEdit(ANode:TcxTreeListNode; AViewFunc, AEditFunc: TNodeFunc; ADefaultEdit:Boolean):Boolean;
var
  I: Integer;
  curResult:Boolean;
  canEdit:Boolean;
begin
  ///可编辑取决于父节点可编辑（ADefaultEdit）或者本身可编辑。
  canEdit := ADefaultEdit or AEditFunc(ANode);

  if canEdit then
    ANode.Data := Pointer(1)
  else
    ANode.Data := Pointer(0);

  ///可查看取决于本身是否可编辑，可查看或者子节点可查看。
  Result := canEdit or AViewFunc(ANode);

  for I := 0 to ANode.Count - 1 do
  begin
    curResult := SetNodeViewEdit(ANode.Items[I], AViewFunc, AEditFunc, canEdit);
    Result := Result or curResult;
  end;

  ANode.Visible := Result;
end;

{ TcxComboBoxPropertiesHelper }

procedure TcxComboBoxPropertiesHelper.FillItemsFromDataSet(
  const ADataSet: TDataSet; const AFieldName : string);
begin
  if ADataSet.IsEmpty or not Assigned(ADataSet.FindField(AFieldName)) then Exit;
  Items.Clear;
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    Items.Add(ADataSet.FieldByName(AFieldName).AsString);
    ADataSet.Next;
  end;
end;

{ TdxBarManagerHelper }

procedure TdxBarManagerHelper.RemoveToolButtonShortCut;

  function _GetCaption(const AText : string):string;
  var
    p1, p2 : Integer;
  begin
    Result  :=  AText;
    p1  :=  Pos('(', AText);
    p2  :=  Pos(')', AText);
    if (p1 > 0) and (p2 - p1 = 3) and (AText[p1 + 1] = '&') then
    begin
      Delete(Result, p1, 4);
      Exit;
    end;
    p1 := Pos('&', AText);
    if (p1 > 0) and (p1 < Length(AText)) then
      Delete(Result, p1, 2);
  end;

var
  i : Integer;
  j : Integer;
begin
  for i := 0 to Bars.Count - 1 do
    for j := 0 to Bars[i].ItemLinks.Count - 1 do
      if Assigned(Bars[i].ItemLinks[j].Control) and Assigned(Bars[i].ItemLinks[j].Control.item) then
        Bars[i].ItemLinks[j].Control.item.Caption  :=  _GetCaption(Bars[i].ItemLinks[j].Caption);
end;

procedure TdxBarManagerHelper.SetDefaultPropertyValues;
begin
  Style :=  bmsUseLookAndFeel;
  RemoveToolButtonShortCut;
  SetLargeButtonAutoGrayScale(False);
end;

procedure TdxBarManagerHelper.SetLargeButtonAutoGrayScale(
  const AValue: Boolean);
var
  i : Integer;
begin
  for i := 0 to Self.ItemCount - 1 do
    if Items[i] is TdxBarLargeButton then
      TdxBarLargeButton(Items[i]).AutoGrayScale :=  AValue;
end;

{ TdxNavBarHelper }

{procedure TdxNavBarHelper.SetDefaultStylesFont(const AFontName: string;
  const AItemFontSize, AGroupFontSize: Integer);

  procedure _SetFont(const AStyle : TdxNavBarBaseStyle; const AFontSize : Integer);
  begin
    AStyle.Font.Name  :=  AFontName;
    if AFontSize <> 0 then
      AStyle.Font.Size  :=  AFontSize;
  end;

begin
  _SetFont(DefaultStyles.Button, AItemFontSize);
  _SetFont(DefaultStyles.ButtonHotTracked, AItemFontSize);
  _SetFont(DefaultStyles.ButtonPressed, AItemFontSize);
  _SetFont(DefaultStyles.Item, AItemFontSize);
  _SetFont(DefaultStyles.ItemDisabled, AItemFontSize);
  _SetFont(DefaultStyles.ItemHotTracked, AItemFontSize);
  _SetFont(DefaultStyles.ItemPressed, AItemFontSize);
  _SetFont(DefaultStyles.GroupHeader, AItemFontSize);
  _SetFont(DefaultStyles.GroupHeaderActive, AItemFontSize);
  _SetFont(DefaultStyles.GroupHeaderActiveHotTracked, AItemFontSize);
  _SetFont(DefaultStyles.GroupHeaderActivePressed, AItemFontSize);
  _SetFont(DefaultStyles.GroupHeaderHotTracked, AGroupFontSize);
  _SetFont(DefaultStyles.GroupHeaderPressed, AItemFontSize);
end;//}

{ TcxTreeListHelper }

function TcxTreeListHelper.CompareTreeNode(ANode1, ANode2: TcxTreeListNode;
  isNumberColumn: boolean): Integer;
var
  strValue1, strValue2: String;
  size1, size2: Double;
begin
  if SortedColumnCount = 0 then
  begin
    Result := 0;
    Exit;
  end;

  strValue1 := VarToStrDef(ANode1.Values[SortedColumns[0].ItemIndex], '');
  strValue2 := VarToStrDef(ANode2.Values[SortedColumns[0].ItemIndex], '');

  if isNumberColumn then
  begin
    size1 := StrToFloatDef(Str.GetTokenBeforeChar(strValue1, ' ', true, false), 0);
    size2 := StrToFloatDef(Str.GetTokenBeforeChar(strValue2, ' ', true, false), 0);

    if SortedColumns[0].SortOrder = soAscending then
      Result := CompareValue(size1, size2)
    else
      Result := CompareValue(size2, size1);
  end
  else
  begin
    if SortedColumns[0].SortOrder = soAscending then
      Result := CompareStr(strValue1, strValue2)
    else
      Result := CompareStr(strValue2, strValue1);
  end;
end;

function TcxTreeListHelper.CompareTreeNode(ANode1, ANode2: TcxTreeListNode;
  ANumberColumn: TcxTreeListColumn): Integer;
begin
  if SortedColumnCount = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := CompareTreeNode(ANode1, ANode2, SortedColumns[0] = ANumberColumn);
end;

{ TcxGridTableViewHelper }

function TcxGridTableViewHelper.SaveColumnInfoToStr: string;
var
  i : Integer;
  lst : TStrings;
begin
  lst :=  TStringList.Create;
  try
    for i := 0 to ColumnCount - 1 do
      lst.Values[Columns[i].Name] :=  IntToStr(Columns[i].Width);
    Result  :=  lst.CommaText;
  finally
    lst.Free;
  end;
end;

procedure TcxGridTableViewHelper.LoadColumnInfoFromStr(
  const AStr: string);
var
  i : Integer;
  lst : TStrings;
  w : Integer;
begin
  lst :=  TStringList.Create;
  try
    lst.CommaText :=  AStr;
    for i := 0 to ColumnCount - 1 do
    begin
      w :=  StrToIntDef(lst.Values[Columns[i].Name], 0);
      if w > 0 then
        Columns[i].Width  :=  w;
    end;
  finally
    lst.Free;
  end;
end;

{ TcxDBTreeListHelper }

function TcxDBTreeListHelper.SetTreeViewEdit(AViewFunc,
  AEditFunc: TNodeFunc): Boolean;
var
  i:Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if SetNodeViewEdit(Items[i], AViewFunc, AEditFunc, False) then
      Result := True;
  end;
end;

function TcxDBTreeListHelper.SetTreeVisible(AFunc: TNodeFunc; AAllItem : Boolean): Boolean;
var
  i:Integer;
begin
  Result := False;
  if AAllItem then
  begin
    for i := 0 to AbsoluteCount - 1 do
    begin
      if SetNodeVisible(AbsoluteItems[i], AFunc) then
        Result := True;
    end;
  end
  else begin
    for i := 0 to Count - 1 do
    begin
      if SetNodeVisible(Items[i], AFunc) then
        Result := True;
    end;
  end;
end;

end.

