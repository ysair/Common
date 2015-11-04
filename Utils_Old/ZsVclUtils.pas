unit ZsVclUtils;

interface

uses
  Controls, Menus, Classes, Forms, ActnList, Windows, Graphics, Buttons,
  Variants, cxTL;

procedure ShowControlPopupMenu(ctlShow: TControl; pmShow: TPopupMenu); overload;

procedure ShowControlPopupMenu(ctlShow: TControl; X, Y: Integer; pmShow: TPopupMenu); overload;

procedure ShowPopupMenuAtCursorPosition(APopupMenu: TPopupMenu);

function SetTopWindow(hwnd: THandle): boolean;

function ShowModalForm(Owner: TComponent; frmClass: TFormClass): Integer;

function MyExecuteAction(aAction: TAction): boolean;

function GetTaskBar: Integer; // 任务栏的高度

function SelectFont(AForm: TForm; ADestFont: TFont; const ACanSelectStyle: boolean): boolean;

function GetFormFromComponent(AComp: TComponent): TForm;

// isNumberColumn表示是否需要按照数字方式进行比较
function CompareTreeNode(ATreeList: TcxTreeList; ANode1, ANode2: TcxTreeListNode;
  isNumberColumn: boolean): Integer; overload;

function CompareTreeNode(ATreeList: TcxTreeList; ANode1, ANode2: TcxTreeListNode;
  ANumberColumn: TcxTreeListColumn): Integer; overload;

procedure GetAllTreeListNode(ATreeList: TcxTreeList; AList: TList);

procedure SetFontFromString(var AFont: TFont; ASaveStr: String; defaultFontSize: Integer;
  defaultCharset: TFontCharset);

function FontToString(const AFont: TFont): String;

function ImageListLoadPictureFile(AImageList: TImageList; AFileName: String;
  const AClearOrgImage: boolean = true): boolean;

procedure SetFocusAWinControl(AWinControl: TWinControl);

function GetTopParentForm(AControl: TControl): TForm;

procedure ButtonResource_AddBmpToImageList(AImageList: TImageList; ABmp: TBitmap);

implementation

uses
  Types, SysUtils, ShellAPI, MyDlg, Dialogs, ZsStringUtils,
  cxCustomData, Math, fcVCL;

procedure ShowControlPopupMenu(ctlShow: TControl; pmShow: TPopupMenu);
var
  Point: TPoint;
begin
  Point.X := 0;
  Point.Y := ctlShow.Height;
  Point := ctlShow.ClientToScreen(Point);

  if Point.X > Screen.Width - 130 then
    Point.X := Point.X + ctlShow.Width;

  pmShow.Popup(Point.X, Point.Y);
end;

procedure ShowPopupMenuAtCursorPosition(APopupMenu: TPopupMenu);
var
  p: TPoint;
begin
  GetCursorPos(p);
  APopupMenu.Popup(p.X, p.Y);
end;

// 同上，X, Y是ctlShow上的偏移位置
procedure ShowControlPopupMenu(ctlShow: TControl; X, Y: Integer; pmShow: TPopupMenu);
var
  Point: TPoint;
begin
  Point.X := X;
  Point.Y := Y;
  Point := ctlShow.ClientToScreen(Point);
  pmShow.Popup(Point.X, Point.Y);
end;

// ------------------------------------------------------------------------------
// Win98，微软更改了系统代码，一般的SetForegroundWindow只能将
// 状态栏中应用按钮闪烁，并没有将应用调到最前面。请使用下列函数：
function SetTopWindow(hwnd: THandle): boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  timeout: DWORD;
begin
  try
    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and ((Win32MajorVersion > 4) or
          ((Win32MajorVersion = 4) and (Win32MinorVersion > 0)))) then
    begin
      SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
      SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0), SPIF_SENDCHANGE);
      Result := SetForegroundWindow(hwnd);
      SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
    end
    else
      Result := SetForegroundWindow(hwnd);
  except
    Result := false;
  end;
end;

function ShowModalForm(Owner: TComponent; frmClass: TFormClass): Integer;
begin
  with frmClass.Create(Owner) do
  begin
    try
      Result := ShowModal;
    finally
      Free;
    end;
  end;
end;

function MyExecuteAction(aAction: TAction): boolean;
var
  l_ActionList: TActionList;
  l_Handle: boolean;
begin
  if (aAction.ActionList <> nil) then
  begin
    l_ActionList := aAction.ActionList as TActionList;
    if Assigned(l_ActionList.OnUpdate) then
      l_ActionList.OnUpdate(nil, l_Handle);
  end;

  if Assigned(aAction.OnUpdate) then
    aAction.OnUpdate(nil);

  Result := aAction.Enabled;
  if Result then
    aAction.OnExecute(aAction);
end;

function GetTaskBar: Integer; // 任务栏的高度
var
  abd: TAppBarData;
begin
  abd.cbSize := sizeof(abd);
  SHAppBarMessage(ABM_GETTASKBARPOS, abd);
  Result := abd.rc.Bottom - abd.rc.Top;

  if Result > (Screen.Height div 2) then
    Result := 0;
end;

function SelectFont(AForm: TForm; ADestFont: TFont; const ACanSelectStyle: boolean): boolean;
begin
  with TMyFontDialog.Create(AForm) do
  begin
    try
      Font.Assign(ADestFont);
      if not ACanSelectStyle then
        Options := [fdNoStyleSel];

      Result := Execute;
      if Result then
        ADestFont.Assign(Font);
    finally
      Free;
    end;
  end;
end;

function GetFormFromComponent(AComp: TComponent): TForm;
var
  comp: TComponent;
begin
  comp := AComp;
  while Assigned(comp) do
  begin
    if comp is TForm then
      break;
    comp := comp.Owner;
  end;

  if (Assigned(comp) and (comp is TForm)) then
    Result := comp as TForm
  else
    Result := Application.MainForm;
end;

function CompareTreeNode(ATreeList: TcxTreeList; ANode1, ANode2: TcxTreeListNode;
  isNumberColumn: boolean): Integer;
var
  strValue1, strValue2: String;
  size1, size2: Double;
begin
  if ATreeList.SortedColumnCount = 0 then
  begin
    Result := 0;
    Exit;
  end;

  strValue1 := VarToStrDef(ANode1.Values[ATreeList.SortedColumns[0].ItemIndex], '');
  strValue2 := VarToStrDef(ANode2.Values[ATreeList.SortedColumns[0].ItemIndex], '');

  if isNumberColumn then
  begin
    size1 := StrToFloatDef(GetTokenBeforeChar(strValue1, ' ', true, false), 0);
    size2 := StrToFloatDef(GetTokenBeforeChar(strValue2, ' ', true, false), 0);

    if ATreeList.SortedColumns[0].SortOrder = soAscending then
      Result := CompareValue(size1, size2)
    else
      Result := CompareValue(size2, size1);
  end
  else
  begin
    if ATreeList.SortedColumns[0].SortOrder = soAscending then
      Result := CompareStr(strValue1, strValue2)
    else
      Result := CompareStr(strValue2, strValue1);
  end;
end;

function CompareTreeNode(ATreeList: TcxTreeList; ANode1, ANode2: TcxTreeListNode;
  ANumberColumn: TcxTreeListColumn): Integer; overload;
begin
  if ATreeList.SortedColumnCount = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := CompareTreeNode(ATreeList, ANode1, ANode2, ATreeList.SortedColumns[0] = ANumberColumn);
end;

procedure AddNodeToList(ANode: TcxTreeListNode; AList: TList);
var
  I: Integer;
begin
  AList.Add(ANode);
  if ANode.Count = 0 then
    Exit;

  for I := 0 to ANode.Count - 1 do
    AddNodeToList(ANode.Items[I], AList);
end;

procedure AllItemsToList(ATree: TcxTreeList; AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  if ATree.Count = 0 then
    Exit;

  for I := 0 to ATree.Count - 1 do
    AddNodeToList(ATree.Items[I], AList);
end;

// todo:该函数需要测试！！！
procedure GetAllTreeListNode(ATreeList: TcxTreeList; AList: TList);
begin
  AllItemsToList(ATreeList, AList);
end;

function FontToString(const AFont: TFont): String;
begin
  Result := Vcl.FontToString(AFont);
end;

procedure SetFontFromString(var AFont: TFont; ASaveStr: String; defaultFontSize: Integer;
  defaultCharset: TFontCharset);
begin
  Vcl.SetFontFromString(AFont, ASaveStr, defaultFontSize ,defaultCharset);
end;

function ImageListLoadPictureFile(AImageList: TImageList; AFileName: String;
  const AClearOrgImage: boolean): boolean;
var
  I, l_X, l_Y, l_Row, l_Col: Integer;
  l_SrcPic, l_ABMP: Graphics.TBitmap;
const
  l_BKColor = clFuchsia;
begin
  Result := true;
  if AClearOrgImage then
    AImageList.Clear;

  l_ABMP := Graphics.TBitmap.Create;
  l_SrcPic := Graphics.TBitmap.Create;
  try
    l_SrcPic.LoadFromFile(AFileName);
    l_Row := l_SrcPic.Height div AImageList.Height; // 多少行
    l_Col := l_SrcPic.Width div AImageList.Width; // 多少列
    l_ABMP.Width := AImageList.Width;
    l_ABMP.Height := AImageList.Height;

    for I := 0 to l_Col * l_Row - 1 do
    begin
      l_X := (I mod l_Col) * AImageList.Width;
      l_Y := (I div l_Col) * AImageList.Height;
      BitBlt(l_ABMP.Canvas.Handle, 0, 0, AImageList.Width, AImageList.Height,
        l_SrcPic.Canvas.Handle, l_X, l_Y, SRCCOPY);
      try
        AImageList.AddMasked(l_ABMP, l_BKColor);
      except
      end;
    end;
  finally
    l_ABMP.Free;
    l_SrcPic.Free;
  end;
end;

procedure SetFocusAWinControl(AWinControl: TWinControl);
begin
  try
    AWinControl.SetFocus;
  except
  end;
end;

function GetTopParentForm(AControl: TControl): TForm;
var
  l_Form: TControl;
begin
  l_Form := AControl.Parent;
  while (l_Form <> nil) and (not(l_Form is TForm)) do
    l_Form := l_Form.Parent;

  if l_Form <> nil then
    Result := l_Form as TForm
  else
    Result := nil;
end;

procedure ButtonResource_AddBmpToImageList(AImageList: TImageList; ABmp: TBitmap);
var
  l_bmp, l_Center: TBitmap;
  l_Indent: Integer;
const
  l_BKColor = clFuchsia;
begin
  if AImageList.Height <> ABmp.Height then
    AImageList.Height := ABmp.Height;

  if ABmp.Width = AImageList.Width then
  begin
    AImageList.AddMasked(ABmp, l_BKColor);
  end
  else
  begin
    l_bmp := TBitmap.Create;
    l_Center := TBitmap.Create;
    try
      if ABmp.Width < AImageList.Width then
        l_Indent := ABmp.Width div 2 - 1
      else
        l_Indent := AImageList.Width div 2 - 1;

      l_Center.Height := AImageList.Height;
      l_Center.Width := l_Indent;
      BitBlt(l_Center.Canvas.Handle, 0, 0, l_Indent, l_Center.Height, ABmp.Canvas.Handle,
        ABmp.Width div 2 - 1, 0, SRCCOPY);

      l_bmp.Width := AImageList.Width;
      l_bmp.Height := AImageList.Height;
      StretchBlt(l_bmp.Canvas.Handle, 0, 0, l_bmp.Width, l_bmp.Height, l_Center.Canvas.Handle, 0,
        0, l_Center.Width, l_Center.Height, SRCCOPY);

      BitBlt(l_bmp.Canvas.Handle, 0, 0, l_Indent, ABmp.Height, ABmp.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(l_bmp.Canvas.Handle, l_bmp.Width - l_Indent, 0, l_bmp.Width, ABmp.Height,
        ABmp.Canvas.Handle, ABmp.Width - l_Indent, 0, SRCCOPY);

      AImageList.AddMasked(l_bmp, l_BKColor);
    finally
      l_bmp.Free;
      l_Center.Free;
    end;
  end;
end;

end.
