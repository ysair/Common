unit fcVCL;

interface
uses
  Forms, ExtCtrls, Windows, Controls, ImgList, SysUtils, Classes, Graphics,
  ComCtrls, Menus, Buttons, ActnList, Messages;

type
  Vcl = record
    class procedure ShowAFormInControl(AForm: TForm; AControl: TWinControl); static;
    class procedure ShowAFormInAPanel(AForm: TForm; APanel: TPanel); static;
    class function ImageListLoadPictureFile(AImageList: TImageList; AFileName: String; const AClearOrgImage: Boolean=true): Boolean; static;

    /// <summary>
    /// 将PageControl中所有页面设置为不可见
    /// </summary>
    /// <param name="APageControl">PageControl</param>
    class procedure HidePageControlTab(const APageControl: TPageControl); static;

    class procedure ShowControlPopupMenu(ctlShow:TControl; pmShow: TPopupMenu); overload; static;
    class procedure ShowControlPopupMenu(ctlShow: TControl; X, Y : Integer;pmShow:TPopupMenu); overload; static;
    class procedure ShowPopupMenuAtCursorPosition(APopupMenu: TPopupMenu); static;

    class procedure SetControlVisible(AForm:TForm; ATag: Integer; AVisible: Boolean);static;
    class procedure SetControlEnable(AControl: TWinControl; AEnabled: Boolean; const AEnableColor: TColor=clWindow; const ADisableColor: TColor=clBtnFace); static;

    class procedure SetControlOneLine(ABaseControl: TControl; AControls: array of TControl); static; {多个组件对齐为一行}

    class procedure SetSpeedButtonImage(AButton: TSpeedButton; AImageList: TImageList; AIndex: Integer);static;

    class function GetParentForm(AComp:TWinControl):TForm; static;
    class function SetFocusAControl(AControl: TWinControl): Boolean; static;

    /// <summary>
    /// 根据保存的字符串构建字体
    /// </summary>
    /// <param name="AFont">目标字体</param>
    /// <param name="ASaveStr">保存的字符串: size|color|charset|fsBold|fsItalic|fsUnderline|fsStrikeOut|Name</param>
    /// <param name="defaultFontSize">默认字体大小</param>
    /// <param name="defaultCharset">默认字体集</param>
    class procedure SetFontFromString(var AFont: TFont; ASaveStr: String;
      defaultFontSize: Integer; defaultCharset: TFontCharset); static;

    /// <summary>
    /// 将Font保存为一个字符串： size|color|charset|fsBold|fsItalic|fsUnderline|fsStrikeOut|Name
    /// <seealso>SetFontFromString</seealso>
    /// </summary>
    /// <param name="AFont">源字体</param>
    /// <returns>保存字符串</returns>
    class function FontToString(const AFont: TFont): String; static;

    /// <summary>
    /// 将输入热键分解为状态(shift, control, alt)以及按键(key)
    /// </summary>
    /// <param name="aHotKey">输入热键</param>
    /// <param name="shift">状态</param>
    /// <param name="Key">按键</param>
    class procedure SplitHotKeyInfo(var aHotKey, shift, Key: word); static;

    // 判断当前光标是否在 AListView 的 AColumnIndex 列
    // X 是 OnMouseMove 等事件的 X值
    class function MouseIsInListColumn(AListView: TListView; AColumnIndex: Integer; X, Y: Integer): Boolean; static;
    class function MouseInListColumnIndex(AListView: TListView; X, Y: Integer): Integer; static;
    class function MouseInClient(AControl: TControl): Boolean; static;

    //判断光标是否在当前组件的某个区域
    class function MouseInClientRect(AControl: TControl; ARect: TRect): Boolean; static;

    //判断一个组件是否在一堆列表中： fcVCL.Vcl.ComponentInArray(A, [B, C])>=0
    class function ComponentInArray(AComponent: TComponent; AArrs: array of TComponent): Integer; static;

    {按顺序设置几个Panel的位置}
    class procedure UpdateAlignTopPanelOrder(APanels: array of TControl); static;
    class procedure UpdateAlientRigthPanelOrder(ABKPanel: TControl; APanels: array of TControl); static;
  end;

  RichEdit = record
    class procedure RichEditSetLineText(ARichEditor: TRichEdit; ATextList: TStringList); overload; static;
    class procedure RichEditSetLineText(ARichEditor: TRichEdit; AText: String; const ACanAutoSelect: Boolean=True); overload; static;
    class procedure RichEditSetStrings(ARichEditor: TRichEdit; AText: String; const ACanAutoSelect: Boolean); static;
    class procedure RichEditAutoSelText(ARichEditor: TRichEdit); static;
    class function  GetRichEditSelText(ARichEdit: TRichEdit): string; static;
  end;

  Form = class
    class procedure SetFormRegionWithBitmap(AFormHandle: THandle; ABitmap: TBitmap; const ABKColor: TColor=clFuchsia{ABitmap.Canvas.Pixels[0, 0]}); static;
  end;

implementation

uses
  fcStr, fcRTTI, Math;

{ vcl }

class function vcl.GetParentForm(AComp: TWinControl): TForm;
var
  comp: TWinControl;
begin
	comp := AComp.Parent;
	while(comp <> nil) and (not (comp is TForm)) do
		comp := comp.Parent;

	if(comp = nil) then
		Result := nil
	else
		Result := comp as TForm;
end;

class procedure vcl.HidePageControlTab(const APageControl: TPageControl);
var
  I: Integer;
begin
  with APageControl do
  begin
    Style := tsFlatButtons;
    for I := APageControl.PageCount - 1 downto 0 do
    begin
      Pages[I].TabVisible := false;
    end;
    ActivePageIndex := 0;
  end;
end;

class procedure vcl.ShowAFormInAPanel(AForm: TForm; APanel: TPanel);
begin
  ShowAFormInControl(AForm, APanel);
end;

class procedure vcl.ShowAFormInControl(AForm: TForm;
  AControl: TWinControl);
begin
  AForm.Parent := AControl;
  AForm.BorderStyle := bsNone;
  AForm.Align := alClient;
  AForm.Show;
end;

class function vcl.ImageListLoadPictureFile(AImageList: TImageList;
  AFileName: String; const AClearOrgImage: Boolean): Boolean;
var
  I, l_X, l_Y, l_Row, l_Col: Integer;
  l_SrcPic, l_ABMP: TBitmap;
const
  l_BKColor = clFuchsia;
begin
  if not FileExists(AFileName) then
    Exit(False);

  Result := True;
  if AClearOrgImage then
    AImageList.Clear;

  l_ABMP  := TBitmap.Create;
  l_SrcPic := TBitmap.Create;
  try
    l_SrcPic.LoadFromFile(AFileName);
    l_Row := l_SrcPic.Height  div AImageList.Height;  //多少行
    l_Col := l_SrcPic.Width div AImageList.Width;     //多少列
    l_ABMP.Width := AImageList.Width;
    l_ABMP.Height := AImageList.Height;

    for I := 0 to l_Col * l_Row-1 do
    begin
      l_X := (I mod l_Col) * AImageList.Width;
      l_Y := (I div l_Col) * AImageList.Height;
      BitBlt(l_ABMP.Canvas.Handle, 0, 0, AImageList.Width, AImageList.Height, l_SrcPic.Canvas.Handle, l_X, l_Y, SRCCOPY);
      AImageList.AddMasked(l_ABMP, l_BKColor);
    end;
  finally
    l_ABMP.Free;
    l_SrcPic.Free;
  end;
end;

class function Vcl.MouseInClient(AControl: TControl): Boolean;
var
  l_P: TPoint;
begin
  GetCursorPos(l_P);
  l_P := AControl.ScreenToClient(l_P);
  Result := PtInRect(AControl.ClientRect, l_P);
end;

class function Vcl.MouseInClientRect(AControl: TControl; ARect: TRect): Boolean;
var
  l_Rect: TRect;
begin
  l_Rect.TopLeft     := AControl.ClientToScreen(ARect.TopLeft);
  l_Rect.BottomRight := AControl.ClientToScreen(ARect.BottomRight);
  Result := PtInRect(l_Rect, Mouse.CursorPos);
end;

class function Vcl.MouseInListColumnIndex(AListView: TListView; X,
  Y: Integer): Integer;
var
  I, l_Left, l_Right: Integer;
begin
  l_Left := 0;
  for I := 0 to AListView.Columns.Count-1 do
  begin
    l_Right := l_Left + AListView.Columns[I].Width;
    if (X>=l_Left) and (X < l_Right) then
    begin
      if AListView.GetItemAt(X, Y)<>nil then
        Exit(I)
      else
        Exit(-1);
    end;
    l_Left := l_Right;
  end;
  Exit(-1);
end;

class function Vcl.MouseIsInListColumn(AListView: TListView;
  AColumnIndex: Integer; X, Y: Integer): Boolean;
  procedure _LeftRight(var ALeft, ARight: Integer);
  var
    I: Integer;
  begin
    ALeft := 0;
    ARight := 0;
    for I := 0 to AColumnIndex do
    begin
      if I>=AListView.Columns.Count then
        Break;
      if I<AColumnIndex then
        ALeft := ALeft + AListView.Columns[I].Width
      else
        ARight := AListView.Columns[I].Width;
    end;
    ARight := ARight + ALeft;
  end;
var
  l_Item: TListItem;
  l_Left, l_Right: Integer;
begin
  _LeftRight(l_Left, l_Right);
  if (X>l_Left) and (X<l_Right) then
  begin
    l_Item := AListView.GetItemAt(X, Y);
    if l_Item<>nil then
      Exit(True);
  end;
  Result := False;
end;

class procedure Vcl.SetControlEnable(AControl: TWinControl; AEnabled: Boolean;
  const AEnableColor, ADisableColor: TColor);
begin
  AControl.Enabled := AEnabled;
  if AEnabled then
    fcRTTI.Rtti.SetPropValue(AControl, 'Color', AEnableColor)
  else
    fcRTTI.Rtti.SetPropValue(AControl, 'Color', ADisableColor);
end;

class procedure Vcl.SetControlOneLine(ABaseControl: TControl; AControls: array of TControl);
var
  I: Integer;
begin
  for I := Low(AControls) to High(AControls) do
    AControls[I].Top := ABaseControl.Top + (ABaseControl.Height-AControls[I].Height) div 2;
end;

class procedure vcl.SetControlVisible(AForm: TForm; ATag: Integer;
  AVisible: Boolean);
var
  I: Integer;
  comp: TControl;
begin
  for I := 0 to AForm.ControlCount - 1 do
  begin
    comp := AForm.Controls[I];
    comp.Visible := comp.Tag <> ATag;
  end;
end;

class procedure Vcl.SetSpeedButtonImage(AButton: TSpeedButton;
  AImageList: TImageList; AIndex: Integer);
var
  l_bmp: Graphics.TBitmap;
  l_Action: TAction;
  l_ActionList: TActionList;
  l_ClickEvent: TNotifyEvent;
  l_Caption, l_Hint: String;
begin
{SpeedButton.Glyph 可以放置4种属性的图：Normal, Disable, Click, Down}
  if AIndex < 0 then
    Exit;

  AButton.NumGlyphs := 1;
  AButton.Glyph.Assign(nil);

  l_ClickEvent := AButton.OnClick;
  l_Caption := AButton.Caption;
  l_Hint := AButton.Hint;

  try
    l_ActionList := TActionList.Create(nil);
    l_ActionList.Images := AImageList;
    try
      l_Action := TAction.Create(l_ActionList);
      l_Action.ActionList := l_ActionList;
      l_Action.Enabled := AButton.Enabled;
      l_Action.Visible := AButton.Visible;
      try
        l_Action.ImageIndex := AIndex;
        l_Action.OnExecute := l_ClickEvent;
        AButton.Action := l_Action;
      finally
        l_Action.Free;
      end;
    finally
      l_ActionList.Free;
    end;
  except
    l_bmp := Graphics.TBitmap.Create;
    try
      AImageList.GetBitmap(AIndex, l_bmp);
      l_bmp.Transparent := true;
      AButton.Glyph.Assign(l_bmp);
    finally
      l_bmp.Free;
    end;
  end;

  AButton.OnClick := l_ClickEvent;
  AButton.Caption := l_Caption;
  AButton.Hint := l_Hint;
end;

class procedure vcl.ShowControlPopupMenu(ctlShow: TControl; X, Y: Integer;
  pmShow: TPopupMenu);
//X, Y是ctlShow上的偏移位置
var
  Point:TPoint;
begin
  Point.x := X;
  Point.y := Y;
  Point := ctlShow.ClientToScreen(Point);
  pmShow.Popup(Point.x,Point.y);
end;

class procedure vcl.ShowControlPopupMenu(ctlShow: TControl; pmShow: TPopupMenu);
var
  Point:TPoint;
begin
  Point.x := 0;
  Point.y := ctlShow.Height;
  Point := ctlShow.ClientToScreen(Point);

  if Point.X > Screen.Width - 130 then
    Point.X := Point.X + ctlShow.Width;

  pmShow.Popup(Point.x,Point.y);
end;

class procedure vcl.ShowPopupMenuAtCursorPosition(APopupMenu: TPopupMenu);
var
  p: TPoint;
begin
  GetCursorPos(p);
  APopupMenu.Popup(p.x, p.y);
end;

class function Vcl.SetFocusAControl(AControl: TWinControl): Boolean;
begin
  if AControl.Visible and AControl.Enabled then
  begin
    Result := Windows.SetFocus(AControl.Handle) > 0;
//    if not Result then
//      try
//        AControl.SetFocus;
//        Result := True;
//      except
//      end;
  end
  else
    Result := False;
end;

class procedure Vcl.SetFontFromString(var AFont: TFont; ASaveStr: String; defaultFontSize: Integer;
  defaultCharset: TFontCharset);
var
  l_List: TStringList;
begin
  l_List := TStringList.Create;
  Str.GetTokenToList(ASaveStr, '|', l_List, 8);

  AFont.Size := StrToIntDef(l_List[0], defaultFontSize);
  AFont.Color := StrToIntDef(l_List[1], AFont.Color);
  AFont.Charset := StrToIntDef(l_List[2], defaultCharset);
  AFont.Style := [];
  if l_List[3] = '1' then
    AFont.Style := AFont.Style + [fsBold];
  if l_List[4] = '1' then
    AFont.Style := AFont.Style + [fsItalic];
  if l_List[5] = '1' then
    AFont.Style := AFont.Style + [fsUnderline];
  if l_List[6] = '1' then
    AFont.Style := AFont.Style + [fsStrikeOut];
  if l_List[7] <> '' then
    AFont.Name := l_List[7];

  l_List.Free;
end;

class function Vcl.ComponentInArray(AComponent: TComponent; AArrs: array of TComponent): Integer;
var
  I: Integer;
begin
  for I := Low(AArrs) to High(AArrs) do
    if AArrs[I] = AComponent then
      Exit(I);

  Result := -1;
end;

class function Vcl.FontToString(const AFont: TFont): String;
begin
  Result := Format('%d|%d|%d|%d|%d|%d|%d|%s', [AFont.Size, AFont.Color, AFont.Charset,
    Integer(fsBold in AFont.Style), Integer(fsItalic in AFont.Style),
    Integer(fsUnderline in AFont.Style), Integer(fsStrikeOut in AFont.Style), AFont.Name]);
end;

class procedure Vcl.SplitHotKeyInfo(var aHotKey, shift, Key: word);
begin
  case aHotKey of
    $2000 .. $4000:
      begin
        aHotKey := aHotKey - $2000;
        shift := shift + MOD_SHIFT;
        SplitHotKeyInfo(aHotKey, shift, Key);
      end;
    $4001 .. $8000:
      begin
        aHotKey := aHotKey - $4000;
        shift := shift + MOD_CONTROL;
        SplitHotKeyInfo(aHotKey, shift, Key);
      end;
    $8001 .. $FFFF:
      begin
        aHotKey := aHotKey - $8000;
        shift := shift + MOD_ALT;
        SplitHotKeyInfo(aHotKey, shift, Key);
      end;
  else
    Key := aHotKey;
  end;
end;

class procedure Vcl.UpdateAlientRigthPanelOrder(ABKPanel: TControl; APanels: array of TControl);
var
  l_List: TList;
  I, l_UsedLen, l_BorderWidth: Integer;
  l_c: TControl;
begin
  {APanels: 从右向左的顺序}
  l_List := TList.Create;
  try
    for I := High(APanels) to Low(APanels) do
      if APanels[I].Visible then
      begin
        l_List.Insert(0, APanels[I]);
        APanels[I].Align := alNone;
      end;

    if l_List.Count >0 then
    begin
      l_BorderWidth := 0;
      if ABKPanel is TPanel then
        l_BorderWidth := TPanel(ABKPanel).BorderWidth;

      l_UsedLen := 0;
      while l_List.Count > 0 do
      begin
        l_c := l_List[0];
        l_List.Delete(0);
        l_c.Left := ABKPanel.Width-l_BorderWidth - l_UsedLen - l_c.Width;
        l_c.Align := alRight;
        l_UsedLen := l_UsedLen + l_c.Width;
      end;
    end;
  finally
    l_List.Free;
  end;
end;

class procedure Vcl.UpdateAlignTopPanelOrder(APanels: array of TControl);
var
  l_List: TList;
  I, l_Top: Integer;
  l_FindErr: Boolean;
begin
  l_List := TList.Create;
  try
    for I := Low(APanels) to High(APanels) do
      if APanels[I].Visible then
      begin
        l_List.Add(APanels[I]);
      end;

    {判断一下，如果当前位置正确，则不动}
    l_FindErr := false;
    for I := 0 to l_List.Count - 2 do
    begin
      if TControl(l_List[I]).Top > TControl(l_List[I+1]).Top then
      begin
        l_FindErr := True;
        Break;
      end;
    end;

    if l_FindErr and (l_List.Count >0) then
    begin
      l_Top := TControl(l_List[0]).Top+TControl(l_List[0]).Height;
      for I := 1 to l_List.Count -1 do
      begin
        TControl(l_List[I]).Top := l_Top;
        l_Top := l_Top + TControl(l_List[I]).Height;
      end;
    end;
  finally
    l_List.Free;
  end;
end;

{ RichEdit }

class function RichEdit.GetRichEditSelText(ARichEdit: TRichEdit): string;
var
  ps : array[0..4096] of char;
begin
  //  l_Text := Trim(AEdit.SelText);  //某些俄文的邮件，访问SelText直接崩溃

  SendMessage(ARichEdit.Handle, WM_USER + 62{EM_GETSELTEXT}, 0, lParam(@ps[0]));
  Result :=  StrPas(ps);
end;

class procedure RichEdit.RichEditAutoSelText(ARichEditor: TRichEdit);
var
  l_OrgColor, l_First, l_End, l_LastSelStart: Integer;
  l_Text: String;
begin
  if ARichEditor.SelLength > 0 then
  begin
    Exit;
{    ARichEditor.SelStart := ARichEditor.SelStart + ARichEditor.SelLength div 2;
    ARichEditor.SelLength := 0;}
  end;

  ARichEditor.Lines.BeginUpdate;
  try
    l_OrgColor := ARichEditor.SelAttributes.Color;
    if l_OrgColor = ARichEditor.Font.Color then
      Exit;

    while ARichEditor.SelStart > 0 do
    begin
      if l_OrgColor = ARichEditor.SelAttributes.Color  then
      begin
        l_LastSelStart := ARichEditor.SelStart;
        ARichEditor.SelStart := l_LastSelStart -1;
        if (l_LastSelStart = ARichEditor.SelStart)
          and (ARichEditor.SelStart>0) then
        begin
          ARichEditor.SelStart := l_LastSelStart -2;
        end;
      end
      else
        Break;
    end;
    l_First := ARichEditor.SelStart;

    l_LastSelStart := ARichEditor.SelStart;
    l_Text := TrimRight(ARichEditor.Text);
    while ARichEditor.SelStart<Length(l_Text) do
    begin
      ARichEditor.SelStart := ARichEditor.SelStart +1;
      if (l_LastSelStart = ARichEditor.SelStart)
        and (ARichEditor.SelStart< Length(l_Text)) then
      begin
        ARichEditor.SelStart := ARichEditor.SelStart +2;
        if l_LastSelStart = ARichEditor.SelStart then
        begin
          l_LastSelStart := ARichEditor.SelStart +2;
          Break;
        end;
      end;

      if l_OrgColor <> ARichEditor.SelAttributes.Color  then
        Break
      else
        l_LastSelStart := ARichEditor.SelStart;
    end;
    l_End := l_LastSelStart;

    ARichEditor.SelStart := l_First;
    ARichEditor.SelLength := l_End-l_First;
  finally
    ARichEditor.Lines.EndUpdate;
  end;
end;

class procedure RichEdit.RichEditSetLineText(ARichEditor: TRichEdit;
  ATextList: TStringList);
var
  l_Idx, l_SelStart: Integer;
  l_Text: String;

  l_GRB  : Integer;
  b, g, r: Integer;
  l_Hex : String;
begin
  l_GRB := ColorToRGB(ARichEditor.Font.Color);
  l_Hex := IntToHex(l_GRB, 6); //The result will be BBGGRR
  b := StrToIntDef('$'+Copy(l_Hex, 1,2), 0);
  g := StrToIntDef('$'+Copy(l_Hex, 3,2), 0);
  r := StrToIntDef('$'+Copy(l_Hex, 5,2), 0);

  ARichEditor.Lines.BeginUpdate;
  try
    ARichEditor.Lines.Clear;
    l_Text := '';
    for l_Idx := 0 to ATextList.Count -1 do
    begin
      if ATextList[l_Idx]='' then
        Continue;
      l_Text := l_Text + ATextList[l_Idx];
    end;
    ARichEditor.Text := l_Text;
    l_SelStart := 0;
    for l_Idx := 0 to ATextList.Count -1 do
    begin
      if ATextList[l_Idx]='' then
        Continue;

      ARichEditor.SelStart := l_SelStart;
      ARichEditor.SelLength := Length(ATextList[l_Idx]);
      with ARichEditor.SelAttributes do
      begin
        Name  := ARichEditor.Font.Name;
        Style := ARichEditor.Font.Style;
//        ARichEditor.SelAttributes
        if Integer(ATextList.Objects[l_Idx])>0 then
        begin
          case l_Idx mod 3 of
            0: if b >0 then dec(b) else inc(b);
            1: if g >0 then dec(g) else inc(g);
            2: if r >0 then dec(r) else inc(r)
          end;

          Color := RGB(r, g, b);
          Size  := ARichEditor.Font.Size;// GlobalOptions.ApplicationFontInfo.Size;
        end
        else
        if Integer(ATextList.Objects[l_Idx])=0 then
        begin
          Color := ARichEditor.Font.Color;
          Size  := ARichEditor.Font.Size;// GlobalOptions.ApplicationFontInfo.Size;
        end
        else
        begin
          Color := ABS(Integer(ATextList.Objects[l_Idx]));    //小于0 则直接指明颜色
          Size  := ARichEditor.Font.Size;
        end;

        //Protected := True;
      end;

      Inc(l_SelStart, Length(ATextList[l_Idx]));
      ARichEditor.SelLength := 0;
    end;

    ARichEditor.SelStart := 0;
    ARichEditor.SelLength := 0;
  finally
    ARichEditor.Lines.EndUpdate;
  end;
end;

class procedure RichEdit.RichEditSetLineText(ARichEditor: TRichEdit;
  AText: String; const ACanAutoSelect: Boolean);
var
  l_List: TStringList;
begin
  l_List := TStringList.Create;
  try
    l_List.AddObject(AText, Pointer(Integer(ACanAutoSelect)));
    RichEditSetLineText(ARichEditor, l_List);
  finally
    l_List.Free;
  end;
end;

class procedure RichEdit.RichEditSetStrings(ARichEditor: TRichEdit;
  AText: String; const ACanAutoSelect: Boolean);
var
  l_List: TStringList;
  I: Integer;
begin
  l_List := TStringList.Create;
  try
    ExtractStrings([';',','], [], PChar(AText), l_List);
    for I := 0 to l_List.Count - 1 do
      l_List.Objects[I] := Pointer(1);
    RichEditSetLineText(ARichEditor, l_List);
  finally
    l_List.Free;
  end;
end;

{ Form }

class procedure Form.SetFormRegionWithBitmap(AFormHandle: THandle; ABitmap: TBitmap;
  const ABKColor: TColor);

  function _BitmapToRegion(bmp: TBitmap; TransparentColor: TColor = clBlack;
    RedTol: Byte = 1; GreenTol: Byte = 1; BlueTol: Byte = 1): HRGN;
  const
    AllocUnit = 100;
  type
    PRectArray = ^TRectArray;
    TRectArray = Array [0 .. (MaxInt div SizeOf(TRect)) - 1] of TRect;
  var
    pr: PRectArray;
    h: HRGN;
    RgnData: PRgnData;
    lr, lg, lb, hr, hg, hb: Byte;
    x, y, x0: Integer;
    b: PByteArray;
    ScanLinePtr: Pointer;
    ScanLineInc: Integer;
    maxRects: Cardinal;
  begin
    Result := 0;
    { Keep on hand lowest and highest values for the "transparent" pixels }
    lr := GetRValue(TransparentColor);
    lg := GetGValue(TransparentColor);
    lb := GetBValue(TransparentColor);
    hr := Min($FF, lr + RedTol);
    hg := Min($FF, lg + GreenTol);
    hb := Min($FF, lb + BlueTol);

    bmp.PixelFormat := pf32bit;

    maxRects := AllocUnit;
    GetMem(RgnData, SizeOf(RGNDATAHEADER) + (SizeOf(TRect) * maxRects));
    try
      with RgnData^.rdh do
      begin
        dwSize := SizeOf(RGNDATAHEADER);
        iType := RDH_RECTANGLES;
        nCount := 0;
        nRgnSize := 0;
        SetRect(rcBound, MAXLONG, MAXLONG, 0, 0);
      end;

      ScanLinePtr := bmp.ScanLine[0];
      ScanLineInc := Integer(bmp.ScanLine[1]) - Integer(ScanLinePtr);
      for y := 0 to bmp.Height - 1 do
      begin
        x := 0;
        while x < bmp.Width do
        begin
          x0 := x;
          while x < bmp.Width do
          begin
            b := @PByteArray(ScanLinePtr)[x * SizeOf(TRGBQuad)];
            // BGR-RGB: Windows 32bpp BMPs are made of BGRa quads (not RGBa)
            if (b[2] >= lr) and (b[2] <= hr) and (b[1] >= lg) and (b[1] <= hg)
              and (b[0] >= lb) and (b[0] <= hb) then
              Break; // pixel is transparent
            Inc(x);
          end;
          { test to see if we have a non-transparent area in the image }
          if x > x0 then
          begin
            { increase RgnData by AllocUnit rects if we exceeds maxRects }
            if RgnData^.rdh.nCount >= maxRects then
            begin
              Inc(maxRects, AllocUnit);
              ReallocMem(RgnData,
                SizeOf(RGNDATAHEADER) + (SizeOf(TRect) * maxRects));
            end;
            { Add the rect (x0, y)-(x, y+1) as a new visible area in the region }
            pr := @RgnData^.Buffer; // Buffer is an array of rects
            with RgnData^.rdh do
            begin
              SetRect(pr[nCount], x0, y, x, y + 1);
              { adjust the bound rectangle of the region if we are "out-of-bounds" }
              if x0 < rcBound.Left then
                rcBound.Left := x0;
              if y < rcBound.Top then
                rcBound.Top := y;
              if x > rcBound.Right then
                rcBound.Right := x;
              if y + 1 > rcBound.Bottom then
                rcBound.Bottom := y + 1;
              Inc(nCount);
            end;
          end; // if x > x0

          if RgnData^.rdh.nCount = 2000 then
          begin
            h := ExtCreateRegion(nil,
              SizeOf(RGNDATAHEADER) + (SizeOf(TRect) * maxRects), RgnData^);
            if Result > 0 then
            begin // Expand the current region
              CombineRgn(Result, Result, h, RGN_OR);
              DeleteObject(h);
            end
            else // First region, assign it to Result
              Result := h;
            RgnData^.rdh.nCount := 0;
            SetRect(RgnData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
          end;
          Inc(x);
        end; // scan every sample byte of the image
        Inc(Integer(ScanLinePtr), ScanLineInc);
      end;
      { need to call ExCreateRegion one more time because we could have left }
      { a RgnData with less than 2000 rects, so it wasn't yet created/combined }
      h := ExtCreateRegion(nil,
        SizeOf(RGNDATAHEADER) + (SizeOf(TRect) * maxRects), RgnData^);
      if Result > 0 then
      begin
        CombineRgn(Result, Result, h, RGN_OR);
        DeleteObject(h);
      end
      else
        Result := h;
    finally
      FreeMem(RgnData, SizeOf(RGNDATAHEADER) + (SizeOf(TRect) * maxRects));
    end;
  end;

var
  l_rgn: HRGN;
begin
  l_rgn := _BitmapToRegion(ABitmap, ABKColor);
  SetWindowRgn(AFormHandle, l_rgn, true);
end;

end.
