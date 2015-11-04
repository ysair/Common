unit dmVclUtils;

interface
uses Forms, ExtCtrls, Windows, Controls, ImgList, SysUtils, Classes, Graphics,
  ComCtrls, Menus;


procedure dmvclShowAFormInControl(AForm: TForm; AControl: TWinControl);
procedure dmvclShowAFormInAPanel(AForm: TForm; APanel: TPanel);
function ImageListLoadPictureFile(AImageList: TImageList; AFileName: String; const AClearOrgImage: Boolean=true): Boolean;
procedure dmvclHidePageControlTab(APageControl: TPageControl);

procedure ShowControlPopupMenu(ctlShow:TControl; pmShow: TPopupMenu); overload;
procedure ShowControlPopupMenu(ctlShow: TControl; X, Y : Integer;pmShow:TPopupMenu); overload;
procedure ShowPopupMenuAtCursorPosition(APopupMenu: TPopupMenu);

procedure SetControlVisible(AForm:TForm;ATag: Integer; AVisible: Boolean);

function dmGetParentForm(AComp:TWinControl):TForm;

implementation

uses
  fcVCL;

procedure dmvclShowAFormInAPanel(AForm: TForm; APanel: TPanel);
begin
  dmvclShowAFormInControl(AForm, APanel);
end;

procedure dmvclShowAFormInControl(AForm: TForm; AControl: TWinControl);
begin
  AForm.Parent := AControl;
  AForm.BorderStyle := bsNone;
  AForm.Align := alClient;
  AForm.Show;
end;


function ImageListLoadPictureFile(AImageList: TImageList; AFileName: String;
  const AClearOrgImage: Boolean): Boolean;
var
  I, l_X, l_Y, l_Row, l_Col: Integer;
  l_SrcPic, l_ABMP: TBitmap;
const
  l_BKColor = clFuchsia;
begin
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

procedure dmvclHidePageControlTab(APageControl: TPageControl);
begin
  Vcl.HidePageControlTab(APageControl);
end;

procedure ShowControlPopupMenu(ctlShow:TControl; pmShow: TPopupMenu);
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

procedure ShowPopupMenuAtCursorPosition(APopupMenu: TPopupMenu);
var
  p: TPoint;
begin
  GetCursorPos(p);
  APopupMenu.Popup(p.x, p.y);
end;

//同上，X, Y是ctlShow上的偏移位置
procedure ShowControlPopupMenu(ctlShow: TControl; X, Y : Integer;pmShow:TPopupMenu);
var
  Point:TPoint;
begin
  Point.x := X;
  Point.y := Y;
  Point := ctlShow.ClientToScreen(Point);
  pmShow.Popup(Point.x,Point.y);
end;

procedure SetControlVisible(AForm:TForm;ATag: Integer; AVisible: Boolean);
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

function dmGetParentForm(AComp:TWinControl):TForm;
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

end.
