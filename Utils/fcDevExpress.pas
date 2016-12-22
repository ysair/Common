unit fcDevExpress;

interface

uses
  dxBar, Classes, Controls, dxRibbon, cxGridTableView, Variants, SysUtils, Types,
  dxGDIPlusAPI, dxGDIPlusClasses, ImgList, ComCtrls, Graphics, cxClasses,
  dxCore, dxCoreGraphics, cxGraphics, Windows, ExtCtrls, cxImage, Forms;

type
  TdxBarAccess = class(TdxBar);

  Dev = record
    class procedure ImageListDpiCovert(AImageList: TcxImageList; AImgListWidthIn96Dpi, AImgListHeightIn96Dpi: Integer; const AConvertOrgImagesInList: Boolean=True); static;

    /// <summary>
    /// 设置BarItem的Visible
    /// </summary>
    /// <param name="AItem"></param>
    /// <param name="AValue"></param>
    /// <returns></returns>
    class function SetItemVisible(AItem: TdxBarItem; AValue: Boolean): Boolean; static;

    /// <summary>
    /// 在TargetLink集合中，查找连接到TargetItem的链接
    /// </summary>
    /// <param name="pmiTargetLinks"></param>
    /// <param name="pmiTargetItem"></param>
    /// <returns></returns>
    class function FindItemLink(pmiTargetLinks: TdxBarItemLinks;
      pmiTargetItem: TdxBarItem): TdxBarItemLink; static;

      /// <summary>
      /// 将pmiSource中的Links复制并合并到pmiTarget中去。
      /// </summary>
      /// <param name="ATarget"></param>
      /// <param name="ASource"></param>
    class procedure CopyItemLinks(ATarget: TdxBarSubItem; ASource: TdxBarSubItem); overload; static;

    class procedure BuildMenuURM(AParentItem: TdxBarSubItem; AAddToBelowItem: TdxBarItem;
      // 在哪个Item上面，如果是nil则添加在最后面
      AURMList: TStringList; // 内容
      AOnClick: TNotifyEvent; // 点击事件
      const ADefaultDataID: Integer = -1; // 默认的选择
      const AGroupID: Integer = 99; // 设置默认的GroupID，如果同一个菜单中有2组是自动生成的，则要指明不同的GroupID
      const ADataIsImageIndex: Boolean = false; // 保存在 Integer(AURMList.Objects[i]) 的值是图片ID
      const AImageList: TImageList = nil // 生成的Item使用的ImageList
      ); static;

    /// <summary>
    /// 将ASource中的Links复制到ARibbon相应Tab中的一个ToolBar上。
    /// </summary>
    /// <param name="ARibbon"></param>
    /// <param name="ASource"></param>
    class procedure BuildRibbon(ARibbon: TdxRibbon; ASource: TdxBarSubItem); static;

    /// <summary>
    /// 将SourceRibbon中的Tab，Toolbar，Item合并到TargetRibbon中。
    /// </summary>
    /// <param name="SourceRibbon"></param>
    /// <param name="TargetRibbon"></param>
    /// <param name="TargetManager"></param>
    class procedure MergeRibbon(SourceRibbon, TargetRibbon: TdxRibbon; TargetManager: TdxBarManager); static;

    /// <summary>
    /// 将Bar中的那些BarManager与该Bar不一致的Item删除。这些Item往往是从其他窗口合并过来的。
    /// </summary>
    /// <param name="ABar"></param>
    class procedure ClearBar(ABar: TdxBar); static;


    //读取一个值
    class function GetSelectedRecordValue(AList: TcxGridTableView; AColumn: TcxGridColumn): Variant; static;
    class function GetSelectedRecordIntValue(AList: TcxGridTableView; AColumn: TcxGridColumn): Int64; static;
    class function GetSelectedRecordStrValue(AList: TcxGridTableView; AColumn: TcxGridColumn): string; static;

    //定位到一条记录
    class function FocusToARecord(AList: TcxGridTableView; AColumn: TcxGridColumn; AValue: Variant; const AFocusTo1stIfNotFind: Boolean=True): Boolean; static;

    class procedure SetVisible(AItem: TdxBarItem; AVisible: Boolean); static;

    class function ImageListLoadPictureFile(AImageList: TcxImageList; AFileName: String; const AClearOrgImage: Boolean): Boolean; static;
    class function ImageListLoadPictureFileWithStretch(AImageList: TcxImageList; AFileName: String; const AImgFileOneIconWidth, AImgFileOneIconHeight: Integer; const AClearOrgImage: Boolean): Boolean; static;

    class function GetImageInfoFromFile(const AFileName: string; AImageInfo: TcxImageInfo): Boolean; static;


    class function DrawFromImageList(AImageList: TcxImageList; AImage: TImage; AIndex: Integer; ABkColor: TColor): Boolean; overload; static;
    class function DrawFromImageList(AImageList: TcxImageList; AImage: TImage; AIndex: Integer; ABkDc: THandle; ABk_X, ABk_Y: Integer): Boolean; overload; static;
    class function DrawFromImageList(AImageList: TcxImageList; AImage: TcxImage; AIndex: Integer; ABkColor: TColor): Boolean; overload; static;
    class function DrawFromImageList(AImageList: TcxImageList; AImage: TcxImage; AIndex: Integer; ABkDc: THandle; ABk_X, ABk_Y: Integer): Boolean; overload; static;

    {ABkDC 和 ABkColor二选一，作为背景来处理}
    class function DrawFromImageListExt(AImages: TCustomImageList; AImageIndex: Integer;
        const ABkDC: THandle=0; const ABk_X:Integer=0; const ABk_Y: Integer=0;  {Line1, 二选一}
        const ABkColor: TColor=clWhite;                                         {Line2, 二选一}
        const ASmoothImage: Boolean = False;
        const AOutBmpWidth: Integer=0; const AOutBmpHeigth: Integer=0): TcxAlphaBitmap; overload; static;
  end;


implementation
uses Math, cxGeometry;

{ Dev }

class procedure Dev.CopyItemLinks(ATarget, ASource: TdxBarSubItem);
var
  link: TdxBarItemLink;
  i: Integer;
  bStartGroup: Boolean;
begin
  bStartGroup := ATarget.ItemLinks.Count > 0;

  for i := 0 to ASource.ItemLinks.Count - 1 do
  begin
    link := ATarget.ItemLinks.Add;
    link.Assign(ASource.ItemLinks[i]);
    if 0 = i then
      link.BeginGroup := bStartGroup;
  end;

end;

class procedure Dev.BuildMenuURM(AParentItem: TdxBarSubItem;
  AAddToBelowItem: TdxBarItem; AURMList: TStringList; AOnClick: TNotifyEvent;
  const ADefaultDataID, AGroupID: Integer; const ADataIsImageIndex: Boolean;
  const AImageList: TImageList);
var
  i, pos: Integer;
  link: TdxBarItemLink;
  startGroup: Boolean;
begin
  for i := AParentItem.ItemLinks.Count - 1 downto 0 do
    if AParentItem.ItemLinks[i].item.Tag = AGroupID then
      AParentItem.ItemLinks.Delete(i);

  pos := -1;
  if AAddToBelowItem <> nil then
  begin
    for i := 0 to AParentItem.ItemLinks.Count - 1 do
      if AAddToBelowItem = AParentItem.ItemLinks[i].item then
      begin
        pos := i;
        Break;
      end;
  end;

  startGroup := false;
  for i := 0 to AURMList.Count - 1 do
  begin
    if AURMList[i] = '-' then
    begin
      startGroup := True;
      Continue;
    end;

    link := AParentItem.ItemLinks.AddButton;
    link.BeginGroup := startGroup;
    startGroup := false;

    if pos <> -1 then
      link.Index := pos;

    with link.item as TdxBarButton do
    begin
      Caption := AURMList[i];
      OnClick := AOnClick;
      Tag := AGroupID;

      // l_AItem.Checked     := (ADefaultDataID<>-1) and (ADefaultDataID=Integer(AURMList.Objects[i]));
      if ADataIsImageIndex then
      begin
        ImageIndex := Integer(AURMList.Objects[i]);
        BarManager.Images := AImageList;
      end
      else
      begin
        Data := AURMList.Objects[i];
      end;

      if ADefaultDataID = Integer(AURMList.Objects[i]) then
        ButtonStyle := bsChecked
      else
        ButtonStyle := bsDefault;
    end;

  end;
end;

class procedure Dev.BuildRibbon(ARibbon: TdxRibbon; ASource: TdxBarSubItem);
var
  i, J: Integer;
  bar: TdxBar;
  link: TdxBarItemLink;
begin
  for i := 0 to ARibbon.Tabs.Count - 1 do
  begin
    if ARibbon.Tabs[i].Name = ASource.Name then
    begin
      bar := ARibbon.BarManager.AddToolBar(false);
      for J := 0 to ASource.ItemLinks.Count - 1 do
      begin
        link := bar.ItemLinks.AddButton;
        link.Assign(ASource.ItemLinks[J]);
      end;

      with ARibbon.Tabs[i].Groups.Add do
      begin
        ToolBar := bar;
        Caption := '';
      end;
    end;
  end;
end;

class procedure Dev.ClearBar(ABar: TdxBar);
var
  i: Integer;
begin
  for i := ABar.ItemLinks.Count - 1 downto 0 do
    if ABar.ItemLinks[i].item.BarManager <> ABar.BarManager then
      ABar.ItemLinks.Delete(i);
end;

class function Dev.FindItemLink(pmiTargetLinks: TdxBarItemLinks;
  pmiTargetItem: TdxBarItem): TdxBarItemLink;
var
  i: Integer;
  item: TdxBarSubItem;
begin
  Result := nil;

  if pmiTargetLinks = nil then
    Exit;

  for i := 0 to pmiTargetLinks.Count - 1 do
  begin
    if pmiTargetLinks.Items[i].item = pmiTargetItem then
    begin
      Result := pmiTargetLinks.Items[i];
      Exit;
    end;

    if pmiTargetLinks.Items[i].item is TdxBarSubItem then
    begin
      item := pmiTargetLinks.Items[i].item as TdxBarSubItem;
      Result := FindItemLink(item.ItemLinks, pmiTargetItem);
      if Result <> nil then
        Exit;
    end;
  end;
end;

class function Dev.FocusToARecord(AList: TcxGridTableView;
  AColumn: TcxGridColumn; AValue: Variant;
  const AFocusTo1stIfNotFind: Boolean): Boolean;
var
  I: Integer;
begin
  for I := 0 to AList.DataController.RowCount -1 do
  begin
    if AList.DataController.Values[I, AColumn.Index] = AValue then
    begin
      AList.DataController.FocusedRecordIndex :=I;
      Exit(True);
    end;
  end;
  if AFocusTo1stIfNotFind and (AList.DataController.RowCount >0) then
  begin
    AList.DataController.FocusedRowIndex := 0;  //第一行
    Exit(True);
  end;

  Result := False;
end;

class function Dev.GetImageInfoFromFile(const AFileName: string;
  AImageInfo: TcxImageInfo): Boolean;
  function _GetClassName: TGraphicClass;
  begin
    if SameText(ExtractFileExt(AFileName), '.bmp') then
      Result := Graphics.TBitmap
    else
    if SameText(ExtractFileExt(AFileName), '.png') then
      Result := TdxPNGImage
    else
      Result := nil;
  end;
var
  l_Graphic: TGraphic;
  l_GraphicClass: TGraphicClass;
  l_Bmp : Graphics.TBitmap;
begin
  AImageInfo.Image := nil;
  AImageInfo.Mask := nil;
  AImageInfo.MaskColor := clNone;
  l_GraphicClass := _GetClassName;
  Result := l_GraphicClass <> nil;
  if Result then
  begin
    l_Graphic := l_GraphicClass.Create;
    try
      l_Graphic.LoadFromFile(AFileName);
      if l_Graphic is TdxPNGImage then
      begin
        l_Bmp := TdxPNGImage(l_Graphic).GetAsBitmap;
        try
          AImageInfo.Image := l_Bmp;
        finally
          l_Bmp.Free;
        end;
      end
      else if l_Graphic is Graphics.TBitmap then
        AImageInfo.Image := Graphics.TBitmap(l_Graphic);
    finally
      l_Graphic.Free;
    end;
  end;
end;

class function Dev.GetSelectedRecordIntValue(AList: TcxGridTableView;
  AColumn: TcxGridColumn): Int64;
begin
  Result := StrToIntDef(GetSelectedRecordStrValue(AList, AColumn), 0);
end;

class function Dev.GetSelectedRecordStrValue(AList: TcxGridTableView;
  AColumn: TcxGridColumn): string;
begin
  Result := VarToStrDef(GetSelectedRecordValue(AList, AColumn), '');
end;

class function Dev.GetSelectedRecordValue(AList: TcxGridTableView;
  AColumn: TcxGridColumn): Variant;
begin
  Result := null;
  if AList.Controller.SelectedRecordCount>0 then
    Result := AList.Controller.SelectedRecords[0].Values[AColumn.Index];
end;

class function Dev.DrawFromImageList(AImageList: TcxImageList;
  AImage: TImage; AIndex: Integer; ABkColor: TColor): Boolean;
var
  l_bmp: TcxAlphaBitmap;
begin
  l_bmp := DrawFromImageListExt(AImageList, AIndex, 0, 0, 0, ABkColor);
  Result := Assigned(l_bmp);
  if Result then
  begin
    AImage.Picture.Bitmap.Assign(l_Bmp);
    l_bmp.Free;
  end;
{  AImage.Picture.Bitmap.Width := AImageList.Width+1;
  AImage.Picture.Bitmap.Height := AImageList.Height+1;

  AImage.Picture.Bitmap.Canvas.Brush.Color := ABkColor;
  AImage.Picture.Bitmap.Canvas.FillRect(Rect(0,0, AImageList.Width+1, AImageList.Height+1));
  AImage.Transparent := True;
  AImageList.Draw(AImage.Picture.Bitmap.Canvas, 1,1, AIndex, dsTransparent, itImage, True);
  AImage.Picture.Bitmap.Transparent := True;
  Result := True;}
end;

class function Dev.DrawFromImageList(AImageList: TcxImageList;
  AImage: TcxImage; AIndex: Integer; ABkColor: TColor): Boolean;
var
  l_bmp: TcxAlphaBitmap;
begin
  l_bmp := DrawFromImageListExt(AImageList, AIndex, 0, 0, 0, ABkColor);
  Result := Assigned(l_bmp);
  if Result then
  begin
    AImage.Picture.Bitmap.Assign(l_Bmp);
    l_bmp.Free;
  end;
end;

class function Dev.DrawFromImageList(AImageList: TcxImageList; AImage: TcxImage;
  AIndex: Integer; ABkDc: THandle; ABk_X, ABk_Y: Integer): Boolean;
var
  l_bmp: TcxAlphaBitmap;
begin
  l_bmp := Dev.DrawFromImageListExt(AImageList,AIndex, ABkDc, ABk_X, ABk_Y, 0);
  Result := Assigned(l_bmp);
  if Result then
  begin
    AImage.Picture.Bitmap.Assign(l_bmp);
    l_bmp.Free;
  end;
end;

class function Dev.DrawFromImageList(AImageList: TcxImageList; AImage: TImage; AIndex: Integer;
  ABkDc: THandle; ABk_X, ABk_Y: Integer): Boolean;
var
  l_bmp: TcxAlphaBitmap;
begin
  l_bmp := Dev.DrawFromImageListExt(AImageList,AIndex, ABkDc, ABk_X, ABk_Y, 0);
  Result := Assigned(l_bmp);
  if Result then
  begin
    AImage.Picture.Bitmap.Assign(l_bmp);
    l_bmp.Free;
  end;
end;

class function Dev.DrawFromImageListExt(AImages: TCustomImageList;
  AImageIndex: Integer; const ABkDC: THandle; const ABk_X, ABk_Y: Integer;
  const ABkColor: TColor; const ASmoothImage: Boolean; const AOutBmpWidth,
  AOutBmpHeigth: Integer): TcxAlphaBitmap;

  procedure _DrawBackGround(ABitmap: TcxAlphaBitmap);
  begin
    if ABkDC>0 then
      cxBitBlt(ABitmap.Canvas.Handle, ABkDC, ABitmap.ClientRect, Point(ABk_X, ABk_Y), SRCCOPY)
    else
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ABkColor);
  end;

  procedure _DrawImage(ABitmap: TcxAlphaBitmap; ADrawMode: TcxImageDrawMode);
  var
    l_ImageBitmap, l_MaskBitmap: TcxAlphaBitmap;
    l_IsAlphaUsed: Boolean;
  begin
    cxPrepareBitmapForDrawing(nil, AImages, AImageIndex, True, clNone, l_ImageBitmap, l_MaskBitmap, l_IsAlphaUsed);
    l_ImageBitmap.AlphaBlend(ABitmap, ABitmap.ClientRect, ASmoothImage, $FF);
  end;
var
  l_Rect: TRect;
begin
  l_Rect := Rect(0, 0, IfThen(AOutBmpWidth>0, AOutBmpWidth, AImages.Width), IfThen(AOutBmpHeigth>0, AOutBmpHeigth, AImages.Height));
  Result := TcxAlphaBitmap.CreateSize(l_Rect.Right, l_Rect.Bottom, True);
  if Assigned(Result) then
  begin
    _DrawBackGround(Result);
    _DrawImage(Result, idmNormal);
  end;
end;

class procedure Dev.ImageListDpiCovert(AImageList: TcxImageList; AImgListWidthIn96Dpi,
  AImgListHeightIn96Dpi: Integer; const AConvertOrgImagesInList: Boolean);
var
  l_Radio: Real;
  l_NewBmps, l_NewMasks: array of Graphics.TBitmap;
  I, l_NewWidht, l_NewHeight: Integer;
begin
  l_Radio := Screen.PixelsPerInch div 48 / 2;
  if l_Radio <1 then
    l_Radio := 1;

(*  if Screen.PixelsPerInch>=288 then     {300%}
    l_Radio := 3
  else
  if Screen.PixelsPerInch>=240 then     {250%}
    l_Radio := 2.5
  else
  if Screen.PixelsPerInch>192 then     {200%}
    l_Radio := 2.0
  else
  if Screen.PixelsPerInch>=144 then     {150%}
    l_Radio := 1.5
  else
    l_Radio := 1;  //*)
  if l_Radio<>1 then
  begin
    l_NewWidht := Trunc(AImgListWidthIn96Dpi * l_Radio);
    l_NewHeight := Trunc(AImgListHeightIn96Dpi * l_Radio);
    {保存原来大小}
    if AConvertOrgImagesInList then
    begin
      SetLength(l_NewBmps, AImageList.Count);
      SetLength(l_NewMasks, AImageList.Count);
      for I := 0 to AImageList.Count - 1 do
      begin
        l_NewBmps[I] := Graphics.TBitmap.Create;
        l_NewMasks[I] := Graphics.TBitmap.Create;
        AImageList.GetImageInfo(I, l_NewBmps[I], l_NewMasks[I]);
      end;
    end;

    AImageList.Width := l_NewWidht;
    AImageList.Height := l_NewHeight;
    AImageList.Clear;

    {把原图载入}
    if AConvertOrgImagesInList then
      for I := Low(l_NewBmps) to High(l_NewBmps) do
      begin
        AImageList.Add(l_NewBmps[I], l_NewMasks[I]);
        l_NewBmps[I].Free;
        l_NewMasks[I].Free;
      end;
  end;
end;

class function Dev.ImageListLoadPictureFile(AImageList: TcxImageList;
  AFileName: String; const AClearOrgImage: Boolean): Boolean;

  function GetDefaultTransparentColor(AImage, AMask: Graphics.TBitmap): TColor;
  begin
    if IsGlyphAssigned(AMask) or dxIsAlphaUsed(AImage) then
      Result := clNone
    else
      Result := AImage.Canvas.Pixels[0, AImage.Height - 1];
  end;

  procedure AddImage(AImage, AMask: Graphics.TBitmap; AMaskColor: TColor; var AInsertedImageIndex: Integer);
  var
    l_ImageInfo: TcxImageInfo;
  begin
    l_ImageInfo := TcxImageInfo.Create;
    try
      l_ImageInfo.Image := AImage;
      l_ImageInfo.Mask := AMask;
      l_ImageInfo.MaskColor := AMaskColor;
      if IsGlyphAssigned(l_ImageInfo.Mask) then
        AImageList.Add(l_ImageInfo.Image, l_ImageInfo.Mask)
      else
        AImageList.AddMasked(l_ImageInfo.Image, l_ImageInfo.MaskColor);
    finally
      l_ImageInfo.Free;
    end;
  end;

  function InternalAddImage(AImage, AMask: Graphics.TBitmap;
    var AInsertedItemIndex: Integer): Integer;

  var
    l_ColCount, l_RowCount, l_ColIndex, l_RowIndex: Integer;
    l_SourceImageSize: TSize;
    l_DestBitmap, l_DestMask: TcxBitmap;
    l_DestRect: TRect;
    l_SrcPoint: TPoint;
  begin
    Result := 0;

    //if ((AImage.Width mod AImageList.Width) + (AImage.Height mod AImageList.Height)) = 0 then
      l_SourceImageSize := cxGeometry.cxSize(AImageList.Width, AImageList.Height);    //拆分
    //else
    //  l_SourceImageSize := cxSize(AImage.Width, AImage.Height);    //所有的作为一个

    l_ColCount := AImage.Width div l_SourceImageSize.cx;
    l_RowCount := AImage.Height div l_SourceImageSize.cy;

    l_DestBitmap := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height, pf32bit);
    if IsGlyphAssigned(AMask) then
      l_DestMask := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height, pf1bit)
    else
      l_DestMask := nil;
    try
      for l_RowIndex := 0 to l_RowCount - 1 do
        for l_ColIndex := 0 to l_ColCount - 1 do
        begin
          l_SrcPoint := Point(l_ColIndex * l_SourceImageSize.cx, l_RowIndex * l_SourceImageSize.cy);

          l_DestRect := cxGeometry.cxRectCenter(l_DestBitmap.ClientRect, Min(AImageList.Width, l_SourceImageSize.cx), Min(AImageList.Height, l_SourceImageSize.cy));
          l_DestBitmap.Canvas.Brush.Color := GetDefaultTransparentColor(AImage, AMask);
          l_DestBitmap.Canvas.FillRect(l_DestBitmap.ClientRect);

          cxDrawBitmap(l_DestBitmap.Canvas.Handle, AImage, l_DestRect, l_SrcPoint);
          if IsGlyphAssigned(AMask) then
            cxDrawBitmap(l_DestMask.Canvas.Handle, AMask, l_DestRect, l_SrcPoint);
          AddImage(l_DestBitmap, l_DestMask, GetDefaultTransparentColor(l_DestBitmap, l_DestMask), AInsertedItemIndex);
          Result := Result + 1;
        end;
    finally
      l_DestMask.Free;
      l_DestBitmap.Free;
    end;
  end;

var
  l_ImageInfo: TcxImageInfo;
  l_InsertedItemIndex: Integer;
begin
  Result := False;
  AImageList.BeginUpdate;
  l_ImageInfo := TcxImageInfo.Create;
  try
    if AClearOrgImage then
      AImageList.Clear;
    l_InsertedItemIndex := AImageList.Count;

    if GetImageInfoFromFile(AFileName, l_ImageInfo) then
      Result := InternalAddImage(l_ImageInfo.Image, l_ImageInfo.Mask, l_InsertedItemIndex) >0;
  finally
    l_ImageInfo.Free;
    AImageList.EndUpdate;
  end;
end;

class function Dev.ImageListLoadPictureFileWithStretch(AImageList: TcxImageList; AFileName: String;
  const AImgFileOneIconWidth, AImgFileOneIconHeight: Integer;
  const AClearOrgImage: Boolean): Boolean;

  function GetDefaultTransparentColor(AImage, AMask: Graphics.TBitmap): TColor;
  begin
    if IsGlyphAssigned(AMask) or dxIsAlphaUsed(AImage) then
      Result := clNone
    else
      Result := AImage.Canvas.Pixels[0, AImage.Height - 1];
  end;

  procedure AddImage(AImage, AMask: Graphics.TBitmap; AMaskColor: TColor; var AInsertedImageIndex: Integer);
  var
    l_ImageInfo: TcxImageInfo;
  begin
    l_ImageInfo := TcxImageInfo.Create;
    try
      l_ImageInfo.Image := AImage;
      l_ImageInfo.Mask := AMask;
      l_ImageInfo.MaskColor := AMaskColor;
      if IsGlyphAssigned(l_ImageInfo.Mask) then
        AImageList.Add(l_ImageInfo.Image, l_ImageInfo.Mask)
      else
        AImageList.AddMasked(l_ImageInfo.Image, l_ImageInfo.MaskColor);
    finally
      l_ImageInfo.Free;
    end;
  end;

  function InternalAddImage(AImage, AMask: Graphics.TBitmap;
    var AInsertedItemIndex: Integer): Integer;

  var
    l_ColCount, l_RowCount, l_ColIndex, l_RowIndex: Integer;
    l_SourceImageSize: TSize;
    l_DestBitmap, l_DestMask: TcxBitmap;
    l_DestRect: TRect;
    l_SrcPoint: TPoint;
    l_StretchBmp: TcxBitmap32;
  begin
    Result := 0;

    //if ((AImage.Width mod AImgFileOneIconWidth) + (AImage.Height mod AImgFileOneIconHeight)) = 0 then
      l_SourceImageSize := cxGeometry.cxSize(AImgFileOneIconWidth, AImgFileOneIconHeight);    //拆分
    //else
    //  l_SourceImageSize := cxSize(AImage.Width, AImage.Height);    //所有的作为一个

    l_ColCount := AImage.Width div l_SourceImageSize.cx;
    l_RowCount := AImage.Height div l_SourceImageSize.cy;

    l_DestBitmap := TcxBitmap.CreateSize(AImgFileOneIconWidth, AImgFileOneIconHeight, pf32bit);
    if IsGlyphAssigned(AMask) then
      l_DestMask := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height, pf1bit)
    else
      l_DestMask := nil;
    try
      for l_RowIndex := 0 to l_RowCount - 1 do
        for l_ColIndex := 0 to l_ColCount - 1 do
        begin
          l_SrcPoint := Point(l_ColIndex * l_SourceImageSize.cx, l_RowIndex * l_SourceImageSize.cy);

          l_DestRect := cxGeometry.cxRectCenter(l_DestBitmap.ClientRect, Min(AImgFileOneIconWidth, l_SourceImageSize.cx), Min(AImgFileOneIconHeight, l_SourceImageSize.cy));
          l_DestBitmap.Canvas.Brush.Color := GetDefaultTransparentColor(AImage, AMask);
          l_DestBitmap.Canvas.FillRect(l_DestBitmap.ClientRect);

          cxDrawBitmap(l_DestBitmap.Canvas.Handle, AImage, l_DestRect, l_SrcPoint);
          if IsGlyphAssigned(AMask) then
            cxDrawBitmap(l_DestMask.Canvas.Handle, AMask, l_DestRect, l_SrcPoint);

          if (AImgFileOneIconWidth<>AImageList.Width) or (AImgFileOneIconHeight<>AImageList.Height) then
          begin
            l_StretchBmp := TcxBitmap32.CreateSize(AImageList.Width, AImageList.Height, True);
            try
              cxAlphaBlend(l_StretchBmp, l_DestBitmap, l_StretchBmp.ClientRect, l_DestBitmap.ClientRect, True, $FF);
              AddImage(l_StretchBmp, l_DestMask, GetDefaultTransparentColor(l_StretchBmp, l_DestMask), AInsertedItemIndex);
            finally
              l_StretchBmp.Free;
            end;
          end
          else
            AddImage(l_DestBitmap, l_DestMask, GetDefaultTransparentColor(l_DestBitmap, l_DestMask), AInsertedItemIndex);
          Result := Result + 1;
        end;
    finally
      l_DestMask.Free;
      l_DestBitmap.Free;
    end;
  end;

var
  l_ImageInfo: TcxImageInfo;
  l_InsertedItemIndex: Integer;
begin
  Result := False;
  AImageList.BeginUpdate;
  l_ImageInfo := TcxImageInfo.Create;
  try
    if AClearOrgImage then
      AImageList.Clear;
    l_InsertedItemIndex := AImageList.Count;

    if GetImageInfoFromFile(AFileName, l_ImageInfo) then
      Result := InternalAddImage(l_ImageInfo.Image, l_ImageInfo.Mask, l_InsertedItemIndex) >0;
  finally
    l_ImageInfo.Free;
    AImageList.EndUpdate;
  end;
end;

class procedure Dev.MergeRibbon(SourceRibbon, TargetRibbon: TdxRibbon;
  TargetManager: TdxBarManager);
var
  i: Integer;
  ATargetTab:TdxRibbonTab;
  ASourceBar:TdxBar;
  ATargetBar:TdxBar;
  j: Integer;

  function GetTabByName(const ATabName:string): TdxRibbonTab;
  var
    k:Integer;
  begin
    for k := 0 to TargetRibbon.Tabs.Count - 1 do
      if(ATabName = TargetRibbon.Tabs[k].Name) then
      begin
        Result := TargetRibbon.Tabs[k];
        Exit;
      end;

    Result := TargetRibbon.Tabs.Add;
  end;

  function GetToolbarByGroupCaption(ATargetTab: TdxRibbonTab; const AGroupCaption:string): TdxBar;
  var
    k:Integer;
    group:TdxRibbonTabGroup;
  begin
    group := nil;
    for k := 0 to ATargetTab.Groups.Count - 1 do
    begin
      if(AGroupCaption = ATargetTab.Groups[k].Caption) then
      begin
        group := ATargetTab.Groups[k];
        Break;
      end;
    end;

    if(nil = group) then
    begin
      group := ATargetTab.Groups.Add;
      group.Caption := AGroupCaption;
    end;

    if(nil = group.ToolBar) then
    begin
      group.ToolBar := TargetManager.Bars.Add;
      group.ToolBar.Visible := True;
    end;

    Result := group.ToolBar;
  end;

begin
  TargetRibbon.BeginUpdate;
  try
    for i := 0 to SourceRibbon.Tabs.Count - 1 do
    begin
      ATargetTab := GetTabByName(SourceRibbon.Tabs[i].Name);
      for j := 0 to SourceRibbon.Tabs[i].Groups.Count - 1 do
      begin
        ASourceBar := SourceRibbon.Tabs[i].Groups[j].ToolBar;
        if (nil <> ASourceBar) then
        begin
          ATargetBar := GetToolbarByGroupCaption(ATargetTab, SourceRibbon.Tabs[i].Groups[j].Caption);
          ATargetBar.Merge(ASourceBar);
        end;
      end
    end;
    SourceRibbon.Visible := False;
  finally
    TargetRibbon.EndUpdate;
  end;
end;

class function Dev.SetItemVisible(AItem: TdxBarItem; AValue: Boolean): Boolean;
begin
  Result := True;

  if AValue then
    AItem.visible := ivAlways
  else
    AItem.visible := ivNever;
end;

class procedure Dev.SetVisible(AItem: TdxBarItem; AVisible: Boolean);
begin
  if AVisible then
    AItem.Visible := ivAlways
  else
    AItem.Visible := ivNever;
end;

end.
