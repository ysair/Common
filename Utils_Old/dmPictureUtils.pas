unit dmPictureUtils;

interface

uses
  Graphics, Classes, SysUtils, Windows, Jpeg;

 //按比例缩放一个图片，输出的结果都是 jpg格式
//function AutoZoomAPicture(ASrcFile, ADestFile: String; AMaxOutWidth, AMaxOutHeight: Integer;
//  TransparentBmpFile: Boolean): Boolean;

//无损缩放一个图片
procedure StretchBitmapWithEdge(AIn, AOut: Graphics.TBitmap; AOutWidth, AOutHeight: Integer;
  const AEdgeWidth: Integer=5;const AEdgeHeight: Integer=5);

implementation


//uses
// ZsMiscUtils, dmStringUtils;

 //按比例缩放一个图片
 (*
function AutoZoomAPicture(ASrcFile, ADestFile: String; AMaxOutWidth, AMaxOutHeight: Integer;
  TransparentBmpFile: Boolean): Boolean;
  procedure _ZoomBmp(imagen: Graphics.TBitMap; dWidth, dHeight: Integer; var des: Graphics.TBitMap);
  var
    ori: Graphics.TBitMap;
    dispositivo_o, dispositivo_d: HDC;
    pepito: HBitmap;
  begin
    ori := Graphics.TBitMap.Create;
    des := Graphics.TBitMap.Create;
    ori.handle := imagen.handle;
    des.width := dWidth;
    des.height := dHeight;
    dispositivo_o := CreateCompatibleDC(0);
    dispositivo_d := CreateCompatibleDC(0);
    SelectObject(dispositivo_o, ori.handle);
    pepito := SelectObject(dispositivo_d, des.handle);
    SetStretchBltMode(dispositivo_d, { COLORONCOLOR } STRETCH_DELETESCANS);
    StretchBlt(dispositivo_d, 0, 0, dWidth, dHeight, dispositivo_o, 0, 0, ori.width, ori.height,
      SRCCOPY);
    SelectObject(dispositivo_d, pepito);
    ori.free;

    DeleteObject(pepito);
    DeleteDC(dispositivo_o);
    DeleteDC(dispositivo_d);
  end;

var
  l_SrcJpg: TJPEGImage;
  l_SrcBmp, l_DestBmp: Graphics.TBitMap;
  l_Ext: String;
const
  c_Ext: array [0 .. 2] of String = ('.jpg', '.jpeg', '.bmp');
begin
  if (AMaxOutWidth = 0) or (AMaxOutHeight = 0) or (not FileExists(ASrcFile)) then
    Exit(False);

  l_Ext := Lowercase(ExtractFileExt(ASrcFile));
  l_SrcBmp := Graphics.TBitMap.Create;
  try
    // 统一转换成 bmp格式，只有这个格式才能压缩......

    case PosInStrArray(l_Ext, c_Ext) of
      0, 1:
        begin
          l_SrcJpg := TJPEGImage.Create;
          try
            try
              l_SrcJpg.LoadFromFile(ASrcFile);
            except
              Exit;
            end;
            l_SrcBmp.width := l_SrcJpg.width;
            l_SrcBmp.height := l_SrcJpg.height;
            l_SrcBmp.Canvas.Draw(0, 0, l_SrcJpg);
          finally
            l_SrcJpg.free;
          end;
        end;
      2:
        begin
          l_SrcBmp.LoadFromFile(ASrcFile);
          l_SrcBmp.Transparent := TransparentBmpFile;
        end;
    else
      Exit;
    end;
    if (l_SrcBmp = nil) or (l_SrcBmp.width = 0) or (l_SrcBmp.height = 0) then
      Exit;

    l_DestBmp := Graphics.TBitMap.Create;
    try
      // 计算要压缩后的尺寸
      if AMaxOutWidth / AMaxOutHeight >= l_SrcBmp.width / l_SrcBmp.height then // 以高度为基准
      begin
        l_DestBmp.height := AMaxOutHeight;
        l_DestBmp.width := Trunc(l_SrcBmp.width * (AMaxOutHeight / l_SrcBmp.height));
      end
      else
      begin
        l_DestBmp.width := AMaxOutWidth;
        l_DestBmp.height := Trunc(l_SrcBmp.height * (AMaxOutWidth / l_SrcBmp.width));
      end;

      // 压缩，把图片输出到 l_DestBmp 中


      // 把最后的结果转换成 Jpg 格式输出
      SysUtils.ForceDirectories(ExtractFilePath(ADestFile));
      if FileExists(ADestFile) then
        SysUtils.DeleteFile(ADestFile);
      if not BmpToJpegFile(l_DestBmp, ADestFile, 100) then
        Exit(False);

      Result := True;
    finally
      l_DestBmp.free;
    end;
  finally
    l_SrcBmp.free;
  end;
end;     *)


//无损缩放一个图片
procedure StretchBitmapWithEdge(AIn, AOut: Graphics.TBitmap; AOutWidth, AOutHeight: Integer;
  const AEdgeWidth, AEdgeHeight: Integer);
var
  l_SrcBmp, l_Center: Graphics.TBitmap;
  l_X, l_Y: Integer;
begin
  //控制下值
  if AEdgeHeight >= (AIn.Height div 2) then
    l_Y := (AIn.Height div 4) -1 +1                 //防止为0
  else
    l_Y := AEdgeHeight;
  if AEdgeWidth >= (AIn.Width div 2) then
    l_X :=  (AIn.Width div 4) -1  +1
  else
    l_X := AEdgeWidth;

  l_SrcBmp := Graphics.TBitmap.Create;
  l_Center := Graphics.TBitmap.Create;
  try
    //拉长
    if AIn.Width <> AOutWidth then
    begin
      l_SrcBmp.Assign(AIn);

      l_Center.Height := l_SrcBmp.Height;
      l_Center.Width := l_SrcBmp.Width- 2 * l_X;
      BitBlt(l_Center.Canvas.Handle, 0, 0, l_Center.Width, l_Center.Height,
          l_SrcBmp.Canvas.Handle, l_X , 0, SRCCOPY);

      AOut.Width := AOutWidth;
      AOut.Height := l_SrcBmp.Height;
      StretchBlt( AOut.Canvas.Handle, l_X, 0, AOutWidth-l_X*2, AOut.Height,
                  l_Center.Canvas.Handle, 0, 0, l_Center.Width, l_Center.Height,
                  SRCCOPY);
      BitBlt(AOut.Canvas.Handle, 0, 0, l_X, l_SrcBmp.Height, l_SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(AOut.Canvas.Handle, AOutWidth-l_X, 0, AOutWidth, l_SrcBmp.Height,
          l_SrcBmp.Canvas.Handle, l_SrcBmp.Width-l_X, 0, SRCCOPY);
    end
    else
      AOut.Assign(AIn);

    //拉高
    if AIn.Height <> AOutHeight then
    begin
      l_SrcBmp.Assign(AOut);

      l_Center.Height := l_SrcBmp.Height - 2 * l_Y;
      l_Center.Width  := l_SrcBmp.Width;
      BitBlt(l_Center.Canvas.Handle, 0, 0, l_Center.Width, l_Center.Height,
          l_SrcBmp.Canvas.Handle, 0, l_Y, SRCCOPY);

      AOut.Width := l_SrcBmp.Width;
      AOut.Height := AOutHeight;
      StretchBlt( AOut.Canvas.Handle, 0, l_Y, AOut.Width, AOutHeight-l_Y*2,
                  l_Center.Canvas.Handle, 0, 0, l_Center.Width, l_Center.Height,
                  SRCCOPY);
      BitBlt(AOut.Canvas.Handle, 0, 0, l_SrcBmp.Width, l_Y, l_SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
      BitBlt(AOut.Canvas.Handle, 0, AOutHeight-l_Y, l_SrcBmp.Width, AOutHeight,
          l_SrcBmp.Canvas.Handle, 0, l_SrcBmp.Height-l_Y, SRCCOPY);
    end;

  finally
    l_SrcBmp.Free;
    l_Center.Free;
  end;
end;

end.
