//Image Framework
//图像操作相关

unit fcImage;

interface

uses Graphics, Windows, Classes, ExtCtrls, pngimage, Controls, GIFImg, ImgList,
  jpeg, SysUtils;

type
  Image = record
  public
    class function GetPicType(const bf: Pointer; const bflen: Integer): Integer; static;

    class function ImageListLoadFromBitmap(AImageList: TImageList; ABMP: Graphics.TBitmap;
        const AClearOrgImage: Boolean): Boolean; static;

    //加载一个图片文件到TImage， 加载的时候，会读取图片文件的前几个字节，判断图片的格式然后再加载。
    class function LoadAPicFileToImage(AFileName: String; AImage: TImage): Boolean; static;

    //无损缩放一个图片，适合于按钮图片的拉伸
    class procedure StretchBitmapWithEdge(AIn, AOut: Graphics.TBitmap; AOutWidth, AOutHeight: Integer;
        const AEdgeWidth: Integer=5;const AEdgeHeight: Integer=5); static;

    //按比例缩放一个图片，输出的结果都是 jpg格式
    class function ZoomAPicture(ASrcFile, ADestFile: String; AMaxOutWidth, AMaxOutHeight: Integer;
        TransparentBmpFile: Boolean): Boolean;static;

    class function LoadFromStream(AImagePicture: TPicture; AStream: TCustomMemoryStream): Boolean;  static;

    class function BmpToJpegFile(SourceBMP: Graphics.TBitMap; AOutFile: String; const ACompressionQuality: Integer=75): Boolean;static;

    class function GetFileImageToIcon(FileName: String; IsSmallIcon: Boolean): TIcon; static;
    class function GetFilesImageToList(ImageList: TImageList; FileName: String; IsSmallIcon: Boolean): Boolean; overload; static;
    class function GetFilesImageToList(ImageList: TImageList; FileList: TStringList; IsSmallIcon: Boolean): Boolean; overload; static;

    class function ImageListDrawToAImage(AImageList: TImageList; AImage: TImage; AIndex: Integer; const ABkColor: TColor=clWhite): Boolean; static;
  end;

implementation

uses
  fcStr, ShellAPI, fcWindows, fcFile, StrUtils, fcPlus_VCL;

{ Image }

class function Image.BmpToJpegFile(SourceBMP: Graphics.TBitMap; AOutFile: String;
  const ACompressionQuality: Integer): Boolean;
var
  l_Jpg: TJPEGImage;
begin
  l_Jpg := TJPEGImage.Create;
  try
    try
      l_Jpg.Assign(SourceBMP);
      l_Jpg.JPEGNeeded;
      l_Jpg.CompressionQuality := ACompressionQuality;
      l_Jpg.Compress;
      l_Jpg.SaveToFile(AOutFile);
      Result := True;
    except
      Result := false;
    end;
  finally
    l_Jpg.free;
  end;
end;


class function Image.GetFilesImageToList(ImageList: TImageList;
  FileName: String; IsSmallIcon: Boolean): Boolean;
var
  l_ICO: TIcon;
begin
  l_ICO := GetFileImageToIcon(FileName, IsSmallIcon);
  Result := Assigned(l_ICO);
  if Result then
  begin
    ImageList.AddIcon(l_ICO);   //把ICO装入ImageList中
    l_ICO.Free;
  end;
end;


class function Image.GetFileImageToIcon(FileName: String;
  IsSmallIcon: Boolean): TIcon;
var
  l_Ext: String;
  l_FileInfo: SHFILEINFO;
  l_Flag: Integer;
begin
  Result := TIcon.Create;
  if FileName <> '' then if fileName[1] = '"' then Delete(FileName, 1, 1);
  if FileName <> '' then
    if fileName[Length(fileName)] = '"' then
      FileName := Copy(FileName, 1, Length(fileName) -1);

  l_Ext := ExtractFileExt(FileName);
  l_Flag := SHGFI_USEFILEATTRIBUTES or SHGFI_ICON;
  if isSmallIcon then
    l_Flag := l_Flag or SHGFI_SMALLICON
  else
    l_Flag := l_Flag or SHGFI_LARGEICON;

  if SHGetFileInfo(PChar(l_Ext), 0, l_FileInfo, sizeof(l_FileInfo), l_Flag)<>0 then
    Result.Handle := l_FileInfo.hIcon
  else
    FreeAndNil(Result);
end;

class function Image.GetFilesImageToList(ImageList: TImageList;
  FileList: TStringList; IsSmallIcon: Boolean): Boolean;
var
  i : integer;
begin
  ImageList.Clear;
  if IsSmallIcon then ImageList.Height := 16
  else ImageList.Height := 32;
  ImageList.Width := ImageList.Height;

  for i := 0 to FileList.Count -1 do
    GetFilesImageToList(ImageList, FileList[i], IsSmallIcon);

  result := True;
end;

class function Image.GetPicType(const bf: Pointer;
  const bflen: Integer): Integer;
// 判断图片格式 1:bmp 2:jpeg 3:gif 4:png
const
  FH_BMP: array [0 .. 1] of Byte = ($4D, $42);
  FH_JPEG: array [0 .. 1] of Byte = ($FF, $D8);
  FH_GIF: array [0 .. 2] of Byte = ($47, $49, $46);
  FH_PNG: array [0 .. 7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
var
  p: Integer;
begin
  Result := -1;
  if bflen < 8 then
    Exit;
  p := Integer(bf);
  if (PByte(p)^ = FH_BMP[0]) and (PByte(p + 1)^ = FH_BMP[1]) then
    Result := 1
  else if (PByte(p)^ = FH_JPEG[0]) and (PByte(p + 1)^ = FH_JPEG[1]) then
    Result := 2
  else if (PByte(p)^ = FH_GIF[0]) and (PByte(p + 1)^ = FH_GIF[1]) and
    (PByte(p + 2)^ = FH_GIF[2]) then
    Result := 3
  else if (PByte(p)^ = FH_PNG[0]) and (PByte(p + 1)^ = FH_PNG[1]) and
    (PByte(p + 2)^ = FH_PNG[2]) and (PByte(p + 3)^ = FH_PNG[3]) and
    (PByte(p + 4)^ = FH_PNG[4]) and (PByte(p + 5)^ = FH_PNG[5]) and
    (PByte(p + 6)^ = FH_PNG[6]) and (PByte(p + 7)^ = FH_PNG[7]) then
    Result := 4
end;

class function Image.ImageListDrawToAImage(AImageList: TImageList;
  AImage: TImage; AIndex: Integer; const ABkColor: TColor): Boolean;
begin
  AImage.Picture.Bitmap.Width := AImageList.Width+1;
  AImage.Picture.Bitmap.Height := AImageList.Height+1;

  AImage.Picture.Bitmap.Canvas.Brush.Color := ABkColor;
  AImage.Picture.Bitmap.Canvas.FillRect(Rect(0,0, AImageList.Width+1, AImageList.Height+1));
  AImage.Transparent := True;
  AImageList.Draw(AImage.Picture.Bitmap.Canvas, 1,1,
      AIndex, dsTransparent, itImage, True);
  AImage.Picture.Bitmap.Transparent := True;
  Result := True;
end;

class function Image.ImageListLoadFromBitmap(AImageList: TImageList;
  ABMP: Graphics.TBitmap; const AClearOrgImage: Boolean): Boolean;
var
  I, l_X, l_Y, l_Row, l_Col: Integer;
  l_ABMP: Graphics.TBitmap;
const
  l_BKColor = clFuchsia;
begin
  Result := True;
  if AClearOrgImage then
    AImageList.Clear;

  l_ABMP := Graphics.TBitmap.Create;
  try
    l_Row := ABMP.Height div AImageList.Height; // 多少行
    l_Col := ABMP.Width div AImageList.Width; // 多少列
    l_ABMP.Width := AImageList.Width;
    l_ABMP.Height := AImageList.Height;

    for I := 0 to l_Col * l_Row - 1 do
    begin
      l_X := (I mod l_Col) * AImageList.Width;
      l_Y := (I div l_Col) * AImageList.Height;
      BitBlt(l_ABMP.Canvas.Handle, 0, 0, AImageList.Width, AImageList.Height,
        ABMP.Canvas.Handle, l_X, l_Y, SRCCOPY);
      AImageList.AddMasked(l_ABMP, l_BKColor);
    end;
  finally
    l_ABMP.Free;
  end;
end;

class function Image.LoadAPicFileToImage(AFileName: String;
  AImage: TImage): Boolean;
begin
  Result := AImage.LoadFromFileExFc(AFileName, false)
end;

class function Image.LoadFromStream(AImagePicture: TPicture; AStream: TCustomMemoryStream): Boolean;
var
  l_Type: Integer;
  l_jpg: TJPEGImage;
  l_png: TPngImage;
  l_gif: TGIFImage;
  l_Bmp: Graphics.TBitmap;
begin
  Result := false;
  try
    l_Type := fcImage.Image.GetPicType(AStream.Memory, AStream.Size);
    AStream.Position := 0;
    case l_Type of // 1:bmp 2:jpeg 3:gif 4:png
      1:
        begin
          l_Bmp := Graphics.TBitmap.Create;
          try
            l_Bmp.LoadFromStream(AStream);
            AImagePicture.Assign(l_Bmp);
            Result := True;
          finally
            l_Bmp.Free;
          end;
        end;
      2:
        begin
          l_jpg := TJPEGImage.Create;
          try
            l_jpg.LoadFromStream(AStream);
            AImagePicture.Assign(l_jpg);
            Result := True;
          finally
            l_jpg.Free;
          end;
        end;
      3:
        begin
          l_gif := TGIFImage.Create;
          try
            l_gif.LoadFromStream(AStream);
            AImagePicture.Assign(l_gif);
            if (AImagePicture.Graphic <> nil) and (AImagePicture.Graphic is TGIFImage) then
              TGIFImage(AImagePicture.Graphic).Animate := True;
            Result := True;
          finally
            l_gif.Free;
          end;
        end;
      4:
        begin
          l_png := TPngImage.Create;
          try
            l_png.LoadFromStream(AStream);
            AImagePicture.Assign(l_png);
            Result := True;
          finally
            l_png.Free;
          end;
        end;
    end;
  except
    AImagePicture.Assign(nil);
  end;
end;

class procedure Image.StretchBitmapWithEdge(AIn, AOut: Graphics.TBitmap;
  AOutWidth, AOutHeight: Integer; const AEdgeWidth, AEdgeHeight: Integer);
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


class function Image.ZoomAPicture(ASrcFile, ADestFile: String; AMaxOutWidth,
  AMaxOutHeight: Integer; TransparentBmpFile: Boolean): Boolean;
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
  Result := false;
  if (AMaxOutWidth = 0) or (AMaxOutHeight = 0) or (not FileExists(ASrcFile)) then
    Exit;

  l_Ext := Lowercase(ExtractFileExt(ASrcFile));
  l_SrcBmp := Graphics.TBitMap.Create;
  try
    // 统一转换成 bmp格式，只有这个格式才能压缩......

    case Arr.PosInStrArray(l_Ext, c_Ext) of
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
end;


end.
