{ -----------------------------------------------------------------------------
  Unit Name: GDIPOBJ
  Author:    pux
  Purpose:
  History:
  2006-11-30: + add TGPImage.GetFormat,此函数用于xpGDIPicture单元，用于判断图片的类型

  -----------------------------------------------------------------------------
  Copyright(c) Xeme Soft 1998-2007
  ----------------------------------------------------------------------------- }

{ ****************************************************************** }
{ GDI+ Class }
{ }
{ home page : http://www.progdigy.com }
{ email     : hgourvest@progdigy.com }
{ }
{ date      : 15-02-2002 }
{ }
{ The contents of this file are used with permission, subject to }
{ the Mozilla Public License Version 1.1 (the "License"); you may }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at }
{ http://www.mozilla.org/MPL/MPL-1.1.html }
{ }
{ Software distributed under the License is distributed on an }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or }
{ implied. See the License for the specific language governing }
{ rights and limitations under the License. }
{ }
{ ***************************************************************** }

unit GDIPOBJ;

interface

uses
  Windows,
  ActiveX,
  GDIPAPI;

(* *************************************************************************\
  *
  *   GDI+ Codec Image APIs
  *
  \************************************************************************* *)

// --------------------------------------------------------------------------
// Codec Management APIs
// --------------------------------------------------------------------------

function GetImageDecodersSize(out numDecoders, size: UINT): TStatus;
function GetImageDecoders(numDecoders, size: UINT;
  decoders: PImageCodecInfo): TStatus;
function GetImageEncodersSize(out numEncoders, size: UINT): TStatus;
function GetImageEncoders(numEncoders, size: UINT;
  encoders: PImageCodecInfo): TStatus;

(* *************************************************************************\
  *
  *   Private GDI+ header file.
  *
  \************************************************************************* *)

// ---------------------------------------------------------------------------
// GDI+ classes for forward reference
// ---------------------------------------------------------------------------

type
  TGPGraphics = class;
  TGPPen = class;
  TGPBrush = class;
  TGPMatrix = class;
  TGPBitmap = class;
  TGPMetafile = class;
  TGPFontFamily = class;
  TGPGraphicsPath = class;
  TGPRegion = class;
  TGPImage = class;
  TGPHatchBrush = class;
  TGPSolidBrush = class;
  TGPLinearGradientBrush = class;
  TGPPathGradientBrush = class;
  TGPFont = class;
  TGPFontCollection = class;
  TGPInstalledFontCollection = class;
  TGPPrivateFontCollection = class;
  TGPImageAttributes = class;
  TGPCachedBitmap = class;

  (* *************************************************************************\
    *
    *   GDI+ Region, Font, Image, CustomLineCap class definitions.
    *
    \************************************************************************* *)

  TGPRegion = class(TGdiplusBase)
  protected
    nativeRegion: GpRegion;
    lastResult: TStatus;
    function SetStatus(status: TStatus): TStatus;
    procedure SetNativeRegion(nativeRegion: GpRegion);
    constructor Create(nativeRegion: GpRegion); reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(rect: TGPRectF); reintroduce; overload;
    constructor Create(rect: TGPRect); reintroduce; overload;
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create(regionData: PBYTE; size: Integer); reintroduce; overload;
    constructor Create(hRgn: hRgn); reintroduce; overload;
    function FromHRGN(hRgn: hRgn): TGPRegion;
    destructor Destroy; override;
    function Clone: TGPRegion;
    function MakeInfinite: TStatus;
    function MakeEmpty: TStatus;
    function GetDataSize: UINT;
    // buffer     - where to put the data
    // bufferSize - how big the buffer is (should be at least as big as GetDataSize())
    // sizeFilled - if not NULL, this is an OUT param that says how many bytes
    // of data were written to the buffer.
    function GetData(buffer: PBYTE; bufferSize: UINT;
      sizeFilled: PUINT = nil): TStatus;
    function Intersect(const rect: TGPRect): TStatus; overload;
    function Intersect(const rect: TGPRectF): TStatus; overload;
    function Intersect(path: TGPGraphicsPath): TStatus; overload;
    function Intersect(region: TGPRegion): TStatus; overload;
    function Union(const rect: TGPRect): TStatus; overload;
    function Union(const rect: TGPRectF): TStatus; overload;
    function Union(path: TGPGraphicsPath): TStatus; overload;
    function Union(region: TGPRegion): TStatus; overload;
    function Xor_(const rect: TGPRect): TStatus; overload;
    function Xor_(const rect: TGPRectF): TStatus; overload;
    function Xor_(path: TGPGraphicsPath): TStatus; overload;
    function Xor_(region: TGPRegion): TStatus; overload;
    function Exclude(const rect: TGPRect): TStatus; overload;
    function Exclude(const rect: TGPRectF): TStatus; overload;
    function Exclude(path: TGPGraphicsPath): TStatus; overload;
    function Exclude(region: TGPRegion): TStatus; overload;
    function Complement(const rect: TGPRect): TStatus; overload;
    function Complement(const rect: TGPRectF): TStatus; overload;
    function Complement(path: TGPGraphicsPath): TStatus; overload;
    function Complement(region: TGPRegion): TStatus; overload;
    function Translate(dx, dy: Single): TStatus; overload;
    function Translate(dx, dy: Integer): TStatus; overload;
    function Transform(matrix: TGPMatrix): TStatus;
    function GetBounds(out rect: TGPRect; g: TGPGraphics): TStatus; overload;
    function GetBounds(out rect: TGPRectF; g: TGPGraphics): TStatus; overload;
    function GetHRGN(g: TGPGraphics): hRgn;
    function IsEmpty(g: TGPGraphics): BOOL;
    function IsInfinite(g: TGPGraphics): BOOL;
    function IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const point: TGPPoint; g: TGPGraphics = nil): BOOL;
      overload;
    function IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const point: TGPPointF; g: TGPGraphics = nil): BOOL;
      overload;
    function IsVisible(x, y, width, height: Integer; g: TGPGraphics): BOOL;
      overload;
    function IsVisible(const rect: TGPRect; g: TGPGraphics = nil): BOOL;
      overload;
    function IsVisible(x, y, width, height: Single;
      g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(const rect: TGPRectF; g: TGPGraphics = nil): BOOL;
      overload;
    function Equals(region: TGPRegion; g: TGPGraphics): BOOL;
    function GetRegionScansCount(matrix: TGPMatrix): UINT;
    function GetRegionScans(matrix: TGPMatrix; rects: PGPRectF;
      out count: Integer): TStatus; overload;
    function GetRegionScans(matrix: TGPMatrix; rects: PGPRect;
      out count: Integer): TStatus; overload;
    function GetLastStatus: TStatus;
  end;

  // --------------------------------------------------------------------------
  // FontFamily
  // --------------------------------------------------------------------------

  TGPFontFamily = class(TGdiplusBase)
  protected
    nativeFamily: GpFontFamily;
    lastResult: TStatus;
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeOrig: GpFontFamily; status: TStatus); reintroduce;
      overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(name: WideString;
      fontCollection: TGPFontCollection = nil); reintroduce; overload;
    destructor Destroy; override;
    class function GenericSansSerif: TGPFontFamily;
    class function GenericSerif: TGPFontFamily;
    class function GenericMonospace: TGPFontFamily;
    function GetFamilyName(out name: String; language: LANGID = 0): TStatus;
    function Clone: TGPFontFamily;
    function IsAvailable: BOOL;
    function IsStyleAvailable(style: Integer): BOOL;
    function GetEmHeight(style: Integer): UINT16;
    function GetCellAscent(style: Integer): UINT16;
    function GetCellDescent(style: Integer): UINT16;
    function GetLineSpacing(style: Integer): UINT16;
    function GetLastStatus: TStatus;
  end;

  // --------------------------------------------------------------------------
  // Font Collection
  // --------------------------------------------------------------------------

  TGPFontCollection = class(TGdiplusBase)
  protected
    nativeFontCollection: GpFontCollection;
    lastResult: TStatus;
    function SetStatus(status: TStatus): TStatus;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFamilyCount: Integer;
    function GetFamilies(numSought: Integer;
      out gpfamilies: array of TGPFontFamily; out numFound: Integer): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPInstalledFontCollection = class(TGPFontCollection)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TGPPrivateFontCollection = class(TGPFontCollection)
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddFontFile(filename: WideString): TStatus;
    function AddMemoryFont(memory: Pointer; length: Integer): TStatus;
  end;

  // --------------------------------------------------------------------------
  // TFont
  // --------------------------------------------------------------------------

  TGPFont = class(TGdiplusBase)
  protected
    nativeFont: GpFont;
    lastResult: TStatus;
    procedure SetNativeFont(Font: GpFont);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(Font: GpFont; status: TStatus); overload;
  public
    constructor Create(hdc: hdc); reintroduce; overload;
    constructor Create(hdc: hdc; logfont: PLogFontA); reintroduce; overload;
    constructor Create(hdc: hdc; logfont: PLogFontW); reintroduce; overload;
    constructor Create(hdc: hdc; hfont: hfont); reintroduce; overload;
    constructor Create(family: TGPFontFamily; emSize: Single;
      style: TFontStyle = FontStyleRegular;
      unit_: TUnit = UnitPoint); reintroduce; overload;
    constructor Create(familyName: WideString; emSize: Single;
      style: TFontStyle = FontStyleRegular; unit_: TUnit = UnitPoint;
      fontCollection: TGPFontCollection = nil); reintroduce; overload;
    function GetLogFontA(g: TGPGraphics; out logfontA: TLogFontA): TStatus;
    function GetLogFontW(g: TGPGraphics; out logfontW: TLogFontW): TStatus;
    function Clone: TGPFont;
    destructor Destroy; override;
    function IsAvailable: BOOL;
    function GetStyle: Integer;
    function GetSize: Single;
    function GetUnit: TUnit;
    function GetLastStatus: TStatus;
    function GetHeight(graphics: TGPGraphics): Single; overload;
    function GetHeight(dpi: Single): Single; overload;
    function GetFamily(family: TGPFontFamily): TStatus;
  end;

  // --------------------------------------------------------------------------
  // Abstract base class for Image and Metafile
  // --------------------------------------------------------------------------

  TGPImage = class(TGdiplusBase)
  protected
    nativeImage: GpImage;
    lastResult: TStatus;
    loadStatus: TStatus;
    procedure SetNativeImage(nativeImage: GpImage);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeImage: GpImage; status: TStatus); reintroduce;
      overload;
  public
    constructor Create(filename: WideString;
      useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    constructor Create(stream: IStream;
      useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    function FromFile(filename: WideString;
      useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
    function FromStream(stream: IStream;
      useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
    destructor Destroy; override;
    function Clone: TGPImage;
    function Save(filename: WideString; const clsidEncoder: TGUID;
      encoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(stream: IStream; const clsidEncoder: TGUID;
      encoderParams: PEncoderParameters = nil): TStatus; overload;
    function SaveAdd(encoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(newImage: TGPImage;
      encoderParams: PEncoderParameters): TStatus; overload;
    function GetType: TImageType;
    // --- begin add 2006.11.30 by xin.pu ---
    function GetFormat: TGUID;
    // ---  end  add 2006.11.30 by xin.pu ---
    function GetPhysicalDimension(out size: TGPSizeF): TStatus;
    function GetBounds(out srcRect: TGPRectF; out srcUnit: TUnit): TStatus;
    function GetWidth: UINT;
    function GetHeight: UINT;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetFlags: UINT;
    function GetRawFormat(out format: TGUID): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(palette: PColorPalette; size: Integer): TStatus;
    function SetPalette(palette: PColorPalette): TStatus;
    function GetThumbnailImage(thumbWidth, thumbHeight: UINT;
      callback: GetThumbnailImageAbort = nil;
      callbackData: Pointer = nil): TGPImage;
    function GetFrameDimensionsCount: UINT;
    function GetFrameDimensionsList(dimensionIDs: PGUID; count: UINT): TStatus;
    function GetFrameCount(const dimensionID: TGUID): UINT;
    function SelectActiveFrame(const dimensionID: TGUID;
      frameIndex: UINT): TStatus;
    function RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
    function GetPropertyCount: UINT;
    function GetPropertyIdList(numOfProperty: UINT; list: PPropID): TStatus;
    function GetPropertyItemSize(propId: propId): UINT;
    function GetPropertyItem(propId: propId; propSize: UINT;
      buffer: PPropertyItem): TStatus;
    function GetPropertySize(out totalBufferSize, numProperties: UINT): TStatus;
    function GetAllPropertyItems(totalBufferSize, numProperties: UINT;
      allItems: PPropertyItem): TStatus;
    function RemovePropertyItem(propId: TPROPID): TStatus;
    function SetPropertyItem(const item: TPropertyItem): TStatus;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID): UINT;
    function GetEncoderParameterList(const clsidEncoder: TGUID; size: UINT;
      buffer: PEncoderParameters): TStatus;
    function GetLastStatus: TStatus;
  end;

  TGPBitmap = class(TGPImage)
  protected
    constructor Create(nativeBitmap: GpBitmap); reintroduce; overload;
  public
    constructor Create(filename: WideString;
      useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    constructor Create(stream: IStream;
      useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
    function FromFile(filename: WideString;
      useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
    function FromStream(stream: IStream;
      useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
    constructor Create(width, height, stride: Integer; format: TPixelFormat;
      scan0: PBYTE); reintroduce; overload;
    constructor Create(width, height: Integer;
      format: TPixelFormat = PixelFormat32bppARGB); reintroduce; overload;
    constructor Create(width, height: Integer; target: TGPGraphics);
      reintroduce; overload;
    function Clone(rect: TGPRect; format: TPixelFormat): TGPBitmap; overload;
    function Clone(x, y, width, height: Integer;
      format: TPixelFormat): TGPBitmap; overload;
    function Clone(rect: TGPRectF; format: TPixelFormat): TGPBitmap; overload;
    function Clone(x, y, width, height: Single;
      format: TPixelFormat): TGPBitmap; overload;
    function LockBits(rect: TGPRect; flags: UINT; format: TPixelFormat;
      out lockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(var lockedBitmapData: TBitmapData): TStatus;
    function GetPixel(x, y: Integer; out color: TGPColor): TStatus;
    function SetPixel(x, y: Integer; color: TGPColor): TStatus;
    function SetResolution(xdpi, ydpi: Single): TStatus;
    constructor Create(var gdiBitmapInfo: TBITMAPINFO; gdiBitmapData: Pointer);
      reintroduce; overload;
    constructor Create(hbm: HBITMAP; hpal: HPALETTE); reintroduce; overload;
    constructor Create(hicon: hicon); reintroduce; overload;
    constructor Create(hInstance: HMODULE; bitmapName: WideString);
      reintroduce; overload;
    function FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO;
      gdiBitmapData: Pointer): TGPBitmap;
    function FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE): TGPBitmap;
    function FromHICON(hicon: hicon): TGPBitmap;
    function FromResource(hInstance: HMODULE;
      bitmapName: WideString): TGPBitmap;
    function GetHBITMAP(colorBackground: TGPColor;
      out hbmReturn: HBITMAP): TStatus;
    function GetHICON(out hicon: hicon): TStatus;
  end;

  TGPCustomLineCap = class(TGdiplusBase)
  protected
    nativeCap: GpCustomLineCap;
    lastResult: TStatus;
    procedure SetNativeCap(nativeCap: GpCustomLineCap);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeCap: GpCustomLineCap; status: TStatus);
      reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    constructor Create(fillPath, strokePath: TGPGraphicsPath;
      baseCap: TLineCap = LineCapFlat; baseInset: Single = 0);
      reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPCustomLineCap;
    function SetStrokeCap(strokeCap: TLineCap): TStatus;
    function SetStrokeCaps(startCap, endCap: TLineCap): TStatus;
    function GetStrokeCaps(out startCap, endCap: TLineCap): TStatus;
    function SetStrokeJoin(lineJoin: TLineJoin): TStatus;
    function GetStrokeJoin: TLineJoin;
    function SetBaseCap(baseCap: TLineCap): TStatus;
    function GetBaseCap: TLineCap;
    function SetBaseInset(inset: Single): TStatus;
    function GetBaseInset: Single;
    function SetWidthScale(widthScale: Single): TStatus;
    function GetWidthScale: Single;
    function GetLastStatus: TStatus;
  end;

  TGPCachedBitmap = class(TGdiplusBase)
  protected
    nativeCachedBitmap: GpCachedBitmap;
    lastResult: TStatus;
  public
    constructor Create(bitmap: TGPBitmap; graphics: TGPGraphics); reintroduce;
    destructor Destroy; override;
    function GetLastStatus: TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Image Attributes used with Graphics.DrawImage
    *
    * There are 5 possible sets of color adjustments:
    *          ColorAdjustDefault,
    *          ColorAdjustBitmap,
    *          ColorAdjustBrush,
    *          ColorAdjustPen,
    *          ColorAdjustText,
    *
    * Bitmaps, Brushes, Pens, and Text will all use any color adjustments
    * that have been set into the default ImageAttributes until their own
    * color adjustments have been set.  So as soon as any "Set" method is
    * called for Bitmaps, Brushes, Pens, or Text, then they start from
    * scratch with only the color adjustments that have been set for them.
    * Calling Reset removes any individual color adjustments for a type
    * and makes it revert back to using all the default color adjustments
    * (if any).  The SetToIdentity method is a way to force a type to
    * have no color adjustments at all, regardless of what previous adjustments
    * have been set for the defaults or for that type.
    *
    \********************************************************************F***** *)

  TGPImageAttributes = class(TGdiplusBase)
  protected
    nativeImageAttr: GpImageAttributes;
    lastResult: TStatus;
    procedure SetNativeImageAttr(nativeImageAttr: GpImageAttributes);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(imageAttr: GpImageAttributes; status: GpStatus);
      reintroduce; overload;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPImageAttributes;
    function SetToIdentity(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function Reset(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrix(const colorMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrix(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetColorMatrices(const colorMatrix: TColorMatrix;
      const grayMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrices
      (type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetThreshold(threshold: Single;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearThreshold(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetGamma(gamma: Single;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearNoOp(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetColorKey(colorLow, colorHigh: TGPColor;
      type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function ClearColorKey(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetOutputChannel(channelFlags: TColorChannelFlags;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannel
      (type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetOutputChannelColorProfile(colorProfileFilename: WideString;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannelColorProfile
      (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetRemapTable(mapSize: Cardinal; map: PColorMap;
      type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearRemapTable(type_: TColorAdjustType = ColorAdjustTypeDefault)
      : TStatus;
    function SetBrushRemapTable(mapSize: Cardinal; map: PColorMap): TStatus;
    function ClearBrushRemapTable: TStatus;
    function SetWrapMode(wrap: TWrapMode; color: TGPColor = aclBlack;
      clamp: BOOL = FALSE): TStatus;
    // The flags of the palette are ignored.
    function GetAdjustedPalette(colorPalette: PColorPalette;
      colorAdjustType: TColorAdjustType): TStatus;
    function GetLastStatus: TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Matrix class
    *
    \************************************************************************* *)

  TMatrixArray = array [0 .. 5] of Single;

  TGPMatrix = class(TGdiplusBase)
  protected
    nativeMatrix: GpMatrix;
    lastResult: GpStatus;
    procedure SetNativeMatrix(nativeMatrix: GpMatrix);
    function SetStatus(status: GpStatus): TStatus;
    constructor Create(nativeMatrix: GpMatrix); reintroduce; overload;
  public
    // Default constructor is set to identity matrix.
    constructor Create; reintroduce; overload;
    constructor Create(m11, m12, m21, m22, dx, dy: Single); reintroduce;
      overload;
    constructor Create(const rect: TGPRectF; const dstplg: TGPPointF);
      reintroduce; overload;
    constructor Create(const rect: TGPRect; const dstplg: TGPPoint);
      reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPMatrix;
    function GetElements(const m: TMatrixArray): TStatus;
    function SetElements(m11, m12, m21, m22, dx, dy: Single): TStatus;
    function OffsetX: Single;
    function OffsetY: Single;
    function Reset: TStatus;
    function Multiply(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Translate(OffsetX, OffsetY: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Scale(scaleX, scaleY: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Rotate(angle: Single; order: TMatrixOrder = MatrixOrderPrepend)
      : TStatus; // ok
    function RotateAt(angle: Single; const center: TGPPointF;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Shear(shearX, shearY: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus; // ok
    function Invert: TStatus; // ok

    function TransformPoints(pts: PGPPointF; count: Integer = 1): TStatus;
      overload;
    function TransformPoints(pts: PGPPoint; count: Integer = 1): TStatus;
      overload;

    function TransformVectors(pts: PGPPointF; count: Integer = 1): TStatus;
      overload;
    function TransformVectors(pts: PGPPoint; count: Integer = 1): TStatus;
      overload;

    function IsInvertible: BOOL;
    function IsIdentity: BOOL;
    function Equals(matrix: TGPMatrix): BOOL;
    function GetLastStatus: TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Brush class
    *
    \************************************************************************* *)

  // --------------------------------------------------------------------------
  // Abstract base class for various brush types
  // --------------------------------------------------------------------------

  TGPBrush = class(TGdiplusBase)
  protected
    nativeBrush: GpBrush;
    lastResult: TStatus;
    procedure SetNativeBrush(nativeBrush: GpBrush);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativeBrush: GpBrush; status: TStatus); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Clone: TGPBrush; virtual;
    function GetType: TBrushType;
    function GetLastStatus: TStatus;
  end;

  // --------------------------------------------------------------------------
  // Solid Fill Brush Object
  // --------------------------------------------------------------------------

  TGPSolidBrush = class(TGPBrush)
  public
    constructor Create(color: TGPColor); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function GetColor(out color: TGPColor): TStatus;
    function SetColor(color: TGPColor): TStatus;
  end;

  // --------------------------------------------------------------------------
  // Texture Brush Fill Object
  // --------------------------------------------------------------------------

  TGPTextureBrush = class(TGPBrush)
  public
    constructor Create(image: TGPImage; wrapMode: TWrapMode = WrapModeTile);
      reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode;
      dstRect: TGPRectF); reintroduce; overload;
    constructor Create(image: TGPImage; dstRect: TGPRectF;
      imageAttributes: TGPImageAttributes = nil); reintroduce; overload;
    constructor Create(image: TGPImage; dstRect: TGPRect;
      imageAttributes: TGPImageAttributes = nil); reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode; dstRect: TGPRect);
      reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode;
      dstX, dstY, dstWidth, dstHeight: Single); reintroduce; overload;
    constructor Create(image: TGPImage; wrapMode: TWrapMode;
      dstX, dstY, dstWidth, dstHeight: Integer); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: MatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(wrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
    function GetImage: TGPImage;
  end;

  // --------------------------------------------------------------------------
  // Linear Gradient Brush Object
  // --------------------------------------------------------------------------

  TGPLinearGradientBrush = class(TGPBrush)
  public
    constructor Create; reintroduce; overload;
    constructor Create(const point1, point2: TGPPointF;
      color1, color2: TGPColor); reintroduce; overload;
    constructor Create(const point1, point2: TGPPoint;
      color1, color2: TGPColor); reintroduce; overload;
    constructor Create(rect: TGPRectF; color1, color2: TGPColor;
      mode: TLinearGradientMode); reintroduce; overload;
    constructor Create(rect: TGPRect; color1, color2: TGPColor;
      mode: TLinearGradientMode); reintroduce; overload;
    constructor Create(rect: TGPRectF; color1, color2: TGPColor; angle: Single;
      isAngleScalable: BOOL = FALSE); overload;
    constructor Create(rect: TGPRect; color1, color2: TGPColor; angle: Single;
      isAngleScalable: BOOL = FALSE); overload;
    function SetLinearColors(color1, color2: TGPColor): TStatus;
    function GetLinearColors(out color1, color2: TGPColor): TStatus;
    function GetRectangle(out rect: TGPRectF): TStatus; overload;
    function GetRectangle(out rect: TGPRect): TStatus; overload;
    function SetGammaCorrection(useGammaCorrection: BOOL): TStatus;
    function GetGammaCorrection: BOOL;
    function GetBlendCount: Integer;
    function SetBlend(blendFactors, blendPositions: PSingle;
      count: Integer): TStatus;
    function GetBlend(blendFactors, blendPositions: PSingle;
      count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(presetColors: PGPColor;
      blendPositions: PSingle; count: Integer): TStatus;
    function GetInterpolationColors(presetColors: PGPColor;
      blendPositions: PSingle; count: Integer): TStatus;
    function SetBlendBellShape(focus: Single; Scale: Single = 1.0): TStatus;
    function SetBlendTriangularShape(focus: Single;
      Scale: Single = 1.0): TStatus;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(wrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
  end;

  // --------------------------------------------------------------------------
  // Hatch Brush Object
  // --------------------------------------------------------------------------

  TGPHatchBrush = class(TGPBrush)
  public
    constructor Create; reintroduce; overload;
    constructor Create(hatchStyle: THatchStyle; foreColor: TGPColor;
      backColor: TGPColor = aclBlack); reintroduce; overload; // ok
    function GetHatchStyle: THatchStyle;
    function GetForegroundColor(out color: TGPColor): TStatus;
    function GetBackgroundColor(out color: TGPColor): TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Pen class
    *
    \************************************************************************* *)

  // --------------------------------------------------------------------------
  // Pen class
  // --------------------------------------------------------------------------

  TGPPen = class(TGdiplusBase)
  protected
    nativePen: GpPen;
    lastResult: TStatus;
    procedure SetNativePen(nativePen: GpPen);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativePen: GpPen; status: TStatus); reintroduce;
      overload;
  public
    constructor Create(color: TGPColor; width: Single = 1.0); reintroduce;
      overload;
    constructor Create(brush: TGPBrush; width: Single = 1.0); reintroduce;
      overload;
    destructor Destroy; override;
    function Clone: TGPPen;
    function SetWidth(width: Single): TStatus;
    function GetWidth: Single;
    // Set/get line caps: start, end, and dash
    // Line cap and join APIs by using LineCap and LineJoin enums.
    function SetLineCap(startCap, endCap: TLineCap; dashCap: TDashCap): TStatus;
    function SetStartCap(startCap: TLineCap): TStatus;
    function SetEndCap(endCap: TLineCap): TStatus;
    function SetDashCap(dashCap: TDashCap): TStatus;
    function GetStartCap: TLineCap;
    function GetEndCap: TLineCap;
    function GetDashCap: TDashCap;
    function SetLineJoin(lineJoin: TLineJoin): TStatus;
    function GetLineJoin: TLineJoin;
    function SetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
    function GetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
    function SetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
    function GetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
    function SetMiterLimit(miterLimit: Single): TStatus;
    function GetMiterLimit: Single;
    function SetAlignment(penAlignment: TPenAlignment): TStatus;
    function GetAlignment: TPenAlignment;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetPenType: TPenType;
    function SetColor(color: TGPColor): TStatus;
    function SetBrush(brush: TGPBrush): TStatus;
    function GetColor(out color: TGPColor): TStatus;
    function GetBrush: TGPBrush;
    function GetDashStyle: TDashStyle;
    function SetDashStyle(dashStyle: TDashStyle): TStatus;
    function GetDashOffset: Single;
    function SetDashOffset(dashOffset: Single): TStatus;
    function SetDashPattern(dashArray: PSingle; count: Integer): TStatus;
    function GetDashPatternCount: Integer;
    function GetDashPattern(dashArray: PSingle; count: Integer): TStatus;
    function SetCompoundArray(compoundArray: PSingle; count: Integer): TStatus;
    function GetCompoundArrayCount: Integer;
    function GetCompoundArray(compoundArray: PSingle; count: Integer): TStatus;
    function GetLastStatus: TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ StringFormat class
    *
    \************************************************************************* *)

  TGPStringFormat = class(TGdiplusBase)
  protected
    nativeFormat: GpStringFormat;
    lastError: TStatus;
    function SetStatus(newStatus: GpStatus): TStatus;
    procedure Assign(source: TGPStringFormat);
    constructor Create(clonedStringFormat: GpStringFormat; status: TStatus);
      reintroduce; overload;
  public
    constructor Create(formatFlags: Integer = 0;
      language: LANGID = LANG_NEUTRAL); reintroduce; overload;
    constructor Create(format: TGPStringFormat); reintroduce; overload;
    destructor Destroy; override;
    class function GenericDefault: TGPStringFormat;
    class function GenericTypographic: TGPStringFormat;
    function Clone: TGPStringFormat;
    function SetFormatFlags(flags: Integer): TStatus;
    function GetFormatFlags: Integer;
    function SetAlignment(align: TStringAlignment): TStatus;
    function GetAlignment: TStringAlignment;
    function SetLineAlignment(align: TStringAlignment): TStatus;
    function GetLineAlignment: TStringAlignment;
    function SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
    function GetHotkeyPrefix: THotkeyPrefix;
    function SetTabStops(firstTabOffset: Single; count: Integer;
      tabStops: PSingle): TStatus;
    function GetTabStopCount: Integer;
    function GetTabStops(count: Integer;
      firstTabOffset, tabStops: PSingle): TStatus;
    function SetDigitSubstitution(language: LANGID;
      substitute: TStringDigitSubstitute): TStatus;
    function GetDigitSubstitutionLanguage: LANGID;
    function GetDigitSubstitutionMethod: TStringDigitSubstitute;
    function SetTrimming(trimming: TStringTrimming): TStatus;
    function GetTrimming: TStringTrimming;
    function SetMeasurableCharacterRanges(rangeCount: Integer;
      ranges: PCharacterRange): TStatus;
    function GetMeasurableCharacterRangeCount: Integer;
    function GetLastStatus: TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Graphics Path class
    *
    \************************************************************************* *)

  TGPGraphicsPath = class(TGdiplusBase)
  protected
    nativePath: GpPath;
    lastResult: TStatus;
    procedure SetNativePath(nativePath: GpPath);
    function SetStatus(status: TStatus): TStatus;
    constructor Create(nativePath: GpPath); reintroduce; overload;
  public
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create(fillMode: TFillMode = FillModeAlternate); reintroduce;
      overload;
    constructor Create(points: PGPPointF; types: PBYTE; count: Integer;
      fillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    constructor Create(points: PGPPoint; types: PBYTE; count: Integer;
      fillMode: TFillMode = FillModeAlternate); reintroduce; overload;
    destructor Destroy; override;
    function Clone: TGPGraphicsPath;
    // Reset the path object to empty (and fill mode to FillModeAlternate)
    function Reset: TStatus;
    function GetFillMode: TFillMode;
    function SetFillMode(fillMode: TFillMode): TStatus;
    function GetPathData(pathData: TPathData): TStatus;
    function StartFigure: TStatus;
    function CloseFigure: TStatus;
    function CloseAllFigures: TStatus;
    function SetMarker: TStatus;
    function ClearMarkers: TStatus;
    function Reverse: TStatus;
    function GetLastPoint(out lastPoint: TGPPointF): TStatus;

    function AddLine(const pt1, pt2: TGPPointF): TStatus; overload;
    function AddLine(x1, y1, x2, y2: Single): TStatus; overload;
    function AddLines(points: PGPPointF; count: Integer): TStatus; overload;
    function AddLine(const pt1, pt2: TGPPoint): TStatus; overload;
    function AddLine(x1, y1, x2, y2: Integer): TStatus; overload;
    function AddLines(points: PGPPoint; count: Integer): TStatus; overload;

    function AddArc(rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
      overload;
    function AddArc(x, y, width, height, startAngle, sweepAngle: Single)
      : TStatus; overload;
    function AddArc(rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
      overload;
    function AddArc(x, y, width, height: Integer;
      startAngle, sweepAngle: Single): TStatus; overload;

    function AddBezier(pt1, pt2, pt3, pt4: TGPPointF): TStatus; overload;
    function AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Single): TStatus;
      overload;
    function AddBeziers(points: PGPPointF; count: Integer): TStatus; overload;
    function AddBezier(pt1, pt2, pt3, pt4: TGPPoint): TStatus; overload;
    function AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer): TStatus;
      overload;
    function AddBeziers(points: PGPPoint; count: Integer): TStatus; overload;

    function AddCurve(points: PGPPointF; count: Integer): TStatus; overload;
    function AddCurve(points: PGPPointF; count: Integer;
      tension: Single): TStatus; overload;
    function AddCurve(points: PGPPointF; count, offset,
      numberOfSegments: Integer; tension: Single): TStatus; overload;
    function AddCurve(points: PGPPoint; count: Integer): TStatus; overload;
    function AddCurve(points: PGPPoint; count: Integer;
      tension: Single): TStatus; overload;
    function AddCurve(points: PGPPoint; count, offset,
      numberOfSegments: Integer; tension: Single): TStatus; overload;

    function AddClosedCurve(points: PGPPointF; count: Integer): TStatus;
      overload;
    function AddClosedCurve(points: PGPPointF; count: Integer;
      tension: Single): TStatus; overload;
    function AddClosedCurve(points: PGPPoint; count: Integer): TStatus;
      overload;
    function AddClosedCurve(points: PGPPoint; count: Integer;
      tension: Single): TStatus; overload;

    function AddRectangle(rect: TGPRectF): TStatus; overload;
    function AddRectangles(rects: PGPRectF; count: Integer): TStatus; overload;
    function AddRectangle(rect: TGPRect): TStatus; overload;
    function AddRectangles(rects: PGPRect; count: Integer): TStatus; overload;

    function AddEllipse(rect: TGPRectF): TStatus; overload;
    function AddEllipse(x, y, width, height: Single): TStatus; overload;
    function AddEllipse(rect: TGPRect): TStatus; overload;
    function AddEllipse(x, y, width, height: Integer): TStatus; overload;

    function AddPie(rect: TGPRectF; startAngle, sweepAngle: Single): TStatus;
      overload;
    function AddPie(x, y, width, height, startAngle, sweepAngle: Single)
      : TStatus; overload;
    function AddPie(rect: TGPRect; startAngle, sweepAngle: Single): TStatus;
      overload;
    function AddPie(x, y, width, height: Integer;
      startAngle, sweepAngle: Single): TStatus; overload;

    function AddPolygon(points: PGPPointF; count: Integer): TStatus; overload;
    function AddPolygon(points: PGPPoint; count: Integer): TStatus; overload;

    function AddPath(addingPath: TGPGraphicsPath; connect: BOOL): TStatus;

    function AddString(string_: WideString; length: Integer;
      family: TGPFontFamily; style: Integer; emSize: Single;
      origin: TGPPointF; format: TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; length: Integer;
      family: TGPFontFamily; style: Integer; emSize: Single;
      layoutRect: TGPRectF; format: TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; length: Integer;
      family: TGPFontFamily; style: Integer; emSize: Single; origin: TGPPoint;
      format: TGPStringFormat): TStatus; overload;
    function AddString(string_: WideString; length: Integer;
      family: TGPFontFamily; style: Integer; emSize: Single;
      layoutRect: TGPRect; format: TGPStringFormat): TStatus; overload;

    function Transform(matrix: TGPMatrix): TStatus;

    // This is not always the tightest bounds.
    function GetBounds(out bounds: TGPRectF; matrix: TGPMatrix = nil;
      pen: TGPPen = nil): TStatus; overload;
    function GetBounds(out bounds: TGPRect; matrix: TGPMatrix = nil;
      pen: TGPPen = nil): TStatus; overload;
    // Once flattened, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL the
    // identity matrix is assumed.
    function Flatten(matrix: TGPMatrix = nil;
      flatness: Single = FlatnessDefault): TStatus;
    function Widen(pen: TGPPen; matrix: TGPMatrix = nil;
      flatness: Single = FlatnessDefault): TStatus;
    function Outline(matrix: TGPMatrix = nil;
      flatness: Single = FlatnessDefault): TStatus;
    // Once this is called, the resultant path is made of line segments and
    // the original path information is lost.  When matrix is NULL, the
    // identity matrix is assumed.
    function Warp(destPoints: PGPPointF; count: Integer; srcRect: TGPRectF;
      matrix: TGPMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
      flatness: Single = FlatnessDefault): TStatus;
    function GetPointCount: Integer;
    function GetPathTypes(types: PBYTE; count: Integer): TStatus;
    function GetPathPoints(points: PGPPointF; count: Integer): TStatus;
      overload;
    function GetPathPoints(points: PGPPoint; count: Integer): TStatus; overload;
    function GetLastStatus: TStatus;

    function IsVisible(point: TGPPointF; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(point: TGPPoint; g: TGPGraphics = nil): BOOL; overload;
    function IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL; overload;

    function IsOutlineVisible(point: TGPPointF; pen: TGPPen;
      g: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(x, y: Single; pen: TGPPen;
      g: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(point: TGPPoint; pen: TGPPen;
      g: TGPGraphics = nil): BOOL; overload;
    function IsOutlineVisible(x, y: Integer; pen: TGPPen;
      g: TGPGraphics = nil): BOOL; overload;
  end;

  // --------------------------------------------------------------------------
  // GraphisPathIterator class
  // --------------------------------------------------------------------------

  TGPGraphicsPathIterator = class(TGdiplusBase)
  protected
    nativeIterator: GpPathIterator;
    lastResult: TStatus;
    procedure SetNativeIterator(nativeIterator: GpPathIterator);
    function SetStatus(status: TStatus): TStatus;
  public
    constructor Create(path: TGPGraphicsPath); reintroduce;
    destructor Destroy; override;
    function NextSubpath(out startIndex, endIndex: Integer;
      out isClosed: BOOL): Integer; overload;
    function NextSubpath(path: TGPGraphicsPath; out isClosed: BOOL): Integer;
      overload;
    function NextPathType(out pathType: TPathPointType;
      out startIndex, endIndex: Integer): Integer;
    function NextMarker(out startIndex, endIndex: Integer): Integer; overload;
    function NextMarker(path: TGPGraphicsPath): Integer; overload;
    function GetCount: Integer;
    function GetSubpathCount: Integer;
    function HasCurve: BOOL;
    procedure Rewind;
    function Enumerate(points: PGPPointF; types: PBYTE;
      count: Integer): Integer;
    function CopyData(points: PGPPointF; types: PBYTE;
      startIndex, endIndex: Integer): Integer;
    function GetLastStatus: TStatus;
  end;

  // --------------------------------------------------------------------------
  // Path Gradient Brush
  // --------------------------------------------------------------------------

  TGPPathGradientBrush = class(TGPBrush)
  public
    constructor Create(points: PGPPointF; count: Integer;
      wrapMode: TWrapMode = WrapModeClamp); reintroduce; overload;
    constructor Create(points: PGPPoint; count: Integer;
      wrapMode: TWrapMode = WrapModeClamp); reintroduce; overload;
    constructor Create(path: TGPGraphicsPath); reintroduce; overload;
    constructor Create; reintroduce; overload;
    function GetCenterColor(out color: TGPColor): TStatus;
    function SetCenterColor(color: TGPColor): TStatus;
    function GetPointCount: Integer;
    function GetSurroundColorCount: Integer;
    function GetSurroundColors(colors: PARGB; var count: Integer): TStatus;
    function SetSurroundColors(colors: PARGB; var count: Integer): TStatus;
    function GetGraphicsPath(path: TGPGraphicsPath): TStatus;
    function SetGraphicsPath(path: TGPGraphicsPath): TStatus;
    function GetCenterPoint(out point: TGPPointF): TStatus; overload;
    function GetCenterPoint(out point: TGPPoint): TStatus; overload;
    function SetCenterPoint(point: TGPPointF): TStatus; overload;
    function SetCenterPoint(point: TGPPoint): TStatus; overload;
    function GetRectangle(out rect: TGPRectF): TStatus; overload;
    function GetRectangle(out rect: TGPRect): TStatus; overload;
    function SetGammaCorrection(useGammaCorrection: BOOL): TStatus; overload;
    function GetGammaCorrection: BOOL; overload;
    function GetBlendCount: Integer;
    function GetBlend(blendFactors, blendPositions: PSingle;
      count: Integer): TStatus;
    function SetBlend(blendFactors, blendPositions: PSingle;
      count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(presetColors: PARGB;
      blendPositions: PSingle; count: Integer): TStatus;
    function GetInterpolationColors(presetColors: PARGB;
      blendPositions: PSingle; count: Integer): TStatus;
    function SetBlendBellShape(focus: Single; Scale: Single = 1.0): TStatus;
    function SetBlendTriangularShape(focus: Single;
      Scale: Single = 1.0): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function SetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetFocusScales(out xScale, yScale: Single): TStatus;
    function SetFocusScales(xScale, yScale: Single): TStatus;
    function GetWrapMode: TWrapMode;
    function SetWrapMode(wrapMode: TWrapMode): TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Graphics Object
    *
    \************************************************************************* *)

  TGPGraphics = class(TGdiplusBase)
  protected
    nativeGraphics: GpGraphics;
    lastResult: TStatus;
    procedure SetNativeGraphics(graphics: GpGraphics);
    function SetStatus(status: TStatus): TStatus;
    function GetNativeGraphics: GpGraphics;
    function GetNativePen(pen: TGPPen): GpPen;
    constructor Create(graphics: GpGraphics); reintroduce; overload;
  public
    function FromHDC(hdc: hdc): TGPGraphics; overload;
    function FromHDC(hdc: hdc; hdevice: THANDLE): TGPGraphics; overload;
    function FromHWND(hwnd: hwnd; icm: BOOL = FALSE): TGPGraphics;
    function FromImage(image: TGPImage): TGPGraphics;
    constructor Create(hdc: hdc); reintroduce; overload;
    constructor Create(hdc: hdc; hdevice: THANDLE); reintroduce; overload;
    constructor Create(hwnd: hwnd; icm: BOOL
      { = FALSE } ); reintroduce; overload;
    constructor Create(image: TGPImage); reintroduce; overload;
    destructor Destroy; override;
    procedure Flush(intention: TFlushIntention = FlushIntentionFlush);
    // ------------------------------------------------------------------------
    // GDI Interop methods
    // ------------------------------------------------------------------------
    // Locks the graphics until ReleaseDC is called
    function GetHDC: hdc;
    procedure ReleaseHDC(hdc: hdc);
    // ------------------------------------------------------------------------
    // Rendering modes
    // ------------------------------------------------------------------------
    function SetRenderingOrigin(x, y: Integer): TStatus;
    function GetRenderingOrigin(out x, y: Integer): TStatus;
    function SetCompositingMode(compositingMode: TCompositingMode): TStatus;
    function GetCompositingMode: TCompositingMode;
    function SetCompositingQuality(compositingQuality: TCompositingQuality)
      : TStatus;
    function GetCompositingQuality: TCompositingQuality;
    function SetTextRenderingHint(newMode: TTextRenderingHint): TStatus;
    function GetTextRenderingHint: TTextRenderingHint;
    function SetTextContrast(contrast: UINT): TStatus; // 0..12
    function GetTextContrast: UINT;
    function GetInterpolationMode: TInterpolationMode;
    function SetInterpolationMode(interpolationMode: TInterpolationMode)
      : TStatus;
    function GetSmoothingMode: TSmoothingMode;
    function SetSmoothingMode(smoothingMode: TSmoothingMode): TStatus;
    function GetPixelOffsetMode: TPixelOffsetMode;
    function SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
    // ------------------------------------------------------------------------
    // Manipulate current world transform
    // ------------------------------------------------------------------------
    function SetTransform(matrix: TGPMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(matrix: TGPMatrix;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(dx, dy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(sx, sy: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(angle: Single;
      order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetTransform(matrix: TGPMatrix): TStatus;
    function SetPageUnit(unit_: TUnit): TStatus;
    function SetPageScale(Scale: Single): TStatus;
    function GetPageUnit: TUnit;
    function GetPageScale: Single;
    function GetDpiX: Single;
    function GetDpiY: Single;
    function TransformPoints(destSpace: TCoordinateSpace;
      srcSpace: TCoordinateSpace; pts: PGPPointF; count: Integer): TStatus;
      overload;
    function TransformPoints(destSpace: TCoordinateSpace;
      srcSpace: TCoordinateSpace; pts: PGPPoint; count: Integer): TStatus;
      overload;
    // ------------------------------------------------------------------------
    // GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
    // ------------------------------------------------------------------------
    function GetNearestColor(var color: TGPColor): TStatus;

    // DrawLine(s)
    function DrawLine(pen: TGPPen; x1, y1, x2, y2: Single): TStatus; overload;
    function DrawLine(pen: TGPPen; const pt1, pt2: TGPPointF): TStatus;
      overload;
    function DrawLines(pen: TGPPen; points: PGPPointF;
      count: Integer): TStatus; overload;
    function DrawLine(pen: TGPPen; x1, y1, x2, y2: Integer): TStatus; overload;
    function DrawLine(pen: TGPPen; const pt1, pt2: TGPPoint): TStatus; overload;
    function DrawLines(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
      overload;

    // DrawArc
    function DrawArc(pen: TGPPen; x, y, width, height, startAngle,
      sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; const rect: TGPRectF;
      startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; x, y, width, height: Integer;
      startAngle, sweepAngle: Single): TStatus; overload;
    function DrawArc(pen: TGPPen; const rect: TGPRect;
      startAngle, sweepAngle: Single): TStatus; overload;

    // DrawBezier(s)
    function DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Single)
      : TStatus; overload;
    function DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPointF)
      : TStatus; overload;
    function DrawBeziers(pen: TGPPen; points: PGPPointF;
      count: Integer): TStatus; overload;
    function DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4, y4: Integer)
      : TStatus; overload;
    function DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPoint)
      : TStatus; overload;
    function DrawBeziers(pen: TGPPen; points: PGPPoint;
      count: Integer): TStatus; overload;

    // DrawRectangle(s)
    function DrawRectangle(pen: TGPPen; const rect: TGPRectF): TStatus;
      overload;
    function DrawRectangle(pen: TGPPen; x, y, width, height: Single): TStatus;
      overload;
    function DrawRectangles(pen: TGPPen; rects: PGPRectF;
      count: Integer): TStatus; overload;
    function DrawRectangle(pen: TGPPen; const rect: TGPRect): TStatus; overload;
    function DrawRectangle(pen: TGPPen; x, y, width, height: Integer): TStatus;
      overload;
    function DrawRectangles(pen: TGPPen; rects: PGPRect;
      count: Integer): TStatus; overload;

    // DrawEllipse
    function DrawEllipse(pen: TGPPen; const rect: TGPRectF): TStatus; overload;
    function DrawEllipse(pen: TGPPen; x, y, width, height: Single): TStatus;
      overload;
    function DrawEllipse(pen: TGPPen; const rect: TGPRect): TStatus; overload;
    function DrawEllipse(pen: TGPPen; x, y, width, height: Integer): TStatus;
      overload;

    // DrawPie
    function DrawPie(pen: TGPPen; const rect: TGPRectF;
      startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; x, y, width, height, startAngle,
      sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; const rect: TGPRect;
      startAngle, sweepAngle: Single): TStatus; overload;
    function DrawPie(pen: TGPPen; x, y, width, height: Integer;
      startAngle, sweepAngle: Single): TStatus; overload;

    // DrawPolygon
    function DrawPolygon(pen: TGPPen; points: PGPPointF;
      count: Integer): TStatus; overload;
    function DrawPolygon(pen: TGPPen; points: PGPPoint;
      count: Integer): TStatus; overload;

    // DrawPath
    function DrawPath(pen: TGPPen; path: TGPGraphicsPath): TStatus;

    // DrawCurve
    function DrawCurve(pen: TGPPen; points: PGPPointF;
      count: Integer): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPointF; count: Integer;
      tension: Single): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPointF; count, offset,
      numberOfSegments: Integer; tension: Single = 0.5): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer): TStatus;
      overload;
    function DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer;
      tension: Single): TStatus; overload;
    function DrawCurve(pen: TGPPen; points: PGPPoint; count, offset,
      numberOfSegments: Integer; tension: Single = 0.5): TStatus; overload;

    // DrawClosedCurve
    function DrawClosedCurve(pen: TGPPen; points: PGPPointF;
      count: Integer): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; points: PGPPointF; count: Integer;
      tension: Single): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; points: PGPPoint;
      count: Integer): TStatus; overload;
    function DrawClosedCurve(pen: TGPPen; points: PGPPoint; count: Integer;
      tension: Single): TStatus; overload;

    // Clear
    function Clear(color: TGPColor): TStatus;

    // FillRectangle(s)
    function FillRectangle(brush: TGPBrush; const rect: TGPRectF): TStatus;
      overload;
    function FillRectangle(brush: TGPBrush; x, y, width, height: Single)
      : TStatus; overload;
    function FillRectangles(brush: TGPBrush; rects: PGPRectF;
      count: Integer): TStatus; overload;
    function FillRectangle(brush: TGPBrush; const rect: TGPRect): TStatus;
      overload;
    function FillRectangle(brush: TGPBrush; x, y, width, height: Integer)
      : TStatus; overload;
    function FillRectangles(brush: TGPBrush; rects: PGPRect;
      count: Integer): TStatus; overload;

    // FillPolygon
    function FillPolygon(brush: TGPBrush; points: PGPPointF;
      count: Integer): TStatus; overload;
    function FillPolygon(brush: TGPBrush; points: PGPPointF; count: Integer;
      fillMode: TFillMode): TStatus; overload;
    function FillPolygon(brush: TGPBrush; points: PGPPoint;
      count: Integer): TStatus; overload;
    function FillPolygon(brush: TGPBrush; points: PGPPoint; count: Integer;
      fillMode: TFillMode): TStatus; overload;

    // FillEllipse
    function FillEllipse(brush: TGPBrush; const rect: TGPRectF): TStatus;
      overload;
    function FillEllipse(brush: TGPBrush; x, y, width, height: Single)
      : TStatus; overload;
    function FillEllipse(brush: TGPBrush; const rect: TGPRect): TStatus;
      overload;
    function FillEllipse(brush: TGPBrush; x, y, width, height: Integer)
      : TStatus; overload;

    // FillPie
    function FillPie(brush: TGPBrush; const rect: TGPRectF;
      startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; x, y, width, height, startAngle,
      sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; const rect: TGPRect;
      startAngle, sweepAngle: Single): TStatus; overload;
    function FillPie(brush: TGPBrush; x, y, width, height: Integer;
      startAngle, sweepAngle: Single): TStatus; overload;

    // FillPath
    function fillPath(brush: TGPBrush; path: TGPGraphicsPath): TStatus;

    // FillClosedCurve
    function FillClosedCurve(brush: TGPBrush; points: PGPPointF;
      count: Integer): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; points: PGPPointF;
      count: Integer; fillMode: TFillMode; tension: Single = 0.5): TStatus;
      overload;
    function FillClosedCurve(brush: TGPBrush; points: PGPPoint;
      count: Integer): TStatus; overload;
    function FillClosedCurve(brush: TGPBrush; points: PGPPoint; count: Integer;
      fillMode: TFillMode; tension: Single = 0.5): TStatus; overload;

    // FillRegion
    function FillRegion(brush: TGPBrush; region: TGPRegion): TStatus;

    // DrawString
    function DrawString(string_: WideString; length: Integer; Font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat;
      brush: TGPBrush): TStatus; overload;
    function DrawString(string_: WideString; length: Integer; Font: TGPFont;
      const origin: TGPPointF; brush: TGPBrush): TStatus; overload;
    function DrawString(string_: WideString; length: Integer; Font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat;
      brush: TGPBrush): TStatus; overload;

    // MeasureString
    function MeasureString(string_: WideString; length: Integer; Font: TGPFont;
      const layoutRect: TGPRectF; stringFormat: TGPStringFormat;
      out boundingBox: TGPRectF; codepointsFitted: PInteger = nil;
      linesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; Font: TGPFont;
      const layoutRectSize: TGPSizeF; stringFormat: TGPStringFormat;
      out size: TGPSizeF; codepointsFitted: PInteger = nil;
      linesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; Font: TGPFont;
      const origin: TGPPointF; stringFormat: TGPStringFormat;
      out boundingBox: TGPRectF): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; Font: TGPFont;
      const layoutRect: TGPRectF; out boundingBox: TGPRectF): TStatus; overload;
    function MeasureString(string_: WideString; length: Integer; Font: TGPFont;
      const origin: TGPPointF; out boundingBox: TGPRectF): TStatus; overload;

    // MeasureCharacterRanges
    function MeasureCharacterRanges(string_: WideString; length: Integer;
      Font: TGPFont; const layoutRect: TGPRectF;
      stringFormat: TGPStringFormat; regionCount: Integer;
      const regions: array of TGPRegion): TStatus; overload;

    // DrawDriverString
    function DrawDriverString(text: PUINT16; length: Integer; Font: TGPFont;
      brush: TGPBrush; positions: PGPPointF; flags: Integer;
      matrix: TGPMatrix): TStatus;

    // MeasureDriverString
    function MeasureDriverString(text: PUINT16; length: Integer; Font: TGPFont;
      positions: PGPPointF; flags: Integer; matrix: TGPMatrix;
      out boundingBox: TGPRectF): TStatus;

    // Draw a cached bitmap on this graphics destination offset by
    // x, y. Note this will fail with WrongState if the CachedBitmap
    // native format differs from this Graphics.
    function DrawCachedBitmap(cb: TGPCachedBitmap; x, y: Integer): TStatus;
    function DrawImage(image: TGPImage; const point: TGPPointF): TStatus;
      overload;
    function DrawImage(image: TGPImage; x, y: Single): TStatus; overload;
    function DrawImage(image: TGPImage; const rect: TGPRectF): TStatus;
      overload;
    function DrawImage(image: TGPImage; x, y, width, height: Single): TStatus;
      overload;
    function DrawImage(image: TGPImage; const point: TGPPoint): TStatus;
      overload;
    function DrawImage(image: TGPImage; x, y: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; const rect: TGPRect): TStatus; overload;
    function DrawImage(image: TGPImage; x, y, width, height: Integer): TStatus;
      overload;

    // Affine Draw Image
    // destPoints.length = 3: rect => parallelogram
    // destPoints[0] <=> top-left corner of the source rectangle
    // destPoints[1] <=> top-right corner
    // destPoints[2] <=> bottom-left corner
    // destPoints.length = 4: rect => quad
    // destPoints[3] <=> bottom-right corner
    function DrawImage(image: TGPImage; destPoints: PGPPointF;
      count: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; destPoints: PGPPoint;
      count: Integer): TStatus; overload;
    function DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Single; srcUnit: TUnit): TStatus; overload;
    function DrawImage(image: TGPImage; const destRect: TGPRectF;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil;
      callback: DrawImageAbort = nil; callbackData: Pointer = nil): TStatus;
      overload;
    function DrawImage(image: TGPImage; destPoints: PGPPointF; count: Integer;
      srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil;
      callback: DrawImageAbort = nil; callbackData: Pointer = nil): TStatus;
      overload;
    function DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth,
      srcheight: Integer; srcUnit: TUnit): TStatus; overload;
    function DrawImage(image: TGPImage; const destRect: TGPRect;
      srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil;
      callback: DrawImageAbort = nil; callbackData: Pointer = nil): TStatus;
      overload;
    function DrawImage(image: TGPImage; destPoints: PGPPoint;
      count, srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
      imageAttributes: TGPImageAttributes = nil;
      callback: DrawImageAbort = nil; callbackData: Pointer = nil): TStatus;
      overload;

    // The following methods are for playing an EMF+ to a graphics
    // via the enumeration interface.  Each record of the EMF+ is
    // sent to the callback (along with the callbackData).  Then
    // the callback can invoke the Metafile::PlayRecord method
    // to play the particular record.
    function EnumerateMetafile(metafile: TGPMetafile;
      const destPoint: TGPPointF; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
      : TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile;
      const destPoint: TGPPoint; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
      : TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRect;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
      count: Integer; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
      : TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
      count: Integer; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
      : TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile;
      const destPoint: TGPPointF; const srcRect: TGPRectF;
      srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
      : TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile;
      const destPoint: TGPPoint; const srcRect: TGPRect; srcUnit: TUnit;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; const destRect: TGPRectF;
      const srcRect: TGPRectF; srcUnit: TUnit; callback: EnumerateMetafileProc;
      callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
      : TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile;
      const destRect, srcRect: TGPRect; srcUnit: TUnit;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPointF;
      count: Integer; const srcRect: TGPRectF; srcUnit: TUnit;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(metafile: TGPMetafile; destPoints: PGPPoint;
      count: Integer; const srcRect: TGPRect; srcUnit: TUnit;
      callback: EnumerateMetafileProc; callbackData: Pointer = nil;
      imageAttributes: TGPImageAttributes = nil): TStatus; overload;

    // SetClip
    function SetClip(g: TGPGraphics;
      combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(rect: TGPRectF;
      combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(rect: TGPRect;
      combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(path: TGPGraphicsPath;
      combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(region: TGPRegion;
      combineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    // This is different than the other SetClip methods because it assumes
    // that the HRGN is already in device units, so it doesn't transform
    // the coordinates in the HRGN.
    function SetClip(hRgn: hRgn; combineMode: TCombineMode = CombineModeReplace)
      : TStatus; overload;

    // IntersectClip
    function IntersectClip(const rect: TGPRectF): TStatus; overload;
    function IntersectClip(const rect: TGPRect): TStatus; overload;
    function IntersectClip(region: TGPRegion): TStatus; overload;
    // ExcludeClip
    function ExcludeClip(const rect: TGPRectF): TStatus; overload;
    function ExcludeClip(const rect: TGPRect): TStatus; overload;
    function ExcludeClip(region: TGPRegion): TStatus; overload;

    function ResetClip: TStatus;

    function TranslateClip(dx, dy: Single): TStatus; overload;
    function TranslateClip(dx, dy: Integer): TStatus; overload;

    function GetClip(region: TGPRegion): TStatus;

    function GetClipBounds(out rect: TGPRectF): TStatus; overload;
    function GetClipBounds(out rect: TGPRect): TStatus; overload;

    function IsClipEmpty: BOOL;

    function GetVisibleClipBounds(out rect: TGPRectF): TStatus; overload;
    function GetVisibleClipBounds(out rect: TGPRect): TStatus; overload;

    function IsVisibleClipEmpty: BOOL;

    function IsVisible(x, y: Integer): BOOL; overload;
    function IsVisible(const point: TGPPoint): BOOL; overload;
    function IsVisible(x, y, width, height: Integer): BOOL; overload;
    function IsVisible(const rect: TGPRect): BOOL; overload;
    function IsVisible(x, y: Single): BOOL; overload;
    function IsVisible(const point: TGPPointF): BOOL; overload;
    function IsVisible(x, y, width, height: Single): BOOL; overload;
    function IsVisible(const rect: TGPRectF): BOOL; overload;

    function Save: GraphicsState;
    function Restore(gstate: GraphicsState): TStatus;

    function BeginContainer(const dstRect, srcRect: TGPRectF;
      unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer(const dstRect, srcRect: TGPRect;
      unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer: GraphicsContainer; overload;
    function EndContainer(state: GraphicsContainer): TStatus;

    // Only valid when recording metafiles.
    function AddMetafileComment(data: PBYTE; sizeData: UINT): TStatus;

    function GetHalftonePalette: HPALETTE;
    function GetLastStatus: TStatus;
  end;

  (* *************************************************************************\
    *
    *   GDI+ CustomLineCap APIs
    *
    \************************************************************************* *)

  TGPAdjustableArrowCap = class(TGPCustomLineCap)
  public
    constructor Create(height, width: Single; isFilled: BOOL = TRUE);
    function SetHeight(height: Single): TStatus;
    function GetHeight: Single;
    function SetWidth(width: Single): TStatus;
    function GetWidth: Single;
    function SetMiddleInset(middleInset: Single): TStatus;
    function GetMiddleInset: Single;
    function SetFillState(isFilled: BOOL): TStatus;
    function isFilled: BOOL;
  end;

  (* *************************************************************************\
    *
    *   GDI+ Metafile class
    *
    \************************************************************************* *)

  TGPMetafile = class(TGPImage)
  public
    // Playback a metafile from a HMETAFILE
    // If deleteWmf is TRUE, then when the metafile is deleted,
    // the hWmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hWmf: HMETAFILE;
      var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      deleteWmf: BOOL = FALSE); overload;
    // Playback a metafile from a HENHMETAFILE
    // If deleteEmf is TRUE, then when the metafile is deleted,
    // the hEmf will also be deleted.  Otherwise, it won't be.
    constructor Create(hEmf: HENHMETAFILE; deleteEmf: BOOL = FALSE); overload;
    constructor Create(filename: WideString); overload;
    // Playback a WMF metafile from a file.
    constructor Create(filename: WideString;
      var wmfPlaceableFileHeader: TWmfPlaceableFileHeader); overload;
    constructor Create(stream: IStream); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: hdc; type_: TEmfType = EmfTypeEmfPlusDual;
      description: PWCHAR = nil); overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: hdc; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    // Record a metafile to memory.
    constructor Create(referenceHdc: hdc; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create(filename: WideString; referenceHdc: hdc;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create(filename: WideString; referenceHdc: hdc;
      frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create(filename: WideString; referenceHdc: hdc;
      frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create(stream: IStream; referenceHdc: hdc;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create(stream: IStream; referenceHdc: hdc; frameRect: TGPRectF;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create(stream: IStream; referenceHdc: hdc; frameRect: TGPRect;
      frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
      overload;
    constructor Create; reintroduce; overload;
    function GetMetafileHeader(hWmf: HMETAFILE;
      var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      header: TMetafileHeader): TStatus; overload;
    function GetMetafileHeader(hEmf: HENHMETAFILE;
      header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(filename: WideString;
      header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(stream: IStream;
      header: TMetafileHeader): TStatus; overload; // ok
    function GetMetafileHeader(header: TMetafileHeader): TStatus; overload;
    // ok
    // Once this method is called, the Metafile object is in an invalid state
    // and can no longer be used.  It is the responsiblity of the caller to
    // invoke DeleteEnhMetaFile to delete this hEmf.                                              // ok
    function GetHENHMETAFILE: HENHMETAFILE;
    // Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
    // The data must be DWORD aligned if it's an EMF or EMF+.  It must be
    // WORD aligned if it's a WMF.
    function PlayRecord(recordType: TEmfPlusRecordType; flags, dataSize: UINT;
      data: PBYTE): TStatus;
    // If you're using a printer HDC for the metafile, but you want the
    // metafile rasterized at screen resolution, then use this API to set
    // the rasterization dpi of the metafile to the screen resolution,
    // e.g. 96 dpi or 120 dpi.
    function SetDownLevelRasterizationLimit(metafileRasterizationLimitDpi: UINT)
      : TStatus;
    function GetDownLevelRasterizationLimit: UINT;
    function EmfToWmfBits(hEmf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
      iMapMode: Integer = MM_ANISOTROPIC;
      eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

var
  GenericSansSerifFontFamily: TGPFontFamily = nil;
  GenericSerifFontFamily: TGPFontFamily = nil;
  GenericMonospaceFontFamily: TGPFontFamily = nil;

  GenericTypographicStringFormatBuffer: TGPStringFormat = nil;
  GenericDefaultStringFormatBuffer: TGPStringFormat = nil;

  StartupInput: TGDIPlusStartupInput;
  Startupoutput: TGDIPlusStartupoutput;
  stat: status = GdiplusNotInitialized;
  gdiplusBGThreadToken: Cardinal;
  gdiplusToken: ULONG;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses SysUtils, PluginFunc;

const
  MFileName: Pchar = 'GDI_ShareData_refcount';

  // 定义一个记录类型，你所需要共享的数据就保存在这里。
  // 当在进程中调用GetDllData时，进程中也应该定义一个与这个一样的记录类型。
type
  PGlobalDllData = ^TGlobalDllData;

  TGlobalDllData = record
    refcount: Integer;
    gdiplusToken: Cardinal;
  end;

var
  GlobalData: PGlobalDllData = nil; // 这是一个全局变量，指向创建的内存映射文件。
  MapHandle: THANDLE = 0;

function DoInitializeGdi: Boolean;
begin
  Result := FALSE;
  if (stat = GdiplusNotInitialized) then
  begin
    // Initialize StartupInput structure
    StartupInput.DebugEventCallback := nil;
    StartupInput.SuppressExternalCodecs := FALSE;
    StartupInput.GdiplusVersion := 1;
    StartupInput.SuppressBackgroundThread := FALSE;
    stat := GdiplusStartup(gdiplusToken, @StartupInput, @Startupoutput);
    Result := stat = Ok;
  end;
end;

function DoFinalizeGdi(gToken: Cardinal): Boolean;
begin
  Result := FALSE;
  //如果是dll，不释放gdi，让系统最终自己释放。
  if (stat = Ok) and (not ModuleIsLib) then
  begin
    GdiplusShutdown(gToken);

    stat := GdiplusNotInitialized;
    Result := TRUE;
  end;
end;

procedure InitializeGdi;
//var
//  size: Integer;
//  alreadyLoaded: Boolean;
begin
  DoInitializeGdi;
  stat := Ok;

//  size := sizeof(TGlobalDllData);
//  // 创建一个内存文件映射对象，MfileName保存的值就是该对象的名字。
//  MapHandle := CreateFileMapping(Dword(-1), nil, page_readWrite, 0, size,
//    MFileName);
//
//  if MapHandle = 0 then
//  begin
//    Exit;
//  end;
//
//  alreadyLoaded := GetLastError = ERROR_ALREADY_EXISTS;
//
//  // 把文件的视图映射到调用进程的地址空间，该函数的返回值就是该对象的首地址。注//意，这是调用进程的地址，两个应用程序调用该DLL，返回值是不一样的。
//  GlobalData := MapViewOfFile(MapHandle, File_map_all_Access, 0, 0, size);
//  if GlobalData = nil then
//  begin
//    CloseHandle(MapHandle);
//    MapHandle := 0;
//    DoInitializeGdi;
//    Exit;
//  end;
//
//  if (alreadyLoaded) then
//  begin
//    GlobalData^.refcount := GlobalData^.refcount + 1;
//    stat := Ok;
//  end
//  else
//  begin
//    GlobalData^.refcount := 1;
//    if (DoInitializeGdi) then
//    begin
//      GlobalData^.gdiplusToken := gdiplusToken;
//    end;
//  end;
end;

procedure FinalizeGdi;
begin
  if assigned(GenericSansSerifFontFamily) then
    FreeAndNil(GenericSansSerifFontFamily);

  if assigned(GenericSerifFontFamily) then
    FreeAndNil(GenericSerifFontFamily);

  if assigned(GenericMonospaceFontFamily) then
    FreeAndNil(GenericMonospaceFontFamily);

  if assigned(GenericTypographicStringFormatBuffer) then
    FreeAndNil(GenericTypographicStringFormatBuffer);

  if assigned(GenericDefaultStringFormatBuffer) then
    FreeAndNil(GenericDefaultStringFormatBuffer);

//  if MapHandle = 0 then
//  begin
    DoFinalizeGdi(gdiplusToken);
//    Exit;
//  end;
//
//  if GlobalData = nil then
//  begin
//    DoFinalizeGdi(gdiplusToken);
//    Exit;
//  end;
//
//  GlobalData^.refcount := GlobalData^.refcount - 1;
//
//  if (GlobalData^.refcount = 0) then
//  begin
//    DoFinalizeGdi(GlobalData^.gdiplusToken);
//  end;
//
//  UnmapViewOfFile(GlobalData);
end;

(* *************************************************************************\
  *
  * Image Attributes
  *
  * Abstract:
  *
  *   GDI+ Image Attributes used with Graphics.DrawImage
  *
  * There are 5 possible sets of color adjustments:
  *          ColorAdjustDefault,
  *          ColorAdjustBitmap,
  *          ColorAdjustBrush,
  *          ColorAdjustPen,
  *          ColorAdjustText,
  *
  * Bitmaps, Brushes, Pens, and Text will all use any color adjustments
  * that have been set into the default ImageAttributes until their own
  * color adjustments have been set.  So as soon as any "Set" method is
  * called for Bitmaps, Brushes, Pens, or Text, then they start from
  * scratch with only the color adjustments that have been set for them.
  * Calling Reset removes any individual color adjustments for a type
  * and makes it revert back to using all the default color adjustments
  * (if any).  The SetToIdentity method is a way to force a type to
  * have no color adjustments at all, regardless of what previous adjustments
  * have been set for the defaults or for that type.
  *
  \********************************************************************F***** *)

constructor TGPImageAttributes.Create;
begin
  nativeImageAttr := nil;
  lastResult := GdipCreateImageAttributes(nativeImageAttr);
end;

destructor TGPImageAttributes.Destroy;
begin
  GdipDisposeImageAttributes(nativeImageAttr);
  inherited Destroy;
end;

function TGPImageAttributes.Clone: TGPImageAttributes;
var
  Clone: GpImageAttributes;
begin
  SetStatus(GdipCloneImageAttributes(nativeImageAttr, Clone));
  Result := TGPImageAttributes.Create(Clone, lastResult);
end;

function TGPImageAttributes.SetToIdentity
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesToIdentity(nativeImageAttr, type_));
end;

function TGPImageAttributes.Reset(type_: TColorAdjustType =
    ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipResetImageAttributes(nativeImageAttr, type_));
end;

function TGPImageAttributes.SetColorMatrix(const colorMatrix: TColorMatrix;
  mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
      TRUE, @colorMatrix, nil, mode));
end;

function TGPImageAttributes.ClearColorMatrix
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
      FALSE, nil, nil, ColorMatrixFlagsDefault));
end;

function TGPImageAttributes.SetColorMatrices(const colorMatrix: TColorMatrix;
  const grayMatrix: TColorMatrix;
  mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
      TRUE, @colorMatrix, @grayMatrix, mode));
end;

function TGPImageAttributes.ClearColorMatrices
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(nativeImageAttr, type_,
      FALSE, nil, nil, ColorMatrixFlagsDefault));
end;

function TGPImageAttributes.SetThreshold(threshold: Single;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(nativeImageAttr, type_,
      TRUE, threshold));
end;

function TGPImageAttributes.ClearThreshold
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(nativeImageAttr, type_,
      FALSE, 0.0));
end;

function TGPImageAttributes.SetGamma(gamma: Single;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(nativeImageAttr, type_, TRUE,
      gamma));
end;

function TGPImageAttributes.ClearGamma
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(nativeImageAttr, type_,
      FALSE, 0.0));
end;

function TGPImageAttributes.SetNoOp(type_: TColorAdjustType =
    ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(nativeImageAttr, type_, TRUE));
end;

function TGPImageAttributes.ClearNoOp(type_: TColorAdjustType =
    ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(nativeImageAttr, type_,
      FALSE));
end;

function TGPImageAttributes.SetColorKey(colorLow, colorHigh: TGPColor;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(nativeImageAttr, type_,
      TRUE, colorLow, colorHigh));
end;

function TGPImageAttributes.ClearColorKey
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(nativeImageAttr, type_,
      FALSE, 0, 0));
end;

function TGPImageAttributes.SetOutputChannel(channelFlags: TColorChannelFlags;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(nativeImageAttr,
      type_, TRUE, channelFlags));
end;

function TGPImageAttributes.ClearOutputChannel
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(nativeImageAttr,
      type_, FALSE, ColorChannelFlagsLast));
end;

function TGPImageAttributes.SetOutputChannelColorProfile
  (colorProfileFilename: WideString;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile
      (nativeImageAttr, type_, TRUE, PWideChar(colorProfileFilename)));
end;

function TGPImageAttributes.ClearOutputChannelColorProfile
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile
      (nativeImageAttr, type_, FALSE, nil));
end;

function TGPImageAttributes.SetRemapTable(mapSize: Cardinal; map: PColorMap;
  type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(nativeImageAttr, type_,
      TRUE, mapSize, map));
end;

function TGPImageAttributes.ClearRemapTable
  (type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(nativeImageAttr, type_,
      FALSE, 0, nil));
end;

function TGPImageAttributes.SetBrushRemapTable(mapSize: Cardinal;
  map: PColorMap): TStatus;
begin
  Result := SetRemapTable(mapSize, map, ColorAdjustTypeBrush);
end;

function TGPImageAttributes.ClearBrushRemapTable: TStatus;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TGPImageAttributes.SetWrapMode(wrap: TWrapMode;
  color: TGPColor = aclBlack; clamp: BOOL = FALSE): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesWrapMode(nativeImageAttr, wrap,
      color, clamp));
end;

// The flags of the palette are ignored.

function TGPImageAttributes.GetAdjustedPalette(colorPalette: PColorPalette;
  colorAdjustType: TColorAdjustType): TStatus;
begin
  Result := SetStatus(GdipGetImageAttributesAdjustedPalette(nativeImageAttr,
      colorPalette, colorAdjustType));
end;

function TGPImageAttributes.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPImageAttributes.Create(imageAttr: GpImageAttributes;
  status: TStatus);
begin
  SetNativeImageAttr(imageAttr);
  lastResult := status;
end;

procedure TGPImageAttributes.SetNativeImageAttr
  (nativeImageAttr: GpImageAttributes);
begin
  self.nativeImageAttr := nativeImageAttr;
end;

function TGPImageAttributes.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

(* *************************************************************************\
  *
  *   GDI+ Matrix class
  *
  \************************************************************************* *)

// Default constructor is set to identity matrix.
constructor TGPMatrix.Create;
var
  matrix: GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix(matrix);
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(m11, m12, m21, m22, dx, dy: Single);
var
  matrix: GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix2(m11, m12, m21, m22, dx, dy, matrix);
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(const rect: TGPRectF; const dstplg: TGPPointF);
var
  matrix: GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix3(@rect, @dstplg, matrix);
  SetNativeMatrix(matrix);
end;

constructor TGPMatrix.Create(const rect: TGPRect; const dstplg: TGPPoint);
var
  matrix: GpMatrix;
begin
  matrix := nil;
  lastResult := GdipCreateMatrix3I(@rect, @dstplg, matrix);
  SetNativeMatrix(matrix);
end;

destructor TGPMatrix.Destroy;
begin
  GdipDeleteMatrix(nativeMatrix);
end;

function TGPMatrix.Clone: TGPMatrix;
var
  cloneMatrix: GpMatrix;
begin
  cloneMatrix := nil;
  SetStatus(GdipCloneMatrix(nativeMatrix, cloneMatrix));
  if (lastResult <> Ok) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TGPMatrix.Create(cloneMatrix);
end;

function TGPMatrix.GetElements(const m: TMatrixArray): TStatus;
begin
  Result := SetStatus(GdipGetMatrixElements(nativeMatrix, @m));
end;

function TGPMatrix.SetElements(m11, m12, m21, m22, dx, dy: Single): TStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(nativeMatrix, m11, m12, m21, m22,
      dx, dy));
end;

function TGPMatrix.OffsetX: Single;
var
  elements: TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[4]
  else
    Result := 0.0;
end;

function TGPMatrix.OffsetY: Single;
var
  elements: TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[5]
  else
    Result := 0.0;
end;

function TGPMatrix.Reset: TStatus;
begin
  // set identity matrix elements
  Result := SetStatus(GdipSetMatrixElements(nativeMatrix, 1.0, 0.0, 0.0, 1.0,
      0.0, 0.0));
end;

function TGPMatrix.Multiply(matrix: TGPMatrix;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyMatrix(nativeMatrix, matrix.nativeMatrix,
      order));
end;

function TGPMatrix.Translate(OffsetX, OffsetY: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateMatrix(nativeMatrix, OffsetX, OffsetY,
      order));
end;

function TGPMatrix.Scale(scaleX, scaleY: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleMatrix(nativeMatrix, scaleX, scaleY, order));
end;

function TGPMatrix.Rotate(angle: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
end;

function TGPMatrix.RotateAt(angle: Single; const center: TGPPointF;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  if (order = MatrixOrderPrepend) then
  begin
    SetStatus(GdipTranslateMatrix(nativeMatrix, center.x, center.y, order));
    SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
    Result := SetStatus(GdipTranslateMatrix(nativeMatrix, -center.x, -center.y,
        order));
  end
  else
  begin
    SetStatus(GdipTranslateMatrix(nativeMatrix, -center.x, -center.y, order));
    SetStatus(GdipRotateMatrix(nativeMatrix, angle, order));
    Result := SetStatus(GdipTranslateMatrix(nativeMatrix, center.x, center.y,
        order));
  end;
end;

function TGPMatrix.Shear(shearX, shearY: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipShearMatrix(nativeMatrix, shearX, shearY, order));
end;

function TGPMatrix.Invert: TStatus;
begin
  Result := SetStatus(GdipInvertMatrix(nativeMatrix));
end;

// float version
function TGPMatrix.TransformPoints(pts: PGPPointF; count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPoints(nativeMatrix, pts, count));
end;

function TGPMatrix.TransformPoints(pts: PGPPoint; count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPointsI(nativeMatrix, pts, count));
end;

function TGPMatrix.TransformVectors(pts: PGPPointF;
  count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPoints(nativeMatrix, pts,
      count));
end;

function TGPMatrix.TransformVectors(pts: PGPPoint; count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPointsI(nativeMatrix, pts,
      count));
end;

function TGPMatrix.IsInvertible: BOOL;
begin
  Result := FALSE;
  SetStatus(GdipIsMatrixInvertible(nativeMatrix, Result));
end;

function TGPMatrix.IsIdentity: BOOL;
begin
  Result := FALSE;
  SetStatus(GdipIsMatrixIdentity(nativeMatrix, Result));
end;

function TGPMatrix.Equals(matrix: TGPMatrix): BOOL;
begin
  Result := FALSE;
  SetStatus(GdipIsMatrixEqual(nativeMatrix, matrix.nativeMatrix, Result));
end;

function TGPMatrix.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPMatrix.Create(nativeMatrix: GpMatrix);
begin
  lastResult := Ok;
  SetNativeMatrix(nativeMatrix);
end;

procedure TGPMatrix.SetNativeMatrix(nativeMatrix: GpMatrix);
begin
  self.nativeMatrix := nativeMatrix;
end;

function TGPMatrix.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

(* *************************************************************************\
  *
  *   GDI+ StringFormat class
  *
  \************************************************************************* *)

constructor TGPStringFormat.Create(formatFlags: Integer = 0;
  language: LANGID = LANG_NEUTRAL);
begin
  nativeFormat := nil;
  lastError := GdipCreateStringFormat(formatFlags, language, nativeFormat);
end;

class function TGPStringFormat.GenericDefault: TGPStringFormat;
begin
  if not assigned(GenericDefaultStringFormatBuffer) then
  begin
    GenericDefaultStringFormatBuffer := TGPStringFormat.Create;
    GenericDefaultStringFormatBuffer.lastError :=
      GdipStringFormatGetGenericDefault
      (GenericDefaultStringFormatBuffer.nativeFormat);
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TGPStringFormat.GenericTypographic: TGPStringFormat;
begin
  if not assigned(GenericTypographicStringFormatBuffer) then
  begin
    GenericTypographicStringFormatBuffer := TGPStringFormat.Create;
    GenericTypographicStringFormatBuffer.lastError :=
      GdipStringFormatGetGenericTypographic
      (GenericTypographicStringFormatBuffer.
        nativeFormat);
  end;
  Result := GenericTypographicStringFormatBuffer;
end;

constructor TGPStringFormat.Create(format: TGPStringFormat);
var
  gpstf: GpStringFormat;
begin
  nativeFormat := nil;
  if assigned(format) then
    gpstf := format.nativeFormat
  else
    gpstf := nil;
  lastError := GdipCloneStringFormat(gpstf, nativeFormat);
end;

function TGPStringFormat.Clone: TGPStringFormat;
var
  clonedStringFormat: GpStringFormat;
begin
  clonedStringFormat := nil;
  lastError := GdipCloneStringFormat(nativeFormat, clonedStringFormat);
  if (lastError = Ok) then
    Result := TGPStringFormat.Create(clonedStringFormat, lastError)
  else
    Result := nil;
end;

destructor TGPStringFormat.Destroy;
begin
  GdipDeleteStringFormat(nativeFormat);
end;

function TGPStringFormat.SetFormatFlags(flags: Integer): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatFlags(nativeFormat, flags));
end;

function TGPStringFormat.GetFormatFlags: Integer;
begin
  SetStatus(GdipGetStringFormatFlags(nativeFormat, Result));
end;

function TGPStringFormat.SetAlignment(align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatAlign(nativeFormat, align));
end;

function TGPStringFormat.GetAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatAlign(nativeFormat, Result));
end;

function TGPStringFormat.SetLineAlignment(align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatLineAlign(nativeFormat, align));
end;

function TGPStringFormat.GetLineAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatLineAlign(nativeFormat, Result));
end;

function TGPStringFormat.SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatHotkeyPrefix(nativeFormat,
      Integer(hotkeyPrefix)));
end;

function TGPStringFormat.GetHotkeyPrefix: THotkeyPrefix;
var
  hotkeyPrefix: Integer;
begin
  SetStatus(GdipGetStringFormatHotkeyPrefix(nativeFormat, hotkeyPrefix));
  Result := THotkeyPrefix(hotkeyPrefix);
end;

function TGPStringFormat.SetTabStops(firstTabOffset: Single; count: Integer;
  tabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTabStops(nativeFormat, firstTabOffset,
      count, tabStops));
end;

function TGPStringFormat.GetTabStopCount: Integer;
begin
  SetStatus(GdipGetStringFormatTabStopCount(nativeFormat, Result));
end;

function TGPStringFormat.GetTabStops(count: Integer;
  firstTabOffset, tabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipGetStringFormatTabStops(nativeFormat, count,
      firstTabOffset, tabStops));
end;

function TGPStringFormat.SetDigitSubstitution(language: LANGID;
  substitute: TStringDigitSubstitute): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatDigitSubstitution(nativeFormat,
      language, substitute));
end;

function TGPStringFormat.GetDigitSubstitutionLanguage: LANGID;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(nativeFormat, @Result, nil));
end;

function TGPStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(nativeFormat, nil, @Result));
end;

function TGPStringFormat.SetTrimming(trimming: TStringTrimming): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTrimming(nativeFormat, trimming));
end;

function TGPStringFormat.GetTrimming: TStringTrimming;
begin
  SetStatus(GdipGetStringFormatTrimming(nativeFormat, Result));
end;

function TGPStringFormat.SetMeasurableCharacterRanges(rangeCount: Integer;
  ranges: PCharacterRange): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges
      (nativeFormat, rangeCount, ranges));
end;

function TGPStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(nativeFormat,
      Result));
end;

function TGPStringFormat.GetLastStatus: TStatus;
begin
  Result := lastError;
  lastError := Ok;
end;

function TGPStringFormat.SetStatus(newStatus: GpStatus): TStatus;
begin
  if (newStatus <> Ok) then
    lastError := newStatus;
  Result := newStatus;
end;

// operator =
procedure TGPStringFormat.Assign(source: TGPStringFormat);
begin
  assert(assigned(source));
  GdipDeleteStringFormat(nativeFormat);
  lastError := GdipCloneStringFormat(source.nativeFormat, nativeFormat);
end;

constructor TGPStringFormat.Create(clonedStringFormat: GpStringFormat;
  status: TStatus);
begin
  lastError := status;
  nativeFormat := clonedStringFormat;
end;



// ---------------------------------------------------------------------------
// TAdjustableArrowCap
// ---------------------------------------------------------------------------

constructor TGPAdjustableArrowCap.Create(height, width: Single;
  isFilled: BOOL = TRUE);
var
  cap: GpAdjustableArrowCap;
begin
  cap := nil;
  lastResult := GdipCreateAdjustableArrowCap(height, width, isFilled, cap);
  SetNativeCap(cap);
end;

function TGPAdjustableArrowCap.SetHeight(height: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapHeight
      (GpAdjustableArrowCap(nativeCap), height));
end;

function TGPAdjustableArrowCap.GetHeight: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap(nativeCap),
      Result));
end;

function TGPAdjustableArrowCap.SetWidth(width: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapWidth
      (GpAdjustableArrowCap(nativeCap), width));
end;

function TGPAdjustableArrowCap.GetWidth: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap(nativeCap),
      Result));
end;

function TGPAdjustableArrowCap.SetMiddleInset(middleInset: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapMiddleInset
      (GpAdjustableArrowCap(nativeCap), middleInset));
end;

function TGPAdjustableArrowCap.GetMiddleInset: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapMiddleInset
      (GpAdjustableArrowCap(nativeCap), Result));
end;

function TGPAdjustableArrowCap.SetFillState(isFilled: BOOL): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapFillState
      (GpAdjustableArrowCap(nativeCap), isFilled));
end;

function TGPAdjustableArrowCap.isFilled: BOOL;
begin
  SetStatus(GdipGetAdjustableArrowCapFillState(GpAdjustableArrowCap(nativeCap),
      Result));
end;

(* *************************************************************************\
  *
  *   GDI+ Metafile class
  *
  \************************************************************************* *)

// Playback a metafile from a HMETAFILE
// If deleteWmf is TRUE, then when the metafile is deleted,
// the hWmf will also be deleted.  Otherwise, it won't be.

constructor TGPMetafile.Create(hWmf: HMETAFILE;
  var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
  deleteWmf: BOOL = FALSE);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromWmf(hWmf, deleteWmf,
    @wmfPlaceableFileHeader, metafile);
  SetNativeImage(metafile);
end;

// Playback a metafile from a HENHMETAFILE
// If deleteEmf is TRUE, then when the metafile is deleted,
// the hEmf will also be deleted.  Otherwise, it won't be.

constructor TGPMetafile.Create(hEmf: HENHMETAFILE; deleteEmf: BOOL = FALSE);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromEmf(hEmf, deleteEmf, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromFile(PWideChar(filename), metafile);
  SetNativeImage(metafile);
end;

// Playback a WMF metafile from a file.

constructor TGPMetafile.Create(filename: WideString;
  var wmfPlaceableFileHeader: TWmfPlaceableFileHeader);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromWmfFile(PWideChar(filename),
    @wmfPlaceableFileHeader, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipCreateMetafileFromStream(stream, metafile);
  SetNativeImage(metafile);
end;

// Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: hdc;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafile(referenceHdc, type_, nil,
    MetafileFrameUnitGdi, description, metafile);
  SetNativeImage(metafile);
end;

// Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: hdc; frameRect: TGPRectF;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafile(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile);
  SetNativeImage(metafile);
end;

// Record a metafile to memory.

constructor TGPMetafile.Create(referenceHdc: hdc; frameRect: TGPRect;
  frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileI(referenceHdc, type_, @frameRect, frameUnit,
    description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString; referenceHdc: hdc;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileFileName(PWideChar(filename), referenceHdc,
    type_, nil, MetafileFrameUnitGdi, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString; referenceHdc: hdc;
  frameRect: TGPRectF; frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileFileName(PWideChar(filename), referenceHdc,
    type_, @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(filename: WideString; referenceHdc: hdc;
  frameRect: TGPRect; frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileFileNameI(PWideChar(filename), referenceHdc,
    type_, @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream; referenceHdc: hdc;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileStream(stream, referenceHdc, type_, nil,
    MetafileFrameUnitGdi, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream; referenceHdc: hdc;
  frameRect: TGPRectF; frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileStream(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

constructor TGPMetafile.Create(stream: IStream; referenceHdc: hdc;
  frameRect: TGPRect; frameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil);
var
  metafile: GpMetafile;
begin
  metafile := nil;
  lastResult := GdipRecordMetafileStreamI(stream, referenceHdc, type_,
    @frameRect, frameUnit, description, metafile);
  SetNativeImage(metafile);
end;

function TGPMetafile.GetMetafileHeader(hWmf: HMETAFILE;
  var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
  header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromWmf(hWmf, @wmfPlaceableFileHeader,
    @header.type_);
end;

function TGPMetafile.GetMetafileHeader(hEmf: HENHMETAFILE;
  header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromEmf(hEmf, @header.type_);
end;

function TGPMetafile.GetMetafileHeader(filename: WideString;
  header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromFile(PWideChar(filename), @header.type_);
end;

function TGPMetafile.GetMetafileHeader(stream: IStream;
  header: TMetafileHeader): TStatus;
begin
  Result := GdipGetMetafileHeaderFromStream(stream, @header.type_);
end;

function TGPMetafile.GetMetafileHeader(header: TMetafileHeader): TStatus;
begin
  Result := SetStatus(GdipGetMetafileHeaderFromMetafile
      (GpMetafile(nativeImage), @header.type_));
end;

// Once this method is called, the Metafile object is in an invalid state
// and can no longer be used.  It is the responsiblity of the caller to
// invoke DeleteEnhMetaFile to delete this hEmf.

function TGPMetafile.GetHENHMETAFILE: HENHMETAFILE;
begin
  SetStatus(GdipGetHemfFromMetafile(GpMetafile(nativeImage), Result));
end;

// Used in conjuction with Graphics::EnumerateMetafile to play an EMF+
// The data must be DWORD aligned if it's an EMF or EMF+.  It must be
// WORD aligned if it's a WMF.

function TGPMetafile.PlayRecord(recordType: TEmfPlusRecordType;
  flags, dataSize: UINT; data: PBYTE): TStatus;
begin
  Result := SetStatus(GdipPlayMetafileRecord(GpMetafile(nativeImage),
      recordType, flags, dataSize, data));
end;

// If you're using a printer HDC for the metafile, but you want the
// metafile rasterized at screen resolution, then use this API to set
// the rasterization dpi of the metafile to the screen resolution,
// e.g. 96 dpi or 120 dpi.

function TGPMetafile.SetDownLevelRasterizationLimit
  (metafileRasterizationLimitDpi: UINT): TStatus;
begin
  Result := SetStatus(GdipSetMetafileDownLevelRasterizationLimit
      (GpMetafile(nativeImage), metafileRasterizationLimitDpi));
end;

function TGPMetafile.GetDownLevelRasterizationLimit: UINT;
var
  metafileRasterizationLimitDpi: UINT;
begin
  metafileRasterizationLimitDpi := 0;
  SetStatus(GdipGetMetafileDownLevelRasterizationLimit(GpMetafile(nativeImage),
      metafileRasterizationLimitDpi));
  Result := metafileRasterizationLimitDpi;
end;

function TGPMetafile.EmfToWmfBits(hEmf: HENHMETAFILE; cbData16: UINT;
  pData16: PBYTE; iMapMode: Integer = MM_ANISOTROPIC;
  eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
begin
  Result := GdipEmfToWmfBits(hEmf, cbData16, pData16, iMapMode,
    Integer(eFlags));
end;

constructor TGPMetafile.Create;
begin
  SetNativeImage(nil);
  lastResult := Ok;
end;

(* *************************************************************************\
  *
  *   GDI+ Codec Image APIs
  *
  \************************************************************************* *)

// --------------------------------------------------------------------------
// Codec Management APIs
// --------------------------------------------------------------------------

function GetImageDecodersSize(out numDecoders, size: UINT): TStatus;
begin
  Result := GdipGetImageDecodersSize(numDecoders, size);
end;

function GetImageDecoders(numDecoders, size: UINT;
  decoders: PImageCodecInfo): TStatus;
begin
  Result := GdipGetImageDecoders(numDecoders, size, decoders);
end;

function GetImageEncodersSize(out numEncoders, size: UINT): TStatus;
begin
  Result := GdipGetImageEncodersSize(numEncoders, size);
end;

function GetImageEncoders(numEncoders, size: UINT;
  encoders: PImageCodecInfo): TStatus;
begin
  Result := GdipGetImageEncoders(numEncoders, size, encoders);
end;

(* *************************************************************************\
  *
  *   GDI+ Region class implementation
  *
  \************************************************************************* *)

constructor TGPRegion.Create;
var
  region: GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegion(region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(rect: TGPRectF);
var
  region: GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionRect(@rect, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(rect: TGPRect);
var
  region: GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionRectI(@rect, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(path: TGPGraphicsPath);
var
  region: GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionPath(path.nativePath, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(regionData: PBYTE; size: Integer);
var
  region: GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionRgnData(regionData, size, region);
  SetNativeRegion(region);
end;

constructor TGPRegion.Create(hRgn: hRgn);
var
  region: GpRegion;
begin
  region := nil;
  lastResult := GdipCreateRegionHrgn(hRgn, region);
  SetNativeRegion(region);
end;

function TGPRegion.FromHRGN(hRgn: hRgn): TGPRegion;
var
  region: GpRegion;
begin
  region := nil;
  if (GdipCreateRegionHrgn(hRgn, region) = Ok) then
  begin
    Result := TGPRegion.Create(region);
    if (Result = nil) then
      GdipDeleteRegion(region);
    Exit;
  end
  else
    Result := nil;
end;

destructor TGPRegion.Destroy;
begin
  GdipDeleteRegion(nativeRegion);
end;

function TGPRegion.Clone: TGPRegion;
var
  region: GpRegion;
begin
  region := nil;
  SetStatus(GdipCloneRegion(nativeRegion, region));
  Result := TGPRegion.Create(region);
end;

function TGPRegion.MakeInfinite: TStatus;
begin
  Result := SetStatus(GdipSetInfinite(nativeRegion));
end;

function TGPRegion.MakeEmpty: TStatus;
begin
  Result := SetStatus(GdipSetEmpty(nativeRegion));
end;

// Get the size of the buffer needed for the GetData method
function TGPRegion.GetDataSize: UINT;
var
  bufferSize: UINT;
begin
  bufferSize := 0;
  SetStatus(GdipGetRegionDataSize(nativeRegion, bufferSize));
  Result := bufferSize;
end;


// buffer     - where to put the data
// bufferSize - how big the buffer is (should be at least as big as GetDataSize())
// sizeFilled - if not nil, this is an OUT param that says how many bytes
// of data were written to the buffer.

function TGPRegion.GetData(buffer: PBYTE; bufferSize: UINT;
  sizeFilled: PUINT = nil): TStatus;
begin
  Result := SetStatus(GdipGetRegionData(nativeRegion, buffer, bufferSize,
      sizeFilled));
end;

function TGPRegion.Intersect(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect,
      CombineModeIntersect));
end;

function TGPRegion.Intersect(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect,
      CombineModeIntersect));
end;

function TGPRegion.Intersect(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
      CombineModeIntersect));
end;

function TGPRegion.Intersect(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
      region.nativeRegion, CombineModeIntersect));
end;

function TGPRegion.Union(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect,
      CombineModeUnion));
end;

function TGPRegion.Union(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect,
      CombineModeUnion));
end;

function TGPRegion.Union(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
      CombineModeUnion));
end;

function TGPRegion.Union(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
      region.nativeRegion, CombineModeUnion));
end;

function TGPRegion.Xor_(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect,
      CombineModeXor));
end;

function TGPRegion.Xor_(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect,
      CombineModeXor));
end;

function TGPRegion.Xor_(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
      CombineModeXor));
end;

function TGPRegion.Xor_(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
      region.nativeRegion, CombineModeXor));
end;

function TGPRegion.Exclude(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect,
      CombineModeExclude));
end;

function TGPRegion.Exclude(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect,
      CombineModeExclude));
end;

function TGPRegion.Exclude(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
      CombineModeExclude));
end;

function TGPRegion.Exclude(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
      region.nativeRegion, CombineModeExclude));
end;

function TGPRegion.Complement(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(nativeRegion, @rect,
      CombineModeComplement));
end;

function TGPRegion.Complement(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(nativeRegion, @rect,
      CombineModeComplement));
end;

function TGPRegion.Complement(path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(nativeRegion, path.nativePath,
      CombineModeComplement));
end;

function TGPRegion.Complement(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(nativeRegion,
      region.nativeRegion, CombineModeComplement));
end;

function TGPRegion.Translate(dx, dy: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateRegion(nativeRegion, dx, dy));
end;

function TGPRegion.Translate(dx, dy: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateRegionI(nativeRegion, dx, dy));
end;

function TGPRegion.Transform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipTransformRegion(nativeRegion, matrix.nativeMatrix));
end;

function TGPRegion.GetBounds(out rect: TGPRect; g: TGPGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBoundsI(nativeRegion, g.nativeGraphics,
      @rect));
end;

function TGPRegion.GetBounds(out rect: TGPRectF; g: TGPGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBounds(nativeRegion, g.nativeGraphics,
      @rect));
end;

function TGPRegion.GetHRGN(g: TGPGraphics): hRgn;
begin
  SetStatus(GdipGetRegionHRgn(nativeRegion, g.nativeGraphics, Result));
end;

function TGPRegion.IsEmpty(g: TGPGraphics): BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsEmptyRegion(nativeRegion, g.nativeGraphics, booln));
  Result := booln;
end;

function TGPRegion.IsInfinite(g: TGPGraphics): BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsInfiniteRegion(nativeRegion, g.nativeGraphics, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionPointI(nativeRegion, x, y, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const point: TGPPoint; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionPointI(nativeRegion, point.x, point.y, GPX,
      booln));
  Result := booln;
end;

function TGPRegion.IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionPoint(nativeRegion, x, y, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const point: TGPPointF;
  g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionPoint(nativeRegion, point.x, point.y, GPX,
      booln));
  Result := booln;
end;

function TGPRegion.IsVisible(x, y, width, height: Integer;
  g: TGPGraphics): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionRectI(nativeRegion, x, y, width, height, GPX,
      booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const rect: TGPRect; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionRectI(nativeRegion, rect.x, rect.y, rect.width,
      rect.height, GPX, booln));
  Result := booln;
end;

function TGPRegion.IsVisible(x, y, width, height: Single;
  g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionRect(nativeRegion, x, y, width, height, GPX,
      booln));
  Result := booln;
end;

function TGPRegion.IsVisible(const rect: TGPRectF; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  GPX: GpGraphics;
begin
  booln := FALSE;
  if assigned(g) then
    GPX := g.nativeGraphics
  else
    GPX := nil;
  SetStatus(GdipIsVisibleRegionRect(nativeRegion, rect.x, rect.y, rect.width,
      rect.height, GPX, booln));
  Result := booln;
end;

function TGPRegion.Equals(region: TGPRegion; g: TGPGraphics): BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsEqualRegion(nativeRegion, region.nativeRegion,
      g.nativeGraphics, booln));
  Result := booln;
end;

function TGPRegion.GetRegionScansCount(matrix: TGPMatrix): UINT;
var
  count: UINT;
begin
  count := 0;
  SetStatus(GdipGetRegionScansCount(nativeRegion, count, matrix.nativeMatrix));
  Result := count;
end;

// If rects is nil, result := the count of rects in the region.
// Otherwise, assume rects is big enough to hold all the region rects
// and fill them in and result := the number of rects filled in.
// The rects are result :=ed in the units specified by the matrix
// (which is typically a world-to-device transform).
// Note that the number of rects result :=ed can vary, depending on the
// matrix that is used.

function TGPRegion.GetRegionScans(matrix: TGPMatrix; rects: PGPRectF;
  out count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScans(nativeRegion, rects, count,
      matrix.nativeMatrix));
end;

function TGPRegion.GetRegionScans(matrix: TGPMatrix; rects: PGPRect;
  out count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScansI(nativeRegion, rects, count,
      matrix.nativeMatrix));
end;

function TGPRegion.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

function TGPRegion.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

constructor TGPRegion.Create(nativeRegion: GpRegion);
begin
  SetNativeRegion(nativeRegion);
end;

procedure TGPRegion.SetNativeRegion(nativeRegion: GpRegion);
begin
  self.nativeRegion := nativeRegion;
end;

(* *************************************************************************\
  *
  *   GDI+ CustomLineCap APIs
  *
  \************************************************************************* *)

constructor TGPCustomLineCap.Create(fillPath, strokePath: TGPGraphicsPath;
  baseCap: TLineCap = LineCapFlat; baseInset: Single = 0);
var
  nativeFillPath, nativeStrokePath: GpPath;
begin
  nativeCap := nil;
  nativeFillPath := nil;
  nativeStrokePath := nil;

  if assigned(fillPath) then
    nativeFillPath := fillPath.nativePath;
  if assigned(strokePath) then
    nativeStrokePath := strokePath.nativePath;

  lastResult := GdipCreateCustomLineCap(nativeFillPath, nativeStrokePath,
    baseCap, baseInset, nativeCap);
end;

destructor TGPCustomLineCap.Destroy;
begin
  GdipDeleteCustomLineCap(nativeCap);
end;

function TGPCustomLineCap.Clone: TGPCustomLineCap;
var
  newNativeLineCap: GpCustomLineCap;
begin
  newNativeLineCap := nil;
  SetStatus(GdipCloneCustomLineCap(nativeCap, newNativeLineCap));
  if (lastResult = Ok) then
  begin
    Result := TGPCustomLineCap.Create(newNativeLineCap, lastResult);
    if (Result = nil) then
      SetStatus(GdipDeleteCustomLineCap(newNativeLineCap));
  end
  else
    Result := nil;
end;

// This changes both the start and end cap.
function TGPCustomLineCap.SetStrokeCap(strokeCap: TLineCap): TStatus;
begin
  Result := SetStrokeCaps(strokeCap, strokeCap);
end;

function TGPCustomLineCap.SetStrokeCaps(startCap, endCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeCaps(nativeCap, startCap,
      endCap));
end;

function TGPCustomLineCap.GetStrokeCaps(out startCap, endCap: TLineCap)
  : TStatus;
begin
  Result := SetStatus(GdipGetCustomLineCapStrokeCaps(nativeCap, startCap,
      endCap));
end;

function TGPCustomLineCap.SetStrokeJoin(lineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeJoin(nativeCap, lineJoin));
end;

function TGPCustomLineCap.GetStrokeJoin: TLineJoin;
begin
  SetStatus(GdipGetCustomLineCapStrokeJoin(nativeCap, Result));
end;

function TGPCustomLineCap.SetBaseCap(baseCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseCap(nativeCap, baseCap));
end;

function TGPCustomLineCap.GetBaseCap: TLineCap;
begin
  SetStatus(GdipGetCustomLineCapBaseCap(nativeCap, Result));
end;

function TGPCustomLineCap.SetBaseInset(inset: Single): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseInset(nativeCap, inset));
end;

function TGPCustomLineCap.GetBaseInset: Single;
begin
  SetStatus(GdipGetCustomLineCapBaseInset(nativeCap, Result));
end;

function TGPCustomLineCap.SetWidthScale(widthScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapWidthScale(nativeCap, widthScale));
end;

function TGPCustomLineCap.GetWidthScale: Single;
begin
  SetStatus(GdipGetCustomLineCapWidthScale(nativeCap, Result));
end;

function TGPCustomLineCap.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPCustomLineCap.Create;
begin
  nativeCap := nil;
  lastResult := Ok;
end;

constructor TGPCustomLineCap.Create(nativeCap: GpCustomLineCap;
  status: TStatus);
begin
  lastResult := status;
  SetNativeCap(nativeCap);
end;

procedure TGPCustomLineCap.SetNativeCap(nativeCap: GpCustomLineCap);
begin
  self.nativeCap := nativeCap;
end;

function TGPCustomLineCap.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

(* *************************************************************************
  *
  * CachedBitmap class definition
  *
  *   GDI+ CachedBitmap is a representation of an accelerated drawing
  *   that has restrictions on what operations are allowed in order
  *   to accelerate the drawing to the destination.
  *
  ************************************************************************* *)

constructor TGPCachedBitmap.Create(bitmap: TGPBitmap; graphics: TGPGraphics);
begin
  nativeCachedBitmap := nil;
  lastResult := GdipCreateCachedBitmap(GpBitmap(bitmap.nativeImage),
    graphics.nativeGraphics, nativeCachedBitmap);
end;

destructor TGPCachedBitmap.Destroy;
begin
  GdipDeleteCachedBitmap(nativeCachedBitmap);
end;

function TGPCachedBitmap.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

(* *************************************************************************\
  *
  *   GDI+ Pen class
  *
  \************************************************************************* *)

// --------------------------------------------------------------------------
// Pen class
// --------------------------------------------------------------------------

constructor TGPPen.Create(color: TGPColor; width: Single = 1.0);
var
  unit_: TUnit;
begin
  unit_ := UnitWorld;
  nativePen := nil;
  lastResult := GdipCreatePen1(color, width, unit_, nativePen);
end;

constructor TGPPen.Create(brush: TGPBrush; width: Single = 1.0);
var
  unit_: TUnit;
begin
  unit_ := UnitWorld;
  nativePen := nil;
  lastResult := GdipCreatePen2(brush.nativeBrush, width, unit_, nativePen);
end;

destructor TGPPen.Destroy;
begin
  GdipDeletePen(nativePen);
end;

function TGPPen.Clone: TGPPen;
var
  clonePen: GpPen;
begin
  clonePen := nil;
  lastResult := GdipClonePen(nativePen, clonePen);
  Result := TGPPen.Create(clonePen, lastResult);
end;

function TGPPen.SetWidth(width: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenWidth(nativePen, width));
end;

function TGPPen.GetWidth: Single;
begin
  SetStatus(GdipGetPenWidth(nativePen, Result));
end;

// Set/get line caps: start, end, and dash
// Line cap and join APIs by using LineCap and LineJoin enums.

function TGPPen.SetLineCap(startCap, endCap: TLineCap;
  dashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenLineCap197819(nativePen, startCap, endCap,
      dashCap));
end;

function TGPPen.SetStartCap(startCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenStartCap(nativePen, startCap));
end;

function TGPPen.SetEndCap(endCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenEndCap(nativePen, endCap));
end;

function TGPPen.SetDashCap(dashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenDashCap197819(nativePen, dashCap));
end;

function TGPPen.GetStartCap: TLineCap;
begin
  SetStatus(GdipGetPenStartCap(nativePen, Result));
end;

function TGPPen.GetEndCap: TLineCap;
begin
  SetStatus(GdipGetPenEndCap(nativePen, Result));
end;

function TGPPen.GetDashCap: TDashCap;
begin
  SetStatus(GdipGetPenDashCap197819(nativePen, Result));
end;

function TGPPen.SetLineJoin(lineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetPenLineJoin(nativePen, lineJoin));
end;

function TGPPen.GetLineJoin: TLineJoin;
begin
  SetStatus(GdipGetPenLineJoin(nativePen, Result));
end;

function TGPPen.SetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
var
  nativeCap: GpCustomLineCap;
begin
  nativeCap := nil;
  if assigned(customCap) then
    nativeCap := customCap.nativeCap;
  Result := SetStatus(GdipSetPenCustomStartCap(nativePen, nativeCap));
end;

function TGPPen.GetCustomStartCap(customCap: TGPCustomLineCap): TStatus;
begin
  if (customCap = nil) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCustomStartCap(nativePen,
        customCap.nativeCap));
end;

function TGPPen.SetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
var
  nativeCap: GpCustomLineCap;
begin
  nativeCap := nil;
  if assigned(customCap) then
    nativeCap := customCap.nativeCap;
  Result := SetStatus(GdipSetPenCustomEndCap(nativePen, nativeCap));
end;

function TGPPen.GetCustomEndCap(customCap: TGPCustomLineCap): TStatus;
begin
  if (customCap = nil) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCustomEndCap(nativePen, customCap.nativeCap));
end;

function TGPPen.SetMiterLimit(miterLimit: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenMiterLimit(nativePen, miterLimit));
end;

function TGPPen.GetMiterLimit: Single;
begin
  SetStatus(GdipGetPenMiterLimit(nativePen, Result));
end;

function TGPPen.SetAlignment(penAlignment: TPenAlignment): TStatus;
begin
  Result := SetStatus(GdipSetPenMode(nativePen, penAlignment));
end;

function TGPPen.GetAlignment: TPenAlignment;
begin
  SetStatus(GdipGetPenMode(nativePen, Result));
end;

function TGPPen.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPenTransform(nativePen, matrix.nativeMatrix));
end;

function TGPPen.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPenTransform(nativePen, matrix.nativeMatrix));
end;

function TGPPen.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetPenTransform(nativePen));
end;

function TGPPen.MultiplyTransform(matrix: TGPMatrix;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPenTransform(nativePen, matrix.nativeMatrix,
      order));
end;

function TGPPen.TranslateTransform(dx, dy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePenTransform(nativePen, dx, dy, order));
end;

function TGPPen.ScaleTransform(sx, sy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePenTransform(nativePen, sx, sy, order));
end;

function TGPPen.RotateTransform(angle: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePenTransform(nativePen, angle, order));
end;

function TGPPen.GetPenType: TPenType;
begin
  SetStatus(GdipGetPenFillType(nativePen, Result));
end;

function TGPPen.SetColor(color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipSetPenColor(nativePen, color));
end;

function TGPPen.SetBrush(brush: TGPBrush): TStatus;
begin
  Result := SetStatus(GdipSetPenBrushFill(nativePen, brush.nativeBrush));
end;

function TGPPen.GetColor(out color: TGPColor): TStatus;
var
  type_: TPenType;
  argb: Dword;
begin
  type_ := GetPenType;

  if (type_ <> PenTypeSolidColor) then
  begin
    Result := WrongState;
    Exit;
  end;

  SetStatus(GdipGetPenColor(nativePen, argb));
  if (lastResult = Ok) then
    color := argb;
  Result := lastResult;
end;

function TGPPen.GetBrush: TGPBrush;
var
  type_: TPenType;
  brush: TGPBrush;
  nativeBrush: GpBrush;
begin
  type_ := GetPenType;
  brush := nil;

  case type_ of
    PenTypeSolidColor:
      brush := TGPSolidBrush.Create;
    PenTypeHatchFill:
      brush := TGPHatchBrush.Create;
    PenTypeTextureFill:
      brush := TGPTextureBrush.Create;
    PenTypePathGradient:
      brush := TGPBrush.Create;
    PenTypeLinearGradient:
      brush := TGPLinearGradientBrush.Create;
  end;

  if (brush <> nil) then
  begin
    SetStatus(GdipGetPenBrushFill(nativePen, nativeBrush));
    brush.SetNativeBrush(nativeBrush);
  end;

  Result := brush;
end;

function TGPPen.GetDashStyle: TDashStyle;
begin
  SetStatus(GdipGetPenDashStyle(nativePen, Result));
end;

function TGPPen.SetDashStyle(dashStyle: TDashStyle): TStatus;
begin
  Result := SetStatus(GdipSetPenDashStyle(nativePen, dashStyle));
end;

function TGPPen.GetDashOffset: Single;
begin
  SetStatus(GdipGetPenDashOffset(nativePen, Result));
end;

function TGPPen.SetDashOffset(dashOffset: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenDashOffset(nativePen, dashOffset));
end;

function TGPPen.SetDashPattern(dashArray: PSingle; count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenDashArray(nativePen, dashArray, count));
end;

function TGPPen.GetDashPatternCount: Integer;
begin
  SetStatus(GdipGetPenDashCount(nativePen, Result));
end;

function TGPPen.GetDashPattern(dashArray: PSingle; count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPenDashArray(nativePen, dashArray, count));
end;

function TGPPen.SetCompoundArray(compoundArray: PSingle;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenCompoundArray(nativePen, compoundArray, count));
end;

function TGPPen.GetCompoundArrayCount: Integer;
begin
  SetStatus(GdipGetPenCompoundCount(nativePen, Result));
end;

function TGPPen.GetCompoundArray(compoundArray: PSingle;
  count: Integer): TStatus;
begin
  if (count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCompoundArray(nativePen, compoundArray,
        count));
end;

function TGPPen.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPPen.Create(nativePen: GpPen; status: TStatus);
begin
  lastResult := status;
  SetNativePen(nativePen);
end;

procedure TGPPen.SetNativePen(nativePen: GpPen);
begin
  self.nativePen := nativePen;
end;

function TGPPen.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

(* *************************************************************************\
  *
  *   GDI+ Brush class
  *
  \************************************************************************* *)

// --------------------------------------------------------------------------
// Abstract base class for various brush types
// --------------------------------------------------------------------------

destructor TGPBrush.Destroy;
begin
  GdipDeleteBrush(nativeBrush);
end;

function TGPBrush.Clone: TGPBrush;
var
  brush: GpBrush;
  newBrush: TGPBrush;
begin
  brush := nil;
  SetStatus(GdipCloneBrush(nativeBrush, brush));
  newBrush := TGPBrush.Create(brush, lastResult);
  if (newBrush = nil) then
    GdipDeleteBrush(brush);
  Result := newBrush;
end;

function TGPBrush.GetType: TBrushType;
var
  type_: TBrushType;
begin
  type_ := TBrushType(-1);
  SetStatus(GdipGetBrushType(nativeBrush, type_));
  Result := type_;
end;

function TGPBrush.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPBrush.Create;
begin
  SetStatus(NotImplemented);
end;

constructor TGPBrush.Create(nativeBrush: GpBrush; status: TStatus);
begin
  lastResult := status;
  SetNativeBrush(nativeBrush);
end;

procedure TGPBrush.SetNativeBrush(nativeBrush: GpBrush);
begin
  self.nativeBrush := nativeBrush;
end;

function TGPBrush.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

// --------------------------------------------------------------------------
// Solid Fill Brush Object
// --------------------------------------------------------------------------

constructor TGPSolidBrush.Create(color: TGPColor);
var
  brush: GpSolidFill;
begin
  brush := nil;
  lastResult := GdipCreateSolidFill(color, brush);
  SetNativeBrush(brush);
end;

function TGPSolidBrush.GetColor(out color: TGPColor): TStatus;
begin
  SetStatus(GdipGetSolidFillColor(GpSolidFill(nativeBrush), color));
  Result := lastResult;
end;

function TGPSolidBrush.SetColor(color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipSetSolidFillColor(GpSolidFill(nativeBrush), color));
end;

constructor TGPSolidBrush.Create;
begin
  // hide parent function
end;

// --------------------------------------------------------------------------
// Texture Brush Fill Object
// --------------------------------------------------------------------------

constructor TGPTextureBrush.Create(image: TGPImage;
  wrapMode: TWrapMode = WrapModeTile);
var
  texture: GpTexture;
begin
  // texture := nil;
  lastResult := GdipCreateTexture(image.nativeImage, wrapMode, texture);
  SetNativeBrush(texture);
end;

// When creating a texture brush from a metafile image, the dstRect
// is used to specify the size that the metafile image should be
// rendered at in the device units of the destination graphics.
// It is NOT used to crop the metafile image, so only the width
// and height values matter for metafiles.

constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode;
  dstRect: TGPRectF);
var
  texture: GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2(image.nativeImage, wrapMode, dstRect.x,
    dstRect.y, dstRect.width, dstRect.height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; dstRect: TGPRectF;
  imageAttributes: TGPImageAttributes = nil);
var
  texture: GpTexture;
  ImgAtt: GpImageAttributes;
begin
  texture := nil;
  if assigned(imageAttributes) then
    ImgAtt := imageAttributes.nativeImageAttr
  else
    ImgAtt := nil;

  lastResult := GdipCreateTextureIA(image.nativeImage, ImgAtt, dstRect.x,
    dstRect.y, dstRect.width, dstRect.height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; dstRect: TGPRect;
  imageAttributes: TGPImageAttributes = nil);
var
  texture: GpTexture;
  ImgAtt: GpImageAttributes;
begin
  texture := nil;
  if assigned(imageAttributes) then
    ImgAtt := imageAttributes.nativeImageAttr
  else
    ImgAtt := nil;
  lastResult := GdipCreateTextureIAI(image.nativeImage, ImgAtt, dstRect.x,
    dstRect.y, dstRect.width, dstRect.height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode;
  dstRect: TGPRect);
var
  texture: GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2I(image.nativeImage, wrapMode, dstRect.x,
    dstRect.y, dstRect.width, dstRect.height, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode;
  dstX, dstY, dstWidth, dstHeight: Single);
var
  texture: GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2(image.nativeImage, wrapMode, dstX, dstY,
    dstWidth, dstHeight, texture);
  SetNativeBrush(texture);
end;

constructor TGPTextureBrush.Create(image: TGPImage; wrapMode: TWrapMode;
  dstX, dstY, dstWidth, dstHeight: Integer);
var
  texture: GpTexture;
begin
  texture := nil;
  lastResult := GdipCreateTexture2I(image.nativeImage, wrapMode, dstX, dstY,
    dstWidth, dstHeight, texture);
  SetNativeBrush(texture);
end;

function TGPTextureBrush.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetTextureTransform(GpTexture(nativeBrush),
      matrix.nativeMatrix));
end;

function TGPTextureBrush.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetTextureTransform(GpTexture(nativeBrush),
      matrix.nativeMatrix));
end;

function TGPTextureBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetTextureTransform(GpTexture(nativeBrush)));
end;

function TGPTextureBrush.MultiplyTransform(matrix: TGPMatrix;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyTextureTransform(GpTexture(nativeBrush),
      matrix.nativeMatrix, order));
end;

function TGPTextureBrush.TranslateTransform(dx, dy: Single;
  order: MatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateTextureTransform(GpTexture(nativeBrush), dx,
      dy, order));
end;

function TGPTextureBrush.ScaleTransform(sx, sy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleTextureTransform(GpTexture(nativeBrush), sx, sy,
      order));
end;

function TGPTextureBrush.RotateTransform(angle: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateTextureTransform(GpTexture(nativeBrush), angle,
      order));
end;

function TGPTextureBrush.SetWrapMode(wrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetTextureWrapMode(GpTexture(nativeBrush), wrapMode));
end;

function TGPTextureBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetTextureWrapMode(GpTexture(nativeBrush), Result));
end;

function TGPTextureBrush.GetImage: TGPImage;
var
  image: GpImage;
begin
  SetStatus(GdipGetTextureImage(GpTexture(nativeBrush), image));
  Result := TGPImage.Create(image, lastResult);
  if (Result = nil) then
    GdipDisposeImage(image);
end;

constructor TGPTextureBrush.Create;
begin
  // hide parent function
end;

// --------------------------------------------------------------------------
// Linear Gradient Brush Object
// --------------------------------------------------------------------------

constructor TGPLinearGradientBrush.Create(const point1, point2: TGPPointF;
  color1, color2: TGPColor);
var
  brush: GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrush(@point1, @point2, color1, color2,
    WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(const point1, point2: TGPPoint;
  color1, color2: TGPColor);
var
  brush: GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushI(@point1, @point2, color1, color2,
    WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRectF;
  color1, color2: TGPColor; mode: TLinearGradientMode);
var
  brush: GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRect(@rect, color1, color2, mode,
    WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRect;
  color1, color2: TGPColor; mode: TLinearGradientMode);
var
  brush: GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRectI(@rect, color1, color2, mode,
    WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRectF;
  color1, color2: TGPColor; angle: Single; isAngleScalable: BOOL = FALSE);
var
  brush: GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRectWithAngle(@rect, color1, color2,
    angle, isAngleScalable, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

constructor TGPLinearGradientBrush.Create(rect: TGPRect;
  color1, color2: TGPColor; angle: Single; isAngleScalable: BOOL = FALSE);
var
  brush: GpLineGradient;
begin
  brush := nil;
  lastResult := GdipCreateLineBrushFromRectWithAngleI(@rect, color1, color2,
    angle, isAngleScalable, WrapModeTile, brush);
  SetNativeBrush(brush);
end;

function TGPLinearGradientBrush.SetLinearColors(color1, color2: TGPColor)
  : TStatus;
begin
  Result := SetStatus(GdipSetLineColors(GpLineGradient(nativeBrush), color1,
      color2));
end;

function TGPLinearGradientBrush.GetLinearColors(out color1, color2: TGPColor)
  : TStatus;
var
  colors: array [0 .. 1] of TGPColor;
begin
  SetStatus(GdipGetLineColors(GpLineGradient(nativeBrush), @colors));
  if (lastResult = Ok) then
  begin
    color1 := colors[0];
    color2 := colors[1];
  end;
  Result := lastResult;
end;

function TGPLinearGradientBrush.GetRectangle(out rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetLineRect(GpLineGradient(nativeBrush), @rect));
end;

function TGPLinearGradientBrush.GetRectangle(out rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetLineRectI(GpLineGradient(nativeBrush), @rect));
end;

function TGPLinearGradientBrush.SetGammaCorrection(useGammaCorrection: BOOL)
  : TStatus;
begin
  Result := SetStatus(GdipSetLineGammaCorrection(GpLineGradient(nativeBrush),
      useGammaCorrection));
end;

function TGPLinearGradientBrush.GetGammaCorrection: BOOL;
var
  useGammaCorrection: BOOL;
begin
  SetStatus(GdipGetLineGammaCorrection(GpLineGradient(nativeBrush),
      useGammaCorrection));
  Result := useGammaCorrection;
end;

function TGPLinearGradientBrush.GetBlendCount: Integer;
var
  count: Integer;
begin
  count := 0;
  SetStatus(GdipGetLineBlendCount(GpLineGradient(nativeBrush), count));
  Result := count;
end;

function TGPLinearGradientBrush.SetBlend(blendFactors, blendPositions: PSingle;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetLineBlend(GpLineGradient(nativeBrush),
      blendFactors, blendPositions, count));
end;

function TGPLinearGradientBrush.GetBlend(blendFactors, blendPositions: PSingle;
  count: Integer): TStatus;
begin
  if ((count <= 0) or (blendFactors = nil) or (blendPositions = nil)) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLineBlend(GpLineGradient(nativeBrush),
        blendFactors, blendPositions, count));
end;

function TGPLinearGradientBrush.GetInterpolationColorCount: Integer;
var
  count: Integer;
begin
  count := 0;
  SetStatus(GdipGetLinePresetBlendCount(GpLineGradient(nativeBrush), count));
  Result := count;
end;

function TGPLinearGradientBrush.SetInterpolationColors(presetColors: PGPColor;
  blendPositions: PSingle; count: Integer): TStatus;
begin
  if (count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipSetLinePresetBlend(GpLineGradient(nativeBrush),
        PARGB(presetColors), blendPositions, count));
end;

function TGPLinearGradientBrush.GetInterpolationColors(presetColors: PGPColor;
  blendPositions: PSingle; count: Integer): TStatus;
begin
  if (count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLinePresetBlend(GpLineGradient(nativeBrush),
        PARGB(presetColors), blendPositions, count));
end;

function TGPLinearGradientBrush.SetBlendBellShape(focus: Single;
  Scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetLineSigmaBlend(GpLineGradient(nativeBrush), focus,
      Scale));
end;

function TGPLinearGradientBrush.SetBlendTriangularShape(focus: Single;
  Scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetLineLinearBlend(GpLineGradient(nativeBrush),
      focus, Scale));
end;

function TGPLinearGradientBrush.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetLineTransform(GpLineGradient(nativeBrush),
      matrix.nativeMatrix));
end;

function TGPLinearGradientBrush.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetLineTransform(GpLineGradient(nativeBrush),
      matrix.nativeMatrix));
end;

function TGPLinearGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetLineTransform(GpLineGradient(nativeBrush)));
end;

function TGPLinearGradientBrush.MultiplyTransform(matrix: TGPMatrix;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyLineTransform(GpLineGradient(nativeBrush),
      matrix.nativeMatrix, order));
end;

function TGPLinearGradientBrush.TranslateTransform(dx, dy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateLineTransform(GpLineGradient(nativeBrush),
      dx, dy, order));
end;

function TGPLinearGradientBrush.ScaleTransform(sx, sy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleLineTransform(GpLineGradient(nativeBrush), sx,
      sy, order));
end;

function TGPLinearGradientBrush.RotateTransform(angle: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateLineTransform(GpLineGradient(nativeBrush),
      angle, order));
end;

function TGPLinearGradientBrush.SetWrapMode(wrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetLineWrapMode(GpLineGradient(nativeBrush),
      wrapMode));
end;

function TGPLinearGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetLineWrapMode(GpLineGradient(nativeBrush), Result));
end;

constructor TGPLinearGradientBrush.Create;
begin
  // hide parent function
end;

// --------------------------------------------------------------------------
// Hatch Brush Object
// --------------------------------------------------------------------------

constructor TGPHatchBrush.Create(hatchStyle: THatchStyle; foreColor: TGPColor;
  backColor: TGPColor = aclBlack);
var
  brush: GpHatch;
begin
  brush := nil;
  lastResult := GdipCreateHatchBrush(Integer(hatchStyle), foreColor, backColor,
    brush);
  SetNativeBrush(brush);
end;

function TGPHatchBrush.GetHatchStyle: THatchStyle;
begin
  SetStatus(GdipGetHatchStyle(GpHatch(nativeBrush), Result));
end;

function TGPHatchBrush.GetForegroundColor(out color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGetHatchForegroundColor(GpHatch(nativeBrush), color));
end;

function TGPHatchBrush.GetBackgroundColor(out color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGetHatchBackgroundColor(GpHatch(nativeBrush), color));
end;

constructor TGPHatchBrush.Create;
begin
end;

constructor TGPImage.Create(filename: WideString;
  useEmbeddedColorManagement: BOOL = FALSE);
begin
  nativeImage := nil;
  if (useEmbeddedColorManagement) then
  begin
    lastResult := GdipLoadImageFromFileICM(PWideChar(filename), nativeImage);
  end
  else
  begin
    lastResult := GdipLoadImageFromFile(PWideChar(filename), nativeImage);
  end;
end;

constructor TGPImage.Create(stream: IStream;
  useEmbeddedColorManagement: BOOL = FALSE);
begin
  nativeImage := nil;
  if (useEmbeddedColorManagement) then
    lastResult := GdipLoadImageFromStreamICM(stream, nativeImage)
  else
    lastResult := GdipLoadImageFromStream(stream, nativeImage);
end;

function TGPImage.FromFile(filename: WideString;
  useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
begin
  Result := TGPImage.Create(PWideChar(filename), useEmbeddedColorManagement);
end;

function TGPImage.FromStream(stream: IStream;
  useEmbeddedColorManagement: BOOL = FALSE): TGPImage;
begin
  Result := TGPImage.Create(stream, useEmbeddedColorManagement);
end;

destructor TGPImage.Destroy;
begin
  GdipDisposeImage(nativeImage);
end;

function TGPImage.Clone: TGPImage;
var
  cloneimage: GpImage;
begin
  cloneimage := nil;
  SetStatus(GdipCloneImage(nativeImage, cloneimage));
  Result := TGPImage.Create(cloneimage, lastResult);
end;

function TGPImage.Save(filename: WideString; const clsidEncoder: TGUID;
  encoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToFile(nativeImage, PWideChar(filename),
      @clsidEncoder, encoderParams));
end;

function TGPImage.Save(stream: IStream; const clsidEncoder: TGUID;
  encoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToStream(nativeImage, stream, @clsidEncoder,
      encoderParams));
end;

function TGPImage.SaveAdd(encoderParams: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipSaveAdd(nativeImage, encoderParams));
end;

function TGPImage.SaveAdd(newImage: TGPImage;
  encoderParams: PEncoderParameters): TStatus;
begin
  if (newImage = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSaveAddImage(nativeImage, newImage.nativeImage,
      encoderParams));
end;

function TGPImage.GetType: TImageType;
begin
  SetStatus(GdipGetImageType(nativeImage, Result));
end;

function TGPImage.GetPhysicalDimension(out size: TGPSizeF): TStatus;
var
  width, height: Single;
  status: TStatus;
begin
  status := SetStatus(GdipGetImageDimension(nativeImage, width, height));
  size.width := width;
  size.height := height;
  Result := status;
end;

function TGPImage.GetBounds(out srcRect: TGPRectF; out srcUnit: TUnit): TStatus;
begin
  Result := SetStatus(GdipGetImageBounds(nativeImage, @srcRect, srcUnit));
end;

function TGPImage.GetWidth: UINT;
var
  width: UINT;
begin
  width := 0;
  SetStatus(GdipGetImageWidth(nativeImage, width));
  Result := width;
end;

function TGPImage.GetHeight: UINT;
var
  height: UINT;
begin
  height := 0;
  SetStatus(GdipGetImageHeight(nativeImage, height));
  Result := height;
end;

function TGPImage.GetHorizontalResolution: Single;
var
  resolution: Single;
begin
  resolution := 0.0;
  SetStatus(GdipGetImageHorizontalResolution(nativeImage, resolution));
  Result := resolution;
end;

function TGPImage.GetVerticalResolution: Single;
var
  resolution: Single;
begin
  resolution := 0.0;
  SetStatus(GdipGetImageVerticalResolution(nativeImage, resolution));
  Result := resolution;
end;

function TGPImage.GetFlags: UINT;
var
  flags: UINT;
begin
  flags := 0;
  SetStatus(GdipGetImageFlags(nativeImage, flags));
  Result := flags;
end;

function TGPImage.GetRawFormat(out format: TGUID): TStatus;
begin
  Result := SetStatus(GdipGetImageRawFormat(nativeImage, @format));
end;

function TGPImage.GetPixelFormat: TPixelFormat;
begin
  SetStatus(GdipGetImagePixelFormat(nativeImage, Result));
end;

function TGPImage.GetPaletteSize: Integer;
var
  size: Integer;
begin
  size := 0;
  SetStatus(GdipGetImagePaletteSize(nativeImage, size));
  Result := size;
end;

function TGPImage.GetPalette(palette: PColorPalette; size: Integer): TStatus;
begin
  Result := SetStatus(GdipGetImagePalette(nativeImage, palette, size));
end;

function TGPImage.SetPalette(palette: PColorPalette): TStatus;
begin
  Result := SetStatus(GdipSetImagePalette(nativeImage, palette));
end;

function TGPImage.GetThumbnailImage(thumbWidth, thumbHeight: UINT;
  callback: GetThumbnailImageAbort = nil;
  callbackData: Pointer = nil): TGPImage;
var
  thumbimage: GpImage;
  newImage: TGPImage;
begin
  thumbimage := nil;
  SetStatus(GdipGetImageThumbnail(nativeImage, thumbWidth, thumbHeight,
      thumbimage, callback, callbackData));

  newImage := TGPImage.Create(thumbimage, lastResult);
  if (newImage = nil) then
    GdipDisposeImage(thumbimage);

  Result := newImage;
end;

function TGPImage.GetFrameDimensionsCount: UINT;
var
  count: UINT;
begin
  count := 0;
  SetStatus(GdipImageGetFrameDimensionsCount(nativeImage, count));
  Result := count;
end;

function TGPImage.GetFrameDimensionsList(dimensionIDs: PGUID;
  count: UINT): TStatus;
begin
  Result := SetStatus(GdipImageGetFrameDimensionsList(nativeImage,
      dimensionIDs, count));
end;

function TGPImage.GetFrameCount(const dimensionID: TGUID): UINT;
var
  count: UINT;
begin
  count := 0;
  SetStatus(GdipImageGetFrameCount(nativeImage, @dimensionID, count));
  Result := count;
end;

function TGPImage.SelectActiveFrame(const dimensionID: TGUID;
  frameIndex: UINT): TStatus;
begin
  Result := SetStatus(GdipImageSelectActiveFrame(nativeImage, @dimensionID,
      frameIndex));
end;

function TGPImage.RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
begin
  Result := SetStatus(GdipImageRotateFlip(nativeImage, rotateFlipType));
end;

function TGPImage.GetPropertyCount: UINT;
var
  numProperty: UINT;
begin
  numProperty := 0;
  SetStatus(GdipGetPropertyCount(nativeImage, numProperty));
  Result := numProperty;
end;

function TGPImage.GetPropertyIdList(numOfProperty: UINT;
  list: PPropID): TStatus;
begin
  Result := SetStatus(GdipGetPropertyIdList(nativeImage, numOfProperty, list));
end;

function TGPImage.GetPropertyItemSize(propId: propId): UINT;
var
  size: UINT;
begin
  size := 0;
  SetStatus(GdipGetPropertyItemSize(nativeImage, propId, size));
  Result := size;
end;

function TGPImage.GetPropertyItem(propId: propId; propSize: UINT;
  buffer: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetPropertyItem(nativeImage, propId, propSize,
      buffer));
end;

function TGPImage.GetPropertySize(out totalBufferSize, numProperties: UINT)
  : TStatus;
begin
  Result := SetStatus(GdipGetPropertySize(nativeImage, totalBufferSize,
      numProperties));
end;

function TGPImage.GetAllPropertyItems(totalBufferSize, numProperties: UINT;
  allItems: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetAllPropertyItems(nativeImage, totalBufferSize,
      numProperties, allItems));
end;

function TGPImage.RemovePropertyItem(propId: TPROPID): TStatus;
begin
  Result := SetStatus(GdipRemovePropertyItem(nativeImage, propId));
end;

function TGPImage.SetPropertyItem(const item: TPropertyItem): TStatus;
begin
  Result := SetStatus(GdipSetPropertyItem(nativeImage, @item));
end;

function TGPImage.GetEncoderParameterListSize(const clsidEncoder: TGUID): UINT;
var
  size: UINT;
begin
  size := 0;
  SetStatus(GdipGetEncoderParameterListSize(nativeImage, @clsidEncoder, size));
  Result := size;
end;

function TGPImage.GetEncoderParameterList(const clsidEncoder: TGUID;
  size: UINT; buffer: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipGetEncoderParameterList(nativeImage, @clsidEncoder,
      size, buffer));
end;

function TGPImage.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPImage.Create(nativeImage: GpImage; status: TStatus);
begin
  SetNativeImage(nativeImage);
  lastResult := status;
end;

procedure TGPImage.SetNativeImage(nativeImage: GpImage);
begin
  self.nativeImage := nativeImage;
end;

function TGPImage.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

// TGPBitmap

constructor TGPBitmap.Create(filename: WideString;
  useEmbeddedColorManagement: BOOL = FALSE);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  if (useEmbeddedColorManagement) then
    lastResult := GdipCreateBitmapFromFileICM(PWideChar(filename), bitmap)
  else
    lastResult := GdipCreateBitmapFromFile(PWideChar(filename), bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(stream: IStream;
  useEmbeddedColorManagement: BOOL = FALSE);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  if (useEmbeddedColorManagement) then
    lastResult := GdipCreateBitmapFromStreamICM(stream, bitmap)
  else
    lastResult := GdipCreateBitmapFromStream(stream, bitmap);
  SetNativeImage(bitmap);
end;

function TGPBitmap.FromFile(filename: WideString;
  useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
begin
  Result := TGPBitmap.Create(PWideChar(filename), useEmbeddedColorManagement);
end;

function TGPBitmap.FromStream(stream: IStream;
  useEmbeddedColorManagement: BOOL = FALSE): TGPBitmap;
begin
  Result := TGPBitmap.Create(stream, useEmbeddedColorManagement);
end;

constructor TGPBitmap.Create(width, height, stride: Integer;
  format: TPixelFormat; scan0: PBYTE);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromScan0(width, height, stride, format, scan0,
    bitmap);

  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(width, height: Integer;
  format: TPixelFormat = PixelFormat32bppARGB);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromScan0(width, height, 0, format, nil,
    bitmap);

  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(width, height: Integer; target: TGPGraphics);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromGraphics(width, height,
    target.nativeGraphics, bitmap);

  SetNativeImage(bitmap);
end;

function TGPBitmap.Clone(rect: TGPRect; format: TPixelFormat): TGPBitmap;
begin
  Result := Clone(rect.x, rect.y, rect.width, rect.height, format);
end;

function TGPBitmap.Clone(x, y, width, height: Integer;
  format: TPixelFormat): TGPBitmap;
var
  bitmap: TGPBitmap;
  gpdstBitmap: GpBitmap;
begin
  gpdstBitmap := nil;
  lastResult := GdipCloneBitmapAreaI(x, y, width, height, format,
    GpBitmap(nativeImage), gpdstBitmap);

  if (lastResult = Ok) then
  begin
    bitmap := TGPBitmap.Create(gpdstBitmap);
    if (bitmap = nil) then
      GdipDisposeImage(gpdstBitmap);
    Result := bitmap;
    Exit;
  end
  else
    Result := nil;
end;

function TGPBitmap.Clone(rect: TGPRectF; format: TPixelFormat): TGPBitmap;
begin
  Result := Clone(rect.x, rect.y, rect.width, rect.height, format);
end;

function TGPBitmap.Clone(x, y, width, height: Single;
  format: TPixelFormat): TGPBitmap;
var
  bitmap: TGPBitmap;
  gpdstBitmap: GpBitmap;
begin
  gpdstBitmap := nil;
  SetStatus(GdipCloneBitmapArea(x, y, width, height, format,
      GpBitmap(nativeImage), gpdstBitmap));

  if (lastResult = Ok) then
  begin
    bitmap := TGPBitmap.Create(gpdstBitmap);
    if (bitmap = nil) then
      GdipDisposeImage(gpdstBitmap);
    Result := bitmap;
  end
  else
    Result := nil;
end;

function TGPBitmap.LockBits(rect: TGPRect; flags: UINT; format: TPixelFormat;
  out lockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapLockBits(GpBitmap(nativeImage), @rect, flags,
      format, @lockedBitmapData));
end;

function TGPBitmap.UnlockBits(var lockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapUnlockBits(GpBitmap(nativeImage),
      @lockedBitmapData));
end;

function TGPBitmap.GetPixel(x, y: Integer; out color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipBitmapGetPixel(GpBitmap(nativeImage), x, y, color));
end;

function TGPBitmap.SetPixel(x, y: Integer; color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipBitmapSetPixel(GpBitmap(nativeImage), x, y, color));
end;

function TGPBitmap.SetResolution(xdpi, ydpi: Single): TStatus;
begin
  Result := SetStatus(GdipBitmapSetResolution(GpBitmap(nativeImage), xdpi,
      ydpi));
end;

constructor TGPBitmap.Create(var gdiBitmapInfo: TBITMAPINFO;
  gdiBitmapData: Pointer);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromGdiDib(@gdiBitmapInfo, gdiBitmapData,
    bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(hbm: HBITMAP; hpal: HPALETTE);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromHBITMAP(hbm, hpal, bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(hicon: hicon);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromHICON(hicon, bitmap);
  SetNativeImage(bitmap);
end;

constructor TGPBitmap.Create(hInstance: HMODULE; bitmapName: WideString);
var
  bitmap: GpBitmap;
begin
  bitmap := nil;
  lastResult := GdipCreateBitmapFromResource(hInstance, PWideChar(bitmapName),
    bitmap);
  SetNativeImage(bitmap);
end;

function TGPBitmap.FromBITMAPINFO(var gdiBitmapInfo: TBITMAPINFO;
  gdiBitmapData: Pointer): TGPBitmap;
begin
  Result := TGPBitmap.Create(gdiBitmapInfo, gdiBitmapData);
end;

function TGPBitmap.FromHBITMAP(hbm: HBITMAP; hpal: HPALETTE): TGPBitmap;
begin
  Result := TGPBitmap.Create(hbm, hpal);
end;

function TGPBitmap.FromHICON(hicon: hicon): TGPBitmap;
begin
  Result := TGPBitmap.Create(hicon);
end;

function TGPBitmap.FromResource(hInstance: HMODULE;
  bitmapName: WideString): TGPBitmap;
begin
  Result := TGPBitmap.Create(hInstance, PWideChar(bitmapName));
end;

function TGPBitmap.GetHBITMAP(colorBackground: TGPColor;
  out hbmReturn: HBITMAP): TStatus;
begin
  Result := SetStatus(GdipCreateHBITMAPFromBitmap(GpBitmap(nativeImage),
      hbmReturn, colorBackground));
end;

function TGPBitmap.GetHICON(out hicon: hicon): TStatus;
begin
  Result := SetStatus(GdipCreateHICONFromBitmap(GpBitmap(nativeImage), hicon));
end;

constructor TGPBitmap.Create(nativeBitmap: GpBitmap);
begin
  lastResult := Ok;
  SetNativeImage(nativeBitmap);
end;

(* *************************************************************************\
  *
  *   GDI+ Graphics Object
  *
  \************************************************************************* *)

function TGPGraphics.FromHDC(hdc: hdc): TGPGraphics;
begin
  Result := TGPGraphics.Create(hdc);
end;

function TGPGraphics.FromHDC(hdc: hdc; hdevice: THANDLE): TGPGraphics;
begin
  Result := TGPGraphics.Create(hdc, hdevice);
end;

function TGPGraphics.FromHWND(hwnd: hwnd; icm: BOOL = FALSE): TGPGraphics;
begin
  Result := TGPGraphics.Create(hwnd, icm);
end;

function TGPGraphics.FromImage(image: TGPImage): TGPGraphics;
begin
  Result := TGPGraphics.Create(image);
end;

constructor TGPGraphics.Create(hdc: hdc);
var
  graphics: GpGraphics;
begin
  graphics := nil;
  lastResult := GdipCreateFromHDC(hdc, graphics);
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(hdc: hdc; hdevice: THANDLE);
var
  graphics: GpGraphics;
begin
  graphics := nil;
  lastResult := GdipCreateFromHDC2(hdc, hdevice, graphics);
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(hwnd: hwnd; icm: BOOL { = FALSE } );
var
  graphics: GpGraphics;
begin
  graphics := nil;
  if icm then
    lastResult := GdipCreateFromHWNDICM(hwnd, graphics)
  else
    lastResult := GdipCreateFromHWND(hwnd, graphics);
  SetNativeGraphics(graphics);
end;

constructor TGPGraphics.Create(image: TGPImage);
var
  graphics: GpGraphics;
begin
  graphics := nil;
  if (image <> nil) then
    lastResult := GdipGetImageGraphicsContext(image.nativeImage, graphics);
  SetNativeGraphics(graphics);
end;

destructor TGPGraphics.Destroy;
begin
  GdipDeleteGraphics(nativeGraphics);
end;

procedure TGPGraphics.Flush(intention: TFlushIntention = FlushIntentionFlush);
begin
  GdipFlush(nativeGraphics, intention);
end;

// ------------------------------------------------------------------------
// GDI Interop methods
// ------------------------------------------------------------------------

// Locks the graphics until ReleaseDC is called

function TGPGraphics.GetHDC: hdc;
begin
  SetStatus(GdipGetDC(nativeGraphics, Result));
end;

procedure TGPGraphics.ReleaseHDC(hdc: hdc);
begin
  SetStatus(GdipReleaseDC(nativeGraphics, hdc));
end;

// ------------------------------------------------------------------------
// Rendering modes
// ------------------------------------------------------------------------

function TGPGraphics.SetRenderingOrigin(x, y: Integer): TStatus;
begin
  Result := SetStatus(GdipSetRenderingOrigin(nativeGraphics, x, y));
end;

function TGPGraphics.GetRenderingOrigin(out x, y: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRenderingOrigin(nativeGraphics, x, y));
end;

function TGPGraphics.SetCompositingMode(compositingMode: TCompositingMode)
  : TStatus;
begin
  Result := SetStatus(GdipSetCompositingMode(nativeGraphics, compositingMode));
end;

function TGPGraphics.GetCompositingMode: TCompositingMode;
begin
  SetStatus(GdipGetCompositingMode(nativeGraphics, Result));
end;

function TGPGraphics.SetCompositingQuality
  (compositingQuality: TCompositingQuality): TStatus;
begin
  Result := SetStatus(GdipSetCompositingQuality(nativeGraphics,
      compositingQuality));
end;

function TGPGraphics.GetCompositingQuality: TCompositingQuality;
begin
  SetStatus(GdipGetCompositingQuality(nativeGraphics, Result));
end;

function TGPGraphics.SetTextRenderingHint(newMode: TTextRenderingHint): TStatus;
begin
  Result := SetStatus(GdipSetTextRenderingHint(nativeGraphics, newMode));
end;

function TGPGraphics.GetTextRenderingHint: TTextRenderingHint;
begin
  SetStatus(GdipGetTextRenderingHint(nativeGraphics, Result));
end;

function TGPGraphics.SetTextContrast(contrast: UINT): TStatus;
begin
  Result := SetStatus(GdipSetTextContrast(nativeGraphics, contrast));
end;

function TGPGraphics.GetTextContrast: UINT;
begin
  SetStatus(GdipGetTextContrast(nativeGraphics, Result));
end;

function TGPGraphics.GetInterpolationMode: TInterpolationMode;
var
  mode: TInterpolationMode;
begin
  mode := InterpolationModeInvalid;
  SetStatus(GdipGetInterpolationMode(nativeGraphics, mode));
  Result := mode;
end;

function TGPGraphics.SetInterpolationMode(interpolationMode: TInterpolationMode)
  : TStatus;
begin
  Result := SetStatus(GdipSetInterpolationMode(nativeGraphics,
      interpolationMode));
end;

function TGPGraphics.GetSmoothingMode: TSmoothingMode;
var
  smoothingMode: TSmoothingMode;
begin
  smoothingMode := SmoothingModeInvalid;
  SetStatus(GdipGetSmoothingMode(nativeGraphics, smoothingMode));
  Result := smoothingMode;
end;

function TGPGraphics.SetSmoothingMode(smoothingMode: TSmoothingMode): TStatus;
begin
  Result := SetStatus(GdipSetSmoothingMode(nativeGraphics, smoothingMode));
end;

function TGPGraphics.GetPixelOffsetMode: TPixelOffsetMode;
var
  pixelOffsetMode: TPixelOffsetMode;
begin
  pixelOffsetMode := PixelOffsetModeInvalid;
  SetStatus(GdipGetPixelOffsetMode(nativeGraphics, pixelOffsetMode));
  Result := pixelOffsetMode;
end;

function TGPGraphics.SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode)
  : TStatus;
begin
  Result := SetStatus(GdipSetPixelOffsetMode(nativeGraphics, pixelOffsetMode));
end;

// ------------------------------------------------------------------------
// Manipulate current world transform
// ------------------------------------------------------------------------

function TGPGraphics.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetWorldTransform(nativeGraphics,
      matrix.nativeMatrix));
end;

function TGPGraphics.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetWorldTransform(nativeGraphics));
end;

function TGPGraphics.MultiplyTransform(matrix: TGPMatrix;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyWorldTransform(nativeGraphics,
      matrix.nativeMatrix, order));
end;

function TGPGraphics.TranslateTransform(dx, dy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateWorldTransform(nativeGraphics, dx, dy,
      order));
end;

function TGPGraphics.ScaleTransform(sx, sy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleWorldTransform(nativeGraphics, sx, sy, order));
end;

function TGPGraphics.RotateTransform(angle: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateWorldTransform(nativeGraphics, angle, order));
end;

function TGPGraphics.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetWorldTransform(nativeGraphics,
      matrix.nativeMatrix));
end;

function TGPGraphics.SetPageUnit(unit_: TUnit): TStatus;
begin
  Result := SetStatus(GdipSetPageUnit(nativeGraphics, unit_));
end;

function TGPGraphics.SetPageScale(Scale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPageScale(nativeGraphics, Scale));
end;

function TGPGraphics.GetPageUnit: TUnit;
begin
  SetStatus(GdipGetPageUnit(nativeGraphics, Result));
end;

function TGPGraphics.GetPageScale: Single;
begin
  SetStatus(GdipGetPageScale(nativeGraphics, Result));
end;

function TGPGraphics.GetDpiX: Single;
begin
  SetStatus(GdipGetDpiX(nativeGraphics, Result));
end;

function TGPGraphics.GetDpiY: Single;
begin
  SetStatus(GdipGetDpiY(nativeGraphics, Result));
end;

function TGPGraphics.TransformPoints(destSpace: TCoordinateSpace;
  srcSpace: TCoordinateSpace; pts: PGPPointF; count: Integer): TStatus;
begin
  Result := SetStatus(GdipTransformPoints(nativeGraphics, destSpace, srcSpace,
      pts, count));
end;

function TGPGraphics.TransformPoints(destSpace: TCoordinateSpace;
  srcSpace: TCoordinateSpace; pts: PGPPoint; count: Integer): TStatus;
begin

  Result := SetStatus(GdipTransformPointsI(nativeGraphics, destSpace, srcSpace,
      pts, count));
end;

// ------------------------------------------------------------------------
// GetNearestColor (for <= 8bpp surfaces).  Note: Alpha is ignored.
// ------------------------------------------------------------------------

function TGPGraphics.GetNearestColor(var color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGetNearestColor(nativeGraphics, @color));
end;

function TGPGraphics.DrawLine(pen: TGPPen; x1, y1, x2, y2: Single): TStatus;
begin
  Result := SetStatus(GdipDrawLine(nativeGraphics, pen.nativePen, x1, y1, x2,
      y2));
end;

function TGPGraphics.DrawLine(pen: TGPPen; const pt1, pt2: TGPPointF): TStatus;
begin
  Result := DrawLine(pen, pt1.x, pt1.y, pt2.x, pt2.y);
end;

function TGPGraphics.DrawLines(pen: TGPPen; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLines(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawLine(pen: TGPPen; x1, y1, x2, y2: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLineI(nativeGraphics, pen.nativePen, x1, y1, x2,
      y2));
end;

function TGPGraphics.DrawLine(pen: TGPPen; const pt1, pt2: TGPPoint): TStatus;
begin
  Result := DrawLine(pen, pt1.x, pt1.y, pt2.x, pt2.y);
end;

function TGPGraphics.DrawLines(pen: TGPPen; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLinesI(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawArc(pen: TGPPen; x, y, width, height, startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawArc(nativeGraphics, pen.nativePen, x, y, width,
      height, startAngle, sweepAngle));
end;

function TGPGraphics.DrawArc(pen: TGPPen; const rect: TGPRectF;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawArc(pen, rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawArc(pen: TGPPen; x, y, width, height: Integer;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawArcI(nativeGraphics, pen.nativePen, x, y, width,
      height, startAngle, sweepAngle));
end;

function TGPGraphics.DrawArc(pen: TGPPen; const rect: TGPRect;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawArc(pen, rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4,
  y4: Single): TStatus;
begin
  Result := SetStatus(GdipDrawBezier(nativeGraphics, pen.nativePen, x1, y1, x2,
      y2, x3, y3, x4, y4));
end;

function TGPGraphics.DrawBezier(pen: TGPPen;
  const pt1, pt2, pt3, pt4: TGPPointF): TStatus;
begin
  Result := DrawBezier(pen, pt1.x, pt1.y, pt2.x, pt2.y, pt3.x, pt3.y, pt4.x,
    pt4.y);
end;

function TGPGraphics.DrawBeziers(pen: TGPPen; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawBeziers(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawBezier(pen: TGPPen; x1, y1, x2, y2, x3, y3, x4,
  y4: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawBezierI(nativeGraphics, pen.nativePen, x1, y1,
      x2, y2, x3, y3, x4, y4));
end;

function TGPGraphics.DrawBezier(pen: TGPPen; const pt1, pt2, pt3, pt4: TGPPoint)
  : TStatus;
begin
  Result := DrawBezier(pen, pt1.x, pt1.y, pt2.x, pt2.y, pt3.x, pt3.y, pt4.x,
    pt4.y);
end;

function TGPGraphics.DrawBeziers(pen: TGPPen; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawBeziersI(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; const rect: TGPRectF): TStatus;
begin
  Result := DrawRectangle(pen, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; x, y, width, height: Single)
  : TStatus;
begin
  Result := SetStatus(GdipDrawRectangle(nativeGraphics, pen.nativePen, x, y,
      width, height));
end;

function TGPGraphics.DrawRectangles(pen: TGPPen; rects: PGPRectF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawRectangles(nativeGraphics, pen.nativePen, rects,
      count));
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; const rect: TGPRect): TStatus;
begin
  Result := DrawRectangle(pen, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.DrawRectangle(pen: TGPPen; x, y, width, height: Integer)
  : TStatus;
begin
  Result := SetStatus(GdipDrawRectangleI(nativeGraphics, pen.nativePen, x, y,
      width, height));
end;

function TGPGraphics.DrawRectangles(pen: TGPPen; rects: PGPRect;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawRectanglesI(nativeGraphics, pen.nativePen, rects,
      count));
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; const rect: TGPRectF): TStatus;
begin
  Result := DrawEllipse(pen, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; x, y, width, height: Single)
  : TStatus;
begin
  Result := SetStatus(GdipDrawEllipse(nativeGraphics, pen.nativePen, x, y,
      width, height));
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; const rect: TGPRect): TStatus;
begin
  Result := DrawEllipse(pen, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.DrawEllipse(pen: TGPPen; x, y, width, height: Integer)
  : TStatus;
begin
  Result := SetStatus(GdipDrawEllipseI(nativeGraphics, pen.nativePen, x, y,
      width, height));
end;

function TGPGraphics.DrawPie(pen: TGPPen; const rect: TGPRectF;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawPie(pen, rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawPie(pen: TGPPen; x, y, width, height, startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawPie(nativeGraphics, pen.nativePen, x, y, width,
      height, startAngle, sweepAngle));
end;

function TGPGraphics.DrawPie(pen: TGPPen; const rect: TGPRect;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := DrawPie(pen, rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphics.DrawPie(pen: TGPPen; x, y, width, height: Integer;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipDrawPieI(nativeGraphics, pen.nativePen, x, y, width,
      height, startAngle, sweepAngle));
end;

function TGPGraphics.DrawPolygon(pen: TGPPen; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawPolygon(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawPolygon(pen: TGPPen; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawPolygonI(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawPath(pen: TGPPen; path: TGPGraphicsPath): TStatus;
var
  nPen: GpPen;
  nPath: GpPath;
begin
  if assigned(pen) then
    nPen := pen.nativePen
  else
    nPen := nil;
  if assigned(path) then
    nPath := path.nativePath
  else
    nPath := nil;
  Result := SetStatus(GdipDrawPath(nativeGraphics, nPen, nPath));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawCurve(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPointF; count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawCurve2(nativeGraphics, pen.nativePen, points,
      count, tension));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPointF; count, offset,
  numberOfSegments: Integer; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipDrawCurve3(nativeGraphics, pen.nativePen, points,
      count, offset, numberOfSegments, tension));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawCurveI(nativeGraphics, pen.nativePen, points,
      count));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPoint; count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawCurve2I(nativeGraphics, pen.nativePen, points,
      count, tension));
end;

function TGPGraphics.DrawCurve(pen: TGPPen; points: PGPPoint; count, offset,
  numberOfSegments: Integer; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipDrawCurve3I(nativeGraphics, pen.nativePen, points,
      count, offset, numberOfSegments, tension));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve(nativeGraphics, pen.nativePen,
      points, count));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPointF;
  count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve2(nativeGraphics, pen.nativePen,
      points, count, tension));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurveI(nativeGraphics, pen.nativePen,
      points, count));
end;

function TGPGraphics.DrawClosedCurve(pen: TGPPen; points: PGPPoint;
  count: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve2I(nativeGraphics, pen.nativePen,
      points, count, tension));
end;

function TGPGraphics.Clear(color: TGPColor): TStatus;
begin
  Result := SetStatus(GdipGraphicsClear(nativeGraphics, color));
end;

function TGPGraphics.FillRectangle(brush: TGPBrush;
  const rect: TGPRectF): TStatus;
begin
  Result := FillRectangle(brush, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.FillRectangle(brush: TGPBrush; x, y, width, height: Single)
  : TStatus;
begin
  Result := SetStatus(GdipFillRectangle(nativeGraphics, brush.nativeBrush, x,
      y, width, height));
end;

function TGPGraphics.FillRectangles(brush: TGPBrush; rects: PGPRectF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangles(nativeGraphics, brush.nativeBrush,
      rects, count));
end;

function TGPGraphics.FillRectangle(brush: TGPBrush;
  const rect: TGPRect): TStatus;
begin
  Result := FillRectangle(brush, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.FillRectangle(brush: TGPBrush;
  x, y, width, height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangleI(nativeGraphics, brush.nativeBrush, x,
      y, width, height));
end;

function TGPGraphics.FillRectangles(brush: TGPBrush; rects: PGPRect;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectanglesI(nativeGraphics, brush.nativeBrush,
      rects, count));
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := FillPolygon(brush, points, count, FillModeAlternate);
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPointF;
  count: Integer; fillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygon(nativeGraphics, brush.nativeBrush,
      points, count, fillMode));
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := FillPolygon(brush, points, count, FillModeAlternate);
end;

function TGPGraphics.FillPolygon(brush: TGPBrush; points: PGPPoint;
  count: Integer; fillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygonI(nativeGraphics, brush.nativeBrush,
      points, count, fillMode));
end;

function TGPGraphics.FillEllipse(brush: TGPBrush;
  const rect: TGPRectF): TStatus;
begin
  Result := FillEllipse(brush, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; x, y, width, height: Single)
  : TStatus;
begin
  Result := SetStatus(GdipFillEllipse(nativeGraphics, brush.nativeBrush, x, y,
      width, height));
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; const rect: TGPRect): TStatus;
begin
  Result := FillEllipse(brush, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.FillEllipse(brush: TGPBrush; x, y, width, height: Integer)
  : TStatus;
begin
  Result := SetStatus(GdipFillEllipseI(nativeGraphics, brush.nativeBrush, x, y,
      width, height));
end;

function TGPGraphics.FillPie(brush: TGPBrush; const rect: TGPRectF;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := FillPie(brush, rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphics.FillPie(brush: TGPBrush; x, y, width, height, startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPie(nativeGraphics, brush.nativeBrush, x, y,
      width, height, startAngle, sweepAngle));
end;

function TGPGraphics.FillPie(brush: TGPBrush; const rect: TGPRect;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := FillPie(brush, rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphics.FillPie(brush: TGPBrush; x, y, width, height: Integer;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPieI(nativeGraphics, brush.nativeBrush, x, y,
      width, height, startAngle, sweepAngle));
end;

function TGPGraphics.fillPath(brush: TGPBrush; path: TGPGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipFillPath(nativeGraphics, brush.nativeBrush,
      path.nativePath));
end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve(nativeGraphics, brush.nativeBrush,
      points, count));

end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPointF;
  count: Integer; fillMode: TFillMode; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2(nativeGraphics, brush.nativeBrush,
      points, count, tension, fillMode));
end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurveI(nativeGraphics, brush.nativeBrush,
      points, count));
end;

function TGPGraphics.FillClosedCurve(brush: TGPBrush; points: PGPPoint;
  count: Integer; fillMode: TFillMode; tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2I(nativeGraphics, brush.nativeBrush,
      points, count, tension, fillMode));
end;

function TGPGraphics.FillRegion(brush: TGPBrush; region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipFillRegion(nativeGraphics, brush.nativeBrush,
      region.nativeRegion));
end;

function TGPGraphics.DrawString(string_: WideString; length: Integer;
  Font: TGPFont; const layoutRect: TGPRectF; stringFormat: TGPStringFormat;
  brush: TGPBrush): TStatus;
var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(stringFormat) then
    nStringFormat := stringFormat.nativeFormat
  else
    nStringFormat := nil;
  if assigned(brush) then
    nBrush := brush.nativeBrush
  else
    nBrush := nil;
  Result := SetStatus(GdipDrawString(nativeGraphics, PWideChar(string_),
      length, nFont, @layoutRect, nStringFormat, nBrush));
end;

function TGPGraphics.DrawString(string_: WideString; length: Integer;
  Font: TGPFont; const origin: TGPPointF; brush: TGPBrush): TStatus;
var
  rect: TGPRectF;
  nFont: GpFont;
  nBrush: GpBrush;
begin
  rect.x := origin.x;
  rect.y := origin.y;
  rect.width := 0.0;
  rect.height := 0.0;
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(brush) then
    nBrush := brush.nativeBrush
  else
    nBrush := nil;
  Result := SetStatus(GdipDrawString(nativeGraphics, PWideChar(string_),
      length, nFont, @rect, nil, nBrush));
end;

function TGPGraphics.DrawString(string_: WideString; length: Integer;
  Font: TGPFont; const origin: TGPPointF; stringFormat: TGPStringFormat;
  brush: TGPBrush): TStatus;
var
  rect: TGPRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;
begin
  rect.x := origin.x;
  rect.y := origin.y;
  rect.width := 0.0;
  rect.height := 0.0;
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(stringFormat) then
    nStringFormat := stringFormat.nativeFormat
  else
    nStringFormat := nil;
  if assigned(brush) then
    nBrush := brush.nativeBrush
  else
    nBrush := nil;
  Result := SetStatus(GdipDrawString(nativeGraphics, PWideChar(string_),
      length, nFont, @rect, nStringFormat, nBrush));
end;

function TGPGraphics.MeasureString(string_: WideString; length: Integer;
  Font: TGPFont; const layoutRect: TGPRectF; stringFormat: TGPStringFormat;
  out boundingBox: TGPRectF; codepointsFitted: PInteger = nil;
  linesFilled: PInteger = nil): TStatus;
var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(stringFormat) then
    nStringFormat := stringFormat.nativeFormat
  else
    nStringFormat := nil;
  Result := SetStatus(GdipMeasureString(nativeGraphics, PWideChar(string_),
      length, nFont, @layoutRect, nStringFormat, @boundingBox,
      codepointsFitted, linesFilled));
end;

function TGPGraphics.MeasureString(string_: WideString; length: Integer;
  Font: TGPFont; const layoutRectSize: TGPSizeF;
  stringFormat: TGPStringFormat; out size: TGPSizeF;
  codepointsFitted: PInteger = nil;
  linesFilled: PInteger = nil): TStatus;
var
  layoutRect, boundingBox: TGPRectF;
  status: TStatus;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  layoutRect.x := 0;
  layoutRect.y := 0;
  layoutRect.width := layoutRectSize.width;
  layoutRect.height := layoutRectSize.height;

  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(stringFormat) then
    nStringFormat := stringFormat.nativeFormat
  else
    nStringFormat := nil;

  status := SetStatus(GdipMeasureString(nativeGraphics, PWideChar(string_),
      length, nFont, @layoutRect, nStringFormat, @boundingBox,
      codepointsFitted, linesFilled));

  if (status = Ok) then
  begin
    size.width := boundingBox.width;
    size.height := boundingBox.height;
  end;
  Result := status;
end;

function TGPGraphics.MeasureString(string_: WideString; length: Integer;
  Font: TGPFont; const origin: TGPPointF; stringFormat: TGPStringFormat;
  out boundingBox: TGPRectF): TStatus;
var
  rect: TGPRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  rect.x := origin.x;
  rect.y := origin.y;
  rect.width := 0.0;
  rect.height := 0.0;

  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(stringFormat) then
    nStringFormat := stringFormat.nativeFormat
  else
    nStringFormat := nil;

  Result := SetStatus(GdipMeasureString(nativeGraphics, PWideChar(string_),
      length, nFont, @rect, nStringFormat, @boundingBox, nil, nil));
end;

function TGPGraphics.MeasureString(string_: WideString; length: Integer;
  Font: TGPFont; const layoutRect: TGPRectF; out boundingBox: TGPRectF)
  : TStatus;
var
  nFont: GpFont;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  Result := SetStatus(GdipMeasureString(nativeGraphics, PWideChar(string_),
      length, nFont, @layoutRect, nil, @boundingBox, nil, nil));
end;

function TGPGraphics.MeasureString(string_: WideString; length: Integer;
  Font: TGPFont; const origin: TGPPointF; out boundingBox: TGPRectF): TStatus;
var
  nFont: GpFont;
  rect: TGPRectF;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  rect.x := origin.x;
  rect.y := origin.y;
  rect.width := 0.0;
  rect.height := 0.0;

  Result := SetStatus(GdipMeasureString(nativeGraphics, PWideChar(string_),
      length, nFont, @rect, nil, @boundingBox, nil, nil));
end;

function TGPGraphics.MeasureCharacterRanges(string_: WideString;
  length: Integer; Font: TGPFont; const layoutRect: TGPRectF;
  stringFormat: TGPStringFormat; regionCount: Integer;
  const regions: array of TGPRegion): TStatus;
var
  nativeRegions: Pointer;
  i: Integer;
  status: TStatus;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
type
  TArrayGpRegion = array of GpRegion;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(stringFormat) then
    nStringFormat := stringFormat.nativeFormat
  else
    nStringFormat := nil;

  if (regionCount <= 0) then
  begin
    Result := InvalidParameter;
    Exit;
  end;

  getmem(nativeRegions, sizeof(GpRegion) * regionCount);

  for i := 0 to regionCount - 1 do
    TArrayGpRegion(nativeRegions)[i] := regions[i].nativeRegion;

  status := SetStatus(GdipMeasureCharacterRanges(nativeGraphics,
      PWideChar(string_), length, nFont, @layoutRect, nStringFormat,
      regionCount, nativeRegions));

  freemem(nativeRegions, sizeof(GpRegion) * regionCount);
  Result := status;
end;

function TGPGraphics.DrawDriverString(text: PUINT16; length: Integer;
  Font: TGPFont; brush: TGPBrush; positions: PGPPointF; flags: Integer;
  matrix: TGPMatrix): TStatus;
var
  nFont: GpFont;
  nBrush: GpBrush;
  nmatrix: GpMatrix;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(brush) then
    nBrush := brush.nativeBrush
  else
    nBrush := nil;
  if assigned(matrix) then
    nmatrix := matrix.nativeMatrix
  else
    nmatrix := nil;

  Result := SetStatus(GdipDrawDriverString(nativeGraphics, text, length, nFont,
      nBrush, positions, flags, nmatrix));
end;

function TGPGraphics.MeasureDriverString(text: PUINT16; length: Integer;
  Font: TGPFont; positions: PGPPointF; flags: Integer; matrix: TGPMatrix;
  out boundingBox: TGPRectF): TStatus;
var
  nFont: GpFont;
  nmatrix: GpMatrix;
begin
  if assigned(Font) then
    nFont := Font.nativeFont
  else
    nFont := nil;
  if assigned(matrix) then
    nmatrix := matrix.nativeMatrix
  else
    nmatrix := nil;

  Result := SetStatus(GdipMeasureDriverString(nativeGraphics, text, length,
      nFont, positions, flags, nmatrix, @boundingBox));
end;

// Draw a cached bitmap on this graphics destination offset by
// x, y. Note this will fail with WrongState if the CachedBitmap
// native format differs from this Graphics.

function TGPGraphics.DrawCachedBitmap(cb: TGPCachedBitmap;
  x, y: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawCachedBitmap(nativeGraphics,
      cb.nativeCachedBitmap, x, y));
end;

function TGPGraphics.DrawImage(image: TGPImage;
  const point: TGPPointF): TStatus;
begin
  Result := DrawImage(image, point.x, point.y);
end;

function TGPGraphics.DrawImage(image: TGPImage; x, y: Single): TStatus;
var
  nImage: GpImage;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImage(nativeGraphics, nImage, x, y));
end;

function TGPGraphics.DrawImage(image: TGPImage; const rect: TGPRectF): TStatus;
begin
  Result := DrawImage(image, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.DrawImage(image: TGPImage; x, y, width, height: Single)
  : TStatus;
var
  nImage: GpImage;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImageRect(nativeGraphics, nImage, x, y, width,
      height));
end;

function TGPGraphics.DrawImage(image: TGPImage; const point: TGPPoint): TStatus;
begin
  Result := DrawImage(image, point.x, point.y);
end;

function TGPGraphics.DrawImage(image: TGPImage; x, y: Integer): TStatus;
var
  nImage: GpImage;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImageI(nativeGraphics, nImage, x, y));
end;

function TGPGraphics.DrawImage(image: TGPImage; const rect: TGPRect): TStatus;
begin
  Result := DrawImage(image, rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphics.DrawImage(image: TGPImage; x, y, width, height: Integer)
  : TStatus;
var
  nImage: GpImage;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImageRectI(nativeGraphics, nImage, x, y, width,
      height));
end;


// Affine Draw Image
// destPoints.length = 3: rect => parallelogram
// destPoints[0] <=> top-left corner of the source rectangle
// destPoints[1] <=> top-right corner
// destPoints[2] <=> bottom-left corner
// destPoints.length = 4: rect => quad
// destPoints[3] <=> bottom-right corner

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPointF;
  count: Integer): TStatus;
var
  nImage: GpImage;
begin
  if (((count <> 3) and (count <> 4)) or (destPoints = nil)) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImagePoints(nativeGraphics, nImage, destPoints,
      count));
end;

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPoint;
  count: Integer): TStatus;
var
  nImage: GpImage;
begin
  if (((count <> 3) and (count <> 4)) or (destPoints = nil)) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;

  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImagePointsI(nativeGraphics, nImage, destPoints,
      count));
end;

function TGPGraphics.DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth,
  srcheight: Single; srcUnit: TUnit): TStatus;
var
  nImage: GpImage;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImagePointRect(nativeGraphics, nImage, x, y,
      srcx, srcy, srcwidth, srcheight, srcUnit));
end;

function TGPGraphics.DrawImage(image: TGPImage; const destRect: TGPRectF;
  srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
  imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage: GpImage;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipDrawImageRectRect(nativeGraphics, nImage, destRect.x,
      destRect.y, destRect.width, destRect.height, srcx, srcy, srcwidth,
      srcheight, srcUnit, nimageAttributes, callback, callbackData));
end;

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPointF;
  count: Integer; srcx, srcy, srcwidth, srcheight: Single; srcUnit: TUnit;
  imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage: GpImage;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipDrawImagePointsRect(nativeGraphics, nImage,
      destPoints, count, srcx, srcy, srcwidth, srcheight, srcUnit,
      nimageAttributes, callback, callbackData));
end;

function TGPGraphics.DrawImage(image: TGPImage; x, y, srcx, srcy, srcwidth,
  srcheight: Integer; srcUnit: TUnit): TStatus;
var
  nImage: GpImage;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  Result := SetStatus(GdipDrawImagePointRectI(nativeGraphics, nImage, x, y,
      srcx, srcy, srcwidth, srcheight, srcUnit));
end;

function TGPGraphics.DrawImage(image: TGPImage; const destRect: TGPRect;
  srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
  imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage: GpImage;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipDrawImageRectRectI(nativeGraphics, nImage,
      destRect.x, destRect.y, destRect.width, destRect.height, srcx, srcy,
      srcwidth, srcheight, srcUnit, nimageAttributes, callback, callbackData));
end;

function TGPGraphics.DrawImage(image: TGPImage; destPoints: PGPPoint;
  count, srcx, srcy, srcwidth, srcheight: Integer; srcUnit: TUnit;
  imageAttributes: TGPImageAttributes = nil; callback: DrawImageAbort = nil;
  callbackData: Pointer = nil): TStatus;
var
  nImage: GpImage;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(image) then
    nImage := image.nativeImage
  else
    nImage := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;

  Result := SetStatus(GdipDrawImagePointsRectI(nativeGraphics, nImage,
      destPoints, count, srcx, srcy, srcwidth, srcheight, srcUnit,
      nimageAttributes, callback, callbackData));
end;

// The following methods are for playing an EMF+ to a graphics
// via the enumeration interface.  Each record of the EMF+ is
// sent to the callback (along with the callbackData).  Then
// the callback can invoke the Metafile::PlayRecord method
// to play the particular record.

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destPoint: TGPPointF; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPoint(nativeGraphics, nMetafile,
      @destPoint, callback, callbackData, nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destPoint: TGPPoint; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPointI(nativeGraphics,
      nMetafile, @destPoint, callback, callbackData, nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destRect: TGPRectF; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestRect(nativeGraphics, nMetafile,
      @destRect, callback, callbackData, nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destRect: TGPRect; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestRectI(nativeGraphics, nMetafile,
      @destRect, callback, callbackData, nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  destPoints: PGPPointF; count: Integer; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPoints(nativeGraphics,
      nMetafile, destPoints, count, callback, callbackData, nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  destPoints: PGPPoint; count: Integer; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileDestPointsI(nativeGraphics,
      nMetafile, destPoints, count, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destPoint: TGPPointF; const srcRect: TGPRectF; srcUnit: TUnit;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoint(nativeGraphics,
      nMetafile, @destPoint, @srcRect, srcUnit, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destPoint: TGPPoint; const srcRect: TGPRect; srcUnit: TUnit;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointI(nativeGraphics,
      nMetafile, @destPoint, @srcRect, srcUnit, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destRect: TGPRectF; const srcRect: TGPRectF; srcUnit: TUnit;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestRect(nativeGraphics,
      nMetafile, @destRect, @srcRect, srcUnit, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  const destRect, srcRect: TGPRect; srcUnit: TUnit;
  callback: EnumerateMetafileProc; callbackData: Pointer = nil;
  imageAttributes: TGPImageAttributes = nil): TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestRectI(nativeGraphics,
      nMetafile, @destRect, @srcRect, srcUnit, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  destPoints: PGPPointF; count: Integer; const srcRect: TGPRectF;
  srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoints(nativeGraphics,
      nMetafile, destPoints, count, @srcRect, srcUnit, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.EnumerateMetafile(metafile: TGPMetafile;
  destPoints: PGPPoint; count: Integer; const srcRect: TGPRect;
  srcUnit: TUnit; callback: EnumerateMetafileProc;
  callbackData: Pointer = nil; imageAttributes: TGPImageAttributes = nil)
  : TStatus;
var
  nMetafile: GpMetafile;
  nimageAttributes: GpImageAttributes;
begin
  if assigned(metafile) then
    nMetafile := GpMetafile(metafile.nativeImage)
  else
    nMetafile := nil;
  if assigned(imageAttributes) then
    nimageAttributes := imageAttributes.nativeImageAttr
  else
    nimageAttributes := nil;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointsI(nativeGraphics,
      nMetafile, destPoints, count, @srcRect, srcUnit, callback, callbackData,
      nimageAttributes));
end;

function TGPGraphics.SetClip(g: TGPGraphics;
  combineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipGraphics(nativeGraphics, g.nativeGraphics,
      combineMode));
end;

function TGPGraphics.SetClip(rect: TGPRectF;
  combineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(nativeGraphics, rect.x, rect.y,
      rect.width, rect.height, combineMode));
end;

function TGPGraphics.SetClip(rect: TGPRect;
  combineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(nativeGraphics, rect.x, rect.y,
      rect.width, rect.height, combineMode));
end;

function TGPGraphics.SetClip(path: TGPGraphicsPath;
  combineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipPath(nativeGraphics, path.nativePath,
      combineMode));
end;

function TGPGraphics.SetClip(region: TGPRegion;
  combineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(nativeGraphics, region.nativeRegion,
      combineMode));
end;

// This is different than the other SetClip methods because it assumes
// that the HRGN is already in device units, so it doesn't transform
// the coordinates in the HRGN.

function TGPGraphics.SetClip(hRgn: hRgn;
  combineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipHrgn(nativeGraphics, hRgn, combineMode));
end;

function TGPGraphics.IntersectClip(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(nativeGraphics, rect.x, rect.y,
      rect.width, rect.height, CombineModeIntersect));
end;

function TGPGraphics.IntersectClip(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(nativeGraphics, rect.x, rect.y,
      rect.width, rect.height, CombineModeIntersect));
end;

function TGPGraphics.IntersectClip(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(nativeGraphics, region.nativeRegion,
      CombineModeIntersect));
end;

function TGPGraphics.ExcludeClip(const rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(nativeGraphics, rect.x, rect.y,
      rect.width, rect.height, CombineModeExclude));
end;

function TGPGraphics.ExcludeClip(const rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(nativeGraphics, rect.x, rect.y,
      rect.width, rect.height, CombineModeExclude));
end;

function TGPGraphics.ExcludeClip(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(nativeGraphics, region.nativeRegion,
      CombineModeExclude));
end;

function TGPGraphics.ResetClip: TStatus;
begin
  Result := SetStatus(GdipResetClip(nativeGraphics));
end;

function TGPGraphics.TranslateClip(dx, dy: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateClip(nativeGraphics, dx, dy));
end;

function TGPGraphics.TranslateClip(dx, dy: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateClipI(nativeGraphics, dx, dy));
end;

function TGPGraphics.GetClip(region: TGPRegion): TStatus;
begin
  Result := SetStatus(GdipGetClip(nativeGraphics, region.nativeRegion));
end;

function TGPGraphics.GetClipBounds(out rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetClipBounds(nativeGraphics, @rect));
end;

function TGPGraphics.GetClipBounds(out rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetClipBoundsI(nativeGraphics, @rect));
end;

function TGPGraphics.IsClipEmpty: BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsClipEmpty(nativeGraphics, @booln));
  Result := booln;
end;

function TGPGraphics.GetVisibleClipBounds(out rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBounds(nativeGraphics, @rect));
end;

function TGPGraphics.GetVisibleClipBounds(out rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBoundsI(nativeGraphics, @rect));
end;

function TGPGraphics.IsVisibleClipEmpty: BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsVisibleClipEmpty(nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(x, y: Integer): BOOL;
var
  pt: TGPPoint;
begin
  pt.x := x;
  pt.y := y;
  Result := IsVisible(pt);
end;

function TGPGraphics.IsVisible(const point: TGPPoint): BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsVisiblePointI(nativeGraphics, point.x, point.y, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(x, y, width, height: Integer): BOOL;
var
  booln: BOOL;
begin
  booln := TRUE;
  SetStatus(GdipIsVisibleRectI(nativeGraphics, x, y, width, height, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(const rect: TGPRect): BOOL;
var
  booln: BOOL;
begin
  booln := TRUE;
  SetStatus(GdipIsVisibleRectI(nativeGraphics, rect.x, rect.y, rect.width,
      rect.height, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(x, y: Single): BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsVisiblePoint(nativeGraphics, x, y, booln));

  Result := booln;
end;

function TGPGraphics.IsVisible(const point: TGPPointF): BOOL;
var
  booln: BOOL;
begin
  booln := FALSE;
  SetStatus(GdipIsVisiblePoint(nativeGraphics, point.x, point.y, booln));

  Result := booln;
end;

function TGPGraphics.IsVisible(x, y, width, height: Single): BOOL;
var
  booln: BOOL;
begin
  booln := TRUE;
  SetStatus(GdipIsVisibleRect(nativeGraphics, x, y, width, height, booln));
  Result := booln;
end;

function TGPGraphics.IsVisible(const rect: TGPRectF): BOOL;
var
  booln: BOOL;
begin
  booln := TRUE;
  SetStatus(GdipIsVisibleRect(nativeGraphics, rect.x, rect.y, rect.width,
      rect.height, booln));
  Result := booln;
end;

function TGPGraphics.Save: GraphicsState;
begin
  SetStatus(GdipSaveGraphics(nativeGraphics, Result));
end;

function TGPGraphics.Restore(gstate: GraphicsState): TStatus;
begin
  Result := SetStatus(GdipRestoreGraphics(nativeGraphics, gstate));
end;

function TGPGraphics.BeginContainer(const dstRect, srcRect: TGPRectF;
  unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainer(nativeGraphics, @dstRect, @srcRect, unit_,
      Result));
end;

function TGPGraphics.BeginContainer(const dstRect, srcRect: TGPRect;
  unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainerI(nativeGraphics, @dstRect, @srcRect, unit_,
      Result));
end;

function TGPGraphics.BeginContainer: GraphicsContainer;
begin
  SetStatus(GdipBeginContainer2(nativeGraphics, Result));
end;

function TGPGraphics.EndContainer(state: GraphicsContainer): TStatus;
begin
  Result := SetStatus(GdipEndContainer(nativeGraphics, state));
end;

// Only valid when recording metafiles.

function TGPGraphics.AddMetafileComment(data: PBYTE; sizeData: UINT): TStatus;
begin
  Result := SetStatus(GdipComment(nativeGraphics, sizeData, data));
end;

function TGPGraphics.GetHalftonePalette: HPALETTE;
begin
  Result := GdipCreateHalftonePalette;
end;

function TGPGraphics.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

constructor TGPGraphics.Create(graphics: GpGraphics);
begin
  lastResult := Ok;
  SetNativeGraphics(graphics);
end;

procedure TGPGraphics.SetNativeGraphics(graphics: GpGraphics);
begin
  self.nativeGraphics := graphics;
end;

function TGPGraphics.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

function TGPGraphics.GetNativeGraphics: GpGraphics;
begin
  Result := self.nativeGraphics;
end;

function TGPGraphics.GetNativePen(pen: TGPPen): GpPen;
begin
  Result := pen.nativePen;
end;

(* *************************************************************************\
  *
  *   GDI+ Font Family class
  *
  \************************************************************************* *)

constructor TGPFontFamily.Create;
begin
  nativeFamily := nil;
  lastResult := Ok;
end;

constructor TGPFontFamily.Create(name: WideString;
  fontCollection: TGPFontCollection = nil);
var
  nfontCollection: GpFontCollection;
begin
  nativeFamily := nil;
  if assigned(fontCollection) then
    nfontCollection := fontCollection.nativeFontCollection
  else
    nfontCollection := nil;
  lastResult := GdipCreateFontFamilyFromName(PWideChar(name), nfontCollection,
    nativeFamily);
end;

destructor TGPFontFamily.Destroy;
begin
  GdipDeleteFontFamily(nativeFamily);
end;

class function TGPFontFamily.GenericSansSerif: TGPFontFamily;
var
  nFontFamily: GpFontFamily;
begin
  if (GenericSansSerifFontFamily <> nil) then
  begin
    Result := GenericSansSerifFontFamily;
    Exit;
  end;
  GenericSansSerifFontFamily := TGPFontFamily.Create;
  GenericSansSerifFontFamily.lastResult := GdipGetGenericFontFamilySansSerif
    (nFontFamily);
  GenericSansSerifFontFamily.nativeFamily := nFontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TGPFontFamily.GenericSerif: TGPFontFamily;
var
  nFontFamily: GpFontFamily;
begin
  if (GenericSerifFontFamily <> nil) then
  begin
    Result := GenericSerifFontFamily;
    Exit;
  end;

  GenericSerifFontFamily := TGPFontFamily.Create;
  // (GenericSerifFontFamilyBuffer);
  GenericSerifFontFamily.lastResult := GdipGetGenericFontFamilySerif
    (nFontFamily);
  GenericSerifFontFamily.nativeFamily := nFontFamily;
  Result := GenericSerifFontFamily;
end;

class function TGPFontFamily.GenericMonospace: TGPFontFamily;
var
  nFontFamily: GpFontFamily;
begin
  if (GenericMonospaceFontFamily <> nil) then
  begin
    Result := GenericMonospaceFontFamily;
    Exit;
  end;
  GenericMonospaceFontFamily := TGPFontFamily.Create;
  // (GenericMonospaceFontFamilyBuffer);
  GenericMonospaceFontFamily.lastResult := GdipGetGenericFontFamilyMonospace
    (nFontFamily);
  GenericMonospaceFontFamily.nativeFamily := nFontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TGPFontFamily.GetFamilyName(out name: string;
  language: LANGID = 0): TStatus;
var
  str: array [0 .. LF_FACESIZE - 1] of WideChar;
begin
  Result := SetStatus(GdipGetFamilyName(nativeFamily, @str, language));
  name := str;
end;

function TGPFontFamily.Clone: TGPFontFamily;
var
  clonedFamily: GpFontFamily;
begin
  clonedFamily := nil;
  SetStatus(GdipCloneFontFamily(nativeFamily, clonedFamily));
  Result := TGPFontFamily.Create(clonedFamily, lastResult);
end;

function TGPFontFamily.IsAvailable: BOOL;
begin
  Result := (nativeFamily <> nil);
end;

function TGPFontFamily.IsStyleAvailable(style: Integer): BOOL;
var
  StyleAvailable: BOOL;
  status: TStatus;
begin
  status := SetStatus(GdipIsStyleAvailable(nativeFamily, style,
      StyleAvailable));
  if (status <> Ok) then
    StyleAvailable := FALSE;
  Result := StyleAvailable;
end;

function TGPFontFamily.GetEmHeight(style: Integer): UINT16;
begin
  SetStatus(GdipGetEmHeight(nativeFamily, style, Result));
end;

function TGPFontFamily.GetCellAscent(style: Integer): UINT16;
begin
  SetStatus(GdipGetCellAscent(nativeFamily, style, Result));
end;

function TGPFontFamily.GetCellDescent(style: Integer): UINT16;
begin
  SetStatus(GdipGetCellDescent(nativeFamily, style, Result));
end;

function TGPFontFamily.GetLineSpacing(style: Integer): UINT16;
begin
  SetStatus(GdipGetLineSpacing(nativeFamily, style, Result));
end;

function TGPFontFamily.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

function TGPFontFamily.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

constructor TGPFontFamily.Create(nativeOrig: GpFontFamily; status: TStatus);
begin
  lastResult := status;
  nativeFamily := nativeOrig;
end;

(* *************************************************************************\
  *
  *   GDI+ Font class
  *
  \************************************************************************* *)

constructor TGPFont.Create(hdc: hdc);
var
  Font: GpFont;
begin
  Font := nil;
  lastResult := GdipCreateFontFromDC(hdc, Font);
  SetNativeFont(Font);
end;

constructor TGPFont.Create(hdc: hdc; logfont: PLogFontA);
var
  Font: GpFont;
begin
  Font := nil;
  if assigned(logfont) then
    lastResult := GdipCreateFontFromLogfontA(hdc, logfont, Font)
  else
    lastResult := GdipCreateFontFromDC(hdc, Font);
  SetNativeFont(Font);
end;

constructor TGPFont.Create(hdc: hdc; logfont: PLogFontW);
var
  Font: GpFont;
begin
  Font := nil;
  if assigned(logfont) then
    lastResult := GdipCreateFontFromLogfontW(hdc, logfont, Font)
  else
    lastResult := GdipCreateFontFromDC(hdc, Font);
  SetNativeFont(Font);
end;

constructor TGPFont.Create(hdc: hdc; hfont: hfont);
var
  Font: GpFont;
  lf: logfontA;
begin
  Font := nil;
  if BOOL(hfont) then
  begin
    if (BOOL(GetObjectA(hfont, sizeof(logfontA), @lf))) then
      lastResult := GdipCreateFontFromLogfontA(hdc, @lf, Font)
    else
      lastResult := GdipCreateFontFromDC(hdc, Font);
  end
  else
    lastResult := GdipCreateFontFromDC(hdc, Font);
  SetNativeFont(Font);
end;

constructor TGPFont.Create(family: TGPFontFamily; emSize: Single;
  style: TFontStyle = FontStyleRegular; unit_: TUnit = UnitPoint);
var
  Font: GpFont;
  nFontFamily: GpFontFamily;
begin
  Font := nil;
  if assigned(family) then
    nFontFamily := family.nativeFamily
  else
    nFontFamily := nil;
  lastResult := GdipCreateFont(nFontFamily, emSize, Integer(style),
    Integer(unit_), Font);
  SetNativeFont(Font);
end;

constructor TGPFont.Create(familyName: WideString; emSize: Single;
  style: TFontStyle = FontStyleRegular; unit_: TUnit = UnitPoint;
  fontCollection: TGPFontCollection = nil);
var
  family: TGPFontFamily;
  nativeFamily: GpFontFamily;
begin
  nativeFont := nil;
  family := TGPFontFamily.Create(familyName, fontCollection);
  nativeFamily := family.nativeFamily;
  lastResult := family.GetLastStatus;
  if (lastResult <> Ok) then
  begin
    nativeFamily := TGPFontFamily.GenericSansSerif.nativeFamily;
    lastResult := TGPFontFamily.GenericSansSerif.lastResult;
    if (lastResult <> Ok) then
    begin
      family.Free;
      Exit;
    end;
  end;

  lastResult := GdipCreateFont(nativeFamily, emSize, Integer(style),
    Integer(unit_), nativeFont);

  if (lastResult <> Ok) then
  begin
    nativeFamily := TGPFontFamily.GenericSansSerif.nativeFamily;
    lastResult := TGPFontFamily.GenericSansSerif.lastResult;
    if (lastResult <> Ok) then
    begin
      family.Free;
      Exit;
    end;

    lastResult := GdipCreateFont(nativeFamily, emSize, Integer(style),
      Integer(unit_), nativeFont);
  end;
  family.Free;
end;

function TGPFont.GetLogFontA(g: TGPGraphics; out logfontA: TLogFontA): TStatus;
var
  nGraphics: GpGraphics;
begin
  if assigned(g) then
    nGraphics := g.nativeGraphics
  else
    nGraphics := nil;
  Result := SetStatus(GdipGetLogFontA(nativeFont, nGraphics, logfontA));
end;

function TGPFont.GetLogFontW(g: TGPGraphics; out logfontW: TLogFontW): TStatus;
var
  nGraphics: GpGraphics;
begin
  if assigned(g) then
    nGraphics := g.nativeGraphics
  else
    nGraphics := nil;
  Result := SetStatus(GdipGetLogFontW(nativeFont, nGraphics, logfontW));
end;

function TGPFont.Clone: TGPFont;
var
  cloneFont: GpFont;
begin
  cloneFont := nil;
  SetStatus(GdipCloneFont(nativeFont, cloneFont));
  Result := TGPFont.Create(cloneFont, lastResult);
end;

destructor TGPFont.Destroy;
begin
  GdipDeleteFont(nativeFont);
end;

function TGPFont.IsAvailable: BOOL;
begin
  Result := (nativeFont <> nil);
end;

function TGPFont.GetStyle: Integer;
begin
  SetStatus(GdipGetFontStyle(nativeFont, Result));
end;

function TGPFont.GetSize: Single;
begin
  SetStatus(GdipGetFontSize(nativeFont, Result));
end;

function TGPFont.GetUnit: TUnit;
begin
  SetStatus(GdipGetFontUnit(nativeFont, Result));
end;

function TGPFont.GetLastStatus: TStatus;
begin
  Result := lastResult;
end;

function TGPFont.GetHeight(graphics: TGPGraphics): Single;
var
  nGraphics: GpGraphics;
begin
  if assigned(graphics) then
    nGraphics := graphics.nativeGraphics
  else
    nGraphics := nil;
  SetStatus(GdipGetFontHeight(nativeFont, nGraphics, Result));
end;

function TGPFont.GetHeight(dpi: Single): Single;
begin
  SetStatus(GdipGetFontHeightGivenDPI(nativeFont, dpi, Result));
end;

function TGPFont.GetFamily(family: TGPFontFamily): TStatus;
var
  status: TStatus;
  nFamily: GpFontFamily;
begin
  if (family = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;

  status := GdipGetFamily(nativeFont, nFamily);
  family.nativeFamily := nFamily;
  family.SetStatus(status);
  Result := SetStatus(status);
end;

constructor TGPFont.Create(Font: GpFont; status: TStatus);
begin
  lastResult := status;
  SetNativeFont(Font);
end;

procedure TGPFont.SetNativeFont(Font: GpFont);
begin
  nativeFont := Font;
end;

function TGPFont.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

(* *************************************************************************\
  *
  *   Font collections (Installed and Private)
  *
  \************************************************************************* *)

constructor TGPFontCollection.Create;
begin
  nativeFontCollection := nil;
end;

destructor TGPFontCollection.Destroy;
begin
  inherited Destroy;
end;

function TGPFontCollection.GetFamilyCount: Integer;
var
  numFound: Integer;
begin
  numFound := 0;
  lastResult := GdipGetFontCollectionFamilyCount(nativeFontCollection,
    numFound);
  Result := numFound;
end;

function TGPFontCollection.GetFamilies(numSought: Integer;
  out gpfamilies: array of TGPFontFamily; out numFound: Integer): TStatus;
var
  nativeFamilyList: Pointer;
  status: TStatus;
  i: Integer;
type
  ArrGpFontFamily = array of GpFontFamily;
begin
  if ((numSought <= 0) or (length(gpfamilies) = 0)) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  numFound := 0;

  getmem(nativeFamilyList, numSought * sizeof(GpFontFamily));
  if nativeFamilyList = nil then
  begin
    Result := SetStatus(OutOfMemory);
    Exit;
  end;

  status := SetStatus(GdipGetFontCollectionFamilyList(nativeFontCollection,
      numSought, nativeFamilyList, numFound));

  if (status = Ok) then
    for i := 0 to numFound - 1 do
      GdipCloneFontFamily(ArrGpFontFamily(nativeFamilyList)[i],
        gpfamilies[i].nativeFamily);
  freemem(nativeFamilyList, numSought * sizeof(GpFontFamily));
  Result := status;
end;

function TGPFontCollection.GetLastStatus: TStatus;
begin
  Result := lastResult;
end;

function TGPFontCollection.SetStatus(status: TStatus): TStatus;
begin
  lastResult := status;
  Result := lastResult;
end;

constructor TGPInstalledFontCollection.Create;
begin
  nativeFontCollection := nil;
  lastResult := GdipNewInstalledFontCollection(nativeFontCollection);
end;

destructor TGPInstalledFontCollection.Destroy;
begin
  inherited Destroy;
end;

constructor TGPPrivateFontCollection.Create;
begin
  nativeFontCollection := nil;
  lastResult := GdipNewPrivateFontCollection(nativeFontCollection);
end;

destructor TGPPrivateFontCollection.Destroy;
begin
  GdipDeletePrivateFontCollection(nativeFontCollection);
  inherited Destroy;
end;

function TGPPrivateFontCollection.AddFontFile(filename: WideString): TStatus;
begin
  Result := SetStatus(GdipPrivateAddFontFile(nativeFontCollection,
      PWideChar(filename)));
end;

function TGPPrivateFontCollection.AddMemoryFont(memory: Pointer;
  length: Integer): TStatus;
begin
  Result := SetStatus(GdipPrivateAddMemoryFont(nativeFontCollection, memory,
      length));
end;

(* *************************************************************************\
  *
  *   GDI+ Graphics Path class
  *
  \************************************************************************* *)

constructor TGPGraphicsPath.Create(fillMode: TFillMode = FillModeAlternate);
begin
  nativePath := nil;
  lastResult := GdipCreatePath(fillMode, nativePath);
end;

constructor TGPGraphicsPath.Create(points: PGPPointF; types: PBYTE;
  count: Integer; fillMode: TFillMode = FillModeAlternate);
begin
  nativePath := nil;
  lastResult := GdipCreatePath2(points, types, count, fillMode, nativePath);
end;

constructor TGPGraphicsPath.Create(points: PGPPoint; types: PBYTE;
  count: Integer; fillMode: TFillMode = FillModeAlternate);
begin
  nativePath := nil;
  lastResult := GdipCreatePath2I(points, types, count, fillMode, nativePath);
end;

destructor TGPGraphicsPath.Destroy;
begin
  GdipDeletePath(nativePath);
end;

function TGPGraphicsPath.Clone: TGPGraphicsPath;
var
  clonepath: GpPath;
begin
  clonepath := nil;
  SetStatus(GdipClonePath(nativePath, clonepath));
  Result := TGPGraphicsPath.Create(clonepath);
end;

// Reset the path object to empty (and fill mode to FillModeAlternate)

function TGPGraphicsPath.Reset: TStatus;
begin
  Result := SetStatus(GdipResetPath(nativePath));
end;

function TGPGraphicsPath.GetFillMode: TFillMode;
var
  FMode: TFillMode;
begin
  FMode := FillModeAlternate;
  SetStatus(GdipGetPathFillMode(nativePath, Result));
  Result := FMode;
end;

function TGPGraphicsPath.SetFillMode(fillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipSetPathFillMode(nativePath, fillMode));
end;

function TGPGraphicsPath.GetPathData(pathData: TPathData): TStatus;
var
  count: Integer;
begin
  count := GetPointCount;
  if ((count <= 0) or ((pathData.count > 0) and (pathData.count < count))) then
  begin
    pathData.count := 0;
    if assigned(pathData.points) then
    begin
      freemem(pathData.points);
      pathData.points := nil;
    end;
    if assigned(pathData.types) then
    begin
      freemem(pathData.types);
      pathData.types := nil;
    end;
    if (count <= 0) then
    begin
      Result := lastResult;
      Exit;
    end;
  end;

  if (pathData.count = 0) then
  begin
    getmem(pathData.points, sizeof(TGPPointF) * count);
    if (pathData.points = nil) then
    begin
      Result := SetStatus(OutOfMemory);
      Exit;
    end;
    getmem(pathData.types, count);
    if (pathData.types = nil) then
    begin
      freemem(pathData.points);
      pathData.points := nil;
      Result := SetStatus(OutOfMemory);
      Exit;
    end;
    pathData.count := count;
  end;

  Result := SetStatus(GdipGetPathData(nativePath, @pathData.count));
end;

function TGPGraphicsPath.StartFigure: TStatus;
begin
  Result := SetStatus(GdipStartPathFigure(nativePath));
end;

function TGPGraphicsPath.CloseFigure: TStatus;
begin
  Result := SetStatus(GdipClosePathFigure(nativePath));
end;

function TGPGraphicsPath.CloseAllFigures: TStatus;
begin
  Result := SetStatus(GdipClosePathFigures(nativePath));
end;

function TGPGraphicsPath.SetMarker: TStatus;
begin
  Result := SetStatus(GdipSetPathMarker(nativePath));
end;

function TGPGraphicsPath.ClearMarkers: TStatus;
begin
  Result := SetStatus(GdipClearPathMarkers(nativePath));
end;

function TGPGraphicsPath.Reverse: TStatus;
begin
  Result := SetStatus(GdipReversePath(nativePath));
end;

function TGPGraphicsPath.GetLastPoint(out lastPoint: TGPPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathLastPoint(nativePath, @lastPoint));
end;

function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPointF): TStatus;
begin
  Result := AddLine(pt1.x, pt1.y, pt2.x, pt2.y);
end;

function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathLine(nativePath, x1, y1, x2, y2));
end;

function TGPGraphicsPath.AddLines(points: PGPPointF; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2(nativePath, points, count));
end;

function TGPGraphicsPath.AddLine(const pt1, pt2: TGPPoint): TStatus;
begin
  Result := AddLine(pt1.x, pt1.y, pt2.x, pt2.y);
end;

function TGPGraphicsPath.AddLine(x1, y1, x2, y2: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLineI(nativePath, x1, y1, x2, y2));
end;

function TGPGraphicsPath.AddLines(points: PGPPoint; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2I(nativePath, points, count));
end;

function TGPGraphicsPath.AddArc(rect: TGPRectF;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddArc(rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphicsPath.AddArc(x, y, width, height, startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArc(nativePath, x, y, width, height,
      startAngle, sweepAngle));
end;

function TGPGraphicsPath.AddArc(rect: TGPRect;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddArc(rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphicsPath.AddArc(x, y, width, height: Integer;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArcI(nativePath, x, y, width, height,
      startAngle, sweepAngle));
end;

function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPointF): TStatus;
begin
  Result := AddBezier(pt1.x, pt1.y, pt2.x, pt2.y, pt3.x, pt3.y, pt4.x, pt4.y);
end;

function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Single)
  : TStatus;
begin
  Result := SetStatus(GdipAddPathBezier(nativePath, x1, y1, x2, y2, x3, y3, x4,
      y4));
end;

function TGPGraphicsPath.AddBeziers(points: PGPPointF; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziers(nativePath, points, count));
end;

function TGPGraphicsPath.AddBezier(pt1, pt2, pt3, pt4: TGPPoint): TStatus;
begin
  Result := AddBezier(pt1.x, pt1.y, pt2.x, pt2.y, pt3.x, pt3.y, pt4.x, pt4.y);
end;

function TGPGraphicsPath.AddBezier(x1, y1, x2, y2, x3, y3, x4, y4: Integer)
  : TStatus;
begin
  Result := SetStatus(GdipAddPathBezierI(nativePath, x1, y1, x2, y2, x3, y3,
      x4, y4));
end;

function TGPGraphicsPath.AddBeziers(points: PGPPoint; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziersI(nativePath, points, count));
end;

function TGPGraphicsPath.AddCurve(points: PGPPointF; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve(nativePath, points, count));
end;

function TGPGraphicsPath.AddCurve(points: PGPPointF; count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2(nativePath, points, count, tension));
end;

function TGPGraphicsPath.AddCurve(points: PGPPointF; count, offset,
  numberOfSegments: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3(nativePath, points, count, offset,
      numberOfSegments, tension));
end;

function TGPGraphicsPath.AddCurve(points: PGPPoint; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurveI(nativePath, points, count));
end;

function TGPGraphicsPath.AddCurve(points: PGPPoint; count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2I(nativePath, points, count, tension));
end;

function TGPGraphicsPath.AddCurve(points: PGPPoint; count, offset,
  numberOfSegments: Integer; tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3I(nativePath, points, count, offset,
      numberOfSegments, tension));
end;

function TGPGraphicsPath.AddClosedCurve(points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve(nativePath, points, count));
end;

function TGPGraphicsPath.AddClosedCurve(points: PGPPointF; count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2(nativePath, points, count,
      tension));
end;

function TGPGraphicsPath.AddClosedCurve(points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurveI(nativePath, points, count));
end;

function TGPGraphicsPath.AddClosedCurve(points: PGPPoint; count: Integer;
  tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2I(nativePath, points, count,
      tension));
end;

function TGPGraphicsPath.AddRectangle(rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangle(nativePath, rect.x, rect.y,
      rect.width, rect.height));
end;

function TGPGraphicsPath.AddRectangles(rects: PGPRectF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangles(nativePath, rects, count));
end;

function TGPGraphicsPath.AddRectangle(rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangleI(nativePath, rect.x, rect.y,
      rect.width, rect.height));
end;

function TGPGraphicsPath.AddRectangles(rects: PGPRect; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectanglesI(nativePath, rects, count));
end;

function TGPGraphicsPath.AddEllipse(rect: TGPRectF): TStatus;
begin
  Result := AddEllipse(rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphicsPath.AddEllipse(x, y, width, height: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipse(nativePath, x, y, width, height));
end;

function TGPGraphicsPath.AddEllipse(rect: TGPRect): TStatus;
begin
  Result := AddEllipse(rect.x, rect.y, rect.width, rect.height);
end;

function TGPGraphicsPath.AddEllipse(x, y, width, height: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipseI(nativePath, x, y, width, height));
end;

function TGPGraphicsPath.AddPie(rect: TGPRectF;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddPie(rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphicsPath.AddPie(x, y, width, height, startAngle,
  sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPie(nativePath, x, y, width, height,
      startAngle, sweepAngle));
end;

function TGPGraphicsPath.AddPie(rect: TGPRect;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := AddPie(rect.x, rect.y, rect.width, rect.height, startAngle,
    sweepAngle);
end;

function TGPGraphicsPath.AddPie(x, y, width, height: Integer;
  startAngle, sweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPieI(nativePath, x, y, width, height,
      startAngle, sweepAngle));
end;

function TGPGraphicsPath.AddPolygon(points: PGPPointF; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygon(nativePath, points, count));
end;

function TGPGraphicsPath.AddPolygon(points: PGPPoint; count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygonI(nativePath, points, count));
end;

function TGPGraphicsPath.AddPath(addingPath: TGPGraphicsPath;
  connect: BOOL): TStatus;
var
  nativePath2: GpPath;
begin
  nativePath2 := nil;
  if assigned(addingPath) then
    nativePath2 := addingPath.nativePath;
  Result := SetStatus(GdipAddPathPath(nativePath, nativePath2, connect));
end;

function TGPGraphicsPath.AddString(string_: WideString; length: Integer;
  family: TGPFontFamily; style: Integer; emSize: Single; // World units
  origin: TGPPointF; format: TGPStringFormat): TStatus;
var
  rect: TGPRectF;
  gpff: GpFontFamily;
  gpsf: GpStringFormat;
begin
  rect.x := origin.x;
  rect.y := origin.y;
  rect.width := 0.0;
  rect.height := 0.0;

  gpff := nil;
  gpsf := nil;
  if assigned(family) then
    gpff := family.nativeFamily;
  if assigned(format) then
    gpsf := format.nativeFormat;
  Result := SetStatus(GdipAddPathString(nativePath, PWideChar(string_), length,
      gpff, style, emSize, @rect, gpsf));
end;

function TGPGraphicsPath.AddString(string_: WideString; length: Integer;
  family: TGPFontFamily; style: Integer; emSize: Single; // World units
  layoutRect: TGPRectF; format: TGPStringFormat): TStatus;
var
  gpff: GpFontFamily;
  gpsf: GpStringFormat;
begin
  gpff := nil;
  gpsf := nil;
  if assigned(family) then
    gpff := family.nativeFamily;
  if assigned(format) then
    gpsf := format.nativeFormat;
  Result := SetStatus(GdipAddPathString(nativePath, PWideChar(string_), length,
      gpff, style, emSize, @layoutRect, gpsf));
end;

function TGPGraphicsPath.AddString(string_: WideString; length: Integer;
  family: TGPFontFamily; style: Integer; emSize: Single; // World units
  origin: TGPPoint; format: TGPStringFormat): TStatus;
var
  rect: TGPRect;
  gpff: GpFontFamily;
  gpsf: GpStringFormat;
begin
  rect.x := origin.x;
  rect.y := origin.y;
  rect.width := 0;
  rect.height := 0;
  gpff := nil;
  gpsf := nil;
  if assigned(family) then
    gpff := family.nativeFamily;
  if assigned(format) then
    gpsf := format.nativeFormat;
  Result := SetStatus(GdipAddPathStringI(nativePath, PWideChar(string_),
      length, gpff, style, emSize, @rect, gpsf));
end;

function TGPGraphicsPath.AddString(string_: WideString; length: Integer;
  family: TGPFontFamily; style: Integer; emSize: Single; // World units
  layoutRect: TGPRect; format: TGPStringFormat): TStatus;
var
  gpff: GpFontFamily;
  gpsf: GpStringFormat;
begin
  gpff := nil;
  gpsf := nil;
  if assigned(family) then
    gpff := family.nativeFamily;
  if assigned(format) then
    gpsf := format.nativeFormat;
  Result := SetStatus(GdipAddPathStringI(nativePath, PWideChar(string_),
      length, gpff, style, emSize, @layoutRect, gpsf));
end;

function TGPGraphicsPath.Transform(matrix: TGPMatrix): TStatus;
begin
  if assigned(matrix) then
    Result := SetStatus(GdipTransformPath(nativePath, matrix.nativeMatrix))
  else
    Result := Ok;
end;

// This is not always the tightest bounds.

function TGPGraphicsPath.GetBounds(out bounds: TGPRectF;
  matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;
var
  nativeMatrix: GpMatrix;
  nativePen: GpPen;
begin
  nativeMatrix := nil;
  nativePen := nil;
  if assigned(matrix) then
    nativeMatrix := matrix.nativeMatrix;
  if assigned(pen) then
    nativePen := pen.nativePen;

  Result := SetStatus(GdipGetPathWorldBounds(nativePath, @bounds, nativeMatrix,
      nativePen));
end;

function TGPGraphicsPath.GetBounds(out bounds: TGPRect;
  matrix: TGPMatrix = nil; pen: TGPPen = nil): TStatus;
var
  nativeMatrix: GpMatrix;
  nativePen: GpPen;
begin
  nativeMatrix := nil;
  nativePen := nil;
  if assigned(matrix) then
    nativeMatrix := matrix.nativeMatrix;
  if assigned(pen) then
    nativePen := pen.nativePen;

  Result := SetStatus(GdipGetPathWorldBoundsI(nativePath, @bounds,
      nativeMatrix, nativePen));
end;

// Once flattened, the resultant path is made of line segments and
// the original path information is lost.  When matrix is nil the
// identity matrix is assumed.

function TGPGraphicsPath.Flatten(matrix: TGPMatrix = nil;
  flatness: Single = FlatnessDefault): TStatus;
var
  nativeMatrix: GpMatrix;
begin
  nativeMatrix := nil;
  if assigned(matrix) then
    nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipFlattenPath(nativePath, nativeMatrix, flatness));
end;

function TGPGraphicsPath.Widen(pen: TGPPen; matrix: TGPMatrix = nil;
  flatness: Single = FlatnessDefault): TStatus;
var
  nativeMatrix: GpMatrix;
begin
  nativeMatrix := nil;
  if assigned(matrix) then
    nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipWidenPath(nativePath, pen.nativePen, nativeMatrix,
      flatness));
end;

function TGPGraphicsPath.Outline(matrix: TGPMatrix = nil;
  flatness: Single = FlatnessDefault): TStatus;
var
  nativeMatrix: GpMatrix;
begin
  nativeMatrix := nil;
  if assigned(matrix) then
    nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipWindingModeOutline(nativePath, nativeMatrix,
      flatness));
end;

// Once this is called, the resultant path is made of line segments and
// the original path information is lost.  When matrix is nil, the
// identity matrix is assumed.

function TGPGraphicsPath.Warp(destPoints: PGPPointF; count: Integer;
  srcRect: TGPRectF; matrix: TGPMatrix = nil;
  warpMode: TWarpMode = WarpModePerspective;
  flatness: Single = FlatnessDefault): TStatus;
var
  nativeMatrix: GpMatrix;
begin
  nativeMatrix := nil;
  if assigned(matrix) then
    nativeMatrix := matrix.nativeMatrix;
  Result := SetStatus(GdipWarpPath(nativePath, nativeMatrix, destPoints, count,
      srcRect.x, srcRect.y, srcRect.width, srcRect.height, warpMode, flatness));
end;

function TGPGraphicsPath.GetPointCount: Integer;
var
  count: Integer;
begin
  count := 0;
  SetStatus(GdipGetPointCount(nativePath, count));
  Result := count;
end;

function TGPGraphicsPath.GetPathTypes(types: PBYTE; count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathTypes(nativePath, types, count));
end;

function TGPGraphicsPath.GetPathPoints(points: PGPPointF;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPoints(nativePath, points, count));
end;

function TGPGraphicsPath.GetPathPoints(points: PGPPoint;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPointsI(nativePath, points, count));
end;

function TGPGraphicsPath.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

function TGPGraphicsPath.IsVisible(point: TGPPointF;
  g: TGPGraphics = nil): BOOL;
begin
  Result := IsVisible(point.x, point.y, g);
end;

function TGPGraphicsPath.IsVisible(x, y: Single; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
begin
  booln := FALSE;
  nativeGraphics := nil;
  if assigned(g) then
    nativeGraphics := g.nativeGraphics;
  SetStatus(GdipIsVisiblePathPoint(nativePath, x, y, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsVisible(point: TGPPoint; g: TGPGraphics = nil): BOOL;
begin
  Result := IsVisible(point.x, point.y, g);
end;

function TGPGraphicsPath.IsVisible(x, y: Integer; g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
begin
  booln := FALSE;
  nativeGraphics := nil;
  if assigned(g) then
    nativeGraphics := g.nativeGraphics;
  SetStatus(GdipIsVisiblePathPointI(nativePath, x, y, nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsOutlineVisible(point: TGPPointF; pen: TGPPen;
  g: TGPGraphics = nil): BOOL;
begin
  Result := IsOutlineVisible(point.x, point.y, pen, g);
end;

function TGPGraphicsPath.IsOutlineVisible(x, y: Single; pen: TGPPen;
  g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
  nativePen: GpPen;
begin
  booln := FALSE;
  nativeGraphics := nil;
  nativePen := nil;
  if assigned(g) then
    nativeGraphics := g.nativeGraphics;
  if assigned(pen) then
    nativePen := pen.nativePen;
  SetStatus(GdipIsOutlineVisiblePathPoint(nativePath, x, y, nativePen,
      nativeGraphics, booln));
  Result := booln;
end;

function TGPGraphicsPath.IsOutlineVisible(point: TGPPoint; pen: TGPPen;
  g: TGPGraphics = nil): BOOL;
begin
  Result := IsOutlineVisible(point.x, point.y, pen, g);
end;

function TGPGraphicsPath.IsOutlineVisible(x, y: Integer; pen: TGPPen;
  g: TGPGraphics = nil): BOOL;
var
  booln: BOOL;
  nativeGraphics: GpGraphics;
  nativePen: GpPen;
begin
  booln := FALSE;
  nativeGraphics := nil;
  nativePen := nil;
  if assigned(g) then
    nativeGraphics := g.nativeGraphics;
  if assigned(pen) then
    nativePen := pen.nativePen;
  SetStatus(GdipIsOutlineVisiblePathPointI(nativePath, x, y, nativePen,
      nativeGraphics, booln));
  Result := booln;
end;

constructor TGPGraphicsPath.Create(path: TGPGraphicsPath);
var
  clonepath: GpPath;
begin
  clonepath := nil;
  SetStatus(GdipClonePath(path.nativePath, clonepath));
  SetNativePath(clonepath);
end;

constructor TGPGraphicsPath.Create(nativePath: GpPath);
begin
  lastResult := Ok;
  SetNativePath(nativePath);
end;

procedure TGPGraphicsPath.SetNativePath(nativePath: GpPath);
begin
  self.nativePath := nativePath;
end;

function TGPGraphicsPath.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

// --------------------------------------------------------------------------
// GraphisPathIterator class
// --------------------------------------------------------------------------

constructor TGPGraphicsPathIterator.Create(path: TGPGraphicsPath);
var
  nativePath: GpPath;
  iter: GpPathIterator;
begin
  nativePath := nil;
  if assigned(path) then
    nativePath := path.nativePath;
  iter := nil;
  lastResult := GdipCreatePathIter(iter, nativePath);
  SetNativeIterator(iter);
end;

destructor TGPGraphicsPathIterator.Destroy;
begin
  GdipDeletePathIter(nativeIterator);
end;

function TGPGraphicsPathIterator.NextSubpath(out startIndex, endIndex: Integer;
  out isClosed: BOOL): Integer;
begin
  SetStatus(GdipPathIterNextSubpath(nativeIterator, Result, startIndex,
      endIndex, isClosed));
end;

function TGPGraphicsPathIterator.NextSubpath(path: TGPGraphicsPath;
  out isClosed: BOOL): Integer;
var
  nativePath: GpPath;
  resultCount: Integer;
begin
  nativePath := nil;
  if assigned(path) then
    nativePath := path.nativePath;
  SetStatus(GdipPathIterNextSubpathPath(nativeIterator, resultCount,
      nativePath, isClosed));
  Result := resultCount;
end;

function TGPGraphicsPathIterator.NextPathType(out pathType: TPathPointType;
  out startIndex, endIndex: Integer): Integer;
var
  resultCount: Integer;
begin
  SetStatus(GdipPathIterNextPathType(nativeIterator, resultCount, @pathType,
      startIndex, endIndex));
  Result := resultCount;
end;

function TGPGraphicsPathIterator.NextMarker(out startIndex, endIndex: Integer)
  : Integer;
begin
  SetStatus(GdipPathIterNextMarker(nativeIterator, Result, startIndex,
      endIndex));
end;

function TGPGraphicsPathIterator.NextMarker(path: TGPGraphicsPath): Integer;
var
  nativePath: GpPath;
begin
  nativePath := nil;
  if assigned(path) then
    nativePath := path.nativePath;
  SetStatus(GdipPathIterNextMarkerPath(nativeIterator, Result, nativePath));
end;

function TGPGraphicsPathIterator.GetCount: Integer;
begin
  SetStatus(GdipPathIterGetCount(nativeIterator, Result));
end;

function TGPGraphicsPathIterator.GetSubpathCount: Integer;
begin
  SetStatus(GdipPathIterGetSubpathCount(nativeIterator, Result));
end;

function TGPGraphicsPathIterator.HasCurve: BOOL;
begin
  SetStatus(GdipPathIterHasCurve(nativeIterator, Result));
end;

procedure TGPGraphicsPathIterator.Rewind;
begin
  SetStatus(GdipPathIterRewind(nativeIterator));
end;

function TGPGraphicsPathIterator.Enumerate(points: PGPPointF; types: PBYTE;
  count: Integer): Integer;
begin
  SetStatus(GdipPathIterEnumerate(nativeIterator, Result, points, types,
      count));
end;

function TGPGraphicsPathIterator.CopyData(points: PGPPointF; types: PBYTE;
  startIndex, endIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterCopyData(nativeIterator, Result, points, types,
      startIndex, endIndex));
end;

function TGPGraphicsPathIterator.GetLastStatus: TStatus;
begin
  Result := lastResult;
  lastResult := Ok;
end;

procedure TGPGraphicsPathIterator.SetNativeIterator
  (nativeIterator: GpPathIterator);
begin
  self.nativeIterator := nativeIterator;
end;

function TGPGraphicsPathIterator.SetStatus(status: TStatus): TStatus;
begin
  if (status <> Ok) then
    lastResult := status;
  Result := status;
end;

// --------------------------------------------------------------------------
// Path Gradient Brush
// --------------------------------------------------------------------------

constructor TGPPathGradientBrush.Create(points: PGPPointF; count: Integer;
  wrapMode: TWrapMode = WrapModeClamp);
var
  brush: GpPathGradient;
begin
  brush := nil;
  lastResult := GdipCreatePathGradient(points, count, wrapMode, brush);
  SetNativeBrush(brush);
end;

constructor TGPPathGradientBrush.Create(points: PGPPoint; count: Integer;
  wrapMode: TWrapMode = WrapModeClamp);
var
  brush: GpPathGradient;
begin
  brush := nil;
  lastResult := GdipCreatePathGradientI(points, count, wrapMode, brush);
  SetNativeBrush(brush);
end;

constructor TGPPathGradientBrush.Create(path: TGPGraphicsPath);
var
  brush: GpPathGradient;
begin
  brush := nil;
  lastResult := GdipCreatePathGradientFromPath(path.nativePath, brush);
  SetNativeBrush(brush);
end;

function TGPPathGradientBrush.GetCenterColor(out color: TGPColor): TStatus;
begin
  SetStatus(GdipGetPathGradientCenterColor(GpPathGradient(nativeBrush), color));
  Result := lastResult;
end;

function TGPPathGradientBrush.SetCenterColor(color: TGPColor): TStatus;
begin
  SetStatus(GdipSetPathGradientCenterColor(GpPathGradient(nativeBrush), color));
  Result := lastResult;
end;

function TGPPathGradientBrush.GetPointCount: Integer;
begin
  SetStatus(GdipGetPathGradientPointCount(GpPathGradient(nativeBrush), Result));
end;

function TGPPathGradientBrush.GetSurroundColorCount: Integer;
begin
  SetStatus(GdipGetPathGradientSurroundColorCount(GpPathGradient(nativeBrush),
      Result));
end;

function TGPPathGradientBrush.GetSurroundColors(colors: PARGB;
  var count: Integer): TStatus;
var
  count1: Integer;
begin
  if not assigned(colors) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;

  SetStatus(GdipGetPathGradientSurroundColorCount(GpPathGradient(nativeBrush),
      count1));

  if (lastResult <> Ok) then
  begin
    Result := lastResult;
    Exit;
  end;

  if ((count < count1) or (count1 <= 0)) then
  begin
    Result := SetStatus(InsufficientBuffer);
    Exit;
  end;

  SetStatus(GdipGetPathGradientSurroundColorsWithCount
      (GpPathGradient(nativeBrush), colors, count1));
  if (lastResult = Ok) then
    count := count1;

  Result := lastResult;
end;

function TGPPathGradientBrush.SetSurroundColors(colors: PARGB;
  var count: Integer): TStatus;
var
  count1: Integer;
type
  TDynArrDWORD = array of Dword;
begin
  if (colors = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;

  count1 := GetPointCount;

  if ((count > count1) or (count1 <= 0)) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;

  count1 := count;

  SetStatus(GdipSetPathGradientSurroundColorsWithCount
      (GpPathGradient(nativeBrush), colors, count1));

  if (lastResult = Ok) then
    count := count1;
  Result := lastResult;
end;

function TGPPathGradientBrush.GetGraphicsPath(path: TGPGraphicsPath): TStatus;
begin
  if (path = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipGetPathGradientPath(GpPathGradient(nativeBrush),
      path.nativePath));
end;

function TGPPathGradientBrush.SetGraphicsPath(path: TGPGraphicsPath): TStatus;
begin
  if (path = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSetPathGradientPath(GpPathGradient(nativeBrush),
      path.nativePath));
end;

function TGPPathGradientBrush.GetCenterPoint(out point: TGPPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPoint
      (GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.GetCenterPoint(out point: TGPPoint): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPointI
      (GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.SetCenterPoint(point: TGPPointF): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPoint
      (GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.SetCenterPoint(point: TGPPoint): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPointI
      (GpPathGradient(nativeBrush), @point));
end;

function TGPPathGradientBrush.GetRectangle(out rect: TGPRectF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRect(GpPathGradient(nativeBrush),
      @rect));
end;

function TGPPathGradientBrush.GetRectangle(out rect: TGPRect): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRectI(GpPathGradient(nativeBrush),
      @rect));
end;

function TGPPathGradientBrush.SetGammaCorrection(useGammaCorrection: BOOL)
  : TStatus;
begin
  Result := SetStatus(GdipSetPathGradientGammaCorrection
      (GpPathGradient(nativeBrush), useGammaCorrection));
end;

function TGPPathGradientBrush.GetGammaCorrection: BOOL;
begin
  SetStatus(GdipGetPathGradientGammaCorrection(GpPathGradient(nativeBrush),
      Result));
end;

function TGPPathGradientBrush.GetBlendCount: Integer;
var
  count: Integer;
begin
  count := 0;
  SetStatus(GdipGetPathGradientBlendCount(GpPathGradient(nativeBrush), count));
  Result := count;
end;

function TGPPathGradientBrush.GetBlend(blendFactors, blendPositions: PSingle;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientBlend(GpPathGradient(nativeBrush),
      blendFactors, blendPositions, count));
end;

function TGPPathGradientBrush.SetBlend(blendFactors, blendPositions: PSingle;
  count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientBlend(GpPathGradient(nativeBrush),
      blendFactors, blendPositions, count));
end;

function TGPPathGradientBrush.GetInterpolationColorCount: Integer;
var
  count: Integer;
begin
  count := 0;
  SetStatus(GdipGetPathGradientPresetBlendCount(GpPathGradient(nativeBrush),
      count));
  Result := count;
end;

function TGPPathGradientBrush.SetInterpolationColors(presetColors: PARGB;
  blendPositions: PSingle; count: Integer): TStatus;
var
  status: TStatus;
begin
  if ((count <= 0) or (presetColors = nil)) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;

  status := SetStatus(GdipSetPathGradientPresetBlend
      (GpPathGradient(nativeBrush), presetColors, blendPositions, count));
  Result := status;
end;

function TGPPathGradientBrush.GetInterpolationColors(presetColors: PARGB;
  blendPositions: PSingle; count: Integer): TStatus;
var
  status: GpStatus;
  i: Integer;
  argbs: PARGB;
begin
  if ((count <= 0) or (presetColors = nil)) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  getmem(argbs, count * sizeof(argb));
  if (argbs = nil) then
  begin
    Result := SetStatus(OutOfMemory);
    Exit;
  end;

  status := SetStatus(GdipGetPathGradientPresetBlend(nativeBrush, argbs,
      blendPositions, count));

  for i := 0 to count - 1 do
    TColorDynArray(presetColors)[i] := TColorDynArray(argbs)[i];

  freemem(argbs);

  Result := status;
end;

function TGPPathGradientBrush.SetBlendBellShape(focus: Single;
  Scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientSigmaBlend
      (GpPathGradient(nativeBrush), focus, Scale));
end;

function TGPPathGradientBrush.SetBlendTriangularShape(focus: Single;
  Scale: Single = 1.0): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientLinearBlend
      (GpPathGradient(nativeBrush), focus, Scale));
end;

function TGPPathGradientBrush.GetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientTransform(GpPathGradient(nativeBrush),
      matrix.nativeMatrix));
end;

function TGPPathGradientBrush.SetTransform(matrix: TGPMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientTransform(GpPathGradient(nativeBrush),
      matrix.nativeMatrix));
end;

function TGPPathGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdipResetPathGradientTransform(GpPathGradient(nativeBrush)
      ));
end;

function TGPPathGradientBrush.MultiplyTransform(matrix: TGPMatrix;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPathGradientTransform
      (GpPathGradient(nativeBrush), matrix.nativeMatrix, order));
end;

function TGPPathGradientBrush.TranslateTransform(dx, dy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePathGradientTransform
      (GpPathGradient(nativeBrush), dx, dy, order));
end;

function TGPPathGradientBrush.ScaleTransform(sx, sy: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePathGradientTransform
      (GpPathGradient(nativeBrush), sx, sy, order));
end;

function TGPPathGradientBrush.RotateTransform(angle: Single;
  order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePathGradientTransform
      (GpPathGradient(nativeBrush), angle, order));
end;

function TGPPathGradientBrush.GetFocusScales(out xScale, yScale: Single)
  : TStatus;
begin
  Result := SetStatus(GdipGetPathGradientFocusScales
      (GpPathGradient(nativeBrush), xScale, yScale));
end;

function TGPPathGradientBrush.SetFocusScales(xScale, yScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientFocusScales
      (GpPathGradient(nativeBrush), xScale, yScale));
end;

function TGPPathGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetPathGradientWrapMode(GpPathGradient(nativeBrush), Result));
end;

function TGPPathGradientBrush.SetWrapMode(wrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientWrapMode(GpPathGradient(nativeBrush),
      wrapMode));
end;

constructor TGPPathGradientBrush.Create;
begin
  // 閏rase la fonction parent
end;

{ -----------------------------------------------------------------------------
  Procedure: TGPImage.GetFormat
  Purose:    此函数用于xpGDIPicture单元，用于判断图片的类型
  Date:      2006-11-30
  Arguments: None
  Result:    TGUID
  ----------------------------------------------------------------------------- }
function TGPImage.GetFormat: TGUID;
var
  format: TGUID;
begin
  GdipGetImageRawFormat(nativeImage, @format);

  Result := ImageFormatUndefined;

  if IsEqualGUID(format, ImageFormatMemoryBMP) then
    Result := ImageFormatMemoryBMP;

  if IsEqualGUID(format, ImageFormatBMP) then
    Result := ImageFormatBMP;

  if IsEqualGUID(format, ImageFormatEMF) then
    Result := ImageFormatEMF;

  if IsEqualGUID(format, ImageFormatWMF) then
    Result := ImageFormatWMF;

  if IsEqualGUID(format, ImageFormatJPEG) then
    Result := ImageFormatJPEG;

  if IsEqualGUID(format, ImageFormatGIF) then
    Result := ImageFormatGIF;

  if IsEqualGUID(format, ImageFormatPNG) then
    Result := ImageFormatPNG;

  if IsEqualGUID(format, ImageFormatTIFF) then
    Result := ImageFormatTIFF;

  if IsEqualGUID(format, ImageFormatEXIF) then
    Result := ImageFormatEXIF;

  if IsEqualGUID(format, ImageFormatIcon) then
    Result := ImageFormatIcon;
end;

initialization

InitializeGdi;

finalization

FinalizeGdi;

end.
