unit fcCompress;

interface

uses
  Classes, SysUtils, zlib, Variants, Windows;

type
  // Ñ¹Ëõ
  Compression = class
  public type
    TCompressionType = (ctZlib);

    zlib = record
      class procedure CompressionStream(const AInStream: TStream; const AOutStream: TStream;
        const ACompressionLevel: TCompressionLevel = clDefault); static;
      class function DeCompressionStream(const AInStream: TStream;
        const AOutStream: TStream): Boolean; static;
    end;
  private
    class var FCompressionType: TCompressionType;
  private
    class function DoCompressionBytes(const AInBytes: TBytes; const ACompress : Boolean; const ACompressionLevel: TCompressionLevel = clDefault) : TBytes;
  public
    class property CompressionType: TCompressionType read FCompressionType write FCompressionType;
  public
    class procedure CompressionStream(const AInStream: TStream; const AOutStream: TStream; const ACompressionLevel: TCompressionLevel = clDefault);
    class function DeCompressionStream(const AInStream: TStream; const AOutStream: TStream): Boolean;

    class function CompressionBytes(const AInBytes: TBytes; const ACompressionLevel: TCompressionLevel = clDefault) : TBytes;
    class function DeCompressionBytes(const AInBytes: TBytes) : TBytes;

    class function CompressionVariant(const AValue: Variant; const ACompressionLevel: TCompressionLevel = clDefault): Variant;
    class function DeCompressionVariant(const AValue: Variant): Variant;

    class function CompressionFileToVariant(const AFileName: string; const ACompressionLevel: TCompressionLevel = clDefault): Variant;
    class function DeCompressionVariantToFile(const AData: Variant; const AFileName: string): Boolean;

    class function CompressionFileToStream(const AFileName: string; const ACompressionLevel: TCompressionLevel = clDefault): TStream; static;
    class function DeCompressionStreamToFile(const AStream: TStream; AFileName: string): Boolean; static;

    class procedure CompressionFile(const AInFileName : string; const AOutFileName : string; const ACompressionLevel: TCompressionLevel = clDefault); static;
    class function  DeCompressionFile(const AInFileName : string; const AOutFileName : string):Boolean; static;
  end;

implementation

uses
  fcConvert;

{ Compression }

class procedure Compression.CompressionStream(const AInStream, AOutStream: TStream;
  const ACompressionLevel: TCompressionLevel);
begin
  case CompressionType of
    ctZlib:
      zlib.CompressionStream(AInStream, AOutStream, ACompressionLevel);
  end;
end;

class function Compression.DeCompressionStream(const AInStream, AOutStream: TStream): Boolean;
begin
  case CompressionType of
    ctZlib:
      Result := zlib.DeCompressionStream(AInStream, AOutStream);
  else
    Result := False;
  end;
end;

class function Compression.CompressionVariant(const AValue: Variant;
  const ACompressionLevel: TCompressionLevel): Variant;
var
  InStream: TMemoryStream;
  OutStream: TMemoryStream;
begin
  if not VarIsArray(AValue) then
    Exit(Null);
  InStream := TMemoryStream.Create;
  try
    Convert.VariantToStream(AValue, InStream);
    OutStream := TMemoryStream.Create;
    try
      CompressionStream(InStream, OutStream, ACompressionLevel);
      Result := Convert.StreamToVariant(OutStream);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

class function Compression.DeCompressionVariant(const AValue: Variant): Variant;
var
  InStream: TMemoryStream;
  OutStream: TMemoryStream;
begin
  if not VarIsArray(AValue) then
    Exit(Null);
  InStream := TMemoryStream.Create;
  try
    Convert.VariantToStream(AValue, InStream);
    OutStream := TMemoryStream.Create;
    try
      DeCompressionStream(InStream, OutStream);
      Result := Convert.StreamToVariant(OutStream);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

class function Compression.CompressionFileToVariant(const AFileName: string;
  const ACompressionLevel: TCompressionLevel): Variant;
var
  InStream: TFileStream;
  OutStream: TMemoryStream;
begin
  InStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    OutStream := TMemoryStream.Create;
    try
      CompressionStream(InStream, OutStream, ACompressionLevel);
      Result := Convert.StreamToVariant(OutStream);
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

class function Compression.DeCompressionVariantToFile(const AData: Variant;
  const AFileName: string): Boolean;
var
  InStream: TMemoryStream;
  OutStream: TFileStream;
begin
  Result := False;
  if not VarIsArray(AData) then
    Exit;
  try
    InStream := TMemoryStream.Create;
    try
      Convert.VariantToStream(AData, InStream);
      ForceDirectories(ExtractFilePath(AFileName));
      OutStream := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
      try
        DeCompressionStream(InStream, OutStream);
        Result := True;
      finally
        OutStream.Free;
      end;
    finally
      InStream.Free;
    end;
  except
  end;
end;

class function Compression.CompressionFileToStream(const AFileName: string;
  const ACompressionLevel: TCompressionLevel): TStream;
var
  InStream: TFileStream;
begin
  InStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := TMemoryStream.Create;
    CompressionStream(InStream, Result, ACompressionLevel);
    Result.Position := 0;
  finally
    InStream.Free;
  end;
end;

class function Compression.DeCompressionStreamToFile(const AStream: TStream;
  AFileName: string): Boolean;
var
  OutStream: TFileStream;
begin
  Result := False;
  try
    if FileExists(AFileName) then
      DeleteFile(PChar(AFileName))
    else
      ForceDirectories(ExtractFilePath(AFileName));
    OutStream := TFileStream.Create(AFileName, fmCreate);
    try
      DeCompressionStream(AStream, OutStream);
      Result := True;
    finally
      OutStream.Free;
    end;
  except
  end;
end;

class function Compression.DoCompressionBytes(const AInBytes: TBytes;
    const ACompress : Boolean; const ACompressionLevel: TCompressionLevel): TBytes;
var
  instream, outstream : TMemoryStream;
begin
  instream  :=  TMemoryStream.Create;
  outstream :=  TMemoryStream.Create;
  try
    instream.Size :=  Length(AInBytes);
    instream.Position :=  0;
    CopyMemory(instream.Memory, @AInBytes[0], instream.Size);

    case CompressionType of
      ctZlib:
        if ACompress then
          zlib.CompressionStream(instream, outstream, ACompressionLevel)
        else
          zlib.DeCompressionStream(instream, outstream);
    end;

    SetLength(Result, outstream.Size);
    outstream.Position  :=  0;
    CopyMemory(@Result[0], outstream.Memory, outstream.Size);
  finally
    instream.Free;
    outstream.Free;
  end;
end;

class function Compression.CompressionBytes(const AInBytes: TBytes;
  const ACompressionLevel: TCompressionLevel): TBytes;
begin
  Result  :=  DoCompressionBytes(AInBytes, True, ACompressionLevel);
end;

class function Compression.DeCompressionBytes(const AInBytes: TBytes): TBytes;
begin
  Result  :=  DoCompressionBytes(AInBytes, False);
end;

class procedure Compression.CompressionFile(const AInFileName, AOutFileName: string; const ACompressionLevel: TCompressionLevel);
var
  fsIn : TFileStream;
  fsOut : TFileStream;
begin
  if FileExists(AOutFileName) then
    DeleteFile(PChar(AOutFileName));
  fsIn  :=  TFileStream.Create(AInFileName, fmOpenRead);
  try
    fsOut :=  TFileStream.Create(AOutFileName, fmCreate);
    try
      CompressionStream(fsIn, fsOut, ACompressionLevel);
    finally
      fsOut.Free;
    end;
  finally
    fsIn.Free;
  end;
end;

class function Compression.DeCompressionFile(const AInFileName, AOutFileName: string): Boolean;
var
  fsIn : TFileStream;
  fsOut : TFileStream;
begin
  if FileExists(AOutFileName) then
    DeleteFile(PChar(AOutFileName));
  fsIn  :=  TFileStream.Create(AInFileName, fmOpenRead);
  try
    fsOut :=  TFileStream.Create(AOutFileName, fmCreate);
    try
      Result  :=  DeCompressionStream(fsIn, fsOut);
    finally
      fsOut.Free;
    end;
  finally
    fsIn.Free;
  end;
end;

{ Compression.Zlib }

class procedure Compression.zlib.CompressionStream(const AInStream, AOutStream: TStream;
  const ACompressionLevel: TCompressionLevel);
var
  cs: TCompressionStream;
begin
  AInStream.Position := 0;
  cs := TCompressionStream.Create(ACompressionLevel, AOutStream);
  try
    cs.CopyFrom(AInStream, 0);
  finally
    cs.Free;
  end;
end;

class function Compression.zlib.DeCompressionStream(const AInStream, AOutStream: TStream): Boolean;
var
  ds: TDeCompressionStream;
  buff: array [1 .. 4096] of Byte;
  iread: integer;
begin
  Result := True;
  AInStream.Position := 0;
  try
    ds := TDeCompressionStream.Create(AInStream);
    try
      repeat
        iread := ds.Read(buff, sizeof(buff));
        if iread > 0 then
          AOutStream.Write(buff, iread);
      until (iread = 0);
    finally
      ds.Free;
    end;
  except
    Result := False;
  end;
end;

end.
