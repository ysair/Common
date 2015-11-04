unit ZsMiscUtils;

interface

uses
  Windows, Graphics;

procedure SpilitHotKeyInfo(var aHotKey, shift, Key: word);

function BmpToJpegFile(SourceBMP: TBitMap; AOutFile: String; const ACompressionQuality: Integer=75)
  : Boolean;

implementation

uses
  SysUtils, JPEG, fcVCL;

procedure SpilitHotKeyInfo(var aHotKey, shift, Key: word);
begin
  Vcl.SplitHotKeyInfo(aHotKey, shift, key);
end;

function BmpToJpegFile(SourceBMP: TBitMap; AOutFile: String; const ACompressionQuality: Integer=75)
  : Boolean;
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

end.
