unit StreamZip;

interface
uses Classes, zlib;

procedure ZipStream(const instream, outStream: TStream; ZipType:integer); stdcall;
procedure UnZipStream(const instream: TStream; const outStream: TStream); stdcall;

implementation

uses SysUtils;


//流压缩
procedure ZipStream(const instream, outStream: TStream; ZipType:integer);
{
//instream： 待压缩的已加密文件流
//outStream  压缩后输出文件流
//ZipType:压缩标准
}
var
 ys: TCompressionStream;
begin
  //流指针指向头部
  inStream.Position := 0;
  //压缩标准的选择
  case ZipType of
  1:  ys := TCompressionStream.Create(clnone,OutStream);     //不压缩
  2:  ys := TCompressionStream.Create(clFastest,OutStream);  //快速压缩
  3:  ys := TCompressionStream.Create(cldefault,OutStream);  //标准压缩
  4:  ys := TCompressionStream.Create(clmax,OutStream);      //最大压缩
  else
    ys := TCompressionStream.Create(clFastest,OutStream);
  end;

  try
    //压缩流
    ys.CopyFrom(inStream, 0);
  finally
    FreeAndNil(ys);
  end;
end;

//流解压
procedure UnZipStream(const instream: TStream; const outStream: TStream);
var
 jyl: TDeCompressionStream;
 buf: array[1..4096] of byte;
 sjread: integer;
begin
  inStream.Position := 0;
  jyl := TDeCompressionStream.Create(inStream);
  try
    repeat
     //读入实际大小
     sjRead := jyl.Read(buf, sizeof(buf));
     if sjread > 0 then
       OutStream.Write(buf, sjRead);
    until (sjRead = 0);
  finally
    FreeAndNil(jyl);
  end;
end;

end.
