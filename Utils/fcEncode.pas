unit fcEncode;

interface

uses
  Classes, SysUtils;

type
  Http = record
    /// <summary>
    /// 将字符串编码为html格式
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function Encode(const AStr: string): string; static;

    /// <summary>
    /// 将html编码格式解码为字符串
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function Decode(const AStr: string): string; static;


    {先UTF-8，再Http Encode}
    class function Encode_Utf8First(AStr: string): string; static;
  end;

  Base64 = record
    /// <summary>
    /// 按照默认方式进行Base64编码。其结果等于：
    /// Base64.EncodeByEncoding(AStr, TEncoding.Default)
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function  Encode(const AStr: string): string; overload; static;   {注意：这个函数返回的是一行，中间没有回车换行}
    class procedure EncodeFile(const AFileName: string; const OutputList: TStringList); overload; static;
    class function  EncodeStream(const AStream : TStream):string; static;

    /// <summary>
    /// 按照默认方式进行Base64编码，并将结果保存至字符串列表中。
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="OutputList"></param>
    class procedure Encode(const AStr: string; const OutputList: TStringList); overload; static;

    /// <summary>
    /// 根据指定编码方式进行Base64编码。
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="AEncoding"></param>
    /// <returns></returns>
    class function EncodeByEncoding(const AStr: string; AEncoding: TEncoding): string; static;

    /// <summary>
    /// 根据指定的Charset进行解码。目前支持的有：UTF-8，'UTF-7，UNICODE，ASCII，BIGENDIANUNICODE
    /// </summary>
    class function Decode(const AStr: string; AUpperCharset: string = ''): string; static;


    /// <summary>
    /// 根据给定的编码方式进行解码。如果先通过调用Decode把字符串转成默认字符串，在用
    /// 相应的编码格式去解码，会导致乱码或者末尾字符缺失。因此，需要先把输入字符串转成
    /// Bytes，然后用相应编码直接去做转换。
    /// </summary>
    class function DecodeByEncoding(const AStr: string; AEncoding: TEncoding): string; static;

    /// <summary>
    /// 这个主要是对已经知道的，返回是字符的东西解码。
    /// </summary>
    /// <param name="AInputList"></param>
    /// <param name="AOutputList"></param>
    /// <param name="AUpperCharset"></param>
    class procedure DecodeList(const AInputList: TStringList; var AOutputList: TStringList; const AUpperCharset: string); static;

    //把AList中的内容解码到一个文件
    class function DecodeToFile(const AInList: TStringList; const AOutFileName: string): Boolean; overload; static;
    class function DecodeToFile(const AInStr: String; const AOutFileName: string): Boolean; overload; static;
    class function DecodeToStream(const AInStr: string; const AOutStream: TStream): Boolean; overload; static;
    class function DecodeToStream(const AInList: TStringList; const AOutStream: TStream): Boolean; overload; static;



    /// <summary>
    /// 按照UTF8格式编码
    /// </summary>
    class function EncodeUtf8(const AStr: string): string; static;

    /// <summary>
    /// 根据UTF8格式解码
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function DecodeUtf8(const AStr: string): string; static;

    /// <summary>
    /// 判断一行是否是Base64编码
    /// </summary>
    /// <param name="ALine"></param>
    /// <returns></returns>
    class function IsEncodeLine(const ALine: String): boolean; static;
  end;

  {quoted-printable 解码}
  Quoted_Printable = class
    class function QTDecode(AInString: string; var ADecoded: TBytes): Boolean; static;
  end;

  Utf8 = class
    //判断是否是UTF8编码格式
    //这个要用Const定义，否则当AInBytes传入太大的时候，会崩溃
    class function IsUTF8String(const AInBytes: array of Byte): Boolean; overload; static;
    class function IsUTF8String(const AInStream: TStream; const AMaxCheckSize: Int64=200*1024): Boolean; overload; static;
  end;

  OtherDecode = class
    {几种特殊的编码的解码}
    class function DecodeBySpecialCharset(AInList: TStrings; AOutList: TStrings; ACharset: string): Boolean;
  end;


implementation

uses
  fcStr, HTTPApp, EncdDecd;

//const
  // ArrCRLF  : array[0..1] of Byte = (13, 10);
(*  BASE64_Encode_BUFFER_SIZE = 57; { 这是定义每行编码的字符多少， 即53/3*4 = 74 }
  BASE64_Decode_BUFFER_SIZE = $3000;

  Base64_ENCODE_TABLE: array [0 .. 63] of Byte = (065, 066, 067, 068, 069, 070, 071, 072, // 00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, // 08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, // 16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, // 24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, // 32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, // 40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, // 48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63
*)
(*  Base64_DECODE_TABLE: array [Byte] of Cardinal = (255, 255, 255, 255, 255, 255, 255, 255,
    // 00 -  07
    255, 255, 255, 255, 255, 255, 255, 255, // 08 -  15
    255, 255, 255, 255, 255, 255, 255, 255, // 16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, // 24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, // 32 -  39
    255, 255, 255, 062, 255, 255, 255, 063, // 40 -  47
    052, 053, 054, 055, 056, 057, 058, 059, // 48 -  55
    060, 061, 255, 255, 255, 255, 255, 255, // 56 -  63
    255, 000, 001, 002, 003, 004, 005, 006, // 64 -  71
    007, 008, 009, 010, 011, 012, 013, 014, // 72 -  79
    015, 016, 017, 018, 019, 020, 021, 022, // 80 -  87
    023, 024, 025, 255, 255, 255, 255, 255, // 88 -  95
    255, 026, 027, 028, 029, 030, 031, 032, // 96 - 103
    033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
    041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
    049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255);
*)
  { Http }

class function Http.Encode(const AStr: string): string;
begin
  Result := string(HTTPApp.HttpEncode(AnsiString(AStr)));
end;

class function Http.Encode_Utf8First(AStr: string): string;
begin
  Result := string(HTTPApp.HttpEncode(UTF8Encode(AStr)));
end;

class function Http.Decode(const AStr: string): string;
begin
  Result := string(HTTPApp.HttpDecode(AnsiString(AStr)));
end;

{ Base64 }

class function Base64.Encode(const AStr: string): string;
begin
  Result := Trim(EncodeString(AStr));
  Result := StringReplace(Result, AnsiChar(13), '', [rfReplaceAll]);
  Result := StringReplace(Result, AnsiChar(10), '', [rfReplaceAll]);
end;

class procedure Base64.Encode(const AStr: string; const OutputList: TStringList);
var
  InStr, OutStr: TStringStream;
begin
  OutputList.Clear;
  InStr := TStringStream.Create(AStr);
  try
    OutStr := TStringStream.Create('');
    try
      EncdDecd.EncodeStream(InStr, OutStr);
      OutStr.Position := 0;
      OutputList.LoadFromStream(OutStr);
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

class function Base64.EncodeByEncoding(const AStr: string; AEncoding: TEncoding): string;
var
  bytes: TBytes;
begin
  bytes := AEncoding.GetBytes(AStr);
  Result := Trim(string(EncodeBase64(bytes, Length(bytes))));
end;

class procedure Base64.EncodeFile(const AFileName: string;
  const OutputList: TStringList);
var
  l_InStream: TMemoryStream;
  l_OutStream: TStringStream;
begin
  OutputList.Clear;
  if FileExists(AFileName) then
  begin
    l_InStream := TMemoryStream.Create;
    l_OutStream := TStringStream.Create;
    try
      l_InStream.LoadFromFile(AFileName);
      EncdDecd.EncodeStream(l_InStream, l_OutStream);
      l_OutStream.Position := 0;
      OutputList.LoadFromStream(l_OutStream);
    finally
      l_InStream.Free;
      l_OutStream.Free;
    end;
  end;
end;

class function Base64.EncodeStream(const AStream: TStream): string;
var
  l_OutStream: TStringStream;
begin
  l_OutStream := TStringStream.Create;
  try
    EncdDecd.EncodeStream(AStream, l_OutStream);
    Result  :=  l_OutStream.DataString;
  finally
    l_OutStream.Free;
  end;
end;

class function Base64.EncodeUtf8(const AStr: string): string;
var
  bytes: RawByteString;
begin
  bytes := UTF8Encode(AStr);
  Result := string(EncodeBase64(Pointer(bytes), Length(bytes)));
end;

class function Base64.DecodeByEncoding(const AStr: string; AEncoding: TEncoding): string;
begin
  Result := AEncoding.GetString(DecodeBase64(AnsiString(AStr)))
end;

class function Base64.Decode(const AStr: string; AUpperCharset: string = ''): string;
var
  bytes: TBytes;
  encoding:TEncoding;
begin
  if 'UTF-8' = AUpperCharset then
    encoding := TEncoding.UTF8
  else if 'UTF-7' = AUpperCharset then
    encoding := TEncoding.UTF7
  else if 'UNICODE' = AUpperCharset then
    encoding := TEncoding.Unicode
  else if 'ASCII' = AUpperCharset then
    encoding := TEncoding.ASCII
  else if 'BIGENDIANUNICODE' = AUpperCharset then
    encoding := TEncoding.BigEndianUnicode
  else
  begin
    bytes := DecodeBase64(AnsiString(AStr));
    encoding := nil;
    TEncoding.GetBufferEncoding(bytes, encoding);
  end;

  Result := DecodeByEncoding(AStr, encoding);
end;

procedure Base64EncodeStream(const InputStream: TStream; var OutputList: TStringList);
var
  OutStr: TStringStream;
begin
  OutStr := TStringStream.Create('');
  try
    EncdDecd.EncodeStream(InputStream, OutStr);
    OutputList.Text := OutStr.DataString;
  finally
    OutStr.Free;
  end;
end;

class function Base64.DecodeUtf8(const AStr: string): string;
begin
  Result := Base64.Decode(AStr, 'UTF-8');
end;

class function Base64.IsEncodeLine(const ALine: String): boolean;
var
  i, l_Count: Integer;
  l_Base64Set, l_CrlSet: TSysCharSet;
begin
  l_Base64Set := ['+','/'..'9','A'..'Z','a'..'z','='];    //base64的编码
  l_CrlSet := [AnsiChar(13), AnsiChar(10)];

  //判断
  l_Count := 0;
  for i := 1 to Length(ALine) do
  begin
    if CharInSet(ALine[i],l_Base64Set) then
    begin
      Inc(l_Count);
      Continue;
    end;

    if CharInSet(ALine[i], l_CrlSet) or (Trim(ALine[i])='') then
      Continue;

    Exit(False);
  end;

  Result := (l_Count>0) and ((l_Count mod 4)=0);
end;

class procedure Base64.DecodeList(const AInputList: TStringList; var AOutputList: TStringList;
  const AUpperCharset: string);
begin
  Str.TrimEmptyLine(AInputList);

  if AInputList.Count = 0 then
    Exit;

  if not IsEncodeLine(AInputList[0]) then
    Exit;

  AOutputList.Text := Decode(AInputList.Text, AUpperCharset);
end;

class function Base64.DecodeToFile(const AInList: TStringList;
  const AOutFileName: string): Boolean;
var
  l_OutStream: TMemoryStream;
begin
  Result := True;
  l_OutStream := TMemoryStream.Create;
  try
    if DecodeToStream(AInList, l_OutStream) then
    begin
      try
        if ForceDirectories(ExtractFilePath(AOutFileName)) then
        begin
          l_OutStream.SaveToFile(AOutFileName);
        end;
      except
        Result := False;
      end;
    end;
  finally
    l_OutStream.Free;
  end;
end;

class function Base64.DecodeToFile(const AInStr, AOutFileName: string): Boolean;
var
  l_OutStream: TMemoryStream;
begin
  Result := True;
  l_OutStream := TMemoryStream.Create;
  try
    if DecodeToStream(AInStr, l_OutStream) then
    begin
      try
        if ForceDirectories(ExtractFilePath(AOutFileName)) then
        begin
          l_OutStream.SaveToFile(AOutFileName);
        end;
      except
        Result := False;
      end;
    end;
  finally
    l_OutStream.Free;
  end;
end;

class function Base64.DecodeToStream(const AInStr: string; const AOutStream: TStream): Boolean;
var
  l_InStream: TStringStream;
begin
  l_InStream := TStringStream.Create(AInStr);
  try
    try
      EncdDecd.DecodeStream(l_InStream, AOutStream);
      AOutStream.Position := 0;
      Result := True;
    except
      Result := False;
    end;
  finally
    l_InStream.Free;
  end;
end;

class function Base64.DecodeToStream(const AInList: TStringList;
  const AOutStream: TStream): Boolean;
begin
  Result := DecodeToStream(AInList.Text, AOutStream);
end;

{ OtherDecode }

class function OtherDecode.DecodeBySpecialCharset(AInList, AOutList: TStrings; ACharset: string): Boolean;
var
  l_Stream: TMemoryStream;
  l_Encoding: TEncoding;
const
  c_Charsets: array[0..4] of string = (
      'iso-2022-cn', 'iso-2022-cn-ext',
      'iso-2022-jp', 'csISO2022JP',
      'iso-2022-kr'
  );
begin
  l_Encoding := nil;
  case fcStr.Arr.PosInStrArray(ACharset, c_Charsets, false) of
    0, 1  : l_Encoding := TEncoding.GetEncoding(50229);
    2     : l_Encoding := TEncoding.GetEncoding(50220);
    3     : l_Encoding := TEncoding.GetEncoding(50221);
    4     : l_Encoding := TEncoding.GetEncoding(50225);
  end;

  Result := l_Encoding <> nil;
  if Result then
  begin
    l_Stream := TStringStream.Create;
    try
      AInList.SaveToStream(l_Stream);
      l_Stream.Position := 0;
      AOutList.LoadFromStream(l_Stream, l_Encoding);
    finally
      l_Stream.Free;
      l_Encoding.Free;
    end;
  end;
end;

{ Utf8 }

class function Utf8.IsUTF8String(const AInBytes: array of Byte): Boolean;
var
  l_Idx, l_Length: Integer;
  l_CurChar, l_NextChar1, l_NextChar2: Integer;
  l_FindUtf8Count, l_FindErrCount: Integer;
  l_FindHZASCII: Boolean;
begin
  if High(AInBytes) - Low(AInBytes) <2 then
    Exit(False);


  l_Idx := Low(AInBytes);
  l_Length := High(AInBytes);
  l_FindUtf8Count := 0;
  l_FindErrCount := 0;
  l_FindHZASCII := false;

  while l_Idx <= l_Length do
  begin
    if (l_FindUtf8Count > 5) or (l_FindErrCount>5) then
      Break;

    l_CurChar := AInBytes[l_Idx];
    if l_CurChar < 128 then    //是ASCII码
    begin
      Inc(l_Idx);
      Continue;
    end;

    l_FindHZASCII := True;

    {UTF8两种编码格式
      两字节编码  110xxxxx 10xxxxxx
      三字节编码 1110xxxx 10xxxxxx 10xxxxxx
    }
    if (l_CurChar AND $E0) = $C0 then   { $C0，110xxxxx  }
    begin
      if l_Idx >= l_Length then
        Break;

      l_NextChar1 := AInBytes[l_Idx+1];
      if (l_NextChar1 AND $C0) <> $80 then    {10xxxxxx}
      begin
        {Error !}
        Inc(l_FindErrCount);
        Inc(l_Idx);
        Continue;
      end;

      Inc(l_FindUtf8Count);
      Inc(l_Idx, 2);
    end
    else
    if (l_CurChar AND $F0) = $E0 then    {1110xxxx}
    begin
      if l_Idx >= l_Length-1 then
        Break;

      l_NextChar1 := AInBytes[l_Idx+1];
      l_NextChar2 := AInBytes[l_Idx+2];
      if not ( ((l_NextChar1 AND $C0) = $80) AND ((l_NextChar2 AND $C0) = $80)  ) then
      begin
        {Error !}
        Inc(l_FindErrCount);
        Inc(l_Idx);
        Continue;
      end;

      Inc(l_FindUtf8Count);
      Inc(l_Idx, 3);
    end
    else
    begin
      Inc(l_FindErrCount);
      Inc(l_Idx);;
    end;
  end;

  if not l_FindHZASCII then
    Result := false
  else
  Result := l_FindUtf8Count>l_FindErrCount;
end;

class function Utf8.IsUTF8String(const AInStream: TStream; const AMaxCheckSize: Int64): Boolean;
var
  l_Size, l_OldPos: Int64;
  l_Bytes: TBytes;
begin
  if AInStream.Size <= AMaxCheckSize then
    l_Size := AInStream.Size
  else
    l_Size := AMaxCheckSize;
  if l_Size=0 then
    Exit(False);

  l_OldPos := AInStream.Position;
  try
    SetLength(l_Bytes, l_Size);
    AInStream.Position := 0;
    AInStream.Read(l_Bytes[0], l_Size);
    Result := IsUTF8String(l_Bytes);
  finally
    AInStream.Position := l_OldPos;
  end;
end;

{ Quoted_Printable }

class function Quoted_Printable.QTDecode(AInString: string; var ADecoded: TBytes): Boolean;
  procedure AppendByte(var VBytes: TBytes; const AByte: Byte);
  var
    LOldLen: Integer;
  begin
    LOldLen := Length(VBytes);
    SetLength(VBytes, LOldLen + 1);
    VBytes[LOldLen] := AByte;
  end;

var
  l_data: AnsiString;
  I: Integer;
begin
  SetLength(ADecoded, 0);
  l_data := AnsiString(AInString);

  I := 1;
  while I <= Length(l_data) do
  begin
    if l_data[i] = '_' then
    begin
      AppendByte(ADecoded, Ord(' '));
    end
    else if (l_data[i] = '=') and (Length(l_data) >= (i+2)) then //make sure we can access i+2
    begin
      AppendByte(ADecoded, StrToIntDef(Trim('$' + string(Copy(l_data, i+1, 2))), 32));
      Inc(I, 2);
    end else
    begin
      AppendByte(ADecoded, Ord(l_data[i]));
    end;
    Inc(I);
  end;
  Result := True;
end;

end.
