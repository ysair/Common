unit fcEncrypt;

interface

uses
  SysUtils, Classes, EncdDecd, fcCompress, fcEncode, Windows;

type

  { ***************************************************************************
     字符串普通加密， 采用类似DreamMail中的密码加密算法
     加密强度不高，仅用于普通加密。
     加密的信息与机器无关，可以跨机器使用
  }
  Encrypt = record
    /// <summary>
    /// 利用给定Key对字符串进行加密
    /// </summary>
    /// <param name="ASrc"></param>
    /// <param name="AKey"></param>
    /// <returns></returns>
    class function AnsiEncrypt(const ASrc: AnsiString; const AKey: AnsiString = ''): AnsiString;
      static;

    /// <summary>
    /// 利用给定Key对字符串进行解密
    /// </summary>
    /// <param name="ASrc"></param>
    /// <param name="AKey"></param>
    /// <returns></returns>
    class function AnsiDecrypt(const ASrc: AnsiString; const AKey: AnsiString = ''): AnsiString;
      static;

    /// <summary>
    /// 将字符串压缩后进行Base64编码。
    /// </summary>
    /// <param name="AInStr"></param>
    /// <returns></returns>
    class function ZipAndBase64(AInStr: string): String; static;


    /// <summary>
    /// 将字符串Base64解码后解压缩。
    /// </summary>
    /// <param name="AInStr"></param>
    /// <returns></returns>
    class function UnZipAndBase64(AInStr: string): String; static;

    /// <summary>
    /// 编码密码。算法： Str -> Encrypt -> Zip -> Base64 -> (Add Flag)
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function EncryptPassword(const AStr: string): string; static;

    /// <summary>
    /// 解码密码。算法： Str -> (Remove Flag) -> Base64 Decode -> Un Zip -> Decrypt
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function DecryptPassword(const AStr: string): string; static;
  end;

  { ***************************************************************************
     调用Crypt32.dll 加密，
     加密强度未验证。
     加密后的密码仅本机适用，不合适跨机器
  }
  Crypt32Dll = class
  private
    type
      _CRYPTOAPI_BLOB = packed record
        cbData: DWORD;
        pbData: Pointer;
      end;
      TCryptoApiBlob     = _CRYPTOAPI_BLOB;
      PCrypyoApiBlob     = ^TCryptoApiBlob;
      CRYPT_INTEGER_BLOB = _CRYPTOAPI_BLOB;
      DATA_BLOB          = _CRYPTOAPI_BLOB;
      PDATA_BLOB         = ^CRYPT_INTEGER_BLOB;

      _CRYPTPROTECT_PROMPTSTRUCT = packed record
        cbSize:        DWORD;
        dwPromptFlags: DWORD;
        hwndApp:       HWND;
        szPrompt:      LPCWSTR;
      end;
      TCryptProtectPromptStruct  = _CRYPTPROTECT_PROMPTSTRUCT;
      PCryptProtectPromptStruct  = ^TCryptProtectPromptStruct;
      CRYPTPROTECT_PROMPTSTRUCT  = _CRYPTPROTECT_PROMPTSTRUCT;
      PCRYPTPROTECT_PROMPTSTRUCT = ^_CRYPTPROTECT_PROMPTSTRUCT;

      TCryptProtectData = function(  pDataIn: PDATA_BLOB;
                                     szDataDescr: LPCWSTR;
                                     pOptionalEntropy: PDATA_BLOB;
                                     pReserved: Pointer;
                                     pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                                     dwFlags: DWORD;
                                     pDataOut: PDATA_BLOB): BOOL; stdcall;
      TCryptUnprotectData = function(pDataIn: PDATA_BLOB;
                                     var ppszDataDescr: LPWSTR;
                                     pOptionalEntropy: PDATA_BLOB;
                                     pReserved: Pointer;
                                     pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                                     dwFlags: DWORD;
                                     pDataOut: PDATA_BLOB): BOOL; stdcall;
  private
    class procedure FreeDataBlob(var AData: DATA_BLOB);
    class function  GetDataBlobText(AData: DATA_BLOB): string;
  public
    //加密后并且base64
    class function EncryptData(AInData: string; const ADataDesc: string='pwd'; const AFlags: Integer=0): string;
    class function DecryptData(AInData: string; const AFlags: Integer=0): string;
    class function EncryptBytes(const ABytes : TBytes; const ADataDesc: string='pwd'; const AFlags: Integer=0): TBytes;
    class function DecryptBytes(const ABytes : TBytes; const AFlags: Integer=0): TBytes;
  end;

  { ***************************************************************************
     采用AES256加密
     可以指定算法，加密后的密文包含前缀，用于标识算法
  }
  TExtAES256Version = (
      extaesNONE,             //无
      extaesByLocalMachine,   //加密的密钥与本机有关，采用MachineGUID作为密钥
      extaesByCommonKey       //采用公共密钥加密的
      );
  ExtAES256 = record
  private
    class function GetSeedKey(AVer: TExtAES256Version): string; static;
  public
    class function DoEncrypt(const AStr: string; const AVer: TExtAES256Version=extaesByCommonKey): string; static;
    class function DoDecrypt(const AStr: string): string; static;
  end;

implementation
uses
  fcAES, fcRegistry, fcHash;

const
  C_AnsiEncryptPassKey = '{48598C67-08D8-48AE-8EBB-0E19D59D004C}';
  C_AnsiEncryptPassFirst = '(0E19D59D004C)';

  C_ExtAES256_Headers: array [0 .. 2] of string = (
      '',
      '(Ver1AesLcMACHINEC4C)',
      '(Ver1AesDefaultKeyC4C)');

  C_CryptDll            = 'Crypt32.dll';
  C_CryptProtectData    = 'CryptProtectData';
  C_CryptUnprotectData  = 'CryptUnprotectData';

{ Encrypt }

class function Encrypt.AnsiEncrypt(const ASrc: AnsiString; const AKey: AnsiString): AnsiString;
var
  KeyLen: Integer;
  KeyPos: Integer;
  offset: Integer;
  dest: AnsiString;
  SrcPos: Integer;
  SrcAsc: Integer;
  Range: Integer;
  Key: AnsiString;
begin
  if AKey = '' then
    Key := C_AnsiEncryptPassKey
  else
    Key := AKey;
  KeyLen := Length(Key);
  KeyPos := 0;
  Range := 256;
  offset := Random(Range);
  dest := AnsiString(format('%1.2x', [offset]));
  for SrcPos := 1 to Length(ASrc) do
  begin
    SrcAsc := (Ord(ASrc[SrcPos]) + offset) MOD 255;
    if KeyPos < KeyLen then
      KeyPos := KeyPos + 1
    else
      KeyPos := 1;
    SrcAsc := SrcAsc xor Ord(Key[KeyPos]);
    dest := dest + AnsiString(format('%1.2x', [SrcAsc]));
    offset := SrcAsc;
  end;
  Result := dest;
end;

class function Encrypt.AnsiDecrypt(const ASrc: AnsiString; const AKey: AnsiString): AnsiString;
var
  KeyLen: Integer;
  KeyPos: Integer;
  offset: Integer;
  dest: AnsiString;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Key: AnsiString;
begin
  if AKey = '' then
    Key := C_AnsiEncryptPassKey
  else
    Key := AKey;
  KeyLen := Length(Key);
  KeyPos := 0;
  offset := StrToInt('$' + string(copy(ASrc, 1, 2)));
  SrcPos := 3;
  repeat
    SrcAsc := StrToInt('$' + string(copy(ASrc, SrcPos, 2)));
    if KeyPos < KeyLen Then
      KeyPos := KeyPos + 1
    else
      KeyPos := 1;
    TmpSrcAsc := SrcAsc xor Ord(Key[KeyPos]);
    if TmpSrcAsc <= offset then
      TmpSrcAsc := 255 + TmpSrcAsc - offset
    else
      TmpSrcAsc := TmpSrcAsc - offset;
    dest := dest + AnsiString(AnsiChar(TmpSrcAsc));
    offset := SrcAsc;
    SrcPos := SrcPos + 2;
  until SrcPos >= Length(ASrc);
  Result := dest;
end;

class function Encrypt.ZipAndBase64(AInStr: string): String;
var
  l_BaseInStream, l_BaseOutStream: TMemoryStream;
  l_Buffer: TBytes;
begin
  l_BaseInStream := TMemoryStream.Create;
  l_BaseOutStream := TMemoryStream.Create;
  try
    try
      l_BaseInStream.Write(AnsiString(AInStr)[1], Length(AnsiString(AInStr)));
      l_BaseInStream.Position := 0;
      Compression.CompressionStream(l_BaseInStream, l_BaseOutStream);
      l_BaseInStream.Clear;

      l_BaseOutStream.Position := 0;
      EncdDecd.EncodeStream(l_BaseOutStream, l_BaseInStream);

      l_BaseInStream.Position := 0;
      SetLength(l_Buffer, l_BaseInStream.Size);
      l_BaseInStream.Read(l_Buffer[0], l_BaseInStream.Size);
      Result := TEncoding.ASCII.GetString(l_Buffer);
    except
      Result := '';
    end;
  finally
    l_BaseInStream.Free;
    l_BaseOutStream.Free;
  end;
end;

class function Encrypt.UnZipAndBase64(AInStr: string): String;
var
  l_BaseInStream, l_BaseOutStream: TMemoryStream;
  l_Buffer: TBytes;
begin
  l_BaseInStream := TMemoryStream.Create;
  l_BaseOutStream := TMemoryStream.Create;

  try
    try
      l_BaseInStream.Write(AnsiString(AInStr)[1], Length(AnsiString(AInStr)));
      l_BaseInStream.Position := 0;
      EncdDecd.DecodeStream(l_BaseInStream, l_BaseOutStream);

      l_BaseOutStream.Position := 0;
      l_BaseInStream.Clear;
      Compression.DeCompressionStream(l_BaseOutStream, l_BaseInStream);

      l_BaseInStream.Position := 0;
      SetLength(l_Buffer, l_BaseInStream.Size);
      l_BaseInStream.Read(l_Buffer[0], l_BaseInStream.Size);
      Result := TEncoding.Default.GetString(l_Buffer);
    except
      Result := '';
    end;
  finally
    l_BaseInStream.Free;
    l_BaseOutStream.Free;
  end;
end;

class function Encrypt.EncryptPassword(const AStr: string): string;
var
  l_OutStr: string;
  l_Idx: Integer;
begin
  if AStr = '' then
    Exit('');

  l_OutStr := string(AnsiEncrypt(AnsiString(AStr)));
  l_OutStr := ZipAndBase64(l_OutStr);
  Result := C_AnsiEncryptPassFirst;
  for l_Idx := 1 to Length(l_OutStr) do // 去掉base64编码中的回车换行
    if Trim(l_OutStr[l_Idx]) <> '' then
      Result := Result + l_OutStr[l_Idx];
end;

class function Encrypt.DecryptPassword(const AStr: string): string;
var
  l_OutStr: string;
begin
  if (AStr = '') or (Pos(C_AnsiEncryptPassFirst, AStr) <> 1) then
    Exit(AStr);

  l_OutStr := copy(AStr, Length(C_AnsiEncryptPassFirst) + 1, Length(AStr) - Length(C_AnsiEncryptPassFirst));
  if l_OutStr = '' then
    Exit('');
  try
    l_OutStr := UnZipAndBase64(l_OutStr);
    l_OutStr := string(AnsiDecrypt(AnsiString(l_OutStr)));
  except
    Exit('');
  end;

  Result := l_OutStr;
end;

{ Crypt32Dll }

class function Crypt32Dll.EncryptData(AInData: string; const ADataDesc: string;
  const AFlags: Integer): string;
var
  l_Handle: THandle;
  l_Func: TCryptProtectData;
  l_DataIn:  DATA_BLOB;
  l_DataOut: DATA_BLOB;
  l_StreamIn: TMemoryStream;
  l_StreamOut: TStringStream;
begin
  Result := AInData;
  if AInData = '' then
    Exit;

  l_Handle := LoadLibrary(C_CryptDll);
  if l_Handle>0 then
  begin
    try
      @l_Func := GetProcAddress(l_Handle, C_CryptProtectData);
      if Assigned(@l_Func) then
      begin
        FillChar(l_DataIn, SizeOf(DATA_BLOB), 0);
        FillChar(l_DataOut, SizeOf(DATA_BLOB), 0);
        try
          l_DataIn.pbData := Pointer(WideString(AInData));
          l_DataIn.cbData := Length(AInData) * SizeOf(WChar);

          try
            if l_Func(@l_DataIn, PWideChar(WideString(ADataDesc)), nil, nil, nil, AFlags, @l_DataOut) then
            begin
              l_StreamIn := TMemoryStream.Create;
              l_StreamOut := TStringStream.Create;
              try
                l_StreamIn.Write(l_DataOut.pbData^, l_DataOut.cbData);
                l_StreamIn.Position := 0;
                EncdDecd.EncodeStream(l_StreamIn, l_StreamOut);
                Result := l_StreamOut.DataString;
              finally
                l_StreamIn.Free;
                l_StreamOut.Free;
              end;
            end;
          except
          end;
        finally
          FreeDataBlob(l_DataIn);
          FreeDataBlob(l_DataOut);
        end;
      end;
    finally
      FreeLibrary(l_Handle);
    end;
  end;
end;

class function Crypt32Dll.DecryptData(AInData: string;
  const AFlags: Integer): string;
var
  l_Handle: THandle;
  l_Func: TCryptUnprotectData;
  l_DataIn:  DATA_BLOB;
  l_DataOut: DATA_BLOB;
  l_lpwszDesc: PWideChar;
  l_StreamIn: TMemoryStream;
  l_StreamOut: TMemoryStream;
begin
  Result := AInData;
  if AInData='' then
    Exit;

  l_Handle := LoadLibrary(C_CryptDll);
  if l_Handle>0 then
  begin
    try
      @l_Func := GetProcAddress(l_Handle, C_CryptUnprotectData);
      if Assigned(@l_Func) then
      begin
        FillChar(l_DataIn, SizeOf(DATA_BLOB), 0);
        FillChar(l_DataOut, SizeOf(DATA_BLOB), 0);
        try
          l_StreamIn := TMemoryStream.Create;
          l_StreamOut := TMemoryStream.Create;
          try
            l_StreamIn.Write(AnsiString(AInData)[1], Length(AnsiString(AInData)));
            l_StreamIn.Position := 0;
            try
              EncdDecd.DecodeStream(l_StreamIn, l_StreamOut);
            except
              Exit('');
            end;
            l_StreamOut.Position := 0;
            l_DataIn.cbData := l_StreamOut.Size;
            GetMem(l_DataIn.pbData, l_DataIn.cbData);
            try
              l_StreamOut.Read(l_DataIn.pbData^, l_StreamOut.Size);
              try
                if l_Func(@l_DataIn, l_lpwszDesc, nil, nil, nil, AFlags, @l_DataOut) then
                begin
                  Result := GetDataBlobText(l_DataOut);
                  LocalFree(HLOCAL(l_lpwszDesc));
                end;
              except
              end;
            finally
              FreeMemory(l_DataIn.pbData);
            end;
          finally
            l_StreamIn.Free;
            l_StreamOut.Free;
          end;
        finally
          FreeDataBlob(l_DataOut);
        end;
      end;
    finally
      FreeLibrary(l_Handle);
    end;
  end;
end;

class function Crypt32Dll.EncryptBytes(const ABytes: TBytes;
  const ADataDesc: string; const AFlags: Integer): TBytes;
var
  l_Handle: THandle;
  l_Func: TCryptProtectData;
  l_DataIn:  DATA_BLOB;
  l_DataOut: DATA_BLOB;
begin
  Result := ABytes;
  if Length(ABytes) = 0 then
    Exit;

  l_Handle := LoadLibrary(C_CryptDll);
  if l_Handle>0 then
  begin
    try
      @l_Func := GetProcAddress(l_Handle, C_CryptProtectData);
      if Assigned(@l_Func) then
      begin
        FillChar(l_DataIn, SizeOf(DATA_BLOB), 0);
        FillChar(l_DataOut, SizeOf(DATA_BLOB), 0);
        try
          l_DataIn.pbData := @ABytes[0];
          l_DataIn.cbData := Length(ABytes);
          try
            if l_Func(@l_DataIn, PWideChar(WideString(ADataDesc)), nil, nil, nil, AFlags, @l_DataOut) then
            begin
              SetLength(Result, l_DataOut.cbData);
              CopyMemory(@Result[0], l_DataOut.pbData, l_DataOut.cbData);
            end;
          except
          end;
        finally
          FreeDataBlob(l_DataIn);
          FreeDataBlob(l_DataOut);
        end;
      end;
    finally
      FreeLibrary(l_Handle);
    end;
  end;
end;

class function Crypt32Dll.DecryptBytes(const ABytes: TBytes;
  const AFlags: Integer): TBytes;
var
  l_Handle: THandle;
  l_Func: TCryptUnprotectData;
  l_DataIn:  DATA_BLOB;
  l_DataOut: DATA_BLOB;
  l_lpwszDesc: PWideChar;
begin
  Result := ABytes;
  if Length(ABytes) = 0 then
    Exit;

  l_Handle := LoadLibrary(C_CryptDll);
  if l_Handle>0 then
  begin
    try
      @l_Func := GetProcAddress(l_Handle, C_CryptUnprotectData);
      if Assigned(@l_Func) then
      begin
        FillChar(l_DataIn, SizeOf(DATA_BLOB), 0);
        FillChar(l_DataOut, SizeOf(DATA_BLOB), 0);
        try
          l_DataIn.pbData := @ABytes[0];
          l_DataIn.cbData := Length(ABytes);
          try
            if l_Func(@l_DataIn, l_lpwszDesc, nil, nil, nil, AFlags, @l_DataOut) then
            try
              SetLength(Result, l_DataOut.cbData);
              CopyMemory(@Result[0], l_DataOut.pbData, l_DataOut.cbData);
            finally
              LocalFree(HLOCAL(l_lpwszDesc));
            end;
          except
          end;
        finally
          FreeDataBlob(l_DataIn);
          FreeDataBlob(l_DataOut);
        end;
      end;
    finally
      FreeLibrary(l_Handle);
    end;
  end;
end;

class procedure Crypt32Dll.FreeDataBlob(var AData: DATA_BLOB);
begin
  if Assigned(AData.pbData) then
    LocalFree(HLOCAL(AData.pbData));
  FillChar(AData, SizeOf(DATA_BLOB), 0);
end;

class function Crypt32Dll.GetDataBlobText(AData: DATA_BLOB): string;
var
  l_Out: PWideChar;
begin
  Getmem(l_Out, AData.cbData);
  lstrcpynW(l_Out, PWideChar(AData.pbData), (AData.cbData DIV 2) + 1);
  Result := l_Out;
  FreeMem(l_Out);
end;

{ ExtAES256 }

class function ExtAES256.DoDecrypt(const AStr: string): string;
var
  I: Integer;
  l_Value: string;
begin
  if AStr = '' then
    Exit(AStr);

  for I := 1 to High(C_ExtAES256_Headers) do
  begin
    if Copy(AStr, 1, Length(C_ExtAES256_Headers[I])) = C_ExtAES256_Headers[I] then
    begin
      l_Value := Copy(AStr, Length(C_ExtAES256_Headers[I]) + 1,
        Length(AStr));
      case TExtAES256Version(I) of
        extaesByLocalMachine,
        extaesByCommonKey:
          begin
            Result := fcAES.AES.DecryptString(l_Value, GetSeedKey(TExtAES256Version(I)));
            Exit;
          end;
      end;
    end;
  end;
  Result := AStr;
end;

class function ExtAES256.DoEncrypt(const AStr: string;
  const AVer: TExtAES256Version): string;
begin
  if AStr='' then
    Exit(AStr);

  case AVer of
    extaesByLocalMachine,
    extaesByCommonKey:
      begin
        Result := C_ExtAES256_Headers[Ord(AVer)] + fcAES.AES.EncryptString(AStr, GetSeedKey(AVer));
      end
  else
    Result := AStr;
  end;
end;

class function ExtAES256.GetSeedKey(AVer: TExtAES256Version): string;
const
  c_Path = 'SOFTWARE\Microsoft\Cryptography';
  c_Key = 'MachineGuid';
begin
  case AVer of
    extaesByLocalMachine:
      begin
        Result := fcRegistry.Reg.SH_GetKeyValue(DWORD($80000002), c_Path, c_Key, ''); // HKEY_LOCAL_MACHINE
        if Result = '' then
          Result := fcRegistry.Reg.SH_GetKeyValueX64(DWORD($80000002), c_Path, c_Key, '');
        Result := fcHash.Hash.MD5Str(Result + '{51EE5C0D-6A93-469E-861E-13C6E4268B12}');
      end;
    extaesByCommonKey:
      begin
        Result := fcHash.Hash.MD5Str('{C60E4CA1-BF2A-43BB-8E7E-76B04A16BD39}');
      end;
  end;
end;

initialization

Randomize;

end.
