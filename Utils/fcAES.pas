(* ************************************************************ *)
(* Advanced Encryption Standard (AES) *)
(* Interface Unit v1.3 *)
(* *)
(* Copyright (c) 2002 Jorlen Young *)
(* *)
(* 说明： *)
(* 基于 ElASE.pas 单元封装 *)
(* *)
(* 这是一个 AES 加密算法的标准接口。 *)
(* 调用示例： *)
(* if not EncryptStream(src, key, TStream(Dest), keybit) then *)
(* showmessage('encrypt error'); *)
(* *)
(* if not DecryptStream(src, key, TStream(Dest), keybit) then *)
(* showmessage('encrypt error'); *)
(* *)
(* *** 一定要对Dest进行TStream(Dest) *** *)
(* ========================================================== *)
(* *)
(* 支持 128 / 192 / 256 位的密匙 *)
(* 默认情况下按照 128 位密匙操作 *)
(* *)
(* ************************************************************ *)

unit fcAES;

interface

{$IFDEF VER210}
{$WARN IMPLICIT_STRING_CAST OFF} // 关闭警告
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

uses
  SysUtils, Classes, Math, fcAES_ex, Windows;

const
  SDestStreamNotCreated = 'Dest stream not created.';
  SEncryptStreamError = 'Encrypt stream error.';
  SDecryptStreamError = 'Decrypt stream error.';

type
  TKeyBit = (kb128, kb192, kb256);

  AES = record
    class function StrToHex(Const str: AnsiString): AnsiString; static;
    class function HexToStr(const str: AnsiString): AnsiString; static;
    class function EncryptString(Value: String; Key: String;
      KeyBit: TKeyBit = kb256): String; static;
    class function DecryptString(Value: String; Key: String;
      KeyBit: TKeyBit = kb256): String; static;
    class function EncryptStream(Src: TStream; Key: AnsiString;
      const Dest: TStream; KeyBit: TKeyBit = kb256): Boolean; static;
    class function DecryptStream(Src: TStream; Key: AnsiString;
      const Dest: TStream; KeyBit: TKeyBit = kb256): Boolean; static;
    class procedure EncryptFile(SourceFile, DestFile: String; Key: AnsiString;
      KeyBit: TKeyBit = kb256); static;
    class procedure DecryptFile(SourceFile, DestFile: String; Key: AnsiString;
      KeyBit: TKeyBit = kb256); static;
    class function EncryptBytes(const ABytes : TBytes; const AKey : AnsiString;
        const KeyBit: TKeyBit = kb256):TBytes; static;
    class function DecryptBytes(const ABytes : TBytes; const AKey : AnsiString;
        const KeyBit: TKeyBit = kb256):TBytes; static;
  end;

implementation

uses fcRegistry, fcHash;

class function AES.StrToHex(Const str: AnsiString): AnsiString;
asm
    push ebx
    push esi
    push edi
    test eax,eax
    jz   @@Exit
    mov  esi,edx       // 保存edx值，用来产生新字符串的地址
    mov  edi,eax       // 保存原字符串
    mov  edx,[eax-4]  // 获得字符串长度
    test edx,edx      // 检查长度
    je   @@Exit      { Length(S) = 0 }
    mov  ecx,edx       // 保存长度
    Push ecx
    shl  edx,1
    mov  eax,esi
    {$IFDEF VER210}
    movzx ecx, word ptr [edi-12] { 需要设置CodePage }
    {$ENDIF}
    call System.@LStrSetLength // 设置新串长度
    mov  eax,esi       // 新字符串地址
    Call UniqueString  // 产生一个唯一的新字符串，串位置在eax中
    Pop   ecx
  @@SetHex:
    xor  edx,edx       // 清空edx
    mov  dl, [edi]     // Str字符串字符
    mov  ebx,edx       // 保存当前的字符
    shr  edx,4         // 右移4字节，得到高8位
    mov  dl,byte ptr[edx+@@HexChar] // 转换成字符
    mov  [eax],dl      // 将字符串输入到新建串中存放
    and  ebx,$0F       // 获得低8位
    mov  dl,byte ptr[ebx+@@HexChar] // 转换成字符
    inc  eax             // 移动一个字节,存放低位
    mov  [eax],dl
    inc  edi
    inc  eax
    loop @@SetHex
  @@Exit:
    pop  edi
    pop  esi
    pop  ebx
    ret
  @@HexChar: db '0123456789ABCDEF'
end;

class function AES.HexToStr(const str: AnsiString): AnsiString;
asm
  push ebx
  push edi
  push esi
  test eax,eax // 为空串
  jz   @@Exit
  mov  edi,eax
  mov  esi,edx
  mov  edx,[eax-4]
  test edx,edx
  je   @@Exit
  mov  ecx,edx
  push ecx
  shr  edx,1
  mov  eax,esi // 开始构造字符串
  {$IFDEF VER210}
  movzx ecx, word ptr [edi-12] { 需要设置CodePage }
  {$ENDIF}
  call System.@LStrSetLength // 设置新串长度
  mov  eax,esi       // 新字符串地址
  Call UniqueString  // 产生一个唯一的新字符串，串位置在eax中
  Pop   ecx
  xor  ebx,ebx
  xor  esi,esi
@@CharFromHex:
  xor  edx,edx
  mov  dl, [edi]     // Str字符串字符
  cmp  dl, '0'  // 查看是否在0到f之间的字符
  JB   @@Exit   // 小于0，退出
  cmp  dl,'9'   // 小于=9
  ja  @@DoChar// CompOkNum
  sub  dl,'0'
  jmp  @@DoConvert
@@DoChar:
  // 先转成大写字符
  and  dl,$DF
  cmp  dl,'F'
  ja   @@Exit  // 大于F退出
  add  dl,10
  sub  dl,'A'
@@DoConvert: // 转化
  inc  ebx
  cmp  ebx,2
  je   @@Num1
  xor  esi,esi
  shl  edx,4
  mov  esi,edx
  jmp  @@Num2
@@Num1:
  add  esi,edx
  mov  edx,esi
  mov  [eax],dl
  xor  ebx,ebx
  inc  eax
@@Num2:
  dec  ecx
  inc  edi
  test ecx,ecx
  jnz  @@CharFromHex
@@Exit:
  pop  esi
  pop  edi
  pop  ebx
end;

  { --  字符串加密函数 默认按照 256 位密匙加密 -- }
class function AES.EncryptString(Value: string; Key: string;
  KeyBit: TKeyBit): string;
var
{$IFDEF VER210}
  SS, DS: TMemoryStream;
{$ELSE}
  SS, DS: TStringStream;
{$ENDIF}
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  st: AnsiString;
  l_In, l_Key: AnsiString;
begin
  l_In := AnsiString(Value);
  l_Key := AnsiString(Key);

  Result := '';
{$IFDEF VER210}
  SS := TMemoryStream.Create;
  SS.WriteBuffer(PAnsiChar(l_In)^, Length(l_In));
  DS := TMemoryStream.Create;
{$ELSE}
  SS := TStringStream.Create(l_In);
  DS := TStringStream.Create('');
{$ENDIF}
  try
    Size := SS.Size;
    DS.WriteBuffer(Size, SizeOf(Size));
    { --  128 位密匙最大长度为 16 个字符 -- }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0);
      Move(PAnsiChar(l_Key)^, AESKey128, Min(SizeOf(AESKey128),
          Length(l_Key)));
      EncryptAESStreamECB(SS, 0, AESKey128, DS);
    end;
    { --  192 位密匙最大长度为 24 个字符 -- }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0);
      Move(PAnsiChar(l_Key)^, AESKey192, Min(SizeOf(AESKey192),
          Length(l_Key)));
      EncryptAESStreamECB(SS, 0, AESKey192, DS);
    end;
    { --  256 位密匙最大长度为 32 个字符 -- }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0);
      Move(PAnsiChar(l_Key)^, AESKey256, Min(SizeOf(AESKey256),
          Length(l_Key)));
      EncryptAESStreamECB(SS, 0, AESKey256, DS);
    end;
{$IFDEF VER210}
    SetLength(st, DS.Size);
    DS.Position := 0;
    DS.ReadBuffer(PAnsiChar(st)^, DS.Size);
    Result := String(StrToHex(st));
{$ELSE}
    Result := String(StrToHex(DS.DataString));
{$ENDIF}
  finally
    SS.Free;
    DS.Free;
  end;
end;

{ --  字符串解密函数 默认按照 128 位密匙解密 -- }
class function AES.DecryptString(Value: string; Key: string;
  KeyBit: TKeyBit = kb256): string;
var
  SS, DS: TStringStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
  l_Key: AnsiString;
begin
  l_Key := AnsiString(Key);
  Result := '';
  SS := TStringStream.Create(HexToStr(AnsiString(Value)));
  DS := TStringStream.Create('');
  try
    try
      Size := SS.Size;
      SS.ReadBuffer(Size, SizeOf(Size));
      { --  128 位密匙最大长度为 16 个字符 -- }
      if KeyBit = kb128 then
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(l_Key)^, AESKey128, Min(SizeOf(AESKey128),
            Length(l_Key)));
        DecryptAESStreamECB(SS, SS.Size - SS.Position, AESKey128, DS);
      end;
      { --  192 位密匙最大长度为 24 个字符 -- }
      if KeyBit = kb192 then
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(l_Key)^, AESKey192, Min(SizeOf(AESKey192),
            Length(l_Key)));
        DecryptAESStreamECB(SS, SS.Size - SS.Position, AESKey192, DS);
      end;
      { --  256 位密匙最大长度为 32 个字符 -- }
      if KeyBit = kb256 then
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(l_Key)^, AESKey256, Min(SizeOf(AESKey256),
            Length(l_Key)));
        DecryptAESStreamECB(SS, SS.Size - SS.Position, AESKey256, DS);
      end;
      //Result := Trim(DS.DataString); 不能这样用,会把最后的 #$D#$A 去掉,造成加密前与解密后字符串不相等
      Result  :=  StrPas(PChar(DS.DataString));
    except
      Result := '';
    end;
  finally
    SS.Free;
    DS.Free;
  end;
end;

{ 流加密函数, default keybit: 128bit }
class function AES.EncryptStream(Src: TStream; Key: AnsiString;
  const Dest: TStream; KeyBit: TKeyBit): Boolean;
var
  Count: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  if Dest = nil then
  begin
    raise Exception.Create(SDestStreamNotCreated);
    Result := False;
    Exit;
  end;

  try
    Src.Position := 0;
    Count := Src.Size;
    Dest.Write(Count, SizeOf(Count));
    { --  128 位密匙最大长度为 16 个字符 -- }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0);
      Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
      EncryptAESStreamECB(Src, 0, AESKey128, Dest);
    end;
    { --  192 位密匙最大长度为 24 个字符 -- }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0);
      Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
      EncryptAESStreamECB(Src, 0, AESKey192, Dest);
    end;
    { --  256 位密匙最大长度为 32 个字符 -- }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0);
      Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
      EncryptAESStreamECB(Src, 0, AESKey256, Dest);
    end;

    Result := True;
  except
    raise Exception.Create(SEncryptStreamError);
    Result := False;
  end;
end;

{ 流解密函数, default keybit: 128bit }
class function AES.DecryptStream(Src: TStream; Key: AnsiString;
  const Dest: TStream; KeyBit: TKeyBit): Boolean;
var
  Count, OutPos: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  if Dest = nil then
  begin
    raise Exception.Create(SDestStreamNotCreated);
    Result := False;
    Exit;
  end;

  try
    Src.Position := 0;
    OutPos := Dest.Position;
    Src.ReadBuffer(Count, SizeOf(Count));
    { --  128 位密匙最大长度为 16 个字符 -- }
    if KeyBit = kb128 then
    begin
      FillChar(AESKey128, SizeOf(AESKey128), 0);
      Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
      DecryptAESStreamECB(Src, Src.Size - Src.Position, AESKey128, Dest);
    end;
    { --  192 位密匙最大长度为 24 个字符 -- }
    if KeyBit = kb192 then
    begin
      FillChar(AESKey192, SizeOf(AESKey192), 0);
      Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
      DecryptAESStreamECB(Src, Src.Size - Src.Position, AESKey192, Dest);
    end;
    { --  256 位密匙最大长度为 32 个字符 -- }
    if KeyBit = kb256 then
    begin
      FillChar(AESKey256, SizeOf(AESKey256), 0);
      Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
      DecryptAESStreamECB(Src, Src.Size - Src.Position, AESKey256, Dest);
    end;
    Dest.Size := OutPos + Count;
    Dest.Position := OutPos;

    Result := True;
  except
    Result := False;
  end;
end;

{ --  文件加密函数 默认按照 128 位密匙解密 -- }
class procedure AES.EncryptFile(SourceFile, DestFile: String;
  Key: AnsiString; KeyBit: TKeyBit);
var
  SFS, DFS: TFileStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  SFS := TFileStream.Create(SourceFile, fmOpenRead);
  try
    DFS := TFileStream.Create(DestFile, fmCreate);
    try
      Size := SFS.Size;
      DFS.WriteBuffer(Size, SizeOf(Size));
      { --  128 位密匙最大长度为 16 个字符 -- }
      if KeyBit = kb128 then
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128),
            Length(Key)));
        EncryptAESStreamECB(SFS, 0, AESKey128, DFS);
      end;
      { --  192 位密匙最大长度为 24 个字符 -- }
      if KeyBit = kb192 then
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192),
            Length(Key)));
        EncryptAESStreamECB(SFS, 0, AESKey192, DFS);
      end;
      { --  256 位密匙最大长度为 32 个字符 -- }
      if KeyBit = kb256 then
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256),
            Length(Key)));
        EncryptAESStreamECB(SFS, 0, AESKey256, DFS);
      end;
    finally
      DFS.Free;
    end;
  finally
    SFS.Free;
  end;
end;

{ --  文件解密函数 默认按照 128 位密匙解密 -- }
class procedure AES.DecryptFile(SourceFile, DestFile: String;
  Key: AnsiString; KeyBit: TKeyBit);
var
  SFS, DFS: TFileStream;
  Size: Int64;
  AESKey128: TAESKey128;
  AESKey192: TAESKey192;
  AESKey256: TAESKey256;
begin
  SFS := TFileStream.Create(SourceFile, fmOpenRead);
  try
    SFS.ReadBuffer(Size, SizeOf(Size));
    DFS := TFileStream.Create(DestFile, fmCreate);
    try
      try
        { --  128 位密匙最大长度为 16 个字符 -- }
        if KeyBit = kb128 then
        begin
          FillChar(AESKey128, SizeOf(AESKey128), 0);
          Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128),
              Length(Key)));
          DecryptAESStreamECB(SFS, SFS.Size - SFS.Position, AESKey128, DFS);
        end;
        { --  192 位密匙最大长度为 24 个字符 -- }
        if KeyBit = kb192 then
        begin
          FillChar(AESKey192, SizeOf(AESKey192), 0);
          Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192),
              Length(Key)));
          DecryptAESStreamECB(SFS, SFS.Size - SFS.Position, AESKey192, DFS);
        end;
        { --  256 位密匙最大长度为 32 个字符 -- }
        if KeyBit = kb256 then
        begin
          FillChar(AESKey256, SizeOf(AESKey256), 0);
          Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256),
              Length(Key)));
          DecryptAESStreamECB(SFS, SFS.Size - SFS.Position, AESKey256, DFS);
        end;
        DFS.Size := Size;
      except
        ;
      end;
    finally
      DFS.Free;
    end;
  finally
    SFS.Free;
  end;
end;

class function AES.EncryptBytes(const ABytes: TBytes; const AKey: AnsiString;
  const KeyBit: TKeyBit): TBytes;
var
  instream, outstream : TMemoryStream;
begin
  instream  :=  TMemoryStream.Create;
  outstream :=  TMemoryStream.Create;
  try
    instream.Size :=  Length(ABytes);
    CopyMemory(instream.Memory, @ABytes[0], instream.Size);
    if EncryptStream(instream, AKey, outstream, KeyBit) then
    begin
      SetLength(Result, outstream.Size);
      CopyMemory(@Result[0], outstream.Memory, outstream.Size);
    end
    else
      SetLength(Result, 0);
  finally
    instream.Free;
    outstream.Free;
  end;
end;

class function AES.DecryptBytes(const ABytes: TBytes; const AKey: AnsiString;
  const KeyBit: TKeyBit): TBytes;
var
  instream, outstream : TMemoryStream;
begin
  instream  :=  TMemoryStream.Create;
  outstream :=  TMemoryStream.Create;
  try
    instream.Size :=  Length(ABytes);
    CopyMemory(instream.Memory, @ABytes[0], instream.Size);
    if DecryptStream(instream, AKey, outstream, KeyBit) then
    begin
      SetLength(Result, outstream.Size);
      CopyMemory(@Result[0], outstream.Memory, outstream.Size);
    end
    else
      SetLength(Result, 0);
  finally
    instream.Free;
    outstream.Free;
  end;
end;

end.
