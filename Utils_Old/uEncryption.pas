unit uEncryption;

interface

uses
  SysUtils, Classes, windows, StrUtils;

function EncryptStr(const AStr : string; const AKey : string = '') : string;
function DecryptStr(const AStr : string; const AKey : string = '') : string;
function XorStr(const AStr : string; const AKey : string = ''):string;

procedure XorEncryption(
    const AData : Pointer;
    const ADataLen : Integer;
    const AKey : Pointer;
    const AKeyLen : Integer = 0
    ); overload;
procedure XorEncryption(
    const AData : Pointer;
    const ADataLen : Integer;
    const AKey : string = ''
    ); overload;

function  EncodeData(
    const AData : Pointer;
    const ADataLen  : Integer;
    const ACharList : string = ''
    ):string; overload;
function  EncodeData(
    const AStr : string;
    const ACharList : string = ''
    ):string; overload;
function  DecodeData(
    const AStr : string;
    const ACharList : string = ''
    ):string; 

implementation

const
  DEF_KEY = 'D6FF4AFE83C64FB699EE0928B9E7F95A';
  DEF_CharList = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

var
  CodeTable : array[0..MAXBYTE] of string;
  LastCharList  : string;

function EncryptionEngine(Src: string; Key: string; Encrypt: Boolean): string;
var
  //idx         :integer;
  KeyLen: Integer;
  KeyPos: Integer;
  offset: Integer;
  dest: string;
  SrcPos: Integer;
  SrcAsc: Integer;
  TmpSrcAsc: Integer;
  Range: Integer;
  keybak: string;
  Srcbak: string;
begin
  keybak := key;
  Srcbak := Src;
  KeyLen := Length(Keybak);
  if KeyLen = 0 then
    keybak := DEF_KEY;
  KeyPos := 0;
  //SrcPos:=0;
  //SrcAsc:=0;
  Range := 256;
  if Encrypt then
  begin
    Randomize;
    offset := Random(Range);
    dest := format('%1.2x', [offset]);
    for SrcPos := 1 to Length(Srcbak) do
    begin
      SrcAsc := (Ord(Srcbak[SrcPos]) + offset) MOD 255;
      if KeyPos < KeyLen then
        KeyPos := KeyPos + 1
      else
        KeyPos := 1;
      SrcAsc := SrcAsc xor Ord(Keybak[KeyPos]);
      dest := dest + format('%1.2x', [SrcAsc]);
      offset := SrcAsc;
    end;
  end
  else
  begin
    offset := StrToInt('$' + copy(srcbak, 1, 2));
    SrcPos := 3;
    Dest  :=  '';
    while SrcPos < Length(Src) do
    begin
      SrcAsc := StrToInt('$' + copy(srcbak, SrcPos, 2));
      if KeyPos < KeyLen Then
        KeyPos := KeyPos + 1
      else
        KeyPos := 1;
      TmpSrcAsc := SrcAsc xor Ord(Keybak[KeyPos]);
      if TmpSrcAsc <= offset then
        TmpSrcAsc := 255 + TmpSrcAsc - offset
      else
        TmpSrcAsc := TmpSrcAsc - offset;
      dest := dest + chr(TmpSrcAsc);
      offset := srcAsc;
      SrcPos := SrcPos + 2;
    end;
  end;
  Result := Dest;
end;

function EncryptStr(const AStr : string; const AKey : string = '') : string;
begin
  Result  :=  EncryptionEngine(AStr, AKey, True);
end;

function DecryptStr(const AStr : string; const AKey : string = '') : string;
begin
  if length(AStr) = 0 then
    Result  :=  ''
  else
    Result  :=  EncryptionEngine(AStr, AKey, False);
end;

procedure XorEncryption(
    const AData : Pointer;
    const ADataLen : Integer;
    const AKey : Pointer;
    const AKeyLen : Integer
    );
var
  i   : Integer;
  k   : Integer;
  key : Pointer;
  keylen  : Integer;
  pd  : PByte;
  pk  : PByte;
begin
  key :=  AKey;
  keylen  :=  AKeyLen;
  if key = nil then
  begin
    key :=  @DEF_KEY[1];
    keylen  :=  Length(DEF_KEY);
  end;
  pd  :=  AData;
  pk  :=  key;
  k :=  0;
  for i := 0 to ADataLen - 1 do
  begin
    pd^ :=  pd^ xor pk^;
    Inc(pd);
    Inc(pk);
    Inc(k);
    if k = keylen then
    begin
      pk  :=  key;
      k :=  0;
    end;
  end;
end;

procedure XorEncryption(
    const AData : Pointer;
    const ADataLen : Integer;
    const AKey : string
    ); overload;
var
  key : string;
begin
  if AKey = '' then
    key :=  DEF_KEY
  else
    key :=  AKey;
  XorEncryption(AData, ADataLen, @key[1], Length(key));
end;

function XorStr(const AStr : string; const AKey : string = ''):string;
begin
  Result  :=  AStr;
  XorEncryption(@Result[1], Length(Result), AKey);
end;

function CalcCodeSize(
    const CharListSize  : Integer;
    var   lCharCount  : Integer
    ):Integer;
//计算代码长度
var
  i : Integer;
begin
  Result  :=  0;
  case CharListSize of
    0..1  : Exit;
    2     : Result  :=  8;
    3     : Result  :=  6;
    4..6  : Result  :=  4;
    7..15 : Result  :=  3;
    16..30: Result  :=  2;
    else begin
      Result := 1;
      for i := 1 to CharListSize do
        if (i + 1) * (CharListSize - i) > MAXBYTE then
        begin
          lCharCount  :=  i;
          break;
        end;
    end;
  end;
end;

procedure BuildCodeTable(
    const ACharList : string  = ''
    );
//构造字符-代码映射表
var
  i : Integer;
  j : Integer;
  clist : string;
  clen : Integer;
  codesize  : Integer;
  lcharlen  : Integer;
  idx : Integer;
  x1,x2,x3,x4,x5,x6,x7,x8 : Integer;
begin
  clist := ACharList;
  if clist = '' then clist := DEF_CharList;
  if LastCharList = clist then Exit;
  cLen :=  Length(clist);

  codesize  :=  CalcCodeSize(clen, lcharlen);
  if codesize = 0 then Exit;

  idx :=  0;
  if codesize = 1 then
  begin
    //变长代码
    for i := lcharlen + 1 to cLen do
    begin
      CodeTable[idx]  :=  clist[i];
      Inc(idx);
    end;
    for i := 1 to lcharlen do
    begin
      for j := lcharlen + 1 to cLen do
      begin
        CodeTable[idx]  :=  clist[i] + clist[j];
        Inc(idx);
        if idx > MAXBYTE then break;
      end;
      if idx > MAXBYTE then break;
    end;
  end
  else begin
    //定长代码,2-8
    for x1 := 1 to clen do
    begin
      for x2  :=  1 to clen do
      begin
        if codesize = 2 then
        begin
          CodeTable[idx]  :=  clist[x1] + clist[x2];
          Inc(idx);
          if idx > MAXBYTE then break;
          Continue;
        end;
        for x3  :=  1 to clen do
        begin
          if codesize = 3 then
          begin
            CodeTable[idx]  :=  clist[x1] + clist[x2] + clist[x3];
            Inc(idx);
            if idx > MAXBYTE then break;
            Continue;
          end;
          for x4  :=  1 to clen do
          begin
            if codesize = 4 then
            begin
              CodeTable[idx]  :=  clist[x1] + clist[x2] + clist[x3] + clist[x4];
              Inc(idx);
              if idx > MAXBYTE then break;
              Continue;
            end;
            for x5 :=  1 to clen do
            begin
              if codesize = 5 then
              begin
                CodeTable[idx]  :=  clist[x1] + clist[x2] + clist[x3] + clist[x4] + clist[x5];
                Inc(idx);
                if idx > MAXBYTE then break;
                Continue;
              end;
              for x6  :=  1 to clen do
              begin
                if codesize = 6 then
                begin
                  CodeTable[idx]  :=  clist[x1] + clist[x2] + clist[x3] + clist[x4] + clist[x5] + clist[x6];
                  Inc(idx);
                  if idx > MAXBYTE then break;
                  Continue;
                end;
                for x7 := 1 to clen do
                begin
                  if codesize = 7 then
                  begin
                    CodeTable[idx]  :=  clist[x1] + clist[x2] + clist[x3] + clist[x4] + clist[x5] + clist[x6] + clist[x7];
                    Inc(idx);
                    if idx > MAXBYTE then break;
                    Continue;
                  end;
                  for x8 := 1 to clen do
                  begin
                    CodeTable[idx]  :=  clist[x1] + clist[x2] + clist[x3] + clist[x4] + clist[x5] + clist[x6] + clist[x7] + clist[x8];
                    Inc(idx);
                    if idx > MAXBYTE then break;
                  end;
                  if idx > MAXBYTE then break;
                end;
                if idx > MAXBYTE then break;
              end;
              if idx > MAXBYTE then break;
            end;
            if idx > MAXBYTE then break;
          end;
          if idx > MAXBYTE then break;
        end;
        if idx > MAXBYTE then break;
      end;
      if idx > MAXBYTE then break;
    end;
  end;
  LastCharList  :=  clist;
end;

function  EncodeData(
    const AData : Pointer;
    const ADataLen  : Integer;
    const ACharList : string = ''
    ):string; overload;
var
  i : Integer;
  p : PChar;
begin
  BuildCodeTable(ACharList);
  Result  :=  '';
  p :=  AData;
  for i := 0 to ADataLen - 1 do
  begin
    Result  :=  Result + CodeTable[Ord(p^)];
    Inc(p);
  end;
end;

function  EncodeData(
    const AStr : string;
    const ACharList : string = ''
    ):string; overload;
begin
  Result  :=  EncodeData(@AStr[1], Length(AStr), ACharList);
end;

function  DecodeData(
    const AStr : string;
    const ACharList : string = ''
    ):string;
var
  clist : string;
  cLen  : Integer;
  codesize  : Integer;
  i : Integer;
  lcharlen  : Integer;
  idx : Integer;
  code  : string;
begin
  Result  :=  '';
  clist := ACharList;
  if clist = '' then clist := DEF_CharList;
  cLen :=  Length(clist);
  
  codesize  :=  CalcCodeSize(clen, lcharlen);
  if codesize = 0 then Exit;
  
  BuildCodeTable(ACharList);

  idx :=  1;
  if codesize = 1 then
  begin
    //变长代码
    while idx < Length(AStr) do
    begin
      code  :=  AStr[idx];
      Inc(idx);
      if Pos(code, clist) <= lcharlen then
      begin
        if idx > Length(AStr) then break;
        code  :=  code + AStr[idx];
        Inc(idx);
      end;
      for i := 0 to MAXBYTE do
        if CodeTable[i] = code then
        begin
          Result  :=  Result + Chr(i);
          break;
        end;
    end;
  end
  else begin
    //定长代码
    while idx < Length(AStr) do
    begin
      code  :=  MidStr(AStr, idx, codesize);
      Inc(idx, codesize);
      for i := 0 to MAXBYTE do
        if CodeTable[i] = code then
        begin
          Result  :=  Result + Chr(i);
          break;
        end;
    end;
  end;
end;

end.

