unit fcConvert;

interface

uses
  SysUtils, Windows, Classes, StrUtils, Messages, ActiveX, DateUtils, Variants;

type
  Convert = record
    class function  VarRecToVariant(V: TVarRec): Variant; static;
    class function  StreamToVariant(const AStream : TStream) : Variant; static;
    class function  FileToVariant(const AFileName : string) : Variant; static;
    class procedure VariantToStream(const AValue : Variant; const AStream : TStream); static;
    class procedure VariantToFile(const AValue : Variant; const AFileName : string); static;
    class procedure StringToSafeArray(const AStr : string; var AData : OleVariant); static;
    class procedure SafeArrayToString(const AData : OleVariant; var AStr : string); static;
    class procedure BytesToSafeArray(const ABytes : TBytes; var AData : OleVariant); static;
    class procedure SafeArrayToBytes(const AData : OleVariant; var ABytes : TBytes); static;
    class function  VariantToInteger(const AValue : Variant; const ADefValue : Integer = 0):Integer; static;
    class function  VariantToBoolean(const AValue : Variant; const ADefValue : Boolean  = False):Boolean; static;
    class function  BytesToHexStr(const ABytes : TBytes):string; static;
    class function  HexStrToBytes(const AStr : string):TBytes; static;
    class function  ByteArrayToBytes(const AData : Variant):TBytes; static;
    class function  CurrencyToRMB(const AValue : Currency):string; static;
  end;

implementation

uses
  uCFConsts, uCFResource;

{ CFFunc }

class function Convert.StreamToVariant(const AStream : TStream) : Variant;
var
  p : Pointer;
begin
  Result  :=  VarArrayCreate([0, AStream.Size - 1], varByte);
  p :=  VarArrayLock(Result);
  try
    AStream.Position  :=  0;
    AStream.ReadBuffer(p^, AStream.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;

class function Convert.FileToVariant(const AFileName: string): Variant;
var
  fs : TFileStream;
begin
  fs  :=  TFileStream.Create(AFileName, fmOpenRead);
  try
    Result :=  Convert.StreamToVariant(fs);
  finally
    fs.Free;
  end;
end;

class procedure Convert.VariantToStream(const AValue: Variant;
  const AStream: TStream);
var
  p : Pointer;
begin
  p :=  VarArrayLock(AValue);
  try
    AStream.Write(p^, VarArrayHighBound(AValue, 1) - VarArrayLowBound(AValue, 1) + 1);
  finally
    VarArrayUnlock(AValue);
  end;
end;

class function Convert.VariantToBoolean(const AValue: Variant; const ADefValue: Boolean): Boolean;
begin
  Result  :=  ADefValue;
  if VarIsEmpty(AValue) or VarIsNull(AValue) then
    Exit;
  if VarIsNumeric(AValue) then
    Result  :=  AValue <> 0
  else if VarIsStr(AValue) then
    Result  :=  SameText(AValue, 'true') or (StrToIntDef(AValue, 0) <> 0)
  else
    try
      Result  :=  AValue;
    except
    end;
end;

class procedure Convert.VariantToFile(const AValue: Variant; const AFileName: string);
var
  fs : TFileStream;
begin
  if FileExists(AFileName) then
    DeleteFile(PChar(AFileName))
  else
    ForceDirectories(ExtractFilePath(AFileName));
  fs  :=  TFileStream.Create(AFileName, fmCreate);
  try
    Convert.VariantToStream(AValue, fs);
  finally
    fs.Free;
  end;
end;

class function Convert.VariantToInteger(const AValue: Variant; const ADefValue : Integer): Integer;
begin
  if VarIsNumeric(AValue) then
    Result  :=  Integer(AValue)
  else if VarIsStr(AValue) then
    Result  :=  StrToIntDef(AValue, ADefValue)
  else
    Result  :=  ADefValue;
end;

class function Convert.VarRecToVariant(V: TVarRec): Variant;
begin
  case v.VType of
    vtInteger       : Result := v.VInteger;
    vtBoolean       : Result := v.VBoolean;
    vtChar          : Result := v.VChar;
    vtExtended      : Result := v.VExtended^;
    vtString        : Result := v.VString^;
    vtPointer       : Result := Integer(v.VPointer);
    vtPChar         : Result := Integer(v.VPChar);
    vtObject        : Result := v.VInteger;
    vtClass         : Result := Integer(v.VClass);
    vtWideChar      : Result := v.VWideChar;
    vtPWideChar     : Result := Integer(v.VPWideChar);
    vtAnsiString    : Result := AnsiString(v.VAnsiString);
    vtCurrency      : Result := v.VCurrency^;
    vtVariant       : Result := v.VVariant^;
    vtInterface     : Result := IInterface(v.VInterface);
    vtWideString    : Result := WideString(v.VWideString^);
    vtInt64         : Result := v.VInt64^;
    vtUnicodeString : Result := string(v.VUnicodeString);
  else
    Result := Null;
  end;
end;

class procedure Convert.StringToSafeArray(const AStr: string;
  var AData: OleVariant);
var
  sa : PSafeArray;
  bound : TSafeArrayBound;
  p : Pointer;
begin
  bound.lLbound :=  0;
  bound.cElements :=  Length(AStr) * SizeOf(Char);
  sa  :=  SafeArrayCreate(VT_UI1, 1, bound);

  if S_OK = SafeArrayAccessData(sa, p) then
  try
    ZeroMemory(p, bound.cElements);
    CopyMemory(p, @AStr[1], bound.cElements);
    TVarData(AData).VType :=  VT_ARRAY or VT_UI1;
    TVarData(AData).VAny  :=  sa;
  finally
    SafeArrayUnaccessData(sa);
  end
  else
    AData :=  Null;
end;

class procedure Convert.SafeArrayToString(const AData: OleVariant;
  var AStr: string);
var
  sa : PSafeArray;
  l, u : Integer;
  p : Pointer;
begin
  if (TVarData(AData).VType = VT_ARRAY or VT_UI1) then
  begin
    sa  :=  TVarData(AData).VAny;

    if S_OK <> SafeArrayGetLBound(sa, 1, l) then Exit;
    if S_OK <> SafeArrayGetUBound(sa, 1, u) then Exit;
    if (u = 0) then
    begin
      AStr  :=  '';
      Exit;
    end;
    if S_OK = SafeArrayAccessData(sa, p) then
    try
      SetLength(AStr, (u - l + 1) div SizeOf(Char));
      CopyMemory(@AStr[1], p, u - l + 1);
    finally
      SafeArrayUnaccessData(sa);
    end;
  end
  else
    AStr  :=  '';
end;

class procedure Convert.BytesToSafeArray(const ABytes: TBytes;
  var AData: OleVariant);
var
  sa : PSafeArray;
  bound : TSafeArrayBound;
  p : Pointer;
begin
  bound.lLbound :=  0;
  bound.cElements :=  High(ABytes) - Low(ABytes) + 1;

  sa  :=  SafeArrayCreate(VT_UI1, 1, bound);
  if S_OK = SafeArrayAccessData(sa, p) then
  try
    ZeroMemory(p, bound.cElements);
    CopyMemory(p, @ABytes[0], bound.cElements);
    TVarData(AData).VType :=  VT_ARRAY or VT_UI1;
    TVarData(AData).VAny  :=  sa;
  finally
    SafeArrayUnaccessData(sa);
  end
  else
    AData :=  Null;
end;

class function Convert.CurrencyToRMB(const AValue: Currency): string;
//金额转换为人民币
const
  BigNumber='零壹贰叁肆伍陆柒捌玖';
  BigUnit='万仟佰拾亿仟佰拾万仟佰拾元';
       {共可表示13位金额}
var
  nLeft, nRigth, lTemp, rTemp, BigNumber1, BigUnit1, RMB,s: string;
  I: Integer;
  isminus : Boolean;
begin  {取整数和小数部分}
  isminus :=  AValue < 0;
  if isminus then
    RMB := FormatCurr('0.00', -AValue)
  else
    RMB := FormatCurr('0.00', AValue);
  nLeft:=copy(RMB, 1, Pos('.', RMB) - 1);
  nRigth:=copy(RMB, Pos('.', RMB) + 1, 2);
    {转换整数部分}
  if nLeft<>'0' then
    for I:=1 to Length(nLeft) do
    begin
      BigNumber1:=copy(BigNumber, StrToInt(nLeft[I]) + 1, 1);
      BigUnit1:=copy(BigUnit, (Length(BigUnit) - Length(nleft) + I - 1) + 1, 1);
      s := copy(lTemp, Length(lTemp)-1, 1);
      if (BigNumber1='零') and (s='零') then
        lTemp:=copy(lTemp, 1, Length(lTemp) - 2);
      if (BigNumber1='零') and ((BigUnit1='亿') or (BigUnit1='万') or (BigUnit1='元')) then
      begin
        BigNumber1:=BigUnit1;
        if BigUnit1<>'元' then
          BigUnit1:='零'
        else
          BigUnit1:='';
      end;
      if (BigNumber1='零') and (BigUnit1<>'亿') and (BigUnit1<>'万') and (BigUnit1<>'元') then
        BigUnit1:='';
      lTemp:=lTemp + BigNumber1 + BigUnit1;
    end;
  lTemp:=StringReplace(lTemp, '零零', '零', [rfReplaceAll]);
  lTemp:=StringReplace(lTemp, '零零', '零', [rfReplaceAll]);
  lTemp:=StringReplace(lTemp, '零元', '元', [rfReplaceAll]);
  lTemp:=StringReplace(lTemp, '零万', '万', [rfReplaceAll]);
  lTemp:=StringReplace(lTemp, '零亿', '亿', [rfReplaceAll]);
  lTemp:=StringReplace(lTemp, '亿万', '亿', [rfReplaceAll]);
  {转换小数部分}
  if StrToInt(nRigth[1])<>0 then
    rTemp:=copy(BigNumber, StrToInt(nRigth[1]) * 1 + 1, 1) + '角';
  if StrToInt(nRigth[2])<>0 then
  begin
    if (nLeft<>'0') and (StrToInt(nRigth[1])=0) then
      rTemp:='零';
    rTemp:=rTemp + copy(BigNumber, StrToInt(nRigth[2]) * 1 + 1, 1) + '分';
    RMB:=lTemp + rTemp;
  end
  else
    RMB :=lTemp + rTemp + '整';
  if RMB = '整' then
    RMB :=  '零元整';
  if isminus then
    RMB :=  '负' + RMB;
  Result := RMB;
end;

class procedure Convert.SafeArrayToBytes(const AData: OleVariant;
  var ABytes: TBytes);
var
  sa : PSafeArray;
  l, u : Integer;
  p : Pointer;
begin
  SetLength(ABytes, 0);
  if (TVarData(AData).VType = VT_ARRAY or VT_UI1) then
  begin
    sa  :=  TVarData(AData).VAny;

    if S_OK <> SafeArrayGetLBound(sa, 1, l) then Exit;
    if S_OK <> SafeArrayGetUBound(sa, 1, u) then Exit;
    if (u = 0) then
      Exit;
    if S_OK = SafeArrayAccessData(sa, p) then
    try
      SetLength(ABytes, u - l + 1);
      CopyMemory(@ABytes[0], p, u - l + 1);
    finally
      SafeArrayUnaccessData(sa);
    end;
  end;
end;

class function Convert.ByteArrayToBytes(const AData: Variant): TBytes;
var
  p : Pointer;
  Size : Integer;
begin
  SetLength(Result, 0);
  if VarIsArray(AData) then
  begin
    Size  :=  VarArrayHighBound(AData, 1) - VarArrayLowBound(AData, 1) + 1;
    SetLength(Result, Size);
    p :=  VarArrayLock(AData);
    try
      CopyMemory(@Result[0], p, Size);
    finally
      VarArrayUnlock(AData);
    end;
  end;
end;

class function Convert.BytesToHexStr(const ABytes: TBytes): string;
var
  i : Integer;
begin
  Result  :=  '';
  for i := Low(ABytes) to High(ABytes) do
    Result  :=  Result + IntToHex(ABytes[i], 2);
end;

class function Convert.HexStrToBytes(const AStr: string): TBytes;
var
  Size : Integer;
begin
  Size  :=  Length(astr) div 2;
  SetLength(Result, Size);
  HexToBin(PChar(AStr), @Result[0], Size);
end;

end.
