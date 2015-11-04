unit fcEmail;

interface

uses
  ExtCtrls, Windows, Controls, SysUtils, Classes;

type
  Email = record
    /// <summary>
    /// 判断是否是正确的标准邮件地址。
    /// </summary>
    /// <param name="ABasicEmail">待验证的标准邮件地址</param>
    /// <returns>是否有效</returns>
    class function IsValid(const ABasicEmail: String): Boolean; static;

    /// <summary>
    /// 获取标准的邮件地址。
    /// 通常邮件地址可以是以下三种：
    /// 标准的邮件地址（BasicAddress），如 woodstock@263.net
    /// 扩展的邮件地址（FullAddress、Address）,如 "曾溪泉"<woodstock@263.net>  、 "曾溪泉" <woodstock@263.net> 、 曾溪泉<woodstock@263.net>
    /// 别名  AliasName :  如上面“扩展的邮件地址（FullMailAddress）” 中的 “曾溪泉”
    /// 这里，FullAddress = AliasName + BasicAddress
    /// </summary>
    /// <param name="AEmail">原始邮件地址</param>
    /// <returns>标准的邮件地址</returns>
    /// 输出只会是一个地址！！
    class function GetBasicEmail(const AEmail: String): string; static;

    /// <summary>
    /// 获取邮件中的别名。如果没有设置别名，将返回邮件地址用户名。
    /// </summary>
    /// <param name="AEmail"></param>
    /// <returns></returns>
    class function GetAlias(const AEmail: string): string; static;

    /// <summary>
    /// 是否群组标签，以%开头。
    /// </summary>
    /// <param name="AEmail"></param>
    /// <returns></returns>
    class function IsGroupMicro(const AEmail: string): Boolean; static;

    /// <summary>
    /// 转换多个邮件地址为标准地址。
    /// </summary>
    /// <param name="AEmailList"></param>
    /// <returns></returns>
    class function GetBasicEmailList(const AEmailList: string): string; static;

    /// <summary>
    /// 获得 @ 后面的部分。
    /// </summary>
    /// <param name="ABasicEmail">邮件地址</param>
    /// <returns>主机地址</returns>
    class function GetHostName(const ABasicEmail: String): String; static;

    /// <summary>
    /// 将Email转化为List，Email之间用';'或者','分隔。
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="AList"></param>
    class procedure EmailToList(const AStr: string; const AList: TStringList); static;

    class function Merge(const AEmailList1, AEmailList2: string): string; static;

    class function GetFormatSize(const ASizeInByte: Integer): string; static;

  end;

implementation

uses
  fcStr, FReplace;

{ Email }

class function Email.GetAlias(const AEmail: string): string;
var
  l_old, l_email, l_addr: string;
  l_pos: Integer;
begin
  l_old := Trim(AEmail);

  l_email := GetBasicEmail(l_old);

  if ('' = l_email) then
  begin
    Result := l_old;
    Exit;
  end;

  l_pos := Pos(l_email, l_old);
  if (1 = l_pos) then
  begin
    Result := Str.GetTokenBeforeChar(l_email, '@');
    Exit;
  end;

  l_pos := Pos('<'+l_email, l_old);
  if l_pos <=0 then
    l_pos := Pos(l_email, l_old);

  l_addr := Trim(Copy(l_old, 1, l_pos - 1));
  while ('' <> l_addr) and ('<' = l_addr[Length(l_addr)]) do
    l_addr := Copy(l_addr, 1, Length(l_addr) - 1);

  l_addr := Trim(l_addr);

  while (Length(l_addr) >= 2) and (l_addr[1] = '"') and (l_addr[Length(l_addr)] = '"') do
    l_addr := Copy(l_addr, 2, Length(l_addr) - 2);

  while (Length(l_addr) >= 2) and (l_addr[1] = '''') and (l_addr[Length(l_addr)] = '''') do
    l_addr := Copy(l_addr, 2, Length(l_addr) - 2);

  if (l_addr='')
    or ( (Length(l_addr)=1) and CharInSet(l_addr[1], ['"', '''']) )
  then
    Result := Str.GetTokenBeforeChar(l_email, '@')
  else
    Result := l_addr;
end;

class function Email.GetBasicEmail(const AEmail: String): string;
var
  AInputAddress: string;
  lastCharIsBranket: Boolean;
  l_Idx, l_pos: Integer;
begin
  AInputAddress := Trim(AEmail);
  Result := '';

  // 如果最后一个字符是尖括号，则从最后取回来
  if (AInputAddress <> '') and (AInputAddress[Length(AInputAddress)] = '>') then
  begin
    lastCharIsBranket := true;
    for l_Idx := Length(AInputAddress) - 1 downto 1 do
    begin
      if CharInSet(AInputAddress[l_Idx], ['<', '>', ' ']) then
      begin
        if lastCharIsBranket then
          Continue
        else
          Break;
      end;
      Result := AInputAddress[l_Idx] + Result;
      lastCharIsBranket := false;
    end;
  end;

  if Pos('@', Result) >= 1 then
    Exit;

  // "697336<wood>" <697336@qq.com>;
  // "woodstock,zeng""" <11@263.net>,
  while (AInputAddress <> '') and (AInputAddress[1] = '"') do
  begin
    l_pos := FastPosNoCase(AInputAddress, '"', Length(AInputAddress), 1, 2);
    if l_pos > 1 then
      AInputAddress := Copy(AInputAddress, l_pos + 1, Length(AInputAddress))
    else
      AInputAddress := Copy(AInputAddress, 2, Length(AInputAddress));
  end;

  l_pos := Pos('<', AInputAddress);
  if l_pos > 0 then
  begin
    Result := Copy(AInputAddress, l_pos + 1, Length(AInputAddress));
  end
  else
    Result := AInputAddress;

  l_pos := Pos('>', Result);
  if l_pos = 1 then
    Result := ''
  else if l_pos > 1 then
    Result := Copy(Result, 1, l_pos - 1);

  if Pos('@', Result) <= 0 then
    Result := '';
end;

class function Email.GetBasicEmailList(const AEmailList: string): string;
var
  l_Idx: Integer;
  l_AddressList: TStringList;
  l_Line: string;
begin
  Result := '';
  if AEmailList = '' then
    Exit;

  l_AddressList := TStringList.Create;
  try
    EmailToList(AEmailList, l_AddressList);

    for l_Idx := 0 to l_AddressList.Count - 1 do
    begin
      if IsGroupMicro(l_AddressList[l_Idx]) then
        l_Line := l_AddressList[l_Idx]
      else
        l_Line := GetBasicEmail(l_AddressList[l_Idx]);

      if '' <> l_Line then
      begin
        if Result = '' then
          Result := l_Line
        else
          Result := Result + ',' + l_Line;
      end;
    end;
  finally
    l_AddressList.Free;
  end;
end;

class function Email.GetFormatSize(const ASizeInByte: Integer): string;
begin
  if ASizeInByte > 1024 * 1024 then
    Result := Format('%.1f M', [ASizeInByte / 1024 / 1024])
  else if ASizeInByte > 1024 then
    Result := Format('%.1f K', [ASizeInByte / 1024])
  else
    Result := Format('%d B', [ASizeInByte]);
end;

class function Email.GetHostName(const ABasicEmail: String): String;
begin
  Result := Str.GetTokenAfterChar(ABasicEmail, '@', true, true);
  if Result = '' then
    Exit;

  Result := Lowercase(Result);

  // 判断是woodstock@www.softtend.com 这是错误的，去掉www
  if Str.IsStartWith('www.', Result, true) then
    Result := Copy(Result, 5);
end;

class function Email.IsGroupMicro(const AEmail: string): Boolean;
begin
  Result := (AEmail <> '') and (AEmail[1] = '%');
end;

class function Email.IsValid(const ABasicEmail: String): Boolean;
var
  l_Chars: string;
  l_C: Char;
  l_AtCount: Integer;
begin
  // 因为出现有些信箱是 woodstock@phil这样的格式，多数是内部信箱
  Result := (Pos('@', ABasicEmail) > 1)
      and (not Str.IsEndWith('@', ABasicEmail, false))
      and (not Str.IsEndWith('.', ABasicEmail, false))
      and (not Str.IsStartWith('@', ABasicEmail, false))
      and (not Str.IsStartWith('.', ABasicEmail, false))
      and (Str.NPos('@', ABasicEmail)=1)
      and (Pos('..', ABasicEmail)<=0);

  if Result then
  begin
    // 这样会限制中文邮件地址，比如：溪泉@263.net
    // 增加了%的支持，某些OA系统用到这个字符
    l_Chars := 'abcdefghijklmnopqrstuvwxyz._-@1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ%';
    l_AtCount := 0;
    for l_C in ABasicEmail do
    begin
      if Pos(l_C, l_Chars) <= 0 then
        Exit(false);
      if l_C = '@' then
        Inc(l_AtCount);
    end;

    if l_AtCount <> 1 then
      Exit(False);
  end;
end;

class function Email.Merge(const AEmailList1, AEmailList2: string): string;
var
  l_full1, l_full2, l_basic1, l_basic2: TStringList;
  l_Idx: Integer;

  procedure _EmailToList(const Addr: string; const AFullList, ABasicList: TStringList);
  var
    I: Integer;
  begin
    EmailToList(Addr, AFullList);
    for I := 0 to AFullList.Count - 1 do
    begin
      if Email.IsGroupMicro(AFullList[I]) then
        ABasicList.Add(Lowercase(AFullList[I]))
      else
        ABasicList.Add(Lowercase(Email.GetBasicEmail(AFullList[I])));
    end;
  end;

begin
  l_full1 := TStringList.Create;
  l_full2 := TStringList.Create;
  l_basic1 := TStringList.Create;
  l_basic2 := TStringList.Create;
  try
    _EmailToList(AEmailList1, l_full1, l_basic1);
    _EmailToList(AEmailList2, l_full2, l_basic2);
    for l_Idx := 0 to l_basic2.Count - 1 do
    begin
      if (l_basic2[l_Idx] <> '') and (l_basic1.IndexOf(l_basic2[l_Idx]) = -1) then
        l_full1.Add(l_full2[l_Idx]);
    end;
    Result := Str.ListToStr(l_full1, ', ');
  finally
    l_full1.Free;
    l_full2.Free;
    l_basic1.Free;
    l_basic2.Free;
  end;
end;

class procedure Email.EmailToList(const AStr: string; const AList: TStringList);
begin
  Str.StrToList(AStr, AList, ';,');
end;

end.
