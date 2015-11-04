unit dmStringUtils;

interface
uses ExtCtrls, Windows, Controls, SysUtils, Classes, Generics.Collections ;

function dmstrRight(const S: string; Count: Integer): string; //获得一个字符串后面几个字符
function dmstrIsEndWith(const ASubStr, AStr : String; ACaseSensitive : Boolean) : Boolean;
function dmstrIsStartWith(const ASubStr, AStr : String; ACaseSensitive : Boolean) : Boolean;
function dmstrGetTokenBeforeChar(AStr, ASubStr: String;
  const AResultTrim: Boolean = True; const AEmptyIfNotFind: Boolean = True): string;
function dmstrGetTokenAfterChar(AStr, ASubStr: String;
  const AResultTrim: Boolean = True; const AEmptyIfNotFind: Boolean = True): string;
procedure dmstrGetTokenToList(const Str, ASplitStr: String; AToList: TList<String>; const ADefItemCount: Integer=-1; const ATrim: Boolean=true);

function PosInSmallIntArray(const ASearchInt: SmallInt; const AArray: array of SmallInt): Integer;
function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer;

implementation

function dmstrRight(const S: string; Count: Integer): string; //获得一个字符串后面几个字符
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

function dmstrIsEndWith(const ASubStr, AStr : String; ACaseSensitive : Boolean) : Boolean;
var
  l_RightStr : String;
begin
  if ('' = ASubStr) or ('' = AStr) then
    Exit(False);

  l_RightStr := dmstrRight(AStr, Length(ASubStr));
  if ACaseSensitive then
    Result := ASubStr = l_RightStr
  else
    Result := UpperCase(ASubStr) = UpperCase(l_RightStr);
end;

function dmstrIsStartWith(const ASubStr, AStr : String; ACaseSensitive : Boolean) : Boolean;
begin
  if ('' = ASubStr) or ('' = AStr) then
    Exit(False);

  if ACaseSensitive then
    Result := Pos(ASubStr, AStr) =1
  else
    Result := Pos(UpperCase(ASubStr), UpperCase(AStr)) =1
end;

//取得某一符号前的串
//WantTrim : 返回值是否要Trim一下
//DefaultEmpty : 当不匹配是否返回空值
function dmstrGetTokenBeforeChar(AStr, ASubStr: String;
  const AResultTrim: Boolean = True; const AEmptyIfNotFind: Boolean = True): string;
var
  I: Integer;
begin
  I := Pos(ASubStr, AStr);
  if I <> 0 then
    Result := Copy(AStr, I + Length(ASubStr), Length(AStr))
  else if AEmptyIfNotFind then
    Result := ''
  else
    Result := AStr;

  if AResultTrim then
    Result := Trim(Result);
end;

//取得某一符号前的串
//WantTrim : 返回值是否要Trim一下
//DefaultEmpty : 当不匹配是否返回空值
function dmstrGetTokenAfterChar(AStr, ASubStr: String;
  const AResultTrim: Boolean = True; const AEmptyIfNotFind: Boolean = True):string;
var
  I: Integer;
begin
  I := Pos(ASubStr, AStr);

  if I <> 0 then
    Result := Copy(AStr, 1, I - 1)
  else if AEmptyIfNotFind then
    Result := ''
  else
    Result := AStr;

  if AResultTrim then
    Result := Trim(Result);
end;

procedure dmstrGetTokenToList(const Str, ASplitStr: String; AToList: TList<String>; const ADefItemCount: Integer; const ATrim: Boolean);
var
  l_Pos: Integer;
  l_NewStr, l_Str: String;
begin
  AToList.Clear;

  l_NewStr := Str;
  repeat
    l_Pos := Pos(ASplitStr, l_NewStr);
    if l_Pos > 0 then
    begin
      l_Str := Copy(l_NewStr, 1, l_Pos-1);
      if ATrim then
        l_Str := Trim(l_Str);
      AToList.Add(l_Str);

      l_NewStr := Copy(l_NewStr, l_Pos+Length(ASplitStr), Length(l_NewStr)-l_Pos);
      if ATrim then
        l_NewStr := Trim(l_NewStr);
    end
    else
    begin
      if ATrim then
        AToList.Add(Trim(l_NewStr))
      else
        AToList.Add(l_NewStr);

      l_NewStr := '';
    end;
  until l_NewStr='';

  if ADefItemCount > 0 then
    while AToList.Count < ADefItemCount do
      AToList.Add('');
end;

function PosInSmallIntArray(const ASearchInt: SmallInt; const AArray: array of SmallInt): Integer;
begin
  for Result := Low(AArray) to High(AArray) do
  begin
    if ASearchInt = AArray[Result] then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

{This searches an array of string for an occurance of SearchStr}
function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer;
begin
  for Result := Low(Contents) to High(Contents) do
  begin
    if CaseSensitive then
    begin
      if SearchStr = Contents[Result] then
      begin
        Exit;
      end;
    end
    else
    begin
      if AnsiCompareText(SearchStr, Contents[Result])=0 then
      begin
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

end.
