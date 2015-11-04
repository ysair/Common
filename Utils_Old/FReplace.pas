unit FReplace;

interface

uses
  SysUtils, Classes;

function FastPos(const aSourceString, aFindString: String; const aSourceLen, aFindLen,
  StartPos: Integer): Integer; overload;

function FastPosNoCase(const aSourceString, aFindString: String; const aSourceLen, aFindLen,
  StartPos: Integer): Integer; overload;

function FastPos(const aSourceString, aFindString: String; StartPos: Integer = 1): Integer;
  overload;

function FastPosNoCase(const aSourceString, aFindString: String; StartPos: Integer = 1): Integer;
  overload;

function FastReplace(const aSourceString: String; const aFindString, aReplaceString: String;
  CaseSensitive: Boolean = False): String;

implementation

uses
  StrUtils, Windows;

type
  TCharUpCaseTable = array [Char] of Char;

var
  CharUpCaseTable: TCharUpCaseTable;

procedure InitCharUpCaseTable(var Table: TCharUpCaseTable);
var
  N: cardinal;
begin
  for N := 0 to Length(Table) - 1 do
    Table[Char(N)] := Char(N);
  CharUpperBuff(@Table, Length(Table));
end;

function InsensPosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  N: Integer;
  SubStrLength: Integer;
  SLength: Integer;
label Fail;
begin
  SLength := Length(S);
  if (SLength > 0) and (Offset > 0) then
  begin
    SubStrLength := Length(SubStr);
    result := Offset;
    while SubStrLength <= SLength - result + 1 do
    begin
      for N := 1 to SubStrLength do
        if CharUpCaseTable[SubStr[N]] <> CharUpCaseTable[S[result + N - 1]] then
          goto Fail;
      exit;
    Fail :
      Inc(result);
    end;
  end;
  result := 0;
end;

function FastPos(const aSourceString, aFindString: String; const aSourceLen, aFindLen,
  StartPos: Integer): Integer;
begin
  // woodstock
  if (aFindString = '') or (aSourceString = '') or (aFindLen > aSourceLen) then
  begin
    result := 0;
    exit;
  end;

  result := PosEx(aFindString, aSourceString, StartPos);
end;

function FastPosNoCase(const aSourceString, aFindString: String; const aSourceLen, aFindLen,
  StartPos: Integer): Integer;
begin
  // woodstock
  if (aFindString = '') or (aSourceString = '') or (aFindLen > aSourceLen) then
  begin
    result := 0;
    exit;
  end;

  result := InsensPosEx(aFindString, aSourceString, StartPos);
end;

function FastPos(const aSourceString, aFindString: String; StartPos: Integer): Integer;
begin
  result := FastPos(aSourceString, aFindString, Length(aSourceString), Length(aFindString),
    StartPos);
end;

function FastPosNoCase(const aSourceString, aFindString: String; StartPos: Integer): Integer;
begin
  result := FastPosNoCase(aSourceString, aFindString, Length(aSourceString), Length(aFindString),
    StartPos);
end;

function FastReplace(const aSourceString: String; const aFindString, aReplaceString: String;
  CaseSensitive: Boolean = False): String;
var
  l_SrcLen, l_FindLen, l_ReplaceLen, l_DestLen, l_StartPos, l_Pos, l_Cur, l_Idx, j, l_pSrc,
    l_pDest: Integer;
  l_List: TStringList;
begin
  if aFindString = '' then
  begin
    if aSourceString = '' then
      result := aReplaceString
    else
      result := aSourceString;
    exit;
  end;

  if aSourceString = '' then
  begin
    result := '';
    exit;
  end;

  l_SrcLen := Length(aSourceString);
  l_FindLen := Length(aFindString);
  l_ReplaceLen := Length(aReplaceString);
  l_List := TStringList.Create;
  try
    l_StartPos := 1;

    repeat
      if CaseSensitive then
        l_Pos := FastPos(aSourceString, aFindString, l_SrcLen, l_FindLen, l_StartPos)
      else
        l_Pos := FastPosNoCase(aSourceString, aFindString, l_SrcLen, l_FindLen, l_StartPos);
      if l_Pos >= l_StartPos then
      begin
        l_List.add(IntToStr(l_Pos));
        l_StartPos := l_Pos + l_FindLen
      end
      else
        l_Pos := -1;
    until l_Pos <= 0;

    if l_List.Count > 0 then
    begin
      // 计算最后的长度
      l_DestLen := l_SrcLen + l_List.Count * (l_ReplaceLen - l_FindLen);
      SetLength(result, l_DestLen);

      l_pSrc := 1;
      l_pDest := 1;
      for l_Idx := 0 to l_List.Count - 1 do
      begin
        l_Cur := StrToInt(l_List[l_Idx]);
        while l_pSrc < l_Cur do
        begin
          result[l_pDest] := aSourceString[l_pSrc];
          Inc(l_pDest);
          Inc(l_pSrc);
        end;
        for j := 1 to l_ReplaceLen do
        begin
          result[l_pDest] := aReplaceString[j];
          Inc(l_pDest);
        end;

        Inc(l_pSrc, l_FindLen);
      end;

      while l_pSrc <= l_SrcLen do
      begin
        result[l_pDest] := aSourceString[l_pSrc];
        Inc(l_pDest);
        Inc(l_pSrc);
      end;

      // Result[l_pDest] := #0;
    end
    else
      result := aSourceString;
  finally
    l_List.Free;
  end;
end;

initialization

InitCharUpCaseTable(CharUpCaseTable);

end.
