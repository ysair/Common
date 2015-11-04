unit MyString;

interface

uses Sysutils, graphics, IniFiles,classes;

function IsChineseString(Str:string; DefaultValue: Boolean):boolean;
function IsEnglishString(Str:string; DefaultValue: Boolean):boolean;

function MyStringReplace(const Str,FindStr,ReplaceStr:string;CaseSensitive:boolean):string;
function IsBig5String(const Str:string; DefaultValue: Boolean):boolean;
function DeleteAllBlank(const Str:string):string;
function DeleteTerminalBlank(const Str:string):string;
function ReplaceTab(const aStr: string;const TabNumber:integer):string;
//function HtmlToTxt(const HTMLText:string;MarkLinks,MarkTitle:boolean;bDeleteReturn:boolean):string;

function GetLastReturn(aStr:string):string;
function DeleteReturn(const aStr:string):string;

implementation
 
uses windows,ShlObj,freplace;

function MyDeleteStr(Str,DeleteStr:string):string;
var
  iPos,
  iLength :integer;
  TmpStr:String;
begin
  result := '';
  iLength := Length(DeleteStr);
  TmpStr := Str;
  iPos := AnsiPos(DeleteStr,TmpStr);
  while iPos > 0 do
  begin
    result := result + Copy(TmpStr,1,iPos - 1);
    Delete(TmpStr,1,iPos + iLength - 1);
    iPos := AnsiPos(DeleteStr,TmpStr);
  end;

  result := result + TmpStr;
end;

function MyDeleteStrNoCase(Str,DeleteStr:string):string;
var
  iPos,
  iLength :integer;
  TmpStr1,
  TmpStr2:String;
begin
  result := '';
  iLength := Length(DeleteStr);

  DeleteStr := UpperCase(DeleteStr);
  TmpStr1 := Str;
  TmpStr2 := UpperCase(Str);

  iPos := AnsiPos(DeleteStr,TmpStr2);
  while iPos > 0 do
  begin
    result := result + Copy(TmpStr1,1,iPos - 1);
    Delete(TmpStr1,1,iPos + iLength - 1);
    Delete(TmpStr2,1,iPos + iLength - 1);
    iPos := AnsiPos(DeleteStr,TmpStr2);
  end;

  result := result + TmpStr1;
end;

function MyReplaceStr(Str,FindStr,ReplaceStr:string):string;
var
  iPos,
  iLength :integer;
  TmpStr:string;
begin
  if ReplaceStr = '' then
    result := MyDeleteStr(Str,FindStr)
  else
  begin
    result := '';
    iLength := Length(FindStr);

    TmpStr := Str;
    iPos := AnsiPos(FindStr,TmpStr);
    while iPos > 0 do
    begin
      result := result + Copy(TmpStr,1,iPos - 1) + ReplaceStr;
      Delete(TmpStr,1,iPos + iLength - 1);
      iPos := AnsiPos(FindStr,TmpStr);
    end;

    result := result + TmpStr;
  end;
end;

function MyReplaceStrNoCase(Str,FindStr,ReplaceStr:string):string;
var
  iPos,
  iLength :integer;
  TmpStr1,
  TmpStr2:string;
begin
  if ReplaceStr = '' then
    result := MyDeleteStrNoCase(Str,FindStr)
  else
  begin
    result := '';
    iLength := Length(FindStr);

    TmpStr1 := Str;
    TmpStr2 := UpperCase(Str);
    FindStr := UpperCase(FindStr);

    iPos := AnsiPos(FindStr,TmpStr2);
    while iPos > 0 do
    begin
      result := result + Copy(TmpStr1,1,iPos - 1) + ReplaceStr;
      Delete(TmpStr1,1,iPos + iLength - 1);
      Delete(TmpStr2,1,iPos + iLength - 1);
      iPos := AnsiPos(FindStr,TmpStr2);
    end;

    result := result + TmpStr1;
  end;
end;

//first byte:0xA1-0XFE
//second byte:0x40-0x7E, 0xA1-0xFE
function IsBig5String(const Str:string; DefaultValue: Boolean):boolean;
var
  i:integer;
  iByte:byte;
  bFirstByte,
  bSkipThisChar:boolean;
  iBig5Count :integer;
  iTotal:integer;
begin
  result := DefaultValue;
  bFirstByte := true;
  bSkipThisChar := false;

  iBig5Count := 0;
  iTotal:=0;

  for i := 1 to Length(Str) do
  begin
    if bSkipThisChar then
    begin
      bSkipThisChar := false;
      continue;
    end;

    iByte := byte(Str[i]);

    if bFirstByte then
    begin
      //first byte:0xA1-0XFE
      //如果是中文字符，则设置bFirstByte为false
      //否则，继续循环
      //由于GB码A840-A895，A940-A996是标点符号，所以这两个页面对应的
      //字符无法跟Big5区分，直接忽略
      if (iByte = $A8) or (iByte = $A9) then
        bSkipThisChar := true
      else if (iByte > $A0) and (iByte < $FF) then
        bFirstByte := false;
    end
    else
    begin
      bFirstByte := true;
      inc(iTotal);
      //second byte:0x40-0x7E, 0xA1-0xFE
      //如果是0x40-0x7e，则为典型的big5字符
      if (iByte > $3F) and (iByte < $7F) then
        inc(iBig5Count);
    end;
  end;

  if iBig5Count * 5 > itotal then
    result := true;
end;

function DeleteAllBlank(const Str:string):string;
var
  TmpStr:	string;
	i:	integer;
  LastWordIsEnglish,
  LastCharIsBlank :boolean;
Begin
	if(Str = '') then
  begin
  	Result := '';
    exit;
  end;

  //首先将中文空格转换为英文空格
  TmpStr := Trim(MyReplaceStr(Str,'　',' '));

  i := 1;
  LastWordIsEnglish := false;
  LastCharIsBlank := false;

  result := '';
  while(i <= Length(TmpStr)) do
  Begin
    if(' ' = TmpStr[i]) or (#9 = TmpStr[i]) then
    Begin
      LastCharIsBlank := true;
      Inc(i);
    end
    else
    begin
      if(LastWordIsEnglish and LastCharIsBlank and (TmpStr[i] < #$80)) then
        result := result + ' ';

      LastCharIsBlank := false;
      LastWordIsEnglish := TmpStr[i] < #$80;
      result := result + TmpStr[i];
      Inc(i);
    end;
  end;
end;

function DeleteTerminalBlank(const Str:string):string;
var
	ResultString:string;
Begin
	if(Str = '') then
  begin
  	Result := '';
    exit;
  end;

  ResultString := Trim(Str);

  while(ResultString <> '') and (Copy(ResultString,1,2) = '　') do
  begin
    Delete(ResultString,1,2);
    ResultString := TrimLeft(ResultString);
  end;

  while(Length(ResultString) > 1) and (Copy(ResultString,Length(ResultString)-1,2) = '　') do
  begin
    Delete(ResultString,Length(ResultString)-1,2);
    ResultString := TrimRight(ResultString);
  end;

  result := ResultString;
end;

function MyStringReplace(const Str,FindStr,ReplaceStr:string;CaseSensitive:boolean):string;
var
  TmpStr:string;
  bCase:boolean;
begin
  TmpStr := Str;
  bCase := CaseSensitive or IsChineseString(TmpStr, false);
  result := FastReplace(TmpStr,FindStr,ReplaceStr,bCase);
end;


function ReplaceTab(const aStr: string;const TabNumber:integer):string;
var
  sTabStr,
  sOldStr:string;
begin
  sTabStr := StringOfChar(' ',TabNumber);

  sOldStr := aStr;
  result := FastReplace(sOldStr,char(VK_TAB),sTabStr,true);
end;

//==============================================================================

function GetLastReturn(aStr:string):string;
begin
  result := '';
  while(Length(aStr) > 1) and (aStr[Length(aStr)] = #10) do
  begin
    result := result + #13#10;
    delete(aStr,Length(aStr)-1,2);
  end;
end;

function DeleteReturn(const aStr:string):string;
begin
  result := aStr;
  while(Length(result) > 1) and (result[Length(result)] = #10) do
    result := Copy(result,1,Length(result)-2);
end;

function IsChineseString(Str:string; DefaultValue: Boolean):boolean;
var
  i,iLen:integer;
begin
  result := DefaultValue;
  i := 1;
  iLen := Length(Str) + 1;
  while (i < iLen) do
  begin
    if Str[i] >= #$80 then
    begin
      result := True;
      Exit;
    end
    else
      inc(i,2);
  end;
end;

function IsEnglishString(Str:string; DefaultValue: Boolean):boolean;
var
  i:integer;
begin
  result := DefaultValue;
  for i := 1 to Length(Str) do
  begin
    if Str[i] >= #$80 then
    begin
      result := false;
      break;
    end;
  end;
end;


end.
