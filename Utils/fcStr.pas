// 字符串相关操作相关
unit fcStr;

interface

uses
  Windows, Classes, SysUtils, FReplace, Graphics, Generics.Collections,
  Generics.Defaults, Variants, DateUtils;

type
  TStringArray= array of string;
  TIntegerArray= array of Integer;

  Str = record
    /// <summary>
    /// 获得一个字符串后面几个字符
    /// </summary>
    /// <param name="S"></param>
    /// <param name="Count"></param>
    /// <returns></returns>
    class function Right(const S: string; Count: Integer): string; static;

    //返回 ASub 在 AStr 中出现的次数
    class function NPos(const ASub, AStr: string): Integer; static;

    /// <summary>
    /// 字符串是否以给定子串结束
    /// </summary>
    /// <param name="ASubStr"></param>
    /// <param name="AStr"></param>
    /// <param name="ACaseSensitive"></param>
    /// <returns></returns>
    class function IsEndWith(const ASubStr, AStr: String; ACaseSensitive: Boolean): Boolean;  overload; static;

    /// <summary>
    /// 字符串是否以给定子串开始
    /// </summary>
    /// <param name="ASubStr"></param>
    /// <param name="AStr"></param>
    /// <param name="ACaseSensitive"></param>
    /// <returns></returns>
    class function IsStartWith(const ASubStr, AStr: String; ACaseSensitive: Boolean): Boolean; overload; static;


    /// <summary>
    /// 获取字符串中指定子串之前的字符串
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="ASubStr"></param>
    /// <param name="AResultTrim"></param>
    /// <param name="AEmptyIfNotFind"></param>
    /// <returns></returns>
    class function GetTokenBeforeChar(AStr, ASubStr: string; const AResultTrim: Boolean = True;
      const AEmptyIfNotFind: Boolean = True): string; static;

    /// <summary>
    /// 获取字符串中指定子串之后的字符串
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="ASubStr"></param>
    /// <param name="AResultTrim"></param>
    /// <param name="AEmptyIfNotFind"></param>
    /// <returns></returns>
    class function GetTokenAfterChar(AStr, ASubStr: string; const AResultTrim: Boolean = True;
      const AEmptyIfNotFind: Boolean = True): string; static;

    /// <summary>
    /// 从列表中根据给定名字和分隔符，找到第一个符合条件的值。
    /// 1）列表中名字为大写字符。
    /// 2）列表中的值，可用#9进行多行的分隔。
    /// </summary>
    /// <param name="AFromList"></param>
    /// <param name="AFieldName"></param>
    /// <param name="AValue"></param>
    /// <param name="ASplitChar"></param>
    /// <returns></returns>
    class function GetFieldValueFromList(const AFromList: TStringList; const AFieldName: String;
      var AValue: String; const ASplitChar: String = '='): Boolean; static;

    /// <summary>
    /// 根据分隔符将字符串分解为字符列表
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="ASplitStr"></param>
    /// <param name="AToList"></param>
    /// <param name="ADefItemCount"></param>
    /// <param name="ATrim"></param>
    class procedure GetTokenToList(const AStr, ASplitStr: String; AToList: TStrings;
      const ADefItemCount: Integer = -1; const ATrim: Boolean = True); static;

    /// <summary>
    /// 把一个字符串分成多个子串并放在字符串列表中。结果的子串不为空，并且经过Trim。
    /// 同时，被包含在成对的双引号之间的分隔符将被忽略。
    /// </summary>
    /// <param name="AStr"></param>
    /// <param name="AToList"></param>
    /// <param name="ASplitChars">作为分隔依据的字符</param>
    class procedure StrToList(const AStr: string; const AToList: TStrings; const ASplitChars: string); static;

    /// <summary>
    /// 根据给定Delimiter将字符串列表整合为一个字符串
    /// </summary>
    /// <param name="AList"></param>
    /// <param name="ADelimiter"></param>
    /// <returns></returns>
    class function ListToStr(const AList:TStrings; const ADelimiter:string):string; overload; static;
    class function ListToStr(const AList:TList<Integer>; const ADelimiter:string):string; overload; static;

    class procedure IntListToStrList(AFromList: TList<Integer>; AToList: TStringList); static;
    class procedure StrListToIntList(AFromList: TStringList; AToList: TList<Integer>); static;

    {找出List1 List2 中共同的行，并且放入AToList中。}
    class procedure GetSameLineFrom2List(AList1, AList2: TStringList; AToList: TStringList); static;


    /// <summary>
    /// 安全格式化一个字符串，如果出错，直接返回指定format
    /// </summary>
    /// <param name="Format"></param>
    /// <param name="Args"></param>
    /// <returns></returns>
    class function SafeFormat(const Format: string; const Args: array of const ): string; static;




    /// <summary>
    /// 计算一个stringlist.text的长度，等于Length(AList.Text)但是速度快
    /// </summary>
    /// <param name="AList"></param>
    /// <param name="ACrlfLength">换行符的字符数，默认为2，表示#13#10</param>
    /// <returns></returns>
    class function StringListTextLength(const AList: TStringList; const ACrlfLength: Integer = 2): Integer; static;

    //
    /// <summary>
    /// 取得一个汉字串的拼音，如： “中国共产党”-> ZGGCD。如果字符串中有英文字母，
    /// 保留这些英文字母。
    /// </summary>
    /// <param name="AInStr"></param>
    /// <returns></returns>
    class function HzPy(AInStr: String): String; static;

    /// <summary>
    /// 判读是否中文字符串
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function IsChineseString(AStr: String): Boolean; static;

    /// <summary>
    /// 判断是否英文字符串
    /// </summary>
    /// <param name="AStr"></param>
    /// <returns></returns>
    class function IsEnglishString(AStr: string): Boolean; static;

    class function IsBig5String(const AInStr: string; DefaultValue: Boolean): Boolean; static;

    /// <summary>
    /// 产生一个长度为Len的随机字符串
    /// </summary>
    /// <param name="Len"></param>
    /// <returns></returns>
    class function RandomStr(Len: Integer): string; static;

    /// <summary>
    /// 删除列表中首尾的空行。
    /// </summary>
    /// <param name="AInputList"></param>
    class procedure TrimEmptyLine(const AInputList: TStringList); static;

    /// <summary>
    /// 读取文件到字符串列表
    /// </summary>
    /// <param name="AFileName"></param>
    /// <param name="AToList"></param>
    /// <returns></returns>
    class function StringListLoadFromFile(const AFileName: string; const AToList: TStrings): Boolean; static;

    /// <summary>
    /// 保存字符串列表到文件
    /// </summary>
    /// <param name="AFileName"></param>
    /// <param name="AFromList"></param>
    /// <returns></returns>
    class function StringListSaveToFile(const AFileName: string; const AFromList: TStrings): Boolean; static;

    // 判断一个字符串是否是IP地址
    class function IsIPAddress(const AStr: string): Boolean; static;

    /// <summary>
    /// 快速排序一个TStringList中的字符串
    /// </summary>
    /// <param name="AList"></param>
    class procedure QuickSortList(const AList: TStringList); static;

    // 追加字符串， 相当于代码：
    // if ASourceStr='' then Result := ASubAdded
    // else Reuslt := ASourceStr + ', ' + ASubAdded
    class function  AppendStr(ASourceStr, ASubAdded: string; const ASplit: string = ','): string; static;


    class function StrToCharset(const AStr: string): TSysCharSet; static;

    class function VarToInt(AVar: Variant; const ADef: Integer=0): Int64; static;
    class function VarToBool(AVar: Variant; const ADef: Boolean=false): Boolean; static;
    class function VarToDateTime(AVar: Variant; const ADef: TDateTime=0.0): TDateTime; static;
    class function IntToStrEx(AIn: Int64; const AOutLen: Integer = -1): string;  static;

    class function FileExt2FileFilter(AFileExt: string): string; static;

    class function HexToIntDef(Hex: string; const ADef: Integer=-1): Integer; static;

    class function FastPos(const aSourceString, aFindString: String; const aSourceLen, aFindLen, StartPos: Integer): Integer; overload; static;
    class function FastPosNoCase(const aSourceString, aFindString: String; const aSourceLen, aFindLen, StartPos: Integer): Integer; overload; static;
    class function FastPos(const aSourceString, aFindString: String; const StartPos: Integer = 1): Integer; overload; static;
    class function FastPosNoCase(const aSourceString, aFindString: String; const StartPos: Integer = 1): Integer; overload; static;
    class function FastReplace(const aSourceString: String; const aFindString, aReplaceString: String; CaseSensitive: Boolean = False): String;  static;
    class function FindTagContent(const AText: string; const APos: Integer; const ABeginTag, AEndTag: string; const ADeleteTag: Boolean = False) : string; static;


    {系统的StrToDateTime识别率太低}
    class function  StrToDateTime(AInStr: string; const ADef: TDateTime =0): TDateTime; static;

    {}
    class procedure CopyIdList(AFromList, AToList: TList<Integer>); static;

    class function StrSaveToFile(const AStr : string; const AFileName : string):Boolean; static;
    class function StrLoadFromFile(const AFileName : string):string; static;

    class function GetPart(var AInList: TStringList; out AOutList: TStringList; const AMaxCount: Integer=50): Boolean; overload; static;
    class function GetPart(var AInList: TList<Integer>; out AOutList: TList<Integer>; const AMaxCount: Integer=50): Boolean; overload; static;
    class function GetPart(var AInList: TList<Integer>; var AOutStr: string; const ASplit: string=','; const AMaxCount: Integer=50): Boolean; overload; static;

  end;


{
  Author: Kevin Boylan
  Ported By: Sergey Seroukhov

  This code is meant to allow wildcard pattern matches.
  It is VERY useful for matching filename wildcard patterns.
  It allows unix grep-like pattern comparisons, for instance:

	?	   	Matches any single characer
	*	   	Matches any contiguous characters
	[abc]  	Matches a or b or c at that position
	[^abc]	Matches anything but a or b or c at that position
	[!abc]	Ditto
	[a-e]  	Matches a through e at that position

	'ma?ch.*'	-Would match match.exe, mavch.dat, march.on, etc
	'this [e-n]s a [!zy]est' - Would match 'this is a test',
                               but would not match 'this as a yest'

  This is a Delphi VCL translation from C code that was downloaded from CIS.
  C code was written by J. Kerceval and released to public domain 02/20/1991.
  This code is ofcourse also public domain. I would appreciate it if you would
  let me know if you find any bugs.  I would also appreciate any notes sent my
  way letting me know if you find it useful.

  ?       任何一个字符或者汉字
  *       任何字符
  [^abc]  除了 a b c 意外的任何字符
  [!abc]  同[^abc]
  [a-e]   包含 a 到 e 的字符
}
  Match = class
  private
//    class function Matche(Pattern, Text: string): Integer; static;
    class function MatchAfterStar(Pattern, Text: string): Integer; static;
  public
    class function Matche(Pattern, Text: string): Integer; static;
    class function IsMatch(const Pattern, Text: string): Boolean; static;
  end;


  Arr = record
    class function PosInSmallIntArray(const ASearchInt: SmallInt; const AArray: array of SmallInt): Integer; static;
    class function PosInIntArray(const ASearchInt: Integer; const AArray: array of Integer): Integer; static;
    class function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer; static;

    class function StrListToArray(AList: TStringList): TStringArray; overload; static;
    class function StrListToArray(AList: TStringList; var AOutArr: TStringArray): Boolean; overload; static;
    class function IntListToArray(AList: TList<Integer>): TIntegerArray; static;
    class procedure ArrayToStrList(AArr:  array of string; AList: TStrings; const AClearStrList: Boolean=True); static;
    class function MergeStrArray(AArr1, AArr2: array of string): TStringArray; static;
  end;

  HTML = class
  private
    class var FHtmlCodeDict: TDictionary<string, String>;
    class procedure CheckHtmlCodeDictCreated(const ADefHtmlCodeFile:string);
  public
    class destructor Destroy;
  public
    /// <summary>
    /// 将Windows颜色转换为Html颜色
    /// </summary>
    /// <param name="Color">Windows颜色</param>
    /// <returns>Html格式颜色</returns>
    class function Color2HTML(Color: TColor): string; static;
    class function ColorHTMLToNormal(AHtmlColor: string): TColor; static;

    class function ReplaceHtmlSyntax(const AInputText: String): String; static;

    class function TextToHtml(const sText: string; const AReplaceHtmlFlag: Boolean): string; static;

    // 将Html转为Txt
    // 参数：
    //    AMarkLinks: 是否要把<a>http://xx.xx</a>中的内容转换为 [http://xx.xx],并且显示在文本中
    //    AReturnLen: 要返回的字符长度。  如果仅要使用到前100个字，则设置这个值为100，提高效率。
    //    ADefHtmlCodeFile: 转义字符存在的文件。里面每行保存着类似“&lt;=<”。 可以是相对路径，也可以使绝对路径
    class function HtmlToText(const AHtml: string; AMarkLinks: Boolean=false; const AReturnLen: Integer=-1; const ADefHtmlCodeFile:string='Language\HtmlCode.txt'): string; static;

    class function UpdateHtmlCharset(const AHtmlString: string; Charset: string): String; static;

    class function ToTextBolckQuote(sSource: string): string; static;

    class function ToHtmlBolckQuote(sSource: string; isSourceHtml: Boolean): string; static;

    class function GetHtmlBody(sHtml: string): string; static;

    class function ChangeHtmlCharset(const AInHtml, ACharset: String): String; static;

    class function GetCharset(const sCharset: String): string; static;


    /// <summary>
    /// 将'US-ASCII'或者空的Charset转换为默认的charset
    /// </summary>
    /// <param name="sCharset"></param>
    /// <param name="defaultCharset"></param>
    /// <returns></returns>
    class function GetFilteredCharset(const sCharset, defaultCharset: String): string; static;

    class function RemoveScript(const AHtml : string):string; static;
    class function RemoveBodyOverFlow(const AHtml : string):string; static;
    class function RemoveContentEditable(const AHtml : string):string; static;
  end;

  Binary = record   //二进制

    //找二进制某位的值（从低到高位），返回True表示这位为1，false表示为0
    //如：GetBitValue(23123,2)=true, GetBitValue(23123,3)=false,   23123->>101101001010011
    class function GetBitValue(AIn: Int64; ABitPosition: Integer): Boolean; static;

    //设置某位的值（从低到高位），返回True表示设置成功，
    //如：SetBitValue(23123, 1, True), 表示把最后一位设置为1,
    //    SetBitValue(23123, 1, false), 表示把最后一位设置为0,
    class function SetBitValue(AIn: Int64; ABitPosition: Integer; AValue: Boolean): Int64; static;
  end;

implementation

uses
  fcWindows, fcNetwork, StrUtils;

{ Str }

class function Str.AppendStr(ASourceStr, ASubAdded: string; const ASplit: string): string;
begin
  if ASubAdded='' then
    Result := ASourceStr
  else
  if ASourceStr='' then
    Result := ASubAdded
  else
    Result := ASourceStr + ASplit + ASubAdded
end;

class procedure Str.CopyIdList(AFromList, AToList: TList<Integer>);
var
  I: Integer;
begin
  AToList.Clear;
  for I := 0 to AFromList.Count -1 do
    AToList.Add(AFromList[I]);
end;

class function Str.FastPos(const aSourceString, aFindString: String;
  const StartPos: Integer): Integer;
begin
  Result := FReplace.FastPos(aSourceString, aFindString, StartPos);
end;

class function Str.FastPos(const aSourceString, aFindString: String;
  const aSourceLen, aFindLen, StartPos: Integer): Integer;
begin
  Result := FReplace.FastPos(aSourceString, aFindString, aSourceLen, aFindLen, StartPos);
end;

class function Str.FastPosNoCase(const aSourceString, aFindString: String;
  const aSourceLen, aFindLen, StartPos: Integer): Integer;
begin
  Result := FReplace.FastPosNoCase(aSourceString, aFindString, aSourceLen, aFindLen, StartPos);
end;

class function Str.FastPosNoCase(const aSourceString, aFindString: String;
  const StartPos: Integer): Integer;
begin
  Result := FReplace.FastPosNoCase(aSourceString, aFindString, StartPos);
end;

class function Str.FastReplace(const aSourceString, aFindString,
  aReplaceString: String; CaseSensitive: Boolean): String;
begin
  Result := FReplace.FastReplace(aSourceString, aFindString, aReplaceString, CaseSensitive);
end;

class function Str.FileExt2FileFilter(AFileExt: string): string;
begin
  if AFileExt='' then
    Result := '*.*|*.*'
  else
    Result := Format('*%s|*%s|*.*|*.*', [AFileExt, AFileExt]);
end;

class function Str.FindTagContent(const AText: string; const APos: Integer;
  const ABeginTag, AEndTag: string; const ADeleteTag: Boolean): string;
// 查找标签之间的字符串
// AText 源字符串
// APos 起始位置,下标从1起
// ABeginTag, AEndTag 起止标签
// ADeleteTag 是否删除标签
// FindTagContext('<a>123</a><a>456</a>', 1, '<a>', '</a>', false) = '<a>123</a>'
// FindTagContext('<a>123</a><a>456</a>',10, '<a>', '</a>', true) = '456'
var
  p1, p2: Integer;
begin
  Result := '';
  p1 := PosEx(ABeginTag, AText, APos);
  p2 := PosEx(AEndTag, AText, p1 + Length(ABeginTag));
  if (p1 >= APos) and (p2 > p1) then
  begin
    Result := MidStr(AText, p1, p2 - p1 + Length(AEndTag));
    if ADeleteTag then
    begin
      Delete(Result, 1, Length(ABeginTag));
      Delete(Result, Length(Result) - Length(AEndTag) + 1, Length(AEndTag));
    end;
  end;
end;

class function Str.GetFieldValueFromList(const AFromList: TStringList; const AFieldName: String;
  var AValue: String; const ASplitChar: String): Boolean;
var
  name: String;
  j: Integer;
  tempList: TStringList;
begin
  name := uppercase(AFieldName) + ASplitChar;
  tempList := TStringList.Create;

  try
    Result := false;

    for j := 0 to AFromList.Count - 1 do
    begin
      if Result then
      begin
        if IsStartWith(#9, AFromList[j], True) then
          tempList.Add(Trim(Copy(AFromList[j], 2, Length(AFromList[j]))))
        else
          break;
      end
      else if IsStartWith(name, uppercase(AFromList[j]), True) then
      begin
        Result := True;
        tempList.Add(Trim(Copy(AFromList[j], Length(name) + 1, Length(AFromList[j]))));
      end;
    end;

    if Result then
      AValue := Trim(tempList.Text);
  finally
    tempList.Free;
  end;
end;

class function Str.GetPart(var AInList: TStringList; out AOutList: TStringList;
  const AMaxCount: Integer): Boolean;
var
  l_Count: Integer;
begin
  l_Count := 0;
  AOutList := TStringList.Create;
  try
    while (AInList.Count>0) and (l_Count<AMaxCount) do
    begin
      AOutList.Add(AInList[0]);
      AInList.Delete(0);
      Inc(l_Count);
    end;
  finally
    Result := AOutList.Count > 0;
    if not Result then
      AOutList.Free;
  end;
end;

class function Str.GetPart(var AInList: TList<Integer>;
  out AOutList: TList<Integer>; const AMaxCount: Integer): Boolean;
var
  l_Count: Integer;
begin
  l_Count := 0;
  AOutList := TList<Integer>.Create;
  try
    while (AInList.Count>0) and (l_Count<AMaxCount) do
    begin
      AOutList.Add(AInList[0]);
      AInList.Delete(0);
      Inc(l_Count);
    end;
  finally
    Result := AOutList.Count > 0;
    if not Result then
      AOutList.Free;
  end;
end;

class function Str.GetPart(var AInList: TList<Integer>; var AOutStr: string;
  const ASplit: string; const AMaxCount: Integer): Boolean;
var
  l_OutList: TList<Integer>;
begin
  Result := GetPart(AInList, l_OutList, AMaxCount);
  if Result then
  try
    AOutStr := Str.ListToStr(l_OutList, ASplit);
  finally
    l_OutList.Free;
  end;
end;

class procedure Str.GetSameLineFrom2List(AList1, AList2, AToList: TStringList);
var
  l_Dict: TDictionary<string, Integer>;
  I, l_v: Integer;
begin
  AToList.Clear;
  if (AList1.Count=0) or (AList2.Count=0) then
    Exit;

  l_Dict := TDictionary<string, Integer>.Create;
  try
    for I := 0 to AList1.Count - 1 do
      if not l_Dict.TryGetValue(AList1[I], l_v) then
        l_Dict.AddOrSetValue(AList1[I], 1);

    for I := 0 to AList2.Count - 1 do
      if l_Dict.TryGetValue(AList2[I], l_v) then
        AToList.AddObject(AList2[I], AList2.Objects[I]);
  finally
    l_Dict.Free;
  end;
end;

class function Str.GetTokenBeforeChar(AStr, ASubStr: String;
  const AResultTrim, AEmptyIfNotFind: Boolean): string;
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

class function Str.GetTokenAfterChar(AStr, ASubStr: String; const AResultTrim,
  AEmptyIfNotFind: Boolean): string;
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

class procedure Str.GetTokenToList(const AStr, ASplitStr: String; AToList: TStrings;
  const ADefItemCount: Integer; const ATrim: Boolean);
var
  l_Pos: Integer;
  l_NewStr, l_Str: String;
begin
  AToList.Clear;

  l_NewStr := AStr;
  repeat
    l_Pos := Pos(ASplitStr, l_NewStr);
    if l_Pos > 0 then
    begin
      l_Str := Copy(l_NewStr, 1, l_Pos - 1);
      if ATrim then
        l_Str := Trim(l_Str);
      AToList.Add(l_Str);

      l_NewStr := Copy(l_NewStr, l_Pos + Length(ASplitStr), Length(l_NewStr) - l_Pos);
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
  until l_NewStr = '';

  if ADefItemCount > 0 then
    while AToList.Count < ADefItemCount do
      AToList.Add('');
end;

class function Str.HexToIntDef(Hex: string; const ADef: Integer): Integer;
begin
  Result := StrToIntDef('$'+Hex, ADef);
end;

class function Str.HzPy(AInStr: String): String;
const
  ChinaCode: array [0 .. 25, 0 .. 1] of Integer = ((1601, 1636), (1637, 1832), (1833, 2077),
    (2078, 2273), (2274, 2301), (2302, 2432), (2433, 2593), (2594, 2786), (9999, 0000),
    (2787, 3105), (3106, 3211), (3212, 3471), (3472, 3634), (3635, 3721), (3722, 3729),
    (3730, 3857), (3858, 4026), (4027, 4085), (4086, 4389), (4390, 4557), (9999, 0000),
    (9999, 0000), (4558, 4683), (4684, 4924), (4925, 5248), (5249, 5589));
var
  C1, Len1, C2: Integer;
  ir: Word;
  sr: AnsiString;
begin
  Result := '';
  if AInStr = '' then
    Exit('');

  sr := AnsiString(AInStr);
  C1 := 1;
  Len1 := Length(sr);
  while (C1 <= Len1) do
  begin
    if (ord(sr[C1]) >= 160) then
    begin
      if (ord(sr[C1 + 1]) >= 160) then
        ir := (ord(sr[C1]) - 160) * 100 + ord(sr[C1 + 1]) - 160
      else // 这里可能会出错，但目前只能先这样，因为这里只覆盖了中文简体的4000多字。
        ir := (ord(sr[C1]) - 160) * 100 + ord(sr[C1 + 1]);

      for C2 := 0 to 25 do
      begin
        if (ir >= ChinaCode[C2, 0]) and (ir <= ChinaCode[C2, 1]) then
        begin
          Result := Result + chr(C2 + ord('a'));
          break;
        end;
      end;
      C1 := C1 + 2;
    end
    else
    begin
      Result := Result + string(sr[C1]);
      C1 := C1 + 1;
    end;
  end;
  Result := uppercase(Result);
end;

class procedure Str.IntListToStrList(AFromList: TList<Integer>; AToList: TStringList);
var
  I: Integer;
begin
  AToList.Clear;
  for I := 0 to AFromList.Count - 1 do
    AToList.Add(IntToStr(AFromList[I]));
end;

class function Str.IntToStrEx(AIn: Int64; const AOutLen: Integer): string;
begin
  Result := SysUtils.IntToStr(AIn);
  if AOutLen>0 then
    while Length(Result) < AOutLen do
      Result := '0'+ Result;
end;

class function Str.IsBig5String(const AInStr: string; DefaultValue: Boolean): Boolean;
// GB码A840-A895，A940-A996是标点符号
  function IsChineseInterpunction(iByte: byte): Boolean;
  begin
    Result := (iByte = $A8) or (iByte = $A9);
  end;
  function IsChineseFirstChar(iByte: byte): Boolean;
  begin
    Result := (iByte > $A0) and (iByte < $FF);
  end;
  function IsTypicalBig5SecondChar(iByte: byte): Boolean;
  begin
    Result := (iByte > $3F) and (iByte < $7F);
  end;

var
  I: Integer;
  iByte: byte;
  bFirstByte, bSkipThisChar: Boolean;
  iBig5Count, isGBCount: Integer;
  iTotal: Integer;
  Str: AnsiString;
begin
  /// 这个函数不准！！ 因为现在时UNICODE， 这个函数仅能对 delphi7 以下生效。
  Result := DefaultValue;
  Str := AnsiString(AInStr);

  bFirstByte := True;
  bSkipThisChar := false;

  iBig5Count := 0;
  isGBCount := 0;
  iTotal := 0;

  for I := 1 to Length(Str) do
  begin
    if bSkipThisChar then
    begin
      bSkipThisChar := false;
      Continue;
    end;

    iByte := byte(Str[I]);

    if bFirstByte then
    begin
      if IsChineseInterpunction(iByte) then
        bSkipThisChar := True
      else if IsChineseFirstChar(iByte) then
        bFirstByte := false;
    end
    else
    begin
      bFirstByte := True;
      Inc(iTotal);
      if IsTypicalBig5SecondChar(iByte) then
        Inc(iBig5Count)
      else
        Inc(isGBCount);
    end;
  end;

  if iBig5Count * 5 > iTotal then
    Result := True
  else if isGBCount > 0 then
    Result := false;
end;

class function Str.IsChineseString(AStr: String): Boolean;
var
  I: Integer;
  ansistr: AnsiString;
begin
  Result := false;
  if AStr = '' then
    Exit;

  ansistr := AnsiString(AStr);
  I := 1;
  while I < Length(ansistr) do
  begin
    if (ansistr[I] >= #$81) then
    begin
      if (ansistr[I + 1] >= #$40) then
      begin
        Result := True;
        Exit;
      end;
      Inc(I, 2);
    end
    else
    begin
      Inc(I);
    end;
  end;
end;

class function Str.IsEndWith(const ASubStr, AStr: String; ACaseSensitive: Boolean): Boolean;
var
  l_RightStr: String;
begin
  if ('' = ASubStr) or ('' = AStr) then
    Exit(false);

  l_RightStr := Right(AStr, Length(ASubStr));
  if ACaseSensitive then
    Result := ASubStr = l_RightStr
  else
    Result := SameText(ASubStr, l_RightStr);
end;

class function Str.IsEnglishString(AStr: string): Boolean;
begin
  Result := not Str.IsChineseString(AStr);
end;

class function Str.IsIPAddress(const AStr: string): Boolean;
begin
  Result := Network.IsIPAddressV4(AStr);
end;

class function Str.IsStartWith(const ASubStr, AStr: String; ACaseSensitive: Boolean): Boolean;
begin
  if ('' = ASubStr) or ('' = AStr) then
    Exit(false);

  if ACaseSensitive then
    Result := Pos(ASubStr, AStr) = 1
  else
    Result := Pos(uppercase(ASubStr), uppercase(AStr)) = 1
end;

class function Str.ListToStr(const AList: TList<Integer>; const ADelimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AList.Count - 1 do
    if '' = Result then
      Result := IntToStr(AList[I])
    else
      Result := Result + ADelimiter + IntToStr(AList[I]);
end;

class function Str.ListToStr(const AList: TStrings;
  const ADelimiter: string): string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to AList.Count - 1 do
    if '' = Result then
      Result := AList[I]
    else
      Result := Result + ADelimiter + AList[I];
end;

class function Str.NPos(const ASub, AStr: string): Integer;
var
  l_Pos: Integer;
begin
  Result := 0;
  l_Pos := PosEx(ASub, AStr, 1);
  while l_Pos>0 do
  begin
    Inc(Result);
    l_Pos := PosEx(ASub, AStr, l_Pos+Length(ASub));
  end;
end;

type
  TKeyAndValueClass = class
  public
    Key: string;
    Value: TObject;
  end;

function KeyValueComparisonClass(const Left, Right: TKeyAndValueClass): Integer;
begin
  Result := AnsiCompareStr(Left.Key, Right.Key);
  {if Left.Key > Right.Key then
    Result := 1
  else if Left.Key < Right.Key then
    Result := -1
  else
    Result := 0;
    }
end;

class procedure Str.QuickSortList(const AList: TStringList);
var
  I: Integer;
  tempArray: array of TKeyAndValueClass;
  Comparer: IComparer<TKeyAndValueClass>;
begin
  if not Assigned(AList) then
    Exit;

  AList.Sorted := false;

  SetLength(tempArray, AList.Count);
  try
    for I := 0 to AList.Count - 1 do
    begin
      tempArray[I] := TKeyAndValueClass.Create;
      tempArray[I].Key := AList[I];
      tempArray[I].Value := AList.Objects[I];
    end;

    try
      Comparer := TComparer<TKeyAndValueClass>.Construct(KeyValueComparisonClass);

      TArray.Sort<TKeyAndValueClass>(tempArray, Comparer);
      Comparer := nil;

      AList.BeginUpdate;
      try
        for I := 0 to AList.Count - 1 do
        begin
          AList[I] := tempArray[I].Key;
          AList.Objects[I] := tempArray[I].Value;
        end;
      finally
        AList.EndUpdate;
      end;
    finally
      for I := 0 to AList.Count - 1 do
        tempArray[I].Free;
    end;
  finally
    SetLength(tempArray, 0);
  end;
end;

class function Str.RandomStr(Len: Integer): string;
var
  l_Idx: Integer;
const
  C_Str = 'abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ';
begin
  Result := '';
  Randomize;
  while Length(Result)<Len do
  begin
    l_Idx := Random(10000) mod Length(C_Str) +1;
    Result := Result + C_Str[l_Idx];
  end;
end;

class function Str.Right(const S: string; Count: Integer): string;
begin
  Result := Copy(S, Length(S) - Count + 1, Count);
end;

class function Str.SafeFormat(const Format: string; const Args: array of const ): string;
begin
  try
    Result := SysUtils.Format(Format, Args);
  except
    Result := Format;
  end;
end;


class function Str.StringListLoadFromFile(const AFileName: string; const AToList: TStrings): Boolean;
begin
  Result := false;
  if FileExists(AFileName) then
  begin
    try
      AToList.LoadFromFile(AFileName);
      Result := True;
    except
    end;
  end;
end;

class function Str.StringListSaveToFile(const AFileName: string; const AFromList: TStrings): Boolean;
begin
  ForceDirectories(ExtractFilePath(AFileName));
  try
    AFromList.SaveToFile(AFileName);
    Result := True;
  except
    Result := false;
  end;
end;

class function Str.StringListTextLength(const AList: TStringList;
  const ACrlfLength: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AList.Count - 1 do
    Result := Result + Length(AList[I]) + ACrlfLength;
end;

class procedure Str.StrListToIntList(AFromList: TStringList; AToList: TList<Integer>);
var
  I: Integer;
begin
  AToList.Clear;
  for I := 0 to AFromList.Count - 1 do
    AToList.Add(StrToIntDef(AFromList[I], 0));
end;

class function Str.StrLoadFromFile(const AFileName: string): string;
begin
  Result  :=  '';
  if FileExists(AFileName) then
    with TStringList.Create do
    try
      try
        LoadFromFile(AFileName);
        Result  :=  Text;
      except
      end;
    finally
      Free;
    end;
end;

class function Str.StrSaveToFile(const AStr, AFileName: string): Boolean;
begin
  ForceDirectories(ExtractFilePath(AFileName));
  with TStringList.Create do
  try
    try
      Text  :=  AStr;
      SaveToFile(AFileName);
      Result  :=  True;
    except
      Result  :=  False;
    end;
  finally
    Free;
  end;
end;

class function Str.StrToCharset(const AStr: string): TSysCharSet;
var
  l_Char: AnsiChar;
begin
  Result := [];
  for l_Char in AnsiString(AStr) do
    Result := Result + [l_Char];
end;


class function Str.StrToDateTime(AInStr: string; const ADef: TDateTime): TDateTime;
const
  c_Split : TSysCharSet = ['/','-'];
var
  l_yyyy, l_mm, l_dd, l_hh, l_nn, l_ss: Integer;
begin
  {TryStrToDateTime 有个BUG，当传入是 yyyy/mm/dd hh:nn:ss 格式，并且在系统的日期格式中有 dddd字样时，会导致解析的时候时间丢失}

  if (AInStr='') or SameText(AInStr, 'null') then
    Exit(ADef);

  //C_DB_DateTime_StrFormat = 'yyyy/mm/dd hh:nn:ss';
  if (Length(AInStr) = 19)
    and (CharInSet(AInStr[5], c_Split))
    and (CharInSet(AInStr[8], c_Split))
    and (CharInSet(AInStr[11], [' ']))
  then
  begin
    l_yyyy  := StrToIntDef(Copy(AInStr, 1, 4), -1);
    l_mm    := StrToIntDef(Copy(AInStr, 6, 2), -1);
    l_dd    := StrToIntDef(Copy(AInStr, 9, 2), -1);
    l_hh    := StrToIntDef(Copy(AInStr, 12, 2), -1);
    l_nn    := StrToIntDef(Copy(AInStr, 15, 2), -1);
    l_ss    := StrToIntDef(Copy(AInStr, 18, 2), -1);

    if (l_yyyy>1980) and (l_yyyy<2999)
      and (l_mm in [1..12])
      and (l_dd in [1..31])
      and (l_hh in [0..23])
      and (l_nn in [0..59])
      and (l_ss in [0..59])
    then
      Exit(EncodeDateTime(l_yyyy, l_mm, l_dd, l_hh, l_nn, l_ss, 0));
  end;

  if not TryStrToDateTime(AInStr, Result) then
  begin
    try
      Result := Variants.VarToDateTime(AInStr);
    except
      Result := ADef;
    end;
  end;
end;

class procedure Str.StrToList(const AStr: string; const AToList: TStrings;
  const ASplitChars: string);
var
  curStr, l_Char: string;
  I, l_Pos: Integer;
Label 111;
begin
  AToList.Clear;

  curStr := '';
  I := 1;
  while I <= Length(AStr) do
  begin
    l_Char := AStr[I];
    if (l_Char = '"') and (Trim(curStr) = '') then // 以引号开头的，比如 "woodstock,zeng""" <263.net>, xqzeng@263.net，这样是要处理成2个而不是3个
    begin
    111 :
      l_Pos := FastPosNoCase(AStr, '"', Length(AStr), 1, I + 1);
      if l_Pos > I then
      begin
        // 这里分几种情况，连续几个引号的....
        if (l_Pos < Length(AStr)) and (AStr[l_Pos + 1] = '"') then
        begin
          curStr := curStr + Copy(AStr, I, l_Pos - I + 1);
          I := l_Pos + 1;
          goto 111;
        end
        else
        begin
          curStr := curStr + Copy(AStr, I, l_Pos - I + 1);
          I := l_Pos + 1;
          Continue;
        end;
      end;
    end;
    if FastPosNoCase(ASplitChars, l_Char, Length(ASplitChars), 1, 1) > 0 then
    begin
      AToList.Add(Trim(curStr));
      curStr := '';
    end
    else
    begin
      curStr := curStr + l_Char;
    end;
    Inc(I);
  end;
  AToList.Add(Trim(curStr));

  for I := AToList.Count - 1 downto 0 do
  begin
    if AToList[I] = '' then
      AToList.Delete(I);
  end;
end;

class procedure Str.TrimEmptyLine(const AInputList: TStringList);
begin
  while (AInputList.Count > 0) and (AInputList[0] = '') do
    AInputList.Delete(0);

  while (AInputList.Count > 0) and (AInputList[AInputList.Count - 1] = '') do
    AInputList.Delete(AInputList.Count - 1);
end;

class function Str.VarToBool(AVar: Variant; const ADef: Boolean): Boolean;
begin
  if VarIsNull(AVar) then
    Exit(ADef)
  else begin
    try
      Result := SameText(VarToStrDef(AVar,''), '1')
          or SameText(VarToStrDef(AVar,''), 'true')
          or (VarIsNumeric(AVar) and Boolean(AVar));
    except
      Result := ADef;
    end;
  end;
end;

class function Str.VarToDateTime(AVar: Variant; const ADef: TDateTime): TDateTime;
var
  l_Str: string;
  l_Float: Double;
begin
  try
    l_Str := VarToStrDef(AVar, ''); {这个还是会弹出异常..}
  except
    l_Str := '';
  end;
  Result := Str.StrToDateTime(l_Str, ADef);
  if Result=ADef then
  begin
    l_Float := StrToFloatDef(l_Str, -1);
    if l_Float<>-1 then
      Result := TDateTime(l_Float);
  end;
end;

class function Str.VarToInt(AVar: Variant; const ADef: Integer): Int64;
begin
  Result := StrToInt64Def(VarToStrDef(AVar, ''), ADef);
end;

{ Arr }

class procedure Arr.ArrayToStrList(AArr: array of string; AList: TStrings;
  const AClearStrList: Boolean);
var
  I: Integer;
begin
  if AClearStrList then
    AList.Clear;
  for I := Low(AArr) to High(AArr) do
    if AList.IndexOf(AArr[I])= -1 then
      AList.Add(AArr[I]);
end;

class function Arr.IntListToArray(AList: TList<Integer>): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, AList.Count);
  for I := 0 to AList.Count - 1 do
    Result[I] := AList[I];
end;

class function Arr.MergeStrArray(AArr1, AArr2: array of string): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, High(AArr1)+ High(AArr2)+2);
  for I := Low(AArr1) to High(AArr1) do
    Result[I] := AArr1[I];

  for I := Low(AArr2) to High(AArr2) do
    Result[High(AArr1)+ 1 +I] := AArr2[I];
end;

class function Arr.PosInIntArray(const ASearchInt: Integer;
  const AArray: array of Integer): Integer;
begin
  for Result := Low(AArray) to High(AArray) do
    if ASearchInt = AArray[Result] then
      Exit;
  Result := -1;
end;

class function Arr.PosInSmallIntArray(const ASearchInt: SmallInt;
  const AArray: array of SmallInt): Integer;
begin
  for Result := Low(AArray) to High(AArray) do
    if ASearchInt = AArray[Result] then
      Exit;
  Result := -1;
end;

class function Arr.PosInStrArray(const SearchStr: string; const Contents: array of string;
  const CaseSensitive: Boolean): Integer;
begin
  for Result := Low(Contents) to High(Contents) do
  begin
    if CaseSensitive then
    begin
      if SearchStr = Contents[Result] then
        Exit;
    end
    else
    begin
      if AnsiCompareText(SearchStr, Contents[Result]) = 0 then
        Exit;
    end;
  end;
  Result := -1;
end;

class function Arr.StrListToArray(AList: TStringList;
  var AOutArr: TStringArray): Boolean;
begin
  AOutArr := StrListToArray(AList);
  Result := AList.Count > 0;
end;

class function Arr.StrListToArray(AList: TStringList): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, AList.Count);
  for I := 0 to AList.Count - 1 do
    Result[I] := AList[I];
end;

{ Color }

class procedure HTML.CheckHtmlCodeDictCreated(const ADefHtmlCodeFile:string);
var
  l_FileName, l_Key, l_Value: string;
  l_List: TStringList;
  I: Integer;
begin
  if not Assigned(FHtmlCodeDict) then
  begin
    FHtmlCodeDict :=TDictionary<string, String>.Create;
    l_FileName := ADefHtmlCodeFile;
    if not FileExists(l_FileName) then l_FileName := Win.GetInstancePath + ADefHtmlCodeFile;
    if not FileExists(l_FileName) then l_FileName := Win.GetInstancePath + 'Language\HtmlCode.txt';
    if not FileExists(l_FileName) then l_FileName := Win.GetInstancePath + 'Sys\HtmlCode.txt';
    if FileExists(l_FileName) then
    begin
      l_List := TStringList.Create;
      try
        l_List.LoadFromFile(l_FileName);
        for I := 0 to l_List.Count - 1 do
        begin
          l_Key := Str.GetTokenBeforeChar(l_List[I], '=');
          l_Value := Str.GetTokenAfterChar(l_List[I], '=');
          if l_Key<>'' then
          begin
            if l_Value='' then
              l_Value := ' ';
            FHtmlCodeDict.AddOrSetValue(l_Key, l_Value);
          end;
        end;
      finally
        l_List.Free;
      end;
    end;
  end;
end;

class function HTML.Color2HTML(Color: TColor): string;
var
  l_GRB: Integer;
  l_Hex: String;
begin
  l_GRB := ColorToRGB(Color);
  l_Hex := IntToHex(l_GRB, 6); // The result will be BBGGRR but I want it RRGGBB
  Result := '#' + Copy(l_Hex, 5, 2) + Copy(l_Hex, 3, 2) + Copy(l_Hex, 1, 2);
end;

class function HTML.ColorHTMLToNormal(AHtmlColor: string): TColor;
var
  l_1, l_2, l_3: string;
begin
  Result := clWhite;
  if ((Length(AHtmlColor)=7) or (Length(AHtmlColor)=9)) and (AHtmlColor[1]='#') then
  begin
    l_1 := Copy(AHtmlColor, Length(AHtmlColor)-1, 2);
    l_2 := Copy(AHtmlColor, Length(AHtmlColor)-3, 2);
    l_3 := Copy(AHtmlColor, Length(AHtmlColor)-5, 2);

    Result := RGB( Str.HexToIntDef(l_3, 255),
                   Str.HexToIntDef(l_2, 255),
                   Str.HexToIntDef(l_1, 255) );
  end;
end;

class destructor HTML.Destroy;
begin
  if Assigned(FHtmlCodeDict) then
    FHtmlCodeDict.Free;
end;

class function HTML.GetHtmlBody(sHtml: string): string;
  function _RemoveFlag(var AInStr: string; AFlag: string): Boolean;
  var
    l_h1, l_h2: Integer;
  begin
    Result := False;
    l_h1 := FastPosNoCase(AInStr, AFlag, Length(AInStr), Length(AFlag), 1);
    if l_h1 > 0 then
    begin
      l_h2 := FastPos(AInStr, '>', l_h1 + 1);
      if l_h2 > l_h1 then
      begin
        AInStr := Copy(AInStr, 1, l_h1-1) + Copy(AInStr, l_h2+1, Length(AInStr));
        Result := True;
      end;
    end;
  end;

var
  htmlBegin, bodyBegin, bodyEnd : Integer;
  l_Str: string;
begin
  Result := sHtml;
  try
    {考虑到某种情况（手机回复邮件的时候， 总是在<Html>标签前加入回复的内容。破坏整个html完整性）}
    htmlBegin := FastPosNoCase(sHtml, '<HTML', Length(sHtml), 5, 1);
    if htmlBegin > 10 then  {表示html前面还有东西}
    begin
      l_Str := Copy(sHtml, 1, htmlBegin-1);
      l_Str := HtmlToText(l_Str, false, 200); //如果是控制字符则忽略
      if Trim(l_Str)<>'' then
        Exit;
    end;

    bodyBegin := FastPosNoCase(sHtml, '<BODY', Length(sHtml), 5, 1);
    if bodyBegin <= 0 then
      Exit;

    bodyBegin := FastPosNoCase(sHtml, '>', Length(sHtml), 1, bodyBegin + 4);
    if bodyBegin <= 0 then
      Exit;

    bodyEnd := FastPosNoCase(sHtml, '</BODY>', Length(sHtml), 7, bodyBegin + 1);
    if bodyEnd <= 0 then
      Exit
    else
      Result := Trim( Copy(sHtml, bodyBegin + 1, bodyEnd - bodyBegin - 1));
  finally
    while _RemoveFlag(Result, '<HTML') do ;
    while _RemoveFlag(Result, '</HTML') do ;
    Result := Str.FastReplace(Result, '<body', '<span', false);
    Result := Str.FastReplace(Result, '</body>', '</span>', false);
  end;
end;

class function HTML.HtmlToText(const AHtml: string;
  AMarkLinks: Boolean; const AReturnLen: Integer;
  const ADefHtmlCodeFile:string): string;

  function _HtmlToTxt2: string;
  var
    NextToken, s0: string;
    I: Integer;
    HelpIdx: Integer;
    inQuot: Boolean; // 去除<script>段之用
    InputLen: Integer;
    InputIdx: Integer; // 指向输入字符的下一个待处理字符
    inPre: Boolean; // 表示是否在<pre>...</pre>段内
    CurrLink: string;
    l_BodyBegin, l_BodyEnd: Integer;

    // 取得下一段字符串
    function GetNextToken(const S: string; const StartIdx: Integer): string;
    var
      I, l_FindEndCount, l_MustFindEndCount: Integer;
    begin
      if StartIdx > Length(S) then
      begin
        Result := ' ';
        Exit;
      end;

      Result := S[StartIdx];
      if Result = '&' then
      begin
        for I := StartIdx + 1 to Length(S) do
        begin
          if CharInSet(S[I], ['&', ' ', #13, '<']) then
            break;
          Result := Result + S[I];
          if S[I] = ';' then
            break;
        end;
      end
      else if Result = '<' then
      begin
        // <A href="mailto:<!--AID_FROMADDRESS_BEGIN-->liuyc123@hotmail.com<!--AID_FROMADDRESS_END-->">
        // 这种情况的考虑 : 算法： 如果 发现 <!  则开始找到下一个 >

        l_FindEndCount := 0;
        l_MustFindEndCount := 1;
        for I := StartIdx + 1 to Length(S) do
        begin
          Result := Result + S[I];

          if I <> StartIdx + 1 then // 第一个...
            if Copy(Result, Length(Result) - 1, 2) = '<!' then
              Inc(l_MustFindEndCount);

          if S[I] = '>' then
            Inc(l_FindEndCount);

          if l_FindEndCount = l_MustFindEndCount then
            break;
        end;
      end
      else
      begin
        for I := StartIdx + 1 to Length(S) do
          if CharInSet(S[I], ['&', '<']) then
            break
          else
            Result := Result + S[I];
      end;
    end;

  // 在str 中找到第 id 个 substr的位置, length(SubStr) =1
    function NPos(SubStr, Str: string; Id: Integer): Integer;
    var
      I, Count: Integer;
    begin
      Count := 0;
      for I := 1 to Length(Str) do
      begin
        if Str[I] = SubStr then
          Inc(Count);

        if Count >= Id then
          break;
      end;

      Result := I;
    end;

  // 输入：<a href="http://delphigroup.yeah.net">
  // 输出：http://delphigroup.yeah.net
    function GetLink(S: string): string;
    var
      LPos, RPos, LQuot, RQuot: Integer;
    begin
      Result := '';

      // 去掉'....<'
      LPos := Pos('<', S);
      if LPos = 0 then
        Exit;
      delete(S, 1, LPos);
      S := Trim(S);

      // 去掉'>....'
      RPos := Pos('>', S);
      if RPos = 0 then
        Exit;
      delete(S, RPos, MaxInt);

      if Uppercase(Copy(S, 1, 2)) = 'A ' then
      begin
        LPos := Pos('HREF', Uppercase(S));
        if LPos = 0 then
          Exit;

        LQuot := NPos('"', S, 1);
        RQuot := NPos('"', S, 2);

        if (LQuot < LPos) or (RQuot > RPos) then
          Exit;

        // 开头带'#'的超链接，视为无效
        if S[LQuot + 1] = '#' then
          Exit;

        // 开头带'javascript:'的超链接，也视为无效
        // 如：<div align=right><a href="javascript:window.close()"><IMG SRC="button_close.gif"></a></div>
        if Copy(S, LQuot + 1, 11) = 'javascript:' then
          Exit;

        Result := Copy(S, LQuot + 1, RQuot - LQuot - 1);
      end;
    end;

    function MakeStr(Str: string; Id: Integer): string;
    var
      I: Integer;
      Buffer: string;
    begin
      Buffer := '';
      for I := 1 to Id do
        Buffer := Buffer + Str;

      Result := Buffer;
    end;

    function ReplaceStr(const S, Srch, Replace: string): string;
    var
      I: Integer;
      Source: string;
    begin
      Source := S;
      Result := '';
      repeat
        I := Pos(Uppercase(Srch), Uppercase(Source));
        if I > 0 then
        begin
          Result := Result + Copy(Source, 1, I - 1) + Replace;
          Source := Copy(Source, I + Length(Srch), MaxInt);
        end
        else
          Result := Result + Source;
      until I <= 0;
    end;

  // 把所有&xxx的转义；所有<xxx>取消；其它照样返回
    function ConvertHTMLToken(const S: string; var inPre: Boolean): string;
    var
      s0, s0_2, s0_3, s0_4: string;
    begin
      if S[1] = '&' then
      begin
        s0 := lowerCase(S);
        Result := '';
        if s0 = '&nbsp;' then
          Result := ' '
        else if s0 = '&quot;' then
          Result := '"'
        else if s0 = '&gt;' then
          Result := '>'
        else if s0 = '&lt;' then
          Result := '<'
        else if s0 = '&middot;' then
          Result := '.'
        else if s0 = '&trade;' then
          Result := ' TM '
        else if s0 = '&copy;' then
          Result := '(c)'
        else if s0 = '&amp;' then
          Result := '&'
        else if s0 = '&amp' then
          Result := '&'
        else if FastPosNoCase(S, '=', Length(S), 1, 1) >= 1 then // ..... &id=123
          Result := S
        else
        begin
          if not FHtmlCodeDict.TryGetValue(s0, Result) then
            Result := '?';
        end;
      end
      else if S[1] = '<' then
      begin
        s0 := lowerCase(S);
        s0_2 := Copy(s0, 1, 2);
        s0_3 := Copy(s0, 1, 3);
        s0_4 := Copy(s0, 1, 4);

        Result := '';
        // 将所有<hr>替换成为'------'
        if s0 = '<br>' then
          Result := #13#10
        else if s0_4 = '<pre' then // <pre 一定要在 <p 之前判断！
        begin
          inPre := true;
          Result := #13#10;
        end
        else if s0_2 = '<p' then
          Result := #13#10 + #13#10
        else if s0_3 = '<hr' then
          Result := #13#10 + MakeStr('-', 40) + #13#10
        else if s0_3 = '<ol' then
          Result := #13#10
        else if s0_3 = '<ul' then
          Result := #13#10
          // else if s0_4='</ol' then result:=crlf
          // else if s0_4='</ul' then result:=crlf
        else if s0_3 = '<li' then
          Result := '.'
        else if s0_4 = '<div' then
          Result := #13#10
        else if s0_4 = '</li' then
          Result := #13#10
        else if s0_4 = '</tr' then
          Result := #13#10
        else if s0 = '</td>' then
          Result := #9
          // else if s0='<title>' then result:=' 标题 <<'
          // else if s0='</title>' then result:='>>'+crlf+crlf
        else if s0 = '</pre>' then
          inPre := false
        else if Copy(s0, 1, 6) = '<table' then
          Result := #13#10
        else if AMarkLinks and (s0[2] = 'a') then
        begin
          CurrLink := GetLink(S);
          if CurrLink <> '' then
            Result := '[';
        end
        else if AMarkLinks and (s0 = '</a>') then
          if CurrLink <> '' then
            Result := format(' %s ]', [CurrLink]);
      end
      else if inPre then
        Result := S
      else // 不在<pre>..</pre>内，则删除所有CR
        Result := ReplaceStr(S, #13#10, '');
    end;

  begin
    s0 := AHtml;
    l_BodyBegin := FastPosNoCase(AHtml, '<body', Length(AHtml), 5, 1);
    if l_BodyBegin > 0 then
    begin
      l_BodyEnd := FastPosNoCase(AHtml, '</body>', Length(AHtml), 7, l_BodyBegin);
      if l_BodyEnd > 0 then
        s0 := Copy(AHtml, l_BodyBegin, l_BodyEnd - l_BodyBegin);
    end;

    Result := '';
    InputLen := Length(s0);
    InputIdx := 1;
    inPre := false;
    CurrLink := '';

    while InputIdx <= InputLen do
    begin
      NextToken := GetNextToken(s0, InputIdx);

      if lowerCase(Copy(NextToken, 1, 6)) = '<style' then // 去除<style ...> -- </style>之间的内容
      begin
        while (lowerCase(NextToken) <> '</style>') and (InputIdx <= InputLen) do
        begin
          Inc(InputIdx, Length(NextToken));
          NextToken := GetNextToken(s0, InputIdx);
        end;
        Inc(InputIdx, Length(NextToken));

        NextToken := GetNextToken(s0, InputIdx);
      end
      else if lowerCase(Copy(NextToken, 1, 7)) = '<script' then // 去除<Script ...> -- </Script>之间的内容
      begin
        Inc(InputIdx, Length(NextToken));
        inQuot := false;
        I := InputIdx - 1;
        while I < InputLen do
        begin
          Inc(I);
          if s0[I] = '"' then
          begin
            inQuot := not inQuot;
            continue;
          end;
          if not inQuot then
            // 去除<script>段里的<!-- ... -->注释段, 99.8.2
            if Copy(s0, I, 4) = '<!--' then
            begin
              HelpIdx := Pos('-->', Copy(s0, I + 4, MaxInt));
              if HelpIdx > 0 then
              begin
                Inc(I, 4 + HelpIdx + 2);
              end
              else
              begin
                I := InputLen;
                break;
              end;
            end;
          if lowerCase(Copy(s0, I, 9)) = '</script>' then
          begin
            break;
          end;
        end;
        InputIdx := I;

        NextToken := GetNextToken(s0, InputIdx);
      end;

      Inc(InputIdx, Length(NextToken));
      Result := Result + ConvertHTMLToken(NextToken, inPre);

      if (AReturnLen > 0) and (Length(Result) > AReturnLen) then
      begin
        Result := Copy(Result, 1, AReturnLen);
        Exit;
      end;
    end;
  end;

begin
  CheckHtmlCodeDictCreated(ADefHtmlCodeFile);
  try
    Result := Trim(_HtmlToTxt2);
  except
    Result := AHtml;
  end;
end;

class function HTML.RemoveBodyOverFlow(const AHtml: string): string;
  function _TrueDelete(const AInStr: string): string;
  const
    c_Strs: array[0..1] of string = ('overflow: hidden;', 'overflow:hidden;');
  begin
    Result := AInStr;
    while FastPosNoCase(Result, c_Strs[0])>0 do Result := FastReplace(Result, c_Strs[0], '');
    while FastPosNoCase(Result, c_Strs[1])>0 do Result := FastReplace(Result, c_Strs[1], '');
  end;
  function _DeleteOverFlow(const AInStr: string): string;
  var
    l_BodyBegin, l_BodyEnd, l_Len: Integer;
    l_s1, l_s2, l_s3: string;
  begin
    l_BodyBegin := FastPosNoCase(AInStr, 'body');
    if l_BodyBegin>0 then
    begin
      l_BodyEnd := FastPosNoCase(AInStr, '}', l_BodyBegin);
      if l_BodyEnd>l_BodyBegin then
      begin
        l_Len := 1; //Length('}')
        l_s1 := Copy(AInStr, 1, l_BodyBegin-1);
        l_s2 := Copy(AInStr, l_BodyBegin, l_BodyEnd-l_BodyBegin +l_Len);
        l_s3 := Copy(AInStr, l_BodyEnd+l_Len, Length(AInStr));
        Result := l_s1
            + _TrueDelete(l_s2)
            + _DeleteOverFlow(l_s3);
        Exit;
      end;
    end;
    Result := AInStr;
  end;

var
  l_StyleBegin, l_StyleEnd, l_Len: Integer;
  l_s1, l_s2, l_s3: string;
begin
  //去掉隐藏溢出 overflow: hidden
  (*
    <STYLE type=text/css>
        body{
            width: 100%;
            height: 100%;
            margin: 0;
            color: #989898;
            font-size: 14px;
            line-height: 17px;
            overflow: hidden;
        }
    </STYLE>
  *)
  l_StyleBegin := FastPosNoCase(AHtml, '<STYLE type=');
  if l_StyleBegin>0 then
  begin
    l_StyleEnd := FastPosNoCase(AHtml, '</STYLE>', l_StyleBegin);
    if l_StyleEnd > l_StyleBegin then
    begin
      l_Len := 8; //Length('</STYLE>')
      l_s1 := Copy(AHtml, 1, l_StyleBegin-1);
      l_s2 := Copy(AHtml, l_StyleBegin, l_StyleEnd-l_StyleBegin +l_Len);
      l_s3 := Copy(AHtml, l_StyleEnd+l_Len, Length(AHtml));
      Result := l_s1
          + _DeleteOverFlow(l_s2)
          + RemoveBodyOverFlow(l_s3);
      Exit;
    end;
  end;
  Result := AHtml;
end;

class function HTML.RemoveContentEditable(const AHtml: string): string;
begin

  
end;

class function HTML.RemoveScript(const AHtml: string): string;
var
  istart : Integer;
  iend : Integer;
  l_MetaStr: string;
begin
  Result  :=  AHtml;

  {过滤script}
  istart  :=  Str.FastPosNoCase(Result, '<script');
  iend  :=  Str.FastPosNoCase(Result, '</script>', istart);
  while (istart > 0) and (iend > istart) do
  begin
    Delete(Result, istart, iend - istart + 9);
    istart  :=  Str.FastPosNoCase(Result, '<script');
    iend  :=  Str.FastPosNoCase(Result, '</script>', istart);
  end;

  {过滤<meta url}
  istart  :=  Str.FastPosNoCase(Result, '<meta');
  iend  :=  Str.FastPosNoCase(Result, '>', istart);
  while (istart > 0) and (iend > istart) do
  begin
    l_MetaStr := Copy(Result, istart+5, iend - istart-5);
    if Str.FastPosNoCase(l_MetaStr, 'url') >0 then
    begin
      Delete(Result, istart, iend - istart + 1);
    end
    else
    begin
      istart := iend + 1;
    end;
    istart  :=  Str.FastPosNoCase(Result, '<meta', istart);
    iend  :=  Str.FastPosNoCase(Result, '>', istart);
  end;
end;

class function HTML.ReplaceHtmlSyntax(const AInputText: String): String;
  function _TransLine(ALine: string):string;
  var
    I: Integer;
    l_LineFisrt: string;
  begin
    l_LineFisrt := '';
    for I := 1 to Length(ALine) do
      if Trim(ALine[I]) = '' then
        l_LineFisrt := l_LineFisrt + ALine[I]
      else
        Break;
    if l_LineFisrt<>'' then
    begin
      l_LineFisrt := FastReplace(l_LineFisrt, ' ', '&nbsp;', True);
      l_LineFisrt := FastReplace(l_LineFisrt, #9, '&nbsp;&nbsp;&nbsp;&nbsp;', True);
      Result := l_LineFisrt + TrimLeft(ALine);
    end
    else
      Result := ALine;
  end;
var
  l_Text: TStringList;
  I: Integer;
begin
  Result := AInputText;
  Result := FastReplace(Result, '>', '&gt;', True);
  Result := FastReplace(Result, '<', '&lt;', True);
  Result := FastReplace(Result, '"', '&quot;', True);

  //不能把空格全部替换为“&nbsp;”，这样会导致整一大段文字不会自动换行。
  //解决办法：把行首的处理下就可以了。
  //Result := FastReplace(Result, ' ', '&nbsp;', True);
  l_Text := TStringList.Create;
  try
    l_Text.Text := Result;
    for I := 0 to l_Text.Count - 1 do
      l_Text[I] := _TransLine(l_Text[I]);
    Result := l_Text.Text;
  finally
    l_Text.Free;
  end;

  //  Result := FastReplace(Result, #39, '&apos;', True);
end;

class function HTML.TextToHtml(const sText: string; const AReplaceHtmlFlag: Boolean): string;
var
  Str: String;
  List: TStringList;
  I: Integer;
begin
  Str := Trim(sText);
  // 比如在回信的时候，作为txt模式回复，则这个时候txt里面有很多宏定义。不能替换<>这些，否则宏定义就显示出来了
  if Str = '' then
  begin
    Result := '';
    Exit;
  end;

  // 如果替换，会导致 这个失效<a href=' > </a>
  if not((not AReplaceHtmlFlag) and (Str[1] = '<') and (Str[Length(Str)] = '>')) then
    Str := StringReplace(Str, ' ', '&nbsp;', [rfReplaceAll]);

  if AReplaceHtmlFlag then
    Str := ReplaceHtmlSyntax(sText);

  List := TStringList.Create;
  try
    List.Text := Str;

    for I := 1 to List.Count - 1 do
      List[I] := '<br>' + List[I];

    Result := TrimRight(List.Text);
  finally
    List.Free;
  end;
end;

class function HTML.ToHtmlBolckQuote(sSource: string; isSourceHtml: Boolean): string;
const
  BLOCKQUOTE_BEGIN =
    '<BLOCKQUOTE dir=ltr style="PADDING-RIGHT: 0px; PADDING-LEFT: 5px; MARGIN-LEFT: 5px; BORDER-LEFT: #000000 2px solid; MARGIN-RIGHT: 0px">' + #13#10;
  BLOCKQUOTE_END = '</BLOCKQUOTE>';
begin
  if isSourceHtml then
    Result := BLOCKQUOTE_BEGIN + sSource + BLOCKQUOTE_END
  else
    Result := BLOCKQUOTE_BEGIN + TextToHtml(sSource, false) + BLOCKQUOTE_END;
end;

class function HTML.ToTextBolckQuote(sSource: string): string;
var
  List: TStringList;
  Index: Integer;
begin
  List := TStringList.Create;
  try
    List.Text := sSource;
    for Index := 0 to List.Count - 1 do
      List[Index] := '>' + List[Index];
    Result := List.Text;
  finally
    List.Free;
  end;
end;

class function HTML.UpdateHtmlCharset(const AHtmlString: string; Charset: string): String;

  function ChangeHtmlCharset(const AInHtml, ACharset: String): String;
  var
    startPos, endPos: Integer;
    function FindMeta(AStartPos: Integer): Boolean;
    const
      charsetToken = 'charset';
      metaTokenBegin = '<meta';
      metaTokenEnd = '>';
      microTokenBegin = '<!-';
    var
      metaStr: String;
    begin
      startPos := FastPosNoCase(AInHtml, metaTokenBegin, Length(AInHtml), Length(metaTokenBegin),
        AStartPos);
      if startPos <= 0 then
      begin
        Result := false;
        Exit;
      end;

      endPos := FastPos(AInHtml, metaTokenEnd, Length(AInHtml), Length(metaTokenEnd),
        startPos + Length(metaTokenBegin));
      if endPos <= 0 then
      begin
        Result := false;
        Exit;
      end;

      // charset有可能是一个宏定义，则需要处理一下.....
      // <META http-equiv=Content-Type content="text/html; charset=<!--CHARSET-->">
      metaStr := Copy(AInHtml, startPos, endPos - startPos + 1);
      if FastPosNoCase(metaStr, microTokenBegin, Length(metaStr), Length(microTokenBegin), 1)
        > 0 then
      begin
        endPos := FastPos(AInHtml, metaTokenEnd, Length(AInHtml), Length(metaTokenEnd), endPos + 1);
        if endPos <= 0 then
        begin
          Result := false;
          Exit;
        end;
      end;

      metaStr := Copy(AInHtml, startPos, endPos - startPos + 1);

      if FastPosNoCase(metaStr, charsetToken, Length(metaStr), Length(charsetToken), 1) <= 0 then
        Result := FindMeta(endPos)
      else
        Result := True;
    end;

  begin
    if FindMeta(1) then
      Result := Copy(AInHtml, 1, startPos - 1) + Format(
        '<meta http-equiv="Content-Type" content="text/html; charset=%s">', [ACharset]) + Copy
        (AInHtml, endPos + 1, Length(AInHtml))
    else
      Result := AInHtml;
  end;

var
  l_bHtml, l_eHtml, l_bBody, l_eBody: Integer;
  l_s1, l_s2: String;
begin
  Result := AHtmlString;

  l_bHtml := FastPosNoCase(Result, '<HTML>', Length(Result), 6, 1);
  l_eHtml := FastPosNoCase(Result, '</HTML>', Length(Result), 7, 1);

  if (l_bHtml <= 0) and (l_eHtml <= 0) then // 没有找到HTML标签
  begin
    l_bBody := FastPosNoCase(Result, '<BODY>', Length(Result), 6, 1);
    l_eBody := FastPosNoCase(Result, '</BODY>', Length(Result), 7, 1);

    l_s1 :=
      '<HTML><HEAD><meta http-equiv="Content-Type" content="text/html; charset=' +
      Charset + '"></HEAD>';
    l_s2 := '</HTML>';

    if (l_bBody > 0) or (l_eBody > 0) then
      Result := l_s1 + Result + l_s2
    else
      Result := l_s1 + '<BODY>' + Result + '</BODY>' + l_s2;
  end
  else
  begin
    Result := ChangeHtmlCharset(Result, Charset);
  end;
end;

class function HTML.ChangeHtmlCharset(const AInHtml, ACharset: String): String;
var
  startPos, endPos: Integer;
  function FindMeta(AStartPos: Integer): Boolean;
  const
    charsetToken = 'charset';
    metaTokenBegin = '<meta';
    metaTokenEnd = '>';
    microTokenBegin = '<!-';
  var
    metaStr: String;
  begin
    startPos := FastPosNoCase(AInHtml, metaTokenBegin, Length(AInHtml), Length(metaTokenBegin),
      AStartPos);
    if startPos <= 0 then
    begin
      Result := false;
      Exit;
    end;

    endPos := FastPos(AInHtml, metaTokenEnd, Length(AInHtml), Length(metaTokenEnd),
      startPos + Length(metaTokenBegin));
    if endPos <= 0 then
    begin
      Result := false;
      Exit;
    end;

    // charset有可能是一个宏定义，则需要处理一下.....
    // <META http-equiv=Content-Type content="text/html; charset=<!--CHARSET-->">
    metaStr := Copy(AInHtml, startPos, endPos - startPos + 1);
    if FastPosNoCase(metaStr, microTokenBegin, Length(metaStr), Length(microTokenBegin), 1) > 0 then
    begin
      endPos := FastPos(AInHtml, metaTokenEnd, Length(AInHtml), Length(metaTokenEnd), endPos + 1);
      if endPos <= 0 then
      begin
        Result := false;
        Exit;
      end;
    end;

    metaStr := Copy(AInHtml, startPos, endPos - startPos + 1);

    if FastPosNoCase(metaStr, charsetToken, Length(metaStr), Length(charsetToken), 1) <= 0 then
      Result := FindMeta(endPos)
    else
      Result := True;
  end;

begin
  if FindMeta(1) then
    Result := Copy(AInHtml, 1, startPos - 1) + Format(
      '<meta http-equiv="Content-Type" content="text/html; charset=%s">', [ACharset]) + Copy
      (AInHtml, endPos + 1, Length(AInHtml))
  else
    Result := AInHtml;
end;

class function HTML.GetCharset(const sCharset: String): string;
begin
  Result := Str.GetTokenAfterChar(uppercase(sCharset), '=', True, false);
  while (Length(Result) > 0) and (Result[1] = '"') do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = '"') do
    Result := Copy(Result, 1, Length(Result) - 1);
end;

class function HTML.GetFilteredCharset(const sCharset, defaultCharset: String): string;
begin
  Result := GetCharset(sCharset);

  if (Result = 'US-ASCII') { or (Result = 'ISO-8859-1') } or (Result = '') then
    Result := defaultCharset;
end;

{ Match }
class function Match.IsMatch(const Pattern, Text: string): Boolean;
begin
  //eg: fcstr.Match.IsMatch('已删*邮件*', '已删邮件');
  //前面部分是规则表达式，后面部分是要匹配的内容

  Result := (Matche(Pattern, Text) = 1);
end;

const
{ Match defines }
  MATCH_PATTERN	  = 6;
  MATCH_LITERAL	  = 5;
  MATCH_RANGE	  = 4;
  MATCH_ABORT	  = 3;
  MATCH_END	  = 2;
  MATCH_VALID	  = 1;
{ Pattern defines }
{  PATTERN_VALID	  =  0;
  PATTERN_ESC	  = -1;
  PATTERN_RANGE	  = -2;
  PATTERN_CLOSE	  = -3;
  PATTERN_EMPTY	  = -4;
}{ Character defines }
  MATCH_CHAR_SINGLE	            = '?';
  MATCH_CHAR_KLEENE_CLOSURE     = '*';
  MATCH_CHAR_RANGE_OPEN	        = '[';
  MATCH_CHAR_RANGE	            = '-';
  MATCH_CHAR_RANGE_CLOSE        = ']';
  MATCH_CHAR_CARET_NEGATE       = '^';
  MATCH_CHAR_EXCLAMATION_NEGATE	= '!';

class function Match.MatchAfterStar(Pattern, Text: string): Integer;
var
  P, T, PLen, TLen: Integer;
begin
  Result := 0;
  P := 1;
  T := 1;
  PLen := Length(Pattern);
  TLen := Length(Text);
  if TLen = 1 then
  begin
    Result := MATCH_VALID;
    Exit;
  end;
  if (PLen = 0) or (TLen = 0) then
  begin
    Result := MATCH_ABORT;
    Exit;
  end;
  while ((T <= TLen) and (P < PLen)) and ((Pattern[P] = MATCH_CHAR_SINGLE) or
    (Pattern[P] = MATCH_CHAR_KLEENE_CLOSURE)) do
  begin
    if Pattern[P] = MATCH_CHAR_SINGLE then
      Inc(T);
    Inc(P);
  end;
  if T >= TLen then
  begin
    Result := MATCH_ABORT;
    Exit;
  end;
  if P >= PLen then
  begin
    Result := MATCH_VALID;
    Exit;
  end;
  repeat
    if (Pattern[P] = Text[T]) or (Pattern[P] = MATCH_CHAR_RANGE_OPEN) then
    begin
      Pattern := Copy(Pattern, P, PLen);
      Text    := Copy(Text, T, TLen);
      PLen    := Length(Pattern);
      TLen    := Length(Text);
      p := 1;
      t := 1;
      Result  := Matche(Pattern, Text);
      if Result <> MATCH_VALID then
        Result := 0;//retry until end of Text, (check below) or Result valid
    end;
    Inc(T);
    if (T > TLen) or (P > PLen) then
    begin
      Result := MATCH_ABORT;
      Exit;
    end;
  until Result <> 0;
end;

class function Match.Matche(Pattern, Text: string): Integer;
var
  RangeStart, RangeEnd, P, T, PLen, TLen: Integer;
  Invert, MemberMatch, Loop: Boolean;
begin
  P := 1;
  T := 1;
  Pattern := AnsiLowerCase(pattern);
  Text    := AnsiLowerCase(Text);
  PLen    := Length(pattern);
  TLen    := Length(text);
  Result  := 0;
  while ((Result = 0) and (P <= PLen)) do
  begin
    if T > TLen then
    begin
      if (Pattern[P] = MATCH_CHAR_KLEENE_CLOSURE) and (P+1 > PLen) then
        Result := MATCH_VALID
      else
        Result := MATCH_ABORT;
      Exit;
    end
    else
      case (Pattern[P]) of
        MATCH_CHAR_KLEENE_CLOSURE:
          Result := MatchAfterStar(Copy(Pattern,P,PLen),Copy(Text,T,TLen));
        MATCH_CHAR_RANGE_OPEN:
          begin
            Inc(P);
            Invert := False;
            if (Pattern[P] = MATCH_CHAR_EXCLAMATION_NEGATE) or
              (Pattern[P] = MATCH_CHAR_CARET_NEGATE) then
            begin
              Invert := True;
              Inc(P);
            end;
            if (Pattern[P] = MATCH_CHAR_RANGE_CLOSE) then
            begin
              Result := MATCH_PATTERN;
              Exit;
            end;
            MemberMatch := False;
            Loop := True;
            while (Loop and (Pattern[P] <> MATCH_CHAR_RANGE_CLOSE)) do
            begin
              RangeStart := P;
              RangeEnd := P;
              Inc(P);
              if P > PLen then
              begin
                Result := MATCH_PATTERN;
                Exit;
              end;
              if Pattern[P] = MATCH_CHAR_RANGE then
              begin
                Inc(P);
                RangeEnd := P;
              if (P > PLen) or (Pattern[RangeEnd] = MATCH_CHAR_RANGE_CLOSE) then
              begin
                Result := MATCH_PATTERN;
                Exit;
              end;
              Inc(P);
            end;
            if P > PLen then
            begin
              Result := MATCH_PATTERN;
              Exit;
            end;
            if RangeStart < RangeEnd then
            begin
              if (Text[T] >= Pattern[RangeStart]) and
                (Text[T] <= Pattern[RangeEnd]) then
              begin
                MemberMatch := True;
                Loop := False;
              end;
            end
            else
            begin
              if (Text[T] >= Pattern[RangeEnd]) and
                (Text[T] <= Pattern[RangeStart]) then
              begin
                MemberMatch := True;
                Loop := False;
              end;
            end;
          end;
          if (Invert and MemberMatch) or (not (Invert or MemberMatch)) then
          begin
            Result := MATCH_RANGE;
            Exit;
          end;
          if MemberMatch then
            while (P <= PLen) and (Pattern[P] <> MATCH_CHAR_RANGE_CLOSE) do
              Inc(P);
            if P > PLen then
            begin
              Result := MATCH_PATTERN;
              Exit;
            end;
          end;
        else
          if Pattern[P] <> MATCH_CHAR_SINGLE then
            if Pattern[P] <> Text[T] then
              Result := MATCH_LITERAL;
      end;
    Inc(P);
    Inc(T);
  end;
  if Result = 0 then
    if T <= TLen then
      Result := MATCH_END
    else
      Result := MATCH_VALID;
end;

{ Binary }

class function Binary.GetBitValue(AIn: Int64; ABitPosition: Integer): Boolean;
begin
  if not (ABitPosition in [1..64]) then
    Exit(False);
  Result := AIn shl (64 - ABitPosition) shr 63 =1;
end;

class function Binary.SetBitValue(AIn: Int64; ABitPosition: Integer; AValue: Boolean): Int64;
var
  l_V: Int64;
begin
  if not (ABitPosition in [1..64]) then
    Exit(AIn);

  if AValue then
  begin
    l_V := 1 shl (ABitPosition-1);
    Result := AIn or l_V;
  end
  else
  begin
    l_V := (-1 shl ABitPosition) xor (-1 shl (ABitPosition-1));  //取非
    l_V := not l_V; //取反
    Result := AIn and l_V;
  end;
end;

end.
