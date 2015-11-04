unit ZsHtmlUtils;

interface

uses
  Classes, ActiveX, Graphics;

function ReplaceHtmlSyntax(const AInputText: String): String;

function TextToHtml(const sText: string; const ReplaceHtmlFlag: Boolean): string;

function ChangeHtmlCharset(const AInHtml, ACharset: String): String;

function GetHtmlBody(sText: string): string;

//从Stream中读取源码到IHtmlDocument2
procedure LoadDocumentSourceFromStream(Document: IDispatch; Stream: TStream);

//保存IHtmlDocument2源码到Stream中
procedure SaveDocumentSourceToStream(Document: IDispatch; Stream: TStream);

//从IHtmlDocument2读取源码
function GetHtmlDocument(Document: IDispatch; var ResultStr:String): Boolean; overload;

//向IHtmlDocument2设置源码
function SetHtmlDocument(Document: IDispatch; const SourceStr: String): Boolean;

//得到WebBrowser的源码
//function GetHtmlDocument(theBrowser: TEmbeddedWB; var ResultStr: string): Boolean; overload;

function GetCharset(const sCharset:String): string;

//将'US-ASCII'或者空的Charset转换为默认的charset
function GetFilteredCharset(const sCharset, defaultCharset:String): string;

function Color2HTML(Color: TColor): string;

function UpdateHtmlCharset(const AHtmlString: string; Charset: string): String;

implementation

uses
  SysUtils, ZsStringUtils, FReplace;

function Color2HTML(Color: TColor): string;
var
  l_GRB  : Integer;
  l_Hex : String;
begin
  l_GRB := ColorToRGB(Color);
  l_Hex := IntToHex(l_GRB, 6); //The result will be BBGGRR but I want it RRGGBB
  Result := '#' + Copy(l_Hex, 5, 2) + Copy(l_Hex, 3, 2) + Copy(l_Hex, 1, 2);
end;

{function IEColor2Integer(AColor: String): Integer;
begin
  Result := clWindow;
  if (Length(AColor)=7) and (AColor[1]='#') and (StrToIntDef(Copy(AColor, 2, 6),0)>0) then
  begin
    Result := StrToInt('$'+Copy())

  end;
end;}

function ReplaceHtmlSyntax(const AInputText: String): String;
begin
  Result := AInputText;
  Result := FastReplace(Result, '>', '&gt;', True);
  Result := FastReplace(Result, '<', '&lt;', True);
  Result := FastReplace(Result, '"', '&quot;', True);
  Result := FastReplace(Result, ' ', '&nbsp;', True);
  Result := FastReplace(Result, #39, '&apos;', True);
//  Result := FastReplace(Result, '~', '%7E', True);

end;

function TextToHtml(const sText: string; const ReplaceHtmlFlag: Boolean): string;
var
  Str :String;
  List: TStringList;
  i: integer;
begin
  Str := Trim(sText);
  //比如在回信的时候，作为txt模式回复，则这个时候txt里面有很多宏定义。不能替换<>这些，否则宏定义就显示出来了
  if Str = '' then
  begin
    Result := '';
    Exit;
  end;

  //  如果替换，会导致 这个失效<a href=' > </a>
  if not ( (not ReplaceHtmlFlag) and (Str[1]='<') and (Str[Length(Str)]='>')) then
    Str := StringReplace(Str, ' ', '&nbsp;', [rfReplaceAll]);
    
  if ReplaceHtmlFlag then
    Str := ReplaceHtmlSyntax(sText);

  List := TStringList.Create;
  try
    List.Text := Str;

    for i := 1 to List.Count - 1 do
      List[i] := '<br>' + List[i];

    Result := TrimRight(List.Text);
  finally
    List.Free;
  end;
end;

function ChangeHtmlCharset(const AInHtml, ACharset: String): String;
var
  startPos, endPos: Integer;
  function FindMeta(AStartPos : Integer) : boolean;
  const
    charsetToken  = 'charset';
    metaTokenBegin = '<meta';
    metaTokenEnd = '>';
    microTokenBegin = '<!-';
  var
    metaStr: String;
  begin
    startPos := FastPosNoCase(AInHtml, metaTokenBegin, Length(AInHtml), Length(metaTokenBegin), AStartPos);
    if startPos <= 0 then
    begin
      Result := False;
      Exit;
    end;

    endPos := FastPos(AInHtml, metaTokenEnd, Length(AInHtml), Length(metaTokenEnd), startPos + Length(metaTokenBegin));
    if endPos <= 0 then
    begin
      Result := False;
      Exit;
    end;

    //charset有可能是一个宏定义，则需要处理一下.....
    //<META http-equiv=Content-Type content="text/html; charset=<!--CHARSET-->">
    metaStr := Copy(AInHtml, startPos, endPos - startPos + 1);
    if FastPosNoCase(metaStr, microTokenBegin, Length(metaStr), Length(microTokenBegin), 1) >0 then
    begin
      endPos := FastPos(AInHtml, metaTokenEnd, Length(AInHtml), Length(metaTokenEnd), endPos+1);
      if endPos <=0 then
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
    Result := Copy(AInHtml, 1, startPos - 1) +
            Format('<meta http-equiv="Content-Type" content="text/html; charset=%s">', [ACharset])+
            copy(AInHtml, endPos + 1, Length(AInHtml))
  else
    Result := AInHtml;
end;

function GetHtmlBody(sText: string): string;
var
  bodyBegin : Integer;
  bodyEnd   : Integer;
begin
  bodyBegin := FastPosNoCase(sText, '<BODY', Length(sText), 5, 1);

  if bodyBegin <= 0 then
  begin
    Result := sText;
    Exit;
  end;

  bodyBegin := FastPosNoCase(sText, '>', Length(sText), 1, bodyBegin + 4);
  if bodyBegin <= 0 then
  begin
    Result := sText;
    Exit;
  end;

  bodyEnd := FastPosNoCase(sText, '</BODY>', Length(sText), 7, bodyBegin + 1);
  if bodyEnd <= 0 then
    Result := sText
  else
    Result := Copy(sText, bodyBegin + 1, bodyEnd - bodyBegin - 1);
end;

procedure LoadDocumentSourceFromStream(Document: IDispatch;
  Stream: TStream);
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  Stream.Position:= 0;

  if Document.QueryInterface(IPersistStreamInit,
    PersistStreamInit) = S_OK then
  begin
    StreamAdapter:= TStreamAdapter.Create(Stream, soReference);
    PersistStreamInit.Load(StreamAdapter);
    StreamAdapter:= nil;
  end;
end;

procedure SaveDocumentSourceToStream(Document: IDispatch;
  Stream: TStream);
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  Stream.Size:= 0;
  Stream.Position:= 0;

  if Document.QueryInterface(IPersistStreamInit,
    PersistStreamInit) = S_OK then
  begin
    StreamAdapter:= TStreamAdapter.Create(Stream, soReference);
    PersistStreamInit.Save(StreamAdapter, False);
    StreamAdapter:= nil;
  end;
end;

function GetHtmlDocument(Document: IDispatch; var ResultStr:String): Boolean;
var
  StrStream:TStringStream;
begin
  Result := false;

  if Document <> nil then
  begin
    StrStream := TStringStream.Create('');
    SaveDocumentSourceToStream(Document,StrStream);
    ResultStr := StrStream.DataString;
    StrStream.Free;
    Result := true;
  end;
end;

function SetHtmlDocument(Document: IDispatch; const  SourceStr: String): Boolean;
var
  StrStream:TStringStream;
begin
  Result := false;

  if Document <> nil then
  begin
    StrStream := TStringStream.Create(SourceStr);
    LoadDocumentSourceFromStream(Document,StrStream);
    StrStream.Free;
    Result := true;
  end;
end;

//function GetHtmlDocument(theBrowser: TEmbeddedWB; var ResultStr: string): Boolean;
//begin
//  Result := Assigned(theBrowser) and GetHtmlDocument(theBrowser.Document, ResultStr);
//end;
//
function GetCharset(const sCharset : String): string;
begin
  Result := GetTokenAfterChar(UpperCase(sCharset), '=', true, false);
  while (Length(Result)>0) and (Result[1]='"') do
    Delete(Result, 1,1);
  while (Length(Result)>0) and (Result[Length(Result)]='"') do
    Result := Copy(Result, 1, Length(Result)-1);
end;

function GetFilteredCharset(const sCharset, defaultCharset:String): string;
begin
  Result := GetCharset(sCharset);

  if (Result = 'US-ASCII') {or (Result = 'ISO-8859-1')} or (Result = '') then
    Result := defaultCharset;
end;

function UpdateHtmlCharset(const AHtmlString: string; Charset: string): String;
var
  l_bHtml, l_eHtml, l_bBody, l_eBody: Integer;
  l_s1, l_s2: String;
begin
  Result := AHtmlString;

  l_bHtml := FastPosNoCase(Result, '<HTML>', Length(Result), 6, 1);
  l_eHtml := FastPosNoCase(Result, '</HTML>', Length(Result), 7, 1);


  if (l_bHtml<=0) and (l_eHtml<=0) then   //没有找到HTML标签
  begin
    l_bBody := FastPosNoCase(Result, '<BODY>', Length(Result), 6, 1);
    l_eBody := FastPosNoCase(Result, '</BODY>', Length(Result), 7, 1);

    l_s1 := '<HTML><HEAD><meta http-equiv="Content-Type" content="text/html; charset='+CharSet+'"></HEAD>';
    l_s2 := '</HTML>';

    if (l_bBody>0) or (l_eBody>0) then
      Result := l_s1 +Result + l_s2
    else
      Result := l_s1+ '<BODY>'+Result + '</BODY>'+l_s2;
  end
  else
  begin
    Result := ChangeHtmlCharset(Result, Charset);
  end;
end;

end.
