unit ZsEmailUtils;

interface

uses
  Classes, RegExpr;

function ToTextBolckQuote(sSource: string): string;
function ToHtmlBolckQuote(sSource: string; isSourceHtml: Boolean): string;

function GetIPAddressFromMailLine(const Aline:String):String;

function GetIPAddressFromMailSource(const AMailSourceList: TStringList; var IsFirstIP: Boolean): String;

implementation

uses
  SysUtils, ZsStringUtils, ZsHtmlUtils, ZsNetworkUtils;

function ToTextBolckQuote(sSource: string): string;
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

function ToHtmlBolckQuote(sSource: string; isSourceHtml: Boolean): string;
const
  BLOCKQUOTE_BEGIN = '<BLOCKQUOTE dir=ltr style="PADDING-RIGHT: 0px; PADDING-LEFT: 5px; MARGIN-LEFT: 5px; BORDER-LEFT: #000000 2px solid; MARGIN-RIGHT: 0px">' + #13#10;
  BLOCKQUOTE_END = '</BLOCKQUOTE>';
begin
  if isSourceHtml then
    Result := BLOCKQUOTE_BEGIN + sSource + BLOCKQUOTE_END
  else
    Result := BLOCKQUOTE_BEGIN + TextToHtml(sSource, false) + BLOCKQUOTE_END;
end;

function GetEmailIPAddressRegExpression : TRegExpr;
const
  EMAIL_IP_REGEX_EXPRESSION = '[\[|\(|\ |\:|\;|\,]' + IP_REGEX_EXPRESSION + '[\]|\)|\ |\:|\;|\,]';
 //EMAIL_IP_REGEX_EXPRESSION = '[[( :]' + IP_REGEX_EXPRESSION + '[\]|\)|\ |\:|\;]';
begin     
  Result := TRegExpr.Create;
  Result.Expression := EMAIL_IP_REGEX_EXPRESSION;
end;

function GetIPAddressFromMailLine(const Aline:String):String;
var
  reg:TRegExpr;
begin
  reg := GetEmailIPAddressRegExpression;
  try
    if reg.Exec(Aline) then
    begin
      Result := reg.Match[0];
      Result := Copy(Result, 2, Length(Result) - 2);
      if not IsIPAddress(Result) then
        Result := '';
    end
    else
      Result := '';
  finally
    reg.Free;
  end;
end;

//IsFirstIP指出是否第一个找到的IP，一般的，如果IsFirstIP为True，则这个信息比较准确
function GetIPAddressFromMailSource(const AMailSourceList: TStringList; var IsFirstIP: Boolean): String;
  function GetXOriginatingIP : String;
  var
    iHeader : integer;
    l_Str: String;
  begin
    for iHeader := 0 to AMailSourceList.Count - 1 do
    begin
      if (IsStartWith('X-Originating-IP:', AMailSourceList[iHeader], True)) then
      begin
        Result := GetIPAddressFromMailLine(AMailSourceList[iHeader]);
        if Result = '' then
        begin
          l_Str := GetTokenAfterChar(AMailSourceList[iHeader], ':', True, True);
          if IsIPAddress(l_Str) then
            Result := l_Str;
        end;

        Exit;
      end;
    end;
  end;

  procedure GetValidHeader(const headerList: TStringList);
  var
    iHeader : integer;
  begin
    headerList.Clear;
    iHeader := 0;
    while iHeader < AMailSourceList.Count -1 do
    begin
      if ('' = AMailSourceList[iHeader]) or
         (IsStartWith('From:', AMailSourceList[iHeader], True)) or
         (IsStartWith('To:', AMailSourceList[iHeader], True)) or
         (IsStartWith('Date:', AMailSourceList[iHeader], True)) or
         (IsStartWith('Subject:', AMailSourceList[iHeader], True)) then
         Exit;

      if (IsStartWith('Received:', AMailSourceList[iHeader], True)) or
         (IsStartWith('X-', AMailSourceList[iHeader], False)) then
      begin
        headerList.Add(AMailSourceList[iHeader]);
        while (iHeader < AMailSourceList.Count -2) and (AMailSourceList[iHeader+1]<>'') and (Trim(AMailSourceList[iHeader+1][1])='') do
        begin
          Inc(iHeader);
          headerList.Add(AMailSourceList[iHeader]);
        end;
      end;

      if AMailSourceList[iHeader] = '' then
        Exit;
        
      Inc(iHeader);
    end;
  end;

  function GetIPAddress(AInLine: String; const lanIPList: TStringList) : String;
  begin
    Result := GetIPAddressFromMailLine(AInLine);
    if Result<>'' then
    begin
      if IsLanIPAddress(Result) then
      begin
        lanIPList.Add(Result);
        Result := '';
      end;
    end;
  end;

var
  i: integer;
  l_ValidHeader, l_LanIpList : TStringList;
begin

  Result := GetXOriginatingIP;
  if (Result<>'') and (not IsLanIPAddress(Result)) then
  begin
    IsFirstIP := True;
    Exit;
  end;

  l_LanIpList := TStringList.Create;
  l_ValidHeader := TStringList.Create;

  try
    GetValidHeader(l_ValidHeader);
    
    if l_ValidHeader.Count = 0 then
      Exit;

    for i := l_ValidHeader.Count-1 downto 0 do
    begin
      Result := GetIPAddress(l_ValidHeader[i], l_LanIpList);
      if '' <> Result then
        break;
    end;

    if '' <> Result then
      IsFirstIP := 0 = l_LanIpList.Count
    else
    begin
      IsFirstIP := False;


      if l_LanIpList.Count > 0 then
        Result := l_LanIpList[0]
      else
        Result := '';
    end;
  finally
    l_LanIpList.Free;
    l_ValidHeader.Free;
  end;
end;

initialization


end.
