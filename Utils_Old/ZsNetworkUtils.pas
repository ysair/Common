unit ZsNetworkUtils;

interface

const IP_REGEX_EXPRESSION = '[0123456789]{1,3}(\.[0123456789]{1,3}){3,5}';

function IsIPAddress( const Str : string):boolean;

function IsLanIPAddress(const Str: String): Boolean;

implementation

uses
  Classes, SysUtils, RegExpr, ZsStringUtils;

var
  ipAddressRegExpression:TRegExpr;

function GetIPAddressRegExpression : TRegExpr;
begin
  if not Assigned(ipAddressRegExpression) then
  begin
    ipAddressRegExpression := TRegExpr.Create;
    ipAddressRegExpression.Expression := IP_REGEX_EXPRESSION;
  end;

  Result := ipAddressRegExpression;
end;

function IsIPAddress( const Str : string):boolean;
var
  sections : TStringList;
  i, section: integer;
  reg : TRegExpr;
begin
  reg := GetIPAddressRegExpression;
  
  if reg.Exec(Str) and (Str = reg.Match[0]) then
  begin
    sections := TStringList.Create;
    try
      GetTokenToList(Str, '.', sections);
      for i := 0 to sections.Count - 1 do
      begin
        section := StrToIntDef(sections[i], -1);
        if (section < 0) or (section > 255) then
        begin
          Result := False;
          Exit;
        end;
      end;

      Result := True;
    finally
      sections.Free;
    end;
  end
  else
    Result := False;
end;

function IsLanIPAddress(const Str: String): Boolean;
const
  LanIPStr : array[0..18] of String = (
    '10.',
    '169.',
    '172.16.', '172.17.', '172.18.', '172.19.', '172.20.',
    '172.21.', '172.22.', '172.23.', '172.24.', '172.25.',
    '172.26.', '172.27.', '172.28.', '172.29.', '172.30.',
    '172.31.', '192.168.');
var
  i : Integer;
begin
  Result := IsIPAddress(Str);
  if not Result then
    Exit;

  if '127.0.0.1' = Str then
  begin
    Result := True;
    Exit;
  end;

  for i := Low(LanIPStr) to High(LanIPStr) do
  begin
    if IsStartWith(LanIPStr[i], Str, true) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

initialization

finalization
  if Assigned(ipAddressRegExpression) then
    ipAddressRegExpression.Free;

end.
