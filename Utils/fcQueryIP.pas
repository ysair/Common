unit fcQueryIP;

interface

uses Forms, Windows, Classes, SysUtils, WinSock;

type
  TQueryIP = class
  private
    FDBFileStream: TFileStream;
    IndexStart: Cardinal;
    IndexEnd: Cardinal;
    function ReadByte(Offset: Cardinal; Num: Integer): Cardinal;
    function ReadStr(Offset: Cardinal): AnsiString;
    function QueryIP(aIP: Cardinal; var AOutCountry, AOutArea: string; const AReturnArea: Boolean=True): Boolean; overload;
  public
    constructor Create(IpDataBase: string);
    destructor Destroy; override;
    function QueryIP(aIP: AnsiString; var AOutCountry, AOutArea: string; const AReturnArea: Boolean=True): Boolean; overload;
  end;

implementation

constructor TQueryIP.Create(IpDataBase: string);
begin
  {会引发异常}
  FDBFileStream := TFileStream.Create(IpDataBase, fmOpenRead or fmShareDenyWrite);

  if FDBFileStream<>nil then
  begin
    IndexStart := ReadByte(0, 4);
    IndexEnd := ReadByte(4, 4);
  end else
  begin
    IndexStart := 0;
    IndexEnd := 0;
  end;
end;

function TQueryIP.QueryIP(aIP: Cardinal; var AOutCountry, AOutArea: string;
  const AReturnArea: Boolean=True): Boolean;

var StartPos, EndPos, Middle, ResultRecordPos: Cardinal;
    IndexIP: Cardinal;
    EndIP, RedirectMode: Cardinal;
    CountryPos, AreaPos: Cardinal;
    Country, Area: AnsiString;
    addr: in_addr;
const
  INDEX_SIZE = 7;
begin
  Result := false;
  AOutCountry := '';
  AOutArea := '';
  if (Integer(aIP)=-1) or (FDBFileStream=nil) then
    Exit;

  StartPos := IndexStart;
  EndPos := IndexEnd;
  ResultRecordPos := 0;

  while ResultRecordPos=0 do
  begin
    Middle := StartPos + (((EndPos - StartPos) div INDEX_SIZE) div 2) * INDEX_SIZE;
    if (Middle=StartPos) then
    begin
      IndexIP := ReadByte(EndPos, 4);
      if IndexIP<=aIP then
        ResultRecordPos := ReadByte(EndPos+4, 3)
      else
        ResultRecordPos := ReadByte(StartPos+4, 3);
      break;
    end;

    IndexIP := ReadByte(Middle, 4);
    if IndexIP = aIP then
    begin
      ResultRecordPos := ReadByte(Middle+4, 3);
      break;
    end else
    begin
      if IndexIP>aIP then
        EndPos := Middle
      else
        StartPos := Middle;
    end;
  end;

  if ResultRecordPos=0 then
    Exit;
  EndIP := ReadByte(ResultRecordPos, 4);
  if aIP>EndIP then
    Exit;

  RedirectMode := ReadByte(ResultRecordPos+4, 1);
  if (RedirectMode<>1) and (RedirectMode<>2) then
  begin
    Country := ReadStr(ResultRecordPos+4);
    AreaPos := FDBFileStream.Position;
    RedirectMode := ReadByte(FDBFileStream.Position,1);
    if  RedirectMode<> 2 then
      Area := ReadStr(AreaPos)
    else
      begin
       AreaPos := ReadByte(FDBFileStream.Position, 3);
       Area := ReadStr(AreaPos);
      end;
  end
  else if RedirectMode=1 then
  begin
    CountryPos := ReadByte(ResultRecordPos+5, 3);
    RedirectMode := ReadByte(CountryPos, 1);        // 国家重定向模式
    if (RedirectMode<>1) and (RedirectMode<>2) then
    begin
      Country := ReadStr(CountryPos);
      AreaPos := FDBFileStream.Position;
      RedirectMode := ReadByte(AreaPos, 1);        // 地区重定向模式
      if (RedirectMode<>1) and (RedirectMode<>2) then
      begin
        Area := ReadStr(AreaPos);
      end else
      begin
        AreaPos := ReadByte(AreaPos+1, 3);; // 地区信息字符串地址
        Area := ReadStr(AreaPos);
      end;
    end else
    begin
      AreaPos := CountryPos+4;
      CountryPos := ReadByte(CountryPos+1, 3); // 国家信息字符串地址
      Country := ReadStr(CountryPos);
      RedirectMode := ReadByte(AreaPos, 1);      // 地区重定向模式
      if (RedirectMode<>2) and (RedirectMode<>2) then
      begin
        Area := ReadStr(AreaPos);
      end else
      begin
        AreaPos := ReadByte(AreaPos+1, 3);; // 地区信息字符串地址
        Area := ReadStr(AreaPos);
      end;
    end;
  end else
  if RedirectMode=2 then
  begin
    CountryPos := ReadByte(FDBFileStream.Position, 3); // 国家信息字符串地址
    ResultRecordPos := FDBFileStream.Position;
    RedirectMode := ReadByte(ResultRecordPos, 1);  // 地区信息重定向模式
    Country := ReadStr(CountryPos);
    if (RedirectMode<>1) and (RedirectMode<>2) then
    begin
      AreaPos := ResultRecordPos; // 地区信息字符串地址
      Area := ReadStr(AreaPos);
    end else
    begin
      AreaPos := ReadByte(ResultRecordPos+1, 3);; // 地区信息字符串地址
      Area := ReadStr(AreaPos);
    end;
  end;

  Result := True;
  if (Country='IANA') or (Country='') then
  begin
    addr.S_addr := ntohl(aIP);
    AOutCountry := String(inet_ntoa(addr));
  end
  else
  begin
    AOutCountry := Trim(string(Country));

    if AReturnArea and  (Area<>'') then
      AOutArea := Trim(string(Area));
  end;
end;

// 从指定偏移处读出Num字节的数据
destructor TQueryIP.Destroy;
begin
  if Assigned(FDBFileStream) then
    FDBFileStream.Free;
  inherited;
end;

function TQueryIP.QueryIP(aIP: AnsiString; var AOutCountry, AOutArea: string;
  const AReturnArea: Boolean=True): Boolean;
begin
  try
    Result := QueryIP(htonl(inet_addr(PAnsiChar(aIP))),AOutCountry, AOutArea, AReturnArea);
  except
    Result := false;
  end;
end;

function TQueryIP.ReadByte(Offset: Cardinal; Num: integer): Cardinal;
var Buf: Cardinal;
    I: integer;
begin
  Result := 0;
  if FDBFileStream=nil then
    Exit;
  FDBFileStream.Position := Offset;
  for I := 0 to Num - 1 do
  begin
    FDBFileStream.ReadBuffer(Buf, 1);
    Result := (Buf shl (I*8)) or Result;
  end;
end;

// 从指定偏移处读出以0为结尾的字符串
function TQueryIP.ReadStr(Offset: Cardinal): AnsiString;
var Buf: AnsiChar;
begin
  Result := '';
  if Offset<20 then
    exit;
  if FDBFileStream=nil then
    exit;
  FDBFileStream.Position := Offset;
  repeat
    if FDBFileStream.Read(Buf, 1)<=0 then
      break;
    if Buf=char(0) then
      break;
    Result := Result + Buf;
  until False;
end;

end.

