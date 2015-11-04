unit uHttpRequest;

interface

uses
  WinInet, Windows, SysUtils, Classes, StrUtils;

const
  MAX_HeadersLength = 1024;

type
  THTTPRequestAction  = (haGet, haPost, haPut);

  THttpRequest  = class
  private
    FRedirect: Boolean;
    FStatus: Integer;
    FURL: string;
    FAddress: string;
    FResultStream: TMemoryStream;
    FConnectTime: DWORD;
    FGetTime: DWORD;
    FSize: Integer;
    FAction: THTTPRequestAction;

    FSession  : HInternet;
    FConnect  : HInternet;
    FRequest  : HInternet;
    FHostName : string;
    FFileName : string;
    FContent: string;
    FResultHeaders: string;
    FNoCache: Boolean;
  private
    procedure ParseURL;
    procedure CloseHandles;
    function GetResultStreamAsString: string;
  public
    property URL  : string read FURL Write FURL;
    property Address : string read FAddress Write FAddress;
    property Action : THTTPRequestAction read FAction Write FAction;
    property Redirect : Boolean read FRedirect Write FRedirect;
    property NoCache : Boolean read FNoCache write FNoCache;
    property Content  : string read FContent Write FContent;

    property Status : Integer read FStatus;
    property ResultStream : TMemoryStream read FResultStream Write FResultStream;
    property ResultHeaders  : string read FResultHeaders;
    property ConnectTime  : DWORD read FConnectTime;
    property GetTime  : DWORD read FGetTime;
    property Size : Integer read FSize;
    property ResultStreamAsString : string read GetResultStreamAsString;
  public
    destructor Destroy; override;

    function  OpenRequest:Boolean;
    function  SendRequest:Boolean;
    procedure AddHeader(const AStr  : string); overload;
    procedure AddHeader(const AList : TStrings); overload;
  end;

  THttpRequestThread = class(TThread)
  private
    FRedirect: Boolean;
    FStatus: Integer;
    FURL: string;
    FAddress: string;
    FMD5: string;
    FConnectTime: DWORD;
    FGetTime: DWORD;
    FSize: Integer;
    FCount: Integer;
    FResultText: string;
    FTag: Integer;
    FHttpRequest  : THttpRequest;
    FNoCache: Boolean;
  private
    function  GetResultStream: TMemoryStream;
    function GetResultStreamAsString: string;
  protected
    procedure Execute; override;
  public
    property URL  : string read FURL Write FURL;
    property Address : string read FAddress Write FAddress;
    property Redirect : Boolean read FRedirect Write FRedirect;
    property NoCache : Boolean read FNoCache write FNoCache;
    property Status : Integer read FStatus;
    property ResultStream : TMemoryStream read GetResultStream;
    property ConnectTime  : DWORD read FConnectTime;
    property GetTime  : DWORD read FGetTime;
    property MD5  : string read FMD5;
    property Size : Integer read FSize;
    property Count : Integer read FCount Write FCount;
    property ResultText : string read FResultText;
    property Tag  : Integer read FTag Write FTag;
    property ResultStreamAsString : string read GetResultStreamAsString;
  public
    constructor Create(
        const AURL  : string
        ); overload;
    constructor Create(
        const AAddress  : string;
        const AURL      : string;
        const ARedirect : Boolean;
        const ANoCache  : Boolean;
        const ACount    : Integer
        ); overload;
    destructor Destroy; override;
  end;

{function HttpRequest(
    const AURL  : string;
    const AAddress  : string;
    const ARedirect : Boolean;
    var   AStatus : Integer;
    var   AResultStream : TMemoryStream;
    var   AConnectTime  : DWORD;
    var   AGetTime      : DWORD;
    const AHeader       : TStrings = nil
    ):Boolean;//}

function URLEncode(const S: string; const InQueryString: Boolean = False): string;

implementation

uses md5;

const
  READ_LENGTH = $FFFF;
  SHTTPRequestAction  : array[haGet..haPut] of PChar = (
      'GET',
      'POST',
      'PUT'
      );

function URLEncode(const S: string; const InQueryString: Boolean = False): string;
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    case S[Idx] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.','@',':','/':
        Result := Result + S[Idx];
      ' ':
        if InQueryString then
          Result := Result + '+'
        else
          Result := Result + '%20';
      else
        Result := Result + '%' + SysUtils.IntToHex(Ord(S[Idx]), 2);
    end;
  end;
end;
//function HttpRequest(
//    const AURL  : string;
//    const AAddress  : string;
//    const ARedirect : Boolean;
//    var   AStatus : Integer;
//    var   AResultStream : TMemoryStream;
//    var   AConnectTime  : DWORD;
//    var   AGetTime      : DWORD;
//    const AHeader       : TStrings = nil
//    ):Boolean;
//var
//  hSession, hConnect, hRequest: hInternet;
//  HostName  : string;
//  Address   : string;
//  FileName  : String;
//  Buf: Pointer;
//  dwBufLen, dwIndex: DWord;
//  Data: Array[0..READ_LENGTH - 1] of Char;
//  InternetFlag: DWord;
//  BytesReaded: Integer;
//  BytesToRead: cardinal;
//  AcceptTypes : array[0..1] of PChar;
//  Header  : string;
//
//  procedure ParseURL(URL: String; var HostName, FileName: String);
//  var
//    i: Integer;
//  begin
//    if Pos('http://', LowerCase(URL)) <> 0 then
//      Delete(URL, 1, 7);
//
//    i := Pos('/', URL);
//    HostName := Copy(URL, 1, i);
//    FileName := Copy(URL, i, Length(URL) - i + 1);
//
//    if (Length(HostName) > 0) and (HostName[Length(HostName)] = '/') then
//      SetLength(HostName, Length(HostName) - 1);
//  end;
//
//  procedure AddHeader(const AStr  : string);
//  begin
//    HttpAddRequestHeaders(
//        hRequest,
//        PChar(AStr),
//        Length(AStr),
//        1
//        );
//  end;
//
//  procedure CloseHandles;
//  begin
//    InternetCloseHandle(hRequest);
//    InternetCloseHandle(hConnect);
//    InternetCloseHandle(hSession);
//  end;
//
//begin
//  ParseURL(AURL, HostName, FileName);
//  Address := AAddress;
//  if Address = '' then
//    Address  :=  HostName;
//  AConnectTime  :=  GetTickCount;
//  hSession := InternetOpen(
//      nil,
//      INTERNET_OPEN_TYPE_PRECONFIG,
//      nil,
//      nil,
//      0
//      );
//
//  hConnect := InternetConnect(
//      hSession,
//      PChar(Address),
//      INTERNET_DEFAULT_HTTP_PORT,
//      nil,
//      nil,
//      INTERNET_SERVICE_HTTP,
//      0,
//      0
//      );
//
//  InternetFlag := INTERNET_FLAG_RELOAD;// or INTERNET_FLAG_NO_AUTO_REDIRECT;
//  if not ARedirect then
//    InternetFlag  :=  InternetFlag or INTERNET_FLAG_NO_AUTO_REDIRECT;
//
//  AConnectTime  :=  GetTickCount - AConnectTime;
//  AGetTime  :=  GetTickCount;
//  AcceptTypes[0]  :=  '*.*';
//  AcceptTypes[1]  :=  nil;
//  hRequest := HttpOpenRequest(
//      hConnect,
//      'GET',
//      PChar(FileName),
//      'HTTP/1.1',
//      PChar(AURL),
//      nil,//@AcceptTypes,
//      InternetFlag,
//      0
//      );
//
//  AddHeader('host: ' + HostName);
//  AddHeader('Accept: */*');
//  AddHeader('Accept-Language: zh-cn');
//  AddHeader('Accept-Encoding: gzip, deflate');
//  AddHeader('User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)');
//  AddHeader('Connection: Keep-Alive');
//
//  {Header  :=  'GET /icast/mv/samsung/44611/title.swf HTTP/1.1'#13#10;
//  Header  :=  Header + 'Accept: */*'#13#10;
//  Header  :=  Header + 'Accept-Language: zh-cn'#13#10;
//  Header  :=  Header + 'UA-CPU: x86'#13#10;
//  Header  :=  Header + 'Accept-Encoding: gzip, deflate'#13#10;
//  Header  :=  Header + 'If-Modified-Since: Mon, 21 May 2007 08:35:51 GMT; length=1703'#13#10;
//  Header  :=  Header + 'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)'#13#10;
//  Header  :=  Header + 'Host: dldn.icast.cn'#13#10;
//  Header  :=  Header + 'Connection: Keep-Alive';
//  Header  :=  'Connection: Keep-Alive';  //}
//
//  HttpSendRequest(
//      hRequest,
//      nil,//PChar(Header),
//      0,//Length(Header),
//      nil,
//      0
//      );
//
//  dwIndex := 0;
//  dwBufLen := 1024;
//  GetMem(Buf, dwBufLen);
//  try
//    Result := HttpQueryInfo(
//        hRequest,
//        HTTP_QUERY_STATUS_CODE,
//        Buf,
//        dwBufLen,
//        dwIndex
//        );
//    AStatus :=  StrToIntDef(StrPas(PChar(Buf)), 0);
//  finally
//    FreeMem(Buf);
//  end;
//
//  if Assigned(AResultStream) then
//    AResultStream.Clear;
//  if Result then
//  begin
//    BytesReaded := 0;
//    if AResultStream = nil then
//      AResultStream :=  TMemoryStream.Create;
//    while True do
//    begin
//      if not InternetReadFile(
//          hRequest,
//          @Data,
//          SizeOf(Data),
//          BytesToRead
//          ) then
//        Break
//      else if BytesToRead = 0 then
//        Break
//      else begin
//        AResultStream.Write(Data[0], BytesToRead);
//        inc(BytesReaded, BytesToRead);
//      end;
//    end;
//
//    Result := BytesReaded <> 0;
//  end;
//
//  CloseHandles;
//  AGetTime  :=  GetTickCount - AGetTime;
//end;

{ THttpRequest }

destructor THttpRequest.Destroy;
begin
  CloseHandles;
  if Assigned(FResultStream) then
    FResultStream.Free;
  inherited;
end;

procedure THttpRequest.ParseURL;
var
  i: Integer;
begin
  if Pos('http://', LowerCase(URL)) <> 0 then
    Delete(FURL, 1, 7);

  i := Pos('/', URL);
  FHostName := Copy(FURL, 1, i);
  FFileName := Copy(FURL, i, Length(FURL) - i + 1);

  if (Length(FHostName) > 0) and (FHostName[Length(FHostName)] = '/') then
    SetLength(FHostName, Length(FHostName) - 1);
end;

procedure THttpRequest.AddHeader(const AStr: string);
begin
  if Assigned(FRequest) then
    HttpAddRequestHeaders(
        FRequest,
        PChar(AStr),
        Length(AStr),
        1
        );
end;

procedure THttpRequest.AddHeader(const AList: TStrings);
var
  i : Integer;
begin
  for i := 0 to AList.Count - 1 do
    AddHeader(AList[i]);
end;

function THttpRequest.OpenRequest: Boolean;
var
  Address   : string;
  InternetFlag: DWord;
  Port  : Integer;
begin
  ParseURL;
  Address := FAddress;
  if Address = '' then
    Address  :=  FHostName;

  if Pos(':', Address) > 0 then
  begin
    Port  :=  StrToIntDef(MidStr(Address, Pos(':', Address) + 1, 5), INTERNET_DEFAULT_HTTP_PORT);
    Address :=  LeftStr(Address, Pos(':', Address) - 1);
  end
  else
    Port  :=  INTERNET_DEFAULT_HTTP_PORT;

  FConnectTime  :=  GetTickCount;
  FSession := InternetOpen(
      nil,
      INTERNET_OPEN_TYPE_PRECONFIG,
      nil,
      nil,
      0
      );

  FConnect := InternetConnect(
      FSession,
      PChar(Address),
      Port,
      nil,
      nil,
      INTERNET_SERVICE_HTTP,
      0,
      0
      );

  InternetFlag := 0;//INTERNET_FLAG_RELOAD;
  if FNoCache then
    InternetFlag  :=  InternetFlag or INTERNET_FLAG_RELOAD;
  if not FRedirect then
    InternetFlag  :=  InternetFlag or INTERNET_FLAG_NO_AUTO_REDIRECT;

  FRequest := HttpOpenRequest(
      FConnect,
      SHTTPRequestAction[FAction],
      PChar(FFileName),
      'HTTP/1.1',
      PChar(FURL),
      nil,//@AcceptTypes,
      InternetFlag,
      0
      );

  AddHeader('host: ' + FHostName);
  FConnectTime  :=  GetTickCount - FConnectTime;
  Result  :=  Assigned(FRequest);
end;

function THttpRequest.SendRequest : Boolean;
var
  Buf: Pointer;
  dwBufLen, dwIndex: DWord;
  Data: Array[0..READ_LENGTH - 1] of Char;
  BytesReaded: Integer;
  BytesToRead: Cardinal;
  //AcceptTypes : array[0..1] of PChar;
  //Header  : string;
begin
  if not Assigned(FRequest) then
    OpenRequest;
  FGetTime      :=  GetTickCount;
  HttpSendRequest(
      FRequest,
      nil,//PChar(Header),
      0,//Length(Header),
      PChar(FContent),
      Length(FContent)
      );

  dwIndex := 0;
  dwBufLen := MAX_HeadersLength;
  GetMem(Buf, dwBufLen);
  try
    Result := HttpQueryInfo(
        FRequest,
        HTTP_QUERY_STATUS_CODE,
        Buf,
        dwBufLen,
        dwIndex
        );
    FStatus :=  StrToIntDef(StrPas(PChar(Buf)), 0);
    dwBufLen := MAX_HeadersLength;
    if HttpQueryInfo(
        FRequest,
        HTTP_QUERY_RAW_HEADERS_CRLF,
        Buf,
        dwBufLen,
        dwIndex
        ) then
      FResultHeaders  :=  StrPas(PChar(Buf))
    else
      FResultHeaders  :=  '';
  finally
    FreeMem(Buf);
  end;

  if Assigned(FResultStream) then
    FResultStream.Clear;
  if Result then
  begin
    BytesReaded := 0;
    if FResultStream = nil then
      FResultStream :=  TMemoryStream.Create;
    while True do
    begin
      if not InternetReadFile(
          FRequest,
          @Data,
          SizeOf(Data),
          BytesToRead
          ) then
        Break
      else if BytesToRead = 0 then
        Break
      else begin
        FResultStream.Write(Data[0], BytesToRead);
        inc(BytesReaded, BytesToRead);
      end;
    end;

    Result := BytesReaded <> 0;
    if Result then
      FSize :=  FResultStream.Size;
  end;

  FGetTime  :=  GetTickCount - FGetTime;
  CloseHandles;
end;

procedure THttpRequest.CloseHandles;
begin
  if Assigned(FRequest) then
  begin
    InternetCloseHandle(FRequest);
    FRequest  :=  nil;
  end;
  if Assigned(FConnect) then
  begin
    InternetCloseHandle(FConnect);
    FConnect  :=  nil;
  end;
  if Assigned(FSession) then
  begin
    InternetCloseHandle(FSession);
    FSession  :=  nil;
  end;
end;

function THttpRequest.GetResultStreamAsString: string;
begin
  Result  :=  '';
  if Assigned(ResultStream) then
  begin
    SetLength(Result, ResultStream.Size);
    CopyMemory(@Result[1], ResultStream.Memory, ResultStream.Size);
  end;
end;

{ THttpRequestThread }

constructor THttpRequestThread.Create(const AURL: string);
begin
  FURL  :=  AURL;
  inherited Create(False);
end;

constructor THttpRequestThread.Create(const AAddress, AURL: string;
  const ARedirect, ANoCache : Boolean; const ACount : Integer);
begin
  FAddress  :=  AAddress;
  FURL  :=  AURL;
  FRedirect := ARedirect;
  FNoCache := ANoCache;
  FCount := ACount;
  inherited Create(False);
end;

destructor THttpRequestThread.Destroy;
begin
  if Assigned(FHttpRequest) then
    FHttpRequest.Free;
  inherited;
end;

procedure THttpRequestThread.Execute;

  function GetFormatStr(const AStr : string; const ALen : Integer; const ARight : Boolean = True):string;
  begin
    Result  :=  AStr;
    if Length(AStr) < ALen then
      if ARight then
        Result  :=  DupeString(' ', ALen - Length(AStr)) + Result
      else
        Result  :=  Result + DupeString(' ', ALen - Length(AStr));
  end;

var
  i : Integer;
  c : Integer;
begin
  inherited;
  if not Assigned(FHttpRequest) then
    FHttpRequest  :=  THttpRequest.Create;
  FHttpRequest.URL  :=  FURL;
  FHttpRequest.Address  :=  FAddress;
  FHttpRequest.Redirect :=  FRedirect;
  FHttpRequest.NoCache  :=  FNoCache;
  if FCount < 1 then
    c := 1
  else
    c :=  FCount;
  FResultText :=  '';
  for i := 0 to c - 1 do
  begin
    FHttpRequest.OpenRequest;
    FHttpRequest.SendRequest;

    FStatus       :=  FHttpRequest.Status;
    FConnectTime  :=  FHttpRequest.ConnectTime;
    FGetTime      :=  FHttpRequest.GetTime;
    if Assigned(ResultStream) then
      case Status of
        0   : FMD5  :=  'Error: 调用失败.';
        503 : FMD5  :=  'Error: 服务不可用.';
        403 : FMD5  :=  'Error: 禁止访问.';
        else begin
          FMD5  :=  MD5DigestToStr(MD5Stream(ResultStream));
          FSize :=  ResultStream.Size;
        end;
      end;

    if Length(FMD5) < 32 then
      FMD5  :=  FMD5 + StringOfChar(' ', 32 - Length(FMD5));
    FResultText := FResultText +
        GetFormatStr(Address, 15, False) + ' ' +
        GetFormatStr(IntToStr(Status), 5) + ' ' +
        GetFormatStr(IntToStr(Size), 7) + ' ' +
        GetFormatStr(MD5, 16, False) + ' ' +
        GetFormatStr(IntToStr(ConnectTime), 7) + ' ' +
        GetFormatStr(IntToStr(GetTime), 7) + #13#10;
  end;
end;

function THttpRequestThread.GetResultStream: TMemoryStream;
begin
  Result  :=  nil;
  if Assigned(FHttpRequest.ResultStream) then
    Result  :=  FHttpRequest.ResultStream;
end;

function THttpRequestThread.GetResultStreamAsString: string;
begin
  Result  :=  '';
  if Assigned(ResultStream) then
  begin
    SetLength(Result, ResultStream.Size);
    CopyMemory(@Result[1], ResultStream.Memory, ResultStream.Size);
  end;
end;

end.
