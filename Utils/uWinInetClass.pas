unit uWinInetClass;

interface

uses
  WinInet, Windows, SysUtils, Classes, StrUtils;

const
  MAX_HeadersLength = $1000;

type
  THTTPRequestAction  = (haGet, haPost, haPut);

  THttpRequest  = class
  private
    FRedirect: Boolean;
    FStatus: Integer;
    FURL: string;
    FAddress: string;
    FResultStream: TMemoryStream;
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
    FReferer: string;
    FSecure : Boolean;
    FRedirectURL: string;
  private
    procedure ParseURL;
    function GetResultContent: string;
  public
    property Session : HInternet read FSession;
    property Connect : HInternet read FConnect;
    property Request : HInternet read FRequest;

    property URL  : string read FURL Write FURL;                //请求的URL
    property Address : string read FAddress Write FAddress;     //地址,可以设置为IP
    property Action : THTTPRequestAction read FAction Write FAction;  //请求类型
    property Redirect : Boolean read FRedirect Write FRedirect; //是否自动跳转
    property NoCache : Boolean read FNoCache write FNoCache;    //是否不启用缓存
    property Content  : string read FContent Write FContent;    //发送的内容
    property Referer : string read FReferer write FReferer;     //Referer内容
    property RedirectURL : string read FRedirectURL;            //跳转的地址

    property Status : Integer read FStatus;
    property ResultStream : TMemoryStream read FResultStream;
    property ResultHeaders  : string read FResultHeaders;
    property Size : Integer read FSize;
    property ResultContent : string read GetResultContent;  //返回的内容
    property HostName : string read FHostName;
  public
    destructor Destroy; override;

    function  OpenSession : Boolean;      //打开会话
    function  OpenConnect : Boolean;      //打开连接
    function  OpenRequest : Boolean;      //打开请求
    procedure AddHeader(const AStr  : string); overload;    //增加请求头
    procedure AddHeader(const AList : TStrings); overload;
    function  SendRequest(const ACloseHandle : Boolean = True):Boolean;        //发送请求,发送后将自动关闭请求
    procedure CloseRequest;
    procedure CloseConnect;               //关闭连接
    procedure CloseSession;               //关闭会话
    function  HackCookie(const AURL : string) : Boolean;
  end;

  THttpRequestThread = class(TThread)
  private
    FRedirect: Boolean;
    FStatus: Integer;
    FURL: string;
    FAddress: string;
    FSize: Integer;
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
    property Size : Integer read FSize;
    property ResultStreamAsString : string read GetResultStreamAsString;
  public
    constructor Create(
        const AURL  : string;
        const AFreeOnTerminate : Boolean = False
        ); overload;
    constructor Create(
        const AAddress  : string;
        const AURL      : string;
        const ARedirect : Boolean;
        const ANoCache  : Boolean;
        const ACount    : Integer;
        const AFreeOnTerminate : Boolean = False
        ); overload;
    destructor Destroy; override;
  end;

function HttpGetContent(const AURL : string):string;
procedure HttpRequestAURL(const AURL: string);  //只提交一个URL，不处理返回结果，也不关注是否成功

implementation

const
  READ_LENGTH = $10000;
  SHTTPRequestAction  : array[haGet..haPut] of PChar = (
      'GET',
      'POST',
      'PUT'
      );

{ THttpRequest }

destructor THttpRequest.Destroy;
begin
  CloseSession;
  if Assigned(FResultStream) then
    FResultStream.Free;
  inherited;
end;

procedure THttpRequest.ParseURL;
var
  i: Integer;
  url : string;
begin
  FSecure :=  False;
  url :=  FURL;
  if Pos('http://', LowerCase(URL)) = 1 then
    Delete(url, 1, 7)
  else if Pos('https://', LowerCase(URL)) = 1 then
  begin
    Delete(url, 1, 8);
    FSecure :=  True;
  end;

  i := Pos('/', url);
  if i > 0 then
  begin
    FHostName := Copy(url, 1, i);
    FFileName := Copy(url, i, Length(url) - i + 1);
  end
  else begin
    FHostName :=  url;
    FFileName :=  '';
  end;

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

function THttpRequest.OpenSession: Boolean;
begin
  Result  :=  Assigned(Session);
  if not Assigned(FSession) then
  begin
    FSession := InternetOpen(
        nil,
        INTERNET_OPEN_TYPE_PRECONFIG,
        nil,
        nil,
        0
        );
    Result  :=  Assigned(Session);
  end;
end;

function THttpRequest.OpenConnect: Boolean;
var
  Address   : string;
  Port  : Integer;
begin
  Result  :=  Assigned(FConnect);
  if Result then Exit;

  if not OpenSession then
    Exit;

  ParseURL;
  Address := FAddress;
  if Address = '' then
    Address  :=  FHostName;

  if Pos(':', Address) > 0 then
  begin
    if FSecure then
      Port  :=  StrToIntDef(MidStr(Address, Pos(':', Address) + 1, 5), INTERNET_DEFAULT_HTTPS_PORT)
    else
      Port  :=  StrToIntDef(MidStr(Address, Pos(':', Address) + 1, 5), INTERNET_DEFAULT_HTTP_PORT);
    Address :=  LeftStr(Address, Pos(':', Address) - 1);
  end
  else if FSecure then
    Port  :=  INTERNET_DEFAULT_HTTPS_PORT
  else
    Port  :=  INTERNET_DEFAULT_HTTP_PORT;

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
  Result  :=  Assigned(FConnect);
end;

function THttpRequest.OpenRequest: Boolean;
var
  InternetFlag: DWord;
  ref : string;
  AcceptTypes : string;
begin
  Result  :=  Assigned(FRequest);
  if not OpenConnect then
    Exit;

  InternetFlag := 0;
  if FRedirect then //INTERNET_FLAG_RELOAD;
    InternetFlag  :=  InternetFlag or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;
  if FNoCache then
    InternetFlag  :=  InternetFlag or INTERNET_FLAG_RELOAD;
  if not FRedirect then
    InternetFlag  :=  InternetFlag or INTERNET_FLAG_NO_AUTO_REDIRECT;
  if FSecure then
    InternetFlag  :=  InternetFlag or INTERNET_FLAG_SECURE;

  ref :=  FReferer;
  if ref = '' then
    ref :=  FURL;

  AcceptTypes :=  '';
  FRequest := HttpOpenRequest(
      FConnect,
      SHTTPRequestAction[FAction],
      PChar(FFileName),
      nil,
      PChar(ref),
      @AcceptTypes,
      InternetFlag,
      0
      );

  Result  :=  Assigned(FRequest);
  if Result then
    AddHeader('host: ' + FHostName);
end;

function THttpRequest.SendRequest(const ACloseHandle : Boolean) : Boolean;
var
  Buf: Pointer;
  dwBufLen, dwIndex: DWord;
  Data: Array[0..READ_LENGTH - 1] of Char;
  BytesReaded: Integer;
  BytesToRead: Cardinal;
  //AcceptTypes : array[0..1] of PChar;
  //Header  : string;
  sdata : RawByteString;
  p : Integer;
  s : string;
  l_b: Boolean;
begin
  if not Assigned(FRequest) then
    OpenRequest;
  if not Assigned(FRequest) then
  begin
    Result  :=  False;
    Exit;
  end;

  sdata :=  UTF8Encode(FContent);
  try
    l_b := HttpSendRequest(
        FRequest,
        nil,//PChar(Header),
        0,//Length(Header),
        PChar(Pointer(@sdata[1])),
        Length(sdata)
        );
  except
    Result := False;
    Exit;
  end;

  if l_b then
  begin
    FRedirectURL  :=  '';
    dwIndex := 0;
    dwBufLen := MAX_HeadersLength;
    GetMem(Buf, MAX_HeadersLength);
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

      case FStatus of
        302 :
          begin
            dwBufLen  :=  MAX_HeadersLength;
            if HttpQueryInfo(
                FRequest,
                HTTP_QUERY_CONTENT_LOCATION,
                Buf,
                dwBufLen,
                dwIndex
                ) then
              FRedirectURL  :=  StrPas(PChar(Buf))
            else begin
              s :=  FResultHeaders;
              p  :=  Pos(#13#10'Location:', s);
              if p > 0 then
              begin
                Delete(s, 1, p + 10);
                p :=  Pos(#13#10, s);
                if p > 0 then
                begin
                  s :=  LeftStr(s, p - 1);
                  FRedirectURL :=  Trim(s);
                end;
              end;
            end;
          end;
        200 :
          begin
            dwBufLen := MAX_HeadersLength;
            if HttpQueryInfo(
                FRequest,
                HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER,
                Buf,
                dwBufLen,
                dwIndex) then
            begin
              FSize :=  PDWORD(Buf)^;
            end;
          end;
      end;
    finally
      FreeMem(Buf, MAX_HeadersLength);
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
  end
  else
    Result :=  False;

  if ACloseHandle then CloseRequest;
end;

procedure THttpRequest.CloseRequest;
begin
  if Assigned(FRequest) then
  begin
    InternetCloseHandle(FRequest);
    FRequest  :=  nil;
  end;
end;

procedure THttpRequest.CloseConnect;
begin
  CloseRequest;
  if Assigned(FConnect) then
  begin
    InternetCloseHandle(FConnect);
    FConnect  :=  nil;
  end;
end;

procedure THttpRequest.CloseSession;
begin
  CloseConnect;
  if Assigned(FSession) then
  begin
    InternetCloseHandle(FSession);
    FSession  :=  nil;
  end;
end;

function THttpRequest.GetResultContent: string;
var
  s : RawByteString;
begin
  Result  :=  '';
  if Assigned(ResultStream) then
  begin
    SetLength(s, ResultStream.Size);
    CopyMemory(@s[1], ResultStream.Memory, ResultStream.Size);
    Result  :=  UTF8ToString(s);
  end;
end;

function THttpRequest.HackCookie(const AURL: string) : Boolean;
const
  COOKIE_SIZE = 4096;
var
  szCookie : array[0..COOKIE_SIZE - 1] of Char;
  dwSize : DWORD;
  dwIndex : DWORD;
  bPersistCookie : Boolean;
  sCookie : string;
  sNewCookie : string;
  lst : TStrings;
  i : Integer;
begin
  Result  :=  False;
  if not Assigned(FRequest) then Exit;
  dwSize  :=  COOKIE_SIZE * 2;
  lst := TStringList.Create;
  try
    ZeroMemory(@szCookie, SizeOf(szCookie));
    dwIndex :=  0;
    while HttpQueryInfo(FRequest, HTTP_QUERY_SET_COOKIE, @szCookie, dwSize, dwIndex) do
    begin
      dwSize  :=  COOKIE_SIZE * 2;
      bPersistCookie  :=  False;
      sCookie :=  WideCharToString(@szCookie);
      sNewCookie  :=  '';
      lst.Clear;
      ExtractStrings([';'], [' '], PChar(sCookie), lst);
      for i := 0 to lst.Count - 1 do
      begin
        if SameText(lst.Names[i], 'httponly') then Continue;
        if SameText(lst.Names[i], 'secure') then Continue;
        if SameText(lst.Names[i], 'expires') then
        begin
          bPersistCookie := True;
          Break;
        end;
        sNewCookie  :=  sNewCookie + lst[i] + '; ';
      end;
      if not bPersistCookie then
        InternetSetCookie(PChar(AURL), nil, PChar(sNewCookie + 'expires=Wed, 22-Oct-2031 23:59:59 GMT;'));
    end;
  finally
    lst.Free;
  end;
end;

{ THttpRequestThread }

constructor THttpRequestThread.Create(const AURL: string; const AFreeOnTerminate : Boolean);
begin
  FURL  :=  AURL;
  FreeOnTerminate :=  AFreeOnTerminate;
  inherited Create(False);
end;

constructor THttpRequestThread.Create(const AAddress, AURL: string;
  const ARedirect, ANoCache : Boolean; const ACount : Integer;
  const AFreeOnTerminate : Boolean);
begin
  FAddress  :=  AAddress;
  FURL  :=  AURL;
  FRedirect := ARedirect;
  FNoCache := ANoCache;
  FreeOnTerminate :=  AFreeOnTerminate;
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

begin
  inherited;
  if not Assigned(FHttpRequest) then
    FHttpRequest  :=  THttpRequest.Create;
  FHttpRequest.URL  :=  FURL;
  FHttpRequest.Address  :=  FAddress;
  FHttpRequest.Redirect :=  FRedirect;
  FHttpRequest.NoCache  :=  FNoCache;

  FHttpRequest.OpenRequest;
  FHttpRequest.SendRequest;

  FStatus       :=  FHttpRequest.Status;
  if Assigned(ResultStream) then
    FSize :=  ResultStream.Size;
end;

function THttpRequestThread.GetResultStream: TMemoryStream;
begin
  Result  :=  nil;
  if Assigned(FHttpRequest.ResultStream) then
    Result  :=  FHttpRequest.ResultStream;
end;

function THttpRequestThread.GetResultStreamAsString: string;
var
  ss : TStringStream;
begin
  Result  :=  '';
  if Assigned(ResultStream) then
  begin
    ss  :=  TStringStream.Create;
    try
      ResultStream.Position :=  0;
      ss.LoadFromStream(ResultStream);
      Result  :=  ss.DataString;
    finally
      ss.Free;
    end;
  end;
end;

function HttpGetContent(const AURL : string):string;
begin
  Result  :=  '';
  with THttpRequest.Create do
  try
    URL :=  AURL;
    Redirect  :=  True;
    SendRequest;
    Result  :=  ResultContent;
  finally
    Free;
  end;
end;

procedure HttpRequestAURL(const AURL: string);  //只提交一个URL，不处理返回结果，也不关注是否成功
begin
  THttpRequestThread.Create(AURL, True);
end;

end.
