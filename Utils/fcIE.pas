unit fcIE;

interface

uses
  WinInet, Windows, ActiveX, Classes, SysUtils, Variants;

type
  TInternetSetCookieEx = function(lpszURL: LPCTSTR;
                                  lpszCookieName:LPCTSTR;
                                  lpszCookieData:LPCTSTR;
                                  dwFlags: DWORD;
                                  dwReserved: DWORD_PTR): BOOL; stdcall;

  IE = record
    class function GetLocalFileNameFromIECache(url: string; var LocalFileName: string): DWORD; static;

    /// <summary>
    /// 从IHtmlDocument2接口读取源码
    /// </summary>
    /// <param name="Document">IHtmlDocument2文档接口</param>
    /// <param name="ResultStr">返回的源码</param>
    /// <returns>是否成功</returns>
    class function GetHtmlDocument(Document: IDispatch; var ResultStr: String): Boolean; static;

    /// <summary>
    /// 向IHtmlDocument2接口设置源码
    /// </summary>
    /// <param name="Document">IHtmlDocument2文档接口</param>
    /// <param name="SourceStr">待设置的源码</param>
    /// <returns>是否成功</returns>
    class function SetHtmlDocument(Document: IDispatch; const SourceStr: String): Boolean; static;



    /// <summary>
    /// 从Stream中读取源码到IHtmlDocument2
    /// </summary>
    /// <param name="Document"></param>
    /// <param name="Stream"></param>
    class procedure LoadDocumentSourceFromStream(Document: IDispatch; Stream: TStream); static;

    /// <summary>
    /// 保存IHtmlDocument2源码到Stream中
    /// </summary>
    /// <param name="Document"></param>
    /// <param name="Stream"></param>
    class procedure SaveDocumentSourceToStream(Document: IDispatch; Stream: TStream); static;


    //获得IE代理信息
    //返回：是否有使用IE代理
    class function GetIEProxyInfo(var AHost: string; var APort: Integer): Boolean; static;


//    class function SetIECookie(const AURL, AName, AData: string): Boolean; static;
  end;

implementation
uses fcRegistry, fcStr;

{ IE }

class function IE.GetIEProxyInfo(var AHost: string;
  var APort: Integer): Boolean;
var
  l_Str, l_Host, l_Port: string;
begin
  Result := fcRegistry.Reg.SH_GetKeyIntValue(HKEY_CURRENT_USER,
    'Software\Microsoft\Windows\CurrentVersion\Internet Settings',
    'ProxyEnable') = 1;
  if Result then
  begin
    l_Str := fcRegistry.Reg.SH_GetKeyValue(HKEY_CURRENT_USER,
      'Software\Microsoft\Windows\CurrentVersion\Internet Settings',
      'ProxyServer');
    l_Host := fcStr.Str.GetTokenBeforeChar(l_Str, ':');
    l_Port := fcStr.Str.GetTokenAfterChar(l_Str, ':');

    if (l_Host <> '') and (StrToIntDef(l_Port, 0) > 0) then
    begin
      AHost := l_Host;
      APort := StrToIntDef(l_Port, 0);
    end
    else
      Result := False;
  end;
end;

class function IE.GetLocalFileNameFromIECache(url: string; var LocalFileName: string): DWORD;
var
  d: Cardinal;
  T: PInternetCacheEntryInfo;
begin
  Result := S_OK;
  d := 0;
  T := nil;
  try
    GetUrlCacheEntryInfo(PChar(url), T^, d);
    Getmem(T, d);
    try
      if (GetUrlCacheEntryInfo(PChar(url), T^, d)) then
      begin
        LocalFileName := T^.lpszLocalFileName;
      end
      else
        Result := GetLastError;
    finally
      Freemem(T, d);
    end;
  except
    Result := S_FALSE;
  end;
end;

class procedure IE.LoadDocumentSourceFromStream(Document: IDispatch; Stream: TStream);
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  Stream.Position := 0;

  if Document.QueryInterface(IPersistStreamInit, PersistStreamInit) = S_OK then
  begin
    StreamAdapter := TStreamAdapter.Create(Stream, soReference);
    PersistStreamInit.Load(StreamAdapter);
    StreamAdapter := nil;
  end;
end;

class procedure IE.SaveDocumentSourceToStream(Document: IDispatch; Stream: TStream);
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  Stream.Size := 0;
  Stream.Position := 0;

  if Document.QueryInterface(IPersistStreamInit, PersistStreamInit) = S_OK then
  begin
    StreamAdapter := TStreamAdapter.Create(Stream, soReference);
    PersistStreamInit.Save(StreamAdapter, False);
    StreamAdapter := nil;
  end;
end;

class function IE.GetHtmlDocument(Document: IDispatch; var ResultStr: String): Boolean;
var
  StrStream: TStringStream;
begin
  Result := False;

  if Document <> nil then
  begin
    StrStream := TStringStream.Create('');
    SaveDocumentSourceToStream(Document, StrStream);
    ResultStr := StrStream.DataString;
    StrStream.Free;
    Result := True;
  end;
end;

class function IE.SetHtmlDocument(Document: IDispatch; const SourceStr: String): Boolean;
var
  StrStream: TStringStream;
begin
  Result := False;

  if Document <> nil then
  begin
    StrStream := TStringStream.Create(SourceStr);
    LoadDocumentSourceFromStream(Document, StrStream);
    StrStream.Free;
    Result := True;
  end;
end;

//class function IE.SetIECookie(const AURL, AName, AData: string): Boolean;
//var
//  l_Handle: THandle;
//  l_Func: TInternetSetCookieEx;
//  l_Reserved: LongInt;
//const
//  INTERNET_COOKIE_HTTPONLY = $00002000;
//begin
//  Result := false;
//  l_Handle := LoadLibrary('Wininet.DLL');
//  if l_Handle>0 then
//  begin
//    try
//      @l_Func := GetProcAddress(l_Handle, 'InternetSetCookieExW');
//      if Assigned(@l_Func) then
//      begin
//        //InternetSetCookieEx(m_url, NULL, " phpMyAdmin=; expires=Thu, 01-Jan-1900 00:00:01 GMT", INTERNET_COOKIE_HTTPONLY, NULL);
//        Result := l_Func(PWideChar(AURL), PWideChar(AName), PWideChar(AData), {INTERNET_COOKIE_HTTPONLY}0, &l_Reserved);
//      end;
//    finally
//      FreeLibrary(l_Handle);
//    end;
//  end;
//end;

end.
