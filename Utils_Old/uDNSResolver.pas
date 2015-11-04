unit uDNSResolver;

interface

uses
  Classes, SysUtils, Windows, StrUtils, WinDNS;

type
  TDNSResolverThread = class(TThread)
  private
    FThreadList : TList;
    FIPList     : TStrings;
    FFreeIPList : Boolean;
    FDomain     : string;
    FDNS        : string;
    FDNSList    : TStrings;
    FResolverList : Boolean;
    FTag: Integer;
  protected
    procedure Execute; override;
    procedure ClearThreadList;
  public
    property Tag : Integer read FTag Write FTag;
    property IPList : TStrings read FIPList;
    property DNS  : string read FDNS;
    property Domain : string read FDomain;
  public
    constructor Create(
        const ADNS    : string;
        const ADomain : string;
        const AIPList : TStrings = nil
        ); overload;

    constructor Create(
        const ADNSList  : TStrings;
        const ADomain   : string;
        const AIPList   : TStrings = nil
        ); overload;

    destructor Destroy; override;
  end;

  TDNSResolver = class
  private
    FDomain: string;
    FIPList: TStrings;
    FDnsList: TStrings;
    FIPMemory : Pointer;
    FIPMemorySize : Integer;
    FThreadCount: Integer;
    FTimeOut: Integer;
  private
    procedure SetThreadCount(const Value: Integer);
    procedure SetTimeOut(const Value: Integer);

    procedure InitIPList;
    procedure QueryDNS;
    procedure MakeResult;
    procedure FreeIPMemory;
  public
    property Domain : string read FDomain Write FDomain;
    property DnsList : TStrings read FDnsList;
    property IPList : TStrings read FIPList;
    property ThreadCount : Integer read FThreadCount Write SetThreadCount;
    property TimeOut : Integer read FTimeOut Write SetTimeOut;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResolverDNS;
  end;

function StrIsIp(AStr: string): Boolean;
procedure SortIPList(const AList : TStrings);

implementation

const
  DEF_ThreadCount = 5;
  MAX_ThreadCount = 30;
  DEF_TimeOut     = 2;
  MAX_TimeOut     = 10;

  WAITFOR_TIMEOUT = 30000;
  IP_Length       = 15;
  IPList_Count    = 20;

type
  TIPList = array[0..IP_Length * IPList_Count - 1] of char;
  PIPList = ^TIPList;
  PThreadParam = ^TThreadParam;
  TThreadParam = record
    Domain  : string;
    Param : Pointer;
  end;

//==============================================================================

function StrIsIp(AStr: string): Boolean;
var
  ips: TStringList;
begin
  ips := TStringList.Create;
  try
    Result  :=  (ExtractStrings(['.'], [], PChar(AStr), ips) = 4) and
      (StrToInt(ips[0]) < 256) and (StrToInt(ips[0]) >= 0) and
      (StrToInt(ips[1]) < 256) and (StrToInt(ips[1]) >= 0) and
      (StrToInt(ips[2]) < 256) and (StrToInt(ips[2]) >= 0) and
      (StrToInt(ips[3]) < 256) and (StrToInt(ips[3]) >= 0);
  except
    Result  := False;
  end;
  ips.Free;
end;

procedure SortIPList(const AList : TStrings);
type
  PStrRec = ^TStrRec;
  TStrRec = record
    v : string;
  end;

  function ConverIP(const IP : string) : string;
  const
    Zero  = '000';
  var
    p : Integer;
  begin
    Result  :=  IP + '.';
    p :=  Pos('.', Result);
    if p < 4 then
      Insert(LeftBStr(Zero, 4 - p), Result, 1);
    p :=  PosEx('.', Result, 5);
    if p < 8 then
      Insert(LeftBStr(Zero, 8 - p), Result, 5);
    p :=  PosEx('.', Result, 9);
    if p < 12 then
      Insert(LeftBStr(Zero, 12 - p), Result, 9);
    p :=  PosEx('.', Result, 13);
    if p < 16 then
      Insert(LeftBStr(Zero, 16 - p), Result, 13);
    if Length(Result) = 16 then
      Delete(Result, 16, 1)
    else
      Result  :=  IP;
  end;

  function CompIP(F1, F2: Pointer): Integer;
  var
    s1, s2  : string;
  begin
    s1  :=  ConverIP(PStrRec(F1).v);
    s2  :=  ConverIP(PStrRec(F2).v);
    if s1 > s2 then
      Result := -1
    else if s1 < s2 then
      Result := 1
    else
      Result  :=  0;
  end;

var
  lst : TList;
  i : Integer;
  ps  : PStrRec;
begin
  lst :=  TList.Create;
  try
    for i := 0 to AList.Count - 1 do
    begin
      new(ps);
      ps.v  :=  AList[i];
      lst.Add(ps);
    end;
    lst.Sort(@CompIP);
    AList.Clear;
    for i := lst.Count - 1 downto 0 do
    begin
      AList.Add(PStrRec(lst[i]).v);
      dispose(PStrRec(lst[i]));
    end;
  finally
    lst.Free;
  end;
end;

procedure WriteIP(const IPList :PIPList; const Index : Integer; const AIP : string);
var
  p : PChar;
begin
  if (Index < IPList_Count) and (Length(AIP) <= IP_Length) then
  begin
    p :=  PChar(IPList);
    Inc(p, IP_Length * Index);
    ZeroMemory(p, IP_Length);
    CopyMemory(p, @AIP[1], Length(AIP));
  end;
end;

function ReadIP(const IPList :PIPList; const Index : Integer) : string;
var
  p : PChar;
  c : array[0..IP_Length] of char;
begin
  if Index < IPList_Count then
  begin
    p :=  PChar(IPList);
    Inc(p, IP_Length * Index);
    ZeroMemory(@c[0], IP_Length + 1);
    CopyMemory(@c[0], p, IP_Length);
    Result  :=  StrPas(c);
  end
  else
    Result  :=  '';
end;

function QueryDNSThreadProc(Parameter: Pointer): Integer;
var
  iplist  : PIPList;
  lst : TStrings;
  dns : string;
  i : Integer;
begin
  iplist  :=  PThreadParam(Parameter).Param;
  dns :=  ReadIP(iplist, 0);
  lst :=  TStringList.Create;
  try
    ResolveHost(dns, PThreadParam(Parameter).Domain, lst);
    for i := 0 to lst.Count - 1 do
      if i < IPList_Count - 1 then
        WriteIP(iplist, i + 1, lst[i]);
  finally
    lst.Free;
  end;
  Result  :=  0;
  Dispose(PThreadParam(Parameter));
  EndThread(Result);
end;

//==============================================================================
{ TDNSResolverThread }

constructor TDNSResolverThread.Create(
    const ADNS    : string;
    const ADomain : string;
    const AIPList : TStrings = nil
    );
begin
  FResolverList :=  False;
  FDNS          :=  ADNS;
  FDomain       :=  ADomain;
  FIPList       :=  AIPList;
  if not Assigned(FIPList) then
  begin
    FIPList :=  TStringList.Create;
    FFreeIPList :=  True;
  end;
  //FreeOnTerminate :=  True;
  inherited Create(False);
end;

constructor TDNSResolverThread.Create(
    const ADNSList: TStrings;
    const ADomain: string;
    const AIPList: TStrings
    );
begin
  FResolverList :=  True;
  FDNSList  :=  TStringList.Create;
  FDNSList.AddStrings(ADNSList);
  FDomain   :=  ADomain;
  FIPList   :=  AIPList;
  if not Assigned(FIPList) then
  begin
    FIPList :=  TStringList.Create;
    FFreeIPList :=  True;
  end;
  inherited Create(False);
end;

destructor TDNSResolverThread.Destroy;
begin
  ClearThreadList;
  if FFreeIPList then
    FIPList.Free;
  if Assigned(FDNSList) then
    FDNSList.Free;
  if Assigned(FThreadList) then
    FThreadList.Free;
  inherited;
end;

procedure TDNSResolverThread.ClearThreadList;
var
  i : Integer;
begin
  if Assigned(FThreadList) then
  begin
    for i := 0 to FThreadList.Count - 1 do
      TThread(FThreadList[i]).Free;
    FThreadList.Clear;
  end;
end;

procedure TDNSResolverThread.Execute;

  function DoThreadQuery(const DNS: string): THandle;
  var
    thread  : TThread;
  begin
    if not Assigned(FThreadList) then
      FThreadList :=  TList.Create;
    thread  :=  TDNSResolverThread.Create(DNS, FDomain, Self.FIPList);
    Result  :=  thread.Handle;
    FThreadList.Add(thread);
  end;

var
  i : Integer;
  h : PHandle;
  hl  : Pointer;
  size  : Integer;
begin
  inherited;
  if FResolverList then
  begin
    ClearThreadList;
    size  :=  FDNSList.Count * SizeOf(THandle);
    GetMem(hl, size);
    ZeroMemory(hl, size);
    try
      h :=  hl;
      for i := 0 to FDNSList.Count - 1 do
      begin
        h^  :=  DoThreadQuery(FDNSList[i]);
        Inc(h);
      end;
      WaitForMultipleObjects(FDNSList.Count, PWOHandleArray(hl), True, WAITFOR_TIMEOUT);
      ClearThreadList;
      SortIPList(FIPList);
    finally
      FreeMem(hl, size);
    end;
  end
  else begin
    ResolveHost(FDNS, FDomain, FIPList);
    SortIPList(FIPList);
  end;
end;

procedure ResolverDNS(
    const ADomain : string;
    const ADnsList : TStrings;
    const AIPList : TStrings
    );
begin
end;

{ TDNSResolver }

constructor TDNSResolver.Create;
begin
  inherited;
  FIPList :=  TStringList.Create;
  FDNSList  :=  TStringList.Create;
  ThreadCount :=  DEF_ThreadCount;
  TimeOut :=  DEF_TimeOut;
end;

destructor TDNSResolver.Destroy;
begin
  FIPList.Free;
  FDNSList.Free;
  FreeIPMemory;
  inherited;
end;

procedure TDNSResolver.FreeIPMemory;
begin
  if FIPMemorySize > 0 then
    FreeMem(FIPMemory, FIPMemorySize);
end;

procedure TDNSResolver.InitIPList;
var
  i : Integer;
  p : PIPList;
begin
  FreeIPMemory;
  FIPMemorySize := FDNSList.Count * SizeOf(TIPList);
  GetMem(FIPMemory, FIPMemorySize);
  ZeroMemory(FIPMemory, FIPMemorySize);
  p :=  FIPMemory;
  for i := 0 to FDNSList.Count - 1 do
  begin
    WriteIP(p, 0, FDNSList[i]);
    Inc(p);
  end;
end;

procedure TDNSResolver.QueryDNS;
var
  i : Integer;
  h : PHandle;
  hl  : Pointer;
  size  : Integer;
  c : Integer;
  x : Integer;
  tid : DWORD;
  ip  : PIPList;
  pt  : PThreadParam;
begin
  size  :=  ThreadCount * SizeOf(THandle);
  GetMem(hl, size);
  try
    c :=  0;
    ip  :=  FIPMemory;
    while c < FDNSList.Count - 1 do
    begin
      ZeroMemory(hl, size);
      h :=  hl;
      x :=  0;
      for i := 0 to ThreadCount - 1 do
      begin
        new(pt);
        pt.Domain :=  Domain;
        pt.Param  :=  ip;
        h^  :=  BeginThread(nil, 0, @QueryDNSThreadProc, pt, 0, tid);
        Inc(ip);
        Inc(h);
        Inc(c);
        Inc(x);
        if c >= FDNSList.Count then
          break;
      end;
      if WAIT_FAILED = WaitForMultipleObjects(x, PWOHandleArray(hl), True, TimeOut * 1000) then
        Exit;
    end;
  finally
    FreeMem(hl, size);
  end;
end;

procedure TDNSResolver.MakeResult;
var
  i : Integer;
  j : Integer;
  p : PIPList;
  s : string;
  lst : TStrings;
begin
  p :=  FIPMemory;
  lst :=  TStringList.Create;
  try
    for i := 0 to FDNSList.Count - 1 do
    begin
      for j := 1 to IPList_Count - 1 do
      begin
        s :=  ReadIP(p, j);
        if s <> '' then
          if lst.IndexOf(s) < 0 then
            lst.Add(s);
      end;
      Inc(p);
    end;
    SortIPList(lst);
    IPList.AddStrings(lst);
  finally
    lst.Free;
  end;
end;

procedure TDNSResolver.ResolverDNS;
begin
  if FDomain = '' then Exit;
  if FDNSList.Count = 0 then Exit;
  FIPList.Clear;
  InitIPList;
  QueryDNS;
  MakeResult;
end;

procedure TDNSResolver.SetThreadCount(const Value: Integer);
begin
  if (Value > 0) and (Value <= MAX_ThreadCount) then
    FThreadCount  :=  Value;
end;

procedure TDNSResolver.SetTimeOut(const Value: Integer);
begin
  if (Value > 0) and (Value <= MAX_TimeOut) then
    FTimeOut  :=  Value;
end;

initialization

end.

