unit uDNSResolver;

interface

uses
  Classes, SysUtils, Windows, StrUtils;

type
  TDNSResolverThread = class(TThread)
  private
    FThreadList : TList;
    FIPList     : TStrings;
    FFreeIPList : Boolean;
    FDomain     : string;
    FDNS        : string;
    FDNSList    : TStrings;
    FResultIPList : string;
    FStayComplete : Boolean;
    FResolverList : Boolean;
    FTag: Integer;
  protected
    procedure Execute; override;
    procedure ClearThreadList;
  public
    property Tag : Integer read FTag Write FTag;
    property IPList : TStrings read FIPList;
  public
    constructor Create(
        const ADNS    : string;
        const ADomain : string;
        const AIPList : TStrings = nil;
        const AStayComplete : Boolean = False
        ); overload;

    constructor Create(
        const ADNSList  : TStrings;
        const ADomain   : string;
        const AIPList   : TStrings = nil
        ); overload;

    destructor Destroy; override;

    procedure DoComplete;
  end;

var
  DNSQuery : function (DNSServer : PChar; DomainName : PChar; IPList : PChar; Len : Integer):Integer;stdcall;

procedure SortIPList(const AList : TStrings);

implementation

const
  DNSDLL          = 'dnsdll.dll';
  DNSQueryFunc    = 'DNSQuery';
  WAITFOR_TIMEOUT = 30000;

var
  DNSDllHandle : THandle;

//==============================================================================

function GetInstanceFileName: string;
var
  pPath : array[0..MAXBYTE] of char;
begin
  GetModuleFileName(HInstance, pPath, MAXBYTE);
  Result  :=  pPath;
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

//==============================================================================
{ TDNSResolverThread }

constructor TDNSResolverThread.Create(
    const ADNS    : string;
    const ADomain : string;
    const AIPList : TStrings = nil;
    const AStayComplete : Boolean = False
    );
begin
  FResolverList :=  False;
  FDNS          :=  ADNS;
  FDomain       :=  ADomain;
  FIPList       :=  AIPList;
  FStayComplete :=  AStayComplete;
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

procedure TDNSResolverThread.DoComplete;
var
  lst : TStrings;
  i : Integer;
begin
  lst :=  TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(FResultIPList), lst);
    for i := 0 to lst.Count - 1 do
      if FIPList.IndexOf(lst[i]) = -1 then
        FIPList.Add(lst[i]);
  finally
    lst.Free;
  end;
end;

procedure TDNSResolverThread.Execute;

  function DoThreadQuery(const DNS: string): THandle;
  var
    thread  : TThread;
  begin
    if not Assigned(FThreadList) then
      FThreadList :=  TList.Create;
    thread  :=  TDNSResolverThread.Create(DNS, FDomain, Self.FIPList, True);
    Result  :=  thread.Handle;
    FThreadList.Add(thread);
  end;

var
  ip  : array[0..MAXBYTE - 1] of char;
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
      for i := 0 to FThreadList.Count - 1 do
        TDNSResolverThread(FThreadList[i]).DoComplete;
      ClearThreadList;
      SortIPList(FIPList);
    finally
      FreeMem(hl, size);
    end;
  end
  else begin
    FResultIPList :=  '';
    ZeroMemory(@ip[0], MAXBYTE);
    if DNSQuery(PChar(FDNS), PChar(FDomain), ip, MAXBYTE) > 0 then
      FResultIPList :=  ip;
    if not FStayComplete then
    begin
      DoComplete;
      SortIPList(FIPList);
    end;
  end;
end;

{ initialization }

procedure InitDnsDll;
var
  fname : string;
begin
  fname :=  ExtractFilePath(GetInstanceFileName) + DNSDLL;
  if FileExists(fname) then
  begin
    DNSDllHandle :=  LoadLibrary(PChar(fname));
    if DNSDllHandle > 0 then
      @DNSQuery := GetProcAddress(DNSDllHandle, PChar(DNSQueryFunc));
  end;
end;

initialization
  InitDnsDll;

end.
