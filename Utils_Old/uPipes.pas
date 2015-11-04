unit uPipes;

//{$DEFINE SYNCHRONIZE_THREAD}
//{$IFNDEF SYNCHRONIZE_THREAD}
//{$DEFINE SYNCHRONIZE_CRITICALSECTION}
//{$ENDIF}

interface

uses
  Classes, Windows;

const
  SShutDownMsg      = 'shutdown pipe ';
  SPipeFormat       = '\\%s\pipe\%s';
  PIPE_MSG_MAXSIZE  = $10000; //65536字节
  PIPE_DEFTIMEOUT   = 3000;

type
  TPipeMessage = record
    ID: Integer;
    Msg: string
  end;
  TPipeServerOnMessageEvent = procedure(Sender : TObject; var Msg: TPipeMessage) of object;

  RPIPEMessage = record
    Size: DWORD;
    Kind: Integer;
    Count: DWORD;
    Data: array[0..PIPE_MSG_MAXSIZE - 1] of Char;
  end;

  TPipeServer = class(TThread)
  private
    FHandle: THandle;
    FPipeName: string;
    FOnMessage: TPipeServerOnMessageEvent;
    FMsg  : TPipeMessage;
    {$IFDEF SYNCHRONIZE_CRITICALSECTION}
    FCriticalSection  : TRTLCriticalSection;
    {$ENDIF}    
  protected
    procedure Execute; override;
    procedure DoMessageEvent;
  public
    property OnMessage: TPipeServerOnMessageEvent read FOnMessage Write FOnMessage;
  public
    constructor CreatePipeServer(aServer, aPipe: string; StartServer: Boolean);
    destructor Destroy; override;

    procedure StartUpServer;
    procedure ShutDownServer;
  end;

  TPipeClient = class
  private
    FPipeName: string;
    FTimeOut: Integer;
  protected
    function ProcessMsg(aMsg: RPIPEMessage): RPIPEMessage;
  public
    property TimeOut  : Integer read FTimeOut Write FTimeOut default PIPE_DEFTIMEOUT;
  public
    constructor Create(aServer, aPipe: string);
    function SendString(aStr: string): string;
    function SendMessage(Msg: TPipeMessage): TPipeMessage;
  end;

function PipeSendMessage(const PipeName : string; var Msg : TPipeMessage) : boolean; overload;
function PipeSendMessage(const PipeName : string; var ID  : integer; var Msg : string): boolean; overload;
function PipeSendMessage(const PipeName : string; var ID  : integer): boolean; overload;
function PipeSendMessage(const PipeName : string; var Msg : string): boolean; overload;

implementation

uses
  SysUtils;

function GetLastErrorText: string;
var
  dwSize: DWORD;
  lpszTemp: PChar;
begin
  dwSize := 512;
  lpszTemp := nil;
  try
    GetMem(lpszTemp, dwSize);
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
      nil, GetLastError, LANG_NEUTRAL, lpszTemp, dwSize, nil);
  finally
    Result := StrPas(lpszTemp);
    FreeMem(lpszTemp);
  end;
end;

procedure CalcMsgSize(var Msg: RPIPEMessage);
begin
  Msg.Size :=
    SizeOf(Msg.Size) +
    SizeOf(Msg.Kind) +
    SizeOf(Msg.Count) +
    Msg.Count;
end;

{ TPipeServer }

constructor TPipeServer.CreatePipeServer(
  aServer, aPipe: string; StartServer: Boolean
  );
begin
  if aServer = '' then
    FPipeName := Format(SPipeFormat, ['.', aPipe])
  else
    FPipeName := Format(SPipeFormat, [aServer, aPipe]);
  // clear server handle
  FHandle := INVALID_HANDLE_VALUE;
  if StartServer then
    StartUpServer;
  // create the class
  Create(not StartServer);
  {$IFDEF SYNCHRONIZE_CRITICALSECTION}
  InitializeCriticalSection(FCriticalSection);
  {$ENDIF}
end;

destructor TPipeServer.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    // must shut down the server first
    ShutDownServer;
  {$IFDEF SYNCHRONIZE_CRITICALSECTION}
  DeleteCriticalSection(FCriticalSection);
  {$ENDIF}
  inherited Destroy;
end;

procedure TPipeServer.DoMessageEvent;
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, FMsg);
end;

procedure TPipeServer.Execute;
var
  Written: Cardinal;
  InMsg, OutMsg: RPIPEMessage;
  Msg: TPipeMessage;
  s : string;
begin
  while not Terminated do
  begin
    if FHandle = INVALID_HANDLE_VALUE then
    begin
      // suspend thread for 250 milliseconds and try again
      Sleep(250);
    end
    else
    begin
      if ConnectNamedPipe(FHandle, nil) then
      try
        // read data from pipe
        InMsg.Size := SizeOf(InMsg);
        ReadFile(FHandle, InMsg, InMsg.Size, InMsg.Size, nil);
        if (InMsg.Kind = 0) and
          (string(InMsg.Data) = SShutDownMsg + FPipeName) then
        begin
          // process shut down
          OutMsg.Kind := 0;
          s :=  'OK'#0;
          StrPCopy(OutMsg.Data, s);
          OutMsg.Count := Length(s) * SizeOf(Char);
          Terminate;
        end
        else begin//}
          {
          // data send to pipe should be processed here
          OutMsg := InMsg;
          // we'll just reverse the data sent, byte-by-byte
          for I := 0 to Pred(InMsg.Count) do
            OutMsg.Data[Pred(InMsg.Count) - I] := InMsg.Data[I];
          //}
          Msg.ID := InMsg.Kind;
          Msg.Msg := Copy(InMsg.Data, 1, InMsg.Count div SizeOf(Char));
          FMsg  :=  Msg;
          {$IFDEF SYNCHRONIZE_THREAD}
          Synchronize(DoMessageEvent);
          {$ENDIF}
          {$IFDEF SYNCHRONIZE_CRITICALSECTION}
          EnterCriticalSection(FCriticalSection);
          try
          {$ENDIF}
            DoMessageEvent;
          {$IFDEF SYNCHRONIZE_CRITICALSECTION}
          finally
            LeaveCriticalSection(FCriticalSection);
          end;
          {$ENDIF}
          Msg   :=  FMsg;
          s :=  Msg.Msg;
          if Length(s) > PIPE_MSG_MAXSIZE then
            delete(s, PIPE_MSG_MAXSIZE, Length(s));

          OutMsg.Kind := Msg.ID;
          OutMsg.Count := Length(s) * SizeOf(Char);
          StrPCopy(OutMsg.Data, s);
          // send message
        end;
        CalcMsgSize(OutMsg);
        WriteFile(FHandle, OutMsg, OutMsg.Size, Written, nil);
      finally
        DisconnectNamedPipe(FHandle);
      end;
    end;
  end;
end;

procedure TPipeServer.ShutDownServer;
var
  BytesRead: Cardinal;
  OutMsg, InMsg: RPIPEMessage;
  ShutDownMsg: string; //}
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    Terminate;
    // server still has pipe opened
    OutMsg.Size := SizeOf(OutMsg);
    // prepare shut down message
    with InMsg do
    begin
      Kind := 0;
      ShutDownMsg := SShutDownMsg + FPipeName;
      Count := Succ(Length(ShutDownMsg) * SizeOf(Char));
      StrPCopy(Data, ShutDownMsg);
    end;
    CalcMsgSize(InMsg);
    // send shut down message
    CallNamedPipe(
      PChar(FPipeName), @InMsg, InMsg.Size, @OutMsg, OutMsg.Size, BytesRead, 100
      );//}
    // close pipe on server
    CloseHandle(FHandle);
    // clear handle
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TPipeServer.StartUpServer;
var
  sa: SECURITY_ATTRIBUTES;
  psd: PSECURITY_DESCRIPTOR;
begin
  //允许匿名访问的处理,让本机任何用户都能访问Pipe
  //如果调用CreateNamedPipe,PSecurityAttributes参数为nil,则只能当前用户能访问Pipe
  pSD := PSECURITY_DESCRIPTOR(LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH));
  if InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION) and
      SetSecurityDescriptorDacl(pSD, True, nil, False) then
  begin
    sa.nLength := sizeof(sa);
    sa.lpSecurityDescriptor := pSD;
    sa.bInheritHandle := True;
  end
  else begin
    LocalFree(HLOCAL(pSD));
    raise Exception.Create('Could not Initialize Security Descriptor.');
  end;

  // check whether pipe does exist
  if WaitNamedPipe(PChar(FPipeName), 100 {ms}) then
    raise Exception.Create('Requested PIPE exists already.');
  // create the pipe
  FHandle := CreateNamedPipe(
    PChar(FPipeName),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    PIPE_UNLIMITED_INSTANCES,
    SizeOf(RPIPEMessage),
    SizeOf(RPIPEMessage),
    NMPWAIT_USE_DEFAULT_WAIT,
    @sa
    );
  // check if pipe was created
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Could not create PIPE.');
end;

{ TPipeClient }

constructor TPipeClient.Create(aServer, aPipe: string);
begin
  inherited Create;
  FTimeOut  :=  PIPE_DEFTIMEOUT;
  if aServer = '' then
    FPipeName := Format(SPipeFormat, ['.', aPipe])
  else
    FPipeName := Format(SPipeFormat, [aServer, aPipe]);
end;

function TPipeClient.ProcessMsg(aMsg: RPIPEMessage): RPIPEMessage;
begin
  CalcMsgSize(aMsg);
  Result.Size := SizeOf(Result);
  if WaitNamedPipe(PChar(FPipeName), FTimeOut) then
  begin
    if not CallNamedPipe(
      PChar(FPipeName), @aMsg, aMsg.Size, @Result, Result.Size, Result.Size, FTimeOut
      ) then
      raise Exception.Create(GetLastErrorText);
  end
  else
    raise Exception.Create(GetLastErrorText);
end;

function TPipeClient.SendMessage(Msg: TPipeMessage): TPipeMessage;
var
  pm: RPIPEMessage;
  s : string;
begin
  ZeroMemory(@pm, SizeOf(pm));
  s :=  msg.Msg;
  if Length(s) > PIPE_MSG_MAXSIZE then
    delete(s, PIPE_MSG_MAXSIZE, length(s));

  pm.Kind := Msg.ID;
  pm.Count := Length(s) * SizeOf(Char);
  StrPCopy(pm.Data, s);
  pm := ProcessMsg(pm);
  Result.ID := pm.Kind;
  Result.Msg := Copy(pm.Data, 1, pm.Count div SizeOf(Char));
end;

function TPipeClient.SendString(aStr: string): string;
var
  Msg: RPIPEMessage;
  s : string;
begin
  // prepare outgoing message
  s :=  aStr;
  if Length(s) > PIPE_MSG_MAXSIZE then
    delete(s, PIPE_MSG_MAXSIZE, Length(s));

  Msg.Kind := 0;
  Msg.Count := Length(s) * SizeOf(Char);
  StrPCopy(Msg.Data, s);
  // send message
  Msg := ProcessMsg(Msg);
  // return data send from server
  Result := Copy(Msg.Data, 1, Msg.Count div SizeOf(Char));
end;

function PipeSendMessage(const PipeName : string; var Msg : TPipeMessage) : boolean; overload;
var
  pc  : TPipeClient;
begin
  try
    pc  :=  TPipeClient.Create('', PipeName);
    try
      Msg :=  pc.SendMessage(Msg);
    finally
      pc.Free;
    end;
    result  :=  true;
  except
    result  :=  false;
  end;
end;

function PipeSendMessage(const PipeName : string; var ID  : integer; var Msg : string): boolean; overload;
var
  pm  : TPipeMessage;
begin
  pm.ID   :=  ID;
  pm.Msg  :=  Msg;
  result  :=  PipeSendMessage(PipeName, pm);
  if result then
  begin
    ID  :=  pm.ID;
    Msg :=  pm.Msg;
  end; 
end;

function PipeSendMessage(const PipeName : string; var ID  : integer): boolean; overload;
var
  msg  : string;
begin
  msg :=  '';
  result  :=  PipeSendMessage(PipeName, ID, Msg);
end;

function PipeSendMessage(const PipeName : string; var Msg : string): boolean; overload;
var
  id  : integer;
begin
  id  :=  0;
  result  :=  PipeSendMessage(PipeName, id, Msg);
end;

end.

