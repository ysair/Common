unit uSuicideThread;

interface

uses
  Classes, Windows, Messages, SysUtils;

type
  TSuicideAnchorThread = class(TThread)
  protected
    procedure SetToLastestCPU;
    procedure KillSelfApp;
  end;

  {*** 自杀线程：给一个窗口发消息，如果消息没有响应累计到一定秒数，就会自杀当前进程。
    一般用于：判断程序没有响应一定时间就自杀。
  }
  TSuicide_ByMsgResponse_Thread = class(TSuicideAnchorThread)
  private
    FCheckHandle : HWND;
    FWaitSeconds : Integer;
    FCheckMessage : UINT;
    FMessageResult : UINT;
  protected
    procedure Execute; override;
  public
    property CheckHandle : HWND read FCheckHandle write FCheckHandle;
    property CheckMessage : UINT read FCheckMessage write FCheckMessage;
    property MessageResult : UINT read FMessageresult write FMessageResult;
    property WaitSeconds : Integer read FWaitSeconds write FWaitSeconds;

    constructor Create(
        const ACheckHandle : HWND;
        const ACheckMessage : UINT;
        const AMessageResult :  UINT;
        const AWaitSeconds : Integer = 20
        ); reintroduce;
  end;

  {*** 自杀线程：判断窗口是否存在，如果这个窗口不存在超过一定秒数，就会自杀当前进程。
  }
  TSuicide_ByWindowExists_Thread = class(TSuicideAnchorThread)
  private
    FCheckHandle : HWND;
    FWaitSeconds : Integer;
  protected
    procedure Execute; override;
  public
    property CheckHandle : HWND read FCheckHandle write FCheckHandle;
    property WaitSeconds : Integer read FWaitSeconds write FWaitSeconds;
    constructor Create(
        const ACheckHandle : HWND;
        const AWaitSeconds : Integer = 3
        ); reintroduce;
  end;


implementation

{ TSuicideAnchorThread }
procedure TSuicideAnchorThread.KillSelfApp;
begin
  TerminateProcess(GetCurrentProcess, 0);
end;

procedure TSuicideAnchorThread.SetToLastestCPU;
var
  Info: SYSTEM_INFO;
begin
  FillChar(Info, Sizeof(SYSTEM_INFO),0);
  GetSystemInfo(Info);
  if Info.dwNumberOfProcessors>1 then          //让最后一个CPU来做这个事情
    SetThreadAffinityMask(Self.Handle, Info.dwNumberOfProcessors);
end;


{ TSuicide_ByMsgResponse_Thread }

constructor TSuicide_ByMsgResponse_Thread.Create(const ACheckHandle: HWND; const ACheckMessage,
  AMessageResult: UINT; const AWaitSeconds: Integer);
begin
  inherited Create(False);
  FCheckHandle  :=  ACheckHandle;
  FCheckMessage :=  ACheckMessage;
  FMessageResult  :=  AMessageResult;
  FWaitSeconds  :=  AWaitSeconds;
  SetToLastestCPU;  //让最后一个CPU来做这个事情
end;

procedure TSuicide_ByMsgResponse_Thread.Execute;
var
  iCount : Integer;
  hr :  Cardinal;
const
  DEF_MSG_TIMEOUT = 1000;
begin
  inherited;
  iCount  :=  0;
  while not Terminated do
  begin
    Sleep(1000);
    if SendMessageTimeOut(FCheckHandle, FCheckMessage, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, DEF_MSG_TIMEOUT, hr) = 0 then
      Inc(iCount)
    else if hr = FMessageResult then
      iCount  :=  0
    else
      Inc(iCount);
    if iCount > FWaitSeconds then
      KillSelfApp;
  end;
end;

{ TSuicide_ByWindowExists_Thread }

constructor TSuicide_ByWindowExists_Thread.Create(const ACheckHandle: HWND;
  const AWaitSeconds: Integer);
begin
  inherited Create(False);
  FCheckHandle  :=  ACheckHandle;
  FWaitSeconds  :=  AWaitSeconds;
  SetToLastestCPU;  //让最后一个CPU来做这个事情
end;

procedure TSuicide_ByWindowExists_Thread.Execute;
var
  iCount : Integer;
begin
  inherited;
  iCount  :=  0;
  while not Terminated do
  begin
    Sleep(1000);
    if not IsWindow(FCheckHandle)  then
      Inc(iCount)
    else
      iCount  :=  0;

    if iCount > FWaitSeconds then
      KillSelfApp;
  end;
end;

end.
