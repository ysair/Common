unit uSuicideThread;

interface

uses
  Classes, Windows, Messages;

type
  TSuicideThread = class(TThread)
  private
    FCheckHandle : HWND;
    FCheckMessage : UINT;
    FMessageResult : UINT;
    FWaitSeconds : Integer;
    procedure SetToLastestCPU;
  protected
    procedure Execute; override;
  public
    constructor Create(
        const ACheckHandle : HWND;
        const ACheckMessage : UINT;
        const AMessageResult :  UINT;
        const AWaitSeconds : Integer
        ); reintroduce;
  end;

implementation

const
  DEF_MSG_TIMEOUT = 1000;

{ TSuicideThread }

constructor TSuicideThread.Create(const ACheckHandle: HWND; const ACheckMessage,
  AMessageResult: UINT; const AWaitSeconds : Integer);

begin
  inherited Create(False);
  FCheckHandle  :=  ACheckHandle;
  FCheckMessage :=  ACheckMessage;
  FMessageResult  :=  AMessageResult;
  FWaitSeconds  :=  AWaitSeconds;
  //FreeOnTerminate :=  True;

  SetToLastestCPU;  //让最后一个CPU来做这个事情
end;

procedure TSuicideThread.Execute;

  procedure _KillSelfApp;
  begin
    TerminateProcess(GetCurrentProcess, 0);
  end;

var
  iCount : Integer;
  hr :  Cardinal;
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
      _KillSelfApp;
  end;
end;

procedure TSuicideThread.SetToLastestCPU;
var
  Info: SYSTEM_INFO;
begin
  FillChar(Info, Sizeof(SYSTEM_INFO),0);
  GetSystemInfo(Info);
  if Info.dwNumberOfProcessors>1 then          //让最后一个CPU来做这个事情
    SetThreadAffinityMask(Self.Handle, Info.dwNumberOfProcessors);
end;

end.
