unit uThreadPool;

interface

uses
  Windows, Messages, SysUtils, Classes;

const
  DEF_MAXTHREADCOUNT  = 10;

type
  IWorkThread = interface
    function  GetOnWorkCompleted  : TNotifyEvent;
    procedure SetOnWorkCompleted(AEvent :  TNotifyEvent);
    procedure DoWork;
    procedure DoWorkCompleted;
  end;

  TWorkThread = class(TThread)
  private
    FOnWorkCompleted: TNotifyEvent;
  protected
    property OnWorkCompleted : TNotifyEvent read FOnWorkCompleted Write FOnWorkCompleted;
  protected
    procedure Execute; override;
    procedure DoWork; virtual; abstract;
    procedure DoWorkCompleted;
  end;

  TWorkThreadClass = class of TWorkThread;

  TThreadPool = class
  private
    FThreadClass: TWorkThreadClass;
    FMaxThreadCount: Integer;
    FThreadList : TList;
    FOnWorkCompleted: TNotifyEvent;
    function GetIdleThreadCount: Integer;
  private
    function GetThreadCount: Integer;
  protected
    procedure OnThreadWorkCompleted(Sender : TObject);
    procedure OnThreadTerminate(Sender : TObject);
  public
    property ThreadClass : TWorkThreadClass read FThreadClass;
    property ThreadCount : Integer read GetThreadCount;
    property IdleThreadCount : Integer read GetIdleThreadCount;
    property MaxThreadCount : Integer read FMaxThreadCount Write FMaxThreadCount;
    property OnWorkCompleted : TNotifyEvent read FOnWorkCompleted Write FOnWorkCompleted;
  public
    constructor Create(const AThreadClass : TWorkThreadClass); virtual;
    destructor Destroy; override;

    function  NewThread :  TWorkThread;
    procedure ClearThreadPool;
  end;

implementation

var
  G_ThreadCount : Integer;

{ TWorkThread }

procedure TWorkThread.DoWorkCompleted;
begin
  if Assigned(FOnWorkCompleted) then
    FOnWorkCompleted(Self);
end;

procedure TWorkThread.Execute;
begin
  while not Terminated do
  begin
    DoWork;
    Synchronize(DoWorkCompleted);
  end;
end;

{ TThreadPool }

constructor TThreadPool.Create(const AThreadClass: TWorkThreadClass);
begin
  inherited Create;
  FMaxThreadCount :=  DEF_MAXTHREADCOUNT;
  FThreadList     :=  TList.Create;
  FThreadClass    :=  AThreadClass;
end;

destructor TThreadPool.Destroy;
begin
  ClearThreadPool;
  FThreadList.Free;
  inherited;
end;

function TThreadPool.GetThreadCount: Integer;
begin
  Result  :=  FThreadList.Count;
end;

function TThreadPool.NewThread: TWorkThread;
var
  i : Integer;
begin
  Result  :=  nil;
  for i := 0 to FThreadList.Count - 1 do
    if TWorkThread(FThreadList[i]).Suspended then
    begin
      Result  :=  TWorkThread(FThreadList[i]);
      Exit;
    end;
  if ThreadCount < FMaxThreadCount then
  begin
    Result  :=  FThreadClass.Create(True);
    FThreadList.Add(Result);
    Inc(G_ThreadCount);
    Result.OnWorkCompleted  :=  OnThreadWorkCompleted;
    Result.OnTerminate      :=  OnThreadTerminate;
  end;
end;
        
procedure TThreadPool.ClearThreadPool;
var
  i : Integer;
  t : TWorkThread;
begin
  for i := FThreadList.Count - 1 downto 0 do
  begin
    t :=  TWorkThread(FThreadList[i]);
    t.OnWorkCompleted :=  nil;
    t.OnTerminate :=  nil;
    windows.TerminateThread(t.Handle, $FFFFFFFF);
    t.Free;
    Dec(G_ThreadCount);
  end;
  FThreadList.Clear;
end;

procedure TThreadPool.OnThreadTerminate(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to FThreadList.Count - 1 do
    if Sender = FThreadList[i] then
    begin
      (Sender as TWorkThread).FreeOnTerminate :=  True;
      FThreadList.Delete(i);
      break;
    end;
end;

procedure TThreadPool.OnThreadWorkCompleted(Sender: TObject);
begin
  if Assigned(FOnWorkCompleted) then
  try
    FOnWorkCompleted(Sender);
  except
  end;
  if ThreadCount <= FMaxThreadCount then
    (Sender as TWorkThread).Suspend
  else
    (Sender as TWorkThread).Terminate;
end;

function TThreadPool.GetIdleThreadCount: Integer;
var
  i : Integer;
begin
  Result  :=  0;
  for i := 0 to FThreadList.Count - 1 do
    if TWorkThread(FThreadList[i]).Suspended then
      Inc(Result);
end;

end.
