//目录监视器
unit uDirectoryMonitor;
{$I DVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes;

const
  NotificationBuffer_Size = 1024 * 1024;

type
  TFileAction =   (
      faAdded = 1,
      faRemoved,
      faModifyed,
      faRenamedOldName,
      faRenamedNewName
      );
  TMonitorType  = (mtDirName, mtAttrib, mtSize, mtSecurity, mtCreationDate,
      mtLastAccess, mtLastWrite, mtFileName);
  TMonitorTypes = set of TMonitorType;
  TDirectoryMonitorChangeEvent  =
      procedure(Sender: TObject; FileAction : TFileAction; FileName : string) of object;

  TDirectoryMonitor = class(TComponent)
  private
    FDirectoryHandle: THandle;
    FNotificationBuffer: array[0..NotificationBuffer_Size] of Byte;
    FWatchThread: TThread;
    FNotifyFilter: DWORD;
    FOverlapped: TOverlapped;
    FPOverlapped: POverlapped;
    FBytesWritten: DWORD;
    FCompletionPort: THandle;
    FActive: Boolean;
    FWatchSubTree: Boolean;
    FMonitorDirName: Boolean;
    FMonitorAttributes: Boolean;
    FMonitorSize: Boolean;
    FMonitorSecurity: Boolean;
    FMonitorCreationDate: Boolean;
    FMonitorLastAccess: Boolean;
    FMonitorLastWrite: Boolean;
    FMonitorFileName: Boolean;
    FDirectory: string;
    FOnChange: TDirectoryMonitorChangeEvent;
  private
    function GetMonitorTypes: TMonitorTypes;
    procedure SetMonitorTypes(const Value: TMonitorTypes);
  protected
    property WatchThread  : TThread read FWatchThread;

    procedure DoChange(
        const FileAction : TFileAction;
        const FileName  : string
        );
    procedure Start;
    procedure Stop;
    procedure SetActive(const Value: Boolean);
  public
    property Active : Boolean read FActive Write SetActive;

    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  published
    property Directory  : string read FDirectory Write FDirectory;

    property WatchSubTree : Boolean read FWatchSubTree Write FWatchSubTree default False;

    property MonitorFileName  : Boolean read FMonitorFileName Write FMonitorFileName default False;
    property MonitorDirName : Boolean read FMonitorDirName Write FMonitorDirName default False;
    property MonitorAttributes : Boolean read FMonitorAttributes Write FMonitorAttributes default False;
    property MonitorSize  : Boolean read FMonitorSize Write FMonitorSize default False;
    property MonitorLastWrite : Boolean read FMonitorLastWrite Write FMonitorLastWrite default False;
    property MonitorLastAccess : Boolean read FMonitorLastAccess Write FMonitorLastAccess default False;
    property MonitorCreationDate : Boolean read FMonitorCreationDate Write FMonitorCreationDate default False;
    property MonitorSecurity : Boolean read FMonitorSecurity  Write FMonitorSecurity default False;

    property MonitorTypes : TMonitorTypes read GetMonitorTypes write SetMonitorTypes;

    property OnChange : TDirectoryMonitorChangeEvent read FOnChange Write FOnChange;
  end;

procedure Register;

implementation

type
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..0] of WideChar;
  end;

  TWaitThread = class(TThread)
  private
    FOwner  :   TDirectoryMonitor;
    procedure HandleEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TDirectoryMonitor);
  end;

const
  FILE_LIST_DIRECTORY = $0001;

{ TDirectoryMonitor }

constructor TDirectoryMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompletionPort := 0;
  FDirectoryHandle := 0;
  FPOverlapped := @FOverlapped;
  ZeroMemory(@FOverlapped, SizeOf(FOverlapped));
end;

destructor TDirectoryMonitor.Destroy;
begin
  Active  :=  False;
  inherited;
end;

procedure TDirectoryMonitor.DoChange(
    const FileAction : TFileAction;
    const FileName  : string
    );
var
  fname : string;
begin
  if Pos(':', FileName) > 0 then
    fname :=  FileName
  else
    if FDirectory[Length(FDirectory)] = '\' then
      fname :=  FDirectory + FileName
    else
      fname :=  FDirectory + '\' + FileName;
  if Assigned(FOnChange) then
    FOnChange(Self, FileAction, fname);
end;

function TDirectoryMonitor.GetMonitorTypes: TMonitorTypes;
begin
  result  :=  [];
  if FMonitorAttributes   then result :=  result + [mtAttrib];
  if FMonitorCreationDate then result :=  result + [mtCreationDate];
  if FMonitorDirName      then result :=  result + [mtDirName];
  if FMonitorFileName     then result :=  result + [mtFileName];
  if FMonitorLastAccess   then result :=  result + [mtLastAccess];
  if FMonitorLastWrite    then result :=  result + [mtLastWrite];
  if FMonitorSecurity     then result :=  result + [mtSecurity];
  if FMonitorSize         then result :=  result + [mtSize];
end;

procedure TDirectoryMonitor.SetMonitorTypes(const Value: TMonitorTypes);
begin
  MonitorAttributes   :=  mtAttrib in Value;
  MonitorCreationDate :=  mtCreationDate in Value;
  MonitorDirName      :=  mtDirName in Value;
  MonitorFileName     :=  mtFileName in Value;
  MonitorLastAccess   :=  mtLastAccess in Value;
  MonitorLastWrite    :=  mtLastWrite in Value;
  MonitorSecurity     :=  mtSecurity in Value;
  MonitorSize         :=  mtSize in Value;
end;

procedure TDirectoryMonitor.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if Value then
      Start
    else
      Stop;
    FActive := Value;
  end;
end;

procedure TDirectoryMonitor.Start;
begin
  FNotifyFilter := 0;
  if MonitorFileName then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
  if MonitorDirName then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_DIR_NAME;
  if MonitorAttributes then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if MonitorSize then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_SIZE;
  if MonitorLastWrite then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if MonitorLastAccess then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if MonitorCreationDate then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_CREATION;
  if MonitorSecurity then
    FNotifyFilter := FNotifyFilter or FILE_NOTIFY_CHANGE_SECURITY;

  if FNotifyFilter = 0 then
  begin
    raise Exception.Create('请选择监视内容!');
  end;

  FDirectoryHandle := CreateFile(
      PChar(FDirectory),
      FILE_LIST_DIRECTORY,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      nil,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
      0);
  if FDirectoryHandle = INVALID_HANDLE_VALUE then
  begin
    FDirectoryHandle := 0;
    raise Exception.Create(SysErrorMessage(GetLastError));
  end;
  FCompletionPort := CreateIoCompletionPort(FDirectoryHandle, 0, Longint(pointer(self)), 0);
  ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
  FBytesWritten := 0;
  if not ReadDirectoryChanges(
      FDirectoryHandle,
      @FNotificationBuffer,
      SizeOf(FNotificationBuffer),
      WatchSubTree,
      FNotifyFilter,
      @FBytesWritten,
      @FOverlapped,
      nil
      ) then
  begin
    CloseHandle(FDirectoryHandle);
    FDirectoryHandle := 0;
    CloseHandle(FCompletionPort);
    FCompletionPort := 0;
    raise Exception.Create(SysErrorMessage(GetLastError));
  end;
  FWatchThread := TWaitThread.Create(Self);
  {$IFDEF DELPHI2009_UP}
  TWaitThread(FWatchThread).Start;
  {$ELSE}
  TWaitThread(FWatchThread).Resume;
  {$ENDIF}
end;

procedure TDirectoryMonitor.Stop;
begin
  if FCompletionPort = 0 then
    exit;
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
  FWatchThread.WaitFor;
  FWatchThread.Free;
  FWatchThread  :=  nil;
  CloseHandle(FDirectoryHandle);
  FDirectoryHandle := 0;
  CloseHandle(FCompletionPort);
  FCompletionPort := 0;
end;

{ TWaitThread }

constructor TWaitThread.Create(Owner: TDirectoryMonitor);
begin
  inherited Create(True);
  FOwner := Owner;
  FreeOnTerminate := False;
end;

procedure TWaitThread.Execute;
var
  numBytes: DWORD;
  //cbOffset: DWORD;
  CompletionKey: DWORD;
begin
  while not Terminated do
  begin
    GetQueuedCompletionStatus(FOwner.FCompletionPort, numBytes, CompletionKey, FOwner.FPOverlapped, INFINITE);
    if CompletionKey <> 0 then
    begin
      Synchronize(HandleEvent);
      FOwner.FBytesWritten := 0;
      ZeroMemory(@FOwner.FNotificationBuffer, SizeOf(FOwner.FNotificationBuffer));
      ReadDirectoryChanges(
          FOwner.FDirectoryHandle,
          @FOwner.FNotificationBuffer,
          SizeOf(FOwner.FNotificationBuffer),
          FOwner.WatchSubTree,
          FOwner.FNotifyFilter,
          @FOwner.FBytesWritten,
          @FOwner.FOverlapped,
          nil
          );
    end
    else
      Terminate;
  end;
end;

procedure TWaitThread.HandleEvent;
var
  FileOpNotification: PFileNotifyInformation;
  Offset: Longint;
begin
  Pointer(FileOpNotification) := @FOwner.FNotificationBuffer[0];
  repeat
    Offset := FileOpNotification^.NextEntryOffset;
    FOwner.DoChange(
        TFileAction(FileOpNotification^.Action),
        WideCharLenToString(@(FileOpNotification^.FileName), FileOpNotification^.FileNameLength div 2)
        );
    Pointer(FileOpNotification) := Pointer(Integer(FileOpNotification) + Offset);
  until Offset = 0;
end;

procedure Register;
begin
  RegisterComponents('System', [TDirectoryMonitor]);
end;

end.
