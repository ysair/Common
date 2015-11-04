unit URLDownload;

interface

uses
  Windows, Messages, SysUtils, Classes, ActiveX, UrlMon;

type
  //下载完成类型
  TUrlDownloadCompleteType = (ctOK, ctStop, ctError);
  //下载进度事件
  TUrlDownloadProgressEvent = procedure(Sender: TObject;
    const iProgress: Integer; const iProgressMax: Integer;
    const iStatusCode: ULONG; const sStatusText: string;
    var bCancel: Boolean) of object;
  TUrlDownloadCompletedEvent = procedure(Sender: TObject;
    const CompleteType: TUrlDownloadCompleteType) of object;

  //阻塞下载
  TUrlDownload = class(TComponent, IBindStatusCallback)
  private
    FURL: string;
    FFileName: string;
    FDownloadComplete: Boolean;
    FOnProgress: TUrlDownloadProgressEvent;
    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
    function GetPriority(out nPriority): HResult; stdcall;
    function OnLowResource(reserved: DWORD): HResult; stdcall;
    function IBindStatusCallback.OnProgress = DoProgress;
    function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
    function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
    function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc;
      stgmed: PStgMedium): HResult; stdcall;
    function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;
  protected
    function DoProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
      szStatusText: LPCWSTR): HResult; stdcall;
  public
    function Download: Boolean; overload;
    function Download(const AURL: string; const AFileName: string): Boolean; overload;
  published
    property URL: string read FURL Write FURL;
    property FileName: string read FFileName Write FFileName;
    property OnProgress: TUrlDownloadProgressEvent read FOnProgress Write FOnProgress;
  end;

  //后台下载
  TBackgroundUrlDownload = class(TComponent)
  private
    FFileName: string;
    FURL: string;
    FDownloading: Boolean;
    FOnCompleteDownload: TUrlDownloadCompletedEvent;
    FOnProgress: TUrlDownloadProgressEvent;
    FThread: TThread;
  protected
    procedure DoDownloadProgress(Sender: TObject;
      const iProgress: Integer; const iProgressMax: Integer;
      const iStatusCode: ULONG; const sStatusText: string;
      var bCancel: Boolean);
    procedure DoCompleteDownload(Sender: TObject;
      const CompleteType: TUrlDownloadCompleteType);
    procedure StopThread;                                   //virtual;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Download; overload;
    procedure Download(const AURL: string; const AFileName: string); overload;
    procedure Stop;
  published
    property URL: string read FURL Write FURL;
    property FileName: string read FFileName Write FFileName;
    property OnProgress: TUrlDownloadProgressEvent read FOnProgress Write FOnProgress;
    property OnCompleteDownload: TUrlDownloadCompletedEvent read FOnCompleteDownload Write FOnCompleteDownload;
  end;

implementation

type
  //下载线程
  TDownloadThread = class(TThread)
  private
    FUrlDownload: TUrlDownload;
    FOwner: TBackgroundUrlDownload;
    FURL: string;
    FFileName: string;
    FOnProgress: TUrlDownloadProgressEvent;
    FOnCompleteDownload: TUrlDownloadCompletedEvent;

    FProgress: Integer;
    FProgressMax: Integer;
    FStatusCode: Integer;
    FStatusText: string;
    FCancel: Boolean;

    procedure OnDownloadProgress(Sender: TObject;
      const iProgress: Integer; const iProgressMax: Integer;
      const iStatusCode: ULONG; const sStatusText: string;
      var bCancel: Boolean);
  protected
    procedure Execute; override;
    procedure DoProgress;
  public
    property OnProgress: TUrlDownloadProgressEvent read FOnProgress Write FOnProgress;
    property OnCompleteDownload: TUrlDownloadCompletedEvent read FOnCompleteDownload Write FOnCompleteDownload;
    constructor Create(
      const AOwner: TBackgroundUrlDownload;
      const AURL: string;
      const AFileName: string;
      const AOnProgress: TUrlDownloadProgressEvent;
      const AOnCompleteDownload: TUrlDownloadCompletedEvent
      );
  end;

//==============================================================================
{ TUrlDownload }

function TUrlDownload.Download: Boolean;
//执行下载, 返回下载是否成功
var
  r: HResult;
begin
  FDownloadComplete := False;
  r := URLDownloadToFile(nil, PChar(FURL), PChar(FFileName), 0, Self as IBindStatusCallback);
  Result := (r = S_OK) and FDownloadComplete;
end;

function TUrlDownload.Download(const AURL, AFileName: string): Boolean;
begin
  FURL := AURL;
  FFileName := AFileName;
  Result := Download;
end;

function TUrlDownload.GetBindInfo(out grfBINDF: DWORD;
  var bindinfo: TBindInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TUrlDownload.GetPriority(out nPriority): HResult;
begin
  Result := E_NOTIMPL;
end;

function TUrlDownload.OnDataAvailable(grfBSCF, dwSize: DWORD;
  formatetc: PFormatEtc; stgmed: PStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

function TUrlDownload.OnLowResource(reserved: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TUrlDownload.OnObjectAvailable(const iid: TGUID;
  punk: IInterface): HResult;
begin
  Result := E_NOTIMPL;
end;

function TUrlDownload.DoProgress(ulProgress, ulProgressMax,
  ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult;
//回调函数, 下载的进程
var
  Status: string;
  bCancel: Boolean;
begin
  Result := E_NOTIMPL;
  if Assigned(FOnProgress) then
  begin
    bCancel := False;
    case ulStatusCode of {(*}
      BINDSTATUS_FINDINGRESOURCE            : Status := 'Finding resource ' + szStatusText;
      BINDSTATUS_CONNECTING                 : Status := 'Connecting to ' + szStatusText;
      BINDSTATUS_REDIRECTING                : Status := 'Redirecting...';
      BINDSTATUS_BEGINDOWNLOADDATA          : Status := 'Start to download ' + szStatusText;
      BINDSTATUS_DOWNLOADINGDATA            : Status := 'Downloading...';
      BINDSTATUS_ENDDOWNLOADDATA            : Status := 'Complete downloading ' + szStatusText;
      BINDSTATUS_BEGINDOWNLOADCOMPONENTS    : Status := 'Start to download components';
      BINDSTATUS_INSTALLINGCOMPONENTS       : Status := 'Installing components...';
      BINDSTATUS_ENDDOWNLOADCOMPONENTS      : Status := 'Complete downloading components';
      BINDSTATUS_USINGCACHEDCOPY            : Status := 'Copying form buffer...';
      BINDSTATUS_SENDINGREQUEST             : Status := 'Sending request...';
      BINDSTATUS_CLASSIDAVAILABLE           : Status := 'Class ID is available';
      BINDSTATUS_MIMETYPEAVAILABLE          : Status := 'MIME type is available';
      BINDSTATUS_CACHEFILENAMEAVAILABLE     : Status := 'Cache file name is available';
      BINDSTATUS_BEGINSYNCOPERATION         : Status := 'Start sync operation';
      BINDSTATUS_ENDSYNCOPERATION           : Status := 'Complete sync operation';
      BINDSTATUS_BEGINUPLOADDATA            : Status := 'Start to upload data';
      BINDSTATUS_UPLOADINGDATA              : Status := 'Uploading data';
      BINDSTATUS_ENDUPLOADDATA              : Status := 'Complete Uploading data';
      BINDSTATUS_PROTOCOLCLASSID            : Status := 'Protocol class ID is available';
      BINDSTATUS_ENCODING                   : Status := 'Encoding...';
      BINDSTATUS_VERIFIEDMIMETYPEAVAILABLE  : Status := 'verified MIME is available';
      BINDSTATUS_CLASSINSTALLLOCATION       : Status := 'Class install location';
      BINDSTATUS_DECODING                   : Status := 'Decoding...';
      BINDSTATUS_LOADINGMIMEHANDLER         : Status := 'Loading MIME handler';
      BINDSTATUS_CONTENTDISPOSITIONATTACH   : Status := 'Content disposition attach';
      BINDSTATUS_FILTERREPORTMIMETYPE       : Status := 'Filter report MIME type';
      BINDSTATUS_CLSIDCANINSTANTIATE        : Status := 'Clsid can instantiate';
      BINDSTATUS_IUNKNOWNAVAILABLE          : Status := 'Unknown available';
      BINDSTATUS_DIRECTBIND                 : Status := 'Direct bind';
      BINDSTATUS_RAWMIMETYPE                : Status := 'MIME type of the resource, before any code sniffing is done';
      BINDSTATUS_PROXYDETECTING             : Status := 'Detecting proxy...';
      BINDSTATUS_ACCEPTRANGES               : Status := 'Valid types of range requests for a resource';
      else
        Status := szStatusText; {*)}
    end;
    FOnProgress(Self, ulProgress, ulProgressMax, ulStatusCode, Status, bCancel);
    if bCancel then
      Result := E_ABORT
    else begin
      if ulStatusCode = BINDSTATUS_ENDDOWNLOADDATA then
        FDownloadComplete := True;
    end;
  end;
end;

function TUrlDownload.OnStartBinding(dwReserved: DWORD;
  pib: IBinding): HResult;
begin
  Result := E_NOTIMPL;
end;

function TUrlDownload.OnStopBinding(hresult: HResult;
  szError: LPCWSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

//==============================================================================
{ TBackgroundUrlDownload }

procedure TBackgroundUrlDownload.Download;
//执行下载动作, 如果正在下载, 会抛出异常
begin
  if FDownloading then
    raise Exception.Create('正在下载中, 请先中断下载.');
  FDownloading := True;
  FThread := TDownloadThread.Create(
    Self, FURL, FFileName,
    DoDownloadProgress, DoCompleteDownload
    );
end;

procedure TBackgroundUrlDownload.Download(const AURL: string; const AFileName: string);
begin
  FURL := AURL;
  FFileName := AFileName;
  Download;
end;

procedure TBackgroundUrlDownload.DoDownloadProgress(Sender: TObject;
  const iProgress, iProgressMax: Integer; const iStatusCode: ULONG;
  const sStatusText: string; var bCancel: Boolean);
//供线程回调的事件
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, iProgress, iProgressMax, iStatusCode, sStatusText, bCancel);
  if not FDownloading then
    StopThread;
end;

procedure TBackgroundUrlDownload.DoCompleteDownload(Sender: TObject;
  const CompleteType: TUrlDownloadCompleteType);
begin
  FDownloading := False;
  FThread := nil;                                           //下载完,线程自己释放
  if Assigned(FOnCompleteDownload) then
    FOnCompleteDownload(Self, CompleteType);                //下载完成的事件
end;

procedure TBackgroundUrlDownload.Stop;
//中断下载
begin
  if FDownloading then
  begin
    FDownloading := False;
    StopThread;
  end;
end;

procedure TBackgroundUrlDownload.StopThread;
begin
  if Assigned(FThread) then
  begin
    TDownloadThread(FThread).OnProgress := nil;
    TDownloadThread(FThread).OnCompleteDownload := nil;
    FThread.Terminate;
    FThread := nil;
    DoCompleteDownload(Self, ctStop);
  end;
end;

destructor TBackgroundUrlDownload.Destroy;
begin
  Stop;
  inherited;
end;

//==============================================================================
{ TDownloadThread }

constructor TDownloadThread.Create(
  const AOwner: TBackgroundUrlDownload;
  const AURL: string;
  const AFileName: string;
  const AOnProgress: TUrlDownloadProgressEvent;
  const AOnCompleteDownload: TUrlDownloadCompletedEvent
  );
begin
  FOwner := AOwner;
  FURL := AURL;
  FFileName := AFileName;
  FOnProgress := AOnProgress;
  FOnCompleteDownload := AOnCompleteDownload;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TDownloadThread.DoProgress;
//同步调用的方法
begin
  FOnProgress(Self, FProgress, FProgressMax, FStatusCode, FStatusText, FCancel);
end;

procedure TDownloadThread.Execute;
var
  ok: Boolean;
begin
  inherited;
  FUrlDownload := TUrlDownload.Create(nil);
  try
    FUrlDownload.URL := FURL;
    FUrlDownload.FileName := FFileName;
    FUrlDownload.OnProgress := OnDownloadProgress;
    ok := FUrlDownload.Download;
    if Assigned(FOnCompleteDownload) then                   //下载完成的事件
      if ok then
        FOnCompleteDownload(Self, ctOK)
      else
        FOnCompleteDownload(Self, ctError);
  finally
    FUrlDownload.Free;
  end;
end;

procedure TDownloadThread.OnDownloadProgress(Sender: TObject;
  const iProgress, iProgressMax: Integer; const iStatusCode: ULONG;
  const sStatusText: string; var bCancel: Boolean);
begin
  if Terminated then
    bCancel := True
  else
    if Assigned(FOnProgress) then
    begin
      FProgress := iProgress;
      FProgressMax := iProgressMax;
      FStatusCode := iStatusCode;
      FStatusText := sStatusText;
      FCancel := bCancel;
      Synchronize(DoProgress);
      bCancel := FCancel;
      if FCancel then
        Terminate;
    end;
end;

end.

