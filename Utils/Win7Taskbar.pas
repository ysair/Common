(* ******************************************************************************
  * 文件名: Win7Taskbar.pas                                                     *
  * 创建日期: 2011-05-01                                                        *
  * 实现win7任务栏api的简单封装                                                 *
  * by: bahamut                                                                 *
  ****************************************************************************** *)
unit Win7Taskbar;

interface

uses
  Windows,
  Messages,
  ActiveX { , CommCtrl } ;

(* 相关定义 *)
const
  SID_ITaskbarList = '{56FDF342-FD6D-11D0-958A-006097C9A090}';
  SID_ITaskbarList2 = '{602D4995-B13A-429B-A66E-1935E44F4317}';
  SID_ITaskbarList3 = '{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}';
  SID_ITaskbarList4 = '{C43DC798-95D1-4BEA-9030-BB99E2983A1A}';

const
  IID_ITaskbarList: TGUID = SID_ITaskbarList;
  IID_ITaskbarList2: TGUID = SID_ITaskbarList2;
  IID_ITaskbarList3: TGUID = SID_ITaskbarList3;
  IID_ITaskbarList4: TGUID = SID_ITaskbarList4;
  CLSID_TaskbarList: TGUID = '{56FDF344-FD6D-11D0-958A-006097C9A090}';

const
  TBPF_NOPROGRESS = $00000000;
  TBPF_INDETERMINATE = $00000001;
  TBPF_NORMAL = $00000002;
  TBPF_ERROR = $00000004;
  TBPF_PAUSED = $00000008;
  TBATF_USEMDITHUMBNAIL = $00000001;
  TBATF_USEMDILIVEPREVIEW = $00000002;
  // THUMBBUTTON mask
  THB_BITMAP = $00000001;
  THB_ICON = $00000002;
  THB_TOOLTIP = $00000004;
  THB_FLAGS = $00000008;
  THBN_CLICKED = $00001800;
  // THUMBBUTTON flags
  THBF_ENABLED = $00000000;
  THBF_DISABLED = $00000001;
  THBF_DISMISSONCLICK = $00000002;
  THBF_NOBACKGROUND = $00000004;
  THBF_HIDDEN = $00000008;
  THBF_NONINTERACTIVE = $00000010;
  STPF_NONE = $00000000;
  STPF_USEAPPTHUMBNAILALWAYS = $00000001;
  STPF_USEAPPTHUMBNAILWHENACTIVE = $00000002;
  STPF_USEAPPPEEKALWAYS = $00000004;
  STPF_USEAPPPEEKWHENACTIVE = $00000008;

type
  ULONGLONG = UINT64;
  HIMAGELIST = HWND;
  STPFLAG = UINT;

type
  { interface ITaskbarList3 }
  PThumbButton = ^TThumbButton;

  _THUMBBUTTON = packed record
    dwMask: DWORD;
    iId: UINT;
    iBitmap: UINT;
    hIcon: hIcon;
    szTip: array [0 .. 259] of WideChar;
    dwFlags: DWORD;
  end;

  THUMBBUTTON = _THUMBBUTTON;
  TThumbButton = _THUMBBUTTON;

type
  { interface ITaskbarList }
  ITaskbarList = interface(IUnknown)
    [SID_ITaskbarList]
    function HrInit: HRESULT; stdcall;
    function AddTab(HWND: HWND): HRESULT; stdcall;
    function DeleteTab(HWND: HWND): HRESULT; stdcall;
    function ActivateTab(HWND: HWND): HRESULT; stdcall;
    function SetActiveAlt(HWND: HWND): HRESULT; stdcall;
  end;

type
  { interface ITaskbarList2 }
  ITaskbarList2 = interface(ITaskbarList)
    [SID_ITaskbarList2]
    function MarkFullscreenWindow(HWND: HWND; fFullscreen: BOOL): HRESULT;
      stdcall;
  end;

type
  { interface ITaskbarList3 }
  ITaskbarList3 = interface(ITaskbarList2)
    [SID_ITaskbarList3]
    function SetProgressValue(HWND: HWND; ullCompleted: ULONGLONG;
      ullTotal: ULONGLONG): HRESULT; stdcall;
    function SetProgressState(HWND: HWND; tbpFlags: UINT): HRESULT; stdcall;
    function RegisterTab(hwndTab: HWND; hwndMDI: HWND): HRESULT; stdcall;
    function UnregisterTab(hwndTab: HWND): HRESULT; stdcall;
    function SetTabOrder(hwndTab: HWND; hwndInsertBefore: HWND): HRESULT;
      stdcall;
    function SetTabActive(hwndTab: HWND; hwndMDI: HWND;
      tbatFlags: UINT): HRESULT; stdcall;
    function ThumbBarAddButtons(HWND: HWND; cButtons: UINT;
      pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarUpdateButtons(HWND: HWND; cButtons: UINT;
      pButton: PThumbButton): HRESULT; stdcall;
    function ThumbBarSetImageList(HWND: HWND; himl: HIMAGELIST): HRESULT;
      stdcall;
    function SetOverlayIcon(HWND: HWND; hIcon: hIcon;
      pszDescription: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailTooltip(HWND: HWND; pszTip: LPCWSTR): HRESULT; stdcall;
    function SetThumbnailClip(HWND: HWND; var prcClip: TRect): HRESULT; stdcall;
  end;

type
  { interface ITaskbarList4 }
  ITaskbarList4 = interface(ITaskbarList3)
    [SID_ITaskbarList4]
    function SetTabProperties(hwndTab: HWND; stpFlags: STPFLAG): HRESULT;
      stdcall;
  end;

  (* win7 api 定义完毕 *)
const
  MAX_BUTTON = 7;
  // 最大按钮数, windows系统规定的...
type
  TProgressState = (psNoProgress, psIndeterminate, psNormal, psError, psPaused);

type
  TNotifyEvent = procedure(Sender: TObject) of object;
  (* 自定义类开始 *)
{$M+}

type
  TWin7Taskbar = class; // 主类
  TTaskbarThumbButtons = class;
  // 按钮集合类,用来管理按钮
  TTaskbarThumbButton = class(TObject) // 按钮类,分开定义的目的是简化操作
  private
    FCommandId: DWORD;
    FParent: TTaskbarThumbButtons;
    FVisible: Boolean;
    FShowHint: Boolean;
    FEnabled: Boolean;
    FIcon: hIcon;
    FImageIndex: Integer;
    FHint: WideString;
    FTransparent: Boolean;
    FOnClick: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetHint(const Value: WideString);
    procedure SetIcon(const Value: hIcon);
    procedure SetImageIndex(const Value: Integer);
    procedure SetShowHint(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure DoClick; dynamic;
    procedure DoChanging; dynamic;
    procedure DoChanged; dynamic;
    procedure Click; dynamic;
    procedure Update; virtual;
  public
    constructor Create(AParent: TTaskbarThumbButtons; ACommandId: DWORD);
    destructor Destroy; override;
  published
    // 是否可用
    property Enabled: Boolean read FEnabled write SetEnabled;
    // 是否可见
    property Visible: Boolean read FVisible write SetVisible;
    // 是否显示提示内容
    property ShowHint: Boolean read FShowHint write SetShowHint;
    // 提示内容文本
    property Hint: WideString read FHint write SetHint;
    // 图标句柄
    property Icon: hIcon read FIcon write SetIcon;
    // 图片序号
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    // 是否透明
    property Transparent: Boolean read FTransparent write SetTransparent;
    // 单击事件
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    // 正在改变
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    // 发生改变
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TTaskbarThumbButtons = class(TObject)
  private
    // 按钮数组, 当类一创建就创建最大数的按钮出来, 所有按钮默认全部不显示
    FButtons: array [0 .. MAX_BUTTON - 1] of TTaskbarThumbButton;
    FOwner: TWin7Taskbar;
    FImageList: HWND;
    FOnButtonCreated: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    procedure SetImageList(const Value: HWND);
    function GetButton(Index: Integer): TTaskbarThumbButton;
  protected
    procedure DoButtonCreated; dynamic;
    procedure DoChanging; dynamic;
    procedure DoChanged; dynamic;
    procedure Update(Button: TTaskbarThumbButton); virtual;
    procedure UpdateVisible; virtual;
  public
    constructor Create(AOwner: TWin7Taskbar);
    destructor Destroy; override;
    // 判断一个按钮在列表中的序号
    function IndexOf(Button: TTaskbarThumbButton): Integer;
    // 根据id查找按钮对象
    function FindByCommandId(CommandId: DWORD): TTaskbarThumbButton;
    // 按钮项
    property Button[Index: Integer]: TTaskbarThumbButton read GetButton;
    default;
  published
    // 图标列表句柄
    property ImageList: HWND read FImageList write SetImageList;
    // 按钮创建事件
    property OnButtonCreated
      : TNotifyEvent read FOnButtonCreated write FOnButtonCreated;
    // 正在改变
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    // 发生改变
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TTaskbarProgress = class(TObject)
  private
    FOwner: TWin7Taskbar;
    FPosition: Int64;
    FMin: Int64;
    FMax: Int64;
    FState: TProgressState;
    FOnPosition: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure SetMax(const Value: Int64);
    procedure SetMin(const Value: Int64);
    procedure SetPosition(const Value: Int64);
    procedure SetState(const Value: TProgressState);
  protected
    procedure DoPosition; dynamic;
    procedure DoChanging; dynamic;
    procedure DoChanged; dynamic;
    procedure Update; virtual;
  public
    constructor Create(AOwner: TWin7Taskbar);
    destructor Destroy; override;
  published
    property State: TProgressState read FState write SetState;
    property Min: Int64 read FMin write SetMin;
    property Max: Int64 read FMax write SetMax;
    property Position: Int64 read FPosition write SetPosition;
    // 改变进度事件
    property OnPosition: TNotifyEvent read FOnPosition write FOnPosition;
    // 正在改变
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    // 发生改变
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TTaskbarOverlayIcon = class(TObject)
  private
    FOwner: TWin7Taskbar;
    FIcon: hIcon;
    FDescription: WideString;
    FOnChanging: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    procedure SetDescription(const Value: WideString);
    procedure SetIcon(const Value: hIcon);
  protected
    procedure DoChanging; dynamic;
    procedure DoChanged; dynamic;
    procedure Update; virtual;
  public
    constructor Create(AOwner: TWin7Taskbar);
    destructor Destroy; override;
  published
    // 小图标句柄(图标会显示在工具栏图标的右下角)
    property Icon: hIcon read FIcon write SetIcon;
    // 说明
    property Description: WideString read FDescription write SetDescription;
    // 正在改变
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    // 发生改变
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TWin7Taskbar = class(TObject)
  private
    FCanCreate: Boolean;
    FMainWindow: HWND;
    FTaskbarList: ITaskbarList4;
    FButtons: TTaskbarThumbButtons;
    FProgress: TTaskbarProgress;
    FOverlayIcon: TTaskbarOverlayIcon;
    FCreateButtonMessage: UINT; // windows创建按钮的消息
    FOldWndProc: Pointer;
    FNewWndProc: Pointer;
    function NewWndProc(HWND: HWND; uMsg: UINT; wParam: wParam;
      lParam: lParam): LRESULT; stdcall;
  protected
    // 添加按钮方法, 这个方法只会调用一次,
    // ITaskbarList3的ThumbBarAddButtons方法在重复调用的时候似乎会失效,
    // msdn中没有相关说明, 返回值也总是s_ok
    // bug?
    procedure AppendButton; virtual;
    // 更新按钮状态, 对象首次创建的时候会一次性添加7(最大)个按钮,
    // 以后修改相关属性其实都是调用这个方法修改对应按钮的状态而已.
    procedure UpdateButton(Index: Integer); virtual;
    // 修改按钮的图片列表
    procedure UpdateImageList; virtual;
    // 更新进度条属性
    procedure UpdateProgress; virtual;
    // 更新图标
    procedure UpdateOverlayIcon; virtual;
  public
    constructor Create(AMainWindow: HWND = 0);
    destructor Destroy; override;
    // 刷新所有TaskbarList接口效果(就是全部重新调用一次通知)
    procedure Refresh;
  published
    // 按钮
    property Buttons: TTaskbarThumbButtons read FButtons;
    // 进度条
    property Progress: TTaskbarProgress read FProgress;
    // 小图标
    property OverlayIcon: TTaskbarOverlayIcon read FOverlayIcon;
  end;
{$M-}

function IsVistaLater: Boolean; stdcall;// 判断操作系统是否是vista以后的系统
function GetMainWindow: HWND; stdcall;// 取主窗口句柄

implementation

const
  BitmapMask: array [Boolean] of DWORD = (0, THB_BITMAP);
  IconMask: array [Boolean] of DWORD = (0, THB_ICON);
  HintMask: array [Boolean] of DWORD = (0, THB_TOOLTIP);
  VisibleFlag: array [Boolean] of DWORD = (THBF_HIDDEN, 0);
  EnabledFlag: array [Boolean] of DWORD = (THBF_DISABLED, THBF_ENABLED);
  TransparentFlag: array [Boolean] of DWORD = (0, THBF_NOBACKGROUND);

type
  PEnumParam = ^TEnumParam;

  TEnumParam = packed record
    dwProcessId: DWORD;
    hMainWindow: HWND;
  end;

function EnumProc(HWND: HWND; lParam: lParam): BOOL; stdcall;
var
  dwProcessId: DWORD;
  lpParam: PEnumParam;
begin
  Result := True;
  if not IsWindowVisible(HWND) then
    Exit;
  if GetWindowLong(HWND, GWL_EXSTYLE)
    and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW then
    Exit;
  if GetWindowLong(HWND, GWL_HWNDPARENT) <> 0 then
    Exit;
  GetWindowThreadProcessId(HWND, dwProcessId);
  lpParam := Pointer(lParam);
  if dwProcessId = lpParam^.dwProcessId then
  begin
    lpParam^.hMainWindow := HWND;
    Result := False;
  end;
end;

// 这个函数通过列举窗口的方式查询应用程序的主窗口句柄
function GetMainWindow: HWND;
var
  dwCurrentProcessId: DWORD;
  lpParam: TEnumParam;
begin
  dwCurrentProcessId := GetCurrentProcessId;
  Result := 0;
  if dwCurrentProcessId <> 0 then
  begin
    lpParam.dwProcessId := dwCurrentProcessId;
    lpParam.hMainWindow := 0;
    EnumWindows(@EnumProc, lParam(@lpParam));
    Result := lpParam.hMainWindow;
  end;
end;

function IsVistaLater: Boolean;
var
  lpVersion: TOSVersionInfo;
begin
  Result := False;
  FillChar(lpVersion, SizeOf(lpVersion), 0);
  lpVersion.dwOSVersionInfoSize := SizeOf(lpVersion);
  if GetVersionEx(lpVersion) then
    Result := lpVersion.dwMajorVersion > 5;
end;

// 构造出一段Thunk代码
function CreateThunk(lpObject: TObject; lpCallBackProc: Pointer): Pointer;
const
  PageSize = 4096;
  SizeOfJmpCode = 5;
type
  PCode = ^TCode;

  TCode = packed record
    bInt3: Byte; // 想调试的的时候填Int 3($CC),不想调试的时候填nop($90)
    bPopEAX: Byte; // 把返回地址从栈中弹出
    bPush: Byte; // 压栈指令
    pAddrOfSelf: TObject; // 压入Self地址,把Self作为第一个参数
    bPushEAX: Byte; // 重新压入返回地址
    bJmp: Byte; // 相对跳转指令
    uAddrOfJmp: Cardinal; // 要跳转到的地址,
  end;
var
  lpCode: PCode;
begin
  // 分配一段可以执行,可读写的内存
  Result := VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  lpCode := Result;
  lpCode^.bInt3 := $90; // nop
  // lpCode^.bInt3:= $CC; //Int 3
  lpCode^.bPopEAX := $58;
  lpCode^.bPush := $68;
  lpCode^.pAddrOfSelf := lpObject;
  lpCode^.bPushEAX := $50;
  lpCode^.bJmp := $E9;
  lpCode^.uAddrOfJmp := DWORD(lpCallBackProc) -
    (DWORD(@lpCode^.bJmp) + SizeOfJmpCode); // 计算相对地址
end;

// 销毁thunk代码
procedure ReleaseThunk(lpThunk: Pointer);
begin
  VirtualFree(lpThunk, 0, MEM_RELEASE);
end;

{ TTaskbarThumbButton }
procedure TTaskbarThumbButton.Click;
begin
  if Visible and Enabled then
    DoClick;
end;

constructor TTaskbarThumbButton.Create(AParent: TTaskbarThumbButtons;
  ACommandId: DWORD);
begin
  inherited Create;
  FParent := AParent;
  FCommandId := ACommandId;
  FVisible := False;
  FShowHint := True;
  FEnabled := True;
  FIcon := 0;
  FImageIndex := -1;
  FHint := '';
  FOnClick := nil;
  FOnChanged := nil;
  FOnChanging := nil;
end;

destructor TTaskbarThumbButton.Destroy;
begin
  FOnChanging := nil;
  FOnChanged := nil;
  FOnClick := nil;
  FParent := nil;
  inherited Destroy;
end;

procedure TTaskbarThumbButton.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTaskbarThumbButton.DoChanging;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TTaskbarThumbButton.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TTaskbarThumbButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;
  FEnabled := Value;
  Update;
end;

procedure TTaskbarThumbButton.SetHint(const Value: WideString);
var
  Temp: WideString;
  APos: Integer;
begin
  Temp := Value;
  repeat // 去除字符&
    APos := Pos('&', Temp);
    if APos = 0 then
      Break;
    Delete(Temp, APos, 1);
  until False;
  if FHint = Temp then
    Exit;
  FHint := Temp;
  Update;
end;

procedure TTaskbarThumbButton.SetIcon(const Value: hIcon);
begin
  if FIcon = Value then
    Exit;
  FIcon := Value;
  Update;
end;

procedure TTaskbarThumbButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex = Value then
    Exit;
  FImageIndex := Value;
  Update;
end;

procedure TTaskbarThumbButton.SetShowHint(const Value: Boolean);
begin
  if FShowHint = Value then
    Exit;
  FShowHint := Value;
  Update;
end;

procedure TTaskbarThumbButton.SetTransparent(const Value: Boolean);
begin
  if FTransparent = Value then
    Exit;
  FTransparent := Value;
  Update;
end;

procedure TTaskbarThumbButton.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then
    Exit;
  FVisible := Value;
  Update;
end;

procedure TTaskbarThumbButton.Update;
begin
  // 通知集合类要更新自己...
  FParent.Update(Self);
end;

{ TTaskbarThumbButtons }
constructor TTaskbarThumbButtons.Create(AOwner: TWin7Taskbar);
var
  Loop: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  FOnButtonCreated := nil;
  FOnChanging := nil;
  FOnChanged := nil;
  // 默认创建出系统允许的最大按钮数
  for Loop := Low(FButtons) to High(FButtons) do
    FButtons[Loop] := TTaskbarThumbButton.Create(Self, Loop);
end;

destructor TTaskbarThumbButtons.Destroy;
var
  Loop: Integer;
begin
  for Loop := Low(FButtons) to High(FButtons) do
    FButtons[Loop].Free;
  FOnChanged := nil;
  FOnChanging := nil;
  FOnButtonCreated := nil;
  FOwner := nil;
  inherited Destroy;
end;

procedure TTaskbarThumbButtons.DoButtonCreated;
begin
  if Assigned(FOnButtonCreated) then
    FOnButtonCreated(Self);
end;

procedure TTaskbarThumbButtons.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTaskbarThumbButtons.DoChanging;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

function TTaskbarThumbButtons.FindByCommandId(CommandId: DWORD)
  : TTaskbarThumbButton;
var
  Loop: Integer;
begin
  for Loop := 0 to MAX_BUTTON - 1 do
  begin
    Result := FButtons[Loop];
    if Result.FCommandId = CommandId then
      Exit;
  end;
  Result := nil;
end;

function TTaskbarThumbButtons.GetButton(Index: Integer): TTaskbarThumbButton;
begin
  if (Index > -1) and (Index < MAX_BUTTON) then
    Result := FButtons[Index]
  else
    Result := nil;
end;

function TTaskbarThumbButtons.IndexOf(Button: TTaskbarThumbButton): Integer;
begin
  Result := 0;
  while Result < MAX_BUTTON do
  begin
    if FButtons[Result] = Button then
      Exit;
    Inc(Result);
  end;
  Result := -1;
end;

procedure TTaskbarThumbButtons.SetImageList(const Value: HWND);
begin
  if FImageList = Value then
    Exit;
  FImageList := Value;
  FOwner.UpdateImageList;
end;

procedure TTaskbarThumbButtons.Update(Button: TTaskbarThumbButton);
begin
  FOwner.UpdateButton(IndexOf(Button));
end;

procedure TTaskbarThumbButtons.UpdateVisible;
var
  Loop: Integer;
begin
  for Loop := 0 to MAX_BUTTON - 1 do
    if FButtons[Loop].FVisible then
      FOwner.UpdateButton(Loop);
end;

{ TTaskbarProgress }
constructor TTaskbarProgress.Create(AOwner: TWin7Taskbar);
begin
  inherited Create;
  FOwner := AOwner;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FState := psNoProgress;
  FOnPosition := nil;
  FOnChanged := nil;
  FOnChanging := nil;
end;

destructor TTaskbarProgress.Destroy;
begin
  FOnChanging := nil;
  FOnChanged := nil;
  FOnPosition := nil;
  FOwner := nil;
  inherited Destroy;
end;

procedure TTaskbarProgress.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTaskbarProgress.DoChanging;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TTaskbarProgress.DoPosition;
begin
  if Assigned(FOnPosition) then
    FOnPosition(Self);
end;

procedure TTaskbarProgress.SetMax(const Value: Int64);
begin
  if FMax = Value then
    Exit;
  if Value < FMin then
    FMax := FMin
  else
    FMax := Value;
  SetPosition(FPosition);
end;

procedure TTaskbarProgress.SetMin(const Value: Int64);
begin
  if FMin = Value then
    Exit;
  if Value > FMax then
    FMin := FMax
  else
    FMin := Value;
  SetPosition(FPosition);
end;

procedure TTaskbarProgress.SetPosition(const Value: Int64);
begin
  if FPosition = Value then
    Exit;
  if Value < FMin then
    FPosition := FMin
  else
    if Value > FMax then
      FPosition := FMax
    else
      FPosition := Value;
  DoPosition;
  if FState <> psIndeterminate then
    // 如果样式为TBPF_INDETERMINATE则不需要更新状态
    Update;
end;

procedure TTaskbarProgress.SetState(const Value: TProgressState);
begin
  if FState = Value then
    Exit;
  FState := Value;
  Update;
end;

procedure TTaskbarProgress.Update;
begin
  FOwner.UpdateProgress;
end;

{ TTaskbarOverlayIcon }
constructor TTaskbarOverlayIcon.Create(AOwner: TWin7Taskbar);
begin
  inherited Create;
  FOwner := AOwner;
  FIcon := 0;
  FDescription := '';
  FOnChanging := nil;
  FOnChanged := nil;
end;

destructor TTaskbarOverlayIcon.Destroy;
begin
  FOnChanged := nil;
  FOnChanging := nil;
  FOwner := nil;
  inherited Destroy;
end;

procedure TTaskbarOverlayIcon.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTaskbarOverlayIcon.DoChanging;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TTaskbarOverlayIcon.SetDescription(const Value: WideString);
begin
  if FDescription = Value then
    Exit;
  FDescription := Value;
  Update;
end;

procedure TTaskbarOverlayIcon.SetIcon(const Value: hIcon);
begin
  if FIcon = Value then
    Exit;
  FIcon := Value;
  Update;
end;

procedure TTaskbarOverlayIcon.Update;
begin
  FOwner.UpdateOverlayIcon;
end;

{ TWin7Taskbar }
procedure TWin7Taskbar.AppendButton;
var
  AButtons: array [0 .. MAX_BUTTON - 1] of TThumbButton;
  AButton: TTaskbarThumbButton;
  Loop: Integer;
begin
  if not(FCanCreate and Assigned(FTaskbarList)) then
    Exit;
  // 按钮集合对象事件调用, 通知正在(开始)改变
  FButtons.DoChanging;
  FillChar(AButtons, SizeOf(AButtons), 0);
  for Loop := 0 to MAX_BUTTON - 1 do
  begin
    // 先循环获取按钮对象, 然后通知每个对象开始改变(调用OnChanging事件)
    AButton := FButtons.Button[Loop];
    AButton.DoChanging;
    // 然后根据对象的属性填充windows指定的数据结构的每个域的值
    AButtons[Loop].dwMask := BitmapMask[AButton.FImageIndex <> -1] or IconMask
      [AButton.FIcon <> 0] or HintMask[AButton.ShowHint] or THB_FLAGS;
    AButtons[Loop].iId := AButton.FCommandId;
    AButtons[Loop].iBitmap := AButton.FImageIndex;
    AButtons[Loop].hIcon := AButton.FIcon;
    CopyMemory(@AButtons[Loop].szTip[0], PWideChar(AButton.FHint),
      Length(AButton.FHint) * SizeOf(WideChar));
    AButtons[Loop].dwFlags := VisibleFlag[AButton.Visible] or EnabledFlag
      [AButton.Enabled] or TransparentFlag[AButton.Transparent];
  end;
  // 通过ITaskbarList4接口对象添加按钮
  FTaskbarList.ThumbBarAddButtons(FMainWindow, MAX_BUTTON, @AButtons[0]);
  // 添加完毕后, 循环通知对应按钮对象改变结束
  for Loop := MAX_BUTTON - 1 downto 0 do
    FButtons.Button[Loop].DoChanged;
  // 按钮集合对象事件调用, 通知改变结束
  FButtons.DoChanged;
end;

constructor TWin7Taskbar.Create(AMainWindow: HWND);
begin
  inherited Create;
  FCanCreate := IsVistaLater;
  if not FCanCreate then
    Exit;
  // 以多线程的方式初始化com组件库
  CoInitializeEx(nil, COINIT_MULTITHREADED);
  // windows系统创建任务栏图标时发送过来的消息
  // 应用程序必须在接到这个消息后, 再去调用TaskbarList的函数才会有效果,
  // 所以这里不立即创建com对象, 以避免重复执行自定义对象的事件(OnChanging和OnChanged)
  FCreateButtonMessage := RegisterWindowMessageW('TaskbarButtonCreated');
  // 取程序主窗口,注意d2007以前一定是application的句柄,
  if AMainWindow = 0 then
    FMainWindow := GetMainWindow
  else
    FMainWindow := AMainWindow;
  // 这个函数是创建一个类中方法的通用回调thunk, 源码来自网络(武希松的blog)
  // 在32bit编译下无异常
  FNewWndProc := CreateThunk(Self, @TWin7Taskbar.NewWndProc);
  // 把这个创建出来的thunk作为新的窗口消息处理过程注册给目标窗口, 用来替换目标窗口的消息过程,
  // 这样才能处理任务栏图标被创建的消息, 以及新增加的按钮的通告消息
  FOldWndProc := Pointer(SetWindowLongW(FMainWindow, GWL_WNDPROC,
      DWORD(FNewWndProc)));
  // 创建相关对象, 目前只有按钮, 进度条, 小图标
  FButtons := TTaskbarThumbButtons.Create(Self);
  FProgress := TTaskbarProgress.Create(Self);
  FOverlayIcon := TTaskbarOverlayIcon.Create(Self);
end;

destructor TWin7Taskbar.Destroy;
begin
  // 释放相关对象
  FOverlayIcon.Free;
  FProgress.Free;
  FButtons.Free;
  // 还原原窗口处理过程
  SetWindowLongW(FMainWindow, GWL_WNDPROC, DWORD(FOldWndProc));
  // 释放构造出来的thunk
  ReleaseThunk(FNewWndProc);
  // 释放com对象
  FTaskbarList := nil;
  // 回收com组件库资源
  CoUninitialize;
  inherited Destroy;
end;

function TWin7Taskbar.NewWndProc(HWND: HWND; uMsg: UINT; wParam: wParam;
  lParam: lParam): LRESULT;
var
  Button: TTaskbarThumbButton;
begin
  if not FCanCreate then
    // 不是win7或以后的系统, 则调用原来的窗口过程处理消息
    Result := CallWindowProcW(FOldWndProc, HWND, uMsg, wParam, lParam)
  else
  begin
    Result := 0;
    // 系统创建任务栏按钮时会发送这个消息给窗口, 必须在这里对TaskbarList操作才会有效果
    if uMsg = FCreateButtonMessage then
    begin
      Refresh;
      FButtons.DoButtonCreated;
    end
    else // 其他消息
    begin
      // 通告消息, 新添加到预览窗口上的按钮被单击后, 主窗口会接到一个通告消息
      if uMsg = WM_COMMAND then
      begin
        // wParam高位为THBN_CLICKED的为新按钮被单击,
        if HiWord(wParam) = THBN_CLICKED then
        begin
          // 低位为按钮id
          Button := FButtons.FindByCommandId(LoWord(wParam));
          if Button <> nil then
            Button.Click;
        end;
      end;
      // 调用原来的窗口过程处理其他消息
      Result := CallWindowProcW(FOldWndProc, HWND, uMsg, wParam, lParam);
    end;
  end;
end;

procedure TWin7Taskbar.UpdateButton(Index: Integer);
var
  AButton: TThumbButton;
  Button: TTaskbarThumbButton;
begin
  if not(FCanCreate and Assigned(FTaskbarList)) then
    Exit;
  Button := FButtons.Button[Index];
  if Button = nil then
    Exit;
  // 按钮对象事件调用, 通知要改变的按钮正在(开始)改变
  Button.DoChanging;
  FillChar(AButton, SizeOf(AButton), 0);
  AButton.dwMask := THB_FLAGS or BitmapMask[Button.FImageIndex <> -1]
    or IconMask[Button.FIcon <> 0] or HintMask[Button.FShowHint];
  AButton.iId := Button.FCommandId;
  AButton.iBitmap := Button.FImageIndex;
  AButton.hIcon := Button.FIcon;
  CopyMemory(@AButton.szTip[0], PWideChar(Button.FHint),
    Length(Button.FHint) * SizeOf(WideChar));
  AButton.dwFlags := VisibleFlag[Button.Visible] or EnabledFlag[Button.Enabled]
    or TransparentFlag[Button.Transparent];
  FTaskbarList.ThumbBarUpdateButtons(FMainWindow, Index + 1, @AButton);
  // 按钮对象事件调用, 通知要改变的按钮改变结束
  Button.DoChanged;
end;

procedure TWin7Taskbar.UpdateOverlayIcon;
begin
  if not(FCanCreate and Assigned(FTaskbarList)) then
    Exit;
  // 通知任务栏小图标对象正在(开始)改变
  FOverlayIcon.DoChanging;
  FTaskbarList.SetOverlayIcon(FMainWindow, FOverlayIcon.FIcon,
    PWideChar(FOverlayIcon.FDescription));
  // 通知任务栏小图标对象改变结束
  FOverlayIcon.DoChanged;
end;

procedure TWin7Taskbar.UpdateProgress;
var
  AMax, APos: UINT64;
  AFlags: UINT;
begin
  if not(FCanCreate and Assigned(FTaskbarList)) then
    Exit;
  // 进度条对象事件调用, 通知对象正在(开始)改变
  FProgress.DoChanging;
  AMax := FProgress.FMax - FProgress.FMin;
  APos := FProgress.FPosition - FProgress.FMin;
  case FProgress.FState of
    psNoProgress:
      AFlags := TBPF_NOPROGRESS;
    psIndeterminate:
      AFlags := TBPF_INDETERMINATE;
    psNormal:
      AFlags := TBPF_NORMAL;
    psError:
      AFlags := TBPF_ERROR;
    psPaused:
      AFlags := TBPF_PAUSED;
  else
    AFlags := TBPF_NOPROGRESS;
  end;
  FTaskbarList.SetProgressState(FMainWindow, AFlags);
  if FProgress.FState <> psIndeterminate then
    // 当进度条样式为TBPF_INDETERMINATE时,不需要设置进度值,否则效果就和TBPF_NORMAL一样了
    FTaskbarList.SetProgressValue(FMainWindow, APos, AMax);
  // 进度条对象事件调用, 通知对象改变结束
  FProgress.DoChanged;
end;

procedure TWin7Taskbar.UpdateImageList;
begin
  if not(FCanCreate and Assigned(FTaskbarList)) then
    Exit;
  // 按钮集合对象事件调用, 通知正在(开始)改变
  FButtons.DoChanging;
  FTaskbarList.ThumbBarSetImageList(FMainWindow, FButtons.ImageList);
  // 按钮集合对象事件调用, 通知改变结束
  FButtons.DoChanged;
end;

procedure TWin7Taskbar.Refresh;
begin
  if not FCanCreate then
    Exit;
  if not Assigned(FTaskbarList) then
  // 创建com对象
  begin
    CoCreateInstance(CLSID_TaskbarList, nil, CLSCTX_ALL, IID_ITaskbarList4,
      FTaskbarList);
    FTaskbarList.HrInit;
  end;
  AppendButton;
  UpdateImageList;
  FButtons.UpdateVisible;
  UpdateProgress;
  UpdateOverlayIcon;
end;

end.


