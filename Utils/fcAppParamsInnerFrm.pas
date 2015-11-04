unit fcAppParamsInnerFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Generics.Collections;

const
  UM_App_Noop       = WM_APP  + $221;
  APP_COPY_PARAM    = $1001;
  DEF_MSG_TIMEOUT   = 1000;

type
  TfcAppParamsInnerForm = class(TForm)
    tmrDispatchParams: TTimer;
    procedure tmrDispatchParamsTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FOnDispatchParams: TGetStrProc;
    FApplicationID: string;
    FCurrentAppParamsList: TStringList;       //当前应用程序的参数
    FToDoParamsList: TList<TStringList>;      //需要用Timer处理的命令队列
    FExitApplicationOnClose: Boolean;
    FActiveFormClass: string;
    FActiveFormCaption: string;

    function  GetAppInstanceExists: Boolean;
    function  GetAppInstanceHandle: THandle;
    function  GetAToDoParams(AList: TStringList): Boolean;
    function  GetAppCaption: string;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure DoClose(var Action: TCloseAction); override;
  public
    property AppInstanceHandle : THandle read GetAppInstanceHandle;
    property AppInstanceExists : Boolean read GetAppInstanceExists;
    property AppCaption : string read GetAppCaption;
    property ApplicationID: string read FApplicationID write FApplicationID;
    property OnDispatchParams : TGetStrProc read FOnDispatchParams write FOnDispatchParams;
    property ExitApplicationOnClose : Boolean read FExitApplicationOnClose write FExitApplicationOnClose;
    property ActiveFormClass : string read FActiveFormClass write FActiveFormClass;
    property ActiveFormCaption : string read FActiveFormCaption write FActiveFormCaption;

    procedure InitialCurrApplication;
    procedure SendParamsToExistsInstance;
    procedure DispatchCurrentAppParams;
    procedure DispatchParamsText(const AParamsText : string);
    procedure ActiveExistsInstance;
  end;

var
  V_AppParamsInnerForm: TfcAppParamsInnerForm;

implementation

{$R *.dfm}

uses
  fcWindows;

{ TfcAppParamsInnerForm }

procedure TfcAppParamsInnerForm.ActiveExistsInstance;
var
  h : HWND;
begin
  PostMessage(AppInstanceHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
  if ActiveFormClass <> '' then
    if ActiveFormCaption <> '' then
      h :=  FindWindow(PChar(ActiveFormClass), PChar(ActiveFormCaption))
    else
      h :=  FindWindow(PChar(ActiveFormClass), nil)
  else
    h :=  AppInstanceHandle;
  Win.ForegroundWindow(h);
end;

procedure TfcAppParamsInnerForm.DispatchCurrentAppParams;
var
  l_List: TStringList;
begin
  if FCurrentAppParamsList.Count >0 then
  begin
    l_List := TStringList.Create;
    l_List.Assign(FCurrentAppParamsList);
    FToDoParamsList.Add(l_List);
  end;
end;

procedure TfcAppParamsInnerForm.DispatchParamsText(const AParamsText: string);
var
  l_List: TStringList;
begin
  if Trim(AParamsText)<>''then
  begin
    l_List := TStringList.Create;
    l_List.Text := AParamsText;
    FToDoParamsList.Add(l_List);
  end;
end;

procedure TfcAppParamsInnerForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  if (Action = caFree) and ExitApplicationOnClose then
    if Assigned(Application.MainForm) then
      PostMessage(Application.MainForm.Handle, WM_CLOSE, 0, 0);
end;

procedure TfcAppParamsInnerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action  :=  caFree;
end;

procedure TfcAppParamsInnerForm.FormCreate(Sender: TObject);
  procedure _InitCurrentAppParams;
  var
    I: Integer;
  begin
    FCurrentAppParamsList.Clear;
    for I := 1 to ParamCount do
      FCurrentAppParamsList.Add(ParamStr(I));
  end;
begin
  FToDoParamsList :=  TList<TStringList>.Create;
  FCurrentAppParamsList := TStringList.Create;
  _InitCurrentAppParams;
end;

procedure TfcAppParamsInnerForm.FormDestroy(Sender: TObject);
begin
  while GetAToDoParams(nil) do ;      //清除所有的未处理的命令行参数
  FCurrentAppParamsList.Free;
  FToDoParamsList.Free;
end;

function TfcAppParamsInnerForm.GetAppCaption: string;
begin
  Result  :=  FApplicationID;
end;

function TfcAppParamsInnerForm.GetAppInstanceExists: Boolean;
var
  hr :  Cardinal;
begin
  Result  :=  AppInstanceHandle > 0;
  if Result then
    if SendMessageTimeOut(AppInstanceHandle, UM_App_Noop, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, DEF_MSG_TIMEOUT, hr) = 0 then
      Result  :=  False
    else
      Result  :=  hr = UM_App_Noop;
end;

function TfcAppParamsInnerForm.GetAppInstanceHandle: THandle;
begin
  Result := FindWindow(PChar(Self.ClassName), PChar(AppCaption));
end;

function TfcAppParamsInnerForm.GetAToDoParams(AList: TStringList): Boolean;
begin
  Result := FToDoParamsList.Count >0;
  if Result then
  begin
    if Assigned(AList) then
      AList.Text := FToDoParamsList.Items[0].Text;
    FToDoParamsList.Items[0].Free;
    FToDoParamsList.Delete(0);
  end;
end;

procedure TfcAppParamsInnerForm.InitialCurrApplication;
begin
  Self.Caption := AppCaption;
end;

procedure TfcAppParamsInnerForm.SendParamsToExistsInstance;
var
  s : string;
  cd : TCopyDataStruct;
  hr :  Cardinal;
begin
  s :=  Trim(FCurrentAppParamsList.Text);
  if s = '' then Exit;
  ZeroMemory(@cd, SizeOf(cd));
  cd.dwData :=  APP_COPY_PARAM;
  cd.cbData :=  Length(s) * SizeOf(Char);
  GetMem(cd.lpData, cd.cbData);
  ZeroMemory(cd.lpData, cd.cbData);
  CopyMemory(cd.lpData, @s[1], cd.cbData);
  SendMessageTimeOut(AppInstanceHandle, WM_COPYDATA, 0, Integer(@cd), SMTO_BLOCK or SMTO_ABORTIFHUNG, DEF_MSG_TIMEOUT, hr);
end;

procedure TfcAppParamsInnerForm.tmrDispatchParamsTimer(Sender: TObject);
var
  l_List: TStringList;
begin
  if (FToDoParamsList.Count > 0) and Assigned(FOnDispatchParams) then
  begin
    l_List := TStringList.Create;
    try
      if GetAToDoParams(l_List) then
        FOnDispatchParams(l_List.Text);
    finally
      l_List.Free;
    end;
  end;
end;

procedure TfcAppParamsInnerForm.WndProc(var Message: TMessage);

  procedure _DispatchCopyDataMessage(const ACopyDataStruct: PCopyDataStruct);
  var
    s : string;
  begin
    case ACopyDataStruct.dwData of
      APP_COPY_PARAM  :
        begin
          SetLength(s, ACopyDataStruct^.cbData div 2);
          ZeroMemory(@s[1], Length(s) * SizeOf(Char));
          CopyMemory(@s[1], ACopyDataStruct^.lpData, ACopyDataStruct^.cbData);
          DispatchParamsText(s);
        end;
    end;
  end;

begin
  case Message.Msg of
    WM_SYSCOMMAND :
      if (Message.wParam = SC_RESTORE) and Assigned(Screen.ActiveForm) then
      begin
        if Screen.ActiveForm.WindowState = wsMinimized then
          PostMessage(Screen.ActiveForm.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
          //Screen.ActiveForm.WindowState :=  wsNormal;
        Win.ForegroundWindow(Screen.ActiveForm.Handle);
      end;
    WM_COPYDATA :
      _DispatchCopyDataMessage(Pointer(Message.lParam));
    UM_App_Noop :
      Message.Result  :=  UM_App_Noop;
    else
      inherited;
  end;
end;

end.
