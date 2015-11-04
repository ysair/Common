unit uMouseWheelPlus;
(*******************************************************************************
鼠标钩子,实现将鼠标滚轮消息发往鼠标指向的控件,而不是获得焦点的控件
注意,一些控件收到滚动消息还会把消息转发到它的父窗口,解决方案:
  覆盖 MouseWheelHandler 方法,并实现为:
  message.result  :=  Perform(CM_MOUSEWHEEL, Message.WParam, Message.lParam);

例如让DevExpress的控件支持这个单元,需要修改
cxControls单元,TcxControl类
覆盖 MouseWheelHandler 方法
  public
    procedure MouseWheelHandler(var Message: TMessage); override;
实现 MouseWheelHandler, 不要inherited
  procedure TcxControl.MouseWheelHandler(var Message: TMessage);
  begin
    message.result  :=  Perform(CM_MOUSEWHEEL, Message.WParam, Message.lParam);
  end;
新版本的DevExpress已经没有覆盖MouseWheelHandler过程了,需要手工加上
//*****************************************************************************)

interface

uses
  Windows, Messages;

procedure InstallHook;
procedure UninstallHook;

implementation

type
  PMouseHookStructEx = ^TMouseHookStructEx;
  TMouseHookStructEx = record
    Base : TMouseHookStruct;
    mouseData : DWORD;
  end;

var
  hook: HHOOK;

function MouseHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  pt : TPoint;
  h : HWND;
  ms  : PMouseHookStructEx;
  mw  : TWMMouseWheel;
  pwind : HWND;
begin
  if wParam = WM_MOUSEWHEEL then
  begin
    GetCursorPos(pt);
    h :=  WindowFromPoint(pt);
    ms  :=  Pointer(lParam);
    pwind :=  h;
    while GetParent(pwind) > 0 do
      pwind :=  GetParent(pwind);
    if (ms.Base.hwnd <> h) and (GetActiveWindow = pwind) then
    begin
      mw.WheelDelta :=  HIWORD(ms.mouseData);
      if (mw.WheelDelta mod 120 <> 0) or (mw.WheelDelta > 1000) then
      begin //处理64位WIN7下的问题
        UninstallHook;
        InstallHook;
        Result := -1;
        Exit;
      end;
      mw.Msg  :=  WM_MOUSEWHEEL;
      mw.Keys :=  0;
      mw.XPos :=  ms.Base.pt.X;
      mw.YPos :=  ms.Base.pt.Y;//}
      PostMessage(h, mw.Msg, TMessage(mw).wParam, TMessage(mw).lParam);
      Result  :=  -1;
    end
    else
      Result := CallNextHookEx(hook, nCode, wParam, lParam);
  end
  else
    Result := CallNextHookEx(hook, nCode, wParam, lParam);
end;

procedure InstallHook;
begin
  hook := SetWindowsHookEx(WH_MOUSE, Addr(MouseHook), HInstance, GetCurrentThreadId);
end;

procedure UninstallHook;
begin
  if hook > 0 then
    UnhookWindowsHookEx(hook);
end;

initialization
  InstallHook;

finalization
  UninstallHook;
  
end.
