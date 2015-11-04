unit uSimpleApp;

interface

uses
  Classes, Windows, SysUtils, Messages;

type
  TMessageEvent = procedure (var Msg: TMsg; var Handled: Boolean) of object;

  TSimpleApp = class
  private
    FTerminate  : Boolean;
    FOnMessage: TMessageEvent;
  private
    procedure HandleMessage;
    function ProcessMessage(var Msg: TMsg): Boolean;
  protected
    procedure Idle(const Msg: TMsg);
  public
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property Terminated: Boolean read FTerminate;
  public
    procedure Run; virtual;
  end;

implementation

{ TSimpleApp }

procedure TSimpleApp.HandleMessage;
var
  Msg: TMsg;
begin
  if not ProcessMessage(Msg) then Idle(Msg);
end;

procedure TSimpleApp.Idle(const Msg: TMsg);
var
  Done: Boolean;
begin
  Done := True;
  if (GetCurrentThreadID = MainThreadID) and CheckSynchronize then
    Done := False;
  if Done then WaitMessage;
end;

function TSimpleApp.ProcessMessage(var Msg: TMsg): Boolean;
var
  Handled: Boolean;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
      Handled := False;
      if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
      if not Handled then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end
    else
      FTerminate := True;
  end;
end;

procedure TSimpleApp.Run;
begin
  repeat
    HandleMessage;
  until Terminated;
end;

end.
