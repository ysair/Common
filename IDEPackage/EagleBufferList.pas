unit EagleBufferList;

interface

procedure Register;

implementation

uses Windows, Classes, SysUtils, ToolsAPI, Menus;

type
  TBufferList = class(TNotifierObject, IUnknown, IOTANotifier,
    IOTAKeyboardBinding)
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    // Ö¸¶¨¿ì½Ý¼ü
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  protected
    procedure CodeCompletion(const Context: IOTAKeyContext; KeyCode: TShortcut;
      var BindingResult: TKeyBindingResult);
  end;

resourcestring
  sBufferList = 'Eagle''s Buffer List';

// register this key binding
procedure Register;
begin
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TBufferList.Create);
end;

{ TBufferList }

// the code to bind key
procedure TBufferList.BindKeyboard
  (const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('J'), [ssCtrl])], CodeCompletion,
    Pointer(csCodeList or csManual));
  BindingServices.AddKeyBinding([ShortCut(Ord('L'), [ssCtrl])], CodeCompletion,
    Pointer(csParamList));
end;

// do code completion
procedure TBufferList.CodeCompletion(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  (Context.EditBuffer.TopView as IOTAEditActions).CodeCompletion(Byte(Context.Context));
  BindingResult := krHandled;
end;

function TBufferList.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TBufferList.GetDisplayName: string;
begin
  Result := sBufferList;
end;

function TBufferList.GetName: string;
begin
  Result := 'EagleKing.BufferList'; // do not localize
end;

end.
