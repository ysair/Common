unit fcDialog;

interface

uses
  Classes, Forms, StdCtrls, ExtCtrls, Controls, Windows, Messages;

type
  //¶Ô»°¿ò
  Dialog = record
    class function MsgBox(
        const AHandle : HWND;
        const AText : string;
        const ACaption : string;
        const AFlag : UINT
        ):integer; overload; static;
    class function  MsgBox(const AText : string; AFlag : Integer):integer; overload; static;
    class function  MsgBox(const AText : string):integer; overload; static;
    class function RadioSelect(
        const AText : string;
        const ACaption : string;
        const ARadioList : TStrings;
        var   AIndex : Integer
        ): TModalResult; static;
  end;

implementation

uses
  uCFResource;

{ Dialog }

class function Dialog.RadioSelect(const AText, ACaption: string;
  const ARadioList: TStrings; var AIndex: Integer): TModalResult;
var
  Form : TForm;
  rg :  TRadioGroup;
begin
  Form  :=  TForm.Create(Screen.ActiveForm);
  try
    Form.Caption  :=  ACaption;
    Form.BorderStyle  :=  bsDialog;
    Form.Position :=  poOwnerFormCenter;
    Form.ClientWidth  :=  300;
    if Assigned(Screen.ActiveForm) then
      Form.Font :=  Screen.ActiveForm.Font;

    rg  :=  TRadioGroup.Create(Form);
    rg.Parent :=  Form;
    rg.Left :=  10;
    rg.Width  :=  Form.ClientWidth - 20;
    rg.Top  :=  10;
    rg.Caption  :=  AText;
    rg.Items.Assign(ARadioList);
    rg.Height :=  20 * rg.Items.Count + 20;
    if (AIndex >= 0) and (AIndex < rg.Items.Count) then
      rg.ItemIndex  :=  AIndex
    else if rg.Items.Count > 0 then
      rg.ItemIndex  :=  0;

    with TButton.Create(Form) do
    begin
      Parent  :=  Form;
      ModalResult :=  mrCancel;
      SetBounds(Form.ClientWidth - 90, rg.Top + rg.Height + 10, 80, 25);
      Caption :=  SCancel;
      Cancel  :=  True;
    end;
    with TButton.Create(Form) do
    begin
      Parent  :=  Form;
      ModalResult :=  mrOK;
      SetBounds(Form.ClientWidth - 180, rg.Top + rg.Height + 10, 80, 25);
      Caption :=  SOK;
      Default :=  True;
      Form.ClientHeight :=  Top + Height + 10;
    end;

    Result  :=  Form.ShowModal;
    AIndex  :=  rg.ItemIndex;
  finally
    Form.Free;
  end;
end;

class function Dialog.MsgBox(const AHandle: HWND; const AText, ACaption: string;
  const AFlag: UINT): integer;
var
  c : string;
  f : Integer;
begin
  c :=  ACaption;
  f :=  AFlag;

  if f and MB_YESNO = MB_YESNO then
    f :=  f or MB_ICONQUESTION;

  if f and MB_YESNOCANCEL = MB_YESNOCANCEL then
    f :=  f or MB_ICONQUESTION;

  if f and MB_ICONMASK = 0 then
    f :=  f or MB_ICONINFORMATION;

  if c = '' then
  begin
    if MB_ICONERROR and f = MB_ICONERROR then
      c :=  SError
    else if MB_ICONQUESTION and f = MB_ICONQUESTION then
      c :=  SConfirm
    else if MB_ICONWARNING and f = MB_ICONWARNING then
      c :=  SWarning
    else begin
      f :=  f or MB_ICONINFORMATION;
      c :=  SInformation;
    end;
  end;

  Result  :=  MessageBox(AHandle, PChar(AText), PChar(c), f);
end;

class function Dialog.MsgBox(const AText: string; AFlag: Integer): integer;
var
  h : HWND;
begin
  if Assigned(Screen.ActiveForm) then
    h :=  Screen.ActiveForm.Handle
  else
    h :=  0;
  Result  :=  MsgBox(h, AText, '', AFlag);
end;

class function Dialog.MsgBox(const AText: string): integer;
begin
  Result  :=  MsgBox(AText, 0);
end;

end.
