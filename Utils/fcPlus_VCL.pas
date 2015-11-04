// Common Framework
// class helper单元
{$I CommonFramework.inc}
unit fcPlus_VCL;

interface

uses
  Windows, Classes, Controls, Variants, Forms, TypInfo, SysUtils, ActnList, IniFiles,
  ExtCtrls, GIFImg, jpeg, pngimage, Graphics
  {$IFDEF FRAMEWORK_INCLUDE_DB}, DB, DBClient{$ENDIF}
  ;

type
  TComponentHelper = class helper for TComponent
  public
    function GetAbsoluteName : string;
    function GetAbsoluteClassName : string;
  end;

  TFormHelper = class helper for TForm
  public
    class function ShowDialog(AOwner: TComponent; const AShowOnTaskbarIcon : Boolean = False): TModalResult;
    class function CreateIntoControl(AOwner: TWinControl): TForm;
    class function CreateSingleton(AOwner: TComponent): TForm;
    class function GetInstance: TForm;
    class function CreateSingletonIntoControl(AOwner: TWinControl): TForm;
    class function BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam): Integer;
    procedure ShowOnTaskbarIcon;
    procedure FillActionHints; //将action空的hint用Caption填充
  end;

  TScreenHelper = class helper for TScreen
    function BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam): Integer; overload;
    function BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam; AExcludeHandles : array of THandle): Integer; overload;
    function FindForm(const AClass : TFormClass):TForm;
  end;

  {$IFDEF FRAMEWORK_INCLUDE_DB}
  TDataSetHelper = class helper for TDataSet
  public
    function ApplyFilter(const AFilterStr: string): Boolean;
    procedure CopyRecordFrom(const ADataSet: TDataSet; const APost: Boolean = False);
    procedure CopyDataSetFrom(const ADataSet: TDataSet);
    procedure RecordReadFrom(const AValue: Variant; const APost: Boolean = False);
    procedure RecordWriteTo(var AValue: Variant);
    procedure RecordWriteToControls(const AControl: TWinControl);
    function JoinFieldValue(const AFieldName: string; const ASeparator: string = ',';
      const AQuoted: Boolean = False): string;
  end;

  TClientDataSetHelper = class helper for TClientDataSet
  public
    procedure SetAllFieldCanEdit;
  end;
  {$ENDIF}

  { TPersistentHelper = class helper for TPersistent
    private
    function PropertyReadFromControlByTag(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyWriteToControlByTag(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyReadFromControl(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyWriteToControl(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyIsModifiedInControl(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    function PropertyIsModifiedInControlByTag(const AControl: TControl; const APropInfo: PPropInfo): Boolean;
    public
    function PropertiesReadFromControls(const AControl: TWinControl; const AByTagToo: Boolean = False): Boolean;
    function PropertiesWriteToControls(const AControl: TWinControl; const AByTagToo: Boolean = False): Boolean;
    function PropertiesIsModifiedInControls(const AControl: TWinControl; const AByTagToo: Boolean = False): Boolean;
    end;// }

  TControlHelper = class helper for TControl
  public
    function MouseInControl: Boolean;
    function GetParentForm(const ATopForm : Boolean = False):TForm;
  end; // }

  TWinControlHelper = class helper for TWinControl
  public
    procedure SetChildControlsEnabled(const AEnabled: Boolean);
    procedure SetParentControlsEnabled(const AEnabled: Boolean);
    procedure AutoSetTabOrder(const ATraverse : Boolean = True);
  end;

  TImageHelper = class helper for TImage
  private
    class var FLastFileName: string;
    class var FLastResourceName: string;
  public
    class property LastFileName: string read FLastFileName write FLastFileName;
    class property LastResourceName: string read FLastResourceName write FLastResourceName;

    // 加载一个图片文件到TImage， 加载的时候，会读取图片文件的前几个字节，判断图片的格式然后再加载。
    // AIgnoreIfSameFileName: TRUE:如果本次加载的文件名和上次一样，则不重新加载（不会刷新）
    function LoadFromFileExFc(AFileName: String; const AIgnoreIfSameFileName: Boolean = True): Boolean;

    // 从一个资源加载..
    function LoadFromResourceExFc(AHInst: Cardinal; AResName: string;
      const AIgnoreIfSameResName: Boolean = True; const ATransparent: Boolean = True;
      const APlayGif: Boolean = True): Boolean;

    // 从一个资源加载..
    function LoadFromStreamExFc(AStream : TStream;
      const ATransparent: Boolean = True;
      const APlayGif: Boolean = True): Boolean;
  end;

  TMemoryStreamHelper = class helper for TMemoryStream
  public
    function ToBytes : TBytes;
  end;

  TIniFileHelper = class helper for TIniFile
  public
    function ReadLongString(const Section, Ident, Default: string): string;
  end;

implementation

uses
  fcRTTI, fcImage, fcWindows;

const
  ControlRttiPropList: array [0 .. 4] of string = ('Value', 'Date', 'ItemIndex', 'Checked', 'Text');

{ TComponentHelper }

function TComponentHelper.GetAbsoluteClassName: string;
begin
  Result  :=  ClassName;
  if Assigned(Owner) then
    Result  :=  Owner.GetAbsoluteClassName + '.' + Result;
end;

function TComponentHelper.GetAbsoluteName: string;
begin
  Result  :=  Name;
  if Assigned(Owner) then
    Result  :=  Owner.GetAbsoluteName + '.' + Result;
end;

  { TFormHelper }

procedure TFormHelper.FillActionHints;

  function _GetHint(const ACaption : string):string;
  var // caption(&c) -> caption    c&aption -> caption
    p1, p2 : Integer;
  begin
    Result  :=  ACaption;
    p1  :=  Pos('&', Result);
    if p1 > 0 then
      Delete(Result, p1, 1);
    p1  :=  Pos('(', Result);
    p2  :=  Pos(')', Result);
    if (p1 > 0) and (p2 > p1) then
      Delete(Result, p1, p2-p1+1);
  end;

var
  i : Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TAction then
      if TAction(Components[i]).Hint = '' then
        TAction(Components[i]).Hint  :=  _GetHint(TAction(Components[i]).Caption);
end;

class function TFormHelper.BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is Self then
    begin
      PostMessage(Screen.Forms[i].Handle, Msg, wParam, lParam);
      Inc(Result);
    end;
end;

class function TFormHelper.CreateIntoControl(AOwner: TWinControl): TForm;
begin
  Result := Create(AOwner);
  Result.Parent := AOwner;
  Result.BorderStyle := bsNone;
  Result.Align := alClient;
  Result.Show;
end;

class function TFormHelper.CreateSingleton(AOwner: TComponent): TForm;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    if SameText(Screen.Forms[i].ClassName, ClassName) then
    begin
      Result := TForm(Screen.Forms[i]);
      Exit;
    end;
  Result := Create(AOwner);
end;

class function TFormHelper.CreateSingletonIntoControl(AOwner: TWinControl): TForm;
var
  i: Integer;
begin
  for i := 0 to AOwner.ControlCount - 1 do
    if SameText(AOwner.Controls[i].ClassName, ClassName) then
    begin
      Result := TForm(AOwner.Controls[i]);
      Result.Show;
      Exit;
    end;
  Result := CreateIntoControl(AOwner);
end;

class function TFormHelper.GetInstance: TForm;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
    if SameText(Screen.Forms[i].ClassName, ClassName) then
    begin
      Result := TForm(Screen.Forms[i]);
      Exit;
    end;
  Result := nil;
end;

class function TFormHelper.ShowDialog(AOwner: TComponent;
    const AShowOnTaskbarIcon : Boolean): TModalResult;
begin
  with Self.Create(AOwner) do
  try
    // BorderStyle := bsDialog;
    Position := poOwnerFormCenter;
    if (AOwner = nil) or (AOwner = Application) then
      Position := poDesktopCenter;
    if AShowOnTaskbarIcon then
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_APPWINDOW);
    Result := ShowModal;
  finally
    Free;
  end;
end;

procedure TFormHelper.ShowOnTaskbarIcon;
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  Show;
  Win.ForegroundWindow(Handle);
end;

{ TScreenHelper }

function TScreenHelper.BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam): Integer;
var // 非线程安全,线程中请使用 CFFunc.Win.BroadcastMessage
  i: Integer;
begin
  Result := 0;
  for i := 0 to Screen.FormCount - 1 do
  begin
    PostMessage(Screen.Forms[i].Handle, Msg, wParam, lParam);
    Inc(Result);
  end;
end;

function TScreenHelper.BroadcastMessage(Msg: UINT; wParam: wParam; lParam: lParam; AExcludeHandles: array of THandle): Integer;
var // 非线程安全,线程中请使用 CFFunc.Win.BroadcastMessage
  i: Integer;
  j : Integer;
  exists : Boolean;
begin
  Result := 0;
  for i := 0 to Screen.FormCount - 1 do
  begin
    exists  :=  False;
    for j := Low(AExcludeHandles) to High(AExcludeHandles) do
      if Screen.Forms[i].Handle = AExcludeHandles[j] then
      begin
        exists  :=  True;
        break;
      end;
    if not exists then
    begin
      PostMessage(Screen.Forms[i].Handle, Msg, wParam, lParam);
      Inc(Result);
    end;
  end;
end;

{$IFDEF FRAMEWORK_INCLUDE_DB}
{ TDataSetHelper }

function TDataSetHelper.ApplyFilter(const AFilterStr: string): Boolean;
begin
  Result := False;
  if Active then
  try
    Filter := AFilterStr;
    Filtered := True;
    Result := True;
  except
  end;
end;

procedure TDataSetHelper.CopyRecordFrom(const ADataSet: TDataSet; const APost: Boolean = False);
var
  field: TField;
  sourceField: TField;
begin
  if IsEmpty or ADataSet.IsEmpty then
    Exit;

  if not(State in dsEditModes) then
    Edit;

  for field in Fields do
  begin
    if not field.ReadOnly and (field.FieldKind = fkData) then
    begin
      sourceField := ADataSet.FindField(field.FieldName);
      if sourceField <> nil then
      begin
        field.Value := sourceField.Value;
      end;
    end;
  end;

  if APost and (State in dsEditModes) then
    Post;
end;

procedure TDataSetHelper.CopyDataSetFrom(const ADataSet: TDataSet);
var
  i: Integer;
  bk: TBookmark;
  fd: TField;
begin
  if ADataSet.IsEmpty then
    Exit;
  ADataSet.DisableControls;
  bk := ADataSet.GetBookmark;
  try
    ADataSet.First;
    while not ADataSet.Eof do
      try
        Append;
        for i := 0 to FieldCount - 1 do
          try
            fd := ADataSet.FindField(Fields[i].FieldName);
            if Assigned(fd) then
              Fields[i].Value := fd.Value;
          except
          end;
        Post;
      finally
        ADataSet.Next;
      end;
  finally
    ADataSet.GotoBookmark(bk);
    ADataSet.EnableControls;
  end;
end;

procedure TDataSetHelper.RecordReadFrom(const AValue: Variant; const APost: Boolean);
var
  i: Integer;
  j: Integer;
begin
  if IsEmpty or not(State in dsEditModes) then
    Exit;
  if not VarIsArray(AValue) then
    Exit;
  if VarArrayHighBound(AValue, 2) - VarArrayLowBound(AValue, 2) < 1 then
    Exit;

  for i := 0 to FieldCount - 1 do
    for j := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
      if SameText(Fields[i].FieldName, VarToStr(AValue[j, 0])) then
        try
          Fields[i].Value := AValue[j, 1];
        except
        end;
  if APost and (State in dsEditModes) then
    Post;
end;

procedure TDataSetHelper.RecordWriteTo(var AValue: Variant);
var
  i: Integer;
begin
  if IsEmpty then
    Exit;

  AValue := VarArrayCreate([0, Fields.Count - 1, 0, 1], varVariant);
  for i := 0 to Fields.Count - 1 do
  begin
    AValue[i, 0] := Fields[i].FieldName;
    AValue[i, 1] := Fields[i].Value;
  end;
end;

procedure TDataSetHelper.RecordWriteToControls(const AControl: TWinControl);
var
  i: Integer;
  j: Integer;
begin
  if IsEmpty then
    Exit;
  for i := 0 to Fields.Count - 1 do
    for j := 0 to AControl.ControlCount - 1 do
      if SameText(AControl.Controls[j].Name, string(Fields[i].FieldName)) then
      begin
        Rtti.SetControlValue(AControl.Controls[j], Fields[i].Value);
        Break;
      end;
end;

function TDataSetHelper.JoinFieldValue(const AFieldName: string; const ASeparator: string;
  const AQuoted: Boolean): string;
var
  bk: TBookmark;
  fd: TField;
  s: string;
begin
  Result := '';
  if IsEmpty then
    Exit;
  fd := FindField(AFieldName);
  if not Assigned(fd) then
    Exit;

  DisableControls;
  bk := GetBookmark;
  try
    First;
    while not Eof do
    try
      s := fd.AsString;
      if s <> '' then
      begin
        if AQuoted then
          s := QuotedStr(s);
        Result := Result + ASeparator + s;
      end;
    finally
      Next;
    end;
    if Length(Result) > 0 then
      system.Delete(Result, 1, Length(ASeparator));
  finally
    GotoBookmark(bk);
    EnableControls;
  end;
end;

{ TClientDataSetHelper }

procedure TClientDataSetHelper.SetAllFieldCanEdit;
var
  tmpDst: TClientDataSet;
  I : Integer;
begin
  tmpDst := TClientDataSet.Create(nil);
  Try
    DisableControls;
    tmpDst.Data := Data;
    Close;
    FieldDefs.Clear;
    for I := 0 to tmpDst.FieldDefs.Count - 1 do
      with FieldDefs.AddFieldDef do
      begin
        DataType := tmpDst.FieldDefs[I].DataType;
        Size := tmpDst.FieldDefs[I].Size;
        Name := tmpDst.FieldDefs[I].Name;
      end;
    // CreateAttachColumns(self,AddFields);
    CreateDataSet;
    tmpDst.First;
    while not tmpDst.Eof do
    begin
      Append;
      for I := 0 to Fields.Count - 1 do
        Fields[I].Value := tmpDst.Fields[I].Value;
      tmpDst.Next;
    end;
    if State in [dsInsert,dsEdit] then
      Post;
    MergeChangeLog;
  Finally
    EnableControls;
    tmpDst.Free;
  end;
end;

{$ENDIF}

{ TPersistentHelper }

{ function CheckControlTag(const AControl: TControl;
  const APropName: string): Boolean;
  var
  tagName: string;
  begin
  Result := False;
  if (AControl.Tag <> 0) then
  begin
  try
  tagName := StrPas(PChar(Pointer(AControl.Tag)));
  Result := SameText(tagName, APropName);
  except
  end;
  end;
  end;

  function TPersistentHelper.PropertyReadFromControlByTag
  (const AControl: TControl; const APropInfo: PPropInfo): Boolean;
  var
  j: Integer;
  wincontrol: TWinControl;
  begin
  Result := False;
  if CheckControlTag(AControl, string(APropInfo.Name)) then
  begin
  Rtti.GetControlValue(Self, AControl, APropInfo);
  Result := True;
  Exit;
  end;

  if (AControl is TWinControl) then
  begin
  wincontrol := AControl as TWinControl;
  for j := 0 to wincontrol.ControlCount - 1 do
  begin
  if PropertyReadFromControlByTag(wincontrol.Controls[j], APropInfo) then
  begin
  Result := True;
  Break;
  end;
  end;
  end;
  end;

  function TPersistentHelper.PropertyWriteToControlByTag
  (const AControl: TControl; const APropInfo: PPropInfo): Boolean;
  var
  j: Integer;
  wincontrol: TWinControl;
  begin
  Result := False;
  if CheckControlTag(AControl, string(APropInfo.Name)) then
  begin
  Rtti.SetControlValue(AControl, typinfo.GetPropValue(Self, APropInfo));
  Result := True;
  Exit;
  end;

  if (AControl is TWinControl) then
  begin
  wincontrol := AControl as TWinControl;
  for j := 0 to wincontrol.ControlCount - 1 do
  begin
  if PropertyWriteToControlByTag(wincontrol.Controls[j], APropInfo) then
  begin
  Result := True;
  Break;
  end;
  end;
  end;
  end;

  function TPersistentHelper.PropertyReadFromControl(const AControl: TControl;
  const APropInfo: PPropInfo): Boolean;
  var
  j: Integer;
  wincontrol: TWinControl;
  begin
  Result := False;
  if SameText(AControl.Name, string(APropInfo.Name)) then
  begin
  Rtti.GetControlValue(Self, AControl, APropInfo);
  Result := True;
  Exit;
  end;

  if (AControl is TWinControl) then
  begin
  wincontrol := AControl as TWinControl;
  for j := 0 to wincontrol.ControlCount - 1 do
  begin
  if PropertyReadFromControl(wincontrol.Controls[j], APropInfo) then
  begin
  Result := True;
  Break;
  end;
  end;
  end;
  end;

  function TPersistentHelper.PropertyWriteToControl(const AControl: TControl;
  const APropInfo: PPropInfo): Boolean;
  var
  j: Integer;
  wincontrol: TWinControl;
  begin
  Result := False;
  if SameText(AControl.Name, string(APropInfo.Name)) then
  begin
  Rtti.SetControlValue(AControl, typinfo.GetPropValue(Self, APropInfo));
  Result := True;
  Exit;
  end;

  if (AControl is TWinControl) then
  begin
  wincontrol := AControl as TWinControl;
  for j := 0 to wincontrol.ControlCount - 1 do
  begin
  if PropertyWriteToControl(wincontrol.Controls[j], APropInfo) then
  begin
  Result := True;
  Break;
  end;
  end;
  end;
  end;

  function TPersistentHelper.PropertiesReadFromControls
  (const AControl: TWinControl; const AByTagToo: Boolean): Boolean;
  var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  ret: Boolean;
  begin
  Result := False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
  for i := 0 to plen - 1 do
  if Assigned(plist[i].SetProc) then
  begin
  ret := PropertyReadFromControl(AControl, plist[i]);
  if (not ret) and AByTagToo then
  PropertyReadFromControlByTag(AControl, plist[i]);
  end;
  Result := True;
  finally
  FreeMem(plist, plen * SizeOf(Pointer));
  end;
  end;

  function TPersistentHelper.PropertiesWriteToControls
  (const AControl: TWinControl; const AByTagToo: Boolean): Boolean;
  var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  ret: Boolean;
  begin
  Result := False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
  for i := 0 to plen - 1 do
  if Assigned(plist[i].GetProc) then
  begin
  ret := PropertyWriteToControl(AControl, plist[i]);
  if (not ret) and AByTagToo then
  PropertyWriteToControlByTag(AControl, plist[i]);
  end;
  Result := True;
  finally
  FreeMem(plist, plen * SizeOf(Pointer));
  end;
  end;

  function TPersistentHelper.PropertyIsModifiedInControl
  (const AControl: TControl; const APropInfo: PPropInfo): Boolean;
  var
  v: Variant;
  i, j: Integer;
  wincontrol:TWinControl;
  begin
  Result := False;
  if SameText(AControl.Name, string(APropInfo.Name)) then
  begin
  try
  VarClear(v);
  for i := Low(ControlRttiPropList) to High(ControlRttiPropList) do
  if IsPublishedProp(AControl, ControlRttiPropList[i]) then
  begin
  v := GetPropValue(AControl, ControlRttiPropList[i]);
  Break;
  end;
  if not VarIsClear(v) then
  Result := GetPropValue(Self, string(APropInfo.Name)) <> v;
  except
  end;
  Exit;
  end;

  if (AControl is TWinControl) then
  begin
  wincontrol := AControl as TWinControl;
  for j := 0 to wincontrol.ControlCount - 1 do
  begin
  if PropertyIsModifiedInControl(wincontrol.Controls[j], APropInfo) then
  begin
  Result := True;
  Break;
  end;
  end;
  end;
  end;

  function TPersistentHelper.PropertyIsModifiedInControlByTag
  (const AControl: TControl; const APropInfo: PPropInfo): Boolean;
  var
  v: Variant;
  i, j: Integer;
  wincontrol:TWinControl;
  begin
  Result := False;
  if CheckControlTag(AControl, string(APropInfo.Name)) then
  begin
  try
  VarClear(v);
  for i := Low(ControlRttiPropList) to High(ControlRttiPropList) do
  if IsPublishedProp(AControl, ControlRttiPropList[i]) then
  begin
  v := GetPropValue(AControl, ControlRttiPropList[i]);
  Break;
  end;
  if not VarIsClear(v) then
  Result := GetPropValue(Self, string(APropInfo.Name)) <> v;
  except
  end;
  Exit;
  end;

  if (AControl is TWinControl) then
  begin
  wincontrol := AControl as TWinControl;
  for j := 0 to wincontrol.ControlCount - 1 do
  begin
  if PropertyIsModifiedInControlByTag(wincontrol.Controls[j], APropInfo)
  then
  begin
  Result := True;
  Break;
  end;
  end;
  end;
  end;

  function TPersistentHelper.PropertiesIsModifiedInControls
  (const AControl: TWinControl; const AByTagToo: Boolean): Boolean;
  var
  plist: PPropList;
  plen: Integer;
  i: Integer;
  begin
  Result := False;
  plen := GetPropList(Self, plist);
  if plen > 0 then
  try
  for i := 0 to plen - 1 do
  begin
  if Assigned(plist[i].SetProc) then
  begin
  if PropertyIsModifiedInControl(AControl, plist[i]) then
  begin
  Result := True;
  Exit;
  end;

  if AByTagToo and PropertyIsModifiedInControlByTag(AControl, plist[i])
  then
  begin
  Result := True;
  Exit;
  end;
  end;
  end;
  finally
  FreeMem(plist, plen * SizeOf(Pointer));
  end;
  end;// }

function TScreenHelper.FindForm(const AClass: TFormClass): TForm;
var
  i : Integer;
begin
  for i := 0 to FormCount - 1 do
    if Forms[i] is AClass then
      Exit(Forms[i]);
  Result  :=  nil;
end;

{ TControlHelper }

function TControlHelper.GetParentForm(const ATopForm: Boolean): TForm;
var
  pwc : TWinControl;
begin
  Result  :=  nil;
  pwc :=  Parent;
  while Assigned(pwc) do
  begin
    if pwc is TForm then
      Result  :=  TForm(pwc);
    if Assigned(Result) and not ATopForm
      then Exit;
    pwc :=  pwc.Parent;
  end;
end;

function TControlHelper.MouseInControl: Boolean;
var
  pt: TPoint;
  rc: TRect;
begin
  pt := ClientToScreen(Point(0, 0));
  rc.TopLeft := pt;
  pt := ClientToScreen(Point(Width, Height));
  rc.BottomRight := pt;
  Result := PtInRect(rc, Mouse.CursorPos);
end;

{ TWinControlHelper }

procedure TWinControlHelper.AutoSetTabOrder(const ATraverse: Boolean);
var
  i : Integer;
  c : Integer;
  warr : array of TWinControl;
  ti : Integer;

  function _SetTabOrder : Boolean;
  var
    i : Integer;
    min : TWinControl;
    minidx : Integer;
  begin
    minidx  :=  0;
    min :=  warr[0];
    for i := 1 to Length(warr) - 1 do
      if min = nil then
      begin
        minidx  :=  i;
        min := warr[i];
      end
      else if Assigned(warr[i]) then
        if (Abs(warr[i].Top - min.Top) <= 3) and (warr[i].Left < min.Left) or (Abs(warr[i].Top - min.Top) > 3) and (warr[i].Top < min.Top) then
        begin
          minidx  :=  i;
          min :=  warr[i];
        end;
    Result  :=  Assigned(min);
    if Result then
    begin
      min.TabOrder  :=  ti;
      Inc(ti);
      warr[minidx]  :=  nil;
    end;
  end;

begin
  c :=  0;
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TWinControl then
    begin
      if ATraverse then
        TWinControl(Controls[i]).AutoSetTabOrder(ATraverse);
      Inc(c);
    end;
  if c<=1 then Exit;
  SetLength(warr, c);
  c :=  0;
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TWinControl then
    begin
      warr[c] :=  TWinControl(Controls[i]);
      Inc(c);
    end;
  ti := 0;
  while _SetTabOrder do;
  SetLength(warr, 0);
end;

procedure TWinControlHelper.SetChildControlsEnabled(const AEnabled: Boolean);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    Controls[i].Enabled := AEnabled;
end;

procedure TWinControlHelper.SetParentControlsEnabled(const AEnabled: Boolean);
var
  pc: TWinControl;
begin
  Enabled := AEnabled;
  pc := Parent;
  while Assigned(pc) do
  begin
    pc.Enabled := AEnabled;
    pc := pc.Parent;
  end;
end;

{ TImageHelper }

function TImageHelper.LoadFromFileExFc(AFileName: String; const AIgnoreIfSameFileName: Boolean)
  : Boolean;
var
  l_Type: Integer;
  l_Stream: TMemoryStream;
  l_jpg: TJPEGImage;
  l_png: TPngImage;
  l_gif: TGIFImage;
  l_Bmp: Graphics.TBitmap;
begin
  if not FileExists(AFileName) then
  begin
    Picture.Assign(nil);
    Exit(False);
  end;

  if AIgnoreIfSameFileName and SameFileName(AFileName, LastFileName) then
    Exit(True);

  Result := False;
  l_Stream := TMemoryStream.Create;
  try
    try
      l_Stream.LoadFromFile(AFileName);
      l_Type := fcImage.Image.GetPicType(l_Stream.Memory, l_Stream.Size);
      l_Stream.Position := 0;
      case l_Type of // 1:bmp 2:jpeg 3:gif 4:png
        1:
          begin
            l_Bmp := Graphics.TBitmap.Create;
            try
              l_Bmp.LoadFromFile(AFileName);
              Picture.Assign(l_Bmp);
              Result := True;
            finally
              l_Bmp.Free;
            end;
          end;
        2:
          begin
            l_jpg := TJPEGImage.Create;
            try
              l_jpg.LoadFromStream(l_Stream);
              Picture.Assign(l_jpg);
              Result := True;
            finally
              l_jpg.Free;
            end;
          end;
        3:
          begin
            l_gif := TGIFImage.Create;
            try
              l_gif.LoadFromStream(l_Stream);
              Picture.Assign(l_gif);
              if (Picture.Graphic <> nil) and (Picture.Graphic is TGIFImage) then
                TGIFImage(Picture.Graphic).Animate := True;
              Result := True;
            finally
              l_gif.Free;
            end;
          end;
        4:
          begin
            l_png := TPngImage.Create;
            try
              l_png.LoadFromStream(l_Stream);
              Picture.Assign(l_png);
              Result := True;
            finally
              l_png.Free;
            end;
          end;
      end;
    except
      Picture.Assign(nil);
    end;
  finally
    l_Stream.Free;
    if Result then
    begin
      LastResourceName := '';
      LastFileName := AFileName;
    end;
  end;
end;

function TImageHelper.LoadFromResourceExFc(AHInst: Cardinal; AResName: string;
  const AIgnoreIfSameResName: Boolean; const ATransparent: Boolean;
  const APlayGif: Boolean): Boolean;

  function _FindRes: string;
  const
    c_arr: array [0 .. 4] of string = ('PIC', 'PNG', 'JPG', 'JPEG', 'GIF');
  var
    i: Integer;
  begin
    for i := Low(c_arr) to High(c_arr) do
      if FindResource(AHInst, PChar(AResName), PChar(c_arr[i])) > 0 then
        Exit(c_arr[i]);
    Result := '';
  end;

var
  l_Type: Integer;
  l_jpg: TJPEGImage;
  l_png: TPngImage;
  l_gif: TGIFImage;
  l_Bmp: Graphics.TBitmap;
  l_ResType: string;

  l_ResStream: TResourceStream;
begin
  if AHInst = 0 then
    Exit(False);
  l_ResType := _FindRes;
  if l_ResType = '' then
    Exit(False);

  if AIgnoreIfSameResName and (AResName = LastResourceName) then
    Exit(True);

  Result := False;
  l_ResStream := nil;
  try
    l_ResStream := TResourceStream.Create(AHInst, AResName, PChar(l_ResType));
  except
  end;
  if l_ResStream = nil then
    Exit(False);
  try
    Transparent := ATransparent;
    try
      l_Type := Image.GetPicType(l_ResStream.Memory, l_ResStream.Size);
      l_ResStream.Position := 0;
      case l_Type of // 1:bmp 2:jpeg 3:gif 4:png
        1:
          begin
            l_Bmp := Graphics.TBitmap.Create;
            try
              l_Bmp.LoadFromStream(l_ResStream);
              Picture.Assign(l_Bmp);
            finally
              l_Bmp.Free;
            end;
          end;
        2:
          begin
            l_jpg := TJPEGImage.Create;
            try
              l_jpg.LoadFromStream(l_ResStream);
              Picture.Assign(l_jpg);
            finally
              l_jpg.Free;
            end;
          end;
        3:
          begin
            l_gif := TGIFImage.Create;
            try
              l_gif.LoadFromStream(l_ResStream);
              Picture.Assign(l_gif);
              if APlayGif and (Picture.Graphic <> nil) and (Picture.Graphic is TGIFImage) then
                TGIFImage(Picture.Graphic).Animate := True;
            finally
              l_gif.Free;
            end;
          end;
        4:
          begin
            l_png := TPngImage.Create;
            try
              l_png.LoadFromStream(l_ResStream);
              Picture.Assign(l_png);
            finally
              l_png.Free;
            end;
          end;
      end;
    except
      Picture.Assign(nil);
    end;
  finally
    l_ResStream.Free;
    if Result then
    begin
      LastFileName := '';
      LastResourceName := AResName;
    end;
  end;
end;

function TImageHelper.LoadFromStreamExFc(AStream: TStream; const ATransparent,
  APlayGif: Boolean): Boolean;
const
  BuffSize = 8;
var
  l_Buff : array[0..BuffSize - 1] of Byte;
  l_Type: Integer;
  l_jpg: TJPEGImage;
  l_png: TPngImage;
  l_gif: TGIFImage;
  l_Bmp: Graphics.TBitmap;
begin
  Result := False;
  if AStream = nil then
    Exit(False);

  AStream.Position  :=  0;
  AStream.read(l_Buff[0], BuffSize);

  try
    l_Type := Image.GetPicType(@l_Buff[0], BuffSize);
    AStream.Position := 0;
    case l_Type of // 1:bmp 2:jpeg 3:gif 4:png
      1:
        begin
          l_Bmp := Graphics.TBitmap.Create;
          try
            l_Bmp.LoadFromStream(AStream);
            Picture.Assign(l_Bmp);
          finally
            l_Bmp.Free;
          end;
        end;
      2:
        begin
          l_jpg := TJPEGImage.Create;
          try
            l_jpg.LoadFromStream(AStream);
            Picture.Assign(l_jpg);
          finally
            l_jpg.Free;
          end;
        end;
      3:
        begin
          l_gif := TGIFImage.Create;
          try
            l_gif.LoadFromStream(AStream);
            Picture.Assign(l_gif);
            if APlayGif and (Picture.Graphic <> nil) and (Picture.Graphic is TGIFImage) then
              TGIFImage(Picture.Graphic).Animate := True;
          finally
            l_gif.Free;
          end;
        end;
      4:
        begin
          l_png := TPngImage.Create;
          try
            l_png.LoadFromStream(AStream);
            Picture.Assign(l_png);
          finally
            l_png.Free;
          end;
        end;
    end;
  except
    Picture.Assign(nil);
  end;
end;

{ TMemoryStreamHelper }

function TMemoryStreamHelper.ToBytes: TBytes;
begin
  SetLength(Result, Size);
  CopyMemory(@Result[0], Memory, Size);
end;

{ TIniFileHelper }

function TIniFileHelper.ReadLongString(const Section, Ident, Default: string): string;
const //从ini读长字符串,可以读出64K数据
  DefSize = 2047;
  LongSize = MAXWORD;
var
  Buffer: array[0..DefSize] of Char;
  BufferLen : Integer;
  LongBuffer : array of Char;
begin
  BufferLen :=  GetPrivateProfileString(PChar(Section),
    PChar(Ident), PChar(Default), Buffer, Length(Buffer), PChar(FileName));
  if BufferLen < DefSize then
    SetString(Result, Buffer, BufferLen)
  else begin
    SetLength(LongBuffer, LongSize);
    BufferLen :=  GetPrivateProfileString(PChar(Section),
      PChar(Ident), PChar(Default), @LongBuffer[0], Length(LongBuffer), PChar(FileName));
    SetString(Result, PChar(@LongBuffer[0]), BufferLen);
    SetLength(LongBuffer, 0);
  end;
end;

end.
