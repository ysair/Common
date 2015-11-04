unit uDevExpressFunc;

interface

uses
  Windows, Classes, SysUtils, Controls,
  cxGridTableView, cxDropDownEdit, uRTTI;

procedure SetTableViewComboBoxIncrementalSearch(
    const ATableView : TcxGridTableView;
    const AIncrementalSearch : Boolean = False
    );
procedure SetChildControlsPropertiesReadOnly(
    const AControl : TWinControl;
    const AReadOnly : Boolean = False;
    const ARecursion : Boolean = False
    );

implementation

procedure SetTableViewComboBoxIncrementalSearch(
    const ATableView : TcxGridTableView;
    const AIncrementalSearch : Boolean = False
    );
var
  i : Integer;
begin
  for i := 0 to ATableView.ColumnCount - 1 do
    if ATableView.Columns[i].PropertiesClass = TcxComboBoxProperties then
      TcxComboBoxProperties(ATableView.Columns[i].Properties).IncrementalSearch := AIncrementalSearch;
end;

procedure SetChildControlsPropertiesReadOnly(
    const AControl : TWinControl;
    const AReadOnly : Boolean = False;
    const ARecursion : Boolean = False
    );
var
  i : Integer;
begin
  for i := 0 to AControl.ControlCount - 1 do
  begin
    SetEnumPropertyIfExists(
        GetObjectProperty(AControl.Controls[i], 'Properties'),
        'ReadOnly',
        Ord(AReadOnly)
        );
    if ARecursion and (AControl.Controls[i] is TWinControl) then
      SetChildControlsPropertiesReadOnly(TWinControl(AControl.Controls[i]), AReadOnly, ARecursion);
  end;
end;

end.

