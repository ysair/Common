unit dmClientDataSet;

interface

uses
  DBClient, Classes;

type
  TDmClientDataSet = class(TClientDataSet)
  private
    function GetId: Integer;
    function GetEditMode: Boolean;
  protected
    function GetIdName:string;virtual;
    property IdName:string read GetIdName;
    procedure DoAfterOpen; override;
  public
    property CurrentId: Integer read GetId;
    procedure FilterById(AId: Integer);
    function CurrentHasChild(AParentIDFieldName:string = 'ParentID'):Boolean;
    function HasChild(AId:Integer; AParentIDFieldName:string = 'ParentID'):Boolean;
    property EditMode: Boolean read GetEditMode;
  end;

implementation

uses
  DB, SysUtils;

{ TDmClientDataSet2 }

function TDmClientDataSet.CurrentHasChild(AParentIDFieldName: string): Boolean;
begin
  Result := False;

  if -1 = CurrentId  then
    Exit;

  Result := HasChild(CurrentId, AParentIDFieldName);
end;

procedure TDmClientDataSet.DoAfterOpen;
var
  i : Integer;
begin
  inherited;
  for i := 0 to Fields.Count - 1 do
    if Fields[i] is TBCDField then
      TBCDField(Fields[i]).DisplayFormat :=  '#0.00';//}
end;

procedure TDmClientDataSet.FilterById(AId: Integer);
begin
  Self.Filter := IdName + '=' + IntToStr(AId);
  Self.Filtered := True;
end;

function TDmClientDataSet.GetEditMode: Boolean;
begin
  Result := State in [dsEdit, dsInsert];
end;

function TDmClientDataSet.GetId: Integer;
begin
  if (not Self.Active) or Self.IsEmpty then
    Result := -1
  else
    Result := FieldByName(IdName).AsInteger;
end;

function TDmClientDataSet.GetIdName: string;
begin
  Result := 'ID';
end;

function TDmClientDataSet.HasChild(AId: Integer;
  AParentIDFieldName: string): Boolean;
var
  lastFilter:string;
  lastFiltered:Boolean;
  lastRecNo:Integer;
begin
  Self.DisableControls;
  try
    lastFilter := Self.Filter;
    lastFiltered := Self.Filtered;
    lastRecNo := Self.RecNo;

    Self.Filter := Format('%s=%d',[AParentIDFieldName, AId]);
    Self.Filtered := True;

    Result := not Self.IsEmpty;

    Self.Filter := lastFilter;
    Self.Filtered := lastFiltered;
    Self.RecNo := lastRecNo;
  finally
    Self.EnableControls;
  end;
end;

end.
