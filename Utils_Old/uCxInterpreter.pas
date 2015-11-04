unit uCxInterpreter;

interface

uses
  windows, SysUtils, Classes,
  uStorage, uObjectStorage, cxGridTableView;

implementation

const
  S_Width = 'Width';
  S_ID    = 'ID';

type
  TcxGridTableViewInterpreter = class(TObjectInterpreter)
  public
    class function  SupportClass(ObjClass : TClass) : boolean; override;
    class procedure SaveObject(Obj  : TObject; Storage  : TStorage; Section : string); override;
    class procedure LoadObject(Obj  : TObject; Storage  : TStorage; Section : string); override;
  end;

{ TcxGridTableViewInterpreter }

class function TcxGridTableViewInterpreter.SupportClass(
  ObjClass: TClass): boolean;
begin
  Result  :=  ObjClass =  TcxGridTableView;
end;

class procedure TcxGridTableViewInterpreter.SaveObject(Obj: TObject;
  Storage: TStorage; Section: string);
var
  i : integer;
  tv  : TcxGridTableView;
begin
  tv  :=  Obj as TcxGridTableView;
  Storage.Section :=  Section;
  for i := 0 to tv.ColumnCount - 1 do
  begin
    Storage.Write(tv.Name + S_Width + inttostr(i), tv.Columns[i].Width);
    Storage.Write(tv.Name + S_ID + inttostr(i), tv.Columns[i].ID);
  end;
end;

class procedure TcxGridTableViewInterpreter.LoadObject(Obj: TObject;
  Storage: TStorage; Section: string);
var
  i : integer;
  j : Integer;
  tv  : TcxGridTableView;
  w   : Integer;
  id  : Integer;
begin
  tv  :=  Obj as TcxGridTableView;
  Storage.Section :=  Section;
  for i := 0 to tv.ColumnCount - 1 do
  begin
    w   :=  Storage.ReadInteger(tv.Name + S_Width + inttostr(i));
    id  :=  Storage.ReadInteger(tv.Name + S_ID + inttostr(i));
    if id >= 0 then
    begin
      for j := 0 to tv.ColumnCount - 1 do
        if tv.Columns[j].ID = id then
        begin
          tv.Columns[j].Index :=  i;
          break;
        end;
    end;
    if w > 0 then
      tv.Columns[i].Width :=  w;
  end;
end;

initialization
  ObjectStorage.RegisterInterpreter(TcxGridTableViewInterpreter);

end.
