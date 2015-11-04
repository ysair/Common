unit uComCtrlsInterpreter;

interface

uses
  windows, SysUtils, Classes, ComCtrls,
  uStorage, uObjectStorage;

implementation

const
  S_Width = 'Width';
  S_ID    = 'ID';

type
  TListViewInterpreter = class(TObjectInterpreter)
  public
    class function  SupportClass(ObjClass : TClass) : boolean; override;
    class procedure SaveObject(Obj  : TObject; Storage  : TStorage; Section : string); override;
    class procedure LoadObject(Obj  : TObject; Storage  : TStorage; Section : string); override;
  end;

{ TListViewInterpreter }

class procedure TListViewInterpreter.LoadObject(Obj: TObject; Storage: TStorage;
  Section: string);
var
  i : integer;
  w : integer;
  lv  : TListView;
begin
  lv  :=  Obj as TListView;
  for i := 0 to lv.Columns.Count - 1 do
  begin
    Storage.Section :=  Section;
    w :=  Storage.ReadInteger(lv.Name + inttostr(i));
    if w > 0 then
      lv.Column[i].Width :=  w;
  end;
end;

class procedure TListViewInterpreter.SaveObject(Obj: TObject; Storage: TStorage;
  Section: string);
var
  i : integer;
  lv  : TListView;
begin
  lv  :=  Obj as TListView;
  Storage.Section :=  Section;
  for i := 0 to lv.Columns.Count - 1 do
  begin
    Storage.Write(lv.Name + inttostr(i), lv.Column[i].Width);
  end;
end;

class function TListViewInterpreter.SupportClass(
  ObjClass: TClass): boolean;
begin
  result := ObjClass = TListView;
end;

initialization
  ObjectStorage.RegisterInterpreter(TListViewInterpreter);

end.
