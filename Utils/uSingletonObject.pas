unit uSingletonObject;

interface

uses
  Classes, SysUtils, Windows;

type
  TSingletonObject = class(TObject)
  private
    //class var FList : TThreadList;
  public
    //class constructor Create;
    //class destructor Destroy;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

implementation

type
  TRefInfo = record
    RefClass : TClass;
    Instance : TObject;
    Count : Integer;
  end;
  PRefInfo = ^TRefInfo;

var
  G_RefList : TThreadList;

{ TSingletonObject }

{class constructor TSingletonObject.Create;
begin
  FList :=  TThreadList.Create;
end;

class destructor TSingletonObject.Destroy;
var
  i : Integer;
  p : PRefInfo;
  lst : TList;
begin
  lst :=  FList.LockList;
  try
    for i := lst.Count - 1 downto 0 do
    begin
      p :=  lst[i];
      p.Count :=  0;
      p.Instance.Free;
      Dispose(p);
    end;
  finally
    FList.UnlockList;
  end;
  FList.Free;
end;//}

class function TSingletonObject.NewInstance: TObject;
var
  i : Integer;
  ref : PRefInfo;
  lst : TList;
begin
  lst :=  G_RefList.LockList;
  try
    for i := 0 to lst.Count - 1 do
    begin
      ref :=  lst[i];
      if ref.RefClass = Self then
      begin
        if ref.Instance = nil then
        begin
          ref.Count :=  0;
          ref.Instance :=  inherited NewInstance();
        end;
        ref.Count :=  ref.Count + 1;
        Result  :=  ref.Instance;
        Exit;
      end;
    end;

    Result  :=  inherited NewInstance();
    new(ref);
    ref.RefClass  :=  Self;
    ref.Instance :=  Result;
    ref.Count :=  1;
    lst.Add(ref);
  finally
    G_RefList.UnlockList;
  end;
end;

procedure TSingletonObject.FreeInstance;
var
  i : Integer;
  ref : PRefInfo;
  lst : TList;
begin
  lst :=  G_RefList.LockList;
  try
    for i := 0 to lst.Count - 1 do
    begin
      ref :=  lst[i];
      if ref.RefClass = Self.ClassType then
      begin
        ref.Count :=  ref.Count - 1;
        if ref.Count <= 0 then
        begin
          ref.Instance :=  nil;
          inherited;
        end;
        Break;
      end;
    end;
  finally
    G_RefList.UnlockList;
  end;
end;

procedure FreeRefList;
var
  i : Integer;
  p : PRefInfo;
  lst : TList;
begin
  lst :=  G_RefList.LockList;
  try
    for i := lst.Count - 1 downto 0 do
    begin
      p :=  lst[i];
      p.Count :=  0;
      p.Instance.Free;
      Dispose(p);
    end;
  finally
    G_RefList.UnlockList;
  end;
  G_RefList.Free;
end;//}

initialization
  G_RefList :=  TThreadList.Create;

finalization
  FreeRefList;

end.
