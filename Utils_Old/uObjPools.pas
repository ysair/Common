// 对象池
unit uObjPools;

interface

uses
  Windows, Classes, SysUtils, ExtCtrls, Generics.Collections;

type
  TPoolsClearType = (ctNone, ctOne, ctAll);

  TVirtualPools = class(TObject)
  protected
    FClearInterval: Integer;
    FClearType: TPoolsClearType;
    FClearTimer : TTimer;
    FList: TList;
    FSection: TRTLCriticalSection;
    FCapacity: integer;
    FLastWorkTime : DWORD;
    FUnique : Boolean;
    FOwnsObjects : Boolean;
  protected
    function GetCount: integer;
    function GetIdleTime: Integer;
    procedure DoWorking;
    procedure OnClearTimer(Sender : TObject); virtual;
    procedure SetClearInterval(const Value: Integer);
    procedure Enter;
    procedure Leave;

    procedure DoPush(Item : Pointer); virtual;
    function  DoPop: Pointer; virtual;
    procedure FreeItem(const Item : Pointer); virtual;
    procedure Clear; virtual;
  public
    property Count: integer read GetCount;
    property IdleTime : Integer read GetIdleTime;
    property ClearType : TPoolsClearType read FClearType write FClearType;
    property ClearInterval : Integer read FClearInterval write SetClearInterval;
    property Unique : Boolean read FUnique;
    property OwnsObjects : boolean read FOwnsObjects;
  public
    constructor Create(const AOwnsObjects: Boolean = True; const Capacity: integer = 0; const AUnique : Boolean = False);virtual;
    destructor Destroy; override;
  end;

  TPools<T> = class(TObject)
  private
    FClearInterval: Integer;
    FClearType: TPoolsClearType;
    FClearTimer : TTimer;
  private
    function GetCount: integer;
    function GetIdleTime: Integer;
    procedure DoWorking;
    procedure OnClearTimer(Sender : TObject);
    procedure SetClearInterval(const Value: Integer);
  protected
    FList: TList<T>;
    FSection: TRTLCriticalSection;
    FCapacity: integer;
    FLastWorkTime : DWORD;
    FUnique : Boolean;
    procedure Enter;
    procedure Leave;
  public
    property Count: integer read GetCount;
    property IdleTime : Integer read GetIdleTime;
    property ClearType : TPoolsClearType read FClearType write FClearType;
    property ClearInterval : Integer read FClearInterval write SetClearInterval;
    property Unique : Boolean read FUnique;
  public
    constructor Create(Capacity: integer = 0; AUnique : Boolean = False);
    destructor Destroy; override;

    procedure Push(Obj: T);
    function  Pop(var Obj: T) : Boolean;
    procedure Clear;
  end;

  TObjPools = class(TVirtualPools)
  protected
    procedure FreeItem(const Item : Pointer); override;
  public
    procedure Push(Obj: TObject);
    function  Pop: TObject;
  end;

  TInterfacePools = class(TVirtualPools)
  protected
    procedure FreeItem(const Item : Pointer); override;
  public
    procedure Push(Intf : IInterface);
    function  Pop: IInterface;
  end;

implementation

{ TVirtualPools }

constructor TVirtualPools.Create(const AOwnsObjects: Boolean; const Capacity: integer; const AUnique : Boolean);
begin
  inherited Create;
  InitializeCriticalSection(FSection);
  FList :=  TList.Create;
  FOwnsObjects  :=  AOwnsObjects;
  FUnique :=  AUnique;
  FCapacity := Capacity;
  DoWorking;
  FClearTimer  :=  TTimer.Create(nil);
  FClearTimer.Interval :=  1000;
  FClearTimer.OnTimer  :=  OnClearTimer;
  FClearTimer.Enabled  :=  True;  //}
end;

destructor TVirtualPools.Destroy;
begin
  FClearTimer.Free;
  Clear;
  FreeAndNil(FList);
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TVirtualPools.FreeItem(const Item: Pointer);
begin

end;

procedure TVirtualPools.Clear;
var
  item : Pointer;
begin
  item := DoPop;
  while Assigned(item) do
  begin
    if FOwnsObjects then
      FreeItem(item);
    item := DoPop;
  end;
end;

procedure TVirtualPools.DoWorking;
begin
  FLastWorkTime :=  GetTickCount;
end;

function TVirtualPools.GetIdleTime: Integer;
begin
  Result  :=  GetTickCount - FLastWorkTime;
end;

procedure TVirtualPools.OnClearTimer(Sender: TObject);
var
  item : Pointer;
begin
  if (FClearInterval <= 0) or (IdleTime <= FClearInterval) then Exit;

  case ClearType of
    ctOne:
      begin
        item  :=  DoPop;
        if Assigned(item) and FOwnsObjects then
          FreeItem(item);
      end;
    ctAll: Clear;
  end;
end;

procedure TVirtualPools.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TVirtualPools.Leave;
begin
  LeaveCriticalSection(FSection);
end;

function TVirtualPools.GetCount: integer;
begin
  result := FList.Count;
end;

function TVirtualPools.DoPop: Pointer;
begin
  Enter;
  try
    if FList.Count > 0 then
    begin
      result := FList.Items[0];
      FList.Delete(0);
    end
    else
      result := nil;
    DoWorking;
  finally
    Leave;
  end;
end;

procedure TVirtualPools.DoPush(Item: Pointer);
begin
  if not Assigned(Item) then Exit;
  Enter;
  try
    if FUnique and (FList.IndexOf(Item) >= 0) then Exit;
    if (FList.Count >= FCapacity) and (FCapacity > 0) then
      Raise Exception.Create('池已满！')
    else
      FList.Add(Item);
    DoWorking;
  finally
    Leave;
  end;
end;

procedure TVirtualPools.SetClearInterval(const Value: Integer);
begin
  if Value <> 0 then
    FClearInterval := Value;
end;

{ TPools<T> }

constructor TPools<T>.Create(Capacity: integer; AUnique: Boolean);
begin
  InitializeCriticalSection(FSection);
  FList :=  TList<T>.Create;
  FUnique :=  AUnique;
  FCapacity := Capacity;
  DoWorking;
  FClearTimer  :=  TTimer.Create(nil);
  FClearTimer.Interval :=  1000;
  FClearTimer.OnTimer  :=  OnClearTimer;
  FClearTimer.Enabled  :=  True;  //}
end;

destructor TPools<T>.Destroy;
begin
  FClearTimer.Free;
  Clear;
  FreeAndNil(FList);
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TPools<T>.Clear;
var
  Obj: T;
begin
  while Pop(Obj) do;
end;

procedure TPools<T>.DoWorking;
begin
  FLastWorkTime :=  GetTickCount;
end;

procedure TPools<T>.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TPools<T>.Leave;
begin
  LeaveCriticalSection(FSection);
end;

function TPools<T>.GetCount: integer;
begin
  result := FList.Count;
end;

function TPools<T>.GetIdleTime: Integer;
begin
  Result  :=  GetTickCount - FLastWorkTime;
end;

procedure TPools<T>.OnClearTimer(Sender: TObject);
var
  obj : T;
begin
  if (FClearInterval <= 0) or (IdleTime <= FClearInterval) then Exit;

  case ClearType of
    ctOne:
        Pop(obj);
    ctAll: Clear;
  end;
end;

function TPools<T>.Pop(var Obj: T) : Boolean;
begin
  Enter;
  try
    if FList.Count > 0 then
    begin
      Obj := FList.Items[0];
      FList.Delete(0);
      Result  :=  True;
    end
    else
      result := False;
    DoWorking;
  finally
    Leave;
  end;
end;

procedure TPools<T>.Push(Obj: T);
begin
  Enter;
  try
    if FUnique and (FList.IndexOf(Obj) >= 0) then Exit;
    if (FList.Count >= FCapacity) and (FCapacity > 0) then
      Raise Exception.Create('池已满！')
    else
      FList.Add(Obj);
    DoWorking;
  finally
    Leave;
  end;
end;

procedure TPools<T>.SetClearInterval(const Value: Integer);
begin
  if Value <> 0 then
    FClearInterval := Value;
end;

{ TObjPools }

procedure TObjPools.FreeItem(const Item: Pointer);
begin
  if Assigned(Item) then
    TObject(Item).Free;
end;

function TObjPools.Pop: TObject;
begin
  Result  :=  DoPop;
end;

procedure TObjPools.Push(Obj: TObject);
begin
  DoPush(obj);
end;

{ TInterfacePools }

procedure TInterfacePools.FreeItem(const Item: Pointer);
begin
  if Assigned(Item) then
    IInterface(Item)._Release;
end;

function TInterfacePools.Pop: IInterface;
begin
  Result  :=  IInterface(DoPop);
end;

procedure TInterfacePools.Push(Intf: IInterface);
begin
  if Assigned(Intf) then
    DoPush(Pointer(Intf));
end;

end.
