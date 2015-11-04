unit fcClasses;

/// 这个单元可以用 TThreadList 来替代。

interface

uses
  Windows, Classes, IniFiles;

type
  TThreadListBase = class
  private
    FList: TList;
    FLock: TRTLCriticalSection;
    FDuplicates: TDuplicates;
    function GetCount: integer;
  protected
    procedure DoAdd(Item: Pointer);
    procedure DoAddFirst(Item: Pointer);
    procedure DoRemove(Item: Pointer);
    procedure DoDelete(Index:Integer);
    function DoPopup: Pointer;
    function GetItem(idx: integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function LockList: TList;
    procedure UnlockList;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Count: integer read GetCount;
  end;

  TDmThreadList = class(TThreadListBase)
  public
    procedure Add(Item: Pointer);
    procedure Remove(Item: Pointer);
    function Items(idx: integer): Pointer;
  end;

  TIntegerThreadList = class(TThreadListBase)
  public
    procedure Add(Item: integer);
    procedure AddFirst(Item:Integer);
    procedure Remove(Item: integer);
    function Items(idx: integer): integer;
    function Popup:Integer;
  end;

  Obj = record
    class function IfThen(AValue: Boolean; AObj1, AObj2: TObject): TObject; static;
  end;

  TThreadStringList = class(TStringList)
  private
    FLock: TRTLCriticalSection;
  protected
    function GetCount: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  TThreadIniFile = class(TIniFile)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;


implementation

uses Consts;

constructor TThreadListBase.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TList.Create;
  FDuplicates := dupIgnore;
end;

destructor TThreadListBase.Destroy;
begin
  LockList; // Make sure nobody else is inside the list.
  try
    FList.Free;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;

  inherited;
end;

procedure TThreadListBase.DoAdd(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      FList.Error(@SDuplicateItem, integer(Item));
  finally
    UnlockList;
  end;
end;

procedure TThreadListBase.DoAddFirst(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      FList.Insert(0, Item)
    else if Duplicates = dupError then
      FList.Error(@SDuplicateItem, integer(Item));
  finally
    UnlockList;
  end;
end;

procedure TThreadListBase.DoDelete(Index: Integer);
begin
  LockList;
  try
    if FList.Count > Index then
      FList.Delete(Index);
  finally
    UnlockList;
  end;
end;

function TThreadListBase.DoPopup: Pointer;
begin
  LockList;
  try
    if FList.Count > 0 then
    begin
      Result := FList[0];
      FList.Remove(Result);
    end
    else
      Result := nil;
  finally
    UnlockList;
  end;
end;

procedure TThreadListBase.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadListBase.LockList: TList;
begin
  try
    try
      EnterCriticalSection(FLock);
    except
    end;
  finally
    Result := FList;
  end;
end;

procedure TThreadListBase.DoRemove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadListBase.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

function TThreadListBase.GetCount: integer;
begin
  LockList;
  try
    Result := FList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadListBase.GetItem(idx: integer): Pointer;
begin
  if idx <= FList.Count - 1 then
    Result := FList[idx]
  else
    Result := nil;
end;

{ Obj }

class function Obj.IfThen(AValue: Boolean; AObj1, AObj2: TObject): TObject;
begin
  if AValue then
    Result := AObj1
  else
    Result := AObj2;
end;

{ TIntegerList }

procedure TIntegerThreadList.Add(Item: integer);
begin
  DoAdd(Pointer(Item));
end;

procedure TIntegerThreadList.AddFirst(Item: Integer);
begin
  DoAddFirst(Pointer(Item));
end;

function TIntegerThreadList.Items(idx: integer): integer;
begin
  Result := integer(GetItem(idx));
end;

function TIntegerThreadList.Popup: Integer;
begin
  Result := Integer(DoPopup);
end;

procedure TIntegerThreadList.Remove(Item: integer);
begin
  DoRemove(Pointer(Item));
end;

{ TDmThreadList }

procedure TDmThreadList.Add(Item: Pointer);
begin
  DoAdd(Item);
end;

function TDmThreadList.Items(idx: integer): Pointer;
begin
  Result := GetItem(idx);
end;

procedure TDmThreadList.Remove(Item: Pointer);
begin
  DoRemove(Item);
end;

{ TThreadStringList }

constructor TThreadStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TThreadStringList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

function TThreadStringList.GetCount: Integer;
begin
  try
    Lock;
    Result := inherited GetCount;
  finally
    Unlock;
  end;
end;

procedure TThreadStringList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TThreadStringList.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

{ TThreadIniFile }

constructor TThreadIniFile.Create(const FileName: string);
begin
  inherited Create(FileName);
  InitializeCriticalSection(FLock);
end;

destructor TThreadIniFile.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TThreadIniFile.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TThreadIniFile.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

end.
