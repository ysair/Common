unit fcThreadSafe;

interface
uses
  Windows, Classes, SysUtils;

type
  TThreadSafeList = class(TThreadList);

  TThreadSafeStringList = class
  private
    FList: TStringList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AStr: string; const AData: TObject=nil);
    procedure Insert(AStr: string; const AIndex: Integer=0; const AData: TObject=nil);
    procedure Clear;
    function LockList: TStringList;
    procedure UnlockList; inline;
    function  Count: Integer;
    function  GetFirstLine(var AStr: string): Boolean; overload;
    function  GetFirstLine(var AStr: string ; var AData: TObject): Boolean; overload;
    function  GetAndDelFirstLine(var AStr: string): Boolean; overload;
    function  GetAndDelFirstLine(var AStr: string ; var AData: TObject): Boolean; overload;
    function  Items(AIndex: Integer): string;
    function  Objects(AIndex: Integer): TObject;
    procedure AddFromList(AList: TStringList);
    procedure InsertFromList(AList: TStringList);
  end;


implementation

{ TThreadSafeStringList }

procedure TThreadSafeStringList.Add(AStr: string; const AData: TObject);
begin
  LockList;
  try
    FList.AddObject(AStr, AData);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.AddFromList(AList: TStringList);
var
  I: Integer;
begin
  LockList;
  try
    for I := 0 to AList.Count - 1 do
      FList.AddObject(AList[I], AList.Objects[I]);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.Count: Integer;
begin
  LockList;
  try
    Result := FList.Count;
  finally
    UnlockList;
  end;
end;

constructor TThreadSafeStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TStringList.Create;
end;

destructor TThreadSafeStringList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

function TThreadSafeStringList.GetAndDelFirstLine(var AStr: string): Boolean;
var
  l_Data: TObject;
begin
  Result := GetAndDelFirstLine(AStr, l_Data);
end;

function TThreadSafeStringList.GetAndDelFirstLine(var AStr: string; var AData: TObject): Boolean;
begin
  LockList;
  try
    if FList.Count > 0 then
    begin
      Result := True;
      AStr := FList[0];
      AData := FList.Objects[0];
      FList.Delete(0);
    end
    else
      Result := false;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.GetFirstLine(var AStr: string): Boolean;
var
  l_Data: TObject;
begin
  Result := GetFirstLine(AStr, l_Data);
end;

function TThreadSafeStringList.GetFirstLine(var AStr: string; var AData: TObject): Boolean;
begin
  LockList;
  try
    if FList.Count > 0 then
    begin
      Result := True;
      AStr := FList[0];
      AData := FList.Objects[0];
    end
    else
      Result := false;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.Insert(AStr: string; const AIndex: Integer; const AData: TObject);
begin
  LockList;
  try
    FList.InsertObject(AIndex, AStr, AData);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.InsertFromList(AList: TStringList);
var
  I: Integer;
begin
  LockList;
  try
    for I := 0 to AList.Count - 1 do
      FList.InsertObject(I, AList[I], AList.Objects[I]);
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.Items(AIndex: Integer): string;
begin
  LockList;
  try
    if AIndex <= FList.Count-1 then
    begin
      Result := FList[AIndex];
    end
    else
      Result := '';
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.LockList: TStringList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

function TThreadSafeStringList.Objects(AIndex: Integer): TObject;
begin
  LockList;
  try
    if AIndex <= FList.Count-1 then
    begin
      Result := FList.Objects[AIndex];
    end
    else
      Result := nil;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

end.
