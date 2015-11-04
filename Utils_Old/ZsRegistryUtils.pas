unit ZsRegistryUtils;

interface

uses Registry, Windows, Classes;

function reg_ReadKeyValue(ARootKey: HKEY; AKeyPath, AName, ADefault: String): String; overload;

function reg_ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String; ADefault: Integer): Integer;
  overload;

function reg_ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String; ADefault: Boolean): Boolean;
  overload;

procedure reg_ReadRegKeyNames(ARootKey: HKEY; AKeyPath: String; AOutStrings: TStringList);

function reg_WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: String): Boolean;
  overload;

function reg_WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: Integer): Boolean;
  overload;

function reg_DeleteKey(ARootKey: HKEY; AKeyPath, AName: String): Boolean;

function reg_DeleteSession(ARootKey: HKEY; AName: String): Boolean;

implementation

function reg_ReadKeyValue(ARootKey: HKEY; AKeyPath, AName, ADefault: String): String; overload;
begin
  Result := ADefault;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKeyReadOnly(AKeyPath) then
    begin
      try
        try
          Result := ReadString(AName);
        except
          Result := ADefault;
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function reg_ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String; ADefault: Integer): Integer;
  overload;
begin
  Result := ADefault;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKeyReadOnly(AKeyPath) then
    begin
      try
        try
          Result := ReadInteger(AName);
        except
          Result := ADefault;
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function reg_ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String; ADefault: Boolean): Boolean;
  overload;
begin
  Result := ADefault;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKeyReadOnly(AKeyPath) then
    begin
      try
        try
          Result := ReadBool(AName);
        except
          Result := ADefault;
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

procedure reg_ReadRegKeyNames(ARootKey: HKEY; AKeyPath: String; AOutStrings: TStringList);
begin
  AOutStrings.Clear;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKeyReadOnly(AKeyPath) then
    begin
      try
        try
          GetKeyNames(AOutStrings);
        except
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function reg_WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: String): Boolean;
  overload;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin
      try
        try
          if ReadString(AName) = AValue then
          begin
            Result := True;
            Exit;
          end;
        except
        end;

        try
          WriteString(AName, AValue);
          Result := True;
        except
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function reg_WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: Integer): Boolean;
  overload;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin
      try
        try
          if ReadInteger(AName) = AValue then
          begin
            Result := True;
            Exit;
          end;
        except
        end;

        try
          WriteInteger(AName, AValue);
          Result := True;
        except
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function reg_DeleteKey(ARootKey: HKEY; AKeyPath, AName: String): Boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin

      try
        try
          DeleteKey(AName);
          Result := True;
        except
        end;
      finally
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function reg_DeleteSession(ARootKey: HKEY; AName: String): Boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    try
      if KeyExists(AName) then
        DeleteKey(AName) except
    end;
    Free;
  end;
end;

end.
