unit fcRegistry;

interface

uses
  Windows, SysUtils, Registry, ShLwApi, Classes;

type
  Reg = record
    class function SH_GetKeyValue(vRoot: DWORD; const sKey, sName: string; const ADefault: string=''): string; static;
    class function SH_SetKeyValue(vRoot: DWORD; const sKey, sName, sValue: string): boolean; static;
    class function SH_GetKeyValueX64(vRoot:DWORD; const sKey, sName:string; const ADefault: string='') : string; static;
    class function SH_GetKeyIntValue(vRoot: DWORD; const sKey, sName: string; const ADefValue: Int64 = 0): Int64; static;
    class function SH_SetKeyIntValue(vRoot: DWORD; const sKey, sName: string; const AValue: Integer): boolean; static;
    class function SH_DelKey(vRoot: DWORD; const sKey: string): Boolean; static;
    class function SH_DelValue(vRoot: DWORD; const sKey, sName: string): Boolean; static;

    //------ 以下是对 TRegistry 类操作的
    class function ReadKeyValue(ARootKey: HKEY; AKeyPath, AName, ADefault: String): String; overload; static;
    class function ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String;  ADefault: Integer): Integer; overload; static;
    class function ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String; ADefault: boolean): boolean; overload; static;
    class function ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String; ADefault: TDateTime): TDateTime; overload; static;
    class function WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: String): boolean; overload; static;
    class function WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: Integer): boolean; overload; static;
    class function WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: TDateTime): boolean; overload; static;

    class function WriteStringList(ARootKey: HKEY; AKeyPath, AName: String; AStrings: TStrings): Boolean; static;  // Read的方法还是用ReadKeyValue

    class procedure ReadKeyNames(ARootKey: HKEY; AKeyPath: String; AOutStrings: TStringList); static;
    class function DeleteKey(ARootKey: HKEY; AKeyPath, AName: String): boolean; static;
    class function DeleteValue(ARootKey: HKEY; AKeyPath, AName: String): boolean; static;


    class function DeleteSession(ARootKey: HKEY; AName: String): Boolean; static;

  end;

implementation

{ Reg }

class function Reg.SH_GetKeyValue(vRoot: DWORD; const sKey, sName: string; const ADefault: string): string;
var
  szData: PByte;
  iLen: Cardinal;
  ret : DWORD;
begin
  Result := ADefault;
  ret := SHGetValueW(vRoot, PChar(sKey), PChar(sName), nil, nil, iLen);
  if ret <> ERROR_SUCCESS then Exit;
  if iLen = 0 then
    Exit;
  if iLen > 40 * 1024 then
    iLen := 40 * 1024;
  GetMem(szData, iLen);
  try
    ZeroMemory(szData, iLen);
    ret :=  SHGetValueW(vRoot, PChar(sKey), PChar(sName), nil, szData, iLen);
    if ret = ERROR_SUCCESS then
      Result :=  WideCharToString(PChar(szData));
  finally
    FreeMem(szData, iLen);
  end;
end;

class function Reg.SH_GetKeyValueX64(vRoot: DWORD; const sKey, sName,
  ADefault: string): string;
var
  TempKey: HKey;
  ret : HRESULT;
  DataType: Integer;
  szData: PByte;
  iLen: Integer;
begin
  Result  :=  ADefault;
  ret :=  RegOpenKeyEx(vRoot, PChar(sKey), 0, KEY_READ or KEY_WOW64_64KEY, TempKey);
  if ret = ERROR_SUCCESS then
  try
    DataType := REG_NONE;
    iLen  :=  0;
    szdata  :=  nil;
    ret := RegQueryValueEx(TempKey, PChar(sName), nil, @DataType, szdata, @ilen);
    if ret <> ERROR_SUCCESS then Exit;
    if iLen = 0 then Exit;
    GetMem(szData, iLen);
    try
      ret := RegQueryValueEx(TempKey, PChar(sName), nil, @DataType, szdata, @ilen);
      if ret = ERROR_SUCCESS then
        Result :=  WideCharToString(PChar(szData));
    finally
      FreeMem(szData, iLen);
    end;
  finally
    RegCloseKey(TempKey);
  end;
end;

class function Reg.SH_SetKeyValue(vRoot: DWORD; const sKey, sName, sValue: string): boolean;
var
  szData: PByte;
  iLen: Cardinal;
begin
  iLen := (Length(sValue) + 1) * SizeOf(Char);
  GetMem(szData, iLen);
  try
    ZeroMemory(szData, iLen);
    StringToWideChar(sValue, Pointer(szData), iLen div SizeOf(Char));
    Result := SHSetValueW(vRoot, PChar(sKey), PChar(sName), REG_EXPAND_SZ, szData, iLen) = ERROR_SUCCESS;
  finally
    FreeMem(szData, iLen);
  end;
end;

class function Reg.WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String;
  AValue: TDateTime): boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin
      try
        try
          WriteDateTime(AName, AValue);
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

class function Reg.WriteStringList(ARootKey: HKEY; AKeyPath, AName: String;
  AStrings: TStrings): Boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin
      try
        try
          WriteExpandString(AName, AStrings.Text);
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

class function Reg.SH_DelKey(vRoot: DWORD; const sKey: string): Boolean;
var
  ret : DWORD;
begin
  ret := SHDeleteKey(vRoot, PChar(sKey));
  Result  :=  ret = ERROR_SUCCESS;
end;

class function Reg.SH_DelValue(vRoot: DWORD; const sKey, sName: string): Boolean;
var
  ret : DWORD;
begin
  ret := SHDeleteValue(vRoot, PChar(sKey), PChar(sName));
  Result  :=  ret = ERROR_SUCCESS;
end;

class function Reg.SH_GetKeyIntValue(vRoot: DWORD; const sKey, sName: string;
  const ADefValue: Int64): Int64;
var
  szData: PByte;
  iLen: Cardinal;
begin
  Result := ADefValue;
  SHGetValueW(vRoot, PChar(sKey), PChar(sName), nil, nil, iLen);
  if iLen > 40 * 1024 then
    iLen := 40 * 1024;
  if (iLen <> 4) and (iLen <> 8) then
    Exit;
  GetMem(szData, iLen);
  try
    ZeroMemory(szData, iLen);
    SHGetValueW(vRoot, PChar(sKey), PChar(sName), nil, szData, iLen);
    if iLen = 4 then
      Result := PDWORD(szData)^
    else
      Result := PInt64(szData)^;
  finally
    FreeMem(szData, iLen);
  end;
end;

class function Reg.SH_SetKeyIntValue(vRoot: DWORD; const sKey, sName: string;
  const AValue: Integer): boolean;
var
  szData: PByte;
  iLen: Cardinal;
begin
  iLen := 4;
  GetMem(szData, iLen);
  try
    ZeroMemory(szData, iLen);
    PDWORD(szData)^ := AValue;
    Result := SHSetValueW(vRoot, PChar(sKey), PChar(sName), REG_DWORD, szData, iLen) = 0;
  finally
    FreeMem(szData, iLen);
  end;
end;

class function Reg.ReadKeyValue(ARootKey: HKEY; AKeyPath, AName, ADefault: String): String;
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

class function Reg.ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String;
  ADefault: Integer): Integer;
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

class function Reg.ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String;
  ADefault: boolean): boolean;
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

class function Reg.DeleteSession(ARootKey: HKEY; AName: String): Boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    try
      if KeyExists(AName ) then
        DeleteKey(AName)
    except
    end;
    Free;
  end;
end;

class function Reg.DeleteValue(ARootKey: HKEY; AKeyPath, AName: String): boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin
      try
        try
          DeleteValue(AName);
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

class procedure Reg.ReadKeyNames(ARootKey: HKEY; AKeyPath: String; AOutStrings: TStringList);
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

class function Reg.WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String; AValue: String): boolean;
begin
  Result := false;
  try
    with TRegistry.Create do
    try
      RootKey := ARootKey;
      if OpenKey(AKeyPath, True) then
      try
        WriteString(AName, AValue);
        Result := True;
      finally
        CloseKey;
      end;
    finally
      Free;
    end;
  except
  end;
end;

class function Reg.WriteKeyValue(ARootKey: HKEY; AKeyPath, AName: String;
  AValue: Integer): boolean;
begin
  Result := false;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if OpenKey(AKeyPath, True) then
    begin
      try
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

class function Reg.DeleteKey(ARootKey: HKEY; AKeyPath, AName: String): boolean;
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

class function Reg.ReadKeyValue(ARootKey: HKEY; AKeyPath, AName: String;
  ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
  with TRegistry.Create do
  begin
    RootKey := ARootKey;
    if KeyExists(AKeyPath) and OpenKeyReadOnly(AKeyPath) then
    begin
      try
        try
          Result := ReadDateTime(AName);
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

end.
