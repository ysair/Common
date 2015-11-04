unit RdpCrypt;

interface

uses
  Windows, SysUtils;

const
  CRYPTPROTECT_LOCAL_MACHINE = $4;
  CRYPTPROTECT_UI_FORBIDDEN = $1;

type
  PVOID = Pointer;
  {$EXTERNALSYM PVOID}
  PPVOID = ^PVOID;
  {$EXTERNALSYM PPVOID}
  PVOID64 = Pointer;
  {$EXTERNALSYM PVOID64}

type
   PBOOL = {$IFDEF USE_DELPHI_TYPES} Windows.PBOOL {$ELSE} ^BOOL {$ENDIF};
  {$EXTERNALSYM PBOOL}
  LPBOOL = {$IFDEF USE_DELPHI_TYPES} Windows.PBOOL {$ELSE} ^BOOL {$ENDIF};
  {$EXTERNALSYM LPBOOL}
  PBYTE = {$IFDEF USE_DELPHI_TYPES} Windows.PBYTE {$ELSE} ^Byte {$ENDIF};
  {$EXTERNALSYM PBYTE}
  LPBYTE = {$IFDEF USE_DELPHI_TYPES} Windows.PBYTE {$ELSE} ^Byte {$ENDIF};
  {$EXTERNALSYM LPBYTE}

type
  _CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PBYTE; //LPBYTE
  end;

  PDATA_BLOB = ^_CRYPTOAPI_BLOB;
  DATA_BLOB = _CRYPTOAPI_BLOB;

type
  PCRYPTPROTECT_PROMPTSTRUCT = ^CRYPTPROTECT_PROMPTSTRUCT;
{$EXTERNALSYM PCRYPTPROTECT_PROMPTSTRUCT}

  _CRYPTPROTECT_PROMPTSTRUCT = record
    cbSize: DWORD;
    dwPromptFlags: DWORD;
    hwndApp: HWND;
    szPrompt: LPCWSTR;
  end;
{$EXTERNALSYM _CRYPTPROTECT_PROMPTSTRUCT}

  CRYPTPROTECT_PROMPTSTRUCT = _CRYPTPROTECT_PROMPTSTRUCT;
{$EXTERNALSYM CRYPTPROTECT_PROMPTSTRUCT}
  TCryptProtectPromptStruct = CRYPTPROTECT_PROMPTSTRUCT;
  PCryptProtectPromptStruct = PCRYPTPROTECT_PROMPTSTRUCT;

function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: LPCWSTR;
  pOptionalEntropy: PDATA_BLOB; pvReserved: PVOID;
  pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall; external 'Crypt32.dll';

function CryptUnprotectData(pDataIn: PDATA_BLOB; ppszDataDescr: LPWSTR;
  pOptionalEntropy: PDATA_BLOB; pvReserved: PVOID;
  pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall; external 'Crypt32.dll';

function CryptRDPPassword(sPassword: string): string;
function DecryptRDPPassword(sPasswordHash: string): string;
function BlobDataToHexStr(P: PByte; I: Integer): string;
function PasswordHashToBlobData(sPasswordHash: string): DATA_BLOB;

implementation

{ *********************************************************** }
{ HexToByte: Converts Hex value to Byte }
{ Found this somewhere on the internet }
{ *********************************************************** }
function HexToByte(s: String): Byte;
const
  cs = '0123456789ABCDEF';
begin
  if (length(s) = 2) and
    (AnsiChar(s[1]) in ['0' .. '9', 'A' .. 'F']) and
    (AnsiChar(s[2]) in ['0' .. '9', 'A' .. 'F']) then
    result := ((pos(s[1], cs) - 1) * 16) + (pos(s[2], cs) - 1)
  else
    raise EConvertError.CreateFmt('%s is not a Hexformatstring', [s]);
end;

{ *********************************************************** }
{ PasswordHashToBlobData: Converts a RDP password Hash to }
{ a DATA_BLOB structure }
{ sPasswordHash : RDP Password Hash (HEX String }
{ *********************************************************** }
function PasswordHashToBlobData(sPasswordHash: string): DATA_BLOB;
var
  Buf: array of Byte;
  dwBufSize: Cardinal;
  I: Cardinal;
  j: Cardinal;
  dwHashSize: Cardinal;
begin
  dwBufSize := length(sPasswordHash) DIV 2;
  dwHashSize := length(sPasswordHash);
  SetLength(Buf, dwBufSize);

  I := 1;
  j := 0;
  while I < dwHashSize do
  begin
    Buf[j] := HexToByte(sPasswordHash[I] + sPasswordHash[I + 1]);
    Inc(I, 2);
    Inc(j);
  end;

  GetMem(result.pbData, dwBufSize);
  result.cbData := dwBufSize;
  result.pbData := PByte(Buf);
end;

{ *********************************************************** }
{ BlobDataToHexStr: Converts a PByte from a DATA_BLOB }
{ to a Hex String so it can be saved in }
{ an RDP file }
{ P : PByte (pbData) from DATA_BLOB }
{ I : Integer (cbData) from DATA_BLOB }
{ *********************************************************** }
function BlobDataToHexStr(P: PByte; I: Integer): string;
var
  HexStr: string;
begin
  HexStr := '';
  while (I > 0) do
  begin
    Dec(I);
    HexStr := HexStr + IntToHex(P^, 2);
    Inc(P);
  end;
  result := HexStr;
end;

{ *********************************************************** }
{ CryptRDPPassword: Converts a plaintext password to }
{ encrypted password hash }
{ an RDP file }
{ sPassword: plaintext password }
{ *********************************************************** }
function CryptRDPPassword(sPassword: string): string;
var
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  pwDescription: PWideChar;
  PwdHash: string;
begin
  PwdHash := '';

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  // RDP uses UniCode
  DataIn.pbData := Pointer(WideString(sPassword));
  DataIn.cbData := length(sPassword) * SizeOf(WChar);

  // RDP always sets description to psw
  pwDescription := WideString('');

  if CryptProtectData(
    @DataIn,
    pwDescription,
    nil,
    nil,
    nil,
    CRYPTPROTECT_UI_FORBIDDEN or CRYPTPROTECT_LOCAL_MACHINE, // Never show interface
    @DataOut) then
  begin
    PwdHash := BlobDataToHexStr(DataOut.pbData, DataOut.cbData);
  end;
  result := PwdHash;

  // Cleanup
  LocalFree(Cardinal(DataOut.pbData));
  LocalFree(Cardinal(DataIn.pbData));

end;

{ *********************************************************** }
{ DecryptRDPPassword: Converts an RDP Password Hash back }
{ to it's original password. }
{ Note that this only works for the user }
{ who encrypted the password (or on the }
{ same computer in case it was encrypted }
{ with the computerkey }
{ sPasswordHash: Password hash (string) }
{ *********************************************************** }
function DecryptRDPPassword(sPasswordHash: string): string;
var
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  sPassword: string;
  pwDecrypted: PWideChar;
  pwDescription: PWideChar;
begin

  DataIn := PasswordHashToBlobData(sPasswordHash);

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  if CryptUnprotectData(@DataIn,
    @pwDescription,
    nil,
    nil,
    nil,
    CRYPTPROTECT_UI_FORBIDDEN or CRYPTPROTECT_LOCAL_MACHINE, // Never show interface
    @DataOut) then
  begin
    GetMem(pwDecrypted, DataOut.cbData);
    lstrcpynW(pwDecrypted, PWideChar(DataOut.pbData),
      (DataOut.cbData DIV 2) + 1);
    sPassword := pwDecrypted;
    FreeMem(pwDecrypted);
  end
  else
  begin
    raise EConvertError.CreateFmt('Error decrypting: %s',
      [SysErrorMessage(GetLastError)]);
  end;

  result := sPassword;

  // Cleanup
  LocalFree(Cardinal(DataIn.pbData));
  if DataOut.cbData > 0 then
  begin
    LocalFree(Cardinal(DataOut.pbData));
  end;
end;

end.