unit uAPIHook;

interface

uses
  SysUtils, Windows, Messages, classes;

type
  TImportCode = packed record
    JumpInstruction: Word; //是$25FF，JUMP指令
    FuncAddress: PPointer;//真正开始的地址
  end;
  PImportCode = ^TImportCode;

  TCreateFileA = function(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
      lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
      hTemplateFile: THandle): THandle; stdcall;
  TCreateFileW = function(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
      lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
      hTemplateFile: THandle): THandle; stdcall;

implementation

var
  CodeCreateFileA : PImportCode;
  CodeCreateFileW : PImportCode;
  OldCreateFileA : TCreateFileA;
  OldCreateFileW : TCreateFileW;

function TrueFunctionAddress(func: Pointer): Pointer;
var
  Code: PImportCode;
Begin
  Result:= func;
  if func = nil then exit;
  try
    Code := func;
    if (Code.JumpInstruction = $25FF) then
    begin
      Result := Code.FuncAddress^;
    end;
  except
    Result :=nil;
  end;
end;

Procedure PermuteFunction(OldFunc:Pointer; NewFunc:Pointer);
var
  written: DWORD;
begin
  WriteProcessMemory(GetCurrentProcess,OldFunc,@NewFunc, 4,written);
end;

function MyCreateFileA(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
begin
  Result  :=  OldCreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end;

function MyCreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
begin
  Result  :=  OldCreateFileW(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end;

procedure API_Hookup;
begin
  CodeCreateFileA :=  @CreateFileA;
  CodeCreateFileW :=  @CreateFileW;
  if @OldCreateFileA = nil then
    @OldCreateFileA :=  TrueFunctionAddress(@CreateFileA);
  if @OldCreateFileW = nil then
    @OldCreateFileW :=  TrueFunctionAddress(@CreateFileW);
  PermuteFunction(CodeCreateFileA.FuncAddress, @MyCreateFileA);
  PermuteFunction(CodeCreateFileW.FuncAddress, @MyCreateFileW);
end;

procedure Un_API_Hook;
begin
  if @OldCreateFileA <> nil then
    PermuteFunction(CodeCreateFileA.FuncAddress, @OldCreateFileA);
  if @OldCreateFileW <> nil then
    PermuteFunction(CodeCreateFileW.FuncAddress, @OldCreateFileW);
end;

initialization
  API_Hookup;
finalization
  Un_API_Hook;

end.
