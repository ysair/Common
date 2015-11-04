
unit DBXReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs{, DesignEditors, DesignIntf};

type
  // Email Entity - Extracted from a mail type .dbx file
  PDbxEmail = ^TDbxEmailStruct;
  TDbxEmailStruct=record
    num: Integer;
    ltype: byte;
    email: Pchar;
    psubject: Pchar;
    subject: Pchar;

    messageid: Pchar;
    parent_message_ids: pchar;
    sender_name: pchar;
    sender_address: pchar;
    recip_name: pchar;

    recip_address: pchar;
    date: FILETIME;
    id: Integer;
    data_offset: Integer;
    flag: Integer;

    oe_account_name: Pchar;
    oe_account_num: Pchar;
    fetched_server: Pchar;
  end;

  TDBXReader = class
  private
    FLibHandle: THandle;   //Dll 的Handle
    FDbxHandle: Integer;   //调用dbxOpen返回的

  public
    constructor Create;
    destructor Destroy; override;
  public
    function dbxOpen(AFileName: String): Boolean;
    procedure dbxClose;
    function dbxMailCount: Integer;
    function dbxGetARawMail(AIndex: Integer): String;
  end;

  TdbxOpen = function(fname: PChar): Integer; stdcall;
  TdbxClose = function(Handle: Integer): Integer; stdcall;
  TdbxMailCount=function(Handle: Integer): Integer; stdcall;
  TdbxGetBody=function(Handle: Integer; Index: Integer):PChar; stdcall;
  TdbxFeeeBody=procedure(Item: PChar); stdcall;
  TdbxGetEmail=function(Handle: Integer; Index: Integer; flag: Integer): PDbxEmail; stdcall;
  TdbxFreeEmail=function(Handle: Integer; Email: PDbxEmail): integer; stdcall;


implementation

uses OnlineDownloadFrm;

{ TDBXReader }
constructor TDBXReader.Create;
begin
  FLibHandle := 0;
  FDbxHandle := 0;
end;

destructor TDBXReader.Destroy;
begin
  dbxClose;
  inherited;
end;


function TDBXReader.dbxOpen(AFileName: String): Boolean;
var
  l_DllFileName: String;
  aFunc: TdbxOpen;
begin
  Result := false;
  dbxClose;

  l_DllFileName := ExtractFilePath(Application.ExeName) + 'Sys\libzddbx.dll';
  if not CheckAndOnlineDownloadAFile(nil,l_DllFileName, True) then
    Exit;

  FLibHandle := LoadLibrary(PChar(l_DllFileName));
  if FLibHandle <=0 then
    Exit;

  @aFunc := GetProcAddress(FLibHandle, 'dbxOpen');
  if @aFunc <> nil then
    FDbxHandle := aFunc(PChar(AFileName));

  Result := FDbxHandle > 0;
end;

procedure TDBXReader.dbxClose;
var
  aFunc: TdbxClose;
begin
  if (FLibHandle > 0) and (FDbxHandle >0) then
  begin
    @aFunc := GetProcAddress(FLibHandle, 'dbxClose');
    if @aFunc <> nil then
      aFunc(FDbxHandle);
  end;

  if FLibHandle > 0 then
    FreeLibrary(FLibHandle);

  FDbxHandle := 0;
  FLibHandle := 0;
end;

function TDBXReader.dbxGetARawMail(AIndex: Integer): String;
var
  aEmail: PDbxEmail;
  aGetEmail: TdbxGetEmail;
  aFreeEmail: TdbxFreeEmail;
{  TdbxGetEmail=function(Handle: Integer; Index: Integer; flag: Integer): PDbxEmail; stdcall;
  TdbxFreeEmail=function(Handle: Integer; Email: PDbxEmail): integer; stdcall;}
begin
  Result := '';
  if (FLibHandle<=0) or (FDbxHandle<=0) then
    Exit;

  @aGetEmail := GetProcAddress(FLibHandle, 'dbxGetEmail');
  if @aGetEmail <> nil then
  begin
    aEmail := aGetEmail(FDbxHandle, AIndex, 1);
    if aEmail <> nil then
    begin
      Result := aEmail^.email;
      @aFreeEmail := GetProcAddress(FLibHandle, 'dbxFreeEmail');
      if @aFreeEmail <> nil then
        aFreeEmail(FDbxHandle, aEmail);
    end; 
  end;
end;

function TDBXReader.dbxMailCount: Integer;
var
  aFunc: TdbxMailCount;
begin
  Result := 0;
  if (FLibHandle<=0) or (FDbxHandle<=0) then
    Exit;

  @aFunc := GetProcAddress(FLibHandle, 'dbxMailCount');
  if @aFunc <> nil then
    Result := aFunc(FDbxHandle);
end;


end.



