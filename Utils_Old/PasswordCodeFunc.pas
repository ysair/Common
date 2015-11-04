unit PasswordCodeFunc;

interface
uses Encryp;

function CodePassword(aPassword: string): string;
function UnCodePassword(aCodedPassword: string): string;

implementation

function CodePassword(aPassword: string): string;
var
  TomPassword: TTomEncryption;
begin
  TomPassword := TTomEncryption.Create(nil);

  with TomPassword do
  begin
    Key := 'Inpeta_Password_Code';

    Input := aPassword;
    action := atEncryption;
    execute;

    Result := OutPut;
  end;

  TomPassword.Free;
end;

function UnCodePassword(aCodedPassword: string): string;
var
  TomPassword: TTomEncryption;
begin
  TomPassword := TTomEncryption.Create(nil);

  with TomPassword do
  begin
    Key := 'Inpeta_Password_Code';;

    Input := aCodedPassword;
    action := atDecryption;
    execute;

    Result := Output;
  end;

  TomPassword.Free;
end;

end.
