unit fcDatabase;

interface

uses
  SysUtils, ComObj, ADOInt, Variants;

type
  //数据库
  Database = record
    type
      //Access数据库
      Access = record
        class function CreateDatabase(const AFileName : string; const APassword : string = '') : boolean; static;
        class function RefreshCache(const AConnection: _Connection) : Boolean; static;
        class function CompactDatabase(const AFileName, APassWord: string): Boolean; static;
      end;
  end;

implementation

uses
  Windows, fcWindows, fcFile, uCFConsts;

{ Database.Access }

class function Database.Access.CreateDatabase(const AFileName,
  APassword: string): boolean;
var
  sTempFileName : string;
  vCatalog : OleVariant;
begin
  sTempFileName :=  Win.GetTempFileName;
  try
    vCatalog  :=  CreateOleObject('ADOX.Catalog');
    vCatalog.Create(Format(ConnectionString_Access,[sTempFileName, APassword]));
    vCatalog.ActiveConnection.Close;
    result  :=  Files.CopyFile(sTempFileName, AFileName);
    Files.DeleteFile(sTempFileName);
    vCatalog  :=  null;
  except
    result  :=  false;
  end;
end;

class function Database.Access.RefreshCache(
  const AConnection: _Connection): Boolean;
var
  vJE : OleVariant;
begin
  try
    vJE :=  CreateOleObject('JRO.JetEngine');
    OleCheck(vJE.RefreshCache(AConnection));
    Result  :=  True;
    vJE :=  Null;
  except
    Result  :=  False;
  end;
end;

class function Database.Access.CompactDatabase(const AFileName, APassWord: string): Boolean;
var
  sTempFileName: String;
  vJE: OleVariant;
begin
  sTempFileName := Win.GetTempFileName;
  DeleteFile(PWideChar(sTempFileName)); //删除Windows建立的0字节文件
  try
    vJE := CreateOleObject('JRO.JetEngine'); //建立OLE对象,函数结束OLE对象超过作用域自动释放
    OleCheck(vJE.CompactDatabase(format(ConnectionString_Access,[AFileName,APassWord]),
    Format(ConnectionString_Access,[sTempFileName,APassWord]))); //压缩数据库
    //复制并覆盖源数据库文件,如果复制失败则函数返回假,压缩成功但没有达到函数的功能
    result := Files.CopyFile(sTempFileName, AFileName);
    Files.DeleteFile(sTempFileName); //删除临时文件
    vJE :=  Null;
  except
    result:=false; //压缩失败
  end;
end;

end.
