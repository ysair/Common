unit PluginFunc;

interface

function ConfigDir: string;

function RootDir: string;

procedure WriteLog(strlog: string);

implementation

uses
  Windows, AnchorFunc, SysUtils;

var
  CritSect: TRTLCriticalSection;

function ConfigDir: String;
begin
  Result := AnchorFunc.ApplicationDir;
end;

function RootDir: string;
begin
  if (ModuleIsLib) then
    Result := ExtractFileDir(ExtractFileDir(ConfigDir))
  else
    Result := ConfigDir;
end;

procedure WriteLog(strlog: string);
var
  HFile: TextFile;
  strFile, strMsg: string;
begin
  strFile := ExtractFilePath(paramstr(0)) + 'logs\';
  if not DirectoryExists(strFile) then
    CreateDir(strFile);
  try
    strFile := strFile + FormatDateTime('yyyymmdd', Date()) + '.log';

    EnterCriticalSection(CritSect);
    try
      AssignFile(HFile, strFile);
      try
        if FileExists(strFile) then
          Append(HFile)
        else
          ReWrite(HFile);
        strMsg := FormatDateTime('hh:MM:ss = ', Time()) + strlog;
        writeln(HFile, strMsg);
      finally
        closefile(HFile);
      end;
    finally
      LeaveCriticalSection(CritSect);
    end;
  except
  end;
end;

initialization

InitializeCriticalSection(CritSect);

finalization

DeleteCriticalSection(CritSect);

end.
