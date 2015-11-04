unit fcHzPy;

interface
uses Classes, Windows;

type
  THzPyConvert = class
  private
    FDllHandle: THandle;
    FDllFileName: string;
  public
    constructor Create(AHzpyDllFile: string);
    destructor Destroy; override;
    function CheckDllLoad: Boolean;
  public
    function ToShengMu(AInStr: string; const AIsXingMing:Boolean = false): string;   //×ª»»ÎªÉùÄ¸£¬ÔøÏªÈª ->zxq; AIsXingMing=trueÊ±£¬´Ó°Ù¼ÒÐÕÖÐ²éÕÒ¶ÁÒô
    function ToPinYin(AInStr: string; const AIsXingMing:Boolean = false): string;
    function ToFirstChar(AInStr: string): Char;  //Ê×¸öÉùÄ¸
  end;

implementation
uses SysUtils;

{ THzPyConvert }

type
  TGet_All = procedure (HzChar: PAnsiChar; SpaceChar: PAnsiChar; AOutText: PAnsiChar; const AMaxSize: Integer); stdcall;
  TGet_Header  = procedure (HzChar: PAnsiChar; AOutText: PAnsiChar; const AMaxSize: Integer); stdcall;
  TGet_FirstChar = procedure (HzChar: PAnsiChar; AOutChar: PAnsiChar); stdcall; {ÔøÏªÈª -> z£¬ ¶ÔÐÕÊÏ·¢ÒôÌØ±ð´¦Àí}
const
  C_HzPy_All = 'HzPy_All';
  C_HzPy_Header = 'HzPy_Header';
  C_XingMing_All = 'XingMing_All';
  C_XingMing_Header = 'XingMing_Header';
  C_XingMing_FirstChar = 'XingMing_FirstChar';

constructor THzPyConvert.Create(AHzpyDllFile: string);
begin
  FDllHandle := 0;
  FDllFileName := AHzpyDllFile;
end;

destructor THzPyConvert.Destroy;
begin
  if FDllHandle>0 then
    FreeLibrary(FDllHandle);
  inherited;
end;

function THzPyConvert.ToFirstChar(AInStr: string): Char;
var
  l_Func: TGet_FirstChar;
  l_Out: array [0..0] of AnsiChar;
begin
  Result := ' ';
  if not CheckDllLoad then
    Exit;

  @l_Func := GetProcAddress(FDllHandle, C_XingMing_FirstChar);
  if @l_Func <> nil then
  begin
    try
      l_Func(PAnsiChar(AnsiString(AInStr)), l_Out);
      Result := Char(l_Out[0]);
    except
    end;
  end;
end;

function THzPyConvert.ToPinYin(AInStr: string; const AIsXingMing: Boolean): string;
var
  l_Func: TGet_All;
  l_Out: array [0..1024] of AnsiChar;
begin
  Result := ' ';
  if not CheckDllLoad then
    Exit;

  if AIsXingMing then
    @l_Func := GetProcAddress(FDllHandle, C_XingMing_All)
  else
    @l_Func := GetProcAddress(FDllHandle, C_HzPy_All);
  if @l_Func <> nil then
  begin
    try
      l_Func(PAnsiChar(AnsiString(AInStr)),PAnsiChar(''), l_Out,  1024);
      Result := string(StrPas(l_Out));
    except
    end;
  end;
end;

function THzPyConvert.ToShengMu(AInStr: string; const AIsXingMing: Boolean): string;
var
  l_Func: TGet_Header;
  l_Out: array [0..1024] of AnsiChar;
begin
  Result := ' ';
  if not CheckDllLoad then
    Exit;

  if AIsXingMing then
    @l_Func := GetProcAddress(FDllHandle, C_XingMing_Header)
  else
    @l_Func := GetProcAddress(FDllHandle, C_HzPy_Header);
  if @l_Func <> nil then
  begin
    try
      l_Func(PAnsiChar(AnsiString(AInStr)), l_Out,  1024);
    except
      //FirstName="ÿffffffffffb8ÿffffffffffa3ÿffffffffffd0" »áÒì³£
    end;
    Result := string(StrPas(l_Out));
  end;
end;

function THzPyConvert.CheckDllLoad: Boolean;
begin
  if (FDllHandle=0) and FileExists(FDllFileName) then
    FDllHandle := SafeLoadLibrary(FDllFileName);

  Result := FDllHandle >0;
end;

end.
