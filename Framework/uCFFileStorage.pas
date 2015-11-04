//Common Framework
//XML²Ù×÷µ¥Ôª
{$I CommonFramework.inc}
unit uCFFileStorage;

interface

uses
  SysUtils, Classes, XMLDoc, XMLIntf, typinfo, Variants, StrUtils, IniFiles,
  uCFIntfDef, uCFClasses;

type
  TCFStorage = class(TCFPersistentObject)
  protected
    function SaveString(const ASection : string; const AKey : string; const AValue : string):boolean; virtual; abstract;
    function LoadString(const ASection : string; const AKey : string):string; virtual; abstract;
    function DeleteString(const ASection : string; const AKey : string):boolean; virtual; abstract;
    function DeleteSection(const ASection : string):boolean; virtual; abstract;
    function GetSectionKeys(const ASection : string; const AKeyList : TStrings):Integer; virtual; abstract;
  end;

  TCFFileStorage = class(TCFStorage, ICFFileStorage)
  private
    FFileName: string;
  public
    property FileName : string read FFileName;
  public
    constructor Create(const AFileName : string); reintroduce; virtual;
  end;

  TCFIniStorage = class(TCFFileStorage)
  private
    FIniFile : TMemIniFile;
    FAutoSave: Boolean;
  protected
    function SaveString(const ASection : string; const AKey : string; const AValue : string):boolean; override;
    function LoadString(const ASection : string; const AKey : string):string; override;
    function DeleteString(const ASection : string; const AKey : string):boolean; override;
    function DeleteSection(const ASection : string):boolean; override;
    function GetSectionKeys(const ASection : string; const AKeyList : TStrings):Integer; override;
    procedure DoAutoSave;
  public
    property AutoSave : Boolean read FAutoSave write FAutoSave;
  public
    constructor Create(const AFileName : string); override;
    destructor Destroy; override;
  end;

implementation


{ TCFFileStorage }

constructor TCFFileStorage.Create(const AFileName: string);
begin
  inherited Create;
  FFileName :=  AFileName;
end;

{ TCFIniStorage }

constructor TCFIniStorage.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FIniFile  :=  TMemIniFile.Create(AFileName);
end;

destructor TCFIniStorage.Destroy;
begin
  FIniFile.UpdateFile;
  FIniFile.Free;
  inherited;
end;

procedure TCFIniStorage.DoAutoSave;
begin
  if AutoSave then
    FIniFile.UpdateFile;
end;

function TCFIniStorage.DeleteSection(const ASection: string): boolean;
begin
  FIniFile.EraseSection(ASection);
  DoAutoSave;
  Result  :=  True;
end;

function TCFIniStorage.DeleteString(const ASection, AKey: string): boolean;
begin
  FIniFile.DeleteKey(ASection, AKey);
  DoAutoSave;
  Result  :=  True;
end;

function TCFIniStorage.GetSectionKeys(const ASection: string;
  const AKeyList: TStrings): Integer;
begin
  FIniFile.ReadSection(ASection, AKeyList);
  Result  :=  AKeyList.Count;
end;

function TCFIniStorage.LoadString(const ASection, AKey: string): string;
begin
  Result  :=  FIniFile.ReadString(ASection, AKey, '');
end;

function TCFIniStorage.SaveString(const ASection, AKey,
  AValue: string): boolean;
begin
  FIniFile.WriteString(ASection, AKey, AValue);
  DoAutoSave;
  Result  :=  True;
end;

end.
