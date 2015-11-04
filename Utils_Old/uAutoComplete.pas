unit uAutoComplete;

interface

uses
  Windows, SysUtils, Controls, Classes, ActiveX, ComObj, ShlObj;

const
  IID_IAutoComplete: TGUID = '{00bb2762-6a77-11d0-a535-00c04fd7d062}';
  IID_IAutoComplete2: TGUID = '{EAC04BC0-3791-11d2-BB95-0060977B464C}';
  CLSID_IAutoComplete: TGUID = '{00BB2763-6A77-11D0-A535-00C04FD7D062}';

  {Options for IAutoComplete2}
  ACO_NONE = 0;
  ACO_AUTOSUGGEST = $1;
  ACO_AUTOAPPEND = $2;
  ACO_SEARCH = $4;
  ACO_FILTERPREFIXES = $8;
  ACO_USETAB = $10;
  ACO_UPDOWNKEYDROPSLIST = $20;
  ACO_RTLREADING = $40;

  SHACF_DEFAULT             = $00000000;
  SHACF_FILESYSTEM          = $00000001;
  SHACF_URLHISTORY          = $00000002;
  SHACF_URLMRU              = $00000004;
  SHACF_URLALL              = SHACF_URLHISTORY or SHACF_URLMRU;
  SHACF_USETAB              = $00000008;
  SHACF_FILESYS_ONLY        = $00000010;
  SHACF_AUTOSUGGEST_FORCE_ON= $10000000;
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;
  SHACF_AUTOAPPEND_FORCE_OFF= $80000000;
  

type

  IAutoComplete = interface(IUnknown)
    ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit: HWND; const punkACL: IUnknown; pwszRegKeyPath,
      pwszQuickComplete: POLESTR): HResult; stdcall;
    function Enable(fEnable: BOOL): HResult; stdcall;
  end;

  IAutoComplete2 = interface(IAutoComplete)
    ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

  TEnumString = class(TInterfacedObject, IEnumString)
  private
    FStrings : TStrings;
    FIndex   : Integer;
  protected
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
  public
    constructor Create(Strings: TStrings);
  end;

procedure SetAutoCompleteControl(
    const AControl  : TWinControl;
    const AList     : TStrings
    );overload;
procedure SetAutoCompleteControl(
    const AControlHandle  : HWND;
    const AList     : TStrings
    );overload;

function SHAutoComplete(hWnd: HWND; dwFlags: DWORD): LRESULT; stdcall;

implementation

function SHAutoComplete; external 'shlwapi.dll' name 'SHAutoComplete';

procedure SetAutoCompleteControl(
    const AControl  : TWinControl;
    const AList     : TStrings
    );overload;
begin
  SetAutoCompleteControl(AControl.Handle, AList);
end;

procedure SetAutoCompleteControl(
    const AControlHandle  : HWND;
    const AList     : TStrings
    );overload;
var
  FAutoComplete : IAutoComplete2;
  FStrings : TEnumString;
begin
  FAutoComplete := CreateComObject(CLSID_IAutoComplete) as IAutoComplete2;
  FStrings :=  TEnumString.Create(AList);
  OleCheck(FAutoComplete.SetOptions(
      ACO_AUTOSUGGEST or ACO_AUTOAPPEND or ACO_UPDOWNKEYDROPSLIST or ACO_USETAB
      ));
  OleCheck(FAutoComplete.Init(AControlHandle, FStrings, nil, nil));
end;

{ IUnknownInt }

function TEnumString.Clone(out enm: IEnumString): HResult;
begin
  Result := E_NOTIMPL;
  pointer(enm) := nil;
end;

constructor TEnumString.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

function TEnumString.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
  wStr: WideString;
begin
  I := 0;
  while (I < celt) and (FIndex < FStrings.Count) do
  begin 
    wStr := FStrings[FIndex];
    TPointerList(elt)[I] := CoTaskMemAlloc(2 * (Length(wStr) + 1)); 
    StringToWideChar(wStr, TPointerList(elt)[I], 2 * (Length(wStr) + 1)); 
    Inc(I); 
    Inc(FIndex);
  end; 
  if pceltFetched <> nil then 
    pceltFetched^ := I; 
  if I = celt then 
    Result := S_OK 
  else 
    Result := S_FALSE; 
end; 

function TEnumString.Reset: HResult; 
begin 
  FIndex := 0;
  Result := S_OK; 
end; 

function TEnumString.Skip(celt: Integer): HResult; 
begin 
  if (FIndex + celt) <= FStrings.Count then
  begin 
    Inc(FIndex, celt);
    Result := S_OK;
  end 
  else 
  begin 
    FIndex := FStrings.Count;
    Result := S_FALSE; 
  end; 
end; 

initialization
  Oleinitialize(nil);

finalization
  OleUninitialize;

end.