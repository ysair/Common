//Common Framework
//¿ØÖÆµ¥Ôª
{$I CommonFramework.inc}
unit uCFController;

interface

uses
  Classes, SysUtils, Windows, Generics.Collections,
  uCFIntfDef, uCFClasses;

type
  TCFManager = class(TCFObject, ICFManager)
  private
    FIntfList : TList<ICFInterface>;
  protected
    //ICFManager
    procedure RegisterInterface(const AIntf : ICFInterface); virtual;
    procedure UnregisterInterface(const AIntf : ICFInterface); virtual;
    procedure NotifyMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TCFManager }

constructor TCFManager.Create;
begin
  inherited;
  FIntfList :=  TList<ICFInterface>.Create;
end;

destructor TCFManager.Destroy;
begin
  FIntfList.Free;
  inherited;
end;

procedure TCFManager.RegisterInterface(const AIntf: ICFInterface);
begin
  FIntfList.Add(AIntf);
end;

procedure TCFManager.UnregisterInterface(const AIntf: ICFInterface);
var
  idx : Integer;
begin
  idx :=  FIntfList.IndexOf(AIntf);
  if idx >=0 then
    FIntfList.Delete(idx);
end;

procedure TCFManager.NotifyMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
var
  i : Integer;
begin
  for i := 0 to FIntfList.Count - 1 do
  try
    FIntfList[i].OnNotifyMessage(Msg, wParam, lParam);
  except
  end;
end;

end.
