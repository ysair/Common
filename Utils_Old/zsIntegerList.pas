unit zsIntegerList;

interface

uses
  Classes;

type
  TIntegerList = class(TList)
  protected
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; Item: Integer);
  public
    function Add(Item: Integer): Integer;
    function First: Integer;
    function IndexOf(Item: Integer): Integer;
    procedure Insert(Index: Integer; Item: Integer);
    function Last: Integer;
    function Remove(Item: Integer): Integer;
    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

implementation

function TIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;

function TIntegerList.First: Integer;
begin
  result := Integer( inherited First);
end;

function TIntegerList.Get(Index: Integer): Integer;
begin
  result := Integer( inherited Get(Index));
end;

function TIntegerList.IndexOf(Item: Integer): Integer;
begin
  result := inherited IndexOf(Pointer(Item));
end;

procedure TIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, Pointer(Item))
end;

function TIntegerList.Last: Integer;
begin
  result := Integer( inherited Last);
end;

procedure TIntegerList.Put(Index, Item: Integer);
begin
  inherited Put(Index, Pointer(Item));
end;

function TIntegerList.Remove(Item: Integer): Integer;
begin
  result := inherited Remove(Pointer(Item));
end;

end.
