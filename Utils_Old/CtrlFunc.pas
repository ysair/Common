unit CtrlFunc;

interface

uses
  ComCtrls;
  
function IsParentNode(const ParentNode: TTreeNode; ChildNode: TTreeNode): Boolean;

implementation

function IsParentNode(const ParentNode: TTreeNode; ChildNode: TTreeNode): Boolean;
begin
  if ParentNode = nil then
  begin
    Result := False;
    Exit;
  end;

  while ChildNode <> nil do
  begin
    if ParentNode = ChildNode.Parent then
    begin
      Result := True;
      Exit;
    end;

    ChildNode := ChildNode.Parent;
  end;

  Result := False;
end;

end.
