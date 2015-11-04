unit uXMLStorage;

interface

uses
  windows, SysUtils, Classes, XMLIntf, XMLDoc,
  uStorage;

type
  TXMLFileStorage = class(TFileStorage)
  private
    FXML : IXMLDocument;
  private
    procedure MarkRoot;
  protected
    procedure WriteValue(
        const Key : string;
        const Value : Variant
        );
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Save; override;

    procedure Write(
        const Key : string;
        const Value : string
        ); override;
    procedure Write(
        const Key : string;
        const Value : integer
        ); override;
    procedure Write(
        const Key : string;
        const Value : int64
        ); override;
    procedure Write(
        const Key : string;
        const Value : double
        ); override;
    procedure WriteDateTime(
        const Key : string;
        const Value : TDateTime
        ); override;
    procedure Write(
        const Key : string;
        const Value : boolean
        ); override;

    procedure ReadSections(
        const SectionList : TStrings
        ); override;
    procedure ReadKeys(
        const KeyList : TStrings
        ); override;

    function Read(
        const Key : string
        ): variant; override;
    function ReadString(
        const Key : string
        ): string; override;
    function ReadInteger(
        const Key : string
        ): integer; override;
    function ReadInt64(
        const Key : string
        ): int64; override;
    function ReadFloat(
        const Key : string
        ): double; override;
    function ReadDateTime(
        const Key : string
        ): TDateTime; override;
    function ReadBoolean(
        const Key : string
        ): boolean; override;

    procedure DeleteKey(
        const Key : string
        ); override;
    procedure DeleteSection(
        const Section : string
        ); override;
    procedure Clear; override;
  end;

implementation

uses Variants;

const
  SXMLRoot                = 'root';

{ TXMLFileStorage }

constructor TXMLFileStorage.Create;
begin
  inherited Create;
  FXML  :=  TXMLDocument.Create(nil);  
end;

destructor TXMLFileStorage.Destroy;
begin
  FXML  :=  nil;
  inherited;
end;

procedure TXMLFileStorage.Open;
begin
  if FileExists(FileName) then
  begin
    FXML.Active :=  False;
    FXML.LoadFromFile(FileName);
  end;
end;

procedure TXMLFileStorage.Save;
begin
  FXML.Active :=  True;
  FXML.SaveToFile(FileName);
end;

procedure TXMLFileStorage.Clear;
begin
  FXML.XML.Clear;
end;

procedure TXMLFileStorage.MarkRoot;
begin
  if FXML.XML.Count = 0 then
  begin
    FXML.Active :=  True;
    FXML.DocumentElement  :=  FXML.CreateNode(SXMLRoot);
  end;
end;

procedure TXMLFileStorage.DeleteKey(const Key: string);
var
  node  : IXMLNode;
begin
  if FXML.XML.Count = 0 then Exit;
  FXML.Active :=  True;
  node  :=  FXML.DocumentElement.ChildNodes.FindNode(Section);
  if Assigned(Node) then
  try
    Node.ChildNodes.Delete(Key);
  finally
    node  :=  nil;
  end;
end;

procedure TXMLFileStorage.DeleteSection(const Section: string);
begin
  if FXML.XML.Count = 0 then Exit;
  FXML.Active :=  True;
  FXML.DocumentElement.ChildNodes.Delete(Section);
end;

procedure TXMLFileStorage.ReadSections(const SectionList: TStrings);
var
  i : Integer;
begin
  FXML.Active :=  True;
  for i := 0 to FXML.DocumentElement.ChildNodes.Count - 1 do
    SectionList.Add(FXML.DocumentElement.ChildNodes.Nodes[i].NodeName);
end;

procedure TXMLFileStorage.ReadKeys(const KeyList: TStrings);
var
  node  : IXMLNode;
  i : Integer;
begin
  FXML.Active :=  True;
  node  :=  FXML.DocumentElement.ChildNodes.FindNode(Section);
  if Assigned(Node) then
  try
    for i := 0 to Node.ChildNodes.Count - 1 do
      KeyList.Add(Node.ChildNodes.Nodes[i].NodeName);
  finally
    node  :=  nil;
  end;
end;

function TXMLFileStorage.Read(const Key: string): variant;
var
  node  : IXMLNode;
begin
  if FXML.XML.Count = 0 then Exit;
  FXML.Active :=  True;
  node  :=  FXML.DocumentElement.ChildNodes.FindNode(Section);
  if Assigned(Node) then
  try
    Result  :=  Node.ChildValues[Key];
  finally
    node  :=  nil;
  end;
end;

procedure TXMLFileStorage.WriteValue(const Key: string;
  const Value: Variant);
var
  node  : IXMLNode;
  cnode : IXMLNode;
begin
  if Section = '' then Exit;
  MarkRoot;
  FXML.Active :=  True;
  node  :=  FXML.DocumentElement.ChildNodes.FindNode(Section);
  if not Assigned(Node) then
  begin
    node  :=  FXML.CreateNode(Section);
    FXML.DocumentElement.ChildNodes.Add(node);
  end;
  cnode :=  Node.ChildNodes.FindNode(Key);
  if not Assigned(cnode) then
  begin
    cnode :=  FXML.CreateNode(Key);
    Node.ChildNodes.Add(cnode);
  end;
  cnode.NodeValue :=  Value;
  cnode :=  nil;
  node  :=  nil;
end;

function TXMLFileStorage.ReadBoolean(const Key: string): boolean;
begin
  Result  :=  False;
  try
    Result  :=  read(Key);
  except
  end;
end;

function TXMLFileStorage.ReadDateTime(const Key: string): TDateTime;
begin
  Result  :=  0;
  try
    Result  :=  VarToDateTime(read(Key));
  except
  end;
end;

function TXMLFileStorage.ReadFloat(const Key: string): double;
begin
  Result  :=  0;
  try
    Result  :=  read(Key);
  except
  end;
end;

function TXMLFileStorage.ReadInt64(const Key: string): int64;
begin
  Result  :=  StrToIntDef(VarToStr(read(Key)), 0);
end;

function TXMLFileStorage.ReadInteger(const Key: string): integer;
begin
  Result  :=  StrToIntDef(VarToStr(read(Key)), 0);
end;

function TXMLFileStorage.ReadString(const Key: string): string;
begin
  Result  :=  VarToStrDef(read(Key), '');
end;

procedure TXMLFileStorage.Write(const Key, Value: string);
begin
  WriteValue(key, Value);
end;

procedure TXMLFileStorage.Write(const Key: string; const Value: integer);
begin
  WriteValue(key, Value);
end;

procedure TXMLFileStorage.Write(const Key: string; const Value: boolean);
begin
  WriteValue(key, Value);
end;

procedure TXMLFileStorage.Write(const Key: string; const Value: int64);
begin
  WriteValue(key, Value);
end;

procedure TXMLFileStorage.Write(const Key: string; const Value: double);
begin
  WriteValue(key, Value);
end;

procedure TXMLFileStorage.WriteDateTime(const Key: string;
  const Value: TDateTime);
begin
  WriteValue(key, Value);
end;

end.
