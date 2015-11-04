unit URLCoder;

interface

function URLEncode(const ASource: string): string;

implementation

uses
  IdURI;

function URLEncode(const ASource: string): string;
begin
  Result := TIdURI.PathEncode(string(UTF8Encode(ASource)));
end;

end.