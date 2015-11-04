unit fcHash;

interface

uses
  Classes, SysUtils;

type
  Hash = record
    class function MD5Str(const AStr: string): string; static;
    class function MD5RawStr(const AStr: RawByteString): string; static;
    class function MD5File(const AFileName: string): string; static;
    class function MD5Stream(const AStream: TStream): string; static;
    class function MD5Buffer(const ABuffer; const ASize: Integer): string; static;
  end;

implementation

uses
  md5;

{ Hash }

class function Hash.MD5Str(const AStr: string): string;
begin
  Result := MD5DigestToStr(md5.MD5String(AStr));
end;

class function Hash.MD5File(const AFileName: string): string;
begin
  try
    Result := MD5DigestToStr(md5.MD5File(AFileName));
  except
    Result := '';
  end;
end;

class function Hash.MD5RawStr(const AStr: RawByteString): string;
var
  bt : TBytes;
begin
  bt  :=  BytesOf(AStr);
  Result  :=  MD5Buffer(bt[0], High(bt) - Low(bt) + 1);
end;

class function Hash.MD5Stream(const AStream: TStream): string;
begin
  Result := MD5DigestToStr(md5.MD5Stream(AStream));
end;

class function Hash.MD5Buffer(const ABuffer; const ASize: Integer): string;
begin
  Result := MD5DigestToStr(md5.MD5Buffer(ABuffer, ASize));
end;

end.
