unit fcSystem;

interface

uses
  SysUtils;

type
  FCSysUtils = record
    class function DoubleToBytes(const AValue:Double):TBytes;static;
    class function BytesToDouble(const AValue:TBytes):Double;static;
  end;

  Key = record
    class function KeyPress(AKey: LongInt): Boolean; static;
    class function CtrlPress: Boolean; static;
    class function ShiftPress: Boolean; static;
  end;


implementation

uses
  Windows;

class function FCSysUtils.DoubleToBytes(const AValue:Double):TBytes;
var
  buf:array [0..7] of Byte;
begin
  fillchar(buf, 8, 0);
  Move(AValue, buf[0], 8);

  SetLength(Result, 8);
  CopyMemory(@Result[0], @buf[0], 8 );
end;

class function FCSysUtils.BytesToDouble(const AValue:TBytes):Double;
var
  len:Integer;
begin
  len := SizeOf(Result);
  fillchar(Result, len, 0);
  Move(AValue[0], Result, len);
end;

{ Key }

class function Key.CtrlPress: Boolean;
begin
  Result := KeyPress(VK_CONTROL);
end;

class function Key.KeyPress(AKey: Integer): Boolean;
begin
  Result := GetKeyState(AKey) < 0;
end;

class function Key.ShiftPress: Boolean;
begin
  Result := KeyPress(VK_SHIFT);
end;

end.
