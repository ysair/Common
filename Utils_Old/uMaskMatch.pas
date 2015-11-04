unit uMaskMatch;

interface

uses
  SysUtils, Classes;

function MatchFileName(const AFileName, APatterns: string): Boolean;

implementation

function MaskMatch(const APattern, ASource: string): Boolean;
var
  StringPtr, PatternPtr: PChar;
  StringRes, PatternRes: PChar;
begin
  Result := False;
  StringPtr := PChar(LowerCase(ASource));
  PatternPtr := PChar(LowerCase(APattern));
  StringRes := nil;
  PatternRes := nil;
  repeat
    repeat                                                  // ohne vorangegangenes "*"
      case PatternPtr^ of
        #0:
          begin
            Result := StringPtr^ = #0;
            if Result or (StringRes = nil) or (PatternRes = nil) then
              Exit;
            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
            Break;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat                                                  // mit vorangegangenem "*"
      case PatternPtr^ of
        #0:
          begin
            Result := True;
            Exit;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes := StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;
  until False;
end;

function MatchFileName(const AFileName, APatterns: string): Boolean;
var
  i : Integer;
  lst : TStrings;
begin
  Result  :=  False;
  lst :=  TStringList.Create;
  try
    ExtractStrings([';'], [], PChar(APatterns), lst);
    for i := 0 to lst.Count - 1 do
      if MaskMatch(lst[i], AFileName) then
      begin
        Result  :=  True;
        break;
      end;
  finally
    lst.Free;
  end;
end;

end.
