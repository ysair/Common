unit uRPNParser;
//Reverse Polish Notation Parser
//by ysai
//use case
//  ParserExpression('a*(b-c)')    ParsedExpression:a b c - *
//  CalcExpression('10/(2+3)')     ReturnValue:2

interface

uses
  SysUtils, Classes, Math, Types, StrUtils;

type
  TDoubleArray = array of Double;
  TOnGetParamValue = function(const AParamName: string): Double of object;
  TOnGetFuncValue = function(const AFuncName : string; AParams : TDoubleArray) : Double of object;

  TRPNParser = class
  private
    FExpression: WideString;
    FParsed : boolean;
    FReturnValue: Double;
    FStack : TStrings;
    FCache : TStrings;
    FLastErrorMessage: string;
    FOnGetFuncValue: TOnGetFuncValue;
    FOnGetParamValue: TOnGetParamValue;
    procedure SetExpression(const Value: WideString);
    function GetParsedExpression: string;
  public
    constructor Create();
    destructor Destroy; override;

    function Parse : boolean; overload;
    function Parse(AExpression : WideString) : boolean; overload;
    function Calc : boolean; overload;
    function Calc(AExpression : WideString) : boolean; overload;
  public
    property Expression : WideString read FExpression write SetExpression;
    property ParsedExpression : string read GetParsedExpression;
    property Parsed : Boolean read FParsed;
    property ReturnValue : Double read FReturnValue;
    property LastErrorMessage : string read FLastErrorMessage;
    property OnGetParamValue : TOnGetParamValue read FOnGetParamValue write FOnGetParamValue;
    property OnGetFuncValue : TOnGetFuncValue read FOnGetFuncValue write FOnGetFuncValue;
  end;

implementation

function CharInSet(C: WideChar; const CharSet: TSysCharSet): boolean;
var
  s: string;
begin
  s := WideCharLenToString(PWideChar(@C), 1);
  result := (Length(s) = 1) and (s[1] in CharSet);
end;

function OptPriority(C: WideString) : integer;
begin
  Result  :=  0;
  if C = '|' then   //or
    Result  :=  1
  else if C = '&' then  //and
    Result  :=  2
  else if (C = '>') or (C = '<') or (C = '>=') or (C = '<=') or (C = '=') then
    Result  :=  3
  else if (C = '+') or (C = '-') then
    Result  :=  4
  else if (C = '*') or (C = '/') or (C = '%') then
    Result  :=  5
  else if (C = '^') then //power
    Result  :=  6;
end;

function IsOperator(C : WideString) : boolean;
begin
  Result  := OptPriority(C) > 0;
end;

{ TRPNParser }

constructor TRPNParser.Create;
begin
  inherited;
  FStack := TStringList.Create;
  FCache := TStringList.Create;
end;

destructor TRPNParser.Destroy;
begin
  FStack.Free;
  FCache.Free;
  inherited;
end;

procedure TRPNParser.SetExpression(const Value: WideString);
begin
  FExpression := StringReplace(Trim(Value), ' ', '', [rfReplaceAll]);
  if (Length(FExpression) > 1)
      and (FExpression[1] = '(') and (RightStr(FExpression, 1) = ')') then
  begin  //去掉多余括号
    Delete(FExpression, 1, 1);
    Delete(FExpression, Length(FExpression), 1);
  end;
  FParsed :=  False;
end;

function TRPNParser.Parse: boolean;
var
  charIdx : Integer;
  curObj : WideString;
  lastOperator : WideString;
  fc : array of integer;

  function _Next : boolean;
  var
    i : Integer;
    op : boolean;
  begin
    curObj :=  '';
    op  :=  False;
    for i := charIdx to Length(FExpression) do
      if IsOperator(FExpression[i]) or CharInSet(FExpression[i], ['(', ')', ',']) then
      begin  //操作符
        if curObj = '' then
        begin
          curObj := FExpression[i];
          inc(charIdx);
          if (curObj <> '(') and (lastOperator <> '') and
              not ((RightStr(lastOperator, 1) = '(')
              and (curObj = ')'))then
            if curObj = '-' then
            begin
              Insert('0', FExpression, i);
              curObj  :=  '0';
              lastOperator  :=  '';
              Result  :=  true;
              Exit;
            end
            else begin
              FLastErrorMessage :=  '解析错误 - 操作符错误.';
              FParsed :=  false;
              Result  :=  false;
              Exit;
            end;
          if (curObj = '<') or (curObj = '>') then
            if (i + 1 < Length(FExpression)) and (FExpression[i + 1] = '=') then
            begin
              curObj  :=  curObj + '=';
              inc(charIdx);
            end;
          if curObj <> ')' then
            op  :=  true;
        end
        else if FExpression[i] = '(' then
        begin
          curObj := curObj + FExpression[i];
          inc(charIdx);
          op  :=  true;
        end;
        break;
      end
      else begin
        curObj := curObj + FExpression[i];
        inc(charIdx);
      end;
    if op then
      lastOperator  :=  curObj
    else
      lastOperator  :=  '';
    Result  :=  curObj <> '';
  end;

  procedure _AddParamCount(const ACheckZero : boolean = false);
  begin
    if Length(fc) > 0 then
      if not ACheckZero or (fc[Length(fc) - 1] = 0) then
        fc[Length(fc) - 1]  :=  fc[Length(fc) - 1] + 1;
  end;

begin
  if Parsed then
  begin
    Result  :=  Parsed;
    Exit;
  end;
  FStack.Clear;
  FCache.Clear;
  charIdx :=  1;
  SetLength(fc, 0);
  FParsed :=  true;
  lastOperator  :=  '(';
  while _Next do
  begin
    if curObj = '(' then //括号开始
      FCache.Add(curObj)
    else if RightStr(curObj, 1) = '(' then
    begin  //函数开始
      _AddParamCount(true);
      FCache.Add(curObj);
      SetLength(fc, Length(fc) + 1);
      fc[Length(fc) - 1]  :=  0;
    end
    else if curObj = ')' then
    begin //括号/函数结束
      while (FCache.Count > 0) and (RightStr(FCache[FCache.Count - 1], 1) <> '(') do
      begin
        if FCache[FCache.Count - 1] = ',' then
          _AddParamCount
        else
          FStack.Add(FCache[FCache.Count - 1]);
        FCache.Delete(FCache.Count - 1);
      end;
      if FCache.Count > 0 then
      begin
        if FCache[FCache.Count - 1] <> '(' then
        begin
          FStack.Add(FCache[FCache.Count - 1] + IntToStr(fc[Length(fc) - 1]) + ')');
          SetLength(fc, Length(fc) - 1);
        end;
        FCache.Delete(FCache.Count - 1);
      end
      else begin
        FLastErrorMessage :=  '解析错误 - 括号不匹配.';
        FParsed :=  false;
      end;
    end
    else if IsOperator(curObj[1]) or (curObj = ',') then
    begin //操作符/,
      while (FCache.Count > 0)
          and (IsOperator(FCache[FCache.Count - 1]) or (FCache[FCache.Count - 1] = ','))
          and (OptPriority(curObj) <= OptPriority(FCache[FCache.Count - 1])) do
      begin
        FStack.Add(FCache[FCache.Count - 1]);
        FCache.Delete(FCache.Count - 1);
      end;
      if curObj = ',' then
        _AddParamCount
      else
        FCache.Add(curObj);
    end
    else begin  //值/参数
      _AddParamCount(true);
      FStack.Add(curObj);
    end;
  end;
  if FParsed then
    while FCache.Count > 0 do
    begin
      FStack.Add(FCache[FCache.Count - 1]);
      FCache.Delete(FCache.Count - 1);
    end;
  Result  :=  Parsed;
end;

function TRPNParser.Parse(AExpression: WideString): boolean;
begin
  Expression  :=  AExpression;
  Result  :=  Parse;
end;

function TRPNParser.Calc: Boolean;
var
  arr : array of Double;
  idx : integer;

  procedure Push(AValue : Double);
  begin
    inc(idx);
    arr[idx]  :=  AValue;
  end;

  function Pop : Double;
  begin
    if idx < 0 then
    begin
      FLastErrorMessage :=  '计算错误 - 表达式错误.';
      Result  :=  0;
    end
    else begin
      Result  :=  arr[idx];
      dec(idx);
    end;
  end;

var
  i, j : Integer;
  s : WideString;
  v, v1, v2 : Double;
  pc : Integer;
  params : TDoubleArray;
begin
  Result  :=  False;
  if not Parsed then
    Parse;
  if not Parsed then
    Exit;

  SetLength(arr, FStack.Count);
  idx :=  -1;
  for i := 0 to FStack.Count - 1 do
  begin
    s :=  FStack[i];
    if IsOperator(S) then
    begin  //操作符
      v2  :=  Pop;
      v1  :=  Pop;
      case s[1] of
        '+' : v :=  v1 + v2;
        '-' : v :=  v1 - v2;
        '*' : v :=  v1 * v2;
        '/' :
          if v2 = 0 then
          begin
            FLastErrorMessage :=  '计算错误 - 除数不能为0.';
            Exit;
          end
          else
            v :=  v1 / v2;
        '^' : v :=  Power(v1, Round(v2));
        '%' : v :=  Round(v1) mod Round(v2);
        '=' : v :=  IfThen(CompareValue(v1, v2)=EqualsValue, 1, 0);
        '>' :
          if s = '>=' then
            v :=  IfThen(CompareValue(v1, v2)>LessThanValue, 1, 0)
          else
            v :=  IfThen(CompareValue(v1, v2)=GreaterThanValue, 1, 0);
        '<' :
          if s = '<=' then
            v :=  IfThen(CompareValue(v1, v2)<GreaterThanValue, 1, 0)
          else
            v :=  IfThen(CompareValue(v1, v2)=LessThanValue, 1, 0);
        '&' : v :=  IfThen((CompareValue(v1, 0)=EqualsValue) or (CompareValue(v2, 0)=EqualsValue), 0, 1);
        '|' : v :=  IfThen((CompareValue(v1, 0)<>EqualsValue) or (CompareValue(v2, 0)<>EqualsValue), 1, 0);
        else begin
          FLastErrorMessage :=  '不支持的操作符 - ' + s;
          Exit;
        end;
      end;
      Push(v);
    end
    else if (Pos('(', s) > 0) and (RightStr(s, 1) = ')') then
    begin  //函数
      if not Assigned(OnGetFuncValue) then
      begin
        FLastErrorMessage :=  '计算错误 - 未处理函数计算.';
        Exit;
      end;
      Delete(s, Length(s), 1);
      pc  :=  StrToInt(RightStr(s, Length(s) - Pos('(', s)));
      Delete(s, Pos('(', s), Length(s));
      SetLength(params, pc);
      for j := pc - 1 downto 0 do
        params[j] :=  Pop;
      Push(OnGetFuncValue(s, params));
    end
    else begin
      if TryStrToFloat(s, v) then
        Push(v)
      else begin
        if not Assigned(OnGetParamValue) then
        begin
          FLastErrorMessage :=  '计算错误 - 未处理的参数值.';
          Exit;
        end;
        Push(OnGetParamValue(s));
      end;
    end;
  end;
  if idx = 0 then
  begin
    FReturnValue  :=  Pop;
    Result  :=  True;
  end
  else
    FLastErrorMessage :=  '计算错误 - 表达式错误.';
end;

function TRPNParser.Calc(AExpression: WideString): boolean;
begin
  Expression  :=  AExpression;
  Result  :=  Calc;
end;

function TRPNParser.GetParsedExpression: string;
var
  i : integer;
begin
  Result  :=  '';
  for i := 0 to FStack.Count - 1 do
    Result  :=  Result + ' ' + FStack[i];
  if Result <> '' then
    Delete(Result, 1, 1);
end;

end.
