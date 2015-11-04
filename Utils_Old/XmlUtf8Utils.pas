unit XmlUtf8Utils;

interface

uses
  XMLIntf;

function StringToPAnsiChar(source:RawByteString):PAnsiChar;

//将xml文档用UTF8编码，并产生一个用于dll传递的指针。
//该指针要用GlobalFreePtr释放。
function XmlToUtf8(doc: IXmlDocument): pointer;

function Utf8ToPointer(source:UTF8String):Pointer;

implementation

uses
  Windows;

function StringToPAnsiChar(source:RawByteString):PAnsiChar;
var
  l_s, l_r: PAnsiChar;
begin
  l_s := PAnsiChar(source);
  l_r := PAnsiChar(GlobalAllocPtr(0, Length(l_s) + 1));
  CopyMemory(l_r, l_s, Length(l_s));
  l_r[Length(l_s)] := #0;

  Result := l_r;
end;

function Utf8ToPointer(source:UTF8String):Pointer;
begin
  Result := Pointer(StringToPAnsiChar(source));
end;

function XmlToUtf8(doc: IXmlDocument): pointer;
var
  l_str: UTF8String;
begin
  doc.Encoding := 'UTF-8';
  doc.SaveToXML(l_str);

  Result := Utf8ToPointer(l_str);
end;

end.
