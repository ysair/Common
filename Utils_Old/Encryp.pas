unit Encryp;


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TActionType = (atEncryption,atDecryption);
  TTomEncryption = class(TComponent)
  private
    { Private declarations }
    FInputString:string;
    FOutputString:string;
    FKeyString:string;
    FAction:TActionType;
    procedure SetInputString(input:string);
    procedure SetOutputString(input:string);
    procedure SetKeyString(input:string);
    Function  EncryptionEngine (Src:String; Key : String; Encrypt : Boolean):String;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;
    Procedure Execute;
  published
    { Published declarations }
    property Input: String read FInputString write SetInputString;
    property Output: String read FOutputString write SetOutputString;
    property Key: String read FKeyString write SetKeyString;
    property Action: TActionType read FAction write FAction default atEncryption;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DreamMail', [TTomEncryption]);
end;

constructor TTomEncryption.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     Action:=atEncryption;
end;

Procedure TTomEncryption.SetOutputString(input:string);
begin
   if input<> FOutputString then
     FOutputString:=input;
end;

Procedure TTomEncryption.SetKeyString(input:string);
begin
     if input <> FKeyString Then
        FKeyString:=input;
end;

Procedure TTomEncryption.SetInputString(input:string);
begin
   if input <> FInputString Then
     FInputString:=input;
end;

Function TTomEncryption.EncryptionEngine (Src:String; Key:String; Encrypt : Boolean):string;
var
   KeyLen      :Integer;
   KeyPos      :Integer;
   offset      :Integer;
   dest        :string;
   SrcPos      :Integer;
   SrcAsc      :Integer;
   TmpSrcAsc   :Integer;
   Range       :Integer;

begin
     KeyLen:=Length(Key);
     if KeyLen = 0 then key:='DM';
     KeyPos:=0;
     Range:=256;
     if Encrypt then
     begin
          Randomize;
          offset:=Random(Range);
          dest:=format('%1.2x',[offset]);
          for SrcPos := 1 to Length(Src) do
          begin
               SrcAsc:=(Ord(Src[SrcPos]) + offset) MOD 255;
               if KeyPos < KeyLen then KeyPos:= KeyPos + 1 else KeyPos:=1;
               SrcAsc:= SrcAsc xor Ord(Key[KeyPos]);
               dest:=dest + format('%1.2x',[SrcAsc]);
               offset:=SrcAsc;
          end;
     end
     else
     begin
          offset:=StrToInt('$'+ copy(src,1,2));
          SrcPos:=3;
          repeat
                SrcAsc:=StrToInt('$'+ copy(src,SrcPos,2));
                if KeyPos < KeyLen Then KeyPos := KeyPos + 1 else KeyPos := 1;
                TmpSrcAsc := SrcAsc xor Ord(Key[KeyPos]);
                if TmpSrcAsc <= offset then
                     TmpSrcAsc := 255 + TmpSrcAsc - offset
                else
                     TmpSrcAsc := TmpSrcAsc - offset;
                dest := dest + chr(TmpSrcAsc);
                offset:=srcAsc;
                SrcPos:=SrcPos + 2;
          until SrcPos >= Length(Src);
     end;
     Result:=Dest;
end;


procedure TTomEncryption.Execute;
var
   EncryptionFlag:Boolean;
begin
     if length(FInputString)=0 then
     begin
          FOutputString:='';
          exit;
     end;

     if FAction = atEncryption then
        EncryptionFlag:=True
      else
        EncryptionFlag:=False;

      try
         FOutputString:=EncryptionEngine(FInputString,FKeyString,EncryptionFlag);
      except
        FOutputString := FInputString;
      end;
end;

end.
