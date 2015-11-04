
unit MBXReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs{, DesignEditors, DesignIntf};

const
  MSB_MSG_HDR_LEN = $10;
  IDX_MAGIC	= $39464d4a;
  MBX_MAGIC = $36464d4a;

type
  //IDX File header
  Toe4_Idx_Header = Record
    Magic   :LongWord;   // JMF9 (IDX)
    Version :LongWord;   // A file format version number??
    nItems  :LongWord;   // No. entries in this IDX file (no. msgs in MBX file)
    nBytes  :LongWord;   // No. bytes in the IDX file
    Unknown1:LongWord;
    Unknown2:LongWord;
    Pad     :array[1..40] of byte;
    Pad2    :array[1..16] of byte;
  end;

  //MBX File Header
  Toe4_Mbx_Header = record
    Magic   :LongWord;        // JMF6
    Version :LongWord;        // ? 03000101 ?
    nMsgs   :LongWord;        // No. msgs including those marked for deletion */
    LastUsedMsgNum:LongWord;
    nBytes  :LongWord;        // Size of the MBX file in bytes
    Pad     :array[1..64] of byte;
  end;

  Toe4_Idx_Msg_Header = record
    Flags     :LongWord;
    Unknown1  :LongWord;
    EntryNum  :LongWord;  // Index Number
    FilePos   :LongWord;  // Position from the start of the file to this header
    nBytes    :LongWord;  // Number of bytes in this index entry
    MBXOffset :LongWord;  // Offset of message data in the mbx file
    MBXSize   :LongWord;  // Total Size of the message + header in MBX file
    Pad5      :array [1..6] of byte;
    Attach    :word;
    Pad1      :array [1..4] of byte;
    Pad2      :word;
    MsgSize   :LongWord;  // Total data length of message
    nAttachBytes:LongWord;// Number of bytes in the next attachment section in this file
    Pad3      :array [1..48] of byte;
    nSeparators:LongWord; // Number of MIME or (UUENCODE) sections + 1
    Pad4      :array [1..12] of byte;
    Flag1     :LongWord;
    Flag2     :LongWord;
    Offset    :LongWord;  // Offset to actual message data (skipping Internet headers)
    Pad       :array [1..56] of byte;
  end;

  //Internet Header data Parsed from the message itself
  Toe4_Idx_Msg_Info = record
    DataSize:LongWord;       // Struct Len + String Lengths
    Pad6    :LongWord;       // Perhaps 64 bit data size ?? (always 0)
    Received:FILETIME;
    Sent    :FILETIME;
    Priority:word;       // Priority
  end;

  //Description of each part of a multi part MIME or UUENCODED message
  Toe4_Idx_Part = Record
    Pad           :array [1..7] of LongWord;
    DataStart     :LongWord; // Offset to the data for this part of a multi part MIME
    DataEnd       :LongWord; // Offset to the end of the data (points one char past the end)

    // Start of headers for this part of the multi part MIME or UUENCODE message
    HeaderStart   :LongWord; // Offset to the start of the headers for this section only

    Pad1          :LongWord;
    Idx           :LongWord;

    // Pointer to the start of the multi part boundary line
    // For UUENCODED messges this is the same as header start because
    // the start of the headers represents the boundary too.
    BoundaryOffset:LongWord;

    Pad2          :array [1..10] of LongWord;
  end;

  Toe4_Mbx_Msg_Header = record
    Magic     :LongWord;   // 0x7F007F00
    Idx       :LongWord;   // Index in the MBX file
    TotalLen  :LongWord;   // Total Length taken by this entry
    MsgLen    :LongWord;   // Total length of all message data
  end;

  Toe4_Mail_Msg = record
    IdxEntry  :Toe4_Idx_Msg_Header;
    MsgInfo   :Toe4_Idx_Msg_Info;
    pParts    :array of Toe4_Idx_Part;

    Subject   :String;
    Sender    :String;
    POPServer :String;
    Username  :String;
    MailAccount:String;
    POP3Login :String;
    AccDesc   :String;     // Mail Account Friendly Name!
  end;

  TMBXReader = class(TComponent)
  private
    { Private declarations }
    FActive: boolean;
    FHeaders: integer;
    FMessages: integer;
    FProgress: integer;

    FOe4File: String;

    FIdxFile: String;
    FMbxFile: String;

    hIdxFile  :integer;
    hMbxFile  :integer;

    IdxHdr    :Toe4_Idx_Header;
    MbxHdr    :Toe4_Mbx_Header;

    MessageList:array of Toe4_Mail_Msg;

    FErrorMsg:String;
    FErrored:boolean;

    procedure FillMessageList;
    procedure ClearMessageList;
    procedure ReadIdxMsgHeader(lMessageCount:integer);
    procedure ReadIdxMsgInfo(lMessageCount: integer);
    procedure RetrieveMsgHdr(lMessageCount:integer);
    procedure SetActive(Value: boolean);
    procedure SetOe4File(const Value: string);
    function ReadIdxString(var aStr:String): boolean;
    procedure SetErrorMsg(Value:String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Aowner:TComponent); override;
    destructor Destroy; override;
    procedure GetMessage(Id:integer;var body: string);

    property  Errored:boolean read FErrored;
    property  ErrorMsg:string read FErrorMsg;
  published
    { Published declarations }
    property Active: boolean read FActive write SetActive;
    property HeaderCount: integer read FHeaders;
    property MessageCount: integer read FMessages;
    property Filename: String read FOe4File write SetOe4File;
  end;

  { Dbx Filename property editor }
  {
  TMBXFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
  }

procedure Register;

implementation

constructor TMBXReader.Create(Aowner:TComponent);
begin
  inherited Create(Aowner);
  hIdxFile := 0;
  hMbxFile := 0;
  FHeaders := 0;
  FMessages := 0;
  FIdxFile := '';
  FMbxFile := '';
  SetLength(MessageList,0);

  FErrored := false;
  FErrorMsg := '';
end;

destructor TMBXReader.Destroy;
begin
  if FActive then
    SetActive(False);
  ClearMessageList;
  SetLength(MessageList,0);
  inherited Destroy;
end;

//Read a variable length string from the IDX file.
function TMBXReader.ReadIdxString(var aStr:String):boolean;
var
  i,
  iLength:integer;
  ch:char;
begin
  result := false;

  //Read the 4 byte length value
	if FileRead(hIdxFile,iLength,4) <> 4 then
    exit;

  if iLength > 2048 then
    exit;

  SetLength(aStr,iLength);

  for i := 1 to iLength do
  begin
    FileRead(hIdxFile,ch,1);
    aStr[i] := ch;
    result := true;
  end;
end;

procedure TMBXReader.SetErrorMsg(Value:String);
begin
  FErrorMsg := Value;
  FErrored := true;
end;

procedure TMBXReader.SetActive(Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      if FIdxFile<>'' then
      begin
        hIdxFile:=FileOpen(FIdxFile, fmOpenRead);
        if hIdxFile < 0 then
        begin
          FActive:=false;
          hIdxFile:=0;
          SetErrorMsg('Cannot Open Index File');
          exit;
        end;

        hMbxFile:=FileOpen(FMbxFile, fmOpenRead);
        if hMbxFile < 0 then
        begin
          FActive:=false;
          FileClose(hIdxFile);
          hIdxFile:=0;
          hMbxFile:=0;
          SetErrorMsg('Cannot Open Mailbody File');
          exit;
        end;

        if sizeof(Toe4_Idx_Header) <> FileRead(hIdxFile, IdxHdr, sizeof(Toe4_Idx_Header)) then
        begin
          SetErrorMsg('Read Index File Error');
          exit;
        end;

        if sizeof(Toe4_Mbx_Header) <> FileRead(hMbxFile, MbxHdr, sizeof(Toe4_Mbx_Header)) then
        begin
          SetErrorMsg('Read MailBody File');
          exit;
        end;

        if IdxHdr.Magic <> IDX_MAGIC then
        begin
          SetErrorMsg('Error Outlook Express 4.0 Index File Format');
          exit;
        end;

        if MbxHdr.Magic <> MBX_MAGIC then
        begin
          SetErrorMsg('Error Outlook Express 4.0 MailBody File Format');
          exit;
        end;

        FillMessageList;
      end
      else
        FActive:=False;
    end
    else
    begin
      if hIdxFile > 0 then
      begin
        FileClose(hIdxFile);
        FileClose(hMbxFile);
        hIdxFile:=0;
        hMbxFile:=0;
        ClearMessageList;
      end;
    end;
  end;
end;

procedure TMBXReader.SetOe4File(const Value : string);
begin
  if not FActive then
  begin
    FOe4File := Value;
    FIdxFile := ChangeFileExt(FOe4File,'.idx');
    FMbxFile := ChangeFileExt(FOe4File,'.mbx');
  end;
end;

procedure TMBXReader.RetrieveMsgHdr(lMessageCount:integer);
var
  i:integer;
  CurPos:LongWord;
begin
  CurPos := FileSeek(hIdxFile,0,1);

  ReadIdxMsgHeader(lMessageCount);

  SetLength(MessageList[lMessageCount].pParts,MessageList[lMessageCount].IdxEntry.nSeparators);

  for i := 0 to MessageList[lMessageCount].IdxEntry.nSeparators - 1 do
  begin
    if FileRead(hIdxFile, MessageList[lMessageCount].pParts[i], sizeof(Toe4_Idx_Part)) <> sizeof(Toe4_Idx_Part) then
    begin
      SetErrorMsg('Read Index File Error');
      exit;
    end;
  end;

  FileSeek(hIdxFile,CurPos + MessageList[lMessageCount].IdxEntry.nAttachBytes + $32,0);

  ReadIdxMsgInfo(lMessageCount);

  ReadIdxString(MessageList[lMessageCount].Subject);
  ReadIdxString(MessageList[lMessageCount].Sender);
  ReadIdxString(MessageList[lMessageCount].POPServer);
  ReadIdxString(MessageList[lMessageCount].Username);
  ReadIdxString(MessageList[lMessageCount].MailAccount);
  ReadIdxString(MessageList[lMessageCount].POP3Login);
  ReadIdxString(MessageList[lMessageCount].AccDesc);

  FileSeek(hIdxFile,CurPos + MessageList[lMessageCount].IdxEntry.nBytes,0);
end;

procedure TMBXReader.GetMessage(Id:integer;var body: string);
var
  i:LongWord;
  ch:char;
begin
  SetLength(Body,MessageList[Id].IdxEntry.MbxSize);
  FileSeek(hMbxFile,MessageList[Id].IdxEntry.MbxOffset + sizeof(Toe4_Mbx_Msg_Header),0);

  for i := 1 to MessageList[Id].IdxEntry.MbxSize do
  begin
    FileRead(hMbxFile,ch,1);
    body[i] := ch;
  end;
end;

procedure TMBXReader.FillMessageList;
var
  i: integer;
begin
  if hIdxFile > 0 then
  begin
    FHeaders := IdxHdr.nItems;
    if FHeaders>0 then
    begin
      SetLength(MessageList,FHeaders);
      FProgress:=0;
      FileSeek(hIdxFile, sizeof(Toe4_Idx_Header), 0);
      for i := 0 to FHeaders - 1 do
      begin
        RetrieveMsgHdr(i);
        if FErrored then
          break;
      end;
    end;
    FMessages:=FHeaders;
  end;
end;

procedure TMBXReader.ClearMessageList;
var
  i: integer;
begin
  for i := Low(MessageList) to High(MessageList) do
    SetLength(MessageList[i].pParts,0);

  SetLength(MessageList,0);

  FMessages:=0;
  FHeaders:=0;
end;

{ Mbx Filename property editor }
{
procedure TMBXFilenameProperty.Edit;
var
     FileOpen: TOpenDialog;
begin
     FileOpen := TOpenDialog.Create(Application);
     FileOpen.Filename := GetValue;
     FileOpen.Filter := 'Outlook Express 5 files (*.dbx)|*.dbx';
     FileOpen.Options := FileOpen.Options + [ofPathMustExist, ofFileMustExist];
     try
        if FileOpen.Execute then SetValue(FileOpen.Filename);
     finally
            FileOpen.Free;
     end;
end;


function TMBXFilenameProperty.GetAttributes: TPropertyAttributes;
begin
     Result := [paDialog, paRevertable];
end;
}

procedure Register;
begin
  RegisterComponents('DreamMail', [TMBXReader]);
//  RegisterPropertyEditor(TypeInfo(string), TMBXReader, 'Filename', TMBXFilenameProperty);
end;

procedure TMBXReader.ReadIdxMsgHeader(lMessageCount: integer);
begin
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Flags , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Unknown1 , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.EntryNum , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.FilePos , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.nBytes , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.MBXOffset , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.MBXSize , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Pad5 , 6);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Attach , 2);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Pad1 , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Pad2 , 2);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.MsgSize , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.nAttachBytes , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Pad3 , 48);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.nSeparators , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Pad4 , 12);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Flag1 , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Flag2 , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Offset , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].IdxEntry.Pad , 60);
end;

procedure TMBXReader.ReadIdxMsgInfo(lMessageCount: integer);
begin
  FileRead(hIdxFile, MessageList[lMessageCount].MsgInfo.DataSize , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].MsgInfo.Pad6 , 4);
  FileRead(hIdxFile, MessageList[lMessageCount].MsgInfo.Received , 8);
  FileRead(hIdxFile, MessageList[lMessageCount].MsgInfo.Sent , 8);
  FileRead(hIdxFile, MessageList[lMessageCount].MsgInfo.Priority , 2);
end;

end.
