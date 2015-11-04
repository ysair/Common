unit uSharedMemory;

interface

uses
  classes, SysUtils, Windows;

type
  TSharedMemory = class
  private
    FSize: Integer;
    FFirstCreated: Boolean;
    FFileView: Pointer;
    FName: string;
    FHandle: THandle;
  public
    property Size: Integer Read FSize;
    property Buffer: Pointer Read FFileView;
    property FirstCreated: Boolean Read FFirstCreated;
    property Name : string read FName;
    property Handle : THandle read FHandle;
  public
    constructor Create(Const Name: String; Size: Integer);
    destructor Destroy; Override;

    procedure Clear;
  end;

implementation

procedure TSharedMemory.Clear;
begin
  if FFileView <> nil then
    ZeroMemory(FFileView, FSize);
end;

constructor TSharedMemory.Create(Const Name: String; Size: Integer);
begin
  Inherited Create;
  FName := Name;
  FSize := Size;
  FHandle := CreateFileMapping(
      INVALID_HANDLE_VALUE,
      nil,
      PAGE_READWRITE,
      0,
      Size,
      PChar(Name)
      );
  If FHandle = 0 Then
    Abort;
  FFirstCreated := (GetLastError = 0);
  FFileView := MapViewOfFile(FHandle, FILE_MAP_WRITE, 0, 0, Size);
  If FFileView = Nil Then
    Abort;
end;

destructor TSharedMemory.Destroy;
begin
  If FFileView <> Nil Then
    UnmapViewOfFile(FFileView);
  Inherited Destroy;
end;

end.

