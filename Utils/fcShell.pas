unit fcShell;

interface

uses
  Windows, ShellAPI, ShlObj, CommCtrl, ActiveX, Forms, Generics.Collections;

type
  Shell = record
    class function  DelDirectory(const ADir  : string):boolean; static;
    class function  CopyFile(const AFrom : string; const ATo : string):Boolean; static;
    class function  MoveFile(const AFrom, ATo: string): Boolean; static;
    class function  DeleteFile(const AFileName : string):Boolean; static;
    class function  RenameFile(const AFileName : string; const ANewName  : string):Boolean; static;
    class function  SelectDirectory(
        const Caption: string;
        const Root: String;
        out Directory: string;
        const AHandle : HWND = 0
        ): Boolean; static;
    class function  Execute(const AFileName : string; const AParam : string = '') : HINST; static;
    class function  OpenDir(const ADirectory: string): HINST; static;
    class function  GetFileTypeDescription(const AFileExt : string):string; static;
  end;

implementation

var
  G_FileTypeDescriptionDict : TDictionary<string,string>;

class function Shell.DelDirectory(const ADir: string): boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd   := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(ADir + #0);
    pTo   := #0#0;
    fFlags  := FOF_NOCONFIRMATION + FOF_SILENT;
  end;
  Result := (SHFileOperation(fo) = 0);
end;

class function Shell.CopyFile(const AFrom, ATo: string): Boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd   := 0;
    wFunc := FO_COPY;
    pFrom := PChar(AFrom + #0);
    pTo   := PChar(ATo + #0);
    fFlags  := FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI;
  end;
  Result := (SHFileOperation(fo) = 0);
end;

class function Shell.MoveFile(const AFrom, ATo: string): Boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd   := 0;
    wFunc := FO_MOVE;
    pFrom := PChar(AFrom + #0);
    pTo   := PChar(ATo + #0);
    fFlags  := FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI;
  end;
  Result := (SHFileOperation(fo) = 0);
end;

class function Shell.OpenDir(const ADirectory: string): HINST;
begin
  Result  :=  ShellExecute(0, 'Open', 'explorer.exe', PChar(ADirectory), nil, SW_SHOW);
end;

class function Shell.DeleteFile(const AFileName: string): Boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd   := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(AFileName + #0);
    pTo   := #0#0;
    fFlags  := FOF_NOCONFIRMATION + FOF_SILENT;
  end;
  Result := (SHFileOperation(fo) = 0);
end;

class function Shell.Execute(const AFileName: string; const AParam : string): HINST;
begin
  Result  :=  ShellExecute(0, 'Open', PChar(AFileName), PChar(AParam), nil, SW_SHOW);
end;

class function Shell.GetFileTypeDescription(const AFileExt: string): string;
var
  FileInfo: TSHFileInfo;
begin
  if G_FileTypeDescriptionDict.TryGetValue(AFileExt, Result) then Exit;
  try
    SHGetFileInfo(pchar(AFileExt), 0, FileInfo, sizeof(FileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_TYPENAME);
    Result := FileInfo.szTypeName;
    G_FileTypeDescriptionDict.Add(AFileExt, Result);
  except
    Result  :=  '';
  end;
end;

class function Shell.RenameFile(const AFileName, ANewName: string): Boolean;
var
  fo: TSHFILEOPSTRUCT;
begin
  FillChar(fo, SizeOf(fo), 0);
  with fo do
  begin
    Wnd   := 0;
    wFunc := FO_RENAME;
    pFrom := PChar(AFileName + #0);
    pTo   := PChar(ANewName + #0);
    fFlags  := FOF_NOCONFIRMATION + FOF_SILENT;
  end;
  Result := (SHFileOperation(fo) = 0);
end;

class function Shell.SelectDirectory(
    const Caption: string;
    const Root: String;
    out Directory: string;
    const AHandle : HWND
    ): Boolean;

  {function Shell.BrowseCallbackNewWProc(Handle: HWND; Msg: Integer; wParam, lParam: Longint):
    Longint; stdcall;
  var
    rc  : TRect;
    hc  : HWND;
  begin
    Result := 0;
    case Msg of
      WM_SIZE  :
        begin
          DefWindowProc(Handle, Msg, wParam, lParam);
        end;
      WM_SIZING :
          DefWindowProc(Handle, Msg, wParam, lParam);
      else
        Result := CallWindowProc(BrowseCallbackOldWProc, Handle, Msg, wParam, lParam);
    end;  //case;
  end;//}

  function BrowseCallbackProc(
      Handle  : HWND;
      uMsg    : UINT;
      lParam  : Cardinal;
      lpData  : Cardinal
      ): integer; stdcall;
  var
    dirbuf  : array[0..MAX_PATH - 1] of Char;
    rc  : TRect;
    hc  : HWND;
    rco : TRect;
  begin
    Result := 0;
    case uMsg of
      BFFM_INITIALIZED :
        begin
          SendMessage(Handle, BFFM_SETSELECTION, 1, lpData);
          {BrowseCallbackOldWProc  := TFNWndProc(
              SetWindowLong(Handle, GWL_WNDPROC, Longint(@BrowseCallbackNewWProc)));
          SetWindowLong(Handle, GWL_STYLE,
              GetWindowLong(Handle, GWL_STYLE) or WS_SIZEBOX);//}
          if GetWindowRect(Handle, rc) then
          begin
            GetWindowRect(GetDesktopWindow, rco);
            rc.Right  :=  400;
            rc.Bottom :=  500;
            rc.Top  :=  (rco.Bottom - rc.Right) div 2;
            rc.Left :=  (rco.Right - rc.Right) div 2;
            SetWindowPos(
                Handle,
                0,
                rc.Left,
                rc.Top,
                rc.Right,
                rc.Bottom,
                0
                );         //}

            GetWindowRect(Handle, rc);
            hc  :=  FindWindowEx(Handle, 0, 'SysTreeView32', nil);
            if hc > 0 then
            begin
              SetWindowPos(
                  hc,
                  0,
                  0,
                  0,
                  rc.Right - rc.Left - 26,
                  rc.Bottom - rc.Top - 140,
                  SWP_NOMOVE or SWP_NOZORDER
                  );
              SetWindowLong(hc, GWL_STYLE,
                  GetWindowLong(hc, GWL_STYLE) or TVS_SHOWSELALWAYS);
            end;
            hc  :=  FindWindowEx(Handle, 0, 'Button', nil);
            if hc > 0 then
            begin
              SetWindowPos(
                  hc,
                  0,
                  rc.Right - rc.Left - 180,
                  rc.Bottom - rc.Top - 60,
                  0,
                  0,
                  SWP_NOSIZE or SWP_NOZORDER
                  );
              hc  :=  FindWindowEx(Handle, hc, 'Button', nil);
              SetWindowPos(
                  hc,
                  0,
                  rc.Right - rc.Left - 90,
                  rc.Bottom - rc.Top - 60,
                  0,
                  0,
                  SWP_NOSIZE or SWP_NOZORDER
                  );
            end;
          end;
        end;
      BFFM_SELCHANGED :
        if SHGetPathFromIDList(PItemIDList(lParam) ,@Dirbuf) then
          SendMessage(Handle, BFFM_SETSTATUSTEXT, 0, LongInt(@Dirbuf));
    end;
  end;

var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
begin
  Result := False;
  Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      with BrowseInfo do
      begin
        if AHandle > 0 then
          hwndOwner :=  AHandle
        else
          hwndOwner := Screen.ActiveForm.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;//包含 BIF_STATUSTEXT
        lParam  :=  Integer(PChar(Root));
        lpfn := @BrowseCallbackProc;  //回调函数
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

initialization
  G_FileTypeDescriptionDict :=  TDictionary<string,string>.Create;

finalization
  G_FileTypeDescriptionDict.Free;

end.
