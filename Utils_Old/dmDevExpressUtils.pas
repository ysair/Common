unit dmDevExpressUtils;

interface

uses
  dxBar, Classes, Controls, dxRibbon;

type
  TdxBarAccess = class(TdxBar);

    function SetItemVisible(item: TdxBarItem; visible: Boolean): Boolean;

    function FindItemLink(pmiTargetLinks: TdxBarItemLinks;
      pmiTargetItem: TdxBarItem): TdxBarItemLink;

    procedure BuildItemLink(pmiTargetItem: TdxBarSubItem; pmiSource: TdxBarSubItem); overload;

    procedure BuildMenuURM(AParentItem: TdxBarSubItem; AAddToBelowItem: TdxBarItem;
      // 在哪个Item上面，如果是nil则添加在最后面
      AURMList: TStringList; // 内容
      AOnClick: TNotifyEvent; // 点击事件
      const ADefaultDataID: Integer = -1; // 默认的选择
      const AGroupID: Integer = 99; // 设置默认的GroupID，如果同一个菜单中有2组是自动生成的，则要指明不同的GroupID
      const ADataIsImageIndex: Boolean = false; // 保存在 Integer(AURMList.Objects[i]) 的值是图片ID
      const AImageList: TImageList = nil // 生成的Item使用的ImageList
      );

    procedure BuildRibbon(ARibbon: TdxRibbon; pmiSource: TdxBarSubItem);

procedure MergeRibbon(SourceRibbon, TargetRibbon: TdxRibbon; TargetManager: TdxBarManager);

implementation

function SetItemVisible(item: TdxBarItem; visible: Boolean): Boolean;
begin
  Result := True;

  if visible then
    item.visible := ivAlways
  else
    item.visible := ivNever;
end;

function FindItemLink(pmiTargetLinks: TdxBarItemLinks; pmiTargetItem: TdxBarItem): TdxBarItemLink;

var
  i: Integer;
  item: TdxBarSubItem;
begin
  Result := nil;

  if pmiTargetLinks = nil then
    Exit;

  for i := 0 to pmiTargetLinks.Count - 1 do
  begin
    if pmiTargetLinks.Items[i].item = pmiTargetItem then
    begin
      Result := pmiTargetLinks.Items[i];
      Exit;
    end;

    if pmiTargetLinks.Items[i].item is TdxBarSubItem then
    begin
      item := pmiTargetLinks.Items[i].item as TdxBarSubItem;
      Result := FindItemLink(item.ItemLinks, pmiTargetItem);
      if Result <> nil then
        Exit;
    end;
  end;
end;

procedure BuildItemLink(pmiTargetItem: TdxBarSubItem; pmiSource: TdxBarSubItem);

var
  link: TdxBarItemLink;
  i: Integer;
  bStartGroup: Boolean;
begin
  bStartGroup := pmiTargetItem.ItemLinks.Count > 0;

  for i := 0 to pmiSource.ItemLinks.Count - 1 do
  begin
    link := pmiTargetItem.ItemLinks.Add;
    link.Assign(pmiSource.ItemLinks[i]);
    if 0 = i then
      link.BeginGroup := bStartGroup;
  end;
end;

procedure BuildMenuURM(AParentItem: TdxBarSubItem; AAddToBelowItem: TdxBarItem;
  // 在哪个Item上面，如果是nil则添加在最后面
  AURMList: TStringList; // 内容
  AOnClick: TNotifyEvent; // 点击事件
  const ADefaultDataID: Integer; // 默认的选择
  const AGroupID: Integer; // 设置默认的GroupID，如果同一个菜单中有2组是自动生成的，则要指明不同的GroupID
  const ADataIsImageIndex: Boolean; // 保存在 Integer(AURMList.Objects[i]) 的值是图片ID
  const AImageList: TImageList // 生成的Item使用的ImageList
  );

var
  i, pos: Integer;
  link: TdxBarItemLink;
  startGroup: Boolean;
begin
  for i := AParentItem.ItemLinks.Count - 1 downto 0 do
    if AParentItem.ItemLinks[i].item.Tag = AGroupID then
      AParentItem.ItemLinks.Delete(i);

  pos := -1;
  if AAddToBelowItem <> nil then
  begin
    for i := 0 to AParentItem.ItemLinks.Count - 1 do
      if AAddToBelowItem = AParentItem.ItemLinks[i].item then
      begin
        pos := i;
        Break;
      end;
  end;

  startGroup := false;
  for i := 0 to AURMList.Count - 1 do
  begin
    if AURMList[i] = '-' then
    begin
      startGroup := True;
      Continue;
    end;

    link := AParentItem.ItemLinks.AddButton;
    link.BeginGroup := startGroup;
    startGroup := false;

    if pos <> -1 then
      link.Index := pos;

    with link.item as TdxBarButton do
    begin
      Caption := AURMList[i];
      OnClick := AOnClick;
      Tag := AGroupID;

      // l_AItem.Checked     := (ADefaultDataID<>-1) and (ADefaultDataID=Integer(AURMList.Objects[i]));
      if ADataIsImageIndex then
      begin
        ImageIndex := Integer(AURMList.Objects[i]);
        BarManager.Images := AImageList;
      end
      else
      begin
        Data := AURMList.Objects[i];
      end;

      if ADefaultDataID = Integer(AURMList.Objects[i]) then
        ButtonStyle := bsChecked
      else
        ButtonStyle := bsDefault;
    end;

  end;
end;

procedure ClearBar(ABar: TdxBar);

var
  i: Integer;
begin
  for i := ABar.ItemLinks.Count - 1 downto 0 do
    if ABar.ItemLinks[i].item.BarManager <> ABar.BarManager then
      ABar.ItemLinks.Delete(i);
end;

procedure BuildRibbon(ARibbon: TdxRibbon; pmiSource: TdxBarSubItem);

var
  i, J: Integer;
  bar: TdxBar;
  link: TdxBarItemLink;
begin
  for i := 0 to ARibbon.Tabs.Count - 1 do
  begin
    if ARibbon.Tabs[i].Name = pmiSource.Name then
    begin
      bar := ARibbon.BarManager.AddToolBar(false);
      for J := 0 to pmiSource.ItemLinks.Count - 1 do
      begin
        link := bar.ItemLinks.AddButton;
        link.Assign(pmiSource.ItemLinks[J]);
      end;

      with ARibbon.Tabs[i].Groups.Add do
      begin
        ToolBar := bar;
        Caption := '';
      end;
    end;
  end;
end;

procedure MergeRibbon(SourceRibbon, TargetRibbon: TdxRibbon; TargetManager: TdxBarManager);
var
  i: Integer;
  ATargetTab:TdxRibbonTab;
  ASourceBar:TdxBar;
  ATargetBar:TdxBar;
  j: Integer;

  function GetTabByName(const ATabName:string): TdxRibbonTab;
  var
    k:Integer;
  begin
    for k := 0 to TargetRibbon.Tabs.Count - 1 do
      if(ATabName = TargetRibbon.Tabs[k].Name) then
      begin
        Result := TargetRibbon.Tabs[k];
        Exit;
      end;

    Result := TargetRibbon.Tabs.Add;
  end;

  function GetToolbarByGroupCaption(ATargetTab: TdxRibbonTab; const AGroupCaption:string): TdxBar;
  var
    k:Integer;
    group:TdxRibbonTabGroup;
  begin
    group := nil;
    for k := 0 to ATargetTab.Groups.Count - 1 do
    begin
      if(AGroupCaption = ATargetTab.Groups[k].Caption) then
      begin
        group := ATargetTab.Groups[k];
        Break;
      end;
    end;

    if(nil = group) then
    begin
      group := ATargetTab.Groups.Add;
      group.Caption := AGroupCaption;
    end;

    if(nil = group.ToolBar) then
    begin
      group.ToolBar := TargetManager.Bars.Add;
      group.ToolBar.Visible := True;
    end;

    Result := group.ToolBar;
  end;

begin
  TargetRibbon.BeginUpdate;
  try
    for i := 0 to SourceRibbon.Tabs.Count - 1 do
    begin
      ATargetTab := GetTabByName(SourceRibbon.Tabs[i].Name);
      for j := 0 to SourceRibbon.Tabs[i].Groups.Count - 1 do
      begin
        ASourceBar := SourceRibbon.Tabs[i].Groups[j].ToolBar;
        if (nil <> ASourceBar) then
        begin
          ATargetBar := GetToolbarByGroupCaption(ATargetTab, SourceRibbon.Tabs[i].Groups[j].Caption);
          ATargetBar.Merge(ASourceBar);
        end;
      end
    end;
    SourceRibbon.Visible := False;
  finally
    TargetRibbon.EndUpdate;
  end;
end;

end.
