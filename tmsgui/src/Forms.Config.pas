unit Forms.Config;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.WinXPanels,
  UCommonTypes, UConfigModel, Forms.AddDefine;

type
  TConfigForm = class(TForm)
    pnNav: TPanel;
    lbNav: TListBox;
    cpMain: TCardPanel;
    cdGeneral: TCard;
    cdServers: TCard;
    cdProducts: TCard;
    pnGeneralHeader: TPanel;
    lbGeneralHeader: TLabel;
    bvGeneralHeader: TBevel;
    pnGeneral: TPanel;
    pnServersHeader: TPanel;
    lbServersHeader: TLabel;
    bvServersHeader: TBevel;
    pnServers: TPanel;
    pnProductsHeader: TPanel;
    lbProductsHeader: TLabel;
    bvProductsHeader: TBevel;
    pnProducts: TPanel;
    lbAltRegKey: TLabel;
    edAltRegKey: TEdit;
    lbAltRegKeyHint: TLabel;
    lbServerList: TCheckListBox;
    btServerAdd: TButton;
    btServerRemove: TButton;
    pnServerDetails: TPanel;
    lbServerName: TLabel;
    lbServerUrl: TLabel;
    lbServerType: TLabel;
    edServerName: TEdit;
    edServerUrl: TEdit;
    cbServerType: TComboBox;
    cbAllowInsecure: TCheckBox;
    tcProducts: TTabControl;
    pnProduct: TPanel;
    gbProductGeneral: TGroupBox;
    pnSkipRegister: TPanel;
    pnSkipRegisterCustom: TPanel;
    pnIdLists: TPanel;
    pnOverrideNote: TPanel;
    lbSkipRegister: TLabel;
    cbSkipRegister: TComboBox;
    clbSkipRegister: TCheckListBox;
    lbDelphiVersions: TLabel;
    cbDelphiVersionsNotSet: TCheckBox;
    clbDelphiVersions: TCheckListBox;
    lbPlatforms: TLabel;
    cbPlatformsNotSet: TCheckBox;
    clbPlatforms: TCheckListBox;
    lbOverrideNote: TLabel;
    gbProductCompilation: TGroupBox;
    lbDebugDcus: TLabel;
    cbDebugDcus: TComboBox;
    lbDefines: TLabel;
    clbDefines: TCheckListBox;
    btDefineAdd: TButton;
    btDefineRemove: TButton;
    PanelBtn: TPanel;
    btApply: TButton;
    btCancel: TButton;
    btOk: TButton;
    btProductAdd: TButton;
    btProductRemove: TButton;
    ProdSeparator: TBevel;
    btConfig: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure edAltRegKeyChange(Sender: TObject);
    procedure lbServerListClick(Sender: TObject);
    procedure lbServerListClickCheck(Sender: TObject);
    procedure edServerNameChange(Sender: TObject);
    procedure edServerUrlChange(Sender: TObject);
    procedure cbServerTypeClick(Sender: TObject);
    procedure cbAllowInsecureClick(Sender: TObject);
    procedure btServerAddClick(Sender: TObject);
    procedure btServerRemoveClick(Sender: TObject);
    procedure tcProductsChange(Sender: TObject);
    procedure btProductAddClick(Sender: TObject);
    procedure btProductRemoveClick(Sender: TObject);
    procedure SkipRegisterChanged(Sender: TObject);
    procedure DelphiVersionsChanged(Sender: TObject);
    procedure PlatformsChanged(Sender: TObject);
    procedure cbDebugDcusChange(Sender: TObject);
    procedure clbDefinesClickCheck(Sender: TObject);
    procedure btDefineAddClick(Sender: TObject);
    procedure btDefineRemoveClick(Sender: TObject);
    procedure lbNavClick(Sender: TObject);
    procedure lbNavDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure btConfigClick(Sender: TObject);
  private
    FConfig: TTmsConfig;
    FServers: TServerConfigItems;
    FLoading: Integer;
    FProductIds: TArray<string>;
    FAvailableDefines: TArray<TProductDefine>;
    FInstalledIDEs: TArray<string>;
    FInstalledIDEsKnown: Boolean;
    // The ids for the items in clbDelphiVersions and clbPlatforms, as the Delphi items show names.
    FDelphiIds: TArray<string>;
    FPlatformIds: TArray<string>;
    FSkipRegisterIds: TArray<string>;
    FOnLoad: TFunc<TTmsConfig, Boolean>;
    FOnSave: TFunc<TTmsConfig, Boolean>;
    FOnSaved: TProc;
    FOnOpenConfigFile: TProc;
    FOnShowLastError: TProc<string>;
    procedure BeginLoading;
    procedure EndLoading;
    procedure DoModified;
    function ApplyChanges: Boolean;
    procedure LoadControls(const SelectProductId: string);
    procedure ShowCard(const Card: TCard);

    // Servers
    procedure UpdateServerList;
    procedure SelectServer(Index: Integer; FocusControl: TWinControl = nil);
    function SelectedServer: TServerConfigItem;
    procedure ValidateServers;
    procedure RaiseServerError(const Msg: string; Index: Integer; FocusControl: TWinControl);

    // Products
    function CurrentProduct: TProductConfig;
    procedure UpdateProductTabs(const SelectProductId: string);
    procedure LoadProduct;
    procedure FillDelphiVersions(const Product: TProductConfig);
    procedure ShowSkipRegisterCustom(const Show: Boolean);
    procedure LoadIdList(const List: TCheckListBox; const NotSet: TCheckBox; const ItemIds: TArray<string>;
      const IsSet: Boolean; const Ids: TArray<string>);
    function CheckedItems(const List: TCheckListBox; const ItemIds: TArray<string>): TArray<string>;
    procedure SaveIdList(const List: TCheckListBox; const NotSet: TCheckBox; const ItemIds: TArray<string>;
      const GetAllProducts: TFunc<TProductConfig, TArray<string>>; const Save: TProc<Boolean, TArray<string>>);
    procedure SaveDefines;
  public
    // Returns false if the configuration couldn't be read. TabName can be '' or 'servers'.
    function Execute(const TabName: string): Boolean;
    function IsReady: boolean;
    procedure UpdateSelectedServer;


    // Read and write the configuration. They return false if it failed.
    property OnLoad: TFunc<TTmsConfig, Boolean> read FOnLoad write FOnLoad;
    property OnSave: TFunc<TTmsConfig, Boolean> read FOnSave write FOnSave;
    // Called after the configuration was saved, so the caller can refresh what depends on it.
    property OnSaved: TProc read FOnSaved write FOnSaved;
    property OnOpenConfigFile: TProc read FOnOpenConfigFile write FOnOpenConfigFile;
    property OnShowLastError: TProc<string> read FOnShowLastError write FOnShowLastError;
    // Suggestions for the product id when adding a product configuration.
    property ProductIds: TArray<string> read FProductIds write FProductIds;
    // The defines the products declare in their tmsbuild.yaml, offered when adding a define.
    property AvailableDefines: TArray<TProductDefine> read FAvailableDefines write FAvailableDefines;
    // The Delphi versions installed in this machine, as ids like "delphi12". Only those are shown, plus any
    // version already in the configuration. If InstalledIDEsKnown is false (tms didn't report them), all are shown.
    property InstalledIDEs: TArray<string> read FInstalledIDEs write FInstalledIDEs;
    property InstalledIDEsKnown: Boolean read FInstalledIDEsKnown write FInstalledIDEsKnown;
  end;

implementation

uses
  System.UITypes, Deget.CoreTypes, Deget.IDETypes, Forms.Dialogs, Forms.AddProduct, UVCLUtils;

{$R *.dfm}

const
  AllProductsCaption = 'All products';

function ProductCaption(const Product: TProductConfig): string;
begin
  if Product.IsAllProducts then Result := AllProductsCaption else Result := Product.Id;
end;

function ListContains(const Items: TArray<string>; const Value: string): Boolean;
begin
  for var Item in Items do
    if SameText(Item, Value) then Exit(True);
  Result := False;
end;

{ TConfigForm }

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  FConfig := TTmsConfig.Create;
  FServers := TServerConfigItems.Create;

  cbServerType.Items.Clear;
  cbServerType.Items.Add('api');
  cbServerType.Items.Add('zipfile');

  clbSkipRegister.Items.Clear;
  FSkipRegisterIds := nil;
  for var Part in SkipRegisterParts do
  begin
    clbSkipRegister.Items.Add(Part);
    FSkipRegisterIds := FSkipRegisterIds + [Part];
  end;

  // clbDelphiVersions is filled for each product, see FillDelphiVersions.
  clbPlatforms.Items.Clear;
  FPlatformIds := nil;
  for var Plat := Low(TPlatform) to High(TPlatform) do
  begin
    clbPlatforms.Items.Add(PlatformId[Plat]);
    FPlatformIds := FPlatformIds + [PlatformId[Plat]];
  end;

  btApply.Enabled := False;
  ShowCard(cdGeneral);
end;

procedure TConfigForm.FormDestroy(Sender: TObject);
begin
  FServers.Free;
  FConfig.Free;
end;

function TConfigForm.IsReady: boolean;
begin
  Result := FLoading = 0;
end;

function TConfigForm.Execute(const TabName: string): Boolean;
begin
  if not Assigned(FOnLoad) or not FOnLoad(FConfig) then
  begin
    if Assigned(OnShowLastError) then OnShowLastError('Could not read the configuration.')
    else TDialogForm.ShowMessage('Could not read the configuration. See the log for details.');
    Exit(False);
  end;

  LoadControls('');
  if SameText(TabName, 'servers') then ShowCard(cdServers)
  else if SameText(TabName, 'products') then ShowCard(cdProducts)
  else ShowCard(cdGeneral);

  ShowModal;
  Result := True;
end;

procedure TConfigForm.ShowCard(const Card: TCard);
begin
  // The items in lbNav are in the same order as the cards.
  cpMain.ActiveCard := Card;
  lbNav.ItemIndex := Card.CardIndex;
end;

procedure TConfigForm.lbNavClick(Sender: TObject);
begin
  if (lbNav.ItemIndex < 0) or (lbNav.ItemIndex >= cpMain.CardCount) then Exit;
  cpMain.ActiveCardIndex := lbNav.ItemIndex;
end;

procedure TConfigForm.lbNavDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var Canvas := lbNav.Canvas;
  Canvas.FillRect(Rect);
  var R := Rect;
  Inc(R.Left, 10);
  var Text := lbNav.Items[Index];
  Canvas.TextRect(R, Text, [tfSingleLine, tfVerticalCenter, tfLeft, tfEndEllipsis]);
end;

procedure TConfigForm.BeginLoading;
begin
  Inc(FLoading);
end;

procedure TConfigForm.EndLoading;
begin
  Dec(FLoading);
end;

procedure TConfigForm.DoModified;
begin
  if FLoading > 0 then Exit;
  btApply.Enabled := True;
end;

procedure TConfigForm.LoadControls(const SelectProductId: string);
begin
  BeginLoading;
  try
    edAltRegKey.Text := FConfig.AlternateRegistryKey;

    var ServerIndex := lbServerList.ItemIndex;
    FConfig.GetServers(FServers);
    UpdateServerList;
    if ServerIndex < 0 then ServerIndex := 0;
    SelectServer(ServerIndex);

    UpdateProductTabs(SelectProductId);
  finally
    EndLoading;
  end;
end;

function TConfigForm.ApplyChanges: Boolean;
begin
  try
    ValidateServers;
    FConfig.SetServers(FServers);
    FConfig.ToParams; // raises on values we can't send, before we run tms.

    if not Assigned(FOnSave) or not FOnSave(FConfig) then
    begin
      if Assigned(OnShowLastError) then OnShowLastError('Saving the configuration failed.')
      else TDialogForm.ShowMessage('Saving the configuration failed. See the log for details.');
      Exit(False);
    end;
    btApply.Enabled := False;
    if Assigned(FOnSaved) then FOnSaved();

    // Show what tms actually saved. A product with nothing set isn't saved, so its tab goes away.
    var ProductId := '';
    if CurrentProduct <> nil then ProductId := CurrentProduct.Id;
    if Assigned(FOnLoad) and FOnLoad(FConfig) then LoadControls(ProductId);
    Result := True;
  except on ex:Exception do
  begin
    TDialogForm.ShowMessage('ERROR: ' + ex.Message);
    Result := false;
  end;

  end;
end;

procedure TConfigForm.btApplyClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TConfigForm.btConfigClick(Sender: TObject);
begin
  if Assigned(FOnOpenConfigFile) then FOnOpenConfigFile;
end;

procedure TConfigForm.btOkClick(Sender: TObject);
begin
  if not btApply.Enabled then
  begin
    ModalResult := mrOk;
    Exit;
  end;
  if ApplyChanges then ModalResult := mrOk;
end;

{ General }

procedure TConfigForm.edAltRegKeyChange(Sender: TObject);
begin
  if FLoading > 0 then Exit;
  FConfig.AlternateRegistryKey := edAltRegKey.Text;
  DoModified;
end;

{ Servers }

function TConfigForm.SelectedServer: TServerConfigItem;
begin
  if lbServerList.ItemIndex >= 0 then
    Result := TServerConfigItem(lbServerList.Items.Objects[lbServerList.ItemIndex])
  else
    Result := nil;
end;

procedure TConfigForm.UpdateServerList;
begin
  BeginLoading;
  try
    lbServerList.Items.BeginUpdate;
    try
      lbServerList.Items.Clear;
      for var Item in FServers do
      begin
        lbServerList.AddItem(Item.Name, Item);
        lbServerList.Checked[lbServerList.Count - 1] := Item.Enabled;
      end;
    finally
      lbServerList.Items.EndUpdate;
    end;
  finally
    EndLoading;
  end;
end;

procedure TConfigForm.SelectServer(Index: Integer; FocusControl: TWinControl);
begin
  if Index >= lbServerList.Count then
    Index := lbServerList.Count - 1;
  lbServerList.ItemIndex := Index;
  UpdateSelectedServer;
  if Assigned(FocusControl) and FocusControl.CanFocus then
    FocusControl.SetFocus;
end;

procedure TConfigForm.UpdateSelectedServer;
begin
  BeginLoading;
  try
    var Server := SelectedServer;
    if not Assigned(Server) then
    begin
      pnServerDetails.Visible := False;
      btServerRemove.Enabled := False;
      Exit;
    end;

    edServerName.Text := Server.Name;
    edServerUrl.Text := Server.Url;
    cbServerType.ItemIndex := cbServerType.Items.IndexOf(Server.ServerType);
    cbAllowInsecure.Checked := Server.AllowInsecureConnections;

    // The built-in servers can only be enabled or disabled.
    var Reserved := Server.IsReserved;
    pnServerDetails.Visible := not Reserved;
    btServerRemove.Enabled := not Reserved;
  finally
    EndLoading;
  end;
end;

procedure TConfigForm.lbServerListClick(Sender: TObject);
begin
  UpdateSelectedServer;
end;

procedure TConfigForm.lbServerListClickCheck(Sender: TObject);
begin
  if SelectedServer = nil then Exit;
  SelectedServer.Enabled := lbServerList.Checked[lbServerList.ItemIndex];
  DoModified;
end;

procedure TConfigForm.edServerNameChange(Sender: TObject);
begin
  if (FLoading > 0) or (SelectedServer = nil) then Exit;
  SelectedServer.Name := edServerName.Text;
  lbServerList.Items[lbServerList.ItemIndex] := SelectedServer.Name;
  DoModified;
end;

procedure TConfigForm.edServerUrlChange(Sender: TObject);
begin
  if (FLoading > 0) or (SelectedServer = nil) then Exit;
  SelectedServer.Url := edServerUrl.Text;
  DoModified;
end;

procedure TConfigForm.cbServerTypeClick(Sender: TObject);
begin
  if (FLoading > 0) or (SelectedServer = nil) then Exit;
  SelectedServer.ServerType := cbServerType.Text;
  DoModified;
end;

procedure TConfigForm.cbAllowInsecureClick(Sender: TObject);
begin
  if (FLoading > 0) or (SelectedServer = nil) then Exit;
  SelectedServer.AllowInsecureConnections := cbAllowInsecure.Checked;
  DoModified;
end;

procedure TConfigForm.btServerAddClick(Sender: TObject);
begin
  var Server := TServerConfigItem.Create;
  FServers.Add(Server);
  Server.Name := 'server';
  Server.ServerType := 'zipfile';
  Server.Enabled := True;
  UpdateServerList;
  SelectServer(FServers.Count - 1, edServerName);
  DoModified;
end;

procedure TConfigForm.btServerRemoveClick(Sender: TObject);
begin
  if (SelectedServer = nil) or SelectedServer.IsReserved then Exit;

  var SelectedIndex := lbServerList.ItemIndex;
  FServers.Remove(SelectedServer);
  UpdateServerList;
  SelectServer(SelectedIndex);
  DoModified;
end;

procedure TConfigForm.RaiseServerError(const Msg: string; Index: Integer; FocusControl: TWinControl);
begin
  ShowCard(cdServers);
  SelectServer(Index, FocusControl);
  raise Exception.Create(Msg);
end;

procedure TConfigForm.ValidateServers;
begin
  for var I := 0 to FServers.Count - 1 do
  begin
    var Item := FServers[I];
    if Item.IsReserved then Continue;

    if Item.Name.Trim = '' then
      RaiseServerError('Server name cannot be empty', I, edServerName);

    if Item.Url.Trim = '' then
      RaiseServerError('Url cannot be empty', I, edServerUrl);

    if (Item.ServerType = '') or SameText(Item.ServerType, 'local') then
      RaiseServerError('Invalid type, only zipfile and api are allowed', I, cbServerType);

    for var J := 0 to FServers.Count - 1 do
      if (I <> J) and SameText(FServers[I].Name.Trim, FServers[J].Name.Trim) then
        RaiseServerError(Format('Duplicated server name: %s', [FServers[I].Name]), J, edServerName);
  end;
end;

{ Products }

function TConfigForm.CurrentProduct: TProductConfig;
begin
  if (tcProducts.TabIndex < 0) or (tcProducts.TabIndex >= FConfig.ProductCount) then Exit(nil);
  Result := FConfig.Products[tcProducts.TabIndex];
end;

procedure TConfigForm.UpdateProductTabs(const SelectProductId: string);
begin
  BeginLoading;
  try
    tcProducts.Tabs.BeginUpdate;
    try
      tcProducts.Tabs.Clear;
      for var i := 0 to FConfig.ProductCount - 1 do
        tcProducts.Tabs.Add(ProductCaption(FConfig.Products[i]));
    finally
      tcProducts.Tabs.EndUpdate;
    end;

    var Index := 0;
    for var i := 0 to FConfig.ProductCount - 1 do
      if SameText(FConfig.Products[i].Id, SelectProductId) then Index := i;
    tcProducts.TabIndex := Index;
    LoadProduct;
  finally
    EndLoading;
  end;
end;

procedure TConfigForm.tcProductsChange(Sender: TObject);
begin
  LoadProduct;
end;

procedure TConfigForm.LoadIdList(const List: TCheckListBox; const NotSet: TCheckBox; const ItemIds: TArray<string>;
  const IsSet: Boolean; const Ids: TArray<string>);
begin
  NotSet.Checked := not IsSet;
  for var i := 0 to List.Count - 1 do
    List.Checked[i] := IsSet and ListContains(Ids, ItemIds[i]);
  List.Enabled := IsSet;
end;

procedure TConfigForm.FillDelphiVersions(const Product: TProductConfig);
begin
  // Versions in the configuration are shown even if they aren't installed, so they aren't lost when saving.
  var Configured: TArray<string>;
  Product.GetDelphiVersions(Configured);
  var AllProducts := FConfig.FindProduct(AllProductsId);
  if (AllProducts <> nil) and (AllProducts <> Product) then
  begin
    var AllIds: TArray<string>;
    AllProducts.GetDelphiVersions(AllIds);
    Configured := Configured + AllIds;
  end;

  FDelphiIds := nil;
  clbDelphiVersions.Items.BeginUpdate;
  try
    clbDelphiVersions.Items.Clear;
    for var IDE := Low(TIDEName) to High(TIDEName) do
    begin
      var Installed := not FInstalledIDEsKnown or ListContains(FInstalledIDEs, IDEId[IDE]);
      if not Installed and not ListContains(Configured, IDEId[IDE]) then Continue;

      var Caption := DelphiNames[IDE];
      if not Installed then Caption := Caption + ' (not installed)';
      clbDelphiVersions.Items.Add(Caption);
      FDelphiIds := FDelphiIds + [IDEId[IDE]];
    end;
  finally
    clbDelphiVersions.Items.EndUpdate;
  end;
end;

procedure TConfigForm.ShowSkipRegisterCustom(const Show: Boolean);
begin
  // An alTop panel that becomes visible goes where its Top says, so put it right under the combo first.
  if Show then pnSkipRegisterCustom.Top := pnSkipRegister.Top + pnSkipRegister.Height;
  pnSkipRegisterCustom.Visible := Show;
end;

procedure TConfigForm.LoadProduct;
begin
  var Product := CurrentProduct;
  if Product = nil then Exit;

  BeginLoading;
  try
    btProductRemove.Enabled := not Product.IsAllProducts;

    var NotSetCaption: string;
    if Product.IsAllProducts then NotSetCaption := 'Default' else NotSetCaption := 'Same as all products';

    var Mode: TSkipRegisterMode;
    var Parts: TArray<string>;
    Product.GetSkipRegister(Mode, Parts);
    cbSkipRegister.Items[0] := NotSetCaption;
    cbSkipRegister.ItemIndex := Ord(Mode);
    for var i := 0 to clbSkipRegister.Count - 1 do
      clbSkipRegister.Checked[i] := ListContains(Parts, FSkipRegisterIds[i]);
    ShowSkipRegisterCustom(Mode = TSkipRegisterMode.Custom);

    var Ids: TArray<string>;
    if Product.IsAllProducts then
    begin
      cbDelphiVersionsNotSet.Caption := 'All Delphi versions';
      cbPlatformsNotSet.Caption := 'All platforms';
    end
    else
    begin
      cbDelphiVersionsNotSet.Caption := 'Same as all products';
      cbPlatformsNotSet.Caption := 'Same as all products';
    end;
    FillDelphiVersions(Product);
    var IsSet := Product.GetDelphiVersions(Ids);
    LoadIdList(clbDelphiVersions, cbDelphiVersionsNotSet, FDelphiIds, IsSet, Ids);
    IsSet := Product.GetPlatforms(Ids);
    LoadIdList(clbPlatforms, cbPlatformsNotSet, FPlatformIds, IsSet, Ids);
    pnOverrideNote.Visible := not Product.IsAllProducts;

    cbDebugDcus.Items[0] := NotSetCaption;
    cbDebugDcus.ItemIndex := Ord(Product.DebugDcus);

    clbDefines.Items.BeginUpdate;
    try
      clbDefines.Items.Clear;
      for var Define in Product.GetDefines do
      begin
        clbDefines.Items.Add(Define.Name);
        clbDefines.Checked[clbDefines.Count - 1] := Define.Enabled;
      end;
    finally
      clbDefines.Items.EndUpdate;
    end;
    btDefineRemove.Enabled := clbDefines.Count > 0;
  finally
    EndLoading;
  end;
end;

procedure TConfigForm.SkipRegisterChanged(Sender: TObject);
begin
  if FLoading > 0 then Exit;
  var Product := CurrentProduct;
  if Product = nil then Exit;

  var Mode := TSkipRegisterMode(cbSkipRegister.ItemIndex);
  ShowSkipRegisterCustom(Mode = TSkipRegisterMode.Custom);
  Product.SetSkipRegister(Mode, CheckedItems(clbSkipRegister, FSkipRegisterIds));
  DoModified;
end;

function TConfigForm.CheckedItems(const List: TCheckListBox; const ItemIds: TArray<string>): TArray<string>;
begin
  Result := nil;
  for var i := 0 to List.Count - 1 do
    if List.Checked[i] then Result := Result + [ItemIds[i]];
end;

procedure TConfigForm.SaveIdList(const List: TCheckListBox; const NotSet: TCheckBox; const ItemIds: TArray<string>;
  const GetAllProducts: TFunc<TProductConfig, TArray<string>>; const Save: TProc<Boolean, TArray<string>>);
begin
  var IsSet := not NotSet.Checked;
  if IsSet and not List.Enabled and (Length(CheckedItems(List, ItemIds)) = 0) then
  begin
    // You just unchecked "all", so start from what is used now, instead of from an empty list.
    var Current: TArray<string> := nil;
    var AllProducts := FConfig.FindProduct(AllProductsId);
    if (AllProducts <> nil) and (AllProducts <> CurrentProduct) then Current := GetAllProducts(AllProducts);
    BeginLoading;
    try
      for var i := 0 to List.Count - 1 do
        List.Checked[i] := (Length(Current) = 0) or ListContains(Current, ItemIds[i]);
    finally
      EndLoading;
    end;
  end;
  List.Enabled := IsSet;
  Save(IsSet, CheckedItems(List, ItemIds));
  DoModified;
end;

procedure TConfigForm.DelphiVersionsChanged(Sender: TObject);
begin
  if FLoading > 0 then Exit;
  var Product := CurrentProduct;
  if Product = nil then Exit;

  SaveIdList(clbDelphiVersions, cbDelphiVersionsNotSet, FDelphiIds,
    function(AllProducts: TProductConfig): TArray<string>
    begin
      AllProducts.GetDelphiVersions(Result);
    end,
    procedure(IsSet: Boolean; Ids: TArray<string>)
    begin
      Product.SetDelphiVersions(IsSet, Ids);
    end);
end;

procedure TConfigForm.PlatformsChanged(Sender: TObject);
begin
  if FLoading > 0 then Exit;
  var Product := CurrentProduct;
  if Product = nil then Exit;

  SaveIdList(clbPlatforms, cbPlatformsNotSet, FPlatformIds,
    function(AllProducts: TProductConfig): TArray<string>
    begin
      AllProducts.GetPlatforms(Result);
    end,
    procedure(IsSet: Boolean; Ids: TArray<string>)
    begin
      Product.SetPlatforms(IsSet, Ids);
    end);
end;

procedure TConfigForm.cbDebugDcusChange(Sender: TObject);
begin
  if FLoading > 0 then Exit;
  var Product := CurrentProduct;
  if Product = nil then Exit;

  Product.DebugDcus := TOptionalBool(cbDebugDcus.ItemIndex);
  DoModified;
end;

procedure TConfigForm.SaveDefines;
begin
  var Product := CurrentProduct;
  if Product = nil then Exit;

  var Defines: TArray<TDefineItem> := nil;
  for var i := 0 to clbDefines.Count - 1 do
  begin
    var Define: TDefineItem;
    Define.Name := clbDefines.Items[i];
    Define.Enabled := clbDefines.Checked[i];
    Defines := Defines + [Define];
  end;
  Product.SetDefines(Defines);
  btDefineRemove.Enabled := clbDefines.Count > 0;
  DoModified;
end;

procedure TConfigForm.clbDefinesClickCheck(Sender: TObject);
begin
  if FLoading > 0 then Exit;
  SaveDefines;
end;

procedure TConfigForm.btDefineAddClick(Sender: TObject);
begin
  var Existing: TArray<string> := nil;
  for var i := 0 to clbDefines.Count - 1 do Existing := Existing + [clbDefines.Items[i]];

  // The form checks the name is a valid identifier and not already in the list.
  var Name: string;
  if not TAddDefineForm.AskDefine(FAvailableDefines, Existing, Name) then Exit;

  clbDefines.Items.Add(Name);
  clbDefines.Checked[clbDefines.Count - 1] := True;
  clbDefines.ItemIndex := clbDefines.Count - 1;
  SaveDefines;
end;

procedure TConfigForm.btDefineRemoveClick(Sender: TObject);
begin
  var Index := clbDefines.ItemIndex;
  if Index < 0 then Exit;
  clbDefines.Items.Delete(Index);
  if Index >= clbDefines.Count then Index := clbDefines.Count - 1;
  clbDefines.ItemIndex := Index;
  SaveDefines;
end;

procedure TConfigForm.btProductAddClick(Sender: TObject);
begin
  var Suggestions: TArray<string> := nil;
  for var Id in FProductIds do
    if FConfig.FindProduct(Id) = nil then Suggestions := Suggestions + [Id];

  var Id: string;
  if not TAddProductForm.AskProductId(Suggestions, Id) then Exit;

  var Product := FConfig.AddProduct(Id);
  UpdateProductTabs(Product.Id);
  DoModified;
end;

procedure TConfigForm.btProductRemoveClick(Sender: TObject);
begin
  var Product := CurrentProduct;
  if (Product = nil) or Product.IsAllProducts then Exit;

  if TDialogForm.MessageDlg(Format('Remove the configuration for "%s"?', [Product.Id]), TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbCancel], 0, TMsgDlgBtn.mbYes) <> mrYes then Exit;

  var Index := tcProducts.TabIndex;
  FConfig.RemoveProduct(Product.Id);
  if Index >= FConfig.ProductCount then Index := FConfig.ProductCount - 1;
  UpdateProductTabs(FConfig.Products[Index].Id);
  DoModified;
end;

end.
