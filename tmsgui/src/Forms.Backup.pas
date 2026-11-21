unit Forms.Backup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.WinXPanels,
  System.Generics.Collections, UCommonTypes;

type
  TBackupProduct = record
    Id: string;
    Name: string;
    InstalledVersion: string; // empty if not installed.
  end;

  TBackupOptions = record
    FileName: string;
    ProductIds: TArray<string>; // "id" for the latest version, "id:version" for a specific one.
    BundlesOnly: Boolean; // true when backing up all the products of the servers.
  end;

  TGetVersionsProc = reference to procedure(const ProductId: string; Versions: TVersionInfoList);
  // Reads the products in the servers. Returns false if reading them failed.
  TGetServerProductsFunc = reference to function(out Products: TArray<TBackupProduct>): Boolean;

  TBackupForm = class(TForm)
    CardPanel: TCardPanel;
    CardOptions: TCard;
    CardVersions: TCard;
    lbFile: TLabel;
    edFile: TEdit;
    btBrowse: TButton;
    gbProducts: TGroupBox;
    rbServerProducts: TRadioButton;
    lbServerProducts: TLabel;
    rbSelectedProducts: TRadioButton;
    lbSelectedProducts: TLabel;
    gbVersions: TGroupBox;
    rbLatestVersions: TRadioButton;
    lbLatestVersions: TLabel;
    rbManualVersions: TRadioButton;
    lbManualVersions: TLabel;
    lbVersionsInfo: TLabel;
    lvVersions: TListView;
    lbVersion: TLabel;
    cbVersion: TComboBox;
    btAllLatest: TButton;
    btAllInstalled: TButton;
    Bevel: TBevel;
    btBack: TButton;
    btNext: TButton;
    btCancel: TButton;
    SaveDialog: TFileSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btBackClick(Sender: TObject);
    procedure OptionChanged(Sender: TObject);
    procedure OptionLabelClick(Sender: TObject);
    procedure lvVersionsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure cbVersionChange(Sender: TObject);
    procedure cbVersionDropDown(Sender: TObject);
    procedure btAllLatestClick(Sender: TObject);
    procedure btAllInstalledClick(Sender: TObject);
  private
    class var FInstance: TBackupForm; // kept alive so the choices are remembered between backups. Owned by Application.
  private
    FServers: TArray<string>;
    FSelectedProducts: TArray<TBackupProduct>;
    FGetServerProducts: TGetServerProductsFunc;
    FServerProducts: TArray<TBackupProduct>;
    FServerProductsRead: Boolean; // FServerProducts is only read from the servers when needed, as it is slow.
    FGetVersions: TGetVersionsProc;
    FVersionCache: TDictionary<string, TArray<string>>; // versions in the server, by product id. Read on demand.
    FChosenVersions: TDictionary<string, string>; // what the user picked in the versions page, by product id.
    FUpdatingCombo: Boolean;
    function ServersText: string;
    function EnsureProducts: Boolean;
    function CurrentProducts: TArray<TBackupProduct>;
    procedure SetProducts(const Servers: TArray<string>; const SelectedProducts: TArray<TBackupProduct>);
    procedure ShowCard(const Card: TCard);
    procedure UpdateButtons;
    procedure FillVersionsList;
    procedure UpdateVersionCombo;
    procedure SetItemVersion(const Item: TListItem; const Version: string);
    function GetFileName: string;
    function ConfirmFileName: Boolean;
    function GetProductIds: TArray<string>;
  protected
    function DoExecute(const Servers: TArray<string>; const SelectedProducts: TArray<TBackupProduct>;
      const GetServerProducts: TGetServerProductsFunc; const GetVersions: TGetVersionsProc;
      out Options: TBackupOptions): Boolean;
  public
    /// <summary>Asks the user what to back up.
    /// Servers are the servers where "all the products" come from, and GetServerProducts reads their products.
    /// SelectedProducts are the products selected in the list.
    /// GetVersions reads the versions of a product in the server, for the versions page.
    /// Returns false if the user canceled.</summary>
    class function Execute(const Servers: TArray<string>; const SelectedProducts: TArray<TBackupProduct>;
      const GetServerProducts: TGetServerProductsFunc; const GetVersions: TGetVersionsProc;
      out Options: TBackupOptions): Boolean;
  end;

implementation

uses
  System.UITypes, System.IOUtils, Forms.Dialogs;

{$R *.dfm}

const
  LatestVersion = '(latest)';
  BackupExtension = '.snapshot.zip';

  // Columns in lvVersions. Column 0 is the caption, the rest are SubItems.
  SubItemId = 0;
  SubItemInstalled = 1;
  SubItemVersion = 2;

{ TBackupForm }

class function TBackupForm.Execute(const Servers: TArray<string>; const SelectedProducts: TArray<TBackupProduct>;
  const GetServerProducts: TGetServerProductsFunc; const GetVersions: TGetVersionsProc;
  out Options: TBackupOptions): Boolean;
begin
  if FInstance = nil then
    FInstance := TBackupForm.Create(Application);
  Result := FInstance.DoExecute(Servers, SelectedProducts, GetServerProducts, GetVersions, Options);
end;

procedure TBackupForm.FormCreate(Sender: TObject);
begin
  FVersionCache := TDictionary<string, TArray<string>>.Create;
  FChosenVersions := TDictionary<string, string>.Create;
end;

procedure TBackupForm.FormDestroy(Sender: TObject);
begin
  FVersionCache.Free;
  FChosenVersions.Free;
end;

function TBackupForm.DoExecute(const Servers: TArray<string>; const SelectedProducts: TArray<TBackupProduct>;
  const GetServerProducts: TGetServerProductsFunc; const GetVersions: TGetVersionsProc;
  out Options: TBackupOptions): Boolean;
begin
  Options := Default(TBackupOptions);
  FGetServerProducts := GetServerProducts;
  FGetVersions := GetVersions;
  // The products in the servers, their versions and the installed versions might have changed since the last time.
  FServerProducts := nil;
  FServerProductsRead := False;
  FVersionCache.Clear;
  FChosenVersions.Clear;
  SetProducts(Servers, SelectedProducts);

  if Trim(edFile.Text) = '' then
    edFile.Text := TPath.Combine(TPath.GetDocumentsPath, 'tms-backup-' + FormatDateTime('yyyy-mm-dd', Date) + BackupExtension);

  ShowCard(CardOptions);
  ActiveControl := edFile;

  try
    Result := ShowModal = mrOk;
    if not Result then
      Exit;

    Options.FileName := GetFileName;
    Options.ProductIds := GetProductIds;
    Options.BundlesOnly := rbServerProducts.Checked;
  finally
    // don't keep the main form's closures alive.
    FGetServerProducts := nil;
    FGetVersions := nil;
  end;
end;

function TBackupForm.ServersText: string;
begin
  // "tms" server, "a" and "b" servers, "a", "b" and "c" servers.
  Result := '';
  for var i := 0 to High(FServers) do
  begin
    if i > 0 then
    begin
      if i = High(FServers) then
        Result := Result + ' and '
      else
        Result := Result + ', ';
    end;
    Result := Result + '"' + FServers[i] + '"';
  end;
  if Length(FServers) = 1 then
    Result := Result + ' server'
  else
    Result := Result + ' servers';
end;

procedure TBackupForm.SetProducts(const Servers: TArray<string>; const SelectedProducts: TArray<TBackupProduct>);
begin
  FServers := Servers;
  FSelectedProducts := SelectedProducts;

  rbServerProducts.Enabled := Length(FServers) > 0;
  lbServerProducts.Enabled := rbServerProducts.Enabled;
  if rbServerProducts.Enabled then
  begin
    rbServerProducts.Caption := Format('Back up &all the products from the %s', [ServersText.Replace('&', '&&')]);
    lbServerProducts.Caption := Format('Every product your license gives you access to in the %s is saved, ' +
      'no matter if it is installed or not.', [ServersText]);
  end
  else
  begin
    rbServerProducts.Caption := 'Back up &all the products from the server (no server is enabled)';
    lbServerProducts.Caption := 'Enable the "tms" server in the settings to back up all of its products.';
  end;

  rbSelectedProducts.Enabled := Length(FSelectedProducts) > 0;
  lbSelectedProducts.Enabled := rbSelectedProducts.Enabled;
  if rbSelectedProducts.Enabled then
    rbSelectedProducts.Caption := Format('Back up only the &selected products (%d)', [Length(FSelectedProducts)])
  else
    rbSelectedProducts.Caption := 'Back up only the &selected products (none selected)';

  // keep the last choice, unless it is not possible now.
  if rbSelectedProducts.Checked and not rbSelectedProducts.Enabled then
    rbServerProducts.Checked := True
  else if rbServerProducts.Checked and not rbServerProducts.Enabled and rbSelectedProducts.Enabled then
    rbSelectedProducts.Checked := True;
end;

function TBackupForm.EnsureProducts: Boolean;
begin
  if rbSelectedProducts.Checked then
    Exit(Length(FSelectedProducts) > 0);

  if not FServerProductsRead then
  begin
    var Products: TArray<TBackupProduct>;
    var Ok: Boolean;
    var OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      Ok := Assigned(FGetServerProducts) and FGetServerProducts(Products);
    finally
      Screen.Cursor := OldCursor;
    end;

    if not Ok then
    begin
      TDialogForm.MessageDlg(Format('Could not read the products from the %s. The log has the details.', [ServersText]),
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
      Exit(False); // not marked as read, so it can be retried.
    end;
    FServerProducts := Products;
    FServerProductsRead := True;
  end;

  if Length(FServerProducts) = 0 then
  begin
    TDialogForm.MessageDlg(Format('There are no products your license gives you access to in the %s.', [ServersText]),
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    Exit(False);
  end;
  Result := True;
end;

function TBackupForm.CurrentProducts: TArray<TBackupProduct>;
begin
  if rbSelectedProducts.Checked then
    Result := FSelectedProducts
  else
    Result := FServerProducts;
end;

procedure TBackupForm.ShowCard(const Card: TCard);
begin
  CardPanel.ActiveCard := Card;
  if Card = CardVersions then
    Caption := 'Backup - Choose the versions'
  else
    Caption := 'Backup';
  UpdateButtons;
end;

procedure TBackupForm.UpdateButtons;
begin
  var OnOptions := CardPanel.ActiveCard = CardOptions;
  btBack.Enabled := not OnOptions;
  btBack.Visible := btBack.Enabled;
  if OnOptions and rbManualVersions.Checked then
    btNext.Caption := '&Next >'
  else
    btNext.Caption := '&Backup';
  // The server products are only read when going on, so here we only know if the option can be used.
  var CanBackup := (rbServerProducts.Checked and rbServerProducts.Enabled)
    or (rbSelectedProducts.Checked and (Length(FSelectedProducts) > 0));
  btNext.Enabled := (Trim(edFile.Text) <> '') and CanBackup;
end;

procedure TBackupForm.OptionChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TBackupForm.OptionLabelClick(Sender: TObject);
begin
  // Clicking the explanation selects the option it explains.
  var RadioButton := TLabel(Sender).FocusControl as TRadioButton;
  if RadioButton.Enabled then
  begin
    RadioButton.Checked := True;
    RadioButton.SetFocus;
  end;
end;

procedure TBackupForm.btBrowseClick(Sender: TObject);
begin
  var FileName := Trim(edFile.Text);
  if FileName <> '' then
  begin
    SaveDialog.DefaultFolder := ExtractFileDir(FileName);
    SaveDialog.FileName := ExtractFileName(FileName);
  end;

  if SaveDialog.Execute(Handle) then
    edFile.Text := SaveDialog.FileName;
end;

function TBackupForm.GetFileName: string;
begin
  Result := Trim(edFile.Text);
  if (Result <> '') and (ExtractFileExt(Result) = '') then
    Result := Result + BackupExtension;
  if Result <> '' then
    Result := TPath.GetFullPath(Result);
end;

function TBackupForm.ConfirmFileName: Boolean;
begin
  var FileName: string;
  try
    FileName := GetFileName;
  except
    on E: Exception do // invalid characters in the path, etc.
    begin
      TDialogForm.MessageDlg(Format('"%s" is not a valid file name: %s', [Trim(edFile.Text), E.Message]),
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
      Exit(False);
    end;
  end;

  if TDirectory.Exists(FileName) then
  begin
    TDialogForm.MessageDlg(Format('"%s" is a folder. Please enter the name of the backup file.', [FileName]),
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    Exit(False);
  end;

  if TFile.Exists(FileName) then
    Exit(TDialogForm.MessageDlg(Format('The file "%s" already exists.' + sLineBreak + sLineBreak + 'Do you want to replace it?', [FileName]),
      TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = mrYes);

  Result := True;
end;

procedure TBackupForm.btNextClick(Sender: TObject);
begin
  if CardPanel.ActiveCard = CardOptions then
  begin
    if not EnsureProducts then
      Exit;

    if rbManualVersions.Checked then
    begin
      FillVersionsList;
      ShowCard(CardVersions);
      lvVersions.SetFocus;
      Exit;
    end;
  end;

  if not ConfirmFileName then
  begin
    if CardPanel.ActiveCard = CardOptions then
      edFile.SetFocus;
    Exit;
  end;
  ModalResult := mrOk;
end;

procedure TBackupForm.btBackClick(Sender: TObject);
begin
  ShowCard(CardOptions);
  btNext.SetFocus;
end;

procedure TBackupForm.FillVersionsList;
begin
  lvVersions.Items.BeginUpdate;
  try
    lvVersions.Items.Clear;
    for var Product in CurrentProducts do
    begin
      var Version: string;
      if not FChosenVersions.TryGetValue(Product.Id, Version) then
      begin
        // By default, what you have now.
        if Product.InstalledVersion <> '' then
          Version := Product.InstalledVersion
        else
          Version := LatestVersion;
      end;

      var Item := lvVersions.Items.Add;
      if Product.Name <> '' then
        Item.Caption := Product.Name
      else
        Item.Caption := Product.Id;
      Item.SubItems.Add(Product.Id);
      Item.SubItems.Add(Product.InstalledVersion);
      Item.SubItems.Add(Version);
    end;
  finally
    lvVersions.Items.EndUpdate;
  end;

  if lvVersions.Items.Count > 0 then
  begin
    lvVersions.Items[0].Selected := True;
    lvVersions.Items[0].Focused := True;
  end;
  UpdateVersionCombo;
end;

procedure TBackupForm.SetItemVersion(const Item: TListItem; const Version: string);
begin
  var NewVersion := Trim(Version);
  if NewVersion = '' then
    NewVersion := LatestVersion;
  Item.SubItems[SubItemVersion] := NewVersion;
  FChosenVersions.AddOrSetValue(Item.SubItems[SubItemId], NewVersion);
end;

procedure TBackupForm.UpdateVersionCombo;
begin
  var Item := lvVersions.Selected;
  lbVersion.Enabled := Item <> nil;
  cbVersion.Enabled := Item <> nil;

  FUpdatingCombo := True;
  try
    cbVersion.Items.Clear;
    if Item = nil then
    begin
      cbVersion.Text := '';
      Exit;
    end;

    lbVersion.Caption := Format('&Version of %s:', [Item.Caption.Replace('&', '&&')]);
    // The server versions are read when the list drops down, so moving through the products is fast.
    var Versions: TArray<string>;
    cbVersion.Items.Add(LatestVersion);
    if FVersionCache.TryGetValue(Item.SubItems[SubItemId], Versions) then
      cbVersion.Items.AddStrings(Versions);
    cbVersion.Text := Item.SubItems[SubItemVersion];
  finally
    FUpdatingCombo := False;
  end;
end;

procedure TBackupForm.lvVersionsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateVersionCombo
  else if lvVersions.Selected = nil then
    UpdateVersionCombo;
end;

procedure TBackupForm.cbVersionChange(Sender: TObject);
begin
  if FUpdatingCombo then
    Exit;
  var Item := lvVersions.Selected;
  if Item <> nil then
    SetItemVersion(Item, cbVersion.Text);
end;

procedure TBackupForm.cbVersionDropDown(Sender: TObject);
begin
  var Item := lvVersions.Selected;
  if (Item = nil) or FVersionCache.ContainsKey(Item.SubItems[SubItemId]) or not Assigned(FGetVersions) then
    Exit;

  var ProductId := Item.SubItems[SubItemId];
  var Versions: TArray<string> := [];
  var VersionList := TVersionInfoList.Create;
  try
    var OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      FGetVersions(ProductId, VersionList); // errors go to the log, and we just get no versions.
    finally
      Screen.Cursor := OldCursor;
    end;
    for var Version in VersionList do
      Versions := Versions + [Version.Version];
  finally
    VersionList.Free;
  end;
  FVersionCache.AddOrSetValue(ProductId, Versions);

  // Add them while keeping what the user typed. The dropdown is opening now, so it shows the new items.
  FUpdatingCombo := True;
  try
    var Text := cbVersion.Text;
    cbVersion.Items.BeginUpdate;
    try
      cbVersion.Items.Clear;
      cbVersion.Items.Add(LatestVersion);
      cbVersion.Items.AddStrings(Versions);
    finally
      cbVersion.Items.EndUpdate;
    end;
    cbVersion.Text := Text;
  finally
    FUpdatingCombo := False;
  end;
end;

procedure TBackupForm.btAllLatestClick(Sender: TObject);
begin
  for var Item in lvVersions.Items do
    SetItemVersion(Item, LatestVersion);
  UpdateVersionCombo;
end;

procedure TBackupForm.btAllInstalledClick(Sender: TObject);
begin
  for var Item in lvVersions.Items do
    SetItemVersion(Item, Item.SubItems[SubItemInstalled]); // latest for the ones not installed.
  UpdateVersionCombo;
end;

function TBackupForm.GetProductIds: TArray<string>;
begin
  Result := [];
  if rbManualVersions.Checked then
  begin
    for var Item in lvVersions.Items do
    begin
      var Id := Item.SubItems[SubItemId];
      var Version := Trim(Item.SubItems[SubItemVersion]);
      if (Version = '') or (Version = LatestVersion) then
        Result := Result + [Id]
      else
        Result := Result + [Id + ':' + Version];
    end;
  end
  else
    for var Product in CurrentProducts do
      Result := Result + [Product.Id];
end;

end.
