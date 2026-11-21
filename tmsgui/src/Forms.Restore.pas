unit Forms.Restore;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.WinXPanels,
  System.Generics.Collections;

type
  TRestoreProduct = record
    Id: string;
    Name: string; // can be empty if we don't know the product.
    Version: string; // the version in the backup.
    InstalledVersion: string; // empty if not installed.
    Server: string; // where restore gets it from, if it is not in the file.
    InFile: Boolean; // true if the product is inside the backup file.
  end;

  TRestoreOptions = record
    FileName: string;
    SkipRegister: Boolean; // true to never register in the IDE. false to do what the configuration says.
    ProductIds: TArray<string>; // the products to restore. Empty restores everything in the backup.
  end;

  // Reads the products in a backup. Returns false if reading them failed.
  TGetBackupProductsFunc = reference to function(const FileName: string; out Products: TArray<TRestoreProduct>): Boolean;

  TRestoreForm = class(TForm)
    CardPanel: TCardPanel;
    CardOptions: TCard;
    CardProducts: TCard;
    lbFile: TLabel;
    edFile: TEdit;
    btBrowse: TButton;
    gbRegister: TGroupBox;
    rbRegisterAuto: TRadioButton;
    lbRegisterAuto: TLabel;
    rbSkipRegister: TRadioButton;
    lbSkipRegister: TLabel;
    gbProducts: TGroupBox;
    rbAllProducts: TRadioButton;
    lbAllProducts: TLabel;
    rbManualProducts: TRadioButton;
    lbManualProducts: TLabel;
    lbProductsInfo: TLabel;
    lvProducts: TListView;
    btSelectAll: TButton;
    btSelectNone: TButton;
    lbCheckedCount: TLabel;
    Bevel: TBevel;
    btBack: TButton;
    btNext: TButton;
    btCancel: TButton;
    OpenDialog: TFileOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btBackClick(Sender: TObject);
    procedure OptionChanged(Sender: TObject);
    procedure OptionLabelClick(Sender: TObject);
    procedure lvProductsItemChecked(Sender: TObject; Item: TListItem);
    procedure btSelectAllClick(Sender: TObject);
    procedure btSelectNoneClick(Sender: TObject);
  private
    class var FInstance: TRestoreForm; // kept alive so the choices are remembered between restores. Owned by Application.
  private
    FGetBackupProducts: TGetBackupProductsFunc;
    FProducts: TArray<TRestoreProduct>; // the products in FProductsFile that restore can restore.
    FNotRestorable: Integer; // products in FProductsFile that restore never restores (local products not in the file).
    FProductsFile: string; // the backup FProducts was read from. Empty if not read.
    FUnchecked: THashSet<string>; // the products the user unchecked in the products page, by id.
    FFillingList: Boolean;
    function GetFileName: string;
    function CheckFileName: Boolean;
    function EnsureProducts: Boolean;
    procedure ShowCard(const Card: TCard);
    procedure UpdateButtons;
    procedure FillProductsList;
    function CheckedCount: Integer;
    procedure SetAllChecked(const Value: Boolean);
    function GetProductIds: TArray<string>;
  protected
    function DoExecute(const GetBackupProducts: TGetBackupProductsFunc; out Options: TRestoreOptions): Boolean;
  public
    /// <summary>Asks the user what to restore. GetBackupProducts reads the products in the backup, for the page
    /// where you choose the products to restore. Returns false if the user canceled.</summary>
    class function Execute(const GetBackupProducts: TGetBackupProductsFunc; out Options: TRestoreOptions): Boolean;
  end;

implementation

uses
  System.UITypes, System.IOUtils, Forms.Dialogs;

{$R *.dfm}

const
  // Columns in lvProducts. Column 0 is the caption, the rest are SubItems.
  SubItemId = 0;

{ TRestoreForm }

class function TRestoreForm.Execute(const GetBackupProducts: TGetBackupProductsFunc; out Options: TRestoreOptions): Boolean;
begin
  if FInstance = nil then
    FInstance := TRestoreForm.Create(Application);
  Result := FInstance.DoExecute(GetBackupProducts, Options);
end;

procedure TRestoreForm.FormCreate(Sender: TObject);
begin
  FUnchecked := THashSet<string>.Create;
end;

procedure TRestoreForm.FormDestroy(Sender: TObject);
begin
  FUnchecked.Free;
end;

function TRestoreForm.DoExecute(const GetBackupProducts: TGetBackupProductsFunc; out Options: TRestoreOptions): Boolean;
begin
  Options := Default(TRestoreOptions);
  FGetBackupProducts := GetBackupProducts;
  // The backup file and the installed versions might have changed since the last time.
  FProducts := nil;
  FNotRestorable := 0;
  FProductsFile := '';
  FUnchecked.Clear;

  ShowCard(CardOptions);
  ActiveControl := edFile;

  try
    Result := ShowModal = mrOk;
    if not Result then
      Exit;

    Options.FileName := GetFileName;
    Options.SkipRegister := rbSkipRegister.Checked;
    Options.ProductIds := GetProductIds;
  finally
    // don't keep the main form's closure alive.
    FGetBackupProducts := nil;
  end;
end;

procedure TRestoreForm.btBrowseClick(Sender: TObject);
begin
  var FileName := Trim(edFile.Text);
  if FileName <> '' then
  begin
    OpenDialog.DefaultFolder := ExtractFileDir(FileName);
    OpenDialog.FileName := ExtractFileName(FileName);
  end;

  if OpenDialog.Execute(Handle) then
    edFile.Text := OpenDialog.FileName;
end;

function TRestoreForm.GetFileName: string;
begin
  Result := Trim(edFile.Text);
  if Result <> '' then
    Result := TPath.GetFullPath(Result);
end;

function TRestoreForm.CheckFileName: Boolean;
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

  if not TFile.Exists(FileName) then
  begin
    TDialogForm.MessageDlg(Format('The file "%s" doesn''t exist.', [FileName]),
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    Exit(False);
  end;
  Result := True;
end;

function TRestoreForm.EnsureProducts: Boolean;
begin
  var FileName := GetFileName;
  if SameText(FileName, FProductsFile) then
    Exit(True);

  var Products: TArray<TRestoreProduct>;
  var Ok: Boolean;
  var OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Ok := Assigned(FGetBackupProducts) and FGetBackupProducts(FileName, Products);
  finally
    Screen.Cursor := OldCursor;
  end;

  if not Ok then
  begin
    TDialogForm.MessageDlg(Format('Could not read the products in "%s". The log has the details.', [FileName]),
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    Exit(False); // not marked as read, so it can be retried.
  end;

  // A different backup, so what was unchecked in the other one doesn't apply.
  FUnchecked.Clear;
  FProducts := [];
  FNotRestorable := 0;
  for var Product in Products do
  begin
    // Restore never restores a local product that is not in the file, so there is nothing to choose.
    if Product.InFile or (Product.Server <> '') then
      FProducts := FProducts + [Product]
    else
      Inc(FNotRestorable);
  end;
  FProductsFile := FileName;

  if Length(FProducts) = 0 then
  begin
    TDialogForm.MessageDlg(Format('There are no products to restore in "%s".', [FileName]),
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    Exit(False);
  end;
  Result := True;
end;

procedure TRestoreForm.ShowCard(const Card: TCard);
begin
  CardPanel.ActiveCard := Card;
  if Card = CardProducts then
    Caption := 'Restore backup - Choose the products'
  else
    Caption := 'Restore backup';
  UpdateButtons;
end;

procedure TRestoreForm.UpdateButtons;
begin
  var OnOptions := CardPanel.ActiveCard = CardOptions;
  btBack.Enabled := not OnOptions;
  btBack.Visible := btBack.Enabled;
  if OnOptions and rbManualProducts.Checked then
    btNext.Caption := '&Next >'
  else
    btNext.Caption := 'Res&tore';

  if OnOptions then
    btNext.Enabled := Trim(edFile.Text) <> ''
  else
  begin
    var Count := CheckedCount;
    btNext.Enabled := Count > 0;
    lbCheckedCount.Caption := Format('%d of %d products will be restored.', [Count, lvProducts.Items.Count]);
  end;
end;

procedure TRestoreForm.OptionChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TRestoreForm.OptionLabelClick(Sender: TObject);
begin
  // Clicking the explanation selects the option it explains.
  var RadioButton := TLabel(Sender).FocusControl as TRadioButton;
  if RadioButton.Enabled then
  begin
    RadioButton.Checked := True;
    RadioButton.SetFocus;
  end;
end;

procedure TRestoreForm.btNextClick(Sender: TObject);
begin
  if CardPanel.ActiveCard = CardOptions then
  begin
    if not CheckFileName then
    begin
      edFile.SetFocus;
      Exit;
    end;

    if rbManualProducts.Checked then
    begin
      if not EnsureProducts then
        Exit;
      FillProductsList;
      ShowCard(CardProducts);
      lvProducts.SetFocus;
      Exit;
    end;
  end
  else if CheckedCount = 0 then
    Exit;

  ModalResult := mrOk;
end;

procedure TRestoreForm.btBackClick(Sender: TObject);
begin
  ShowCard(CardOptions);
  btNext.SetFocus;
end;

procedure TRestoreForm.FillProductsList;
begin
  if FNotRestorable = 1 then
    lbProductsInfo.Caption := 'Check the products to restore. 1 product in the backup is local and not saved in the file, ' +
      'so it can''t be restored and it is not shown.'
  else if FNotRestorable > 1 then
    lbProductsInfo.Caption := Format('Check the products to restore. %d products in the backup are local and not saved in the file, ' +
      'so they can''t be restored and they are not shown.', [FNotRestorable])
  else
    lbProductsInfo.Caption := 'Check the products to restore.';

  FFillingList := True;
  lvProducts.Items.BeginUpdate;
  try
    lvProducts.Items.Clear;
    for var Product in FProducts do
    begin
      var Item := lvProducts.Items.Add;
      if Product.Name <> '' then
        Item.Caption := Product.Name
      else
        Item.Caption := Product.Id;
      Item.SubItems.Add(Product.Id);
      Item.SubItems.Add(Product.Version);
      Item.SubItems.Add(Product.InstalledVersion);
      if Product.InFile then
        Item.SubItems.Add('Backup file')
      else
        Item.SubItems.Add(Format('Server "%s"', [Product.Server]));
      Item.Checked := not FUnchecked.Contains(Product.Id);
    end;
  finally
    lvProducts.Items.EndUpdate;
    FFillingList := False;
  end;

  if lvProducts.Items.Count > 0 then
  begin
    lvProducts.Items[0].Selected := True;
    lvProducts.Items[0].Focused := True;
  end;
  UpdateButtons;
end;

procedure TRestoreForm.lvProductsItemChecked(Sender: TObject; Item: TListItem);
begin
  if FFillingList then
    Exit;
  if Item.Checked then
    FUnchecked.Remove(Item.SubItems[SubItemId])
  else
    FUnchecked.Add(Item.SubItems[SubItemId]);
  UpdateButtons;
end;

function TRestoreForm.CheckedCount: Integer;
begin
  Result := 0;
  for var Item in lvProducts.Items do
    if Item.Checked then
      Inc(Result);
end;

procedure TRestoreForm.SetAllChecked(const Value: Boolean);
begin
  lvProducts.Items.BeginUpdate;
  try
    for var Item in lvProducts.Items do
      Item.Checked := Value; // OnItemChecked keeps FUnchecked updated.
  finally
    lvProducts.Items.EndUpdate;
  end;
  UpdateButtons;
end;

procedure TRestoreForm.btSelectAllClick(Sender: TObject);
begin
  SetAllChecked(True);
end;

procedure TRestoreForm.btSelectNoneClick(Sender: TObject);
begin
  SetAllChecked(False);
end;

function TRestoreForm.GetProductIds: TArray<string>;
begin
  Result := [];
  if not rbManualProducts.Checked then
    Exit; // everything in the backup.

  // If everything is checked, it is the same as restoring the whole backup, and the command line is shorter.
  if CheckedCount = lvProducts.Items.Count then
    Exit;

  for var Item in lvProducts.Items do
    if Item.Checked then
      Result := Result + [Item.SubItems[SubItemId]];
end;

end.
