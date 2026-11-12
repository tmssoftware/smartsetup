unit Forms.Restore;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TRestoreOptions = record
    FileName: string;
    SkipRegister: Boolean; // true to never register in the IDE. false to do what the configuration says.
    OnlySelected: Boolean; // true to restore only the products selected in the list, not everything in the backup.
  end;

  TRestoreForm = class(TForm)
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
    rbSelectedProducts: TRadioButton;
    lbSelectedProducts: TLabel;
    btRestore: TButton;
    btCancel: TButton;
    OpenDialog: TFileOpenDialog;
    procedure btBrowseClick(Sender: TObject);
    procedure btRestoreClick(Sender: TObject);
    procedure edFileChange(Sender: TObject);
    procedure OptionLabelClick(Sender: TObject);
  private
    class var FInstance: TRestoreForm; // kept alive so the choices are remembered between restores. Owned by Application.
    procedure SetSelectedCount(const SelectedCount: Integer);
  protected
    function DoExecute(const SelectedCount: Integer; out Options: TRestoreOptions): Boolean;
  public
    /// <summary>Asks the user what to restore. SelectedCount is the number of products selected in the list.
    /// Returns false if the user canceled.</summary>
    class function Execute(const SelectedCount: Integer; out Options: TRestoreOptions): Boolean;
  end;

implementation

uses
  System.UITypes, Forms.Dialogs;

{$R *.dfm}

procedure TRestoreForm.btBrowseClick(Sender: TObject);
begin
  if Trim(edFile.Text) <> '' then
  begin
    OpenDialog.DefaultFolder := ExtractFileDir(Trim(edFile.Text));
    OpenDialog.FileName := ExtractFileName(Trim(edFile.Text));
  end;

  if OpenDialog.Execute(Handle) then
    edFile.Text := OpenDialog.FileName;
end;

procedure TRestoreForm.btRestoreClick(Sender: TObject);
begin
  if not FileExists(Trim(edFile.Text)) then
  begin
    TDialogForm.MessageDlg(Format('The file "%s" doesn''t exist.', [Trim(edFile.Text)]), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    edFile.SetFocus;
    Exit;
  end;
  ModalResult := mrOk;
end;

function TRestoreForm.DoExecute(const SelectedCount: Integer; out Options: TRestoreOptions): Boolean;
begin
  Options := Default(TRestoreOptions);
  SetSelectedCount(SelectedCount);
  edFileChange(nil);

  Result := ShowModal = mrOk;
  if not Result then
    Exit;

  Options.FileName := Trim(edFile.Text);
  Options.SkipRegister := rbSkipRegister.Checked;
  Options.OnlySelected := rbSelectedProducts.Checked;
end;

procedure TRestoreForm.edFileChange(Sender: TObject);
begin
  btRestore.Enabled := Trim(edFile.Text) <> '';
end;

class function TRestoreForm.Execute(const SelectedCount: Integer; out Options: TRestoreOptions): Boolean;
begin
  if FInstance = nil then
    FInstance := TRestoreForm.Create(Application);
  Result := FInstance.DoExecute(SelectedCount, Options);
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

procedure TRestoreForm.SetSelectedCount(const SelectedCount: Integer);
begin
  rbSelectedProducts.Enabled := SelectedCount > 0;
  lbSelectedProducts.Enabled := SelectedCount > 0;
  if SelectedCount > 0 then
    rbSelectedProducts.Caption := Format('Restore only the selected products (%d)', [SelectedCount])
  else
    rbSelectedProducts.Caption := 'Restore only the selected products (none selected)';

  // keep the last choice, unless there is nothing selected now.
  if not rbSelectedProducts.Enabled then
    rbAllProducts.Checked := True;
end;

end.
