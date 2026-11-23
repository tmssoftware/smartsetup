unit Forms.BackupResults;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TBackupResult = record
    ProductId: string;
    ProductName: string;
    Version: string;
    Server: string; // empty for local products, which restore doesn't restore.
    InFile: Boolean; // false if only the snapshot has it, and restore gets it from its server.
  end;

  TBackupResultsForm = class(TForm)
    lbCaption: TLabel;
    edFile: TEdit;
    lbProducts: TLabel;
    lvProducts: TListView;
    Panel1: TPanel;
    btOpenFolder: TButton;
    btClose: TButton;
    procedure btOpenFolderClick(Sender: TObject);
  private
    procedure FillResults(const FileName: string; const Results: TArray<TBackupResult>);
  protected
    procedure DoExecute(const FileName: string; const Results: TArray<TBackupResult>);
  public
    class procedure Execute(const FileName: string; const Results: TArray<TBackupResult>);
  end;

implementation

uses
  Winapi.ShellAPI, System.Generics.Collections, System.Generics.Defaults, UVCLUtils;

{$R *.dfm}

class procedure TBackupResultsForm.Execute(const FileName: string; const Results: TArray<TBackupResult>);
begin
  var Form := TBackupResultsForm.Create(Application);
  try
    Form.DoExecute(FileName, Results);
  finally
    Form.Free;
  end;
end;

procedure TBackupResultsForm.DoExecute(const FileName: string; const Results: TArray<TBackupResult>);
begin
  FillResults(FileName, Results);
  ActiveControl := btClose;
  ShowModal;
end;

procedure TBackupResultsForm.FillResults(const FileName: string; const Results: TArray<TBackupResult>);
begin
  edFile.Text := FileName;
  btOpenFolder.Enabled := FileExists(FileName);

  var Sorted := Copy(Results);
  TArray.Sort<TBackupResult>(Sorted, TComparer<TBackupResult>.Construct(
    function(const Left, Right: TBackupResult): Integer
    begin
      Result := CompareText(Left.ProductId, Right.ProductId);
    end));

  var NotInFile := 0;
  lvProducts.Items.BeginUpdate;
  try
    lvProducts.Items.Clear;
    for var R in Sorted do
    begin
      var Item := lvProducts.Items.Add;
      Item.Caption := R.ProductId;
      Item.SubItems.Add(R.Version);
      Item.SubItems.Add(R.ProductName);
      if R.InFile then
        Item.SubItems.Add('')
      else
      begin
        if R.Server = '' then
          Item.SubItems.Add('Local, not restored')
        else
          Item.SubItems.Add(Format('Restored from server "%s"', [R.Server]));
        Inc(NotInFile);
      end;
    end;
  finally
    lvProducts.Items.EndUpdate;
  end;

  if Length(Sorted) = 1 then
    lbProducts.Caption := 'It contains 1 product:'
  else
    lbProducts.Caption := Format('It contains %d products:', [Length(Sorted)]);
  if NotInFile > 0 then
    lbProducts.Caption :=  CaptionToString(lbProducts.Caption).Replace(':', '') +
      Format(' (%d of them only in the snapshot, not in the file):', [NotInFile]);
end;

procedure TBackupResultsForm.btOpenFolderClick(Sender: TObject);
begin
  var FileName := edFile.Text;
  if FileExists(FileName) then
    ShellExecute(Handle, 'open', 'explorer.exe', PChar('/select,"' + FileName + '"'), nil, SW_SHOWNORMAL)
  else if DirectoryExists(ExtractFileDir(FileName)) then
    ShellExecute(Handle, 'open', PChar(ExtractFileDir(FileName)), nil, nil, SW_SHOWNORMAL);
end;

end.
