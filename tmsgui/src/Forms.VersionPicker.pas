unit Forms.VersionPicker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  Forms.Config.Servers, UCommonTypes;

type
  TVersionPickerForm = class(TForm)
    btCancel: TButton;
    btOk: TButton;
    lbCaption: TLabel;
    cbVersion: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure cbVersionChange(Sender: TObject);
  private
    procedure UpdateDropDown(Versions: TVersionInfoList);
  protected
    function DoExecute(const ProductName: string; Versions: TVersionInfoList): string;
  public
    class function Execute(const ProductName: string; Versions: TVersionInfoList): string;
  end;

implementation

{$R *.dfm}

procedure TVersionPickerForm.btOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TVersionPickerForm.cbVersionChange(Sender: TObject);
begin
  btOk.Enabled := cbVersion.Text <> '';
end;

function TVersionPickerForm.DoExecute(const ProductName: string; Versions: TVersionInfoList): string;
begin
  Result := '';
  lbCaption.Caption := lbCaption.Caption + sLineBreak + ProductName;
  UpdateDropDown(Versions);
  if ShowModal = mrOk then
    Result := cbVersion.Text;
end;

class function TVersionPickerForm.Execute(const ProductName: string; Versions: TVersionInfoList): string;
begin
  var Form := TVersionPickerForm.Create(Application);
  try
    Result := Form.DoExecute(ProductName, Versions);
  finally
    Form.Free;
  end;
end;

procedure TVersionPickerForm.FormCreate(Sender: TObject);
begin
  btOk.Enabled := False;
end;

procedure TVersionPickerForm.UpdateDropDown(Versions: TVersionInfoList);
begin
  cbVersion.Clear;
  for var Version in Versions do
    cbVersion.Items.Add(Version.Version);
end;

end.
