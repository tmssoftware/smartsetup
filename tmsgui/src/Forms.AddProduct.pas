unit Forms.AddProduct;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Forms.Dialogs;

type
  // A small dialog with an editable combo, so you can pick an installed product or type an id like "tms.*".
  TAddProductForm = class(TForm)
    lbProductId: TLabel;
    cbProductId: TComboBox;
    btOk: TButton;
    btCancel: TButton;
    procedure btOkClick(Sender: TObject);
    function GetId: string;
    procedure cbProductIdChange(Sender: TObject);
  public
    class function AskProductId(const Suggestions: TArray<string>; out Id: string): Boolean;
  end;

implementation

uses
  UVCLUtils;

{$R *.dfm}

{ TAddProductForm }

class function TAddProductForm.AskProductId(const Suggestions: TArray<string>; out Id: string): Boolean;
begin
  var Form := TAddProductForm.Create(Application);
  try
    Form.cbProductId.Items.BeginUpdate;
    try
      Form.cbProductId.Items.Clear;
      for var Suggestion in Suggestions do Form.cbProductId.Items.Add(Suggestion);
    finally
      Form.cbProductId.Items.EndUpdate;
    end;

    Id := '';
    Result := Form.ShowModal = mrOk;
    if Result then
    begin
      Id := Form.GetId;
    end;
  finally
    Form.Free;
  end;
end;

procedure TAddProductForm.btOkClick(Sender: TObject);
begin
  if GetId = '' then
  begin
    TDialogForm.ShowMessage('Product Id cannot be empty.');
    exit;
  end;
  ModalResult := mrOk;

end;

procedure TAddProductForm.cbProductIdChange(Sender: TObject);
begin
  btOk.Enabled := GetId <> '';
end;

function TAddProductForm.GetId: string;
begin
  Result := CaptionToString(cbProductId.Text).Trim;
end;

end.
