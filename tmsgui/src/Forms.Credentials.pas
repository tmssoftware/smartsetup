unit Forms.Credentials;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls;

type
  TCredentialsForm = class(TForm)
    edEmail: TLabeledEdit;
    edCode: TLabeledEdit;
    btOk: TButton;
    btCancel: TButton;
    lbInvalidCredentials: TLabel;
    lblNotRequired: TLabel;
    procedure btOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function GetCredentials(var Email, Code: string; LastWasInvalid: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TCredentialsForm }

procedure TCredentialsForm.btOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

class function TCredentialsForm.GetCredentials(var Email, Code: string; LastWasInvalid: Boolean): Boolean;
begin
  var Form := TCredentialsForm.Create(Application);
  try
    Form.edEmail.Text := Email;
    Form.edCode.Text := Code;
    Form.lbInvalidCredentials.Visible := LastWasInvalid;
    Result := Form.ShowModal = mrOk;
    if Result then
    begin
      Email := Form.edEmail.Text;
      Code := Form.edCode.Text;
    end;
  finally
    Form.Free;
  end;
end;

end.
