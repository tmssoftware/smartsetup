unit Forms.Start;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TStartForm = class(TForm)
    cbTMS: TCheckBox;
    Label1: TLabel;
    lbTMS: TLabel;
    cbCommunity: TCheckBox;
    lbCommunity: TLabel;
    Label4: TLabel;
    btOk: TButton;
    procedure lbTMSClick(Sender: TObject);
    procedure lbCommunityClick(Sender: TObject);
    procedure cbTMSClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateButton;
  public
    class function Execute(var TMS, Community: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

procedure TStartForm.btOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TStartForm.cbTMSClick(Sender: TObject);
begin
  UpdateButton;
end;

class function TStartForm.Execute(var TMS, Community: Boolean): Boolean;
begin
  var Form := TStartForm.Create(Application);
  try
    Result := Form.ShowModal = mrOk;
    TMS := Form.cbTMS.Checked;
    Community := Form.cbCommunity.Checked;
  finally
    Form.Free;
  end;
end;

procedure TStartForm.FormCreate(Sender: TObject);
begin
  UpdateButton;
end;

procedure TStartForm.lbCommunityClick(Sender: TObject);
begin
  if cbCommunity.CanFocus then
    cbCommunity.SetFocus;
  cbCommunity.Checked := not cbCommunity.Checked;
end;

procedure TStartForm.lbTMSClick(Sender: TObject);
begin
  if cbTMS.CanFocus then
    cbTMS.SetFocus;
  cbTMS.Checked := not cbTMS.Checked;
end;

procedure TStartForm.UpdateButton;
begin
  var Valid := cbTMS.Checked or cbCommunity.Checked;
  if Valid then
    btOk.Caption := 'Ok'
  else
    btOk.Caption := 'Cancel';
end;

end.
