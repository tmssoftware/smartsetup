unit Forms.LogDetails;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TLogDetailsForm = class(TForm)
    btOk: TButton;
    PanelButtons: TPanel;
    Data: TMemo;
  private
    { Private declarations }
  public
    procedure SetLogText(const Text: string);
  end;


implementation

{$R *.dfm}

{ TLogDetails }

procedure TLogDetailsForm.SetLogText(const Text: string);
begin
  Data.Text := Text;
end;

end.
