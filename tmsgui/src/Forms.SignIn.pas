unit Forms.SignIn;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GUI.Environment;

type
  TSignInForm = class(TForm)
    lbMessage: TLabel;
    lbError: TLabel;
    cbNoAccount: TCheckBox;
    btSignIn: TButton;
    btCancel: TButton;
    procedure btSignInClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure cbNoAccountClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FRunLogin: TFunc<Boolean>;
    FCancelLogin: TProc;
    FLoginRunning: Boolean;
    FCancelRequested: Boolean;
    procedure StartLogin;
    procedure RequestCancel;
    procedure ShowConfirmState(LastFailed: Boolean);
  public
    class function Execute(const RunLogin: TFunc<Boolean>; const CancelLogin: TProc): TSignInOutcome;
  end;

implementation

{$R *.dfm}

const
  ConfirmMessage = 'A browser window will open so you can sign in with your TMS Software account.';
  WaitingMessage = 'Waiting for you to complete the sign in in your browser...';

{ TSignInForm }

class function TSignInForm.Execute(const RunLogin: TFunc<Boolean>; const CancelLogin: TProc): TSignInOutcome;
begin
  var Form := TSignInForm.Create(Application);
  try
    Form.FRunLogin := RunLogin;
    Form.FCancelLogin := CancelLogin;
    Form.ShowConfirmState(False);
    if Form.ShowModal = mrOk then
      Result := TSignInOutcome.SignedIn
    else
    if Form.cbNoAccount.Checked then
      Result := TSignInOutcome.DisableServer
    else
      Result := TSignInOutcome.Dismissed;
  finally
    Form.Free;
  end;
end;

procedure TSignInForm.ShowConfirmState(LastFailed: Boolean);
begin
  FLoginRunning := False;
  FCancelRequested := False;
  lbMessage.Caption := ConfirmMessage;
  lbError.Visible := LastFailed;
  cbNoAccount.Visible := True;
  btSignIn.Enabled := not cbNoAccount.Checked;
  btCancel.Enabled := True;
end;

procedure TSignInForm.StartLogin;
begin
  FLoginRunning := True;
  FCancelRequested := False;
  lbMessage.Caption := WaitingMessage;
  lbError.Visible := False;
  cbNoAccount.Visible := False;
  btSignIn.Enabled := False;

  TThread.CreateAnonymousThread(
    procedure
    begin
      var SignedIn := FRunLogin();
      TThread.Queue(nil,
        procedure
        begin
          // The form is guaranteed to be alive here: FormCloseQuery blocks
          // closing while the login thread is running.
          FLoginRunning := False;
          if SignedIn then
            ModalResult := mrOk
          else
          if FCancelRequested then
            ModalResult := mrCancel
          else
            ShowConfirmState(True);
        end);
    end).Start;
end;

procedure TSignInForm.RequestCancel;
begin
  FCancelRequested := True;
  btCancel.Enabled := False;
  if Assigned(FCancelLogin) then
    FCancelLogin();
end;

procedure TSignInForm.btSignInClick(Sender: TObject);
begin
  StartLogin;
end;

procedure TSignInForm.btCancelClick(Sender: TObject);
begin
  if FLoginRunning then
    RequestCancel
  else
    ModalResult := mrCancel;
end;

procedure TSignInForm.cbNoAccountClick(Sender: TObject);
begin
  btSignIn.Enabled := not cbNoAccount.Checked;
  if cbNoAccount.Checked then
    btCancel.Caption := '&Disable'
  else
    btCancel.Caption := '&Cancel';
end;

procedure TSignInForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FLoginRunning;
  if FLoginRunning then
    RequestCancel;
end;

end.
