unit Forms.Dialogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, UITypes;

//Instead of using our form, we could set UseLatestCommonDialogs := false when automating
//It would work, the old common dialogs are just vcl forms, and they launch onidle.
//But somehow, it feels safer to have our own form that we control, and not to rely in a property
//for backward compat with winxp. The dialog doesn't even have to be pretty, as it will only show when automating tmsgui.

type
  TDialogForm = class(TForm)
    MemoMessage: TMemo;
    Panel1: TPanel;
    btnCancel: TButton;
    btnNo: TButton;
    btnOk: TButton;
    btnYes: TButton;
  private
    procedure FitToContent;
  public
    class procedure ShowMessage(const Message: string); static;
    class function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn; CustomButtonCaptions: TArray<string> = nil): Integer; static;
  end;

implementation
uses GUI.Automation, System.Math, VCL.Menus;

{$R *.dfm}

{ TDialogForm }

procedure TDialogForm.FitToContent;
begin
  Canvas.Font := MemoMessage.Font;

  //Buttons: wide enough for the caption, but never smaller than the designed width.
  var ButtonsWidth := 0;
  for var Button in [btnYes, btnNo, btnOk, btnCancel] do
  begin
    if not Button.Visible then continue;
    Button.Width := Max(ScaleValue(75), Canvas.TextWidth(StripHotkey(Button.Caption)) + ScaleValue(24));
    Inc(ButtonsWidth, Button.Width + Button.Margins.Left + Button.Margins.Right);
  end;

  //Text: measure with word wrap, limited to a reasonable part of the screen.
  var MaxTextWidth := Screen.WorkAreaWidth * 1 div 4;
  var MaxTextHeight := Screen.WorkAreaHeight * 3 div 4 - Panel1.Height;
  var R := Rect(0, 0, MaxTextWidth, 0);
  var Msg := MemoMessage.Text;
  DrawText(Canvas.Handle, PChar(Msg), Length(Msg), R,
    DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX or DT_EXPANDTABS or DT_EDITCONTROL);

  //Padding covers the memo border, internal margins and a possible scrollbar.
  var Padding := ScaleValue(24) + GetSystemMetrics(SM_CXVSCROLL);
  var TextHeight := R.Height + ScaleValue(16);
  if TextHeight > MaxTextHeight then
  begin
    TextHeight := MaxTextHeight;
    MemoMessage.ScrollBars := ssVertical;
  end;

  ClientWidth := Max(Max(ButtonsWidth + ScaleValue(12), R.Width + Padding), ScaleValue(300));
  ClientHeight := TextHeight + Panel1.Height;
end;

class function TDialogForm.MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: TArray<string> = nil): Integer;
begin
{$IFDEF DEBUG}
  //When we are automating and use ShowMessage the Application.OnIdle event stops firing,
  //And we would be stuck. When automating, we will show a fake dialog.
  if TGUIAutomator.IsAutomating then
  begin
    var Form := TDialogForm.Create(nil);
    try
      var Index := -1;
      for var btn in Buttons do
      begin
        Inc(Index);
        var Button: TButton := nil;
        case btn of
          mbYes: Button := Form.btnYes;
          mbNo: Button := Form.btnNo;
          mbOk: Button := Form.btnOk;
          mbCancel: Button := Form.btnCancel;
        end;

        if Button = nil then raise Exception.Create('Unsupported button type in MessageDlg');
        Button.Visible := true;
        if btn = DefaultButton then Button.Default := true;
        if Index < Length(CustomButtonCaptions) then Button.Caption := CustomButtonCaptions[Index];

      end;

      Form.MemoMessage.Text := Msg;
      Form.FitToContent;
      Result := Form.ShowModal;
    finally
      Form.Free;
    end;
    exit;
  end;
{$ENDIF}

  if CustomButtonCaptions = nil
    then Result := Vcl.Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx, DefaultButton)
    else Result := Vcl.Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx, DefaultButton, CustomButtonCaptions);
end;

class procedure TDialogForm.ShowMessage(const Message: string);
begin
{$IFDEF DEBUG}
  //When we are automating and use ShowMessage the Application.OnIdle event stops firing,
  //And we would be stuck. When automating, we will show a fake dialog.
  if TGUIAutomator.IsAutomating then
  begin
    var Form := TDialogForm.Create(nil);
    try
      Form.btnOk.Visible := true;
      Form.btnCancel.Visible := true;
      Form.btnOk.Default := true;
      Form.MemoMessage.Text := Message;
      Form.FitToContent;
      Form.ShowModal;
    finally
      Form.Free;
    end;
    exit;
  end;
{$ENDIF}

  Vcl.Dialogs.ShowMessage(Message);
end;

end.
