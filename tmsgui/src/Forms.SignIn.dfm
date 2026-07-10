object SignInForm: TSignInForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sign in'
  ClientHeight = 156
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    334
    156)
  TextHeight = 15
  object lbMessage: TLabel
    Left = 16
    Top = 13
    Width = 302
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption =
      'A browser window will open so you can sign in with your TMS Sof' +
      'tware account.'
    WordWrap = True
  end
  object lbError: TLabel
    Left = 16
    Top = 56
    Width = 154
    Height = 15
    Caption = 'Sign in failed. Please try again.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object cbNoAccount: TCheckBox
    Left = 16
    Top = 84
    Width = 302
    Height = 17
    Caption = 'I don'#39't have a TMS Software account'
    TabOrder = 0
    OnClick = cbNoAccountClick
  end
  object btSignIn: TButton
    Left = 155
    Top = 120
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Sign in'
    Default = True
    TabOrder = 1
    OnClick = btSignInClick
  end
  object btCancel: TButton
    Left = 236
    Top = 120
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = btCancelClick
  end
end
