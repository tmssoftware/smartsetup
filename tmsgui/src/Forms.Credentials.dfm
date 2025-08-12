object CredentialsForm: TCredentialsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Enter credentials'
  ClientHeight = 234
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    288
    234)
  TextHeight = 15
  object lbInvalidCredentials: TLabel
    Left = 16
    Top = 166
    Width = 95
    Height = 15
    Caption = 'Invalid credentials'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblNotRequired: TLabel
    Left = 16
    Top = 13
    Width = 249
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Enter your TMS Software account credentials'
    WordWrap = True
  end
  object edEmail: TLabeledEdit
    Left = 16
    Top = 56
    Width = 249
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 34
    EditLabel.Height = 15
    EditLabel.Caption = '&E-mail'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = ''
  end
  object edCode: TLabeledEdit
    Left = 16
    Top = 107
    Width = 249
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 28
    EditLabel.Height = 15
    EditLabel.Caption = '&Code'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 1
    Text = ''
  end
  object btOk: TButton
    Left = 109
    Top = 198
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 3
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 190
    Top = 198
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbNoAccount: TCheckBox
    Left = 16
    Top = 141
    Width = 249
    Height = 17
    Caption = 'I don'#39't have a TMS Software account'
    TabOrder = 2
    OnClick = cbNoAccountClick
  end
end
