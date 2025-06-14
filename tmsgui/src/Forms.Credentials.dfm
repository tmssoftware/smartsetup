object CredentialsForm: TCredentialsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Enter credentials'
  ClientHeight = 231
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  DesignSize = (
    258
    231)
  TextHeight = 15
  object lbInvalidCredentials: TLabel
    Left = 16
    Top = 160
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
    Top = 16
    Width = 219
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Credentials are NOT required for third-party products.'
    WordWrap = True
  end
  object edEmail: TLabeledEdit
    Left = 16
    Top = 80
    Width = 219
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
    Top = 131
    Width = 219
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
    ExplicitWidth = 217
  end
  object btOk: TButton
    Left = 79
    Top = 195
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 2
    OnClick = btOkClick
    ExplicitLeft = 77
    ExplicitTop = 187
  end
  object btCancel: TButton
    Left = 160
    Top = 195
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Skip'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 158
    ExplicitTop = 187
  end
end
