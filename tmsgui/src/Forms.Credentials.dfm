object CredentialsForm: TCredentialsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Enter credentials'
  ClientHeight = 173
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  DesignSize = (
    258
    173)
  TextHeight = 15
  object lbInvalidCredentials: TLabel
    Left = 16
    Top = 112
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
  object edEmail: TLabeledEdit
    Left = 16
    Top = 32
    Width = 223
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 34
    EditLabel.Height = 15
    EditLabel.Caption = 'E-mail'
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
    Top = 80
    Width = 223
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 28
    EditLabel.Height = 15
    EditLabel.Caption = 'Code'
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
    Left = 83
    Top = 137
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Ok'
    Default = True
    TabOrder = 2
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 164
    Top = 137
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
