object VersionPickerForm: TVersionPickerForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Install version'
  ClientHeight = 117
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    241
    117)
  TextHeight = 15
  object lbCaption: TLabel
    Left = 5
    Top = 5
    Width = 231
    Height = 32
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Pick the version to install for '
    ExplicitWidth = 200
  end
  object btCancel: TButton
    Left = 161
    Top = 84
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btOk: TButton
    Left = 80
    Top = 84
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    Default = True
    TabOrder = 1
    OnClick = btOkClick
  end
  object cbVersion: TComboBox
    Left = 5
    Top = 46
    Width = 231
    Height = 23
    AutoDropDown = True
    AutoDropDownWidth = True
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cbVersionChange
  end
end
