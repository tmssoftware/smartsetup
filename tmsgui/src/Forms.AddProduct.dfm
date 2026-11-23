object AddProductForm: TAddProductForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add product configuration'
  ClientHeight = 110
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  DesignSize = (
    360
    110)
  TextHeight = 15
  object lbProductId: TLabel
    Left = 12
    Top = 12
    Width = 255
    Height = 15
    Caption = '&Product id (you can use wildcards like tms.vcl.*):'
    FocusControl = cbProductId
  end
  object cbProductId: TComboBox
    Left = 12
    Top = 32
    Width = 336
    Height = 23
    AutoDropDown = True
    Anchors = [akLeft, akTop, akRight]
    Sorted = True
    TabOrder = 0
    OnChange = cbProductIdChange
  end
  object btOk: TButton
    Left = 192
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 273
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
