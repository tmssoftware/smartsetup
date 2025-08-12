object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Configuration'
  ClientHeight = 326
  ClientWidth = 626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    626
    326)
  TextHeight = 15
  object tvMenu: TTreeView
    Left = 5
    Top = 5
    Width = 116
    Height = 282
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '='
    Font.Style = []
    HideSelection = False
    Indent = 19
    ParentFont = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = tvMenuChange
  end
  object btApply: TButton
    Left = 547
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 5
    OnClick = btApplyClick
  end
  object btCancel: TButton
    Left = 466
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btOk: TButton
    Left = 385
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    Default = True
    TabOrder = 3
    OnClick = btOkClick
  end
  object pnDetails: TPanel
    Left = 127
    Top = 32
    Width = 495
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    TabOrder = 2
  end
  object pnTitle: TPanel
    Left = 127
    Top = 5
    Width = 495
    Height = 24
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    TabOrder = 1
  end
end
