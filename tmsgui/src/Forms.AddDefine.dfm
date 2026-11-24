object AddDefineForm: TAddDefineForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Add define'
  ClientHeight = 393
  ClientWidth = 560
  Color = clBtnFace
  Constraints.MinHeight = 330
  Constraints.MinWidth = 420
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  DesignSize = (
    560
    393)
  TextHeight = 15
  object lbProduct: TLabel
    Left = 12
    Top = 15
    Width = 45
    Height = 15
    Caption = '&Product:'
    FocusControl = cbProduct
  end
  object lbDefines: TLabel
    Left = 12
    Top = 44
    Width = 221
    Height = 15
    Caption = '&Defines available in the installed products:'
    FocusControl = lvDefines
  end
  object lbName: TLabel
    Left = 12
    Top = 312
    Width = 70
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Define &name:'
    FocusControl = edName
  end
  object cbProduct: TComboBox
    Left = 72
    Top = 12
    Width = 476
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cbProductChange
  end
  object lvDefines: TListView
    Left = 12
    Top = 64
    Width = 536
    Height = 238
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Product'
        Width = 160
      end
      item
        Caption = 'Define'
        Width = 140
      end
      item
        AutoSize = True
        Caption = 'Description'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = lvDefinesColumnClick
    OnDblClick = lvDefinesDblClick
    OnSelectItem = lvDefinesSelectItem
  end
  object edName: TEdit
    Left = 12
    Top = 332
    Width = 536
    Height = 23
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    OnChange = edNameChange
  end
  object btOk: TButton
    Left = 392
    Top = 361
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 473
    Top = 361
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
