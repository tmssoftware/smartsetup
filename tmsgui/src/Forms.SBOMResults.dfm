object SBOMResultsForm: TSBOMResultsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'SBOM results'
  ClientHeight = 361
  ClientWidth = 684
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object lbCaption: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 668
    Height = 15
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alTop
    ShowAccelChar = False
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 3
  end
  object lvResults: TListView
    AlignWithMargins = True
    Left = 8
    Top = 31
    Width = 668
    Height = 289
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alClient
    Columns = <
      item
        Caption = 'Product'
        Width = 180
      end
      item
        Caption = 'Id'
        Width = 140
      end
      item
        Caption = 'Status'
        Width = 80
      end
      item
        Caption = 'File'
        Width = 260
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvResultsDblClick
    OnSelectItem = lvResultsSelectItem
    ExplicitLeft = 3
    ExplicitTop = 29
    ExplicitHeight = 288
  end
  object Panel1: TPanel
    Left = 0
    Top = 320
    Width = 684
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 264
    ExplicitTop = 312
    ExplicitWidth = 209
    object btRegenerate: TButton
      AlignWithMargins = True
      Left = 355
      Top = 8
      Width = 125
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = '&Regenerate skipped'
      ModalResult = 4
      TabOrder = 0
    end
    object btOpenFolder: TButton
      AlignWithMargins = True
      Left = 488
      Top = 8
      Width = 105
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = '&Open folder'
      TabOrder = 1
      OnClick = btOpenFolderClick
      ExplicitLeft = 490
      ExplicitTop = 16
    end
    object btClose: TButton
      AlignWithMargins = True
      Left = 601
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Cancel = True
      Caption = '&Close'
      Default = True
      ModalResult = 8
      TabOrder = 2
      ExplicitTop = 16
    end
  end
end
