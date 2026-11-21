object BackupResultsForm: TBackupResultsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Backup created'
  ClientHeight = 401
  ClientWidth = 624
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
    Width = 608
    Height = 15
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alTop
    Caption = 'The backup was saved to:'
    ShowAccelChar = False
    ExplicitWidth = 135
  end
  object lbProducts: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 62
    Width = 608
    Height = 15
    Margins.Left = 8
    Margins.Top = 12
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alTop
    ShowAccelChar = False
    ExplicitWidth = 3
  end
  object edFile: TEdit
    AlignWithMargins = True
    Left = 8
    Top = 27
    Width = 608
    Height = 23
    Margins.Left = 8
    Margins.Top = 4
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alTop
    ReadOnly = True
    TabOrder = 0
  end
  object lvProducts: TListView
    AlignWithMargins = True
    Left = 8
    Top = 81
    Width = 608
    Height = 279
    Margins.Left = 8
    Margins.Top = 4
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alClient
    Columns = <
      item
        Caption = 'Id'
        Width = 180
      end
      item
        Caption = 'Version'
        Width = 90
      end
      item
        Caption = 'Product'
        Width = 180
      end
      item
        Caption = 'Notes'
        Width = 140
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 360
    Width = 624
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 2
    object btOpenFolder: TButton
      AlignWithMargins = True
      Left = 398
      Top = 8
      Width = 105
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = '&Open folder'
      TabOrder = 0
      OnClick = btOpenFolderClick
    end
    object btClose: TButton
      AlignWithMargins = True
      Left = 511
      Top = 8
      Width = 105
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 8
      TabOrder = 1
    end
  end
end
