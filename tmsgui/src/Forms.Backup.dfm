object BackupForm: TBackupForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Backup'
  ClientHeight = 403
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Bevel: TBevel
    Left = 12
    Top = 358
    Width = 496
    Height = 2
    Shape = bsTopLine
  end
  object CardPanel: TCardPanel
    Left = 0
    Top = 0
    Width = 520
    Height = 356
    ActiveCard = CardOptions
    BevelOuter = bvNone
    Caption = 'CardPanel'
    TabOrder = 0
    object CardOptions: TCard
      Left = 0
      Top = 0
      Width = 520
      Height = 356
      Caption = 'Options'
      CardIndex = 0
      TabOrder = 0
      object lbFile: TLabel
        Left = 12
        Top = 12
        Width = 190
        Height = 15
        Caption = 'Backup &file (.snapshot.zip) to create:'
        FocusControl = edFile
      end
      object edFile: TEdit
        Left = 12
        Top = 31
        Width = 463
        Height = 23
        TabOrder = 0
        OnChange = OptionChanged
      end
      object btBrowse: TButton
        Left = 481
        Top = 30
        Width = 27
        Height = 25
        Hint = 'Browse for the file where to save the backup'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btBrowseClick
      end
      object gbProducts: TGroupBox
        Left = 12
        Top = 68
        Width = 496
        Height = 130
        Caption = ' Products to back up '
        TabOrder = 2
        object lbServerProducts: TLabel
          Left = 31
          Top = 44
          Width = 453
          Height = 30
          AutoSize = False
          Caption = 
            'Every product your license gives you access to in the server is ' +
            'saved, no matter if it is installed or not.'
          FocusControl = rbServerProducts
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object lbSelectedProducts: TLabel
          Left = 31
          Top = 96
          Width = 453
          Height = 30
          AutoSize = False
          Caption = 
            'Only the products selected in the list are saved, together with ' +
            'the products they depend on.'
          FocusControl = rbSelectedProducts
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object rbServerProducts: TRadioButton
          Left = 12
          Top = 24
          Width = 472
          Height = 17
          Caption = 'Back up &all the products from the server'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = OptionChanged
        end
        object rbSelectedProducts: TRadioButton
          Left = 12
          Top = 76
          Width = 472
          Height = 17
          Caption = 'Back up only the &selected products'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
      object gbVersions: TGroupBox
        Left = 12
        Top = 208
        Width = 496
        Height = 130
        Caption = ' Versions '
        TabOrder = 3
        object lbLatestVersions: TLabel
          Left = 31
          Top = 44
          Width = 453
          Height = 30
          AutoSize = False
          Caption = 
            'The latest version of every product is downloaded from its serve' +
            'r and saved in the backup.'
          FocusControl = rbLatestVersions
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object lbManualVersions: TLabel
          Left = 31
          Top = 96
          Width = 453
          Height = 30
          AutoSize = False
          Caption = 
            'In the next page you choose the version to save for each product' +
            '. By default, the version you have installed.'
          FocusControl = rbManualVersions
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object rbLatestVersions: TRadioButton
          Left = 12
          Top = 24
          Width = 472
          Height = 17
          Caption = 'Use the &latest versions'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = OptionChanged
        end
        object rbManualVersions: TRadioButton
          Left = 12
          Top = 76
          Width = 472
          Height = 17
          Caption = '&Choose the version of each product'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
    end
    object CardVersions: TCard
      Left = 0
      Top = 0
      Width = 520
      Height = 356
      Caption = 'Versions'
      CardIndex = 1
      TabOrder = 1
      object lbVersionsInfo: TLabel
        Left = 12
        Top = 12
        Width = 496
        Height = 30
        AutoSize = False
        Caption = 
          'Choose the version to save for each product. Products they depen' +
          'd on, and that are not in this list, are saved with their latest' +
          ' version.'
        ShowAccelChar = False
        WordWrap = True
      end
      object lbVersion: TLabel
        Left = 12
        Top = 306
        Width = 244
        Height = 15
        AutoSize = False
        Caption = '&Version:'
        EllipsisPosition = epEndEllipsis
        FocusControl = cbVersion
      end
      object lvVersions: TListView
        Left = 12
        Top = 46
        Width = 496
        Height = 250
        Columns = <
          item
            Caption = 'Product'
            Width = 170
          end
          item
            Caption = 'Id'
            Width = 150
          end
          item
            Caption = 'Installed'
            Width = 75
          end
          item
            Caption = 'Back up'
            Width = 75
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lvVersionsSelectItem
      end
      object cbVersion: TComboBox
        Left = 12
        Top = 324
        Width = 244
        Height = 23
        TabOrder = 1
        OnChange = cbVersionChange
        OnDropDown = cbVersionDropDown
      end
      object btAllLatest: TButton
        Left = 268
        Top = 323
        Width = 117
        Height = 25
        Caption = 'All to la&test'
        TabOrder = 2
        OnClick = btAllLatestClick
      end
      object btAllInstalled: TButton
        Left = 391
        Top = 323
        Width = 117
        Height = 25
        Caption = 'All to &installed'
        TabOrder = 3
        OnClick = btAllInstalledClick
      end
    end
  end
  object btBack: TButton
    Left = 271
    Top = 368
    Width = 75
    Height = 25
    Caption = '< Bac&k'
    TabOrder = 1
    OnClick = btBackClick
  end
  object btNext: TButton
    Left = 352
    Top = 368
    Width = 75
    Height = 25
    Caption = '&Backup'
    Default = True
    TabOrder = 2
    OnClick = btNextClick
  end
  object btCancel: TButton
    Left = 433
    Top = 368
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object SaveDialog: TFileSaveDialog
    DefaultExtension = 'snapshot.zip'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Smart Setup backups (*.snapshot.zip)'
        FileMask = '*.snapshot.zip'
      end
      item
        DisplayName = 'All files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist]
    Title = 'Save the backup as'
    Left = 32
    Top = 360
  end
end
