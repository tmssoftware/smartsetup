object RestoreForm: TRestoreForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Restore backup'
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
        Width = 290
        Height = 15
        Caption = '&Backup file (.snapshot.zip or .snapshot.yaml) to restore:'
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
        Hint = 'Browse for the backup file'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btBrowseClick
      end
      object gbRegister: TGroupBox
        Left = 12
        Top = 68
        Width = 496
        Height = 130
        Caption = ' IDE registration '
        TabOrder = 2
        object lbRegisterAuto: TLabel
          Left = 31
          Top = 44
          Width = 453
          Height = 30
          AutoSize = False
          Caption =
            'Components are registered in the IDEs, unless your Smart Setup c' +
            'onfiguration says to skip registering them.'
          FocusControl = rbRegisterAuto
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object lbSkipRegister: TLabel
          Left = 31
          Top = 96
          Width = 453
          Height = 30
          AutoSize = False
          Caption =
            'Components are only built, and not registered in any IDE, whatev' +
            'er your configuration says.'
          FocusControl = rbSkipRegister
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object rbRegisterAuto: TRadioButton
          Left = 12
          Top = 24
          Width = 472
          Height = 17
          Caption = '&Register in the IDE as set in the configuration'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbSkipRegister: TRadioButton
          Left = 12
          Top = 76
          Width = 472
          Height = 17
          Caption = '&Don'#39't register in the IDE'
          TabOrder = 1
        end
      end
      object gbProducts: TGroupBox
        Left = 12
        Top = 208
        Width = 496
        Height = 130
        Caption = ' Products to restore '
        TabOrder = 3
        object lbAllProducts: TLabel
          Left = 31
          Top = 44
          Width = 453
          Height = 30
          AutoSize = False
          Caption =
            'Every product saved in the backup is installed, with the version' +
            ' it had when the backup was made.'
          FocusControl = rbAllProducts
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object lbManualProducts: TLabel
          Left = 31
          Top = 96
          Width = 453
          Height = 30
          AutoSize = False
          Caption =
            'In the next page you see the products in the backup, and you che' +
            'ck the ones to restore.'
          FocusControl = rbManualProducts
          ShowAccelChar = False
          WordWrap = True
          OnClick = OptionLabelClick
        end
        object rbAllProducts: TRadioButton
          Left = 12
          Top = 24
          Width = 472
          Height = 17
          Caption = 'Restore &all the products in the backup'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = OptionChanged
        end
        object rbManualProducts: TRadioButton
          Left = 12
          Top = 76
          Width = 472
          Height = 17
          Caption = '&Manually select the products to restore'
          TabOrder = 1
          OnClick = OptionChanged
        end
      end
    end
    object CardProducts: TCard
      Left = 0
      Top = 0
      Width = 520
      Height = 356
      Caption = 'Products'
      CardIndex = 1
      TabOrder = 1
      object lbProductsInfo: TLabel
        Left = 12
        Top = 12
        Width = 496
        Height = 30
        AutoSize = False
        Caption = 'Check the products to restore.'
        ShowAccelChar = False
        WordWrap = True
      end
      object lbCheckedCount: TLabel
        Left = 258
        Top = 328
        Width = 250
        Height = 15
        Alignment = taRightJustify
        AutoSize = False
        ShowAccelChar = False
      end
      object lvProducts: TListView
        Left = 12
        Top = 46
        Width = 496
        Height = 266
        Checkboxes = True
        Columns = <
          item
            Caption = 'Product'
            Width = 150
          end
          item
            Caption = 'Id'
            Width = 120
          end
          item
            Caption = 'Version'
            Width = 65
          end
          item
            Caption = 'Installed'
            Width = 65
          end
          item
            Caption = 'From'
            Width = 70
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnItemChecked = lvProductsItemChecked
      end
      object btSelectAll: TButton
        Left = 12
        Top = 323
        Width = 100
        Height = 25
        Caption = 'Select &all'
        TabOrder = 1
        OnClick = btSelectAllClick
      end
      object btSelectNone: TButton
        Left = 118
        Top = 323
        Width = 100
        Height = 25
        Caption = 'Select &none'
        TabOrder = 2
        OnClick = btSelectNoneClick
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
    Caption = 'Res&tore'
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
  object OpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Smart Setup backups (*.snapshot.zip, *.snapshot.yaml)'
        FileMask = '*.snapshot.zip;*.snapshot.yaml'
      end
      item
        DisplayName = 'All files (*.*)'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist]
    Title = 'Select the backup to restore'
    Left = 32
    Top = 360
  end
end
