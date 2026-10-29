object RestoreForm: TRestoreForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Restore backup'
  ClientHeight = 385
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 15
  object lbFile: TLabel
    Left = 12
    Top = 12
    Width = 240
    Height = 15
    Caption = '&Backup file (.snapshot.zip or .snapshot.yaml) to restore:'
    FocusControl = edFile
  end
  object edFile: TEdit
    Left = 12
    Top = 31
    Width = 403
    Height = 23
    TabOrder = 0
    OnChange = edFileChange
  end
  object btBrowse: TButton
    Left = 421
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
    Width = 436
    Height = 130
    Caption = ' IDE registration '
    TabOrder = 2
    object lbRegisterAuto: TLabel
      Left = 31
      Top = 44
      Width = 393
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
      Width = 393
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
      Width = 412
      Height = 17
      Caption = '&Register in the IDE as set in the configuration'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbSkipRegister: TRadioButton
      Left = 12
      Top = 76
      Width = 412
      Height = 17
      Caption = '&Don'#39't register in the IDE'
      TabOrder = 1
    end
  end
  object gbProducts: TGroupBox
    Left = 12
    Top = 208
    Width = 436
    Height = 130
    Caption = ' Products to restore '
    TabOrder = 3
    object lbAllProducts: TLabel
      Left = 31
      Top = 44
      Width = 393
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
    object lbSelectedProducts: TLabel
      Left = 31
      Top = 96
      Width = 393
      Height = 30
      AutoSize = False
      Caption = 
        'Only the products selected in the list are restored. Selected pr' +
        'oducts that are not in the backup are ignored.'
      FocusControl = rbSelectedProducts
      ShowAccelChar = False
      WordWrap = True
      OnClick = OptionLabelClick
    end
    object rbAllProducts: TRadioButton
      Left = 12
      Top = 24
      Width = 412
      Height = 17
      Caption = 'Restore &all the products in the backup'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbSelectedProducts: TRadioButton
      Left = 12
      Top = 76
      Width = 412
      Height = 17
      Caption = 'Restore only the &selected products'
      TabOrder = 1
    end
  end
  object btRestore: TButton
    Left = 292
    Top = 350
    Width = 75
    Height = 25
    Caption = 'Res&tore'
    Default = True
    TabOrder = 4
    OnClick = btRestoreClick
  end
  object btCancel: TButton
    Left = 373
    Top = 350
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
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
    Top = 344
  end
end
