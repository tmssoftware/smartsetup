object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMS Smart Setup'
  ClientHeight = 561
  ClientWidth = 971
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 971
    Height = 512
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object LogSplitter: TSplitter
      Left = 0
      Top = 365
      Width = 971
      Height = 12
      Cursor = crVSplit
      Align = alBottom
      Beveled = True
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
      ExplicitTop = 371
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 971
      Height = 365
      ActivePage = tsProducts
      Align = alClient
      MultiLine = True
      TabOrder = 0
      TabPosition = tpBottom
      object tsProducts: TTabSheet
        Caption = 'Products'
        object LeftPanel: TPanel
          Left = 0
          Top = 35
          Width = 105
          Height = 302
          Align = alLeft
          BevelEdges = [beRight]
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 1
          object rbAll: TRadioButton
            Left = 2
            Top = 4
            Width = 113
            Height = 17
            Action = acFilterAll
            TabOrder = 0
          end
          object rbInstalled: TRadioButton
            Left = 2
            Top = 27
            Width = 113
            Height = 17
            Action = acFilterInstalled
            TabOrder = 1
          end
        end
        object lvProducts: TListView
          Left = 105
          Top = 35
          Width = 753
          Height = 302
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
            end
            item
              Caption = 'Product Id'
              Width = 120
            end
            item
              Caption = 'Product Name'
              Width = 250
            end
            item
              Caption = 'Local version'
              Width = 100
            end
            item
              Caption = 'Remote version'
              Width = 100
            end
            item
              Caption = 'Status'
              Width = 100
            end>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          HideSelection = False
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          ParentFont = False
          ParentShowHint = False
          PopupMenu = pmProducts
          ShowHint = True
          TabOrder = 2
          ViewStyle = vsReport
          OnColumnClick = lvProductsColumnClick
          OnCompare = lvProductsCompare
          OnInfoTip = lvProductsInfoTip
          OnMouseMove = lvProductsMouseMove
        end
        object RightPanel: TPanel
          Left = 858
          Top = 35
          Width = 105
          Height = 302
          Align = alRight
          BevelEdges = [beLeft]
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 3
          DesignSize = (
            103
            302)
          object Button1: TButton
            Left = 8
            Top = 66
            Width = 89
            Height = 25
            Action = acFullBuild
            DropDownMenu = BuildMenu
            Style = bsSplitButton
            TabOrder = 0
          end
          object Button2: TButton
            Left = 8
            Top = 3
            Width = 89
            Height = 25
            Action = acInstall
            TabOrder = 1
          end
          object Button3: TButton
            Left = 8
            Top = 34
            Width = 89
            Height = 25
            Action = acUninstall
            TabOrder = 2
          end
          object Button4: TButton
            Left = 8
            Top = 270
            Width = 89
            Height = 25
            Action = acConfigure
            Anchors = [akLeft, akBottom]
            TabOrder = 3
          end
          object btCredentials: TButton
            Left = 8
            Top = 238
            Width = 89
            Height = 25
            Action = acCredentials
            Anchors = [akLeft, akBottom]
            TabOrder = 4
          end
        end
        object TopPanel: TPanel
          Left = 0
          Top = 0
          Width = 963
          Height = 35
          Align = alTop
          BevelEdges = [beBottom]
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            963
            33)
          object btConfiguration2: TSpeedButton
            Left = 932
            Top = 3
            Width = 23
            Height = 25
            Action = acSettings
            Anchors = [akTop, akRight]
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -16
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ExplicitLeft = 871
          end
          object lbServer: TLabel
            Left = 792
            Top = 8
            Width = 35
            Height = 15
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            Caption = 'Server:'
          end
          object edSearch: TEdit
            Left = 5
            Top = 5
            Width = 236
            Height = 23
            TabOrder = 0
            TextHint = 'Search (Ctrl+E)'
            OnChange = edSearchChange
          end
          object cbServer: TComboBox
            Left = 834
            Top = 5
            Width = 91
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnChange = cbServerChange
          end
        end
      end
      object tsOutput: TTabSheet
        Caption = 'Output'
        ImageIndex = 2
        object OutputMemo: TMemo
          Left = 0
          Top = 0
          Width = 963
          Height = 337
          Align = alClient
          BorderStyle = bsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object LogPanel: TPanel
      Left = 0
      Top = 377
      Width = 971
      Height = 135
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lbLog: TControlList
        Left = 0
        Top = 0
        Width = 971
        Height = 135
        Align = alClient
        ItemHeight = 24
        ItemMargins.Left = 0
        ItemMargins.Top = 0
        ItemMargins.Right = 0
        ItemMargins.Bottom = 0
        ItemSelectionOptions.HotColorAlpha = 50
        ItemSelectionOptions.SelectedColorAlpha = 70
        ItemSelectionOptions.FocusedColorAlpha = 80
        ParentColor = False
        TabOrder = 0
        OnBeforeDrawItem = lbLogBeforeDrawItem
        OnItemDblClick = acLogDetailsExecute
        object lblError: TLabel
          AlignWithMargins = True
          Left = 76
          Top = 4
          Width = 711
          Height = 16
          Margins.Left = 20
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alClient
          AutoSize = False
          Caption = 'Error'
          EllipsisPosition = epEndEllipsis
          ShowAccelChar = False
          Transparent = True
          WordWrap = True
          ExplicitWidth = 25
          ExplicitHeight = 15
        end
        object btnShowLog: TControlListButton
          AlignWithMargins = True
          Left = 795
          Top = 2
          Width = 80
          Height = 20
          Margins.Left = 4
          Margins.Top = 2
          Margins.Right = 4
          Margins.Bottom = 2
          Action = acLogDetails
          Align = alRight
          Style = clbkToolButton
          ExplicitTop = -3
          ExplicitHeight = 14
        end
        object lblErrorCaption: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 4
          Width = 13
          Height = 16
          Margins.Top = 4
          Margins.Bottom = 4
          Align = alLeft
          Caption = #10060
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitHeight = 15
        end
        object lblTime: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 4
          Width = 31
          Height = 16
          Margins.Top = 4
          Margins.Bottom = 4
          Align = alLeft
          Caption = '10:00'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitHeight = 15
        end
        object btnOpenHTMLLog: TControlListButton
          AlignWithMargins = True
          Left = 883
          Top = 2
          Width = 80
          Height = 20
          Margins.Left = 4
          Margins.Top = 2
          Margins.Right = 4
          Margins.Bottom = 2
          Action = acViewHtmlLog
          Align = alRight
          Style = clbkToolButton
          ExplicitLeft = 856
          ExplicitTop = 5
        end
      end
    end
  end
  object StatusBar: TStatusBar
    AlignWithMargins = True
    Left = 3
    Top = 539
    Width = 965
    Height = 19
    Panels = <
      item
        Bevel = pbNone
        Width = 60
      end
      item
        Bevel = pbNone
        Width = 300
      end
      item
        Width = 50
      end
      item
        Alignment = taRightJustify
        Width = 200
      end>
    OnClick = StatusBarClick
  end
  object ProgressPanel: TPanel
    Left = 0
    Top = 512
    Width = 971
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object SpeedButton1: TSpeedButton
      Left = 914
      Top = 0
      Width = 57
      Height = 24
      Action = acCancelExecution
      Align = alRight
      Flat = True
      ExplicitLeft = 840
      ExplicitHeight = 22
    end
    object ProgressBar: TProgressBar
      Left = 0
      Top = 0
      Width = 914
      Height = 24
      Align = alClient
      Smooth = True
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Left = 240
    Top = 96
    object acInstallVersion: TAction
      Category = 'Menus'
      Caption = 'Install ver&sion...'
      OnExecute = acInstallVersionExecute
      OnUpdate = acInstallVersionUpdate
    end
    object acInstall: TAction
      Category = 'Commands'
      Caption = '&Install'
      OnExecute = acInstallExecute
      OnUpdate = acInstallUpdate
    end
    object acUninstall: TAction
      Category = 'Commands'
      Caption = '&Uninstall'
      OnExecute = acUninstallExecute
      OnUpdate = acUninstallUpdate
    end
    object acCancelExecution: TAction
      Category = 'Commands'
      Caption = 'Cancel'
      OnExecute = acCancelExecutionExecute
      OnUpdate = acCancelExecutionUpdate
    end
    object acFilterAll: TAction
      Category = 'Commands'
      Caption = 'All'
      OnExecute = acFilterAllExecute
      OnUpdate = acFilterAllUpdate
    end
    object acFilterInstalled: TAction
      Category = 'Commands'
      Caption = 'Installed'
      OnExecute = acFilterInstalledExecute
      OnUpdate = acFilterInstalledUpdate
    end
    object acFullBuild: TAction
      Category = 'Commands'
      Caption = '&Full build'
      OnExecute = acFullBuildExecute
      OnUpdate = acFullBuildUpdate
    end
    object acPartialBuild: TAction
      Category = 'Commands'
      Caption = '&Partial build'
      OnExecute = acPartialBuildExecute
      OnUpdate = acPartialBuildUpdate
    end
    object acCredentials: TAction
      Category = 'Commands'
      Caption = '&Credentials'
      OnExecute = acCredentialsExecute
      OnUpdate = acCredentialsUpdate
    end
    object acConfigure: TAction
      Category = 'Commands'
      Caption = 'C&onfigure'
      OnExecute = acConfigureExecute
      OnUpdate = acConfigureUpdate
    end
    object acVersionHistory: TAction
      Category = 'Menus'
      Caption = '&Version history'
      OnExecute = acVersionHistoryExecute
      OnUpdate = acVersionHistoryUpdate
    end
    object acSettings: TAction
      Category = 'Menus'
      Caption = #9881
      ShortCut = 24698
      OnExecute = acSettingsExecute
      OnUpdate = acSettingsUpdate
    end
    object acSearchFocus: TAction
      Category = 'Menus'
      ShortCut = 16453
      OnExecute = acSearchFocusExecute
    end
    object acPin: TAction
      Category = 'Menus'
      Caption = 'Pin version'
      OnExecute = acPinExecute
      OnUpdate = acPinUpdate
    end
    object acUnpin: TAction
      Category = 'Menus'
      Caption = 'Unpin version'
      OnExecute = acUnpinExecute
      OnUpdate = acUnpinUpdate
    end
    object acLogDetails: TAction
      Category = 'ErrorBox'
      Caption = 'Details'
      OnExecute = acLogDetailsExecute
    end
    object acViewHtmlLog: TAction
      Category = 'ErrorBox'
      Caption = 'Log'
      OnExecute = acViewHtmlLogExecute
    end
  end
  object BuildMenu: TPopupMenu
    Left = 848
    Top = 144
    object Rebuild1: TMenuItem
      Action = acFullBuild
    end
    object Partialbuild1: TMenuItem
      Action = acPartialBuild
    end
  end
  object pmProducts: TPopupMenu
    Left = 344
    Top = 96
    object Installversion1: TMenuItem
      Action = acInstallVersion
    end
    object Openversionhistory1: TMenuItem
      Action = acVersionHistory
    end
    object Pinversion1: TMenuItem
      Action = acPin
    end
    object Unpinversion1: TMenuItem
      Action = acUnpin
    end
  end
  object WorkingFolderDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist, fdoDontAddToRecent]
    Title = 'Select folder to install smartsetup bundles'
    Left = 480
    Top = 288
  end
end
