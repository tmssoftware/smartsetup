object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TMS Smart Setup'
  ClientHeight = 561
  ClientWidth = 921
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
    Width = 921
    Height = 542
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 921
      Height = 383
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
          Height = 320
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
          Width = 703
          Height = 320
          Align = alClient
          BorderStyle = bsNone
          Columns = <
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
          PopupMenu = pmProducts
          TabOrder = 2
          ViewStyle = vsReport
          OnColumnClick = lvProductsColumnClick
          OnCompare = lvProductsCompare
          OnCustomDrawItem = lvProductsCustomDrawItem
        end
        object RightPanel: TPanel
          Left = 808
          Top = 35
          Width = 105
          Height = 320
          Align = alRight
          BevelEdges = [beLeft]
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 3
          DesignSize = (
            103
            320)
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
            Top = 288
            Width = 89
            Height = 25
            Action = acConfigure
            Anchors = [akLeft, akBottom]
            TabOrder = 3
          end
          object btCredentials: TButton
            Left = 8
            Top = 256
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
          Width = 913
          Height = 35
          Align = alTop
          BevelEdges = [beBottom]
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            913
            33)
          object btConfiguration2: TSpeedButton
            Left = 882
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
            Left = 742
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
            Left = 784
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
          Width = 913
          Height = 355
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
      Top = 407
      Width = 921
      Height = 135
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      object LogSplitter: TSplitter
        Left = 0
        Top = 62
        Width = 921
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Visible = False
        ExplicitTop = 0
        ExplicitWidth = 65
      end
      object mmLogDetails: TMemo
        Left = 0
        Top = 65
        Width = 921
        Height = 70
        Align = alBottom
        BorderStyle = bsNone
        Ctl3D = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = False
        TabOrder = 0
        Visible = False
        WordWrap = False
      end
      object lbLogItems: TListBox
        Left = 0
        Top = 0
        Width = 921
        Height = 62
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 1
        OnClick = lbLogItemsClick
      end
    end
    object ProgressPanel: TPanel
      Left = 0
      Top = 383
      Width = 921
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object SpeedButton1: TSpeedButton
        Left = 864
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
        Width = 864
        Height = 24
        Align = alClient
        Smooth = True
        TabOrder = 0
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 542
    Width = 921
    Height = 19
    Panels = <
      item
        Bevel = pbNone
        Width = 60
      end
      item
        Bevel = pbNone
        Width = 300
      end>
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
  end
end
