object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 606
  ClientWidth = 820
  Color = clWindow
  Constraints.MinHeight = 600
  Constraints.MinWidth = 740
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object ProdSeparator: TBevel
    AlignWithMargins = True
    Left = 156
    Top = 32
    Width = 6
    Height = 501
    Margins.Left = 0
    Margins.Top = 32
    Margins.Right = 0
    Margins.Bottom = 32
    Align = alLeft
    Shape = bsRightLine
    Style = bsRaised
    Visible = False
    ExplicitTop = 0
    ExplicitHeight = 565
  end
  object pnNav: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 140
    Height = 551
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 6
    Align = alLeft
    BevelEdges = [beRight]
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    ExplicitHeight = 541
    object lbNav: TListBox
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 124
      Height = 543
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Style = lbOwnerDrawFixed
      Align = alClient
      BevelEdges = [beRight]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ItemHeight = 28
      Items.Strings = (
        'General'
        'Servers'
        'Products')
      TabOrder = 0
      OnClick = lbNavClick
      OnDrawItem = lbNavDrawItem
      ExplicitHeight = 516
    end
  end
  object cpMain: TCardPanel
    AlignWithMargins = True
    Left = 165
    Top = 8
    Width = 647
    Height = 549
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    ActiveCard = cdServers
    BevelOuter = bvNone
    Caption = 'cpMain'
    ParentColor = True
    TabOrder = 1
    ExplicitLeft = 156
    ExplicitWidth = 656
    ExplicitHeight = 557
    object cdGeneral: TCard
      Left = 0
      Top = 0
      Width = 647
      Height = 549
      Caption = 'General'
      CardIndex = 0
      ParentColor = True
      TabOrder = 0
      ExplicitWidth = 656
      ExplicitHeight = 557
      object pnGeneralHeader: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        ExplicitWidth = 656
        DesignSize = (
          647
          36)
        object lbGeneralHeader: TLabel
          Left = 8
          Top = 6
          Width = 69
          Height = 25
          Caption = 'General'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object bvGeneralHeader: TBevel
          Left = 8
          Top = 34
          Width = 631
          Height = 2
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
          ExplicitWidth = 640
        end
      end
      object pnGeneral: TPanel
        Left = 0
        Top = 36
        Width = 647
        Height = 513
        Margins.Top = 0
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        ExplicitWidth = 656
        ExplicitHeight = 521
        DesignSize = (
          647
          513)
        object lbAltRegKey: TLabel
          Left = 16
          Top = 16
          Width = 114
          Height = 15
          Caption = 'Alternate &registry key:'
          FocusControl = edAltRegKey
        end
        object lbAltRegKeyHint: TLabel
          Left = 16
          Top = 66
          Width = 611
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Installs the components in an alternate registry key, so you can' +
            ' have different components installed in the same Rad Studio inst' +
            'all. Leave it empty to use the default registry key.'
          WordWrap = True
          ExplicitWidth = 620
        end
        object edAltRegKey: TEdit
          Left = 16
          Top = 36
          Width = 320
          Height = 23
          TabOrder = 0
          OnChange = edAltRegKeyChange
        end
      end
    end
    object cdServers: TCard
      Left = 0
      Top = 0
      Width = 647
      Height = 549
      Caption = 'Servers'
      CardIndex = 1
      TabOrder = 1
      ExplicitWidth = 656
      ExplicitHeight = 557
      object pnServersHeader: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        ExplicitWidth = 656
        DesignSize = (
          647
          36)
        object lbServersHeader: TLabel
          Left = 8
          Top = 6
          Width = 66
          Height = 25
          Caption = 'Servers'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object bvServersHeader: TBevel
          Left = 8
          Top = 34
          Width = 631
          Height = 2
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
          ExplicitWidth = 640
        end
      end
      object pnServers: TPanel
        Left = 0
        Top = 36
        Width = 647
        Height = 513
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        ExplicitWidth = 656
        ExplicitHeight = 521
        DesignSize = (
          647
          513)
        object lbServerList: TCheckListBox
          Left = 16
          Top = 8
          Width = 177
          Height = 474
          Margins.Left = 20
          Margins.Right = 0
          Margins.Bottom = 0
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 17
          TabOrder = 0
          OnClick = lbServerListClick
          OnClickCheck = lbServerListClickCheck
        end
        object btServerAdd: TButton
          Left = 16
          Top = 485
          Width = 86
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Add'
          TabOrder = 1
          OnClick = btServerAddClick
        end
        object btServerRemove: TButton
          Left = 107
          Top = 485
          Width = 86
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'R&emove'
          TabOrder = 2
          OnClick = btServerRemoveClick
        end
        object pnServerDetails: TPanel
          Left = 203
          Top = 8
          Width = 436
          Height = 497
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 3
          ExplicitWidth = 445
          ExplicitHeight = 505
          DesignSize = (
            436
            497)
          object lbServerName: TLabel
            Left = 0
            Top = 3
            Width = 35
            Height = 15
            Caption = '&Name:'
            FocusControl = edServerName
          end
          object lbServerUrl: TLabel
            Left = 0
            Top = 32
            Width = 18
            Height = 15
            Caption = '&Url:'
            FocusControl = edServerUrl
          end
          object lbServerType: TLabel
            Left = 287
            Top = 3
            Width = 28
            Height = 15
            Anchors = [akTop, akRight]
            Caption = '&Type:'
            FocusControl = cbServerType
            ExplicitLeft = 296
          end
          object edServerName: TEdit
            Left = 48
            Top = 0
            Width = 227
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = edServerNameChange
            ExplicitWidth = 236
          end
          object cbServerType: TComboBox
            Left = 321
            Top = 0
            Width = 115
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnClick = cbServerTypeClick
            ExplicitLeft = 330
          end
          object edServerUrl: TEdit
            Left = 48
            Top = 29
            Width = 388
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            OnChange = edServerUrlChange
            ExplicitWidth = 397
          end
          object cbAllowInsecure: TCheckBox
            Left = 48
            Top = 60
            Width = 388
            Height = 19
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &insecure connections (http)'
            TabOrder = 3
            OnClick = cbAllowInsecureClick
            ExplicitWidth = 397
          end
        end
      end
    end
    object cdProducts: TCard
      Left = 0
      Top = 0
      Width = 647
      Height = 549
      Caption = 'Products'
      CardIndex = 2
      ParentColor = True
      TabOrder = 2
      ExplicitWidth = 656
      ExplicitHeight = 557
      object pnProductsHeader: TPanel
        Left = 0
        Top = 0
        Width = 647
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 1
        ExplicitWidth = 680
        DesignSize = (
          647
          36)
        object lbProductsHeader: TLabel
          Left = 8
          Top = 6
          Width = 80
          Height = 25
          Caption = 'Products'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object bvProductsHeader: TBevel
          Left = 8
          Top = 34
          Width = 631
          Height = 2
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
          ExplicitWidth = 640
        end
        object btProductAdd: TButton
          AlignWithMargins = True
          Left = 457
          Top = 4
          Width = 87
          Height = 28
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 8
          Margins.Bottom = 4
          Align = alRight
          Caption = '&Add...'
          TabOrder = 0
          OnClick = btProductAddClick
          ExplicitLeft = 486
          ExplicitTop = 3
          ExplicitHeight = 25
        end
        object btProductRemove: TButton
          AlignWithMargins = True
          Left = 552
          Top = 4
          Width = 87
          Height = 28
          Margins.Left = 0
          Margins.Top = 4
          Margins.Right = 8
          Margins.Bottom = 4
          Align = alRight
          Caption = 'R&emove'
          TabOrder = 1
          OnClick = btProductRemoveClick
          ExplicitLeft = 579
          ExplicitTop = 3
          ExplicitHeight = 25
        end
      end
      object pnProducts: TPanel
        Left = 0
        Top = 36
        Width = 647
        Height = 513
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        ExplicitWidth = 656
        ExplicitHeight = 521
        object tcProducts: TTabControl
          AlignWithMargins = True
          Left = 8
          Top = 10
          Width = 639
          Height = 503
          Margins.Left = 8
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          HotTrack = True
          Style = tsButtons
          TabOrder = 0
          OnChange = tcProductsChange
          object pnProduct: TPanel
            Left = 4
            Top = 6
            Width = 631
            Height = 493
            Margins.Top = 10
            Align = alClient
            BevelOuter = bvNone
            Padding.Left = 4
            Padding.Top = 4
            Padding.Right = 4
            Padding.Bottom = 4
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            object gbProductGeneral: TGroupBox
              Left = 4
              Top = 4
              Width = 623
              Height = 297
              Align = alTop
              Caption = 'General'
              Padding.Left = 4
              Padding.Right = 4
              Padding.Bottom = 4
              TabOrder = 0
              object pnSkipRegister: TPanel
                Left = 6
                Top = 17
                Width = 611
                Height = 34
                Align = alTop
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 0
                object lbSkipRegister: TLabel
                  Left = 6
                  Top = 8
                  Width = 67
                  Height = 15
                  Caption = 'Skip re&gister:'
                  FocusControl = cbSkipRegister
                end
                object cbSkipRegister: TComboBox
                  Left = 104
                  Top = 4
                  Width = 180
                  Height = 23
                  Style = csDropDownList
                  TabOrder = 0
                  OnChange = SkipRegisterChanged
                  Items.Strings = (
                    'Default'
                    'No'
                    'Yes'
                    'Custom')
                end
              end
              object pnSkipRegisterCustom: TPanel
                Left = 6
                Top = 51
                Width = 611
                Height = 46
                Align = alTop
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 1
                Visible = False
                DesignSize = (
                  611
                  46)
                object clbSkipRegister: TCheckListBox
                  Left = 104
                  Top = 2
                  Width = 501
                  Height = 40
                  Anchors = [akLeft, akTop, akRight]
                  Columns = 4
                  ItemHeight = 17
                  TabOrder = 0
                  OnClickCheck = SkipRegisterChanged
                end
              end
              object pnIdLists: TPanel
                Left = 6
                Top = 97
                Width = 611
                Height = 176
                Align = alTop
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 2
                DesignSize = (
                  611
                  176)
                object lbDelphiVersions: TLabel
                  Left = 6
                  Top = 4
                  Width = 83
                  Height = 15
                  Caption = 'Delphi versions:'
                end
                object lbPlatforms: TLabel
                  Left = 266
                  Top = 4
                  Width = 54
                  Height = 15
                  Caption = 'Platforms:'
                end
                object cbDelphiVersionsNotSet: TCheckBox
                  Left = 6
                  Top = 23
                  Width = 245
                  Height = 19
                  Caption = 'All Delphi versions'
                  TabOrder = 0
                  OnClick = DelphiVersionsChanged
                end
                object clbDelphiVersions: TCheckListBox
                  Left = 6
                  Top = 46
                  Width = 245
                  Height = 125
                  ItemHeight = 17
                  TabOrder = 1
                  OnClickCheck = DelphiVersionsChanged
                end
                object cbPlatformsNotSet: TCheckBox
                  Left = 266
                  Top = 23
                  Width = 339
                  Height = 19
                  Anchors = [akLeft, akTop, akRight]
                  Caption = 'All platforms'
                  TabOrder = 2
                  OnClick = PlatformsChanged
                end
                object clbPlatforms: TCheckListBox
                  Left = 266
                  Top = 46
                  Width = 339
                  Height = 125
                  Anchors = [akLeft, akTop, akRight]
                  Columns = 2
                  ItemHeight = 17
                  TabOrder = 3
                  OnClickCheck = PlatformsChanged
                end
              end
              object pnOverrideNote: TPanel
                Left = 6
                Top = 273
                Width = 611
                Height = 20
                Align = alTop
                BevelOuter = bvNone
                ParentColor = True
                TabOrder = 3
                DesignSize = (
                  611
                  20)
                object lbOverrideNote: TLabel
                  Left = 6
                  Top = 2
                  Width = 599
                  Height = 15
                  Anchors = [akLeft, akTop, akRight]
                  AutoSize = False
                  Caption = 
                    'A list set here replaces the list in All products, it is not add' +
                    'ed to it.'
                  ExplicitWidth = 505
                end
              end
            end
            object gbProductCompilation: TGroupBox
              AlignWithMargins = True
              Left = 4
              Top = 307
              Width = 623
              Height = 182
              Margins.Left = 0
              Margins.Top = 6
              Margins.Right = 0
              Margins.Bottom = 0
              Align = alClient
              Caption = 'Compilation'
              TabOrder = 1
              DesignSize = (
                623
                182)
              object lbDebugDcus: TLabel
                Left = 12
                Top = 27
                Width = 66
                Height = 15
                Caption = 'Debug &dcus:'
                FocusControl = cbDebugDcus
              end
              object lbDefines: TLabel
                Left = 12
                Top = 58
                Width = 42
                Height = 15
                Caption = 'De&fines:'
                FocusControl = clbDefines
              end
              object cbDebugDcus: TComboBox
                Left = 110
                Top = 23
                Width = 180
                Height = 23
                Style = csDropDownList
                TabOrder = 0
                OnChange = cbDebugDcusChange
                Items.Strings = (
                  'Default'
                  'No'
                  'Yes')
              end
              object clbDefines: TCheckListBox
                Left = 110
                Top = 54
                Width = 412
                Height = 115
                Hint = 
                  'Unchecked defines are set to false, which removes them if they a' +
                  're set in All products.'
                Anchors = [akLeft, akTop, akRight, akBottom]
                ItemHeight = 17
                ParentShowHint = False
                ShowHint = True
                TabOrder = 1
                OnClickCheck = clbDefinesClickCheck
                ExplicitWidth = 406
              end
              object btDefineAdd: TButton
                Left = 530
                Top = 54
                Width = 87
                Height = 25
                Anchors = [akTop, akRight]
                Caption = 'Add...'
                TabOrder = 2
                OnClick = btDefineAddClick
                ExplicitLeft = 524
              end
              object btDefineRemove: TButton
                Left = 530
                Top = 85
                Width = 87
                Height = 25
                Anchors = [akTop, akRight]
                Caption = 'Remove'
                TabOrder = 3
                OnClick = btDefineRemoveClick
                ExplicitLeft = 524
              end
            end
          end
        end
      end
    end
  end
  object PanelBtn: TPanel
    Left = 0
    Top = 565
    Width = 820
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btApply: TButton
      AlignWithMargins = True
      Left = 737
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = 'A&pply'
      TabOrder = 0
      OnClick = btApplyClick
      ExplicitTop = 15
    end
    object btCancel: TButton
      AlignWithMargins = True
      Left = 654
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 656
      ExplicitTop = 15
    end
    object btOk: TButton
      AlignWithMargins = True
      Left = 571
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = '&Ok'
      Default = True
      TabOrder = 2
      OnClick = btOkClick
      ExplicitLeft = 575
      ExplicitTop = 15
    end
    object btConfig: TButton
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 121
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alLeft
      Caption = 'Edit Config File'
      TabOrder = 3
      OnClick = btConfigClick
      ExplicitTop = 0
      ExplicitHeight = 39
    end
  end
end
