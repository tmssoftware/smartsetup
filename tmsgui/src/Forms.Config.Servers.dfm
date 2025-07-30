object ServerConfigForm: TServerConfigForm
  Left = 0
  Top = 0
  Caption = 'Servers configuration'
  ClientHeight = 255
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    492
    255)
  TextHeight = 15
  object lbServerList: TCheckListBox
    Left = 0
    Top = 0
    Width = 175
    Height = 224
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 17
    TabOrder = 0
    OnClick = lbServerListClick
    OnClickCheck = lbServerListClickCheck
  end
  object btServerAdd: TButton
    Left = 0
    Top = 229
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add'
    TabOrder = 1
    OnClick = btServerAddClick
  end
  object btServerRemove: TButton
    Left = 90
    Top = 229
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 2
    OnClick = btServerRemoveClick
  end
  object pnServerDetails: TPanel
    Left = 180
    Top = 0
    Width = 309
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    DesignSize = (
      309
      224)
    object Label2: TLabel
      Left = 0
      Top = 3
      Width = 35
      Height = 15
      Caption = '&Name:'
    end
    object Label3: TLabel
      Left = 0
      Top = 32
      Width = 18
      Height = 15
      Caption = '&Url:'
    end
    object Label4: TLabel
      Left = 173
      Top = 3
      Width = 27
      Height = 15
      Anchors = [akTop, akRight]
      Caption = '&Type:'
      ExplicitLeft = 162
    end
    object edServerName: TEdit
      Left = 44
      Top = 0
      Width = 111
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edServerNameChange
    end
    object edServerUrl: TEdit
      Left = 44
      Top = 29
      Width = 265
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edServerUrlChange
    end
    object cbServerType: TComboBox
      Left = 209
      Top = 0
      Width = 100
      Height = 23
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = cbServerTypeClick
    end
  end
end
