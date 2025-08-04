object StartForm: TStartForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Getting started'
  ClientHeight = 269
  ClientWidth = 391
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    391
    269)
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 271
    Height = 15
    Caption = 'What are you going to use TMS Smart Setup for?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbTMS: TLabel
    Left = 36
    Top = 76
    Width = 341
    Height = 43
    AutoSize = False
    Caption = 
      'Use Smart Setup to manage and install official TMS Software libr' +
      'aries and tools.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    OnClick = lbTMSClick
  end
  object lbCommunity: TLabel
    Left = 36
    Top = 145
    Width = 341
    Height = 43
    AutoSize = False
    Caption = 
      'Use Smart Setup to install open-source projects from the Delphi ' +
      'developer community.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    OnClick = lbCommunityClick
  end
  object Label4: TLabel
    Left = 16
    Top = 199
    Width = 251
    Height = 15
    Caption = 'You can update this later in Settings (gear icon).'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object cbTMS: TCheckBox
    Left = 16
    Top = 56
    Width = 269
    Height = 17
    Caption = 'Install TMS Software products'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 0
    OnClick = cbTMSClick
  end
  object cbCommunity: TCheckBox
    Left = 16
    Top = 125
    Width = 269
    Height = 17
    Caption = 'Install open-source community packages'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = cbTMSClick
  end
  object btOk: TButton
    Left = 302
    Top = 232
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 2
    OnClick = btOkClick
  end
end
