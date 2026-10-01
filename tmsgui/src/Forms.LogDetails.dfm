object LogDetailsForm: TLogDetailsForm
  Left = 0
  Top = 0
  Caption = 'LogDetailsForm'
  ClientHeight = 435
  ClientWidth = 792
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object PanelButtons: TPanel
    Left = 0
    Top = 394
    Width = 792
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 192
    ExplicitTop = 216
    ExplicitWidth = 185
    DesignSize = (
      792
      41)
    object btOk: TButton
      Left = 701
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 461
    end
  end
  object Data: TMemo
    Left = 0
    Top = 0
    Width = 792
    Height = 394
    Align = alClient
    BorderStyle = bsNone
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 552
  end
end
