object DoctorWaitForm: TDoctorWaitForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'TMS Smart Setup Doctor'
  ClientHeight = 137
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  TextHeight = 15
  object lbMessage: TLabel
    AlignWithMargins = True
    Left = 16
    Top = 16
    Width = 352
    Height = 80
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 0
    Align = alClient
    Caption = 
      'Doctor is running in its own console window.'#13#10#13#10'This window c' +
      'loses when you close the doctor console. Closing this window sto' +
      'ps doctor.'
    ShowAccelChar = False
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 96
    Width = 384
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object btSwitch: TButton
      AlignWithMargins = True
      Left = 148
      Top = 8
      Width = 120
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = 'S&witch to doctor'
      Default = True
      TabOrder = 0
      OnClick = btSwitchClick
    end
    object btStop: TButton
      AlignWithMargins = True
      Left = 276
      Top = 8
      Width = 100
      Height = 25
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = '&Stop doctor'
      TabOrder = 1
      OnClick = btStopClick
    end
  end
  object Timer: TTimer
    Interval = 200
    OnTimer = TimerTimer
    Left = 24
    Top = 72
  end
end
