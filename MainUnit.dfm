object FormTCreader: TFormTCreader
  Left = 192
  Top = 107
  Caption = 'TC Reader'
  ClientHeight = 623
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBoxWaveform: TPaintBox
    Left = 5
    Top = 438
    Width = 833
    Height = 177
  end
  object Memo1: TMemo
    Left = 5
    Top = 312
    Width = 849
    Height = 105
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBoxTC: TGroupBox
    Left = 5
    Top = 2
    Width = 849
    Height = 103
    Caption = 'GroupBoxTC'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 19
      Top = 22
      Width = 36
      Height = 20
      Caption = 'TC in'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 251
      Top = 22
      Width = 91
      Height = 20
      Caption = 'TC syncpoint'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 483
      Top = 22
      Width = 98
      Height = 20
      Caption = 'TC RAW data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelOffset: TLabel
      Left = 198
      Top = 22
      Width = 9
      Height = 20
      Caption = '0'
      OnDblClick = LabelOffsetDblClick
    end
    object LabelTelnet: TLabel
      Left = 672
      Top = 58
      Width = 9
      Height = 20
      Alignment = taCenter
      Caption = '0'
    end
    object PanelTC: TPanel
      Left = 19
      Top = 48
      Width = 217
      Height = 41
      Caption = '00:00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Arial'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object PanelTCSyncpoint: TPanel
      Left = 251
      Top = 48
      Width = 217
      Height = 41
      Caption = '00:00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'Arial'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = PanelTCSyncpointClick
    end
    object PanelRAW: TPanel
      Left = 483
      Top = 48
      Width = 174
      Height = 41
      Caption = '0000 0000 0000 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object ButtonOffsetMinus: TButton
      Left = 168
      Top = 24
      Width = 17
      Height = 18
      Caption = '-'
      Enabled = False
      TabOrder = 3
      TabStop = False
      OnClick = ButtonOffsetMinusClick
    end
    object ButtonOffsetPlus: TButton
      Left = 213
      Top = 24
      Width = 17
      Height = 18
      Caption = '+'
      Enabled = False
      TabOrder = 4
      TabStop = False
      OnClick = ButtonOffsetPlusClick
    end
    object TelnetSend: TButton
      Left = 672
      Top = 17
      Width = 161
      Height = 25
      Caption = 'Send'
      TabOrder = 5
      OnClick = TelnetSendClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 648
    Top = 232
  end
  object MainMenu1: TMainMenu
    Left = 328
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Settings1: TMenuItem
      Caption = '&Audio'
      object N1: TMenuItem
        Caption = '-'
      end
      object Channel0Sel: TMenuItem
        Caption = 'Left Channel in'
        GroupIndex = 2
        OnClick = ChannelSelClick
      end
      object Channel1sel: TMenuItem
        Tag = 1
        Caption = 'Right Channel in'
        GroupIndex = 2
        OnClick = ChannelSelClick
      end
      object ChannelDiffSel: TMenuItem
        Tag = 2
        Caption = 'Differential Mode'
        GroupIndex = 2
        OnClick = ChannelSelClick
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Autolevel1: TMenuItem
        Tag = 3
        Caption = 'Autolevel'
        GroupIndex = 3
        OnClick = ChannelSelClick
      end
    end
    object Mode1: TMenuItem
      Caption = 'Mode: Automatic'
      object Automatic1: TMenuItem
        AutoCheck = True
        Caption = 'Automatic'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = Automatic1Click
      end
      object Mainonlynoresync1: TMenuItem
        AutoCheck = True
        Caption = 'Main only (no resync)'
        GroupIndex = 1
        RadioItem = True
        OnClick = Mainonlynoresync1Click
      end
      object Idleonly1: TMenuItem
        AutoCheck = True
        Caption = 'Idle only'
        GroupIndex = 1
        RadioItem = True
        OnClick = Idleonly1Click
      end
    end
  end
end
