inherited ViewParamManager: TViewParamManager
  Caption = 'Ger'#234'nciador de Par'#226'metros'
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 240
      Height = 24
      DataSource = DataSource1
      Align = alLeft
      TabOrder = 0
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitHeight = 25
    end
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 33
    Width = 441
    Height = 146
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 182
    Width = 447
    Height = 19
    Panels = <>
    ExplicitLeft = 320
    ExplicitTop = 184
    ExplicitWidth = 0
  end
  object DataSource1: TDataSource
    Left = 120
    Top = 56
  end
end
