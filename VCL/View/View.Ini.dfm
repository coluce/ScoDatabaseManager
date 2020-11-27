object ViewIni: TViewIni
  Left = 0
  Top = 0
  Caption = 'Exportar'
  ClientHeight = 235
  ClientWidth = 372
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    372
    235)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 33
    Height = 13
    Caption = 'Layout'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object Panel1: TPanel
    Left = 0
    Top = 186
    Width = 372
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 152
    ExplicitWidth = 447
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 366
      Height = 2
      Align = alTop
      Shape = bsBottomLine
      ExplicitWidth = 441
    end
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 277
      Top = 11
      Width = 92
      Height = 35
      Action = acnExportar
      Align = alRight
      ExplicitLeft = 352
    end
    object SpeedButton2: TSpeedButton
      AlignWithMargins = True
      Left = 179
      Top = 11
      Width = 92
      Height = 35
      Action = acnCancelar
      Align = alRight
      ExplicitLeft = 352
    end
  end
  object SynMemo1: TSynMemo
    Left = 8
    Top = 73
    Width = 356
    Height = 107
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.ShowCollapsedLine = True
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynIniSyn1
    FontSmoothing = fsmNone
  end
  object ComboBoxLayout: TComboBox
    Left = 8
    Top = 27
    Width = 356
    Height = 21
    AutoCloseUp = True
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 2
    Text = 'Principal'
    OnCloseUp = ComboBoxLayoutCloseUp
    Items.Strings = (
      'Principal')
  end
  object SynIniSyn1: TSynIniSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 232
    Top = 104
  end
  object ActionList1: TActionList
    Left = 80
    Top = 96
    object acnExportar: TAction
      Caption = 'Exportar'
      OnExecute = acnExportarExecute
    end
    object acnCancelar: TAction
      Caption = 'Cancelar'
      OnExecute = acnCancelarExecute
    end
  end
end
