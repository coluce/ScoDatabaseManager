object ViewIni: TViewIni
  Left = 0
  Top = 0
  Caption = 'Exportar'
  ClientHeight = 382
  ClientWidth = 494
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    494
    382)
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
    Top = 104
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object Label3: TLabel
    Left = 8
    Top = 58
    Width = 36
    Height = 13
    Caption = 'Destino'
  end
  object Panel1: TPanel
    Left = 0
    Top = 333
    Width = 494
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 186
    ExplicitWidth = 372
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 488
      Height = 2
      Align = alTop
      Shape = bsBottomLine
      ExplicitWidth = 441
    end
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 399
      Top = 11
      Width = 92
      Height = 35
      Action = acnExportar
      Align = alRight
      ExplicitLeft = 352
    end
    object SpeedButton2: TSpeedButton
      AlignWithMargins = True
      Left = 301
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
    Top = 123
    Width = 478
    Height = 204
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
    Gutter.ShowLineNumbers = True
    Highlighter = SynIniSyn1
    FontSmoothing = fsmNone
    ExplicitWidth = 360
    ExplicitHeight = 130
  end
  object ComboBoxLayout: TComboBox
    Left = 8
    Top = 27
    Width = 478
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
    ExplicitWidth = 356
  end
  object edtLocalDestino: TButtonedEdit
    Left = 8
    Top = 77
    Width = 478
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    RightButton.Visible = True
    TabOrder = 3
    Text = 'E:\dese.git\Executaveis\FormulaCerta\alterdb.ini'
    TextHint = 'local onde o arquivo ini ser'#225' salvo'
    OnRightButtonClick = edtLocalDestinoRightButtonClick
    ExplicitWidth = 360
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
  object FileSaveDialog1: TFileSaveDialog
    DefaultFolder = 'E:\dese.git\Executaveis\FormulaCerta\'
    FavoriteLinks = <>
    FileName = 'alterdb.ini'
    FileTypes = <
      item
        DisplayName = '(*.ini)'
        FileMask = '*.ini'
      end>
    Options = []
    Left = 312
    Top = 104
  end
end
