object ViewLayout: TViewLayout
  Left = 0
  Top = 0
  Caption = 'Layout'
  ClientHeight = 456
  ClientWidth = 578
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 151
    Top = 33
    Height = 404
    ExplicitLeft = 176
    ExplicitTop = 96
    ExplicitHeight = 100
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 36
    Width = 145
    Height = 398
    Align = alLeft
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NAME'
        Title.Caption = 'Layout'
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavigator1: TDBNavigator
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 240
      Height = 27
      DataSource = DataSource1
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh, nbApplyUpdates, nbCancelUpdates]
      Align = alLeft
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 437
    Width = 578
    Height = 19
    Panels = <>
  end
  object Panel2: TPanel
    Left = 154
    Top = 33
    Width = 424
    Height = 404
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      424
      404)
    object lblNome: TLabel
      Left = 16
      Top = 8
      Width = 27
      Height = 13
      Caption = 'Nome'
    end
    object lblLayout: TLabel
      Left = 16
      Top = 66
      Width = 33
      Height = 13
      Caption = 'Layout'
    end
    object DBEdit1: TDBEdit
      Left = 16
      Top = 28
      Width = 393
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DataField = 'NAME'
      DataSource = DataSource1
      TabOrder = 0
    end
    object DBSynEdit1: TDBSynEdit
      Left = 16
      Top = 85
      Width = 393
      Height = 237
      DataField = 'LAYOUT'
      DataSource = DataSource1
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = SynIniSyn1
    end
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 10
      Top = 336
      Width = 404
      Height = 58
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alBottom
      BorderStyle = bsNone
      Lines.Strings = (
        'Tags:'
        ''
        '#server : ser'#225' subistitu'#237'do pelo endere'#231'o do servidor.'
        '#database: ser'#225' subistitu'#237'do pelo caminho do banco de dados.')
      ReadOnly = True
      TabOrder = 2
    end
  end
  object DataSource1: TDataSource
    Left = 200
    Top = 296
  end
  object SynIniSyn1: TSynIniSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 346
    Top = 217
  end
end
