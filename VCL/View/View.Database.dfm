object ViewDatabase: TViewDatabase
  Left = 0
  Top = 0
  Caption = 'ViewDatabase'
  ClientHeight = 401
  ClientWidth = 632
  Color = clBtnFace
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
    Left = 143
    Top = 41
    Height = 341
    ExplicitLeft = 232
    ExplicitTop = 192
    ExplicitHeight = 100
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 382
    Width = 632
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 632
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ToggleSwitch1: TToggleSwitch
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 72
      Height = 35
      Align = alLeft
      TabOrder = 0
      OnClick = ToggleSwitch1Click
      ExplicitHeight = 20
    end
  end
  object TreeViewTabelas: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 137
    Height = 335
    Align = alLeft
    Indent = 19
    TabOrder = 2
    OnDblClick = TreeViewTabelasDblClick
  end
  object Panel2: TPanel
    Left = 146
    Top = 41
    Width = 486
    Height = 341
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter2: TSplitter
      Left = 0
      Top = 207
      Width = 486
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 216
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 213
      Width = 480
      Height = 125
      Align = alBottom
      TabOrder = 0
      object PageControl1: TPageControl
        Left = 1
        Top = 1
        Width = 478
        Height = 123
        ActivePage = TabSheet1
        Align = alClient
        TabOrder = 0
        object TabSheet1: TTabSheet
          Caption = 'Dataset'
          object GridResultado: TDBGrid
            Left = 0
            Top = 32
            Width = 470
            Height = 63
            Align = alClient
            BorderStyle = bsNone
            DataSource = DataSource1
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 470
            Height = 32
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object DBNavigator1: TDBNavigator
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 240
              Height = 26
              DataSource = DataSource1
              VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh, nbApplyUpdates, nbCancelUpdates]
              Align = alLeft
              TabOrder = 0
            end
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'Log'
          ImageIndex = 1
          object MemoLog: TMemo
            Left = 0
            Top = 0
            Width = 470
            Height = 95
            Align = alClient
            Lines.Strings = (
              'MemoLog')
            TabOrder = 0
          end
        end
      end
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 480
      Height = 201
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object ToolBar2: TToolBar
        Left = 0
        Top = 0
        Width = 480
        Height = 29
        Caption = 'ToolBar2'
        TabOrder = 0
      end
      object MemoQuery: TSynMemo
        Left = 0
        Top = 29
        Width = 480
        Height = 172
        Align = alClient
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
        BorderStyle = bsNone
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Highlighter = SynSQLSyn1
        Lines.Strings = (
          '/* Exemplo de SQL*/'
          'select'
          '  *'
          'from'
          '  TABELA')
        FontSmoothing = fsmNone
      end
    end
  end
  object SynSQLSyn1: TSynSQLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    TableNameAttri.Foreground = 8404992
    TableNameAttri.Style = [fsBold]
    TableNames.Strings = (
      'TABELA')
    SQLDialect = sqlInterbase6
    Left = 549
    Top = 92
  end
  object FDQuery1: TFDQuery
    CachedUpdates = True
    Left = 482
    Top = 303
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 554
    Top = 303
  end
end
