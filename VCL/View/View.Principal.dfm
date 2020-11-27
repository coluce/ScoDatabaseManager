object ViewPrincipal: TViewPrincipal
  Left = 0
  Top = 0
  Caption = 'DataBase Manager'
  ClientHeight = 452
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 433
    Width = 274
    Height = 19
    Panels = <>
    ExplicitWidth = 574
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 274
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 574
    object btnNovo: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 35
      Align = alLeft
      Caption = 'Novo Server'
      TabOrder = 0
      OnClick = btnNovoClick
    end
  end
  object TreeView1: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 268
    Height = 386
    Align = alClient
    Indent = 19
    PopupMenu = PopupMenuTreeView
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    SortType = stText
    TabOrder = 2
    OnDblClick = TreeView1DblClick
    ExplicitWidth = 568
  end
  object PopupMenuTreeView: TPopupMenu
    OnPopup = PopupMenuTreeViewPopup
    Left = 320
    Top = 120
    object Editar1: TMenuItem
      Caption = 'Editar'
    end
    object Deletar1: TMenuItem
      Caption = 'Deletar'
      OnClick = Deletar1Click
    end
    object NovoBanco1: TMenuItem
      Caption = 'Novo Banco'
      OnClick = NovoBanco1Click
    end
  end
end
