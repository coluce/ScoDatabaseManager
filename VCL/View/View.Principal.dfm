object ViewPrincipal: TViewPrincipal
  Left = 0
  Top = 0
  Caption = 'DataBase Manager'
  ClientHeight = 452
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 433
    Width = 574
    Height = 19
    Panels = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 574
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
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
      ExplicitLeft = 56
      ExplicitTop = 16
      ExplicitHeight = 25
    end
  end
  object TreeView1: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 568
    Height = 386
    Align = alClient
    Indent = 19
    PopupMenu = PopupMenuTreeView
    TabOrder = 2
    OnDblClick = TreeView1DblClick
  end
  object PopupMenuTreeView: TPopupMenu
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
    end
  end
end
