inherited ViewDataBaseBackup: TViewDataBaseBackup
  Caption = 'Backup'
  ClientHeight = 316
  ClientWidth = 296
  ExplicitWidth = 312
  ExplicitHeight = 355
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 296
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnBackup: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 50
      Height = 35
      Align = alLeft
      Caption = 'Backup'
      OnClick = btnBackupClick
    end
    object btnRestore: TSpeedButton
      AlignWithMargins = True
      Left = 59
      Top = 3
      Width = 50
      Height = 35
      Align = alLeft
      Caption = 'Restore'
      OnClick = btnRestoreClick
    end
    object btnDelete: TSpeedButton
      AlignWithMargins = True
      Left = 115
      Top = 3
      Width = 50
      Height = 35
      Align = alLeft
      Caption = 'Delete'
      OnClick = btnDeleteClick
      ExplicitLeft = 171
    end
  end
  object TreeViewBackupFiles: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 290
    Height = 250
    Align = alClient
    AutoExpand = True
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Indent = 19
    TabOrder = 1
    ExplicitHeight = 269
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 297
    Width = 296
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitLeft = 24
    ExplicitTop = 272
    ExplicitWidth = 0
  end
end
