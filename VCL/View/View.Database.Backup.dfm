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
    ExplicitLeft = -171
    ExplicitWidth = 618
    object SpeedButton3: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 50
      Height = 35
      Align = alLeft
      Caption = 'Backup'
    end
    object SpeedButton4: TSpeedButton
      AlignWithMargins = True
      Left = 59
      Top = 3
      Width = 50
      Height = 35
      Align = alLeft
      Caption = 'Restore'
    end
  end
  object TreeViewBackupFiles: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 290
    Height = 269
    Align = alClient
    AutoExpand = True
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Indent = 19
    TabOrder = 1
    ExplicitTop = -141
    ExplicitWidth = 246
    ExplicitHeight = 342
  end
end
