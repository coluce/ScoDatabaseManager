inherited ViewServer: TViewServer
  BorderStyle = bsSingle
  Caption = 'Server'
  ClientHeight = 216
  ClientWidth = 407
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 423
  ExplicitHeight = 255
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 49
    Height = 49
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000300000
      003008060000005702F9870000000467414D410000B18F0BFC61050000000662
      4B4744000000000000F943BB7F000000097048597300000EC400000EC401952B
      0E1B000000C04944415478DAEDD7C1114030100550A983D18FD1879E68847EE8
      8375731109C9FC5DF97F664F2EFF1D241B57198F431720005D800074819C805E
      6694A9C11D379941668E05AC320DB8FCB54B1B0BD8D1AD43BA16057081DF73C1
      0920000D40A73C80F98BAC93991420CEF2E72AB1C4024C8400747E0DE07B2061
      F81ED0102E730410F011804E7900F31759F1EF81A74320490820401120F4FDE0
      DDEF91809863F7F6584C01D07681793B13404006C0DBA8F989DF267477F2EE36
      480024E60107938646316D9813890000002574455874646174653A6372656174
      6500323032302D31312D32395430343A31353A35372B30303A3030DD070D0D00
      00002574455874646174653A6D6F6469667900323032302D31312D3239543034
      3A31353A35372B30303A3030AC5AB5B10000001974455874536F667477617265
      007777772E696E6B73636170652E6F72679BEE3C1A0000000049454E44AE4260
      82}
  end
  object Label1: TLabel
    Left = 68
    Top = 11
    Width = 27
    Height = 13
    Caption = 'Nome'
  end
  object Label2: TLabel
    Left = 68
    Top = 57
    Width = 10
    Height = 13
    Caption = 'IP'
  end
  object lblPort: TLabel
    Left = 312
    Top = 57
    Width = 26
    Height = 13
    Caption = 'Porta'
  end
  object lblUsername: TLabel
    Left = 68
    Top = 105
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object lblPassword: TLabel
    Left = 228
    Top = 105
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Panel1: TPanel
    Left = 0
    Top = 167
    Width = 407
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 108
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 401
      Height = 2
      Align = alTop
      Shape = bsBottomLine
      ExplicitWidth = 441
    end
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 312
      Top = 11
      Width = 92
      Height = 35
      Align = alRight
      Caption = 'OK'
      OnClick = SpeedButton1Click
      ExplicitLeft = 352
    end
    object SpeedButton2: TSpeedButton
      AlignWithMargins = True
      Left = 214
      Top = 11
      Width = 92
      Height = 35
      Align = alRight
      Caption = 'Cancelar'
      OnClick = SpeedButton2Click
      ExplicitLeft = 352
    end
  end
  object EditNome: TEdit
    Left = 68
    Top = 30
    Width = 331
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    TextHint = 'apelido do servidor'
  end
  object EditLocal: TEdit
    Left = 68
    Top = 76
    Width = 238
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    TextHint = 'endere'#231'o do servidor'
  end
  object spnPort: TSpinEdit
    Left = 312
    Top = 76
    Width = 87
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
  end
  object edtPassword: TEdit
    Left = 228
    Top = 124
    Width = 171
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    AutoSelect = False
    TabOrder = 4
    TextHint = 'senha do banco'
  end
  object edtUserName: TEdit
    Left = 68
    Top = 124
    Width = 141
    Height = 21
    AutoSelect = False
    TabOrder = 5
    TextHint = 'usu'#225'rio do banco'
  end
end
