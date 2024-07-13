inherited ViewRegisterDatabase: TViewRegisterDatabase
  BorderStyle = bsSingle
  Caption = 'Database'
  ClientHeight = 217
  ClientWidth = 407
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 423
  ExplicitHeight = 256
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
      0E1B000002314944415478DAED98BB4B1C5114C6CF824AC067309568E9AB8A29
      63A32062295A888FA86865FE07AD92FF41AB8888CF465307511BB5D546ADD5CE
      A8F828560BFD3EEE1D66332CECCCCE9DB92EB91FFC60D99DDD39DFCC39FB9DDD
      8C94B832B60B70066C17E00CD82EE03D18A806DDE0336807ADE0A3A6521FF304
      6EC10D380767E018EC82471B0658F4373002BE82B2223FE7051C8155B0021E92
      36C0C2E7C08C7E6C52F76001FC900877258A813EF00B34182E3CA82B3005FE98
      34C0565902E50917EFE9194C800D13065A440DDC87948AF794055FC0695C03DB
      A03FE5E23D6D81C1B806385CA60736AC78EEDAFFDE40C9B7503338113B43DC21
      2AB56319A086457D8D56A4543CBF46C7C166A103C31860FF33E27B45055963C2
      C55F880AB21D50236A0E621960FFFFD68FABC45F256A0C17CE42E7C14FF15789
      01517310CB007B909B6636E7391A19137F992BB6B5D82A07604DD44297BB0371
      E618A0AD710DBC827530A94F1814CD7469936D9A7A50A75F135DD81DB8967FD7
      E93D51AB7650BC20CB60A8508D610D505CAEA6C16591573BAC9AC022E8095363
      140314AF24D75DAEBDF721DE1B459CA9EF6056FC3B67C440BE24A611FE0061EF
      1E4AFED60A23B64AA7A8591A0D14EE9D3BF124A6997D513D7DA6F92BAAE7BDA1
      64619C894FA2869273C299E916FF67673EB924F6E49238825C1207E592585C12
      9B3140B9240EC825B14BE2846434893DD948E2828AFAF77ADA496CDC40AE91A4
      933851034133A69338550356E50CD89633605B256FE00D5020BA311A60841200
      00002574455874646174653A63726561746500323032302D31312D3239543034
      3A32343A34322B30303A303035B52F0B0000002574455874646174653A6D6F64
      69667900323032302D31312D32395430343A32343A34322B30303A303044E897
      B70000001974455874536F667477617265007777772E696E6B73636170652E6F
      72679BEE3C1A0000000049454E44AE426082}
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
    Width = 24
    Height = 13
    Caption = 'Local'
  end
  object lblBackupFolder: TLabel
    Left = 68
    Top = 105
    Width = 93
    Height = 13
    Caption = 'Diret'#243'rio de Backup'
  end
  object Panel1: TPanel
    Left = 0
    Top = 168
    Width = 407
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
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
    TextHint = 'apelido do banco de dados'
  end
  object EditLocal: TEdit
    Left = 68
    Top = 76
    Width = 331
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    TextHint = 'diret'#243'rio do banco de dados'
  end
  object edtBackupFolder: TEdit
    Left = 68
    Top = 124
    Width = 331
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    TextHint = 'diret'#243'rio do banco de dados'
  end
end
