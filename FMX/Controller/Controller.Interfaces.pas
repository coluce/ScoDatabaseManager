unit Controller.Interfaces;

interface

uses
  Model.Interfaces;

type

  IFormThemed = interface
    ['{0805C599-DA67-477B-BCD3-9FA49149C129}']
    procedure SetDark;
    procedure SetLight;
    function IsDark: boolean;
    function IsLight: boolean;
  end;

  IControllerConexao = interface
    ['{4F287F43-A9D2-4F3B-9A52-2EADA0C32274}']
    function TestConnection: boolean;
  end;

implementation

end.
