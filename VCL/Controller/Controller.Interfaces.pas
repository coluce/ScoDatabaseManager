unit Controller.Interfaces;

interface

type

  IController = interface
    ['{DCBFFF6E-68AB-4935-8F06-FF0248B526DB}']

    procedure Show;
    procedure ShowModal;

  end;

  IControllerDataBase = interface(IController)
    ['{C870051D-3F20-4A0F-9023-37FEA157BF87}']

    procedure FillTableNames;
    procedure ExecuteQuery(const ASQL: string);

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);
    property Connected: boolean read GetConnected write SetConnected;

  end;

  IControllerLayout = interface(IController)
    ['{15A44337-42BC-408E-9B2D-383DEE1276B7}']
  end;

implementation

end.
