unit Controller.Interfaces;

interface

type

  IControllerDataBase = interface
    ['{C870051D-3F20-4A0F-9023-37FEA157BF87}']
    procedure Show;
    procedure ShowModal;


    procedure FillTableNames;
    procedure ExecuteQuery(const ASQL: string);

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);
    property Connected: boolean read GetConnected write SetConnected;

  end;

implementation

end.
