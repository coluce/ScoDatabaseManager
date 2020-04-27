unit Model.Interfaces;

interface

uses
  FireDAC.Comp.Client;

type

  IDao<T> = interface
    ['{2DC064A3-ABBE-4CD0-BF2D-CED9D5917AAF}']
    function Save(const Entity: T): boolean;
    function Delete(const ID: string): boolean;
    function Get(ID: string = ''): TArray<T>;
  end;

  IFileLayout = interface
    ['{0BA0C01A-91CE-4382-9DFE-7E715873E39E}']

    function GetDefaultDirectory: string;
    function GetDefaultName: string;
    function GetLayout: string;
    function GetThemeLight: boolean;

    procedure SetDefaultDirectory(const Value: string);
    procedure SetDefaultName(const Value: string);
    procedure SetLayout(const Value: string);
    procedure SetThemeLight(const Value: boolean);

    property DefaultDirectory: string read GetDefaultDirectory write SetDefaultDirectory;
    property DefaultName: string read GetDefaultName write SetDefaultName;
    property Layout: string read GetLayout write SetLayout;
    property ThemeLight: boolean read GetThemeLight write SetThemeLight;
  end;

  IDataBaseConfig = interface
    ['{53B98FFB-22DC-454C-B54C-467A179E872C}']

    function GetID: string;
    procedure SetID(const Value: string);
    property ID: string read GetID write SetID;

    function GetDescription: string;
    procedure SetDescription(const Value: string);
    property Description: string read GetDescription write SetDescription;

    function GetServerName: string;
    procedure SetServerName(const Value: string);
    property ServerName: string read GetServerName write SetServerName;

    function GetDataBase: string;
    procedure SetDataBase(const Value: string);
    property DataBase: string read GetDataBase write SetDataBase;

  end;

  IConexao = interface
    ['{0B477AC4-2578-4BFC-9846-846088A22A9B}']
    procedure SetupConnection;
    procedure Open;
    procedure Close;
    function GetBanco: TFDConnection;
    property Banco: TFDConnection read GetBanco;
  end;

implementation

end.
