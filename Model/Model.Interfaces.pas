unit Model.Interfaces;

interface

uses
  FireDAC.Comp.Client, System.Classes, Data.DB, Model.Types;

type

  IModelScript = interface;

  IModelConnection = interface
    ['{57DC635F-1707-4C36-B298-6AF1A7CB4DEA}']
    function GetConnection: TFDCustomConnection;
    function ExecScript(const AScript: IModelScript): boolean;

    procedure Close;
    procedure Open;

    function GetActive: boolean;
    procedure SetActive(const Value: boolean);

    property Active: boolean read GetActive write SetActive;

  end;

  IModelTable = interface
    ['{A7E026BC-0DA0-46DD-9195-0565737059DF}']

    // function ApplyUpdates: boolean;
    procedure Open(const AWhere: string = '');
    procedure Find(const AID: string); overload;
    procedure Find(AParams: TArray<TTableParam>); overload;
    function Delete(const AID: string): boolean;

    function GetDataSet: TDataSet;
    property DataSet: TDataSet read GetDataSet;
  end;

  IModelScript = interface
    ['{0870B4F1-478A-444E-9ED6-D87BBFA2A0FF}']

    function GetID: string;
    procedure SetID(const Value: string);
    property ID: string read GetID write SetID;

    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    property SQL: TStrings read GetSQL write SetSQL;

  end;

  IModelStructureUpdater = interface
    ['{697BCA1F-B50C-46EF-AD41-0C1CF77C6C20}']
    function Execute: boolean;
    procedure AddField(const ATableName: string; const AFieldName: string; const AFieldType: string;
      AFieldSize: integer = 0);
    procedure AddScript(const AScript: IModelScript);
  end;

  IModelDatabaseBackup = interface
    ['{B27E6601-BC2C-4510-A2A3-80A55430587F}']
    procedure Backup(const ADestinyFile: string; const ALevel: integer);
    procedure Restore(const ABackupFile: string);
  end;

implementation

end.
