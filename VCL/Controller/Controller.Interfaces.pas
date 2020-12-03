unit Controller.Interfaces;

interface

uses
  Vcl.ComCtrls;

type

  IControllerView = interface
    ['{DCBFFF6E-68AB-4935-8F06-FF0248B526DB}']

    procedure Show;
    procedure ShowModal;

  end;

  IControllerPrincipal = interface
    ['{48937CC1-ECF8-473C-B155-5F0B9B3D635C}']
    procedure FindInUse;
    procedure FillList;
    procedure RegisterServer;
    procedure UnregisterServer(const ATreeNode: TTreeNode);
    procedure EditServer(const ATreeNode: TTreeNode);
    procedure RegisterDatabase(const ATreeNode: TTreeNode);
    procedure UnregisterDataBase(const ATreeNode: TTreeNode);
    procedure EditDataBase(const ATreeNode: TTreeNode);
    procedure ShowDataBase(const ATreeNode: TTreeNode);
    procedure ExportToDrive(const ATreeNode: TTreeNode);
    procedure IrParaCadastroLayout;
  end;

  IControllerWindow = interface
    ['{4E0E7316-5456-4977-A5DB-2B5D93100D48}']
    procedure SavePosition;
    procedure RestorePosition;
  end;

  IControllerDataBaseData = interface(IControllerView)
    ['{C870051D-3F20-4A0F-9023-37FEA157BF87}']

    procedure FillTableNames;
    procedure UpdateToogleColor;
    procedure ToogleSwitchClick;
    procedure FillSQLFromTreeView;
    procedure ExecuteQuery;

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);
    property Connected: boolean read GetConnected write SetConnected;
  end;

  IControllerDataBaseBackup = interface(IControllerView)
    ['{47F05023-3653-435E-A35A-95C1E8DB289E}']
    procedure FillBackupFiles;
    procedure Backup;
    procedure Restore;
  end;

  IControllerLayout = interface(IControllerView)
    ['{15A44337-42BC-408E-9B2D-383DEE1276B7}']
  end;

  IControllerIni = interface(IControllerView)
    ['{15A44337-42BC-408E-9B2D-383DEE1276B7}']
    procedure FillPreview;
    procedure ExportToDrive;
  end;

  IControllerParam = interface
    ['{D8D428EE-DAE2-4D0E-B987-90F7604B0DC1}']
    function GetParam(const ASession, AKey, ADefault: string): string;
    procedure SetParam(const ASession, AKey, AValue: string);
  end;

implementation

end.
