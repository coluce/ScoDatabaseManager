unit Model.Imp.Database.Backup;

interface

uses
  Model.Interfaces, Model.Types, FireDAC.Phys.IBWrapper, FireDAC.Phys.FB;

type
  TModelDataBaseBackup = class(TInterfacedObject, IModelDatabaseBackup)
  private
    FDataBaseInfo: TDataBase;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;
  public
    constructor Create(ADataBaseInfo: TDataBase);
    destructor Destroy; override;

    procedure Backup(const ADestinyFile: string; const ALevel: integer);
    procedure Restore(const ABackupFile: string);
  end;

implementation

uses
  System.IOUtils;

{ TModelDataBaseBackup }

procedure TModelDataBaseBackup.Backup(const ADestinyFile: string; const ALevel: integer);
var
  FDFBNBackup1: TFDFBNBackup;
begin
  FDFBNBackup1 := TFDFBNBackup.Create(nil);
  try
    FDFBNBackup1.DriverLink := FDPhysFBDriverLink;

    FDFBNBackup1.UserName := FDataBaseInfo.UserName;
    FDFBNBackup1.Password := FDataBaseInfo.Password;
    FDFBNBackup1.Host := FDataBaseInfo.Server.IP;
    FDFBNBackup1.Protocol := ipTCPIP;

    FDFBNBackup1.Database := TPath.Combine(FDataBaseInfo.Path, 'ALTERDB.IB');
    FDFBNBackup1.BackupFile := ADestinyFile;
    FDFBNBackup1.Level := ALevel; // 0 - full backup

    FDFBNBackup1.Backup;
  finally
    FDFBNBackup1.Free;
  end;
end;

constructor TModelDataBaseBackup.Create(ADataBaseInfo: TDataBase);
begin
  FDataBaseInfo := ADataBaseInfo;
  FDPhysFBDriverLink := TFDPhysFBDriverLink.Create(nil);
  { TODO : definir parametro para a dll do firebird / sql }
  FDPhysFBDriverLink.VendorLib := 'C:\Program Files (x86)\Firebird\Firebird_3_0\fbclient.dll';
end;

destructor TModelDataBaseBackup.Destroy;
begin
  FDPhysFBDriverLink.Free;
  inherited;
end;

procedure TModelDataBaseBackup.Restore(const ABackupFile: string);
var
  FDFBNRestore1: TFDFBNRestore;
begin
  FDFBNRestore1 := TFDFBNRestore.Create(nil);
  try
    FDFBNRestore1.DriverLink := FDPhysFBDriverLink;
    FDFBNRestore1.UserName := FDataBaseInfo.UserName;
    FDFBNRestore1.Password := FDataBaseInfo.Password;
    FDFBNRestore1.Host     := FDataBaseInfo.Server.IP;
    FDFBNRestore1.Protocol := ipTCPIP;
    FDFBNRestore1.Database := TPath.Combine(FDataBaseInfo.Path, 'ALTERDB.IB');
    FDFBNRestore1.BackupFiles.Text := TPath.Combine(TPath.Combine(FDataBaseInfo.Path, 'backup'), ABackupFile);
    FDFBNRestore1.Restore;
  finally
    FDFBNRestore1.Free;
  end;
end;

end.
