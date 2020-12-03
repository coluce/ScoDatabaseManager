unit Controller.Imp.DataBase.Backup;

interface

uses
  Controller.Interfaces, Model.Types, View.DataBase.Backup, Model.Interfaces;


type
  TControllerDataBaseBackup = class(TInterfacedObject, IControllerDataBaseBackup)
  private
    FDataBase: TDataBase;
    FView: TViewDataBaseBackup;

    procedure FillBackupFiles;
    procedure CreateBackupDirectory;
    function GetBackupDirectory: string;
    function GetBackupFileName: string;

  public

    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure Backup;
    procedure Restore;
  end;

const
  BACKUP_LEVEL_FULL: integer = 0;

implementation

uses
  System.SysUtils, Model.Factory, System.IOUtils, System.Types;

{ TControllerDataBaseBackup }

procedure TControllerDataBaseBackup.Backup;
var
  vModelManager: IModelDatabaseBackup;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  CreateBackupDirectory;
  vModelManager := TModelFactory.DataBasebackup(FDataBase);
  vModelManager.Backup(TPath.Combine(GetBackupDirectory, GetBackupFileName), BACKUP_LEVEL_FULL);

  Self.FillBackupFiles;

end;

constructor TControllerDataBaseBackup.Create(const ADataBase: TDataBase);
begin

end;

destructor TControllerDataBaseBackup.Destroy;
begin

  inherited;
end;

procedure TControllerDataBaseBackup.Restore;
var
  vModelManager: IModelDatabaseBackup;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  { TODO : apagar banco anterior }

  vModelManager := TModelFactory.DataBaseBackup(FDataBase);
  vModelManager.Restore(FView.TreeViewBackupFiles.Selected.Text);
end;

procedure TControllerDataBaseBackup.Show;
begin

end;

procedure TControllerDataBaseBackup.ShowModal;
begin

end;

procedure TControllerDataBaseBackup.CreateBackupDirectory;
var
  vDirectoryName: string;
begin
  vDirectoryName := GetBackupDirectory;
  if not DirectoryExists(vDirectoryName) then
  begin
    ForceDirectories(vDirectoryName);
  end;
  if not DirectoryExists(vDirectoryName) then
  begin
    raise Exception.Create('Não foi poss[ivel criar o diretório: ' + vDirectoryName);
  end;
end;

procedure TControllerDataBaseBackup.FillBackupFiles;
var
  vList: TStringDynArray;
  vDirectory: string;
  i: integer;
begin
  FView.TreeViewBackupFiles.Items.Clear;

  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  vDirectory := TPath.Combine(FDataBase.Path, 'backup');
  if DirectoryExists(vDirectory) then
  begin
    vList := TDirectory.GetFiles(vDirectory, '*.backup');
    for I := Low(vList) to High(vList) do
    begin
      FView.TreeViewBackupFiles.Items.Add(nil, ExtractFileName(vList[i]));
    end;
  end;

end;

function TControllerDataBaseBackup.GetBackupDirectory: string;
begin
  Result := TPath.Combine(FDataBase.Path, 'backup');
end;

function TControllerDataBaseBackup.GetBackupFileName: string;
begin
  Result := 'backup_' + FormatDateTime('yyyy_mm_dd_hh_nn_ss_zzz', Now) + '.backup';
end;

end.
