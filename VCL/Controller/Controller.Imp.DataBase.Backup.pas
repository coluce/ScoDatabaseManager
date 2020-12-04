unit Controller.Imp.DataBase.Backup;

interface

uses
  Controller.Interfaces, Model.Types, View.DataBase.Backup, Model.Interfaces;


type
  TControllerDataBaseBackup = class(TInterfacedObject, IControllerDataBaseBackup)
  private
    FDataBase: TDataBase;
    FView: TViewDataBaseBackup;
    FDllDatabase: string;
    FControllerParam: IControllerParam;

    procedure FillBackupFiles;
    procedure CreateBackupDirectory;
    function GetBackupDirectory: string;
    function GetBackupFileName: string;
    function GetBackupFullFileName: string;
    function GetDatabaseFullFileName: string;

    procedure PrepareScreen;
    function ValidateDll: boolean;

  public

    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure Backup;
    procedure Restore;

    procedure DeleteBackup;
    procedure SetDLL;

  end;

const
  BACKUP_LEVEL_FULL: integer = 0;

implementation

uses
  System.SysUtils, Model.Factory, System.IOUtils, System.Types, Vcl.Dialogs,
  System.UITypes, Controller.Factory;

{ TControllerDataBaseBackup }

procedure TControllerDataBaseBackup.Backup;
var
  vModelManager: IModelDatabaseBackup;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  ValidateDll;

  try
    CreateBackupDirectory;
    vModelManager := TModelFactory.DataBasebackup(FDataBase, FDllDatabase);
    vModelManager.Backup(TPath.Combine(GetBackupDirectory, GetBackupFileName), BACKUP_LEVEL_FULL);

    ShowMessage('Backup concluído!');

  except
    raise;
  end;
  Self.FillBackupFiles;

end;

constructor TControllerDataBaseBackup.Create(const ADataBase: TDataBase);
begin
  FDataBase    := ADataBase;
  FView        := TViewDataBaseBackup.Create(Self);
  FControllerParam := TControllerFactory.ParamManager;
  FDllDatabase := FControllerParam.GetParam('FIREBIRD', 'DLL_PATH', 'C:\Program Files (x86)\Firebird\Firebird_3_0\fbclient.dll');
end;

procedure TControllerDataBaseBackup.DeleteBackup;
var
  vFileName: string;
begin
  if MessageDlg('Deletar backup?', mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    Exit;

  vFileName := TPath.Combine(GetBackupDirectory, FView.TreeViewBackupFiles.Selected.Text);
  if FileExists(vFileName) then
  begin
    if not DeleteFile(vFileName) then
    begin
      raise Exception.Create('Não foi possível excluir o backup!');
    end;
  end;
  Self.FillBackupFiles;
end;

destructor TControllerDataBaseBackup.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerDataBaseBackup.Restore;
var
  vModelManager: IModelDatabaseBackup;
  vOldFileName: string;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  ValidateDll;

  if MessageDlg('Restaurar a partir do backup?', mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    Exit;

  try
    if FileExists(GetDatabaseFullFileName) then
    begin
      vOldFileName := GetDatabaseFullFileName + '.old';
      if not RenameFile(GetDatabaseFullFileName, vOldFileName) then
      begin
        raise Exception.Create('Não foi possível renomear o banco de dados!');
      end;
    end;

    vModelManager := TModelFactory.DataBaseBackup(FDataBase, FDllDatabase);
    vModelManager.Restore(FView.TreeViewBackupFiles.Selected.Text);

    if FileExists(vOldFileName) then
    begin
      if not DeleteFile(vOldFileName) then
      begin
        raise Exception.Create('Não foi possível excluir o banco de dados antigo!');
      end;
    end;

    ShowMessage('Restore concluído!');

  except
    raise;
  end;

end;

procedure TControllerDataBaseBackup.SetDLL;
var
  vInput: string;
begin
  vInput := InputBox('Firebird', 'Informe o caminho completo da dll do Firebird', FDllDatabase);
  if FileExists(vInput) then
  begin
    FControllerParam.SetParam('FIREBIRD', 'DLL_PATH', vInput);
    FDllDatabase := vInput;
  end;
end;

procedure TControllerDataBaseBackup.Show;
begin
  FView.Show;
  Self.PrepareScreen;
end;

procedure TControllerDataBaseBackup.ShowModal;
begin
  FView.ShowModal;
  Self.PrepareScreen;
end;

function TControllerDataBaseBackup.ValidateDll: boolean;
var
  vRepeat: boolean;
begin
  repeat
    if FileExists(FDllDatabase) then
    begin
      vRepeat := False;
    end
    else
    begin
      SetDLL;
      if FileExists(FDllDatabase) then
      begin
        vRepeat := MessageDlg('Caminho inválido! Tentar novamente:', mtConfirmation, [mbOK, mbCancel], 0) = mrOK;
      end;
    end;
  until vRepeat;
  Result := FileExists(FDllDatabase);
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

function TControllerDataBaseBackup.GetBackupFullFileName: string;
begin
  Result := TPath.Combine(GetBackupDirectory, GetBackupFileName);
end;

function TControllerDataBaseBackup.GetDatabaseFullFileName: string;
begin
  Result := TPath.Combine(FDataBase.Path, 'ALTERDB.IB');
end;

procedure TControllerDataBaseBackup.PrepareScreen;
begin
  FView.Caption := 'Backup [' + FDataBase.Name + ']';
  FillBackupFiles;
  FView.StatusBar1.Panels[0].Text := FView.TreeViewBackupFiles.Items.Count.ToString + ' arquivos';
end;

end.

