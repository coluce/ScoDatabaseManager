unit Controller.Imp.Backup.Manager;

interface

uses
  Controller.Interfaces, Model.Types, View.Backup.Manager, Model.Interfaces,
  View.Wait;


type
  TControllerBackupManager = class(TInterfacedObject, IControllerBackupManager)
  private
    FDataBase: TDataBase;
    FView: TViewBackupManager;
    FViewWait: TViewWait;
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
  System.UITypes, Controller.Factory, System.Classes;

{ TControllerDataBaseBackup }

procedure TControllerBackupManager.Backup;
var
  vViewWait: TViewWait;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  ValidateDll;

  try
    CreateBackupDirectory;

    vViewWait := TViewWait.Create(nil);

    TThread.CreateAnonymousThread(
      procedure
      var
        vModelManager: IModelDatabaseBackup;
      begin
        TThread.Synchronize(
          nil,
          procedure
          begin
            vViewWait.Show;
          end
        );

        vModelManager := TModelFactory.DataBasebackup(FDataBase, FDllDatabase);
        vModelManager.Backup(TPath.Combine(GetBackupDirectory, GetBackupFileName), BACKUP_LEVEL_FULL);
        TThread.Sleep(2000);

        TThread.Synchronize(
          TThread.CurrentThread,
          procedure
          begin
            vViewWait.Close;
            vViewWait.Free;
            Self.FillBackupFiles;
            ShowMessage('Backup concluído!');
          end
        );

      end
    ).Start;

  except
    raise;
  end;
end;

constructor TControllerBackupManager.Create(const ADataBase: TDataBase);
begin
  FDataBase    := ADataBase;
  FView        := TViewBackupManager.Create(Self);
  FControllerParam := TControllerFactory.IniManager;
  FDllDatabase := FControllerParam.GetParam('FIREBIRD', 'DLL_PATH', 'C:\Program Files (x86)\Firebird\Firebird_3_0\fbclient.dll');
end;

procedure TControllerBackupManager.DeleteBackup;
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

destructor TControllerBackupManager.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerBackupManager.Restore;
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

procedure TControllerBackupManager.SetDLL;
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

procedure TControllerBackupManager.Show;
begin
  FView.Show;
  Self.PrepareScreen;
end;

procedure TControllerBackupManager.ShowModal;
begin
  FView.ShowModal;
  Self.PrepareScreen;
end;

function TControllerBackupManager.ValidateDll: boolean;
var
  vRepeat: boolean;
begin
  repeat
    if FileExists(FDllDatabase) then
    begin
      vRepeat := True;
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

procedure TControllerBackupManager.CreateBackupDirectory;
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

procedure TControllerBackupManager.FillBackupFiles;
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

function TControllerBackupManager.GetBackupDirectory: string;
begin
  Result := TPath.Combine(FDataBase.Path, 'backup');
end;

function TControllerBackupManager.GetBackupFileName: string;
begin
  Result := 'backup_' + FormatDateTime('yyyy_mm_dd_hh_nn_ss_zzz', Now) + '.backup';
end;

function TControllerBackupManager.GetBackupFullFileName: string;
begin
  Result := TPath.Combine(GetBackupDirectory, GetBackupFileName);
end;

function TControllerBackupManager.GetDatabaseFullFileName: string;
begin
  Result := TPath.Combine(FDataBase.Path, 'ALTERDB.IB');
end;

procedure TControllerBackupManager.PrepareScreen;
begin
  FView.Caption := 'Backup [' + FDataBase.Name + ']';
  FillBackupFiles;
  FView.StatusBar1.Panels[0].Text := FView.TreeViewBackupFiles.Items.Count.ToString + ' arquivos';
end;

end.

