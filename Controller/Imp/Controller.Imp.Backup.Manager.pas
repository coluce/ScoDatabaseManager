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
    FDllDatabase: string;
    FControllerParam: IControllerParam;

    procedure FillBackupFiles;
    procedure CreateBackupDirectory;
    function GetBackupDirectory: string;
    function GetBackupFileName: string;

    procedure PrepareScreen;
    function ValidateDll: boolean;

  public

    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure OpenFolder;

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
  System.UITypes, Controller.Factory, System.Classes, ShellAPI, Vcl.Forms,
  Winapi.Windows;

{ TControllerDataBaseBackup }

procedure TControllerBackupManager.Backup;
var
  LViewWait: TViewWait;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  ValidateDll;

  //CreateBackupDirectory;

  LViewWait := TViewWait.Create(nil);

  TThread.CreateAnonymousThread(
    procedure
    var
      LModel: IModelDatabaseBackup;
      LBackupFile: string;
    begin
      TThread.Synchronize(
        TThread.CurrentThread,
        procedure
        begin
          LViewWait.Show;
        end
      );

      LBackupFile := TPath.Combine(GetBackupDirectory, GetBackupFileName);

      LModel := TModelFactory.DataBasebackup(FDataBase, FDllDatabase);
      LModel.Backup(LBackupFile, BACKUP_LEVEL_FULL);

      TThread.Synchronize(
        TThread.CurrentThread,
        procedure
        begin
          LViewWait.Close;
          LViewWait.Free;
          Self.FillBackupFiles;
          ShowMessage('Backup concluído!');
        end
      );

    end
  ).Start;
end;

constructor TControllerBackupManager.Create(const ADataBase: TDataBase);
begin
  FDataBase := ADataBase;
  FView := TViewBackupManager.Create(Self);
  FControllerParam := TControllerFactory.IniManager;
  FDllDatabase := FControllerParam.GetParam('FIREBIRD', 'DLL_PATH',
    'C:\Program Files (x86)\Firebird\Firebird_3_0\fbclient.dll');
end;

procedure TControllerBackupManager.DeleteBackup;
var
  vFileName: string;
begin
  if MessageDlg('Deletar backup?', mtConfirmation, [mbOK, mbCancel], 0) = mrCancel
  then
    Exit;

  vFileName := TPath.Combine(GetBackupDirectory,
    FView.TreeViewBackupFiles.Selected.Text);
  if FileExists(vFileName) then
  begin
    if not System.SysUtils.DeleteFile(vFileName) then
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
  vOldFileName: string;
  vViewWait: TViewWait;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  ValidateDll;

  if MessageDlg('Restaurar a partir do backup?', mtConfirmation,
    [mbOK, mbCancel], 0) = mrCancel then
    Exit;

  if FileExists(FDataBase.DatabaseFile) then
  begin
    vOldFileName := FDataBase.DatabaseFile + '.old';
    if not RenameFile(FDataBase.DatabaseFile, vOldFileName) then
      raise Exception.Create('Não foi possível renomear o banco de dados!');
  end;

  vViewWait := TViewWait.Create(nil);
  try
    TThread.CreateAnonymousThread(
      procedure
      var
        vModelManager: IModelDatabaseBackup;
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            vViewWait.Show;
          end);

        vModelManager := TModelFactory.DataBasebackup(FDataBase, FDllDatabase);
        vModelManager.Restore(FView.TreeViewBackupFiles.Selected.Text);

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            vViewWait.Close;
            vViewWait.Free;

            if FileExists(vOldFileName) then
            begin
              if not System.SysUtils.DeleteFile(vOldFileName) then
              begin
                raise Exception.Create
                  ('Não foi possível excluir o banco de dados antigo!');
              end;
            end;

            Self.FillBackupFiles;
            ShowMessage('Restore concluído!');
          end);

      end).Start;

  except
    raise;
  end;

end;

procedure TControllerBackupManager.SetDLL;
var
  vInput: string;
begin
  vInput := InputBox('Firebird',
    'Informe o caminho completo da dll do Firebird', FDllDatabase);
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
  vRepeat := True;
  repeat
    if FileExists(FDllDatabase) then
    begin
      vRepeat := True;
    end
    else
    begin
      SetDLL;
      if not FileExists(FDllDatabase) then
      begin
        vRepeat := MessageDlg('Caminho inválido! Tentar novamente:', mtConfirmation, [mbOK, mbCancel], 0) = mrOK;
      end;
    end;
  until vRepeat;
  Result := FileExists(FDllDatabase);
end;

procedure TControllerBackupManager.CreateBackupDirectory;
var
  LDirectoryName: string;
begin
  LDirectoryName := GetBackupDirectory;
  if not DirectoryExists(LDirectoryName) then
    ForceDirectories(LDirectoryName);
  if not DirectoryExists(LDirectoryName) then
    raise Exception.Create('Não foi possível criar o diretório: ' + LDirectoryName);
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

  vDirectory := FDataBase.BackupFolder;
  if DirectoryExists(vDirectory) then
  begin
    vList := TDirectory.GetFiles(vDirectory, '*.backup');
    for i := Low(vList) to High(vList) do
    begin
      FView.TreeViewBackupFiles.Items.Add(nil, ExtractFileName(vList[i]));
    end;
  end;

end;

function TControllerBackupManager.GetBackupDirectory: string;
begin
  Result := FDataBase.BackupFolder;
end;

function TControllerBackupManager.GetBackupFileName: string;
begin
  Result := 'backup_' + FormatDateTime('yyyy_mm_dd_hh_nn_ss_zzz', Now) +
    '.backup';
end;

procedure TControllerBackupManager.OpenFolder;
begin
  if DirectoryExists(GetBackupDirectory) then
  begin
    ShellExecute(Application.Handle, PChar('explore'),
      PChar(GetBackupDirectory), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TControllerBackupManager.PrepareScreen;
begin
  FView.Caption := 'Backup [' + FDataBase.Name + ']';
  FillBackupFiles;
  FView.StatusBar1.Panels[0].Text := FView.TreeViewBackupFiles.Items.Count.
    ToString + ' arquivos';
end;

end.
