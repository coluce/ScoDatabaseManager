unit View.Backup.Manager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Vcl.ComCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Controller.Interfaces, System.Actions,
  Vcl.ActnList, System.ImageList, Vcl.ImgList;

type
  TViewBackupManager = class(TViewDefault)
    Panel2: TPanel;
    btnBackup: TSpeedButton;
    btnRestore: TSpeedButton;
    TreeViewBackupFiles: TTreeView;
    btnDelete: TSpeedButton;
    StatusBar1: TStatusBar;
    ImageListActionList: TImageList;
    ActionList1: TActionList;
    acnBackup: TAction;
    acnRestore: TAction;
    acnDelete: TAction;
    acnRefresh: TAction;
    acnFolder: TAction;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acnBackupExecute(Sender: TObject);
    procedure acnRestoreExecute(Sender: TObject);
    procedure acnDeleteExecute(Sender: TObject);
    procedure acnRefreshExecute(Sender: TObject);
    procedure acnFolderExecute(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerBackupManager;
  public
    { Public declarations }
    constructor Create(const AController: IControllerBackupManager);
  end;

implementation

{$R *.dfm}

{ TViewDataBaseBackup }

constructor TViewBackupManager.Create(
  const AController: IControllerBackupManager);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewBackupManager.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  FController._Release;
end;

procedure TViewBackupManager.acnBackupExecute(Sender: TObject);
begin
  inherited;
  FController.Backup;
end;

procedure TViewBackupManager.acnDeleteExecute(Sender: TObject);
begin
  inherited;
  FController.DeleteBackup;
end;

procedure TViewBackupManager.acnFolderExecute(Sender: TObject);
begin
  inherited;
  FController.OpenFolder;
end;

procedure TViewBackupManager.acnRefreshExecute(Sender: TObject);
begin
  inherited;
  FController.FillBackupFiles;
end;

procedure TViewBackupManager.acnRestoreExecute(Sender: TObject);
begin
  inherited;
  FController.Restore;
end;

end.
