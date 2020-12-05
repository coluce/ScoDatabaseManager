unit View.Backup.Manager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Vcl.ComCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Controller.Interfaces;

type
  TViewBackupManager = class(TViewDefault)
    Panel2: TPanel;
    btnBackup: TSpeedButton;
    btnRestore: TSpeedButton;
    TreeViewBackupFiles: TTreeView;
    btnDelete: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnBackupClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
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

procedure TViewBackupManager.btnBackupClick(Sender: TObject);
begin
  inherited;
  FController.Backup;
end;

procedure TViewBackupManager.btnDeleteClick(Sender: TObject);
begin
  inherited;
  FController.DeleteBackup;
end;

procedure TViewBackupManager.btnRestoreClick(Sender: TObject);
begin
  inherited;
  FController.Restore;
end;

end.
