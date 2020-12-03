unit View.Database.Backup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Vcl.ComCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Controller.Interfaces;

type
  TViewDataBaseBackup = class(TViewDefault)
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
    FController: IControllerDataBaseBackup;
  public
    { Public declarations }
    constructor Create(const AController: IControllerDataBaseBackup);
  end;

implementation

{$R *.dfm}

{ TViewDataBaseBackup }

constructor TViewDataBaseBackup.Create(
  const AController: IControllerDataBaseBackup);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewDataBaseBackup.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  FController._Release;
end;

procedure TViewDataBaseBackup.btnBackupClick(Sender: TObject);
begin
  inherited;
  FController.Backup;
end;

procedure TViewDataBaseBackup.btnDeleteClick(Sender: TObject);
begin
  inherited;
  FController.DeleteBackup;
end;

procedure TViewDataBaseBackup.btnRestoreClick(Sender: TObject);
begin
  inherited;
  FController.Restore;
end;

end.
