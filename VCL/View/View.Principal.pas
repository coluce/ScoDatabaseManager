unit View.Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, Vcl.Buttons,
  Controller.Interfaces;

type
  TViewMain = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    TreeView1: TTreeView;
    PopupMenuTreeView: TPopupMenu;
    Editar1: TMenuItem;
    Deletar1: TMenuItem;
    NovoBanco1: TMenuItem;
    ImageListTreeView: TImageList;
    SpeedButton1: TSpeedButton;
    ActionListAcoes: TActionList;
    ImageListActionList: TImageList;
    acnServerNovo: TAction;
    acnCadastroLayouts: TAction;
    SpeedButton2: TSpeedButton;
    acnPopupMenuEditar: TAction;
    acnPopupMenuExcluir: TAction;
    acnPopupMenuRegistrarBanco: TAction;
    acnPopupMenuExport: TAction;
    acnPopupMenuDefinirAtual1: TMenuItem;
    acnPopupMenuShowData: TAction;
    Conectar1: TMenuItem;
    acnPopupMenuBackup: TAction;
    Backup1: TMenuItem;
    SpeedButton3: TSpeedButton;
    acnCadastroParametros: TAction;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure PopupMenuTreeViewPopup(Sender: TObject);
    procedure acnServerNovoExecute(Sender: TObject);
    procedure acnCadastroLayoutsExecute(Sender: TObject);
    procedure acnPopupMenuExcluirExecute(Sender: TObject);
    procedure acnPopupMenuRegistrarBancoExecute(Sender: TObject);
    procedure acnPopupMenuShowDataExecute(Sender: TObject);
    procedure acnPopupMenuExportExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acnPopupMenuEditarExecute(Sender: TObject);
    procedure acnPopupMenuBackupExecute(Sender: TObject);
    procedure acnCadastroParametrosExecute(Sender: TObject);
  private
    { Private declarations }
    FControllerMain: IControllerMain;
    FControllerWindow: IControllerWindow;
  public
    { Public declarations }
  end;

var
  ViewMain: TViewMain;

implementation

uses
  Controller.Factory;

{$R *.dfm}

procedure TViewMain.acnCadastroLayoutsExecute(Sender: TObject);
begin
  FControllerMain.CallLayoutManager;
end;

procedure TViewMain.acnPopupMenuShowDataExecute(Sender: TObject);
begin
  FControllerMain.ShowDataBase;
end;

procedure TViewMain.acnCadastroParametrosExecute(Sender: TObject);
begin
  FControllerMain.CallParamManager;
end;

procedure TViewMain.acnPopupMenuBackupExecute(Sender: TObject);
begin
  FControllerMain.CallBackupManager;
end;

procedure TViewMain.acnPopupMenuEditarExecute(Sender: TObject);
begin
  FControllerMain.CallEdit;
end;

procedure TViewMain.acnPopupMenuExcluirExecute(Sender: TObject);
begin
  FControllerMain.CallUnregister;
end;

procedure TViewMain.acnPopupMenuExportExecute(Sender: TObject);
begin
  FControllerMain.ExportToDrive;
end;

procedure TViewMain.acnPopupMenuRegistrarBancoExecute(Sender: TObject);
begin
  FControllerMain.CallRegister;
end;

procedure TViewMain.acnServerNovoExecute(Sender: TObject);
begin
  FControllerMain.CallRegister;
end;

procedure TViewMain.FormActivate(Sender: TObject);
begin
  FControllerMain.FindInUse;
end;

procedure TViewMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FControllerWindow.SavePosition;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  FControllerMain := TControllerFactory.Main(Self);
  FControllerMain.FillList;
  FControllerWindow :=  TControllerFactory.Window(Self);
end;

procedure TViewMain.FormShow(Sender: TObject);
begin
  FControllerWindow.RestorePosition;
end;

procedure TViewMain.PopupMenuTreeViewPopup(Sender: TObject);
begin
  FControllerMain.PreparePopUp;
end;

procedure TViewMain.TreeView1DblClick(Sender: TObject);
begin
  FControllerMain.ShowDataBase;
end;

end.
