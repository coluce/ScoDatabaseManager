unit View.Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, Vcl.Buttons,
  Controller.Interfaces;

type
  TViewPrincipal = class(TForm)
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
    acnPopupMenuConectar: TAction;
    Conectar1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure PopupMenuTreeViewPopup(Sender: TObject);
    procedure acnServerNovoExecute(Sender: TObject);
    procedure acnCadastroLayoutsExecute(Sender: TObject);
    procedure acnPopupMenuExcluirExecute(Sender: TObject);
    procedure acnPopupMenuRegistrarBancoExecute(Sender: TObject);
    procedure acnPopupMenuConectarExecute(Sender: TObject);
    procedure acnPopupMenuExportExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FControllerPrincipal: IControllerPrincipal;
    FControllerWindow: IControllerWindow;
  public
    { Public declarations }
  end;

var
  ViewPrincipal: TViewPrincipal;

implementation

uses
  Controller.Factory, Controller.Factory;

{$R *.dfm}

procedure TViewPrincipal.acnCadastroLayoutsExecute(Sender: TObject);
begin
  FControllerPrincipal.IrParaCadastroLayout;
end;

procedure TViewPrincipal.acnPopupMenuConectarExecute(Sender: TObject);
begin
  if TreeView1.Selected.Level = 1 then
  begin
    FControllerPrincipal.ShowDataBase(TreeView1.Selected);
  end;
end;

procedure TViewPrincipal.acnPopupMenuExcluirExecute(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    FControllerPrincipal.UnregisterServer(TreeView1.Selected);
  end
  else
  begin
    FControllerPrincipal.UnregisterDataBase(TreeView1.Selected);
  end;
  FControllerPrincipal.FillList;
end;

procedure TViewPrincipal.acnPopupMenuExportExecute(Sender: TObject);
begin
  FControllerPrincipal.ExportToDrive(Self.TreeView1.Selected);
end;

procedure TViewPrincipal.acnPopupMenuRegistrarBancoExecute(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    FControllerPrincipal.RegisterDataBase(TreeView1.Selected);
    FControllerPrincipal.FillList;
  end;
end;

procedure TViewPrincipal.acnServerNovoExecute(Sender: TObject);
begin
  FControllerPrincipal.RegisterServer;
  FControllerPrincipal.FillList;
end;

procedure TViewPrincipal.FormActivate(Sender: TObject);
begin
  FControllerPrincipal.FindInUse;
end;

procedure TViewPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FControllerWindow.SavePosition;
end;

procedure TViewPrincipal.FormCreate(Sender: TObject);
begin
  FControllerPrincipal := TControllerFactory.Principal(Self);
  FControllerPrincipal.FillList;

  FControllerWindow :=  TControllerFactory.Window(Self);

end;

procedure TViewPrincipal.FormShow(Sender: TObject);
begin
  FControllerWindow.RestorePosition;
end;

procedure TViewPrincipal.PopupMenuTreeViewPopup(Sender: TObject);
begin
  acnPopupMenuRegistrarBanco.Visible := TreeView1.Selected.Level = 0;
  acnPopupMenuEditar.Visible := TreeView1.Selected.Level = 0;
  acnPopupMenuExport.Visible := TreeView1.Selected.Level = 1;
  acnPopupMenuConectar.Visible := TreeView1.Selected.Level = 1;
end;

procedure TViewPrincipal.TreeView1DblClick(Sender: TObject);
begin
  acnPopupMenuConectar.Execute;
end;

end.
