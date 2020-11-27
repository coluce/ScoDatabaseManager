unit View.Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, Vcl.Buttons;

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
    procedure FormDestroy(Sender: TObject);
    procedure acnServerNovoExecute(Sender: TObject);
    procedure acnCadastroLayoutsExecute(Sender: TObject);
    procedure acnPopupMenuExcluirExecute(Sender: TObject);
    procedure acnPopupMenuRegistrarBancoExecute(Sender: TObject);
    procedure acnPopupMenuConectarExecute(Sender: TObject);
    procedure acnPopupMenuExportExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ViewPrincipal: TViewPrincipal;

implementation

uses
  Controller.Principal;

{$R *.dfm}

procedure TViewPrincipal.acnCadastroLayoutsExecute(Sender: TObject);
begin
  ControllerPrincipal.IrParaCadastroLayout;
end;

procedure TViewPrincipal.acnPopupMenuConectarExecute(Sender: TObject);
begin
  if TreeView1.Selected.Level = 1 then
  begin
    ControllerPrincipal.ShowDataBase(TreeView1.Selected);
  end;
end;

procedure TViewPrincipal.acnPopupMenuExcluirExecute(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    ControllerPrincipal.UnregisterServer(TreeView1.Selected);
  end
  else
  begin
    ControllerPrincipal.UnregisterDataBase(TreeView1.Selected);
  end;
  ControllerPrincipal.FillList;
end;

procedure TViewPrincipal.acnPopupMenuExportExecute(Sender: TObject);
begin
  ControllerPrincipal.ExportToDrive(Self.TreeView1.Selected);
end;

procedure TViewPrincipal.acnPopupMenuRegistrarBancoExecute(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    ControllerPrincipal.RegisterDataBase(TreeView1.Selected);
    ControllerPrincipal.FillList;
  end;
end;

procedure TViewPrincipal.acnServerNovoExecute(Sender: TObject);
begin
  ControllerPrincipal.RegisterServer;
  ControllerPrincipal.FillList;
end;

procedure TViewPrincipal.FormCreate(Sender: TObject);
begin
  ControllerPrincipal := TControllerPrincipal.Create(Self);
  ControllerPrincipal.FillList;
end;

procedure TViewPrincipal.FormDestroy(Sender: TObject);
begin
  ControllerPrincipal.Free;
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
