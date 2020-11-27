unit View.Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus,
  System.ImageList, Vcl.ImgList;

type
  TViewPrincipal = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    TreeView1: TTreeView;
    btnNovo: TButton;
    PopupMenuTreeView: TPopupMenu;
    Editar1: TMenuItem;
    Deletar1: TMenuItem;
    NovoBanco1: TMenuItem;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure Deletar1Click(Sender: TObject);
    procedure NovoBanco1Click(Sender: TObject);
    procedure PopupMenuTreeViewPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

procedure TViewPrincipal.btnNovoClick(Sender: TObject);
begin
  ControllerPrincipal.NewServer;
  ControllerPrincipal.FillList;
end;

procedure TViewPrincipal.Deletar1Click(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    ControllerPrincipal.DeleteServer(TreeView1.Selected);
  end
  else
  begin
    ControllerPrincipal.DeleteDataBase(TreeView1.Selected);
  end;
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

procedure TViewPrincipal.NovoBanco1Click(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    ControllerPrincipal.NewDataBase(TreeView1.Selected);
    ControllerPrincipal.FillList;
  end;
end;

procedure TViewPrincipal.PopupMenuTreeViewPopup(Sender: TObject);
begin
  if TreeView1.Selected.Level = 0 then
  begin
    NovoBanco1.Visible := True;
  end
  else
  begin
    NovoBanco1.Visible := False;
  end;
end;

procedure TViewPrincipal.TreeView1DblClick(Sender: TObject);
begin
  if TreeView1.Selected.Level = 1 then
  begin
    ControllerPrincipal.ShowDataBase(TreeView1.Selected);
  end;
end;

end.
