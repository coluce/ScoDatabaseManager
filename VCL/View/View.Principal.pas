unit View.Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TViewPrincipal = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    TreeView1: TTreeView;
    btnNovo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
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

procedure TViewPrincipal.FormCreate(Sender: TObject);
begin
  ControllerPrincipal := TControllerPrincipal.Create(Self);
  ControllerPrincipal.FillList;
end;

end.
