unit View.Menu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Default, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation;

type

  TViewMenu = class(TFormPadrao)
    tlbMenu: TToolBar;
    btnAdd: TSpeedButton;
    btnRefresh: TSpeedButton;
    btnFileLayout: TSpeedButton;
    procedure btnFileLayoutClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  Controller.Principal,
  Controller.Menu;

{$R *.fmx}

procedure TViewMenu.btnAddClick(Sender: TObject);
begin
  inherited;
  TControllerPrincipal.Instance.ShowConfig(nil);
end;

procedure TViewMenu.btnFileLayoutClick(Sender: TObject);
begin
  inherited;
  TControllerPrincipal.Instance.ShowFileLayout;
end;

procedure TViewMenu.btnRefreshClick(Sender: TObject);
var
  vID: string;
begin
  inherited;
  vID := TControllerPrincipal.Instance.GetActualID;
  TControllerMenu.Instance.Refresh(vID);
end;

procedure TViewMenu.FormCreate(Sender: TObject);
begin
  inherited;
  TControllerMenu.Start(Self);
end;

procedure TViewMenu.FormDestroy(Sender: TObject);
begin
  inherited;
  TControllerMenu.Stop;
end;

end.
