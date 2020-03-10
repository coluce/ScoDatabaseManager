unit View.Menu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Default, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Objects, FMX.ImgList, System.ImageList;

type

  TViewMenu = class(TFormPadrao)
    tlbMenu: TToolBar;
    btnAdd: TSpeedButton;
    btnRefresh: TSpeedButton;
    il1: TImageList;
    gphTema: TGlyph;
    rtgTema: TRectangle;
    rtgLayout: TRectangle;
    Glyph2: TGlyph;
    procedure btnAddClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rtgLayoutClick(Sender: TObject);
    procedure rtgTemaClick(Sender: TObject);
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

procedure TViewMenu.rtgLayoutClick(Sender: TObject);
begin
  inherited;
  TControllerPrincipal.Instance.ShowFileLayout;
end;

procedure TViewMenu.rtgTemaClick(Sender: TObject);
begin
  inherited;
  if TControllerPrincipal.Instance.IsLight then
  begin
    TControllerPrincipal.Instance.SetDark;
    gphTema.ImageIndex := 3;
  end
  else
  begin
    TControllerPrincipal.Instance.SetLight;
    gphTema.ImageIndex := 4;
  end;
end;

end.
