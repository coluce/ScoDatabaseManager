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
    lstConfigs: TListBox;
    procedure btnFileLayoutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ViewMenu: TViewMenu;

implementation

uses
  Controller.Principal;

{$R *.fmx}

procedure TViewMenu.btnFileLayoutClick(Sender: TObject);
begin
  inherited;
  TControllerPrincipal.Instance.ShowFileLayout;
end;

end.
