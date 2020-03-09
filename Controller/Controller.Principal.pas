unit Controller.Principal;

interface

uses
  SCO.FMX.MainLayout,
  FMX.Forms;

type
  TControllerPrincipal = class
  private
    FOwner: TForm;
    FMainLaoyout: TSCOFMXMainLayout;
    class var FSelf: TControllerPrincipal;
  public
    constructor Create(const AOwner: TForm);
    procedure ShowFileLayout;
    procedure ShowMenu;
    class procedure Start(const AOwner: TForm);
    class procedure Stop;
    class function Instance: TControllerPrincipal;
  end;

implementation

uses
  View.FileLayout,
  View.Menu,
  FMX.Types;

{ TControllerPrincipal }

constructor TControllerPrincipal.Create(const AOwner: TForm);
begin
  FOwner := AOwner;
  FMainLaoyout := TSCOFMXMainLayout.Create(FOwner);
  FOwner.AddObject(FMainLaoyout);
  FMainLaoyout.Align := TAlignLayout.Contents;
end;

class function TControllerPrincipal.Instance: TControllerPrincipal;
begin
  Result := FSelf;
end;

procedure TControllerPrincipal.ShowFileLayout;
begin
  FMainLaoyout.OpenForm(TViewFileLayout);
end;

procedure TControllerPrincipal.ShowMenu;
begin
  FMainLaoyout.OpenForm(TViewMenu);
end;

class procedure TControllerPrincipal.Start(const AOwner: TForm);
begin
  FSelf := TControllerPrincipal.Create(AOwner);
end;

class procedure TControllerPrincipal.Stop;
begin
  FSelf.Free;
end;

end.
