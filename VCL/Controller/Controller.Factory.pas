unit Controller.Factory;

interface

uses
  Controller.Interfaces, Model.Types, Vcl.Forms, View.Principal;

type
  TControllerFactory = class
  public
    class function Principal(const AView: TViewPrincipal): IControllerPrincipal;
    class function DataBase(const ADataBase: TDataBase): IControllerDataBase;
    class function Layout: IControllerLayout;
    class function Exportini(const ADataBase: TDataBase): IControllerIni;
    class function Param: IControllerParam;
    class function Window(const AView: TForm): IControllerWindow;
  end;

implementation

uses
  Controller.Principal,
  Controller.DataBase,
  Controller.Layout,
  Controller.Ini,
  Controller.Param,
  Controller.Window;

{ TControllerFactory }

class function TControllerFactory.DataBase(const ADataBase: TDataBase): IControllerDataBase;
begin
  Result := TControllerDataBase.Create(ADataBase);
end;

class function TControllerFactory.Exportini(const ADataBase: TDataBase): IControllerIni;
begin
  Result := TControllerIni.Create(ADataBase);
end;

class function TControllerFactory.Layout: IControllerLayout;
begin
  Result := TControllerLayout.Create;
end;

class function TControllerFactory.Param: IControllerParam;
begin
  Result := TControllerParam.Create;
end;

class function TControllerFactory.Principal(
  const AView: TViewPrincipal): IControllerPrincipal;
begin
  Result := TControllerPrincipal.Create(AView);
end;

class function TControllerFactory.Window(const AView: TForm): IControllerWindow;
begin
  Result := TControllerWindow.Create(AView);
end;

end.
