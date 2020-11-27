unit Controller.Factory;

interface

uses
  Controller.Interfaces, Model.Types;

type
  TControllerFactory = class
  public
    class function DataBase(const ADataBase: TDataBase): IControllerDataBase;
    class function Layout: IControllerLayout;
    class function Exportini(const ADataBase: TDataBase): IControllerIni;
  end;

implementation

uses
  Controller.DataBase,
  Controller.Layout,
  Controller.Ini;

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

end.
