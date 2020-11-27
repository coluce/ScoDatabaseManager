unit Controller.Factory;

interface

uses
  Controller.Interfaces, Model.Types;

type
  TControllerFactory = class
  public
    class function DataBase(const ADataBase: TDataBase): IControllerDataBase;
    class function Layout: IControllerLayout;
  end;

implementation

uses
  Controller.DataBase,
  Controller.Layout;

{ TControllerFactory }

class function TControllerFactory.DataBase(const ADataBase: TDataBase): IControllerDataBase;
begin
  Result := TControllerDataBase.Create(ADataBase);
end;

class function TControllerFactory.Layout: IControllerLayout;
begin
  Result := TControllerLayout.Create;
end;

end.
