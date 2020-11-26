unit Controller.Factory;

interface

uses
  Controller.Interfaces, Model.Types;

type
  TControllerFactory = class
  public
    class function DataBase(const ADataBase: TDataBase): IControllerDataBase;
  end;

implementation

uses
  Controller.DataBase;

{ TControllerFactory }

class function TControllerFactory.DataBase(const ADataBase: TDataBase): IControllerDataBase;
begin
  Result := TControllerDataBase.Create(ADataBase);
end;

end.
