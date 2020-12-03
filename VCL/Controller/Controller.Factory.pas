unit Controller.Factory;

interface

uses
  Controller.Interfaces, Model.Types, Vcl.Forms, View.Principal;

type
  TControllerFactory = class
  public
    class function Main(const AView: TViewMain): IControllerMain;
    class function DataBaseData(const ADataBase: TDataBase): IControllerDataBaseData;
    class function DataBaseBackup(const ADataBase: TDataBase): IControllerDataBaseBackup;
    class function LayoutManager: IControllerLayout;
    class function Exportini(const ADataBase: TDataBase): IControllerIni;
    class function ParamManager: IControllerParam;
    class function Window(const AView: TForm): IControllerWindow;
  end;

implementation

uses
  Controller.Imp.Main,
  Controller.Imp.DataBase.Data,
  Controller.Imp.DataBase.Backup,
  Controller.Imp.Layout,
  Controller.Imp.Ini,
  Controller.Imp.Param,
  Controller.Imp.Window;

{ TControllerFactory }

class function TControllerFactory.DataBaseBackup(
  const ADataBase: TDataBase): IControllerDataBaseBackup;
begin
  Result := TControllerDatabaseBackup.Create(ADataBase);
end;

class function TControllerFactory.DataBaseData(const ADataBase: TDataBase): IControllerDataBaseData;
begin
  Result := TControllerDataBase.Create(ADataBase);
end;

class function TControllerFactory.Exportini(const ADataBase: TDataBase): IControllerIni;
begin
  Result := TControllerIni.Create(ADataBase);
end;

class function TControllerFactory.LayoutManager: IControllerLayout;
begin
  Result := TControllerLayout.Create;
end;

class function TControllerFactory.ParamManager: IControllerParam;
begin
  Result := TControllerParam.Create;
end;

class function TControllerFactory.Main(
  const AView: TViewMain): IControllerMain;
begin
  Result := TControllerMain.Create(AView);
end;

class function TControllerFactory.Window(const AView: TForm): IControllerWindow;
begin
  Result := TControllerWindow.Create(AView);
end;

end.
