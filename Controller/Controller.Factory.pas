unit Controller.Factory;

interface

uses
  Controller.Interfaces, Model.Types, Vcl.Forms;

type
  TControllerFactory = class
  public
    class function Main(const AView: TForm): IControllerMain;
    class function Query(const ADataBase: TDataBase): IControllerQuery;
    class function BackupManager(const ADataBase: TDataBase)
      : IControllerBackupManager;
    class function LayoutManager: IControllerLayout;
    class function ExportIniFile(const ADataBase: TDataBase)
      : IControllerIniFile;
    class function IniManager: IControllerParam;
    class function Window(const AView: TForm): IControllerWindow;
    class function ParamManager: IControllerParamManager;
  end;

implementation

uses
  Controller.Imp.Main,
  Controller.Imp.Query,
  Controller.Imp.Backup.Manager,
  Controller.Imp.Layout,
  Controller.Imp.Ini,
  Controller.Imp.Window,
  Controller.Imp.Param,
  Controller.Imp.Param.Manager;

{ TControllerFactory }

class function TControllerFactory.BackupManager(const ADataBase: TDataBase)
  : IControllerBackupManager;
begin
  Result := TControllerBackupManager.Create(ADataBase);
end;

class function TControllerFactory.Query(const ADataBase: TDataBase): IControllerQuery;
begin
  Result := TControllerQuery.Create(ADataBase);
end;

class function TControllerFactory.ExportIniFile(const ADataBase: TDataBase)
  : IControllerIniFile;
begin
  Result := TControllerIniFile.Create(ADataBase);
end;

class function TControllerFactory.LayoutManager: IControllerLayout;
begin
  Result := TControllerLayout.Create;
end;

class function TControllerFactory.IniManager: IControllerParam;
begin
  Result := TControllerParam.Create;
end;

class function TControllerFactory.Main(const AView: TForm): IControllerMain;
begin
  Result := TControllerMain.Create(AView);
end;

class function TControllerFactory.ParamManager: IControllerParamManager;
begin
  Result := TControllerParamManager.Create;
end;

class function TControllerFactory.Window(const AView: TForm): IControllerWindow;
begin
  Result := TControllerWindow.Create(AView);
end;

end.
