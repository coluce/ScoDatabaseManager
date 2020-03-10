unit Controller.Principal;

interface

uses
  SCO.FMX.MainLayout,
  FMX.Forms,
  Model.Interfaces;

type
  TControllerPrincipal = class
  private
    FFileLayout: IFileLayout;
    FOwner: TForm;
    FMainLaoyout: TSCOFMXMainLayout;
    class var FSelf: TControllerPrincipal;
  public
    constructor Create(const AOwner: TForm);
    function GetActualID: string;
    procedure ShowFileLayout;
    procedure ShowMenu;
    procedure ShowConfig(AConfig: IDataBaseConfig);
    procedure ShowDataBase(const AConfig: IDataBaseConfig);
    procedure SaveToDisk(const AConfig: IDataBaseConfig);
    class procedure Start(const AOwner: TForm);
    class procedure Stop;
    class function Instance: TControllerPrincipal;
  end;

implementation

uses
  Model.Factory,
  Model.FileLayout,
  View.FileLayout,
  View.Menu,
  View.Config,
  Controller.Menu,
  System.Classes,
  FMX.Types,
  FMX.Dialogs,
  System.UITypes,
  System.IOUtils,
  System.SysUtils;

{ TControllerPrincipal }

constructor TControllerPrincipal.Create(const AOwner: TForm);
var
  vDAO: TFileLayoutDao;
begin
  FOwner := AOwner;
  FMainLaoyout := TSCOFMXMainLayout.Create(FOwner);
  FOwner.AddObject(FMainLaoyout);
  FMainLaoyout.Align := TAlignLayout.Contents;
  vDAO := TFileLayoutDao.Create;
  try
    FFileLayout := vDAO.Get(ChangeFileExt(ParamStr(0),'.db'))[0];
  finally
    vDAO.Free;
  end;
  FMainLaoyout.OpenForm(TViewMenu);
end;

function TControllerPrincipal.GetActualID: string;
var
  vFile: TStrings;
  vFileName: string;
begin
  Result := EmptyStr;
  vFileName := TPath.Combine(FFileLayout.DefaultDirectory, FFileLayout.DefaultName);
  if FileExists(vFileName) then
  begin
    vFile := TStringList.Create;
    try
      vFile.LoadFromFile(vFileName);
      Result := vFile[0];
    finally
      vFile.DisposeOf;
    end;
  end;
end;

class function TControllerPrincipal.Instance: TControllerPrincipal;
begin
  Result := FSelf;
end;

procedure TControllerPrincipal.SaveToDisk(const AConfig: IDataBaseConfig);
var
  vFile: TStrings;
  vTemp: string;
  vActualID: string;
begin
  if MessageDlg('Definir como atual?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbYes) = mrYes then
  begin
    vActualID := AConfig.ID;
    vFile := TStringList.Create;
    try
      vFile.Add(vActualID);
      vTemp := FFileLayout.Layout;
      vTemp := vTemp.Replace('#server', AConfig.ServerName, [rfReplaceAll]);
      vTemp := vTemp.Replace('#database', AConfig.DataBase, [rfReplaceAll]);
      vFile.Add(vTemp);
      vFile.SaveToFile(TPath.Combine(FFileLayout.DefaultDirectory, FFileLayout.DefaultName));
    finally
      vFile.DisposeOf;
    end;
  end;
  TControllerMenu.Instance.Refresh(TControllerPrincipal.Instance.GetActualID);
end;

procedure TControllerPrincipal.ShowConfig(AConfig: IDataBaseConfig);
var
  vForm: TFormConfig;
  vDao: IDao<IDataBaseConfig>;
  vNew: boolean;
  vActualID: string;
begin
  vNew := not Assigned(AConfig);

  vActualID := GetActualID;

  vForm := TFormConfig.Create(FOwner);
  try
    if vNew then
    begin
      AConfig := TDataBaseConfigFactory.Config;
      AConfig.ID := TGUID.NewGuid.ToString;
      vForm.btnDelete.Enabled := False;
      vForm.btnOK.Enabled := False;
    end;
    vForm.ID := AConfig.ID;
    vForm.Description := AConfig.Description;
    vForm.ServerName := AConfig.ServerName;
    vForm.DataBase := AConfig.DataBase;
    vForm.ShowModal;
    if vForm.Action <> TCrudAction.caNone then
    begin
      vDao := TDataBaseConfigFactory.Dao;
      if vForm.Action = TCrudAction.caSave then
      begin
        AConfig.ID := vForm.ID;
        AConfig.Description := vForm.Description;
        AConfig.ServerName := vForm.ServerName;
        AConfig.DataBase := vForm.DataBase;
        vDao.Save(AConfig);

        if AConfig.ID.Equals(vActualID) then
        begin
          SaveToDisk(AConfig);
        end;

      end;
      if
        (not vNew) and
        (vForm.Action = TCrudAction.caDelete)
      then
      begin
        vDao.Delete(AConfig.ID);
      end;
      TControllerMenu.Instance.Refresh(vActualID);
    end;
  finally
    FreeAndNil(vForm);
  end;
end;

procedure TControllerPrincipal.ShowDataBase(const AConfig: IDataBaseConfig);
begin

end;

procedure TControllerPrincipal.ShowFileLayout;
begin
  FMainLaoyout.OpenForm(TViewFileLayout);
end;

procedure TControllerPrincipal.ShowMenu;
begin
  FMainLaoyout.OpenForm(TViewMenu);
  TControllerMenu.Instance.Refresh(TControllerPrincipal.Instance.GetActualID);
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