unit Controller.Ini;

interface

uses
  Controller.Interfaces, Model.Interfaces, Model.Factory, Model.Types, View.Ini;

type
  TControllerIni = class(TInterfacedObject, IControllerIni)
  private
    FView: TViewIni;
    FModelLayout: IModelTable;
    FDatabase: TDataBase;
    procedure PrepareData;
  public

    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;
    procedure FillPreview;
    procedure ExportToDrive;
  end;

implementation

uses
  Vcl.FileCtrl, System.SysUtils, System.IOUtils, Controller.Factory;

{ TControllerIni }

constructor TControllerIni.Create(const ADataBase: TDataBase);
begin
  FDatabase := ADataBase;
  FView := TViewIni.Create(Self);
  FView.Caption := 'Exportar [' + FDatabase.Name + ']';
  FModelLayout := TModelTableFactory.New('TLAYOUT');
end;

destructor TControllerIni.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerIni.ExportToDrive;
var
  vControllerParam: IControllerParam;
begin
  if not DirectoryExists(ExtractFilePath(FView.edtLocalDestino.Text)) then
    raise Exception.Create('Local de exporta��o inv�lido!');

  vControllerParam := TControllerFactory.Param;
  vControllerParam.SetParam('INI', 'DEFAULT_PATH', ExtractFilePath(FView.edtLocalDestino.Text));
  vControllerParam.SetParam('INI', 'DEFAULT_FILE_NAME', ExtractFileName(FView.edtLocalDestino.Text));
  vControllerParam.SetParam('INI', 'LAST_ID', FDatabase.ID);
  FView.SynMemo1.Lines.SaveToFile(FView.edtLocalDestino.Text);
end;

procedure TControllerIni.FillPreview;
var
  vControllerParam: IControllerParam;
begin
  FView.SynMemo1.Clear;

  FModelLayout.DataSet.First;
  while not FModelLayout.DataSet.Eof do
  begin
    if FModelLayout.DataSet.RecNo = (FView.ComboBoxLayout.ItemIndex + 1) then
    begin

      FView.SynMemo1.Lines.Add(FModelLayout.DataSet.FieldByName('LAYOUT').AsString);
      FView.SynMemo1.Text := FView.SynMemo1.Text.Replace('#server',FDatabase.Server.IP, [rfReplaceAll, rfIgnoreCase]);
      FView.SynMemo1.Text := FView.SynMemo1.Text.Replace('#database',FDatabase.Path, [rfReplaceAll, rfIgnoreCase]);

      Exit;
    end;
    FModelLayout.DataSet.Next;
  end;
end;

procedure TControllerIni.PrepareData;

  procedure FillComboBox;
  begin
    FView.ComboBoxLayout.Items.Clear;
    FModelLayout.DataSet.First;
    while not FModelLayout.DataSet.Eof do
    begin
      FView.ComboBoxLayout.Items.Add(FModelLayout.DataSet.FieldByName('NAME').AsString);
      FModelLayout.DataSet.Next;
    end;
    if FView.ComboBoxLayout.Items.Count > 0 then
    begin
      FView.ComboBoxLayout.ItemIndex := 0;
    end;
  end;

  procedure FillDefaultPath;
  var
    vControllerParam: IControllerParam;
    vDirectory: string;
    vFileName: string;
  begin
    vControllerParam := TControllerFactory.Param;
    vDirectory := vControllerParam.GetParam('INI','DEFAULT_PATH', ExtractFilePath(ParamStr(0)));
    vFileName := vControllerParam.GetParam('INI','DEFAULT_FILE_NAME', 'ALTERDB.INI');
    FView.edtLocalDestino.Text := TPath.Combine(vDirectory, vFileName);
  end;

begin
  FModelLayout.Open;
  FillComboBox;
  FillPreview;
  FillDefaultPath;
end;

procedure TControllerIni.Show;
begin
  PrepareData;
  FView.Show;
end;

procedure TControllerIni.ShowModal;
begin
  PrepareData;
  FView.ShowModal;
end;

end.
