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
  Vcl.FileCtrl, System.SysUtils;

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
begin
  if not DirectoryExists(FView.edtLocalDestino.Text) then
    raise Exception.Create('Local de exportação inválido!');

  FView.SynMemo1.Lines.SaveToFile(FView.edtLocalDestino.Text);
end;

procedure TControllerIni.FillPreview;
begin
  FView.SynMemo1.Clear;

  FModelLayout.DataSet.First;
  while not FModelLayout.DataSet.Eof do
  begin
    if FModelLayout.DataSet.RecNo = (FView.ComboBoxLayout.ItemIndex + 1) then
    begin
      FView.SynMemo1.Text := FModelLayout.DataSet.FieldByName('LAYOUT').AsString;
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
begin
  FModelLayout.Open;
  FillComboBox;
  FillPreview;
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
