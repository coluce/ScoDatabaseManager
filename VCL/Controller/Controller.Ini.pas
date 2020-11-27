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
  end;

implementation

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

procedure TControllerIni.FillPreview;
begin

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
