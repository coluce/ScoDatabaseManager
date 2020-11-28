unit Controller.Layout;

interface

uses
  Controller.Interfaces, Model.Interfaces, Model.Factory, View.Layout, Data.DB;

type
  TControllerLayout = class(TInterfacedObject, IControllerLayout)
  private
    FView: TViewLayout;
    FModelLayout: IModelTable;

    procedure PrepareData;
    procedure DoOnNewRecord(DataSet: TDataSet);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

  end;

implementation

uses
  System.SysUtils;

{ TControllerLayout }

constructor TControllerLayout.Create;
begin
  FView := TViewLayout.Create(Self);
  FModelLayout := TModelTableFactory.New('TLAYOUT');
  FModelLayout.DataSet.OnNewRecord := DoOnNewRecord;
end;

destructor TControllerLayout.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerLayout.DoOnNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('ID').AsString := TGuid.NewGuid.ToString;
end;

procedure TControllerLayout.PrepareData;
begin
  FModelLayout.Open;
  FView.DataSource1.DataSet := FModelLayout.DataSet;
end;

procedure TControllerLayout.Show;
begin
  PrepareData;
  FView.Show;
end;

procedure TControllerLayout.ShowModal;
begin
  PrepareData;
  FView.ShowModal;
end;

end.
