unit Controller.Layout;

interface

uses
  Controller.Interfaces, Model.Interfaces, Model.Factory, View.Layout;

type
  TControllerLayout = class(TInterfacedObject, IControllerLayout)
  private
    FView: TViewLayout;
    FModelLayout: IModelTable;

    procedure PrepareData;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

  end;

implementation

{ TControllerLayout }

constructor TControllerLayout.Create;
begin
  FView := TViewLayout.Create(Self);
  FModelLayout := TModelTableFactory.New('TLAYOUT');
end;

destructor TControllerLayout.Destroy;
begin
  FView.Free;
  inherited;
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
