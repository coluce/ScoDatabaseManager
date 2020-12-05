unit Controller.Imp.Param.Manager;

interface

uses
  Controller.Interfaces, Model.Interfaces, View.Param.Manager;

type
  TControllerParamManager = class(TInterfacedObject, IControllerParamManager)
  private
    FView: TViewParamManager;
    FModel: IModelTable;
    procedure PrepareScreen;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure ShowModal;
  end;

implementation

uses
  Model.Factory;

{ TControllerParamManager }

constructor TControllerParamManager.Create;
begin
  FView := TViewParamManager.Create(Self);
  FModel := TModelFactory.Table('TPARAM');
  FView.DataSource1.DataSet := FModel.DataSet;
end;

destructor TControllerParamManager.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerParamManager.PrepareScreen;
begin
  FModel.Open;
end;

procedure TControllerParamManager.Show;
begin
  FView.Show;
  PrepareScreen;
end;

procedure TControllerParamManager.ShowModal;
begin
  FView.ShowModal;
  PrepareScreen;
end;

end.
